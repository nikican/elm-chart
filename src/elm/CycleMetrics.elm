module CycleMetrics exposing (threshold, nCycles, ratio)

import App.Types exposing (..)
import Utils exposing (x, y)
import List.Extra exposing (last)
import App.Utils exposing (cyclePeriodToString)
import Date exposing (fromTime, year)


type alias Quote =
    DayPrice


type alias Acc =
    { metrics : CycleMetrics
    , lastPeak : Quote
    , lastLow : Quote
    , brokenTh : Bool
    , lastBreakeven : Quote
    , lastSofts : List Float
    , prevPoint : Quote
    , prefixPoints : List CycleElement
    , threshold : Percentage
    , currYear : ( Int, PointPeriod )
    }


threshold : Float -> Series -> CycleMetrics
threshold th series =
    lastStep series <| currPoint series <| fromRawSeries th series True


nCycles : Int -> Series -> CycleMetrics
nCycles cycles series =
    let
        z =
            fromRawSeries 0 series False

        th =
            adjustedThreshold (cycles - 1) z.metrics.drops
    in
        threshold th series


adjustedThreshold : Int -> List CycleElement -> Float
adjustedThreshold n drops =
    if n == 0 then
        increment <| abs <| .pct <| Maybe.withDefault zeroElement <| List.head <| sortBySize drops
    else
        decrement <| abs <| .pct <| Maybe.withDefault zeroElement <| last <| List.take n <| sortBySize drops


sortBySize : List CycleElement -> List CycleElement
sortBySize =
    List.sortBy .pct


increment : Float -> Float
increment a =
    a + 0.000001


decrement : Float -> Float
decrement a =
    a - 0.000001


fromRawSeries : Float -> Series -> Bool -> Acc
fromRawSeries threshold series trackSoft =
    case List.head series of
        Just h ->
            List.foldl (folding trackSoft) (initAcc h threshold) series

        _ ->
            initAcc zeroQuote 0


lastStep : Series -> Acc -> CycleMetrics
lastStep series acc =
    case last series of
        Just lastPoint ->
            realLastStep lastPoint series acc

        Nothing ->
            acc.metrics


realLastStep : Quote -> Series -> Acc -> CycleMetrics
realLastStep lastPoint series origAcc =
    let
        acc =
            softReset origAcc lastPoint
    in
        appendPrefix acc acc.metrics
            |> extraRecoveryPoint acc.lastBreakeven
            |> lastGrowthPoint acc.lastBreakeven acc.lastPeak
            |> lastDropPoint acc
            |> lastRecoveryPoint acc.lastLow lastPoint
            |> calcSeriesPerformance
            |> finalizeYears acc.currYear
            |> flattenSofts


flattenSofts : CycleMetrics -> CycleMetrics
flattenSofts metrics =
    let
        growths =
            List.Extra.zip metrics.softs metrics.growths
                |> List.map
                    (\( s, gr ) ->
                        if s /= 0 then
                            { gr | soft = Just s }
                        else
                            gr
                    )
    in
        { metrics | growths = growths }


finalizeYears : ( Int, PointPeriod ) -> CycleMetrics -> CycleMetrics
finalizeYears y cm =
    let
        ys =
            y :: cm.years |> List.reverse
    in
        { cm | years = ys }


calcSeriesPerformance : CycleMetrics -> CycleMetrics
calcSeriesPerformance cm =
    let
        p =
            calcPerformance cm.head cm.last
    in
        { cm | performance = p }


appendPrefix : Acc -> CycleMetrics -> CycleMetrics
appendPrefix acc cm =
    { cm | growths = acc.prefixPoints ++ cm.growths }


extraRecoveryPoint : Quote -> CycleMetrics -> CycleMetrics
extraRecoveryPoint lastBreakeven cm =
    if List.length cm.recoveries < List.length cm.drops then
        case last cm.drops of
            Just lastDrop ->
                let
                    extraRec =
                        recoveryPoint lastDrop.period.end lastBreakeven
                in
                    { cm | recoveries = cm.recoveries ++ [ extraRec ] }

            Nothing ->
                cm
    else
        cm


lastGrowthPoint : Quote -> Quote -> CycleMetrics -> CycleMetrics
lastGrowthPoint lastBreakeven lastPeak cm =
    let
        extraPoint =
            growthPoint lastBreakeven lastPeak lastBreakeven
    in
        if extraPoint.pct /= 0 || (not <| List.isEmpty cm.growths) then
            { cm | growths = cm.growths ++ [ extraPoint ] }
        else
            cm


lastDropPoint : Acc -> CycleMetrics -> CycleMetrics
lastDropPoint acc cm =
    let
        extraPoint =
            dropPoint (calcDrawDownFromAcc acc) acc.lastPeak acc.lastLow
    in
        if extraPoint.pct /= 0 then
            { cm | drops = cm.drops ++ [ extraPoint ] }
        else
            cm


lastRecoveryPoint : Quote -> Quote -> CycleMetrics -> CycleMetrics
lastRecoveryPoint lastLow lastPoint cm =
    let
        extraPoint =
            recoveryPoint lastLow lastPoint
    in
        if extraPoint.pct /= 0 then
            { cm | recoveries = cm.recoveries ++ [ extraPoint ] }
        else
            cm


folding : Bool -> Quote -> Acc -> Acc
folding trackSoft quote acc =
    let
        acc_ =
            { acc | metrics = minmax acc.metrics quote }

        aac =
            directionMove trackSoft acc_ quote

        metrics =
            aac.metrics

        acc__ =
            { aac | prevPoint = quote, metrics = { metrics | last = quote } }
    in
        trackYears trackSoft quote acc__


trackYears : Bool -> Quote -> Acc -> Acc
trackYears trackIt quote acc =
    if trackIt then
        let
            metrics =
                acc.metrics

            years =
                acc.metrics.years

            qy =
                quoteYear quote

            ( cy, p ) =
                acc.currYear

            ( curr, ny ) =
                if qy == cy then
                    ( ( cy, { p | end = quote } ), years )
                else if moreThanTwoDays p.start p.end then
                    ( ( qy, { start = quote, end = quote } ), (acc.currYear :: years) )
                else
                    ( ( qy, { start = quote, end = quote } ), years )

            nm =
                { metrics | years = ny }
        in
            { acc | currYear = curr, metrics = nm }
    else
        acc


moreThanTwoDays : Quote -> Quote -> Bool
moreThanTwoDays start end =
    (Tuple.first end) - (Tuple.first start) > twoDays


twoDays : Float
twoDays =
    172800000


directionMove : Bool -> Acc -> Quote -> Acc
directionMove trackSoft acc (( x, y ) as dp) =
    if y >= (Tuple.second acc.lastPeak) then
        growth trackSoft acc dp
    else if y < (Tuple.second acc.lastLow) then
        fall trackSoft acc dp
    else
        acc



-- GROWING


growth : Bool -> Acc -> Quote -> Acc
growth trackSoft acc dp =
    let
        a =
            if acc.brokenTh then
                grow trackSoft acc dp
            else
                acc
    in
        { a | lastPeak = dp, lastLow = dp }


grow : Bool -> Acc -> Quote -> Acc
grow trackSoft origacc dp =
    let
        acc =
            softReset origacc dp

        drawDown =
            calcDrawDownFromAcc acc

        dropPt =
            dropPoint drawDown acc.lastPeak acc.lastLow

        lastBreakeven =
            accurateBreakevenPoint acc.lastPeak acc.prevPoint dp

        metrics_ =
            afterGrow dropPt acc.lastLow acc.metrics

        prefix =
            if 1 == List.length metrics_.drops then
                extraStartingPoint acc.metrics.head dropPt acc
            else
                acc.prefixPoints
    in
        { acc
            | lastBreakeven = lastBreakeven
            , brokenTh = False
            , prefixPoints = prefix
            , metrics = metrics_
        }



--- FALLING


fall : Bool -> Acc -> Quote -> Acc
fall trackSoft acc dp =
    let
        acc_ =
            { acc | lastLow = dp }
    in
        if overTh acc_ then
            breakThreshold acc_ dp
        else if trackSoft then
            softSave acc_ dp
        else
            acc_


breakThreshold : Acc -> Quote -> Acc
breakThreshold acc dp =
    { acc
        | brokenTh = True
        , metrics = resetGrowthAndRecovery acc.lastBreakeven acc.lastPeak acc.metrics
    }


resetGrowthAndRecovery : Quote -> Quote -> CycleMetrics -> CycleMetrics
resetGrowthAndRecovery lastBreakeven lastPeak cm =
    let
        peaks =
            List.append cm.peaks [ lastPeak ]

        lastLow =
            Maybe.withDefault zeroQuote <| last cm.lows
    in
        case last cm.peaks of
            Just opeak ->
                if isNotEq opeak lastPeak then
                    { cm
                        | recoveries = List.append cm.recoveries [ recoveryPoint lastLow lastBreakeven ]
                        , growths = List.append cm.growths [ growthPoint opeak lastPeak lastBreakeven ]
                        , peaks = peaks
                    }
                else
                    { cm | peaks = peaks }

            Nothing ->
                { cm | peaks = peaks }


isEq : Quote -> Quote -> Bool
isEq ( x, y ) ( x_, y_ ) =
    x == x_ && y == y_


isNotEq : Quote -> Quote -> Bool
isNotEq a b =
    not <| isEq a b


overTh : Acc -> Bool
overTh acc =
    acc.threshold < dropPct (y acc.lastPeak) (y acc.lastLow)


dropPct : Price -> Price -> Percentage
dropPct high low =
    1 - low / high


accurateBreakevenPoint : Quote -> Quote -> Quote -> Quote
accurateBreakevenPoint a b c =
    let
        xDelta =
            (x c) - (x b)

        yDelta =
            (y c) - (y b)

        ratio =
            ((y a) - (y b)) / yDelta
    in
        ( timeRound <| xDelta * ratio + (x b)
        , decimalRound <| yDelta * ratio + (y b)
        )


decimalRound : Float -> Float
decimalRound a =
    a


timeRound : Float -> Float
timeRound a =
    let
        const =
            100000
    in
        (a / const) * const


afterGrow : CycleElement -> Quote -> CycleMetrics -> CycleMetrics
afterGrow dropPt quote cm =
    let
        drops_ =
            List.append cm.drops [ dropPt ]

        lows_ =
            List.append cm.lows [ quote ]
    in
        { cm | drops = drops_, lows = lows_ }


currPoint : Series -> Acc -> Acc
currPoint series acc =
    case last series of
        Just lastpoint ->
            let
                metrics =
                    metricsPeakToCurr acc.lastPeak lastpoint acc.metrics
            in
                { acc | metrics = metrics }

        Nothing ->
            acc


metricsPeakToCurr : Quote -> Quote -> CycleMetrics -> CycleMetrics
metricsPeakToCurr lastPeak lastSeries cm =
    let
        p =
            pointPeakToCurr lastPeak lastSeries
    in
        { cm | peakToCur = p }


calcPeakToCurr : Quote -> Quote -> Percentage
calcPeakToCurr lastPeak lastSeries =
    ratio (y lastSeries) (y lastPeak)


softSave : Acc -> Quote -> Acc
softSave acc dp =
    let
        d =
            calcDrawDownFromAcc acc
    in
        { acc | lastSofts = List.append acc.lastSofts [ d ] }


softReset : Acc -> Quote -> Acc
softReset acc dp =
    let
        d =
            Tuple.first acc.lastPeak

        theSoft =
            List.filter (\s -> s <= d) acc.lastSofts
                |> last
                |> Maybe.withDefault 0

        m =
            acc.metrics

        nm =
            { m | softs = m.softs ++ [ theSoft ] }
    in
        { acc | metrics = nm, lastSofts = [] }


calcDrawDownFromAcc : Acc -> Percentage
calcDrawDownFromAcc acc =
    calcDrawDown (y acc.lastLow) (y acc.lastPeak)


calcPerformance : Quote -> Quote -> Percentage
calcPerformance start end =
    ratio (y end) (y start)


ratio : Price -> Price -> Percentage
ratio a b =
    (a / b) - 1


calcDrawDown : Price -> Price -> Percentage
calcDrawDown low high =
    low / high - 1


calcGrowth : Price -> Price -> Percentage
calcGrowth origHigh lastHigh =
    lastHigh / origHigh - 1


calcRecovery : Price -> Price -> Percentage
calcRecovery origLow lastHigh =
    lastHigh / origLow - 1


extraStartingPoint : Quote -> CycleElement -> Acc -> List CycleElement
extraStartingPoint firstPoint firstDrop acc =
    if (Tuple.first firstPoint) /= firstDrop.x then
        [ growthPoint firstPoint ( firstDrop.x, firstDrop.y ) firstPoint ]
    else
        []


recoveryPoint : Quote -> Quote -> CycleElement
recoveryPoint origLow breakeven =
    { x = x origLow
    , y = y origLow
    , pct = calcRecovery (y origLow) (y breakeven)
    , price = y origLow
    , period =
        { start = origLow
        , end = breakeven
        }
    , kind = cyclePeriodToString Recovery
    , cycle = Recovery
    , soft = Nothing
    }


growthPoint : Quote -> Quote -> Quote -> CycleElement
growthPoint origHigh lastHigh breakeven =
    { x = Tuple.first breakeven
    , y = Tuple.second breakeven
    , pct = calcGrowth (Tuple.second origHigh) (Tuple.second lastHigh)
    , price = Tuple.second breakeven
    , period =
        { start = breakeven
        , end = lastHigh
        }
    , kind = cyclePeriodToString Growth
    , cycle = Growth
    , soft = Nothing
    }


dropPoint : Percentage -> Quote -> Quote -> CycleElement
dropPoint drawDown lastHigh low =
    { x = Tuple.first lastHigh
    , y = Tuple.second lastHigh
    , pct = drawDown
    , price = Tuple.second lastHigh
    , period =
        { start = lastHigh
        , end = low
        }
    , kind = cyclePeriodToString Drop
    , cycle = Drop
    , soft = Nothing
    }


pointPeakToCurr : Quote -> Quote -> CycleElement
pointPeakToCurr lastPeak lastSeries =
    { x = x lastSeries
    , y = y lastSeries
    , pct = calcPeakToCurr lastPeak lastSeries
    , price = y lastSeries
    , period =
        { start = lastPeak
        , end = lastSeries
        }
    , kind = cyclePeriodToString PeakToCurr
    , cycle = PeakToCurr
    , soft = Nothing
    }


minmax : CycleMetrics -> Quote -> CycleMetrics
minmax cm quote =
    { cm
        | min = minVal cm.min quote
        , max = maxVal cm.max quote
    }


minVal : Quote -> Quote -> Quote
minVal a b =
    if (Tuple.second a) > (Tuple.second b) then
        b
    else
        a


maxVal : Quote -> Quote -> Quote
maxVal a b =
    if (Tuple.second a) < (Tuple.second b) then
        b
    else
        a


emptyCm : CycleMetrics
emptyCm =
    { recoveries = []
    , drops = []
    , growths = []
    , peaks = []
    , lows = []
    , softs = []
    , min = zeroQuote
    , max = zeroQuote
    , head = zeroQuote
    , last = zeroQuote
    , peakToCur = zeroElement
    , performance = 0
    , years = []
    }


startMetrics : Quote -> CycleMetrics
startMetrics quote =
    { recoveries = []
    , drops = []
    , growths = []
    , peaks = []
    , lows = []
    , softs = []
    , min = quote
    , max = quote
    , head = quote
    , last = quote
    , peakToCur = zeroElement
    , performance = 0
    , years = []
    }


zeroQuote : Quote
zeroQuote =
    ( 0, 0 )


zeroElement : CycleElement
zeroElement =
    { x = 0
    , y = 0
    , pct = 0
    , price = 0
    , period =
        { start = zeroQuote
        , end = zeroQuote
        }
    , kind = ""
    , cycle = Growth
    , soft = Nothing
    }


initAcc : Quote -> Percentage -> Acc
initAcc quote th =
    { metrics = startMetrics quote
    , lastPeak = quote
    , lastLow = quote
    , brokenTh = False
    , lastBreakeven = quote
    , lastSofts = []
    , prevPoint = quote
    , prefixPoints = []
    , threshold = th
    , currYear = ( (aYear <| Tuple.first quote), { start = quote, end = quote } )
    }


aYear : Float -> Int
aYear =
    fromTime >> year


quoteYear : Quote -> Int
quoteYear =
    Tuple.first >> aYear
