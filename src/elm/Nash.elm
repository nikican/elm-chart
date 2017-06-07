module Nash exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import List.Extra


{-

   PLAN:
    - Accept YZoom Limits
    - Time indicators on X axis
    - Zones / Recovery and Drops
    - Scatter Points / Growth, Recovery, Drops
    - Buy Marker Point
    - Hovering Crosshair
    - Annual Performance Lines and Points

-}


type alias Size =
    { width : Float
    , height : Float
    }


plot : Size -> List ( Int, Float ) -> Svg msg
plot size data =
    svg [ width (toString size.width), height (toString size.height) ]
        [ polygon
            [ points <| dataToPoints size data
            , class "area"
            ]
            []
        ]


dataToPoints : Size -> List ( Int, Float ) -> String
dataToPoints size data =
    let
        headStart =
            List.head data |> Maybe.map Tuple.first |> Maybe.withDefault 0

        headEnd =
            List.Extra.last data |> Maybe.map Tuple.first |> Maybe.withDefault 0

        ( valueMin, valueMax ) =
            getMinMax data

        ratioBox =
            getRatioBox size headStart headEnd valueMin valueMax

        points =
            List.map ratioBox data
    in
        stringifyPoints points ++ " " ++ (( headEnd, valueMin ) |> ratioBox |> stringfyPoint)


stringifyPoints : List ( Int, Float ) -> String
stringifyPoints =
    List.map stringfyPoint >> String.join " "


stringfyPoint : ( Int, Float ) -> String
stringfyPoint ( x, y ) =
    (toString x) ++ "," ++ (toString y)


type alias RatioBox =
    ( Int, Float ) -> ( Int, Float )


getRatioBox : Size -> Int -> Int -> Float -> Float -> RatioBox
getRatioBox size xa xb ya yb =
    let
        deltaX =
            xb - xa

        deltaY =
            yb - ya

        ratioX =
            (toFloat deltaX) / size.width

        ratioY =
            deltaY / size.height
    in
        \( x, y ) ->
            ( floor ((toFloat (x - xa)) / ratioX), ((yb - y) / ratioY) )


getMinMax : List ( Int, Float ) -> ( Float, Float )
getMinMax data =
    let
        vls =
            List.map Tuple.second data

        max =
            List.maximum vls |> Maybe.withDefault 0

        min =
            List.minimum vls |> Maybe.withDefault 100
    in
        ( min, max )
