module Pie exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias Item =
    { name : String
    , value : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }


size1 : Float -> Size
size1 b =
    { width = b, height = 30.0 }


getCoordinatesForPercent : Float -> ( Float, Float )
getCoordinatesForPercent percent =
    let
        x =
            cos (2 * pi) * percent

        y =
            sin (2 * pi) * percent
    in
        ( x, y )


calculateSliceData : Item -> Float -> String
calculateSliceData item total =
    let
        cumulativeRadians =
            0

        percent =
            item.value / total

        startX =
            cos (cumulativeRadians)

        startY =
            sin (cumulativeRadians)

        cumulativeRadians =
            2 * pi * percent

        endX =
            cos (cumulativeRadians)

        endY =
            sin (cumulativeRadians)
    in
        toString percent



--let cumulativeRadians = 0;
--
--      slices = scoreSummary.results.map((slice) => {
--        const percent = slice.count / total;
--
--        const startX = Math.cos(cumulativeRadians);
--        const startY = Math.sin(cumulativeRadians);
--
--        cumulativeRadians += 2 * Math.PI * percent;
--
--        const endX = Math.cos(cumulativeRadians);
--        const endY = Math.sin(cumulativeRadians);
--
--        const largeArcFlag = percent > 0.5 ? 1 : 0;
--
--        const d = [
--          `M ${startX} ${startY}`,
--          `A 1 1 0 ${largeArcFlag} 1 ${endX} ${endY}`,
--          `L 0 0`,
--        ].join(` `);
--
--        return path({
--          d,
--          style: {
--            fill: slice.score.color || ``, // set empty string to make iOS ignore fill
--          },
--        });


view : Size -> List Item -> (Item -> msg) -> Svg msg
view viewPortSize items onclick =
    let
        s =
            size1 6

        cumulativeRadians =
            0

        plots =
            items
                |> List.map (\i -> Svg.text_ [] [ Svg.text i.name ])
                |> Svg.g []
    in
        Svg.svg
            [ width "400px"
            , height "300px"
            , viewBox "-1 -1 2 2"
            , style "transform: rotate(-90deg)"
            ]
            [ plots
            ]
