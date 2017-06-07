module Pie exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import String exposing (join)
import Color exposing (..)


type alias Item =
    { name : String
    , value : Float
    , color : Color
    }


type alias Size =
    { width : Float
    , height : Float
    }


calculateSliceData : Item -> Svg msg
calculateSliceData item =
    let
        cumulativeRadians =
            0

        percent =
            item.value / 100

        startX =
            cos (cumulativeRadians)

        startY =
            sin (cumulativeRadians)

        cumulativeRadians2 =
            2 * pi * percent

        endX =
            cos (cumulativeRadians2)

        endY =
            sin (cumulativeRadians2)

        largeArcFlag =
            if percent > 0.5 then
                1
            else
                0

        pathD =
            String.join " "
                [ "M"
                , toString startX
                , toString startY
                , "A 1 1 0"
                , toString largeArcFlag
                , "1"
                , toString endX
                , toString endY
                , "L 0 0"
                ]
    in
        Svg.path
            [ d pathD
            ]



--
--        return path({
--          d,
--          style: {
--            fill: slice.score.color || ``, // set empty string to make iOS ignore fill
--          },
--        });


view : Size -> List Item -> (Item -> msg) -> Svg msg
view viewPortSize items onClick =
    let
        cumulativeRadians =
            0

        slices =
            items
                -- |> List.map (\item -> Svg.text_ [] [ Svg.text i.name ])
                |> List.map calculateSliceData
                |> Svg.g []
    in
        Svg.svg
            [ width "400px"
            , height "300px"
            , viewBox "-1 -1 2 2"
            , style "transform: rotate(-90deg)"
            ]
            [ slices
            ]
