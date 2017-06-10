module Pie exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import String exposing (join)


type alias Item =
    { name : String
    , value : Float
    , color : String
    }


type alias Size =
    { width : Float
    , height : Float
    }


getCoordinates : Float -> ( Float, Float )
getCoordinates radians =
    let
        x =
            cos (radians)

        y =
            sin (radians)
    in
        ( x, y )


calculateSliceData : Float -> Item -> ( List (Svg msg), Float ) -> ( List (Svg msg), Float )
calculateSliceData total item ( paths, radians ) =
    let
        percent =
            item.value / total

        startCoordinates =
            getCoordinates radians

        cumulativeRadians =
            radians + 2 * pi * percent

        endCoordinates =
            getCoordinates cumulativeRadians

        largeArcFlag =
            if percent > 0.5 then
                1
            else
                0

        pathD =
            String.join " "
                [ "M"
                , startCoordinates |> Tuple.first |> toString
                , startCoordinates |> Tuple.second |> toString
                , "A 1 1 0"
                , largeArcFlag |> toString
                , "1"
                , endCoordinates |> Tuple.first |> toString
                , endCoordinates |> Tuple.second |> toString
                , "L 0 0"
                ]
    in
        ( (Svg.path
            [ d pathD
            , fill item.color
            ]
            []
          )
            :: paths
        , cumulativeRadians
        )


view : Size -> List Item -> (Item -> msg) -> Svg msg
view viewPortSize items onClick =
    let
        total =
            List.foldl (\item sum -> item.value + sum) 0 items

        slices =
            List.foldl (calculateSliceData total) ( [], 0 ) items
                |> Tuple.first
    in
        Svg.svg
            [ width <| toString viewPortSize.width
            , height <| toString viewPortSize.height
            , viewBox "-1 -1 2 2"
            , style "transform: rotate(-90deg)"
            ]
            slices
