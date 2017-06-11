module Pie exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias Item =
    { name : String
    , value : Float
    , color : String
    }


type alias Size =
    { width : Float
    , height : Float
    }


calculateCoordinates : Float -> ( Float, Float )
calculateCoordinates radians =
    let
        x =
            cos (radians)

        y =
            sin (radians)
    in
        ( x, y )


createSlicePath : ( Float, Float ) -> Float -> ( Float, Float ) -> String
createSlicePath startCoordinates largeArcFlag endCoordinates =
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


accumulateRadians : Float -> Float -> Float
accumulateRadians radians percent =
    radians + 2 * pi * percent


calculateLargeArcFlag : Float -> number
calculateLargeArcFlag percent =
    if percent > 0.5 then
        1
    else
        0


calculateSliceData : Float -> Item -> ( List (Svg msg), Float ) -> ( List (Svg msg), Float )
calculateSliceData total item ( paths, radians ) =
    let
        percent =
            item.value / total

        startCoordinates =
            calculateCoordinates radians

        cumulativeRadians =
            accumulateRadians radians percent

        endCoordinates =
            calculateCoordinates cumulativeRadians

        largeArcFlag =
            calculateLargeArcFlag percent

        pathD =
            createSlicePath startCoordinates largeArcFlag endCoordinates
    in
        ( (Svg.g []
            [ Svg.title []
                [ Svg.text item.name ]
            , Svg.path
                [ d pathD
                , fill item.color
                , title item.name
                ]
                []
            ]
          )
            :: paths
        , cumulativeRadians
        )


view : Size -> List Item -> (Item -> msg) -> Svg msg
view size items onClick =
    let
        total =
            List.foldl (\item sum -> item.value + sum) 0 items

        slices =
            List.foldl (calculateSliceData total) ( [], 0 ) items
                |> Tuple.first
    in
        Svg.svg
            [ width <| toString size.width
            , height <| toString size.height
            , viewBox "-1 -1 2 2"
            , style "transform: rotate(-90deg)"
            ]
            slices
