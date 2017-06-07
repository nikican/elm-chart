module Main exposing (..)

import Html exposing (..)
import Nash
import Pie exposing (Item, Size)


type alias Flags =
    { values : List ( Int, Float )
    }



--
-- APP


main : Program Flags Model Msg
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { values : List ( Int, Float )
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { values = flags.values
    }
        ! []



-- UPDATE


type Msg
    = NoOp
    | PieClick Pie.Item


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Sub.batch
--     [ Window.resizes WindowResize ]
-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


staticSize : Nash.Size
staticSize =
    { width = 800, height = 400 }


pieSize : Pie.Size
pieSize =
    { width = 800, height = 400 }


oneitem : Pie.Item
oneitem =
    { name = "Technology"
    , value = 45
    }


twoitem : Pie.Item
twoitem =
    { name = "Oil"
    , value = 60
    }


view : Model -> Html Msg
view model =
    div []
        [ Pie.view pieSize [ oneitem ] PieClick
        , Nash.plot staticSize model.values
        ]
