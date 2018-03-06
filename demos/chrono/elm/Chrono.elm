module Chrono exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


type alias Model =
    { elapsed : Float
    , running : Bool
    , laps : List Float
    }


type Msg
    = Start
    | Stop
    | Reset
    | Lap
    | Tick Time


initialState : Model
initialState =
    { elapsed = 0.0
    , running = False
    , laps = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialState, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (10 * Time.millisecond) Tick
    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | elapsed = 0 }, Cmd.none )

        Start ->
            ( { model | running = True }, Cmd.none )

        Stop ->
            ( { model | running = False }, Cmd.none )

        Tick time ->
            ( { model | elapsed = model.elapsed + (10 * Time.millisecond) }, Cmd.none )

        Lap ->
            ( { model | elapsed = 0, laps = model.elapsed :: model.laps }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ h1 [] [ text "Cronômetro" ] ]
        , div [] [ viewLap model.elapsed ]
        , div [] (viewLaps model)
        , button [ onClick Start ] [ text "Inicia" ]
        , button [ onClick Stop ] [ text "Para" ]
        , button [ onClick Reset ] [ text "Zera" ]
        , button [ onClick Lap ] [ text "Lap" ]
        ]


viewLaps : Model -> List (Html Msg)
viewLaps model =
    List.map viewLap model.laps


viewLap : Float -> Html Msg
viewLap lap =
    div [] [ text "Lap: ", text (toString lap) ]


main : Program Never Model Msg
main =
    program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
