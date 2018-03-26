module Main exposing (..)

import Html as H exposing (..)
import Html.Events exposing (onClick)
import Universe exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svga exposing (..)
import Math.Vector2 exposing (toTuple)
import Time exposing (Time, millisecond)


type alias Model =
    { universe : Universe
    , paused : Bool
    }


type Msg
    = Noop
    | Tick Time
    | TogglePause


main : Program Never Model Msg
main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        bodies =
            [ Universe.body
                1.0
                ( 0.1, 5.01 )
                ( 21.0, 1.0 )
            , Universe.body
                1.0
                ( 5.1, 0.01 )
                ( 10.0, 15.0 )
            , Universe.body
                10.0
                ( -0.01, 0.1 )
                ( 50.0, 50.0 )
            ]
    in
        { universe = bang bodies 100 0.1
        , paused = True
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        Tick _ ->
            { model | universe = (tick model.universe) } ! []

        TogglePause ->
            { model | paused = (not model.paused) } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.paused then
        Time.every (50 * millisecond) Tick
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ viewUniverse model.universe
        , button [ onClick TogglePause ] [ H.text ("paused: " ++ (toString model.paused)) ]
        ]


viewUniverse : Universe -> Svg Msg
viewUniverse universe =
    svg [ viewBox "0 0 100 100", Svga.width "600px" ]
        (universe
            |> getBodies
            |> List.map viewBody
        )


viewBody : Body -> Svg Msg
viewBody { mass, position } =
    let
        ( x, y ) =
            (toTuple position)
    in
        circle [ cx (toString x), cy (toString y), r (mass |> sqrt |> toString), fill "#000000" ] []
