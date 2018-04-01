module Universe
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , view
        , subscriptions
        , getRandomUniverse
        , togglePaused
        )

import Html exposing (..)
import Random
import Svg exposing (svg, rect, circle)
import Svg.Attributes as Svga exposing (..)
import Math.Vector2 exposing (toTuple)
import Universe.Physics exposing (..)
import Universe.Random exposing (genUniverse, BodyParams)
import Time exposing (Time)
import AnimationFrame


type alias Model =
    { universe : Universe
    , paused : Bool
    , initialized : Bool
    }


type Msg
    = Noop
    | Tick Time
    | SetG Float
    | SetDT Float
    | SetN Int
    | TogglePaused
    | GetRandomUniverse G DT Int BodyParams
    | RandomUniverseArrived Universe


init : ( Model, Cmd Msg )
init =
    { universe = empty
    , paused = True
    , initialized = False
    }
        ! []


getRandomUniverse : G -> DT -> Int -> BodyParams -> Msg
getRandomUniverse g dt n params =
    GetRandomUniverse g dt n params


togglePaused : Msg
togglePaused =
    TogglePaused


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ universe } as model) =
    case msg of
        Noop ->
            model ! []

        GetRandomUniverse g dt n params ->
            model ! [ Random.generate RandomUniverseArrived (genUniverse g dt n params) ]

        RandomUniverseArrived universe ->
            { model | universe = universe, initialized = True } ! []

        TogglePaused ->
            { model | paused = not model.paused } ! []

        SetG g ->
            { model | universe = (setG g universe) } ! []

        SetDT dt ->
            { model | universe = (setDT dt universe) } ! []

        SetN n ->
            { model | universe = (setN n universe) } ! []

        Tick _ ->
            { model | universe = (tick model.universe) } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.paused && model.initialized then
        AnimationFrame.times Tick
    else
        Sub.none


view : ( Int, Int ) -> Model -> List (Html msg)
view ( width, height ) model =
    let
        box =
            [ 0, 0, width, height ]
                |> List.map toString
                |> String.join " "
    in
        [ rect [ Svga.fill "black", Svga.width "100%", Svga.height "100%" ] []
            :: (model.universe
                    |> getBodies
                    |> List.map viewBody
               )
            |> svg
                [ viewBox box ]
        ]


viewBody : Body -> Html msg
viewBody { mass, position } =
    let
        ( x, y ) =
            (toTuple position)
    in
        circle [ cx (toString x), cy (toString y), r (mass |> sqrt |> toString), fill "#ffffff" ] []
