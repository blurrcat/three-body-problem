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
import Json.Decode as Decode
import Svg exposing (svg, rect, circle)
import Svg.Events exposing (on)
import Svg.Attributes as Svga exposing (..)
import Mouse
import Math.Vector2 exposing (toTuple)
import Universe.Physics exposing (..)
import Universe.Random exposing (genUniverse, BodyParams)
import Time exposing (Time)
import AnimationFrame


type alias Drag =
    { start : Mouse.Position
    , current : Mouse.Position
    }


type alias Model =
    { universe : Universe
    , paused : Bool
    , initialized : Bool
    , drag : Maybe Drag
    , offset : Mouse.Position
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
    | DragStart Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position


init : ( Model, Cmd Msg )
init =
    { universe = empty
    , paused = True
    , initialized = False
    , drag = Nothing
    , offset = Mouse.Position 0 0
    }
        ! []


getRandomUniverse : G -> DT -> Int -> BodyParams -> Msg
getRandomUniverse g dt n params =
    GetRandomUniverse g dt n params


togglePaused : Msg
togglePaused =
    TogglePaused


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ universe, drag } as model) =
    case msg of
        Noop ->
            model ! []

        GetRandomUniverse g dt n params ->
            model ! [ Random.generate RandomUniverseArrived (genUniverse g dt n params) ]

        RandomUniverseArrived universe ->
            { model
                | universe = universe
                , initialized = True
                , offset = Mouse.Position 0 0
            }
                ! []

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

        DragStart pos ->
            { model | drag = (Just (Drag pos pos)) } ! []

        DragAt pos ->
            { model
                | drag = drag |> Maybe.map (\{ start } -> Drag start pos)
                , offset = getOffset model
            }
                ! []

        DragEnd _ ->
            { model | drag = Nothing } ! []


getOffset : Model -> Mouse.Position
getOffset { drag, offset } =
    case drag of
        Nothing ->
            offset

        Just { start, current } ->
            Mouse.Position
                (offset.x + current.x - start.x)
                (offset.y + current.y - start.y)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        afSub =
            if not model.paused && model.initialized then
                AnimationFrame.times Tick
            else
                Sub.none

        dragSub =
            case model.drag of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
    in
        Sub.batch [ afSub, dragSub ]


view : ( Int, Int ) -> Model -> List (Html Msg)
view ( width, height ) model =
    let
        box =
            [ 0, 0, width, height ]
                |> List.map toString
                |> String.join " "
    in
        [ rect
            [ Svga.fill "black"
            , Svga.width "100%"
            , Svga.height "100%"
            ]
            []
            :: (model.universe
                    |> getBodies
                    |> List.map (viewBody model.offset)
               )
            |> svg
                [ viewBox box
                , onMouseDown
                ]
        ]


viewBody : Mouse.Position -> Body -> Html msg
viewBody offset { mass, position } =
    let
        ( px, py ) =
            toTuple position

        x =
            px + 0.005 * (toFloat offset.x)

        y =
            py + 0.005 * (toFloat offset.y)
    in
        circle
            [ cx (toString x)
            , cy (toString y)
            , r (mass |> sqrt |> toString)
            , fill "#ffffff"
            , fillOpacity "0.8"
            ]
            []


onMouseDown : Svg.Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)
