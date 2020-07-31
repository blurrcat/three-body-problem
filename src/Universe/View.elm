module Universe.View exposing
    ( Model
    , Msg(..)
    , getRandomUniverse
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onAnimationFrame, onKeyDown)
import Html exposing (Html)
import Json.Decode as Decode
import Math.Vector2 exposing (getX, getY, toRecord)
import Random
import RingBuffer
import String
import Svg exposing (circle, g, polyline, rect, svg)
import Svg.Attributes as SA
import Time exposing (Posix)
import Universe.Camera as Camera exposing (Camera)
import Universe.Model.Body exposing (Body, DT, G)
import Universe.Model.Universe as Universe
    exposing
        ( Universe
        , empty
        , getBodies
        , setDT
        , setG
        , setN
        )
import Universe.Random exposing (BodyParams, genUniverse)


type alias Model =
    { universe : Universe
    , paused : Bool
    , initialized : Bool
    , t : Posix
    , camera : Camera
    }


type Msg
    = Noop
    | Tick Posix
    | SetG Float
    | SetDT Float
    | SetN Int
    | KeyDown Camera.Command
    | TogglePaused
    | GetRandomUniverse G DT Int BodyParams
    | RandomUniverseArrived Universe


init : ( Model, Cmd Msg )
init =
    ( { universe = empty
      , paused = True
      , initialized = False
      , t = Time.millisToPosix 0
      , camera = Camera.camera size size
      }
    , Cmd.none
    )


getRandomUniverse : G -> DT -> Int -> BodyParams -> Msg
getRandomUniverse g dt n params =
    GetRandomUniverse g dt n params


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ universe } as model) =
    case msg of
        Noop ->
            ( model
            , Cmd.none
            )

        GetRandomUniverse g dt n params ->
            ( model
            , Random.generate RandomUniverseArrived (genUniverse g dt n params)
            )

        RandomUniverseArrived newUniverse ->
            ( { model | universe = newUniverse, initialized = True }
            , Cmd.none
            )

        TogglePaused ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        SetG g ->
            ( { model | universe = setG g universe }
            , Cmd.none
            )

        SetDT dt ->
            ( { model | universe = setDT dt universe }
            , Cmd.none
            )

        SetN n ->
            ( { model | universe = setN n universe }
            , Cmd.none
            )

        Tick t ->
            ( { model
                | universe = Universe.update universe
                , t = t
              }
            , Cmd.none
            )

        KeyDown cmd ->
            ( { model
                | camera = Camera.update cmd model.camera
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationSub =
            if not model.paused && model.initialized then
                onAnimationFrame Tick

            else
                Sub.none

        keyboardSub =
            onKeyDown (Decode.map KeyDown Camera.keyDecoder)
    in
    Sub.batch [ animationSub, keyboardSub ]


size : Float
size =
    100


view : Model -> List (Html msg)
view model =
    let
        box =
            [ 0, 0, size, size ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    [ svg [ SA.viewBox box ]
        [ rect
            [ SA.fill "#113"
            , SA.width "100%"
            , SA.height "100%"
            ]
            []
        , g
            [ Camera.toTransform model.camera ]
            (model.universe
                |> getBodies
                |> List.map viewBody
            )
        ]
    ]


viewBody : Body -> Html msg
viewBody { radious, position, positionHistory } =
    let
        { x, y } =
            toRecord position

        toPoint p =
            String.fromFloat (getX p) ++ "," ++ String.fromFloat (getY p)

        path =
            positionHistory
                |> RingBuffer.toList
                |> List.map toPoint
                |> String.join " "
    in
    g []
        [ circle
            [ SA.cx (String.fromFloat x)
            , SA.cy (String.fromFloat y)
            , SA.r (radious |> String.fromFloat)
            , SA.fillOpacity "0.8"
            , SA.fill "#ffffff"
            ]
            []
        , polyline
            [ SA.points path
            , SA.fill "none"
            , SA.stroke "#ffffff"
            , SA.strokeOpacity "0.2"
            , SA.strokeWidth (String.fromFloat (radious / 4))
            ]
            []
        ]
