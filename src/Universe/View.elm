module Universe.View exposing
    ( Model
    , Msg(..)
    , fps
    , getRandomUniverse
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra exposing (avg)
import Math.Vector2 exposing (getX, getY, toRecord)
import Random
import RingBuffer exposing (RingBuffer)
import String
import Svg exposing (circle, g, polyline, rect, svg)
import Svg.Attributes as SA
import Universe.Camera as Camera exposing (Camera)
import Universe.Model.Body as Body exposing (Body, DT, G)
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
    , animationFrameDelta : RingBuffer Float
    , camera : Camera
    }


type Msg
    = Noop
    | Tick Float
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
      , animationFrameDelta = emptyAnimationFrameDelta
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
            ( { model
                | universe = newUniverse
                , initialized = True
                , camera = Camera.reset model.camera
                , animationFrameDelta = emptyAnimationFrameDelta
              }
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

        Tick delta ->
            ( { model
                | universe = Universe.update universe
                , animationFrameDelta = RingBuffer.push delta model.animationFrameDelta
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
                onAnimationFrameDelta Tick

            else
                Sub.none

        keyboardSub =
            onKeyDown (Decode.map KeyDown Camera.keyDecoder)
    in
    Sub.batch [ animationSub, keyboardSub ]


size : Float
size =
    100


fps : Model -> Float
fps { animationFrameDelta } =
    animationFrameDelta
        |> RingBuffer.toList
        |> avg
        |> Maybe.map ((/) 1000)
        |> Maybe.withDefault 0


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
viewBody b =
    let
        { x, y } =
            toRecord b.position

        toPoint p =
            [ String.fromFloat (getX p), ",", String.fromFloat (getY p), " " ]

        path =
            b
                |> Body.positionHistory
                |> List.concatMap toPoint
                |> String.join ""
    in
    g []
        [ circle
            [ SA.cx (String.fromFloat x)
            , SA.cy (String.fromFloat y)
            , SA.r (b.radious |> String.fromFloat)
            , SA.fillOpacity "0.8"
            , SA.fill "#ffffff"
            ]
            []
        , polyline
            [ SA.points path
            , SA.fill "none"
            , SA.stroke "#ffffff"
            , SA.strokeOpacity "0.2"
            , SA.strokeWidth (String.fromFloat (b.radious / 4))
            ]
            []
        ]


emptyAnimationFrameDelta : RingBuffer Float
emptyAnimationFrameDelta =
    RingBuffer.initialize 20 (always 0)
