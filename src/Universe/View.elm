module Universe.View exposing
    ( Model
    , Msg(..)
    , getRandomUniverse
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onAnimationFrame)
import Html exposing (..)
import Math.Vector2 exposing (getX, getY, toRecord)
import Random
import RingBuffer
import String
import Svg exposing (circle, g, polyline, rect, svg)
import Svg.Attributes as Svga exposing (..)
import Time exposing (Posix)
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
    }


type Msg
    = Noop
    | Tick Posix
    | SetG Float
    | SetDT Float
    | SetN Int
    | TogglePaused
    | GetRandomUniverse G DT Int BodyParams
    | RandomUniverseArrived Universe


init : ( Model, Cmd Msg )
init =
    ( { universe = empty
      , paused = True
      , initialized = False
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

        Tick _ ->
            ( { model | universe = Universe.update universe }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.paused && model.initialized then
        onAnimationFrame Tick

    else
        Sub.none


view : ( Int, Int ) -> Model -> List (Html msg)
view ( width, height ) model =
    let
        box =
            [ 0, 0, width, height ]
                |> List.map String.fromInt
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
viewBody { radious, mass, position, positionHistory } =
    let
        { x, y } =
            toRecord position

        toPoint p =
            String.fromFloat (getX p) ++ "," ++ String.fromFloat (getY p)

        path =
            -- positionHistory
            --     |> Queue.map toPoint
            --     |> Queue.toList
            --     |> String.join " "
            positionHistory
                |> RingBuffer.toList
                |> List.map toPoint
                |> String.join " "
    in
    g []
        [ circle
            [ cx (String.fromFloat x)
            , cy (String.fromFloat y)
            , r (radious |> String.fromFloat)
            , fillOpacity "0.8"
            , fill "#ffffff"
            ]
            []
        , polyline
            [ points path
            , fill "none"
            , stroke "#ffffff"
            , strokeOpacity "0.2"
            , strokeWidth (String.fromFloat (radious / 4))
            ]
            []
        ]
