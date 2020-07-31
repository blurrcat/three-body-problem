module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (avg)
import Math.Vector2 as V
import Round
import String
import Universe.Random exposing (BodyParams)
import Universe.View as U


type alias Model =
    { universe : U.Model
    , bodyParams : BodyParams
    , fieldN : String
    , errorN : String
    , fieldG : String
    , errorG : String
    , fieldDT : String
    , errorDT : String
    }


type Msg
    = Noop
    | Universe U.Msg
    | ChangeG String
    | ChangeDt String
    | ChangeN String


main: Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


wrapUniverseMsg : U.Msg -> Msg
wrapUniverseMsg msg =
    Universe msg


wrapUniverseSubscriptions : Sub U.Msg -> Sub Msg
wrapUniverseSubscriptions sub =
    sub |> Sub.map wrapUniverseMsg


wrapUniverseState : ( U.Model, Cmd U.Msg ) -> ( U.Model, Cmd Msg )
wrapUniverseState ( universe, cmd ) =
    ( universe, cmd |> Cmd.map wrapUniverseMsg )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( universe, cmd ) =
            U.init |> wrapUniverseState
    in
    ( { universe = universe
      , bodyParams =
            { massRange = ( 0.0005, 0.5 )
            , velocityRange = ( -0.02, 0.02 )
            , positionRange = ( 0.0, 99.0 )
            }
      , fieldN = String.fromInt universe.universe.n
      , errorN = ""
      , fieldG = String.fromFloat universe.universe.g
      , errorG = ""
      , fieldDT = String.fromFloat universe.universe.dt
      , errorDT = ""
      }
    , cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model
            , Cmd.none
            )

        Universe uMsg ->
            let
                ( newUniverse, cmds ) =
                    U.update uMsg model.universe |> wrapUniverseState
            in
            ( { model | universe = newUniverse }
            , cmds
            )

        ChangeG raw ->
            case String.toFloat raw of
                Just g ->
                    update
                        (wrapUniverseMsg (U.SetG g))
                        { model | fieldG = raw, errorG = "" }

                Nothing ->
                    ( { model | fieldG = raw, errorG = "invalid number" }
                    , Cmd.none
                    )

        ChangeDt raw ->
            case String.toFloat raw of
                Just dt ->
                    update
                        (wrapUniverseMsg (U.SetDT dt))
                        { model | fieldDT = raw, errorDT = "" }

                Nothing ->
                    ( { model | fieldDT = raw, errorDT = "invalid number" }
                    , Cmd.none
                    )

        ChangeN raw ->
            case String.toInt raw of
                Just n ->
                    update
                        (wrapUniverseMsg (U.SetN n))
                        { model | fieldN = raw, errorN = "" }

                Nothing ->
                    ( { model | fieldN = raw, errorN = "invalid number" }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    U.subscriptions model.universe |> wrapUniverseSubscriptions


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "max-width" "1200px"
        , style "margin" "0 auto"
        ]
        [ viewControlPane
            [ style "flex" "1"
            , style "padding" "0.5em"
            ]
            model
        , div [ style "flex" "3" ] (U.view model.universe)
        ]


viewControlPane : List (Attribute Msg) -> Model -> Html Msg
viewControlPane attrs model =
    div
        (attrs
            ++ [ style "flex-direction" "column"
               , style "justify-content" "space-between"
               ]
        )
        [ viewControls model
        , viewStats model
        , viewHelp
        ]


viewControls : Model -> Html Msg
viewControls ({ universe, bodyParams } as model) =
    let
        u =
            universe.universe

        playBtnText =
            if universe.paused then
                "Go!"

            else
                "Pause"
    in
    div []
        [ boxWithTitle "Big Bang Parameters"
            [ viewNumberInput "N" "# of bodies" model.fieldN model.errorN ChangeN
            , div
                [ style "display" "flex"
                , style "justify-content" "space-around"
                ]
                [ viewButton
                    [ onClick
                        (U.getRandomUniverse
                            u.g
                            u.dt
                            u.n
                            bodyParams
                            |> wrapUniverseMsg
                        )
                    ]
                    "Bang!"
                , viewButton
                    [ disabled (not universe.initialized)
                    , onClick (wrapUniverseMsg U.TogglePaused)
                    , class "pure-button-primary"
                    ]
                    playBtnText
                ]
            ]
        , boxWithTitle
            "Real Time Parameters"
            [ viewNumberInput
                "G"
                "gravitational constant"
                model.fieldG
                model.errorG
                ChangeG
            , viewNumberInput
                "DT"
                "speed of time"
                model.fieldDT
                model.errorDT
                ChangeDt
            ]
        ]


viewStats : Model -> Html Msg
viewStats { universe } =
    let
        u =
            universe.universe

        masses =
            u.bodies |> List.map .mass

        velocities =
            u.bodies |> List.map (.velocity >> V.length)

        summarizeDist xs f =
            f xs |> Maybe.map ((*) 100 >> Round.round 2) |> Maybe.withDefault "N/A"
    in
    boxWithTitle "Stats"
        [ keyValueRow "Epoch" (u.epoch |> String.fromInt)
        , keyValueRow "# of Bodies" (u.bodies |> List.length |> String.fromInt)
        , keyValueRow "Min Mass" (summarizeDist masses List.minimum)
        , keyValueRow "Avg Mass" (summarizeDist masses avg)
        , keyValueRow "Max Mass" (summarizeDist masses List.maximum)
        , keyValueRow "Min Velocity" (summarizeDist velocities List.minimum)
        , keyValueRow "Avg Velocity" (summarizeDist velocities avg)
        , keyValueRow "Max Velocity" (summarizeDist velocities List.maximum)
        ]


boxWithTitle : String -> List (Html Msg) -> Html Msg
boxWithTitle title children =
    (legend [ style "padding" "0 0.25em" ] [ text title ] :: children)
        |> fieldset [ marginTop ]


viewNumberInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewNumberInput name desc val error msg =
    div
        [ marginTop
        ]
        [ label [ for name ]
            [ b [] [ text name ]
            , span
                [ style "margin-left" "0.5em"
                , style "font-size" "90%"
                ]
                [ text desc ]
            ]
        , input
            [ id name
            , value val
            , onInput msg
            ]
            []
        , span [ class "red" ] [ text error ]
        ]


viewButton : List (Attribute Msg) -> String -> Html Msg
viewButton attrs name =
    button
        ([ marginTop
         , class "pure-button"
         ]
            ++ attrs
        )
        [ text name ]


marginTop : Attribute Msg
marginTop =
    style "margin-top" "1em"


viewHelp : Html Msg
viewHelp =
    boxWithTitle "Camera"
        [ keyValueRow "Move" "Arrows or w/a/s/d"
        , keyValueRow "Zoom" "z/x"
        , keyValueRow "Reset" "r"
        ]


keyValueRow : String -> String -> Html Msg
keyValueRow key help =
    div [ class "space-between" ]
        [ span [] [ text key ]
        , span [] [ text help ]
        ]
