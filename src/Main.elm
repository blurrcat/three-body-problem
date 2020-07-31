module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
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
update msg ({ universe } as model) =
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
        , div [ style "flex" "3" ] (U.view ( 100, 100 ) model.universe)
        ]


viewControlPane : List (Attribute Msg) -> Model -> Html Msg
viewControlPane attrs model =
    div
        (attrs
            ++ [ style "flex-direction" "column"
               , style "justify-content" "space-between"
               ]
        )
        [ viewControls [] model
        , viewStats [] model
        ]


viewControls : List (Attribute Msg) -> Model -> Html Msg
viewControls attrs ({ universe, bodyParams } as model) =
    let
        u =
            universe.universe

        playBtnText =
            if universe.paused then
                "Go!"

            else
                "pause"
    in
    div attrs
        [ boxWithTitle "Big Bang Parameters"
            []
            [ viewNumberInput "N" "# of bodies" model.fieldN model.errorN ChangeN
            , viewButton
                False
                (U.getRandomUniverse
                    u.g
                    u.dt
                    u.n
                    bodyParams
                    |> wrapUniverseMsg
                )
                "Bang!"
            ]
        , boxWithTitle
            "Real Time Parameters"
            []
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

        -- controls
        , div
            []
            [ viewButton
                (not universe.initialized)
                (wrapUniverseMsg U.TogglePaused)
                playBtnText
            ]
        ]


viewStats : List (Attribute Msg) -> Model -> Html Msg
viewStats attrs model =
    boxWithTitle "Stats" [] []


boxWithTitle : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
boxWithTitle title attrs children =
    (legend [] [ text title ] :: children)
        |> fieldset [ marginBottom ]


viewNumberInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewNumberInput name desc val error msg =
    div
        [ marginBottom
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


viewButton : Bool -> Msg -> String -> Html Msg
viewButton disabled_ msg name =
    button
        [ onClick msg
        , marginBottom
        , disabled disabled_
        , class "pure-button"
        ]
        [ text name ]


marginBottom : Attribute Msg
marginBottom =
    style "margin-top" "1em"
