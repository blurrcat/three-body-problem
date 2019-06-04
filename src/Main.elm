module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
            { massRange = ( 0.01, 0.5 )
            , velocityRange = ( -1.8, 1.8 )
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
    div [ class "pure-g" ]
        [ model |> viewControls [ class "pure-u-1-4" ]
        , div [ class "pure-u-3-4" ] (U.view ( 100, 100 ) model.universe)
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
        [ div [ style "padding" "0.5em" ]
            [ Html.form
                [ class "pure-form pure-form-stacked"
                ]
                [ fieldset []
                    [ legend []
                        [ text "Big Bang Params" ]
                    , viewNumberInput "N" "# of bodies" model.fieldN model.errorN ChangeN
                    ]
                ]
            , div []
                [ div [ class "pure-u-1-2" ]
                    [ button
                        [ class "pure-button"
                        , onClick
                            (U.getRandomUniverse
                                u.g
                                u.dt
                                u.n
                                bodyParams
                                |> wrapUniverseMsg
                            )
                        ]
                        [ text "Bang!" ]
                    ]
                ]
            , Html.form
                [ class "pure-form pure-form-stacked"
                , style "margin-top" "0.5em"
                ]
                [ fieldset []
                    [ legend []
                        [ text "Realtime Params" ]
                    , viewNumberInput
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

            -- controls
            , div
                [ class "pure-g" ]
                [ div [ class "pure-u-1-2" ]
                    [ button
                        [ class "pure-button"
                        , onClick (U.TogglePaused |> wrapUniverseMsg)
                        , disabled (not universe.initialized)
                        ]
                        [ text playBtnText ]
                    ]
                ]
            ]
        ]


viewNumberInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewNumberInput name desc val error msg =
    div [ class "pure-control-group" ]
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
