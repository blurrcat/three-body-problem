module Main exposing (..)

import Random as R exposing (..)
import Html as H exposing (..)
import Html.Attributes as Ha exposing (..)
import Html.Events exposing (onClick, onInput)
import Universe as U
import Universe.Physics exposing (G, DT)
import Universe.Random exposing (BodyParams)
import Math.Vector2 exposing (toTuple)


type alias Model =
    { universe : U.Model
    , bodyParams : BodyParams
    }


type Msg
    = Noop
    | Universe U.Msg
    | ChangeG Float
    | ChangeDt Float


main : Program Never Model Msg
main =
    H.program
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


init : ( Model, Cmd Msg )
init =
    let
        ( universe, cmd ) =
            U.init |> wrapUniverseState
    in
        { universe = universe
        , bodyParams =
            { massRange = ( 0.1, 0.5 )
            , velocityRange = ( -0.5, 0.5 )
            , positionRange = ( 0.0, 99.0 )
            }
        }
            ! [ cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ universe } as model) =
    case msg of
        Noop ->
            model ! []

        Universe uMsg ->
            let
                ( newUniverse, cmds ) =
                    (U.update uMsg model.universe) |> wrapUniverseState
            in
                { model | universe = newUniverse } ! [ cmds ]

        ChangeG ratio ->
            let
                innerUniverse =
                    universe.universe

                newInnerUniverse =
                    { innerUniverse
                        | g = innerUniverse.g * (1 + ratio)
                    }

                newUniverse =
                    { universe | universe = newInnerUniverse }
            in
                { model | universe = newUniverse } ! []

        ChangeDt ratio ->
            let
                innerUniverse =
                    universe.universe

                newInnerUniverse =
                    { innerUniverse
                        | dt = innerUniverse.dt * (1 + ratio)
                    }

                newUniverse =
                    { universe | universe = newInnerUniverse }
            in
                { model | universe = newUniverse } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    U.subscriptions (model.universe) |> wrapUniverseSubscriptions


view : Model -> Html Msg
view model =
    div [ Ha.class "mx-auto fit" ]
        [ div [ Ha.class "clearfix" ]
            [ model |> viewControls [ Ha.class "col col-3" ]
            , div [ Ha.class "col col-9" ] (U.view ( 100, 100 ) model.universe)
            ]
        ]


viewControls : List (H.Attribute Msg) -> Model -> Html Msg
viewControls attrs { universe, bodyParams } =
    let
        u =
            universe.universe
    in
        div attrs
            [ div [ Ha.class "mx1" ]
                [ -- Big Bang params
                  div []
                    [ p [ class "h2" ] [ H.text "Big Bang Params" ]
                    ]

                -- Realtime params
                , div
                    []
                    [ p [ class "h2" ] [ text "Realtime Params" ]
                    , (viewNumberInput "G" u.g ChangeG)
                    , (viewNumberInput "dt" u.dt ChangeDt)
                    ]

                -- controls
                , div
                    [ class "clearfix" ]
                    [ div [ class "col col-6" ]
                        [ button
                            [ onClick (U.togglePaused |> wrapUniverseMsg)
                            ]
                            [ text ("paused: " ++ (toString universe.paused)) ]
                        ]
                    , div [ class "col col-6" ]
                        [ button
                            [ onClick
                                ((U.getRandomUniverse
                                    u.g
                                    u.dt
                                    u.n
                                    bodyParams
                                 )
                                    |> wrapUniverseMsg
                                )
                            ]
                            [ text "Bang!" ]
                        ]
                    ]
                ]
            ]



-- viewNumberInput : String -> Float -> Msg -> Msg -> Html Msg


viewNumberInput : String -> Float -> (Float -> Msg) -> Html Msg
viewNumberInput name val msg =
    div []
        [ div []
            [ button [ onClick (msg -0.1) ] [ text "-" ]
            , span [ class "bold" ] [ text (name ++ ":") ]
            , button [ onClick (msg 0.1) ] [ text "+" ]
            ]
        , div []
            [ span
                [ class "bold" ]
                [ text (toString val) ]
            ]
        ]
