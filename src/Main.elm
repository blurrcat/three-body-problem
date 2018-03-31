module Main exposing (..)

import Random as R exposing (..)
import Html as H exposing (..)
import Html.Attributes as Ha exposing (..)
import Html.Events exposing (onClick, onInput)
import Universe as U
import Math.Vector2 exposing (toTuple)


type alias Model =
    { universe : U.Model
    }


type Msg
    = Noop
    | Universe U.Msg


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
        }
            ! [ cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        Universe uMsg ->
            let
                ( newUniverse, cmds ) =
                    (U.update uMsg model.universe) |> wrapUniverseState
            in
                { model | universe = newUniverse } ! [ cmds ]


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
viewControls attrs model =
    div attrs
        [ div [ Ha.class "mx1" ]
            [ button [ onClick (U.togglePaused |> wrapUniverseMsg) ] [ H.text ("paused: " ++ (toString model.universe.paused)) ]

            -- , button [ onClick (u.getRandomUniverse  |> wrapUniverseMsg) ] [ H.text "ignite" ]
            ]
        ]
