module Universe.Camera exposing
    ( Camera
    , Command
    , camera
    , keyDecoder
    , toTransform
    , update
    )

import Json.Decode as Decode
import Svg exposing (Attribute)
import Svg.Attributes exposing (transform)


type alias Camera =
    { x : Float
    , y : Float
    , s : Float
    , h : Float
    , w : Float
    }


camera : Float -> Float -> Camera
camera h w =
    { x = 0
    , y = 0
    , s = 1
    , h = h
    , w = w
    }


toTransform : Camera -> Attribute msg
toTransform c =
    transform (translate c ++ " " ++ scale c)


update : Command -> Camera -> Camera
update cmd c =
    case cmd of
        MoveLeft ->
            { c | x = c.x - step * c.w * c.s }

        MoveRight ->
            { c | x = c.x + step * c.w * c.s }

        MoveUp ->
            { c | y = c.y - step * c.h * c.s }

        MoveDown ->
            { c | y = c.y + step * c.h * c.s }

        ZoomIn ->
            { c | s = 1.1 * c.s }

        ZoomOut ->
            { c | s = 0.9 * c.s }

        Reset ->
            { c | x = 0, y = 0, s = 1 }

        _ ->
            c


keyDecoder : Decode.Decoder Command
keyDecoder =
    Decode.map toCommand (Decode.field "key" Decode.string)



-- INTERNAL


step : Float
step =
    0.02


type Command
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | ZoomIn
    | ZoomOut
    | Reset
    | Unknown


toCommand : String -> Command
toCommand s =
    case s of
        "k" ->
            MoveUp

        "w" ->
            MoveUp

        "ArrowUp" ->
            MoveUp

        "s" ->
            MoveDown

        "j" ->
            MoveDown

        "ArrowDown" ->
            MoveDown

        "a" ->
            MoveLeft

        "h" ->
            MoveLeft

        "ArrowLeft" ->
            MoveLeft

        "d" ->
            MoveRight

        "l" ->
            MoveRight

        "ArrowRight" ->
            MoveRight

        "z" ->
            ZoomIn

        "x" ->
            ZoomOut

        "r" ->
            Reset

        _ ->
            Unknown


translate : Camera -> String
translate { x, y } =
    "translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"


scale : Camera -> String
scale { w, h, s } =
    -- "scale(" ++ String.fromFloat s ++ ")"
    let
        e =
            (1 - s) * w / 2

        f =
            (1 - s) * h / 2

        args =
            [ s, 0.0, 0.0, s, e, f ]
                |> List.map String.fromFloat
                |> String.join ","
    in
    "matrix(" ++ args ++ ")"
