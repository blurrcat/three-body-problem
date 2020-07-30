module Math.Vector2.Extra exposing
    ( fromTuple
    , origin
    , sum
    )

import Math.Vector2 as V exposing (Vec2)


origin : Vec2
origin =
    V.vec2 0 0


sum : List Vec2 -> Vec2
sum =
    List.foldl V.add origin


fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
    V.vec2 x y
