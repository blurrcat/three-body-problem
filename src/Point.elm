module Point exposing
    ( Point
    , fromVec
    , toVec
    )

import Math.Vector2 as V exposing (Vec2)


type alias Point =
    ( Float, Float )


toVec : Point -> Vec2
toVec ( x, y ) =
    V.vec2 x y


fromVec : Vec2 -> Point
fromVec v =
    ( V.getX v, V.getY v )
