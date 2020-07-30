module BHTree.Box exposing
    ( Box
    , boundingBox
    , box
    , center
    , direction
    , size
    , subBoxes
    )

import Math.Vector2 as V exposing (Vec2)
import BHTree.Region as R exposing (Direction(..), Regions)


type Box
    = Box Vec2 Float


box : Vec2 -> Float -> Box
box c s =
    Box c s


boundingBox : Vec2 -> Vec2 -> Box
boundingBox bottomLeft topRight =
    let
        c =
            V.add bottomLeft topRight
                |> V.scale 0.5

        delta =
            V.sub topRight bottomLeft

        w =
            V.getX delta

        h =
            V.getY delta

        s =
            if h >= w then
                h

            else
                w
    in
    box c s


size : Box -> Float
size (Box _ s) =
    s


center : Box -> Vec2
center (Box c _) =
    c


direction : Vec2 -> Box -> Direction
direction p (Box c _) =
    let
        delta =
            V.sub p c

        dx =
            V.getX delta

        dy =
            V.getY delta
    in
    case ( dx < 0, dy < 0 ) of
        ( True, True ) ->
            SW

        ( True, False ) ->
            NW

        ( False, True ) ->
            SE

        ( False, False ) ->
            NE


subBoxes : Box -> Regions Box
subBoxes (Box c s) =
    let
        newSize =
            s / 2.0

        subBox dx dy =
            box (V.add c (V.vec2 dx dy)) newSize
    in
    R.regions
        (\d ->
            case d of
                SW ->
                    subBox -newSize -newSize

                NW ->
                    subBox -newSize newSize

                SE ->
                    subBox newSize -newSize

                NE ->
                    subBox newSize newSize
        )
