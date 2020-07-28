module QuadTree.Box exposing
    ( Box
    , Quadrant
    , boundingBox
    , box
    , foldQuadrant
    , getCenter
    , getSize
    , listQuadrant
    , mapQuadrant
    , quad
    , updateQuadrant
    , split
    )

import Point exposing (Point)


type alias Quadrant a =
    { southwest : a
    , northwest : a
    , southeast : a
    , northeast : a
    }


type Box
    = Box Point Float


box : Point -> Float -> Box
box center size =
    Box center size


boundingBox : Point -> Point -> Box
boundingBox ( x0, y0 ) ( x1, y1 ) =
    let
        avg n1 n2 =
            (n1 + n2) / 2.0

        center =
            ( avg x0 x1, avg y0 y1 )

        w =
            x1 - x0

        h =
            y1 - y0

        size =
            if h >= w then
                h

            else
                w
    in
    box center size


getSize : Box -> Float
getSize (Box _ s) =
    s


getCenter : Box -> Point
getCenter (Box c _) =
    c


quad : Point -> Box -> Quadrant a -> a
quad ( px, py ) (Box ( cx, cy ) _) quads =
    case ( px < cx, py < cy ) of
        ( True, True ) ->
            quads.southwest

        ( True, False ) ->
            quads.northwest

        ( False, True ) ->
            quads.southeast

        ( False, False ) ->
            quads.northeast


split : Box -> (Box -> a) -> Quadrant a
split (Box ( cx, cy ) size) createA =
    let
        newSize =
            size / 2.0

        create center =
            box center newSize |> createA
    in
    { southwest = create ( cx - newSize, cy - newSize )
    , northwest = create ( cx - newSize, cy + newSize )
    , southeast = create ( cx + newSize, cy - newSize )
    , northeast = create ( cx + newSize, cy + newSize )
    }


mapQuadrant : (a -> b) -> Quadrant a -> Quadrant b
mapQuadrant f q =
    { southwest = f q.southwest
    , northwest = f q.northwest
    , southeast = f q.southeast
    , northeast = f q.northeast
    }


updateQuadrant : Point -> Box -> (a -> a) -> Quadrant a -> Quadrant a
updateQuadrant ( px, py ) (Box ( cx, cy ) _) f q =
    case ( px < cx, py < cy ) of
        ( True, True ) ->
            { q | southwest = f q.southwest }

        ( True, False ) ->
            { q | northwest = f q.northwest }

        ( False, True ) ->
            { q | southeast = f q.southeast }

        ( False, False ) ->
            { q | northeast = f q.northeast }


foldQuadrant : (a -> b -> b) -> b -> Quadrant a -> b
foldQuadrant f b q =
    listQuadrant q
        |> List.foldr f b


listQuadrant : Quadrant a -> List a
listQuadrant q =
    [ q.southwest
    , q.northwest
    , q.southeast
    , q.northeast
    ]
