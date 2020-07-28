module QuadTree exposing
    ( QuadTree
    , addPoint
    , map
    , takeWhile
    )

import QuadTree.Box as B exposing (Box, Point, Quadrant)


type QuadTree a
    = Branch Box a (Subtrees a)
    | EmptyLeaf Box
    | Leaf Box a


addPoint : Point -> a -> QuadTree a -> QuadTree a
addPoint p a t =
    case t of
        Branch b _ subtrees ->
            B.quad p b subtrees
                |> addPoint p a

        EmptyLeaf b ->
            Leaf b a

        Leaf b leafValue ->
            -- split into a branch and insert the point
            B.split b EmptyLeaf
                |> Branch b leafValue
                |> addPoint (B.getCenter b) leafValue
                |> addPoint p a


map : (a -> b) -> (Subtrees a -> b) -> QuadTree a -> QuadTree b
map mapLeaf mapBranch t =
    case t of
        Branch b _ subtrees ->
            Branch b
                (mapBranch subtrees)
                (B.mapQuadrant (map mapLeaf mapBranch) subtrees)

        EmptyLeaf b ->
            EmptyLeaf b

        Leaf b v ->
            Leaf b (mapLeaf v)



-- takeWhile : (a -> Bool) -> QuadTree a -> List a
-- takeWhile =
--     takeWhileInternal []
-- takeWhileInternal : List a -> (a -> Bool) -> QuadTree a -> List a
-- takeWhileInternal result pred t =
--     case t of
--         Branch _ v subtrees ->
--             let
--                 took =
--                     if pred v then
--                         subtrees
--                             |> B.mapQuadrant (takeWhileInternal [] pred)
--                             |> B.foldQuadrant List.append []
--                     else
--                         [ v ]
--             in
--             List.append took result
--         EmptyLeaf _ ->
--             result
--         Leaf _ v ->
--             v :: result


takeWhile : (Box -> a -> Bool) -> QuadTree a -> List a
takeWhile pred t =
    case t of
        Branch b v subtrees ->
            let
                result =
                    if pred b v then
                        subtrees
                            |> B.mapQuadrant (takeWhile pred)
                            |> B.foldQuadrant List.append []

                    else
                        [ v ]
            in
            result

        EmptyLeaf _ ->
            []

        Leaf _ v ->
            [ v ]



---- INTERNAL


type alias Subtrees a =
    Quadrant (QuadTree a)
