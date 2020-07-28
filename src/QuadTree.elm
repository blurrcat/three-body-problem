module QuadTree exposing
    ( QuadTree
    , addPoint
    , empty
    , getValue
    , summarize
    , takeWhile
    , map
    )

import Point exposing (Point)
import QuadTree.Box as B exposing (Box, Quadrant)


type QuadTree a
    = Branch Box a (Subtrees a)
    | EmptyLeaf Box
    | Leaf Box a

empty : Box -> QuadTree a
empty b =
    EmptyLeaf b


getValue : QuadTree a -> Maybe a
getValue t =
    case t of
        Branch _ a _ ->
            Just a

        EmptyLeaf _ ->
            Nothing

        Leaf _ a ->
            Just a


addPoint : Point -> a -> QuadTree a -> QuadTree a
addPoint p a t =
    case t of
        Branch b v subtrees ->
            subtrees
            |> B.updateQuadrant p b (addPoint p a)
            |> Branch b v

        EmptyLeaf b ->
            Leaf b a

        Leaf b leafValue ->
            B.split b EmptyLeaf
                |> Branch b leafValue
                |> addPoint (B.getCenter b) leafValue
                |> addPoint p a


summarize : (Subtrees a -> a) -> QuadTree a -> QuadTree a
summarize f t =
    case t of
        Branch b _ subtrees ->
            let
                newSubtrees =
                    B.mapQuadrant (summarize f) subtrees
            in
            Branch b (f newSubtrees) newSubtrees

        _ ->
            t


map :(a->b) -> QuadTree a -> QuadTree b
map f t =
    case t of
        Branch b v subtrees ->
            Branch b (f v)
                (B.mapQuadrant (map f) subtrees)
        EmptyLeaf b ->
            EmptyLeaf b

        Leaf b v ->
            Leaf b (f v)



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
