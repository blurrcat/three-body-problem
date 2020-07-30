module BHTree exposing (BHTree, bhtree, massCloseTo)

import Math.Vector2 as V
import Math.Vector2.Extra as VE exposing (origin)
import BHTree.Box as Box exposing (Box)
import BHTree.Region as Region exposing (Regions)
import Universe.Model.Body as Body exposing (Body, CenterOfMass)


{-| 2-D Barnes-Hub Tree
-}
type BHTree
    = Branch Box CenterOfMass (Regions BHTree)
    | EmptyLeaf Box
    | Leaf Box Body


bhtree : List Body -> BHTree
bhtree bodies =
    let
        box =
            boundingBox bodies
    in
    List.foldl addBody (EmptyLeaf box) bodies
        |> summarize aggregateCenterOfMass


massCloseTo : Float -> Body -> BHTree -> ( List CenterOfMass, List Body )
massCloseTo theta me t =
    let
        isClose box centerOfMass =
            (Box.size box / V.distance me.position centerOfMass.position) >= theta
    in
    case t of
        Branch box centerOfMass children ->
            if isClose box centerOfMass then
                children
                    |> Region.toList
                    |> List.foldl
                        (\r ( centers, bodies ) ->
                            let
                                ( newCenters, newBodies ) =
                                    massCloseTo theta me r
                            in
                            ( List.append newCenters centers
                            , List.append newBodies bodies
                            )
                        )
                        ( [], [] )

            else
                ( [ centerOfMass ], [] )

        EmptyLeaf _ ->
            ( [], [] )

        Leaf _ body ->
            ( [], if me.id == body.id then [] else [ body ] )



-- INTERNAL

boundingBox : List Body -> Box
boundingBox bodies =
    let
        xs =
            List.map (.position >> V.getX) bodies

        ys =
            List.map (.position >> V.getY) bodies

        getCorner f =
            Maybe.map2 V.vec2 (f xs) (f ys)
                |> Maybe.withDefault origin

        lowerLeft =
            getCorner List.minimum

        topRight =
            getCorner List.maximum
    in
    Box.boundingBox lowerLeft topRight


addBody : Body -> BHTree -> BHTree
addBody b t =
    case t of
        Branch box centerOfMass children ->
            let
                direction =
                    Box.direction b.position box

                newChildren =
                    Region.mapRegion direction (addBody b) children
            in
            Branch box centerOfMass newChildren

        EmptyLeaf box ->
            Leaf box b

        Leaf box body ->
            box
                |> Box.subBoxes
                |> Region.map EmptyLeaf
                |> Branch box defaultCenterOfMass
                |> addBody body
                |> addBody b


defaultCenterOfMass : CenterOfMass
defaultCenterOfMass =
    Body.centerOfMass 0 origin


summarize : (Regions BHTree -> CenterOfMass) -> BHTree -> BHTree
summarize f t =
    case t of
        Branch box _ children ->
            let
                newChildren =
                    Region.map (summarize f) children
            in
            Branch box (f newChildren) newChildren

        _ ->
            t


aggregateCenterOfMass : Regions BHTree -> CenterOfMass
aggregateCenterOfMass r =
    let
        centers =
            r
                |> Region.toList
                |> List.filterMap centerOfMassOfNode

        totalMass =
            centers
                |> List.map .mass
                |> List.sum

        center =
            centers
                |> List.map
                    (\c -> V.scale (c.mass / totalMass) c.position)
                |> VE.sum
    in
    Body.centerOfMass totalMass center


centerOfMassOfNode : BHTree -> Maybe CenterOfMass
centerOfMassOfNode t =
    case t of
        Branch _ c _ ->
            Just c

        EmptyLeaf _ ->
            Nothing

        Leaf _ b ->
            Just (Body.centerOfMass b.mass b.position)
