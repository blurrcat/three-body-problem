module Universe.Model.Universe exposing
    ( Universe
    , bang
    , empty
    , getBodies
    , setDT
    , setG
    , setN
    , update
    )

import Math.Vector2 as V
import Point as P
import QuadTree as Q exposing (QuadTree)
import QuadTree.Box as B exposing (Box)
import Set
import Universe.Model.Body as Body
    exposing
        ( Body
        , BodyDist
        , DT
        , Force
        , G
        )


type alias Universe =
    -- TODO: change to Array for better performance
    { bodies : List Body
    , g : G
    , dt : DT
    , n : Int
    , epoch : Int
    }


getBodies : Universe -> List Body
getBodies universe =
    universe.bodies


setG : G -> Universe -> Universe
setG g universe =
    { universe | g = g }


setDT : DT -> Universe -> Universe
setDT dt universe =
    { universe | dt = dt }


setN : Int -> Universe -> Universe
setN n universe =
    { universe | n = n }


bang : Int -> G -> DT -> List Body -> Universe
bang n g dt bodies =
    let
        bodiesWithId =
            bodies
                |> List.indexedMap (\id body -> { body | id = id })
    in
    { bodies = bodiesWithId
    , g = g
    , dt = dt
    , n = n
    , epoch = 0
    }


getBox : Universe -> Box
getBox { bodies } =
    let
        xs =
            List.map (.position >> V.getX) bodies

        ys =
            List.map (.position >> V.getY) bodies

        getCorner f =
            Maybe.map2 Tuple.pair (f xs) (f ys)
                |> Maybe.withDefault ( 0, 0 )

        lowerLeft =
            getCorner List.minimum

        topRight =
            getCorner List.maximum
    in
    B.boundingBox lowerLeft topRight


empty : Universe
empty =
    { bodies = []
    , g = 15
    , dt = 0.04
    , n = 150
    , epoch = 0
    }


update : Universe -> Universe
update u =
    let
        index =
            indexUniverse u

        newBodiesWithMerged =
            List.filterMap (updateBody index u) u.bodies

        merged =
            newBodiesWithMerged
                |> List.map (Tuple.second >> List.map .id >> Set.fromList)
                |> List.foldl Set.union Set.empty

        newBodies =
            newBodiesWithMerged
                |> List.filterMap
                    (\( body, _ ) ->
                        if Set.member body.id merged then
                            Nothing

                        else
                            Just body
                    )
    in
    { u | bodies = newBodies, epoch = u.epoch + 1 }


updateBody : QuadTree Body -> Universe -> Body -> Body.UpdateResult
updateBody t { g, dt } b =
    let
        theta =
            1

        bodies =
            getRelatedBodies t theta b

        ( bodiesCollided, bodiesNotCollided ) =
            getBodyDists bodies b
                |> List.partition (shouldCollide b)
    in
    Body.update g dt bodiesCollided bodiesNotCollided b


getBodyDists : List Body -> Body -> List BodyDist
getBodyDists others b =
    List.map (Body.getDist b) others


shouldCollide : Body -> BodyDist -> Bool
shouldCollide me bodyDist =
    if Body.isCenterOfMass bodyDist.body then
        False
        -- previously dist < r1 + r2 was used. However that resulted in jumpy
        -- animation when 2 bodies collide, especially when they have large radious
        -- this allows the bodies to move closer to each other

    else
        bodyDist.dist < me.radious || bodyDist.dist < bodyDist.body.radious


getRelatedBodies : QuadTree Body -> Float -> Body -> List Body
getRelatedBodies t theta b =
    let
        isFarAway box centerOfMass =
            (B.getSize box
                / V.distance b.position centerOfMass.position
            )
                >= theta
    in
    Q.takeWhile isFarAway t
        |> List.filter (.id >> (/=) b.id)


indexUniverse : Universe -> QuadTree Body
indexUniverse ({ bodies } as u) =
    let
        t0 =
            Q.empty (getBox u)

        -- add points
        t =
            List.foldl (\b -> Q.addPoint (P.fromVec b.position) b) t0 bodies

        -- calculate center of mass
        centerOfMass subtrees =
            let
                children =
                    subtrees
                        |> B.listQuadrant
                        |> List.filterMap Q.getValue

                totalMass =
                    children
                        |> List.foldl (\b acc -> b.mass + acc) 0

                center =
                    children
                        |> List.foldl
                            (\b acc ->
                                V.scale (b.mass / totalMass) b.position
                                    |> V.add acc
                            )
                            (V.vec2 0 0)
                        |> P.fromVec
            in
            Body.centerOfMass totalMass center
    in
    Q.summarize centerOfMass t
