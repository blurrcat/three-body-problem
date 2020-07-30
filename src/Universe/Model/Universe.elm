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

import BHTree as BHTree exposing (BHTree)
import Set
import Universe.Model.Body as Body
    exposing
        ( Body
        , DT
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


empty : Universe
empty =
    { bodies = []
    , g = 0.25
    , dt = 0.08
    , n = 800
    , epoch = 0
    }


update : Universe -> Universe
update u =
    let
        index =
            BHTree.bhtree u.bodies

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


updateBody : BHTree -> Universe -> Body -> Maybe ( Body, List Body )
updateBody t { g, dt } me =
    let
        theta =
            1

        closeMatter =
            BHTree.massCloseTo theta me t

    in
    Body.update g dt closeMatter me
