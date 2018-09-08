module Universe.Model.Universe
    exposing
        ( Universe
        , bang
        , empty
        , getBodies
        , setDT
        , setG
        , setN
        , update
        )

import Universe.Model.Body as Body exposing (Body, DT, Force, G)


type alias Universe =
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
    , g = 50
    , dt = 0.04
    , n = 150
    , epoch = 0
    }


update : Universe -> Universe
update ({ bodies, g, dt, epoch } as universe) =
    let
        newBodies =
            universe.bodies
                |> List.map (Body.update bodies g dt)
    in
        { universe | bodies = newBodies, epoch = epoch + 1 }
