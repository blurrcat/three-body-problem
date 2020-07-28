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


empty : Universe
empty =
    { bodies = []
    , g = 25
    , dt = 0.04
    , n = 150
    , epoch = 0
    }


update : Universe -> Universe
update ({ bodies, g, dt, epoch } as universe) =
    let
        updateBody b =
            let
                -- bodyDists = getBodyDists bodies b
                (bodiesCollided, bodiesNotCollided) =
                    getBodyDists bodies b
                    |> List.partition (shouldCollide b) 
            in
                Body.update g dt bodiesCollided bodiesNotCollided b
        newBodies =
            universe.bodies
                |> List.filterMap updateBody
    in
    { universe | bodies = newBodies, epoch = epoch + 1 }


getBodyDists : List Body -> Body -> List BodyDist
getBodyDists others b =
    List.map (Body.getDist b) others


shouldCollide : Body -> BodyDist -> Bool
shouldCollide me bodyDist =
    -- previously dist < r1 + r2 was used. However that resulted in jumpy
    -- animation when 2 bodies collide, especially when they have large radious
    -- this allows the bodies to move closer to each other
    bodyDist.dist < me.radious || bodyDist.dist < bodyDist.body.radious

