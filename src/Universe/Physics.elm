module Universe.Physics
    exposing
        ( bang
        , tick
        , setG
        , setDT
        , setN
        , getBodies
        , Universe
        , Body
        , body
        , empty
        , G
        , DT
        )

import Math.Vector2 exposing (..)


type alias Force =
    Vec2


type alias G =
    Float


type alias DT =
    Float


type alias Body =
    { mass : Float
    , velocity : Vec2
    , position : Vec2
    }


body : Float -> ( Float, Float ) -> ( Float, Float ) -> Body
body mass velocity position =
    Body mass (fromTuple velocity) (fromTuple position)


getR : Body -> Float
getR body =
    sqrt body.mass


type alias IndexedBody =
    ( Int, Body )


getForce : G -> IndexedBody -> IndexedBody -> Maybe Force
getForce g ( id1, b1 ) ( id2, b2 ) =
    if id1 == id2 then
        Nothing
    else
        let
            delta =
                sub b2.position b1.position

            dist =
                length delta
        in
            if dist < ([ b1, b2 ] |> List.map getR |> List.sum) then
                -- do collision
                Nothing
            else
                Just <|
                    scale (g * b1.mass * b2.mass / (dist ^ 3)) delta


applyForce : DT -> IndexedBody -> Force -> IndexedBody
applyForce dt ( id, body ) force =
    let
        { mass, velocity, position } =
            body

        newVelocity =
            force
                |> scale (dt / mass)
                |> add velocity

        newPosition =
            velocity
                |> add newVelocity
                |> scale (dt * 0.5)
                |> add position
    in
        ( id, { body | velocity = newVelocity, position = newPosition } )


type alias Universe =
    { bodies : List IndexedBody
    , g : G
    , dt : DT
    , n : Int
    , epoch : Int
    }


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
    { bodies = bodies |> List.indexedMap (,)
    , g = g
    , dt = dt
    , n = n
    , epoch = 0
    }


empty : Universe
empty =
    { bodies = []
    , g = 5
    , dt = 0.1
    , n = 20
    , epoch = 0
    }


getBodies : Universe -> List Body
getBodies universe =
    universe.bodies
        |> List.map Tuple.second


tickBody : Universe -> IndexedBody -> IndexedBody
tickBody { bodies, g, dt } indexedBody =
    let
        combinedForce =
            bodies
                |> List.filterMap (getForce g indexedBody)
                |> List.foldl add (vec2 0 0)
    in
        applyForce dt indexedBody combinedForce


tick : Universe -> Universe
tick universe =
    let
        newBodies =
            universe.bodies
                |> List.map (tickBody universe)
    in
        { universe | bodies = newBodies, epoch = universe.epoch + 1 }
