module Universe exposing (bang, tick, getBodies, Universe, Body, body)

import Math.Vector2 exposing (..)


type alias Force =
    Vec2


type alias ConstantG =
    Float


type alias Body =
    { mass : Float
    , velocity : Vec2
    , position : Vec2
    }


body : Float -> ( Float, Float ) -> ( Float, Float ) -> Body
body mass velocity position =
    Body mass (fromTuple velocity) (fromTuple position)


type alias IndexedBody =
    ( Int, Body )


getForce : ConstantG -> IndexedBody -> IndexedBody -> Maybe Force
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
            Just <|
                scale (g * b1.mass * b2.mass / (dist ^ 3)) delta


applyForce : Float -> IndexedBody -> Force -> IndexedBody
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
    , constantG : Float
    , dt : Float
    , epoch : Int
    }


bang : List Body -> Float -> Float -> Universe
bang bodies g dt =
    { bodies = bodies |> List.indexedMap (,)
    , constantG = g
    , dt = dt
    , epoch = 0
    }


getBodies : Universe -> List Body
getBodies universe =
    universe.bodies
        |> List.map Tuple.second


tickBody : Universe -> IndexedBody -> IndexedBody
tickBody { bodies, constantG, dt } indexedBody =
    let
        combinedForce =
            bodies
                |> List.filterMap (getForce constantG indexedBody)
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
