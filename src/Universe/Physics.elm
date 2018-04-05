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
    { id : Int
    , mass : Float
    , radious : Float
    , velocity : Vec2
    , position : Vec2
    }


body : Float -> ( Float, Float ) -> ( Float, Float ) -> Body
body mass velocity position =
    Body 0 mass (sqrt mass) (fromTuple velocity) (fromTuple position)


getR : Body -> Float
getR body =
    sqrt body.mass


getMomentum : Body -> Vec2
getMomentum body =
    scale body.mass body.velocity


collide : DT -> Body -> Body -> Force
collide dt b1 b2 =
    let
        totalMass =
            b1.mass + b2.mass

        newVelocity =
            scale (b1.mass / totalMass) b1.velocity
                |> add (scale (b2.mass / totalMass) b2.velocity)
    in
        b1.velocity
            |> sub newVelocity
            |> scale (b1.mass / dt)


sumVec : List Vec2 -> Vec2
sumVec vecs =
    vecs
        |> List.foldl add (vec2 0 0)


getForceNotCollided : G -> Body -> BodyDist -> Force
getForceNotCollided g me { body, dist } =
    let
        delta =
            sub body.position me.position
    in
        if dist == 0 then
            vec2 0 0
        else
            scale (g * me.mass * body.mass / (dist ^ 3)) delta


getForceCollided : DT -> G -> Body -> List BodyDist -> Force
getForceCollided dt g me bodyDists =
    let
        bodies =
            bodyDists
                |> List.map .body
                |> (::) me

        totalMass =
            bodies
                |> List.map .mass
                |> List.sum

        newVelocity =
            bodies
                |> List.map getMomentum
                |> sumVec
                |> scale (1 / totalMass)
    in
        me.velocity
            |> sub newVelocity
            |> scale (me.mass / dt)


applyForce : DT -> Body -> Force -> Body
applyForce dt body force =
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
        { body | velocity = newVelocity, position = newPosition }


type alias BodyDist =
    { body : Body
    , dist : Float
    }


getDist : Body -> Body -> BodyDist
getDist b1 b2 =
    { body = b2, dist = (distance b1.position b2.position) }


shouldCollide : Body -> BodyDist -> Bool
shouldCollide me bodyDist =
    bodyDist.dist < (me.radious + bodyDist.body.radious)


type alias Universe =
    { bodies : List Body
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


getBodies : Universe -> List Body
getBodies universe =
    universe.bodies


tickBody : Universe -> Body -> Body
tickBody { bodies, g, dt } me =
    let
        ( bodiesCollided, bodiesNotCollided ) =
            bodies
                |> List.map (getDist me)
                |> List.partition (shouldCollide me)

        forceNotCollided =
            bodiesNotCollided
                |> List.map (getForceNotCollided g me)
                |> sumVec

        forceCollided =
            getForceCollided dt g me bodiesCollided
    in
        applyForce dt me (add forceNotCollided forceCollided)


tick : Universe -> Universe
tick universe =
    let
        newBodies =
            universe.bodies
                |> List.map (tickBody universe)
    in
        { universe | bodies = newBodies, epoch = universe.epoch + 1 }
