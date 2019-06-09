module Universe.Model.Body exposing
    ( Body
    , DT
    , Force
    , G
    , body
    , update
    )

import Math.Vector2 exposing (..)
import RingBuffer exposing (RingBuffer)


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
    , positionHistory : RingBuffer Vec2
    }


fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
    fromRecord { x = x, y = y }


positionHistorySize =
    50


body : Float -> ( Float, Float ) -> ( Float, Float ) -> Body
body mass velocity position =
    { id = 0
    , mass = mass
    , radious = sqrt mass
    , velocity = fromTuple velocity
    , position = fromTuple position
    , positionHistory = RingBuffer.initialize positionHistorySize (\_ -> vec2 0 0)
    }


getR : Body -> Float
getR b =
    sqrt b.mass


setId : Int -> Body -> Body
setId id b =
    { b | id = id }


update : List Body -> G -> DT -> Body -> Body
update bodies g dt me =
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



-- INTERNAL


shouldCollide : Body -> BodyDist -> Bool
shouldCollide me bodyDist =
    bodyDist.dist < (me.radious + bodyDist.body.radious)


getMomentum : Body -> Vec2
getMomentum b =
    scale b.mass b.velocity


sumVec : List Vec2 -> Vec2
sumVec vecs =
    vecs
        |> List.foldl add (vec2 0 0)


getForceNotCollided : G -> Body -> BodyDist -> Force
getForceNotCollided g me bodyDist =
    let
        b =
            bodyDist.body

        dist =
            bodyDist.dist

        delta =
            sub b.position me.position
    in
    if dist == 0 then
        vec2 0 0

    else
        scale (g * me.mass * b.mass / (dist ^ 3)) delta


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
applyForce dt b force =
    let
        { mass, velocity, position, positionHistory } =
            b

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
    { b
        | velocity = newVelocity
        , position = newPosition
        , positionHistory = RingBuffer.push position positionHistory
    }


type alias BodyDist =
    { body : Body
    , dist : Float
    }


getDist : Body -> Body -> BodyDist
getDist b1 b2 =
    { body = b2, dist = distance b1.position b2.position }
