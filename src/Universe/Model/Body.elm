module Universe.Model.Body exposing
    ( Body
    , BodyDist
    , DT
    , Force
    , G
    , body
    , empty
    , getDist
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
    , radious = getR mass
    , velocity = fromTuple velocity
    , position = fromTuple position
    , positionHistory = RingBuffer.initialize positionHistorySize (\_ -> vec2 0 0)
    }


empty : Body
empty =
    body 0 ( 0, 0 ) ( 0, 0 )


getR : Float -> Float
getR mass =
    sqrt mass / 2


update : G -> DT -> List BodyDist -> List BodyDist -> Body -> Maybe Body
update g dt bodiesCollided bodiesNotCollided me =
    let
        meAfterCollision =
            handleCollision me bodiesCollided

        handleNotCollided b =
            bodiesNotCollided
                |> List.map (getForceNotCollided g b)
                |> sumVec
                |> applyForce dt b
    in
    Maybe.map handleNotCollided meAfterCollision



-- INTERNAL


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



-- When a bunch of bodies collide, they are deterministically merged into one.
-- The one with the biggest mass wins, as that gives the best visual effect.
-- If the current body is absorbed, Nothing is returned


handleCollision : Body -> List BodyDist -> Maybe Body
handleCollision me bodiesCollided =
    if List.isEmpty bodiesCollided then
        Just me

    else
        let
            isTheMostMassive =
                List.all (\other -> me.mass >= other.body.mass) bodiesCollided
        in
        if isTheMostMassive then
            let
                totalMass =
                    List.map (.body >> .mass) bodiesCollided |> List.sum

                totalMomentum =
                    List.map (.body >> getMomentum) bodiesCollided |> sumVec

                velocity =
                    scale (1 / totalMass) totalMomentum
            in
            Just
                { me
                    | mass = totalMass
                    , radious = getR totalMass
                    , velocity = velocity
                }

        else
            Nothing


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
