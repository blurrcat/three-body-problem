module Universe.Model.Body exposing
    ( Body
    , BodyDist
    , DT
    , Force
    , G
    , UpdateResult
    , body
    , centerOfMass
    , getDist
    , isCenterOfMass
    , update
    )

import Math.Vector2 as V exposing (..)
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


centerOfMass : Float -> ( Float, Float ) -> Body
centerOfMass mass position =
    { id = -1
    , mass = mass
    , radious = 0
    , velocity = V.vec2 0 0
    , position = fromTuple position
    , positionHistory = RingBuffer.initialize 0 (\_ -> V.vec2 0 0)
    }


isCenterOfMass : Body -> Bool
isCenterOfMass { id } =
    id == -1


getR : Float -> Float
getR mass =
    sqrt mass / 2


update : G -> DT -> List BodyDist -> List BodyDist -> Body -> UpdateResult
update g dt bodiesCollided bodiesNotCollided me =
    let
        meAfterCollision =
            handleCollision me bodiesCollided

        handleNotCollided ( b, merged ) =
            ( bodiesNotCollided
                |> List.map (getForceNotCollided g b)
                |> sumVec
                |> applyForce dt b
            , merged
            )
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


type alias UpdateResult =
    Maybe ( Body, List Body )


{-|Some other bodies and me are "close" enough. What do I do?

The simple rule is, whoever has the biggest mass in the cluster should
"absorb" all others, while maintaining momentum of the cluster.

To achieve this, it is vital that all bodies in the cluster make the same
decision on whether it should aborb or be absorbed.

With DirectMethod, all bodies in the group have the same view of the
world - everyone can individually make decisions that others will agree.

However with Barnes-Hub, they no longer have a consistent view of the
world. For example,
one body might be using the center of mass of the cell of the other body,
while the other may be using the exact position of its companion.
This leads to a situation where A thinks it aborbs B but B thinks it's
still fine - a mass leak, if you will, has happened.

To deal with this, for each body, the list of bodies one thinks it
absorbed is returned. On the universe level, all those are removed regardless
of their opinion, brutally ensuring consistency.
-}
handleCollision : Body -> List BodyDist -> UpdateResult
handleCollision me bodiesCollided =
    if List.isEmpty bodiesCollided then
        Just ( me, [] )

    else
        let
            isTheMostMassive =
                List.all (\other -> me.mass >= other.body.mass) bodiesCollided
        in
        if isTheMostMassive then
            let
                allBodies =
                    { body = me, dist = 0 } :: bodiesCollided

                totalMass =
                    List.map (.body >> .mass) allBodies |> List.sum

                totalMomentum =
                    List.map (.body >> getMomentum) allBodies |> sumVec

                velocity =
                    scale (1 / totalMass) totalMomentum
            in
            Just
                ( { me
                    | mass = totalMass
                    , radious = getR totalMass
                    , velocity = velocity
                  }
                , List.map .body bodiesCollided
                )

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
