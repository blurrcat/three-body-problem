module Universe.Model.Body exposing
    ( Body
    , CenterOfMass
    , DT
    , Force
    , G
    , MassDist
    , body
    , centerOfMass
    , positionHistory
    , update
    )

import Math.Vector2 as V exposing (Vec2)
import Math.Vector2.Extra as VE
import RingBuffer exposing (RingBuffer)


type alias Force =
    Vec2


type alias G =
    Float


type alias DT =
    Float


type alias Massed a =
    { a
        | mass : Float
        , position : Vec2
    }


type alias Body =
    Massed
        { id : Int
        , velocity : Vec2
        , radious : Float
        , positionHistory : RingBuffer Vec2
        }


type alias CenterOfMass =
    Massed {}


positionHistorySize : Int
positionHistorySize =
    50


positionHistory : Body -> List Vec2
positionHistory b =
    RingBuffer.toList b.positionHistory


body : Float -> ( Float, Float ) -> ( Float, Float ) -> Body
body mass velocity position =
    { id = 0
    , mass = mass
    , radious = getR mass
    , velocity = VE.fromTuple velocity
    , position = VE.fromTuple position
    , positionHistory = RingBuffer.initialize positionHistorySize (always VE.origin)
    }


centerOfMass : Float -> Vec2 -> CenterOfMass
centerOfMass mass position =
    { mass = mass
    , position = position
    }


getR : Float -> Float
getR mass =
    sqrt mass / pi


update : G -> DT -> ( List CenterOfMass, List Body ) -> Body -> Maybe ( Body, List Body )
update g dt ( centers, bodies ) me =
    let
        ( bodiesCollidedWithDist, bodiesNotCollidedWithDist ) =
            getDists bodies me
                |> List.partition (shouldCollide me)

        bodiesCollided =
            List.map Tuple.first bodiesCollidedWithDist

        -- Not collided:
        -- calculating force only needs to know the position and mass of neighbors
        -- convert to MassDist
        centersDist =
            getDists centers (centerOfMass me.mass me.position)

        bodiesDist =
            bodiesNotCollidedWithDist
                |> List.map (Tuple.mapFirst (\b -> centerOfMass b.mass b.position))

        notCollided =
            List.append centersDist bodiesDist

        handleNotCollided newMe =
            notCollided
                |> getCombinedForceNotCollided g me
                |> applyForce dt newMe
    in
    handleCollision me bodiesCollided
        |> Maybe.map (Tuple.mapFirst handleNotCollided)



-- INTERNAL


getMomentum : Body -> Vec2
getMomentum b =
    V.scale b.mass b.velocity


getForceNotCollided : G -> Body -> MassDist a -> Force
getForceNotCollided g me ( b, dist ) =
    if dist == 0 then
        VE.origin

    else
        let
            delta =
                V.sub b.position me.position
        in
        V.scale (g * me.mass * b.mass / (dist ^ 3)) delta


getCombinedForceNotCollided : G -> Body -> List (MassDist a) -> Force
getCombinedForceNotCollided g me centersOfMass =
    centersOfMass
        |> List.map (getForceNotCollided g me)
        |> VE.sum


{-| Some other bodies and me are "close" enough. What do I do?

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
handleCollision : Body -> List Body -> Maybe ( Body, List Body )
handleCollision me bodiesCollided =
    if List.isEmpty bodiesCollided then
        Just ( me, [] )

    else
        let
            isTheMostMassive =
                List.all (\b -> me.mass >= b.mass) bodiesCollided
        in
        if isTheMostMassive then
            let
                cluster =
                    me :: bodiesCollided

                totalMass =
                    List.map .mass cluster |> List.sum

                totalMomentum =
                    List.map getMomentum cluster |> VE.sum

                velocity =
                    V.scale (1 / totalMass) totalMomentum
            in
            Just
                ( { me
                    | mass = totalMass
                    , radious = getR totalMass
                    , velocity = velocity
                  }
                , bodiesCollided
                )

        else
            Nothing


applyForce : DT -> Body -> Force -> Body
applyForce dt b force =
    let
        newVelocity =
            force
                |> V.scale (dt / b.mass)
                |> V.add b.velocity

        newPosition =
            b.velocity
                |> V.add newVelocity
                |> V.scale (dt * 0.5)
                |> V.add b.position

        -- only record position history for large bodies
        newPositionHistory =
            if isSizeSmall b then
                b.positionHistory

            else
                b.positionHistory |> RingBuffer.push b.position
    in
    { b
        | velocity = newVelocity
        , position = newPosition
        , positionHistory = newPositionHistory
    }


type alias MassDist a =
    ( Massed a, Float )


type alias BodyDist =
    ( Body, Float )


getDist : Massed a -> Massed a -> ( Massed a, Float )
getDist b1 b2 =
    ( b2, V.distance b1.position b2.position )


getDists : List (Massed a) -> Massed a -> List (MassDist a)
getDists others me =
    List.map (getDist me) others


shouldCollide : Body -> BodyDist -> Bool
shouldCollide me ( b, d ) =
    -- previously dist < r1 + r2 was used. However that resulted in jumpy
    -- animation when 2 bodies collide, especially when they have large radious
    -- this allows the bodies to move closer to each other
    d < me.radious || d < b.radious


isSizeSmall : Body -> Bool
isSizeSmall { radious } =
    radious < 0.1
