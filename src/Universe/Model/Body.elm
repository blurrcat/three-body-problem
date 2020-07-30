module Universe.Model.Body exposing
    ( Body
    , BodyDist
    , CenterOfMass
    , DT
    , Force
    , G
    , MassDist
    , body
    , centerOfMass
    , getDist
    , getDists
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


fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
    fromRecord { x = x, y = y }


positionHistorySize =
    30


body : Float -> ( Float, Float ) -> ( Float, Float ) -> Body
body mass velocity position =
    { id = 0
    , mass = mass
    , radious = getR mass
    , velocity = fromTuple velocity
    , position = fromTuple position
    , positionHistory = RingBuffer.initialize positionHistorySize (\_ -> vec2 0 0)
    }


centerOfMass : Float -> Vec2 -> CenterOfMass
centerOfMass mass position =
    { mass = mass
    , position = position
    }


getR : Float -> Float
getR mass =
    sqrt mass / pi


update : G -> DT -> List Body -> List (MassDist a) -> Body -> Maybe ( Body, List Body )
update g dt bodiesCollided bodiesNotCollided me =
    let
        handleNotCollided newMe =
            bodiesNotCollided
                |> List.map (getForceNotCollided g newMe)
                |> sumVec
                |> applyForce dt newMe
    in
    bodiesCollided
    |> handleCollision me
    |> Maybe.map (Tuple.mapFirst handleNotCollided)



-- INTERNAL


getMomentum : Body -> Vec2
getMomentum b =
    scale b.mass b.velocity


sumVec : List Vec2 -> Vec2
sumVec vecs =
    vecs
        |> List.foldl add (vec2 0 0)


getForceNotCollided : G -> Body -> MassDist a -> Force
getForceNotCollided g me ( b, dist ) =
    let
        delta =
            sub b.position me.position
    in
    if dist == 0 then
        vec2 0 0

    else
        scale (g * me.mass * b.mass / (dist ^ 3)) delta


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
                    List.map getMomentum cluster |> sumVec

                velocity =
                    scale (1 / totalMass) totalMomentum
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


type alias MassDist a =
    ( Massed a, Float )


type alias BodyDist =
    ( Body, Float )


getDist : Massed a -> Massed a -> ( Massed a, Float )
getDist b1 b2 =
    ( b2, distance b1.position b2.position )


getDists : List (Massed a) -> Massed a -> List (MassDist a)
getDists others me =
    List.map (getDist me) others
