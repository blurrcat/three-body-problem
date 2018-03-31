module Universe.Random exposing (genBody, genUniverse, FloatPair, BodyParams)

import Random as R
import Universe.Physics exposing (..)


type alias FloatPair =
    ( Float, Float )


type alias BodyParams =
    { massRange : FloatPair
    , velocityRange : FloatPair
    , positionRange : FloatPair
    }


randomPairFloat : FloatPair -> R.Generator ( Float, Float )
randomPairFloat ( v1, v2 ) =
    R.pair (R.float v1 v2) (R.float v1 v2)


genBody : BodyParams -> R.Generator Body
genBody { massRange, velocityRange, positionRange } =
    let
        ( minMass, maxMass ) =
            massRange
    in
        R.map3 body
            (R.float minMass maxMass)
            (randomPairFloat velocityRange)
            (randomPairFloat positionRange)


genBodies : Int -> BodyParams -> R.Generator (List Body)
genBodies n params =
    R.list n <| genBody params


genUniverse : G -> DT -> Int -> BodyParams -> R.Generator Universe
genUniverse g dt n params =
    R.map
        (bang g dt)
        (genBodies n params)
