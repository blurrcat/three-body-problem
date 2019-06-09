module Universe.Random exposing (BodyParams, FloatPair, genBody, genUniverse)

import Random as R
import Random.Float as RF
import Universe.Model.Body exposing (Body, DT, G, body)
import Universe.Model.Universe exposing (Universe, bang)


type alias FloatPair =
    ( Float, Float )


type alias BodyParams =
    { massRange : FloatPair
    , velocityRange : FloatPair
    , positionRange : FloatPair
    }


sqrt12 =
    sqrt 12


randomNormal : FloatPair -> R.Generator Float
randomNormal ( min, max ) =
    RF.normal
        ((max - min) / 2.0 + min)
        ((max - min) / sqrt12)


randomNormalPositive : FloatPair -> R.Generator Float
randomNormalPositive ( min, max ) =
    RF.normal
        0
        ((max - min) / sqrt12)
        |> R.map (\v -> abs v + min)


randomUniformPair : FloatPair -> R.Generator FloatPair
randomUniformPair ( min, max ) =
    R.pair (R.float min max) (R.float min max)


randomNormalPair : FloatPair -> R.Generator FloatPair
randomNormalPair range =
    R.pair (randomNormal range) (randomNormal range)


genBody : BodyParams -> R.Generator Body
genBody { massRange, velocityRange, positionRange } =
    R.map3 body
        (randomNormalPositive massRange)
        (randomNormalPair velocityRange)
        (randomUniformPair positionRange)


genBodies : Int -> BodyParams -> R.Generator (List Body)
genBodies n params =
    R.list n <| genBody params


genUniverse : G -> DT -> Int -> BodyParams -> R.Generator Universe
genUniverse g dt n params =
    R.map
        (bang n g dt)
        (genBodies n params)
