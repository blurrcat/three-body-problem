module BHTree.Region exposing
    ( Direction(..)
    , Regions
    , fold
    , map
    , mapRegion
    , regions
    , toList
    )


type Direction
    = NW
    | NE
    | SW
    | SE


type alias Regions a =
    { nw : a
    , ne : a
    , sw : a
    , se : a
    }


regions : (Direction -> a) -> Regions a
regions f =
    { nw = f NW
    , ne = f NE
    , sw = f SW
    , se = f SE
    }


map : (a -> b) -> Regions a -> Regions b
map f { nw, ne, se, sw } =
    { nw = f nw
    , ne = f ne
    , sw = f sw
    , se = f se
    }


fold : (a -> b -> b) -> b -> Regions a -> b
fold f b =
    toList >> List.foldl f b


mapRegion : Direction -> (a -> a) -> Regions a -> Regions a
mapRegion d f r =
    case d of
        NW ->
            { r | nw = f r.nw }

        NE ->
            { r | ne = f r.ne }

        SE ->
            { r | se = f r.se }

        SW ->
            { r | sw = f r.sw }


toList : Regions a -> List a
toList r =
    [ r.nw, r.ne, r.sw, r.se ]
