module Reflector exposing
    ( Reflector
    , reflect
    , allReflectors, reflectorsByName
    , reflectorB, reflectorC
    )

{-| The Reflector

Each reflector maps a letter of the alphabet to another and back


# Definition

@docs Reflector
@docs reflect
@docs allReflectors, reflectorsByName
@docs reflectorB, reflectorC

-}

import Dict exposing (Dict)
import RotorMapping exposing (RotorMapping)


type alias Reflector =
    { name : String
    , mapping : RotorMapping
    }


reflectorB : Reflector
reflectorB =
    { name = "UKW B"
    , mapping = RotorMapping.fromString "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    }


reflectorC : Reflector
reflectorC =
    { name = "UKW C"
    , mapping = RotorMapping.fromString "FVPJIAOYEDRZXWGCTKUQSBNMHL"
    }


allReflectors : List Reflector
allReflectors =
    [ reflectorB, reflectorC ]


reflectorsByName : Dict String Reflector
reflectorsByName =
    List.map (\r -> ( r.name, r )) allReflectors
        |> Dict.fromList


reflect : Reflector -> Int -> Int
reflect { mapping } value =
    RotorMapping.forward mapping value
