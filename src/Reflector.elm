module Reflector exposing (Reflector, reflect, reflectorB)

import RotorMapping exposing (RotorMapping)


type Reflector
    = Reflector RotorMapping


reflectorB : Reflector
reflectorB =
    fromString "YRUHQSLDPXNGOKMIEBFZCWVJAT"


fromString : String -> Reflector
fromString input =
    Reflector (RotorMapping.fromString input)


reflect : Reflector -> Int -> Int
reflect (Reflector mapping) value =
    RotorMapping.forward mapping value
