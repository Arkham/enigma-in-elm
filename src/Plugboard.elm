module Plugboard exposing (Plugboard, empty)


type Plugboard
    = Plugboard (List ( Char, Char ))


empty : Plugboard
empty =
    Plugboard []
