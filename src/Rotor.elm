module Rotor exposing
    ( Rotor
    , ChosenRotor, choose, chooseFromString
    , atNotch, turn
    , forward, backward
    , toRotorOffset, fromRotorOffset
    , allRotors
    , rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII
    )

{-| The Rotor!

Each rotor maps a letter of the alphabet to another.

For example, Rotor I in the original Enigma had this mapping:
ABCDEFGHIJKLMNOPQRSTUVWXYZ
EKMFLGDQVZNTOWYHXUSPAIBRCJ

Each rotor has a turnover notch, which will make the
next rotor advance. For example, Rotor I's notch was positioned
at the letter Q: when the rotor stepped from Q to R it would
then advance the next rotor.


# Definition

@docs Rotor
@docs ChosenRotor, choose, chooseFromString


# Interact with rotors

@docs atNotch, turn
@docs forward, backward
@docs toRotorOffset, fromRotorOffset


# Known rotors

@docs allRotors
@docs rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII

-}

import Dict exposing (Dict)
import RotorMapping exposing (RotorMapping)
import Set exposing (Set)


type alias Rotor =
    { name : String
    , mapping : RotorMapping
    , notches : Set Char
    }


{-| When we choose to use a rotor and we position it in the machine,
we need to track the rotor position. Furthermore, each rotor can be
set up using a ring setting pin that offsets the displayed letter
from the actual configuration of the rotor.
-}
type alias ChosenRotor =
    { rotor : Rotor
    , position : Int
    , setting : Int
    }


rotorI : Rotor
rotorI =
    { name = "I"
    , mapping = RotorMapping.fromString "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    , notches = Set.fromList [ 'Q' ]
    }


rotorII : Rotor
rotorII =
    { name = "II"
    , mapping = RotorMapping.fromString "AJDKSIRUXBLHWTMCQGZNPYFVOE"
    , notches = Set.fromList [ 'E' ]
    }


rotorIII : Rotor
rotorIII =
    { name = "III"
    , mapping = RotorMapping.fromString "BDFHJLCPRTXVZNYEIWGAKMUSQO"
    , notches = Set.fromList [ 'V' ]
    }


rotorIV : Rotor
rotorIV =
    { name = "IV"
    , mapping = RotorMapping.fromString "ESOVPZJAYQUIRHXLNFTGKDCMWB"
    , notches = Set.fromList [ 'J' ]
    }


rotorV : Rotor
rotorV =
    { name = "V"
    , mapping = RotorMapping.fromString "VZBRGITYUPSDNHLXAWMJQOFECK"
    , notches = Set.fromList [ 'Z' ]
    }


rotorVI : Rotor
rotorVI =
    { name = "VI"
    , mapping = RotorMapping.fromString "JPGVOUMFYQBENHZRDKASXLICTW"
    , notches = Set.fromList [ 'Z', 'M' ]
    }


rotorVII : Rotor
rotorVII =
    { name = "VII"
    , mapping = RotorMapping.fromString "NZJHGRCXMYSWBOUFAIVLPEKQDT"
    , notches = Set.fromList [ 'Z', 'M' ]
    }


rotorVIII : Rotor
rotorVIII =
    { name = "VIII"
    , mapping = RotorMapping.fromString "FKQHTLXOCBJSPDZRAMEWNIUYGV"
    , notches = Set.fromList [ 'Z', 'M' ]
    }


allRotors : List Rotor
allRotors =
    [ rotorI
    , rotorII
    , rotorIII
    , rotorIV
    , rotorV
    , rotorVI
    , rotorVII
    , rotorVIII
    ]


rotorsByName : Dict String Rotor
rotorsByName =
    List.map
        (\rotor -> ( rotor.name, rotor ))
        allRotors
        |> Dict.fromList


{-| Choose a rotor to put in the enigma machine. Since there was no computing
they used to use 1-based indexes.
-}
choose : Rotor -> Int -> Int -> ChosenRotor
choose rotor position setting =
    { rotor = rotor
    , position = position - 1
    , setting = setting - 1
    }


{-| Choose a rotor to put in the enigma machine. Since there was no computing
they used to use 1-based indexes.
-}
chooseFromString : String -> Int -> Int -> Maybe ChosenRotor
chooseFromString str position setting =
    Dict.get str rotorsByName
        |> Maybe.map (\rotor -> choose rotor position setting)


{-| When we switch from the plugboard to the rotors, we stop caring about which
is the letter that was pressed, but we only care about the offset of the rotor.
-}
toRotorOffset : Char -> Int
toRotorOffset c =
    Char.toCode c - 65


{-| When we switch from the rotors to the plugboard, we stop caring about the
offset of the rotor and again we care about which is the character connected to
the current circuit.
-}
fromRotorOffset : Int -> Char
fromRotorOffset i =
    Char.fromCode (i + 65)


atNotch : ChosenRotor -> Bool
atNotch { rotor, position } =
    Set.member (Char.fromCode (position + 65)) rotor.notches


turn : ChosenRotor -> ChosenRotor
turn ({ position } as info) =
    { info | position = modBy 26 (position + 1) }


forward : ChosenRotor -> Int -> Int
forward rotor value =
    encipher RotorMapping.forward rotor value


backward : ChosenRotor -> Int -> Int
backward rotor value =
    encipher RotorMapping.backward rotor value


encipher : (RotorMapping -> Int -> Int) -> ChosenRotor -> Int -> Int
encipher fn { rotor, position, setting } value =
    let
        shift =
            position - setting

        addShift v =
            modBy 26 (v + shift)

        applyMapping v =
            fn rotor.mapping v

        removeShift v =
            modBy 26 (v - shift)
    in
    value
        |> addShift
        |> applyMapping
        |> removeShift
