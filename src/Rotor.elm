module Rotor exposing
    ( Rotor(..)
    , atNotch, turn
    , forward, backward
    , rotorI, rotorII, rotorIII
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


# Interact with rotors

@docs atNotch, turn
@docs forward, backward


# Known rotors

@docs rotorI, rotorII, rotorIII

-}

import RotorMapping exposing (RotorMapping)
import Set exposing (Set)


type Rotor
    = Rotor
        { mapping : RotorMapping
        , notches : Set Char
        , position : Int
        }


rotorI : Int -> Rotor
rotorI position =
    Rotor
        { mapping = RotorMapping.fromString "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
        , notches = Set.fromList [ 'Q' ]
        , position = position
        }


rotorII : Int -> Rotor
rotorII position =
    Rotor
        { mapping = RotorMapping.fromString "AJDKSIRUXBLHWTMCQGZNPYFVOE"
        , notches = Set.fromList [ 'E' ]
        , position = position
        }


rotorIII : Int -> Rotor
rotorIII position =
    Rotor
        { mapping = RotorMapping.fromString "BDFHJLCPRTXVZNYEIWGAKMUSQO"
        , notches = Set.fromList [ 'V' ]
        , position = position
        }


atNotch : Rotor -> Bool
atNotch (Rotor { notches, position }) =
    Set.member (Char.fromCode (position + 65)) notches


turn : Rotor -> Rotor
turn (Rotor ({ position } as info)) =
    Rotor { info | position = modBy 26 (position + 1) }


forward : Rotor -> Int -> Int
forward rotor value =
    encipher RotorMapping.forward rotor value


backward : Rotor -> Int -> Int
backward rotor value =
    encipher RotorMapping.backward rotor value


encipher : (RotorMapping -> Int -> Int) -> Rotor -> Int -> Int
encipher fn (Rotor { mapping, position }) value =
    let
        addShift v =
            modBy 26 (v + position)

        applyMapping v =
            fn mapping v

        removeShift v =
            modBy 26 (v - position)
    in
    value
        |> addShift
        |> applyMapping
        |> removeShift
