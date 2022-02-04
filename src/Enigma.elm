module Enigma exposing (Enigma, encodeString, makeOne)

import Reflector exposing (Reflector)
import Rotor exposing (Rotor)



{-
    The journey of a letter:

   - from keyboard to plugboard
   - from plugboard to rightmost rotor
   - from rightmost rotor to middle rotor
   - from middle rotor to leftmost rotor
   - from leftmost rotor to reflector and back
   - from leftmost rotor to middle rotor
   - from middle rotor to rightmost rotor
   - from rightmost rotor to plugboard
   - from plugboard to lamp
-}


type Plugboard
    = Plugboard (List ( Char, Char ))


type Enigma
    = Enigma
        { leftRotor : Rotor
        , middleRotor : Rotor
        , rightRotor : Rotor
        , reflector : Reflector
        , plugboard : Plugboard
        }


makeOne : Enigma
makeOne =
    Enigma
        { leftRotor = Rotor.rotorI 1
        , middleRotor = Rotor.rotorII 0
        , rightRotor = Rotor.rotorIII 0
        , reflector = Reflector.reflectorB
        , plugboard = Plugboard []
        }


step : Enigma -> Enigma
step (Enigma ({ leftRotor, middleRotor, rightRotor } as info)) =
    case
        ( Rotor.atNotch middleRotor
        , Rotor.atNotch rightRotor
        )
    of
        ( True, _ ) ->
            Enigma
                { info
                    | middleRotor = Rotor.turn middleRotor
                    , leftRotor = Rotor.turn leftRotor
                    , rightRotor = Rotor.turn rightRotor
                }

        ( _, True ) ->
            Enigma
                { info
                    | middleRotor = Rotor.turn middleRotor
                    , rightRotor = Rotor.turn rightRotor
                }

        _ ->
            Enigma
                { info
                    | rightRotor = Rotor.turn rightRotor
                }


encode : Char -> Enigma -> ( Char, Enigma )
encode input enigma =
    let
        ((Enigma { leftRotor, middleRotor, rightRotor, reflector }) as stepped) =
            step enigma
    in
    ( input
        |> (\e -> Char.toCode e - 65)
        |> Rotor.forward rightRotor
        |> Rotor.forward middleRotor
        |> Rotor.forward leftRotor
        |> Reflector.reflect reflector
        |> Rotor.backward leftRotor
        |> Rotor.backward middleRotor
        |> Rotor.backward rightRotor
        |> (\e -> Char.fromCode (e + 65))
    , stepped
    )


encodeString : String -> Enigma -> ( String, Enigma )
encodeString input enigma =
    String.toList input
        |> List.filter Char.isAlpha
        |> List.map Char.toUpper
        |> List.foldl
            (\value ( acc, curr ) ->
                let
                    ( encoded, new ) =
                        encode value curr
                in
                ( encoded :: acc, new )
            )
            ( [], enigma )
        |> Tuple.mapFirst (List.reverse >> String.fromList)
