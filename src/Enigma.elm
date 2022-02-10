module Enigma exposing (Enigma, encodeString)

import Plugboard exposing (Plugboard)
import Reflector exposing (Reflector)
import Rotor exposing (ChosenRotor)



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


type alias Enigma =
    { leftRotor : ChosenRotor
    , middleRotor : ChosenRotor
    , rightRotor : ChosenRotor
    , reflector : Reflector
    , plugboard : Plugboard
    }


step : Enigma -> Enigma
step ({ leftRotor, middleRotor, rightRotor } as info) =
    case
        ( Rotor.atNotch middleRotor
        , Rotor.atNotch rightRotor
        )
    of
        ( True, _ ) ->
            { info
                | middleRotor = Rotor.turn middleRotor
                , leftRotor = Rotor.turn leftRotor
                , rightRotor = Rotor.turn rightRotor
            }

        ( _, True ) ->
            { info
                | middleRotor = Rotor.turn middleRotor
                , rightRotor = Rotor.turn rightRotor
            }

        _ ->
            { info
                | rightRotor = Rotor.turn rightRotor
            }


encode : Char -> Enigma -> ( Char, Enigma )
encode input enigma =
    let
        ({ leftRotor, middleRotor, rightRotor, reflector } as stepped) =
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
