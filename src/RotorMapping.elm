module RotorMapping exposing
    ( RotorMapping
    , backward
    , forward
    , fromString
    )

import Array exposing (Array)


{-| Keeps a cache of forward and backward rotor mappings.


# Build one

@docs RotorMapping, fromString


# Interact with it

@docs forward, backward

-}
type RotorMapping
    = RotorMapping (Array Int) (Array Int)


fromString : String -> RotorMapping
fromString input =
    let
        initial =
            String.toList input
                |> List.map (\e -> Char.toCode e - 65)
                |> Array.fromList

        inverse =
            Array.toIndexedList initial
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
                |> Array.fromList
    in
    RotorMapping initial inverse


forward : RotorMapping -> Int -> Int
forward (RotorMapping mapping _) value =
    Array.get value mapping
        |> Maybe.withDefault -1


backward : RotorMapping -> Int -> Int
backward (RotorMapping _ mapping) value =
    Array.get value mapping
        |> Maybe.withDefault -1
