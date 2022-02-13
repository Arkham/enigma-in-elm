module Plugboard exposing (Plugboard, empty, parser, swap)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser, Step)


type Plugboard
    = Plugboard (Dict Char Char)


empty : Plugboard
empty =
    Plugboard Dict.empty


fromPairs : List ( Char, Char ) -> Plugboard
fromPairs pairs =
    Plugboard <|
        List.foldl
            (\( src, dst ) acc ->
                acc
                    |> Dict.insert src dst
                    |> Dict.insert dst src
            )
            Dict.empty
            pairs


swap : Plugboard -> Char -> Char
swap (Plugboard mapping) input =
    Dict.get input mapping
        |> Maybe.withDefault input


parser : Parser Plugboard
parser =
    P.map fromPairs (P.loop [] parserHelp)


type alias Pair =
    ( Char, Char )


pairParser : Parser Pair
pairParser =
    (P.getChompedString <|
        P.succeed ()
            |. P.chompIf Char.isAlpha
            |. P.chompIf Char.isAlpha
    )
        |> P.andThen
            (\parsed ->
                case String.toList parsed of
                    [ src, dst ] ->
                        P.succeed ( Char.toUpper src, Char.toUpper dst )

                    _ ->
                        P.problem "Did not find pair"
            )


parserHelp : List Pair -> Parser (Step (List Pair) (List Pair))
parserHelp acc =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: acc))
            |= pairParser
            |. P.spaces
        , P.succeed Plugboard
            |> P.map (\_ -> P.Done (List.reverse acc))
        ]
