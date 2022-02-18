module Plugboard exposing (Plugboard, empty, parse, swap)

import Dict exposing (Dict)
import List.Extra
import Parser as P exposing ((|.), (|=), Parser, Step)


type Plugboard
    = Plugboard (Dict Char Char)


empty : Plugboard
empty =
    Plugboard Dict.empty


swap : Plugboard -> Char -> Char
swap (Plugboard mapping) input =
    Dict.get input mapping
        |> Maybe.withDefault input



-- Input parser


parse : String -> Result (List String) Plugboard
parse input =
    case P.run parser input of
        Ok plugboard ->
            Ok plugboard

        Err errors ->
            errors
                |> List.filterMap errorToString
                |> List.sort
                |> List.Extra.unique
                |> Err


errorToString : P.DeadEnd -> Maybe String
errorToString { problem } =
    case problem of
        P.Problem string ->
            Just string

        _ ->
            Just "I'm expecting a list of pairs of letters to be swapped (e.g. 'ab cd ef')"


parser : Parser Plugboard
parser =
    P.map Plugboard (P.loop Dict.empty parserHelp)


type alias CharDict =
    Dict Char Char


pairParser : CharDict -> Parser CharDict
pairParser acc =
    let
        twoChars =
            P.getChompedString <|
                P.succeed ()
                    |. P.chompIf Char.isAlpha
                    |. P.chompIf Char.isAlpha
    in
    P.andThen
        (\parsed ->
            case String.toList (String.toUpper parsed) of
                [ src, dst ] ->
                    if src == dst || Dict.member src acc || Dict.member dst acc then
                        P.problem "Pairs of letters to be swapped must be unique"

                    else
                        P.succeed
                            (acc
                                |> Dict.insert src dst
                                |> Dict.insert dst src
                            )

                -- this should never happen
                other ->
                    P.problem ("I expected to find two characters, but instead I found: '" ++ parsed ++ "'")
        )
        twoChars


parserHelp : CharDict -> Parser (Step CharDict CharDict)
parserHelp acc =
    P.oneOf
        [ P.succeed (\newAcc -> P.Loop newAcc)
            |= pairParser acc
            |. P.oneOf
                [ P.succeed ()
                    |. P.chompIf (\c -> c == ' ')
                    |. P.spaces
                , P.end
                ]
        , P.succeed Plugboard
            |> P.map (\_ -> P.Done acc)
        ]
