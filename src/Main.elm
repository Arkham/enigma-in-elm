module Main exposing (main)

import Browser
import Enigma exposing (Enigma)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra
import Plugboard exposing (Plugboard)
import Reflector exposing (Reflector)
import Rotor exposing (ChosenRotor, Rotor)


type alias Model =
    { input : String
    , enigma : Enigma
    }


type Msg
    = InputChanged String
    | LeftRotorChanged String
    | MiddleRotorChanged String
    | RightRotorChanged String


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { input = ""
                  , enigma =
                        { leftRotor = Rotor.choose Rotor.rotorI 0
                        , middleRotor = Rotor.choose Rotor.rotorII 0
                        , rightRotor = Rotor.choose Rotor.rotorIII 0
                        , reflector = Reflector.reflectorB
                        , plugboard = Plugboard.empty
                        }
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    let
        ( encoded, _ ) =
            Enigma.encodeString model.input model.enigma
    in
    Html.section []
        [ Html.h1 [] [ Html.text "Enigma in Elm" ]
        , Html.textarea
            [ Attrs.cols 40
            , Attrs.rows 10
            , Attrs.value model.input
            , Events.onInput InputChanged
            ]
            []
        , Html.div
            []
            [ Html.label [] [ Html.text "Rotor 1" ]
            , Html.select [ Events.onInput LeftRotorChanged ]
                (rotorOptions model.enigma.leftRotor)
            ]
        , Html.div
            []
            [ Html.label [] [ Html.text "Rotor 2" ]
            , Html.select [ Events.onInput MiddleRotorChanged ]
                (rotorOptions model.enigma.middleRotor)
            ]
        , Html.div
            []
            [ Html.label [] [ Html.text "Rotor 3" ]
            , Html.select [ Events.onInput RightRotorChanged ]
                (rotorOptions model.enigma.rightRotor)
            ]
        , Html.p [ Attrs.class "encoded" ] [ Html.text (decorateOutput encoded) ]
        ]


rotorOptions : ChosenRotor -> List (Html msg)
rotorOptions chosenOne =
    List.map
        (\rotor ->
            let
                selectedAttrs =
                    if chosenOne.rotor == rotor then
                        [ Attrs.selected True ]

                    else
                        []
            in
            Html.option (Attrs.value rotor.name :: selectedAttrs)
                [ Html.text rotor.name ]
        )
        Rotor.allRotors


decorateOutput : String -> String
decorateOutput output =
    String.toList output
        |> List.Extra.greedyGroupsOf 4
        |> List.map String.fromList
        |> String.join " "


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ enigma } as model) =
    case msg of
        InputChanged value ->
            ( { model | input = value }, Cmd.none )

        LeftRotorChanged chosen ->
            let
                updatedModel =
                    case Rotor.chooseFromString chosen 0 of
                        Just newRotor ->
                            let
                                updatedEnigma =
                                    { enigma | leftRotor = newRotor }
                            in
                            { model | enigma = updatedEnigma }

                        Nothing ->
                            model
            in
            ( updatedModel, Cmd.none )

        MiddleRotorChanged chosen ->
            let
                updatedModel =
                    case Rotor.chooseFromString chosen 0 of
                        Just newRotor ->
                            let
                                updatedEnigma =
                                    { enigma | middleRotor = newRotor }
                            in
                            { model | enigma = updatedEnigma }

                        Nothing ->
                            model
            in
            ( updatedModel, Cmd.none )

        RightRotorChanged chosen ->
            let
                updatedModel =
                    case Rotor.chooseFromString chosen 0 of
                        Just newRotor ->
                            let
                                updatedEnigma =
                                    { enigma | rightRotor = newRotor }
                            in
                            { model | enigma = updatedEnigma }

                        Nothing ->
                            model
            in
            ( updatedModel, Cmd.none )
