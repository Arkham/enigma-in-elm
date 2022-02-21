module Main exposing (main)

import Browser
import Dict
import Dict.Any as AnyDict exposing (AnyDict)
import Enigma exposing (Enigma)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra
import Plugboard exposing (Plugboard)
import Reflector exposing (Reflector)
import Rotor exposing (ChosenRotor, Rotor)


type alias ValidationErrors =
    AnyDict String FormField (List String)


type alias Model =
    { input : String
    , enigma : Enigma
    , leftRotorPosition : Int
    , leftRotorSetting : Int
    , middleRotorPosition : Int
    , middleRotorSetting : Int
    , rightRotorPosition : Int
    , rightRotorSetting : Int
    , plugboardInput : String
    , validationErrors : ValidationErrors
    }


type WhichRotor
    = LeftRotor
    | MiddleRotor
    | RightRotor



-- Validation logic


type FormField
    = PlugboardInput


fieldToString : FormField -> String
fieldToString field =
    case field of
        PlugboardInput ->
            "PlugboardInput"


type Msg
    = InputChanged String
    | RotorChanged WhichRotor String
    | RotorPositionChanged WhichRotor String
    | RingSettingChanged WhichRotor String
    | ReflectorChanged String
    | PlugboardChanged String


main : Program () Model Msg
main =
    let
        ( leftRotorPosition, leftRotorSetting ) =
            ( 1, 1 )

        ( middleRotorPosition, middleRotorSetting ) =
            ( 1, 1 )

        ( rightRotorPosition, rightRotorSetting ) =
            ( 1, 1 )
    in
    Browser.element
        { init =
            \_ ->
                ( { input = "Hello darkness my old friend"
                  , enigma =
                        { leftRotor = Rotor.choose Rotor.rotorI leftRotorPosition leftRotorSetting
                        , middleRotor = Rotor.choose Rotor.rotorII middleRotorPosition middleRotorSetting
                        , rightRotor = Rotor.choose Rotor.rotorIII rightRotorPosition rightRotorSetting
                        , reflector = Reflector.reflectorB
                        , plugboard = Plugboard.empty
                        }
                  , leftRotorPosition = leftRotorPosition
                  , leftRotorSetting = leftRotorSetting
                  , middleRotorPosition = middleRotorPosition
                  , middleRotorSetting = middleRotorSetting
                  , rightRotorPosition = rightRotorPosition
                  , rightRotorSetting = rightRotorSetting
                  , plugboardInput = ""
                  , validationErrors = AnyDict.empty fieldToString
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


{-| VIEW
-}
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
            [ Attrs.style "display" "flex" ]
            [ Html.label [] [ Html.text "Rotor 1" ]
            , Html.select [ Events.onInput (RotorChanged LeftRotor) ]
                (rotorOptions model.enigma.leftRotor)
            , viewRotorPosition model.leftRotorPosition (RotorPositionChanged LeftRotor)
            , viewRingSetting model.leftRotorSetting (RingSettingChanged LeftRotor)
            ]
        , Html.div
            [ Attrs.style "display" "flex" ]
            [ Html.label [] [ Html.text "Rotor 2" ]
            , Html.select [ Events.onInput (RotorChanged MiddleRotor) ]
                (rotorOptions model.enigma.middleRotor)
            , viewRotorPosition model.middleRotorPosition (RotorPositionChanged MiddleRotor)
            , viewRingSetting model.middleRotorSetting (RingSettingChanged MiddleRotor)
            ]
        , Html.div
            [ Attrs.style "display" "flex" ]
            [ Html.label [] [ Html.text "Rotor 3" ]
            , Html.select [ Events.onInput (RotorChanged RightRotor) ]
                (rotorOptions model.enigma.rightRotor)
            , viewRotorPosition model.rightRotorPosition (RotorPositionChanged RightRotor)
            , viewRingSetting model.rightRotorSetting (RingSettingChanged RightRotor)
            ]
        , Html.div
            []
            [ Html.label [] [ Html.text "Reflector" ]
            , Html.select [ Events.onInput ReflectorChanged ]
                (reflectorOptions model.enigma.reflector)
            ]
        , Html.div
            []
            [ Html.label [] [ Html.text "Plugboard" ]
            , Html.input [ Attrs.value model.plugboardInput, Events.onInput PlugboardChanged ] []
            , viewValidationErrors model.validationErrors PlugboardInput
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


reflectorOptions : Reflector -> List (Html msg)
reflectorOptions chosenOne =
    List.map
        (\reflector ->
            let
                selectedAttrs =
                    if chosenOne == reflector then
                        [ Attrs.selected True ]

                    else
                        []
            in
            Html.option (Attrs.value reflector.name :: selectedAttrs)
                [ Html.text reflector.name ]
        )
        Reflector.allReflectors


viewRotorPosition : Int -> (String -> msg) -> Html msg
viewRotorPosition position onInput =
    Html.div []
        [ Html.input
            [ Attrs.type_ "number"
            , Attrs.min "1"
            , Attrs.max "26"
            , Attrs.value (String.fromInt position)
            , Events.onInput onInput
            ]
            []
        , Html.span [] [ Html.text <| indexToLetter position ]
        ]


viewRingSetting : Int -> (String -> msg) -> Html msg
viewRingSetting setting onInput =
    Html.div []
        [ Html.input
            [ Attrs.type_ "number"
            , Attrs.min "1"
            , Attrs.max "26"
            , Attrs.value (String.fromInt setting)
            , Events.onInput onInput
            ]
            []
        , Html.span [] [ Html.text <| indexToLetter setting ]
        ]


indexToLetter : Int -> String
indexToLetter value =
    Char.fromCode (value + 64)
        |> String.fromChar


viewValidationErrors : ValidationErrors -> FormField -> Html msg
viewValidationErrors validationErrors field =
    case AnyDict.get field validationErrors |> Maybe.withDefault [] of
        [] ->
            Html.text ""

        errors ->
            Html.ul [] <|
                List.map (\e -> Html.li [] [ Html.text e ]) errors


decorateOutput : String -> String
decorateOutput output =
    String.toList output
        |> List.Extra.greedyGroupsOf 5
        |> List.map String.fromList
        |> String.join " "


{-| UPDATE
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged value ->
            ( { model | input = value }, Cmd.none )

        RotorChanged LeftRotor chosen ->
            ( updateEnigma
                (\enigma ->
                    case
                        Rotor.chooseFromString
                            chosen
                            model.leftRotorPosition
                            model.leftRotorSetting
                    of
                        Just new ->
                            { enigma | leftRotor = new }

                        Nothing ->
                            enigma
                )
                model
            , Cmd.none
            )

        RotorChanged MiddleRotor chosen ->
            ( updateEnigma
                (\enigma ->
                    case
                        Rotor.chooseFromString
                            chosen
                            model.middleRotorPosition
                            model.middleRotorSetting
                    of
                        Just new ->
                            { enigma | middleRotor = new }

                        Nothing ->
                            enigma
                )
                model
            , Cmd.none
            )

        RotorChanged RightRotor chosen ->
            ( updateEnigma
                (\enigma ->
                    case
                        Rotor.chooseFromString
                            chosen
                            model.rightRotorPosition
                            model.rightRotorSetting
                    of
                        Just new ->
                            { enigma | rightRotor = new }

                        Nothing ->
                            enigma
                )
                model
            , Cmd.none
            )

        RotorPositionChanged LeftRotor new ->
            ( case clampIndex new of
                Just v ->
                    let
                        updatedModel =
                            updateEnigma
                                (\enigma ->
                                    { enigma
                                        | leftRotor =
                                            Rotor.choose
                                                enigma.leftRotor.rotor
                                                v
                                                model.leftRotorSetting
                                    }
                                )
                                model
                    in
                    { updatedModel | leftRotorPosition = v }

                Nothing ->
                    model
            , Cmd.none
            )

        RotorPositionChanged MiddleRotor new ->
            ( case clampIndex new of
                Just v ->
                    let
                        updatedModel =
                            updateEnigma
                                (\enigma ->
                                    { enigma
                                        | middleRotor =
                                            Rotor.choose
                                                enigma.middleRotor.rotor
                                                v
                                                model.middleRotorSetting
                                    }
                                )
                                model
                    in
                    { updatedModel | middleRotorPosition = v }

                Nothing ->
                    model
            , Cmd.none
            )

        RotorPositionChanged RightRotor new ->
            ( case clampIndex new of
                Just v ->
                    let
                        updatedModel =
                            updateEnigma
                                (\enigma ->
                                    { enigma
                                        | rightRotor =
                                            Rotor.choose
                                                enigma.rightRotor.rotor
                                                v
                                                model.rightRotorSetting
                                    }
                                )
                                model
                    in
                    { updatedModel | rightRotorPosition = v }

                Nothing ->
                    model
            , Cmd.none
            )

        RingSettingChanged LeftRotor new ->
            ( case clampIndex new of
                Just v ->
                    let
                        updatedModel =
                            updateEnigma
                                (\enigma ->
                                    { enigma
                                        | leftRotor =
                                            Rotor.choose
                                                enigma.leftRotor.rotor
                                                model.leftRotorPosition
                                                v
                                    }
                                )
                                model
                    in
                    { updatedModel | leftRotorSetting = v }

                Nothing ->
                    model
            , Cmd.none
            )

        RingSettingChanged MiddleRotor new ->
            ( case clampIndex new of
                Just v ->
                    let
                        updatedModel =
                            updateEnigma
                                (\enigma ->
                                    { enigma
                                        | middleRotor =
                                            Rotor.choose
                                                enigma.middleRotor.rotor
                                                model.middleRotorPosition
                                                v
                                    }
                                )
                                model
                    in
                    { updatedModel | middleRotorSetting = v }

                Nothing ->
                    model
            , Cmd.none
            )

        RingSettingChanged RightRotor new ->
            ( case clampIndex new of
                Just v ->
                    let
                        updatedModel =
                            updateEnigma
                                (\enigma ->
                                    { enigma
                                        | rightRotor =
                                            Rotor.choose
                                                enigma.rightRotor.rotor
                                                model.rightRotorPosition
                                                v
                                    }
                                )
                                model
                    in
                    { updatedModel | rightRotorSetting = v }

                Nothing ->
                    model
            , Cmd.none
            )

        ReflectorChanged chosen ->
            ( updateEnigma
                (\enigma ->
                    case Dict.get chosen Reflector.reflectorsByName of
                        Just newReflector ->
                            { enigma | reflector = newReflector }

                        Nothing ->
                            enigma
                )
                model
            , Cmd.none
            )

        PlugboardChanged input ->
            let
                updatedModel =
                    case Plugboard.parse input of
                        Ok newPlugboard ->
                            updateEnigma
                                (\enigma ->
                                    { enigma | plugboard = newPlugboard }
                                )
                                { model
                                    | validationErrors =
                                        AnyDict.remove PlugboardInput model.validationErrors
                                }

                        Err errors ->
                            { model
                                | validationErrors =
                                    AnyDict.insert PlugboardInput errors model.validationErrors
                            }
            in
            ( { updatedModel | plugboardInput = input }, Cmd.none )


clampIndex : String -> Maybe Int
clampIndex value =
    value
        |> String.toInt
        |> Maybe.map (clamp 1 26)


{-| HELPERS
-}
updateEnigma : (Enigma -> Enigma) -> Model -> Model
updateEnigma fun model =
    { model | enigma = fun model.enigma }
