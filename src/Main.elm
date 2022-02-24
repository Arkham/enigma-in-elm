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

        initialPlugboard =
            "ab cd ef gh"
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
                        , plugboard =
                            case Plugboard.parse initialPlugboard of
                                Ok new ->
                                    new

                                _ ->
                                    Plugboard.empty
                        }
                  , leftRotorPosition = leftRotorPosition
                  , leftRotorSetting = leftRotorSetting
                  , middleRotorPosition = middleRotorPosition
                  , middleRotorSetting = middleRotorSetting
                  , rightRotorPosition = rightRotorPosition
                  , rightRotorSetting = rightRotorSetting
                  , plugboardInput = initialPlugboard
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
    Html.section [ Attrs.class "max-w-md xl:max-w-6xl mx-auto" ]
        [ Html.h1 [ Attrs.class "relative left-[-4px] text-6xl uppercase my-4" ] [ Html.text "Enigma" ]
        , Html.div [ Attrs.class "grid grid-cols-1 xl:grid-cols-3 gap-6" ]
            [ Html.div [ Attrs.class "flex flex-col" ]
                [ Html.label
                    [ Attrs.class "font-semibold uppercase text-xs text-neutral-400"
                    ]
                    [ Html.text "Input" ]
                , Html.textarea
                    [ Attrs.class "flex-grow resize-none"
                    , Attrs.cols 40
                    , Attrs.rows 3
                    , Attrs.autofocus True
                    , Attrs.value model.input
                    , Events.onInput InputChanged
                    ]
                    []
                ]
            , Html.div [ Attrs.class "flex flex-col" ]
                [ Html.div
                    [ Attrs.class "grid grid-cols-3 border-b pb-3" ]
                    [ viewRotorSelect "Rotor 1" model.enigma.leftRotor (RotorChanged LeftRotor)
                    , viewRingSetting model.leftRotorSetting (RingSettingChanged LeftRotor)
                    , viewRotorPosition model.leftRotorPosition (RotorPositionChanged LeftRotor)
                    ]
                , Html.div
                    [ Attrs.class "grid grid-cols-3 border-b py-3" ]
                    [ viewRotorSelect "Rotor 2" model.enigma.middleRotor (RotorChanged MiddleRotor)
                    , viewRingSetting model.middleRotorSetting (RingSettingChanged MiddleRotor)
                    , viewRotorPosition model.middleRotorPosition (RotorPositionChanged MiddleRotor)
                    ]
                , Html.div
                    [ Attrs.class "grid grid-cols-3 border-b py-3" ]
                    [ viewRotorSelect "Rotor 3" model.enigma.rightRotor (RotorChanged RightRotor)
                    , viewRingSetting model.rightRotorSetting (RingSettingChanged RightRotor)
                    , viewRotorPosition model.rightRotorPosition (RotorPositionChanged RightRotor)
                    ]
                , Html.div [ Attrs.class "flex flex-col py-3 border-b" ]
                    [ Html.label
                        [ Attrs.class "pl-3 uppercase text-xs text-neutral-400"
                        ]
                        [ Html.text "Reflector" ]
                    , Html.select
                        [ Attrs.class "border-0 py-0 font-semibold"
                        , Events.onInput ReflectorChanged
                        ]
                        (reflectorOptions model.enigma.reflector)
                    ]
                , Html.div [ Attrs.class "flex flex-none flex-col pt-3 px-3" ]
                    [ Html.label
                        [ Attrs.class "uppercase text-xs text-neutral-400"
                        ]
                        [ Html.text "Plugboard" ]
                    , Html.input
                        [ Attrs.class "border-1 p-1"
                        , Attrs.type_ "text"
                        , Attrs.value model.plugboardInput
                        , Events.onInput PlugboardChanged
                        ]
                        []
                    , viewValidationErrors model.validationErrors PlugboardInput
                    ]
                ]
            , Html.div [ Attrs.class "flex flex-col" ]
                [ Html.label
                    [ Attrs.class "font-semibold uppercase text-xs text-neutral-400"
                    ]
                    [ Html.text "Output" ]
                , Html.div [ Attrs.class "flex-grow min-h-[100px] font-semibold text-2xl border-2 py-1 px-2 border-dotted" ] [ Html.text (decorateOutput encoded) ]
                ]
            ]
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


viewRotorSelect : String -> ChosenRotor -> (String -> msg) -> Html msg
viewRotorSelect label chosenOne onInput =
    Html.div [ Attrs.class "flex flex-col border-r" ]
        [ Html.label
            [ Attrs.class "pl-3 uppercase text-xs text-neutral-400"
            ]
            [ Html.text label ]
        , Html.select
            [ Attrs.class "border-0 py-0 font-semibold"
            , Events.onInput onInput
            ]
            (rotorOptions chosenOne)
        ]


indexOptions : Int -> List (Html msg)
indexOptions chosenOne =
    List.map
        (\index ->
            let
                value =
                    String.fromInt index

                selectedAttrs =
                    if chosenOne == index then
                        [ Attrs.selected True ]

                    else
                        []
            in
            Html.option (Attrs.value value :: selectedAttrs)
                [ Html.text <| value ++ " - " ++ indexToLetter index ]
        )
        (List.range 1 26)


viewRotorPosition : Int -> (String -> msg) -> Html msg
viewRotorPosition position onInput =
    Html.div [ Attrs.class "flex flex-col" ]
        [ Html.label
            [ Attrs.class "pl-3 uppercase text-xs text-neutral-400"
            ]
            [ Html.text "Position" ]
        , Html.select
            [ Attrs.class "border-0 py-0 font-semibold"
            , Events.onInput onInput
            ]
            (indexOptions position)
        ]


viewRingSetting : Int -> (String -> msg) -> Html msg
viewRingSetting setting onInput =
    Html.div [ Attrs.class "flex flex-col border-r" ]
        [ Html.label
            [ Attrs.class "pl-3 uppercase text-xs text-neutral-400"
            ]
            [ Html.text "Setting" ]
        , Html.select
            [ Attrs.class "border-0 py-0 font-semibold"
            , Events.onInput onInput
            ]
            (indexOptions setting)
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
                List.map (\e -> Html.li [] [ Html.p [ Attrs.class "text-xs break-normal" ] [ Html.text e ] ]) errors


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
