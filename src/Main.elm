module Main exposing (main)

import Browser
import Enigma
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra


type alias Model =
    { input : String }


type Msg
    = InputChanged String


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { input = "" }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    let
        enigma =
            Enigma.makeOne

        ( encoded, _ ) =
            Enigma.encodeString model.input enigma
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
        , Html.p [ Attrs.class "encoded" ] [ Html.text (decorateOutput encoded) ]
        ]


decorateOutput : String -> String
decorateOutput output =
    String.toList output
        |> List.Extra.greedyGroupsOf 4
        |> List.map String.fromList
        |> String.join " "


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged value ->
            ( { model | input = value }, Cmd.none )
