module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select
import Json.Encode as E
import Task


type FileState
    = NotLoaded
    | Loaded LoadedState


type alias LoadedState =
    { previewRows : List String
    , headers : List String
    , allRows : List String
    }


type Msg
    = BrowseButtonPressed
    | FileLoaded File
    | FileContentLoaded String
    | ConvertButtonPressed


type alias Model =
    { file : FileState
    }


noOfPreviewRows =
    10


delimiter =
    ";"


initialModel : Model
initialModel =
    { file = NotLoaded }


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


view model =
    { title = "CSV->JSON"
    , body =
        [ layout [ Font.size 13 ] <|
            column
                [ width fill
                , height fill
                , scrollbars
                ]
                [ header, content model ]
        ]
    }


header =
    row
        [ width fill
        , paddingXY 16 8
        , Background.color (rgb255 60 60 60)
        , Font.color (rgb 1 1 1)
        , spacing 8
        ]
        [ Input.button
            [ Border.width 1
            , Border.rounded 4
            , paddingXY 8 4
            ]
            { label = text "Browse CSV", onPress = Just BrowseButtonPressed }
        , Input.button
            [ Border.width 1
            , Border.rounded 4
            , paddingXY 8 4
            ]
            { label = text "Convert", onPress = Just ConvertButtonPressed }
        ]


content model =
    case model.file of
        NotLoaded ->
            text "No file loaded"

        Loaded loadState ->
            column
                [ padding 8
                , spacing 16
                , width fill
                , height fill
                , scrollbars
                ]
                [ renderRawInput loadState.previewRows
                , renderOutput loadState
                ]


renderOutput loadState =
    let
        headers : List String
        headers =
            List.take 1 loadState.previewRows
                |> List.head
                |> Maybe.map (String.split delimiter)
                |> Maybe.withDefault []

        previewRows =
            List.drop 1 loadState.previewRows
    in
    row
        [ width fill
        , height fill
        , spacing 32
        ]
        [ renderParsedCSV headers previewRows
        , renderJsonOutput headers previewRows
        ]


renderParsedCSV headers previewRows =
    section "Parsed CSV"
        (table []
            { columns = List.indexedMap createHeader headers
            , data = previewRows
            }
        )


createHeader : Int -> String -> Column String Msg
createHeader index name =
    { header = el [ paddingXY 8 4 ] (text name)
    , width = shrink
    , view =
        \rowString ->
            el [ paddingXY 8 2 ]
                (rowString
                    |> String.split delimiter
                    |> List.drop index
                    |> List.head
                    |> Maybe.withDefault " "
                    |> text
                )
    }


renderJsonOutput : List String -> List String -> Element msg
renderJsonOutput headers contentRows =
    section "Json out"
        (text (buildJsonString headers contentRows))


buildJsonString : List String -> List String -> String
buildJsonString headers contentRows =
    contentRows
        |> List.map (createJsonObject headers)
        |> E.list identity
        |> E.encode 2


createJsonObject headers stringRow =
    E.object
        (List.map2
            (\name stringValue ->
                ( name, E.string stringValue )
            )
            headers
            (String.split delimiter stringRow)
        )


renderRawInput : List String -> Element msg
renderRawInput previewRows =
    section "Raw input (first 10 lines)"
        (column
            [ width fill
            , height fill
            ]
            (List.map text previewRows)
        )


section name elm =
    column [ spacing 4, width fill, height fill, scrollbars ]
        [ text name
        , el
            [ width fill
            , height fill
            , scrollbars
            , padding 8
            , Background.color (rgb255 240 240 240)
            , Border.rounded 10
            , Border.width 1
            , Border.color (rgb255 220 220 220)
            ]
            elm
        ]


update msg model =
    case msg of
        BrowseButtonPressed ->
            ( model, File.Select.file [] FileLoaded )

        FileLoaded file ->
            ( model, Task.perform FileContentLoaded (File.toString file) )

        FileContentLoaded str ->
            let
                rows =
                    String.lines str
            in
            ( { file =
                    Loaded
                        { previewRows = List.take noOfPreviewRows rows
                        , headers =
                            List.head rows
                                |> Maybe.map (String.split delimiter)
                                |> Maybe.withDefault []
                        , allRows = List.drop 1 rows
                        }
              }
            , Cmd.none
            )

        ConvertButtonPressed ->
            ( model
            , case model.file of
                NotLoaded ->
                    Cmd.none

                Loaded loadState ->
                    let
                        jsonString =
                            buildJsonString loadState.headers loadState.allRows
                    in
                    File.Download.string "xyz.json" "application/json" jsonString
            )
