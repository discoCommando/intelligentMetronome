module Metronomes exposing (..)

import Metronome
import Types
import Return exposing (..)
import Json.Decode exposing (field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Platform.Cmd


type alias WorkingState =
    { actual : Metronome.Model
    , metronomes : List Metronome.Model
    , stopped : Bool
    }


type Status
    = Idle
    | Working WorkingState


type alias Model =
    { song : Types.Song
    , status : Status
    }


song =
    case
        Json.Decode.decodeString Types.decodeSong """
            {
            "track": "Song",
            "artist": "Test",
            "blocks": [
                {
                  "tempo" : 120,
                  "accents": [
                    2, 3
                  ],
                  "maybeCount" : null
                },
                {
                  "tempo" : 200,
                  "accents": [
                    2, 3, 2, 4
                  ],
                  "maybeCount" : 5
                }
            ]
        }
"""
    of
        Result.Ok a ->
            a

        Result.Err x ->
            Debug.crash x


type Msg
    = MetronomeMsg Metronome.Msg
    | Start
    | Stop
    | Reset
    | Next


mapBlock : Types.Block -> Metronome.Model
mapBlock block =
    { block = block, status = Metronome.Idle }


update : Msg -> Model -> Return Msg Model
update msg model =
    case model.status of
        Idle ->
            case model.song.blocks of
                [] ->
                    model |> Return.singleton

                block :: rest ->
                    case msg of
                        Start ->
                            Metronome.update Metronome.Start (mapBlock block)
                                |> Return.mapCmd MetronomeMsg
                                |> Return.map
                                    (\metronomeModel ->
                                        { model | status = Working { actual = metronomeModel, metronomes = List.map mapBlock rest, stopped = False } }
                                    )

                        _ ->
                            model |> Return.singleton

        --model
        --    |> Return.singleton
        Working ws ->
            case ws.stopped of
                False ->
                    case msg of
                        MetronomeMsg msg ->
                            let
                                ( metronomeModel, metronomeCmd ) =
                                    Metronome.update msg ws.actual
                            in
                                case metronomeModel.status of
                                    Metronome.Idle ->
                                        case ws.metronomes of
                                            [] ->
                                                { model | status = Idle }
                                                    |> Return.singleton
                                                    |> Return.command (Platform.Cmd.map MetronomeMsg metronomeCmd)

                                            x :: xs ->
                                                { model | status = Working { actual = x, metronomes = xs, stopped = False } }
                                                    |> Return.singleton

                                    mStatus ->
                                        { model | status = Working { ws | actual = metronomeModel } }
                                            |> Return.singleton
                                            |> Return.command (Platform.Cmd.map MetronomeMsg metronomeCmd)

                        Stop ->
                            Metronome.update Metronome.Stop ws.actual
                                |> Return.mapCmd MetronomeMsg
                                |> Return.map
                                    (\metronomeModel ->
                                        { model | status = Working { ws | actual = metronomeModel, stopped = True } }
                                    )

                        Next ->
                            case ws.metronomes of
                                [] ->
                                    { model | status = Idle }
                                        |> Return.singleton

                                x :: xs ->
                                    Metronome.update Metronome.Start x
                                        |> Return.mapCmd MetronomeMsg
                                        |> Return.map
                                            (\metronomeModel ->
                                                { model | status = Working { ws | actual = metronomeModel, metronomes = xs, stopped = False } }
                                            )

                        _ ->
                            model
                                |> Return.singleton

                True ->
                    case msg of
                        Start ->
                            Metronome.update Metronome.Start ws.actual
                                |> Return.mapCmd MetronomeMsg
                                |> Return.map
                                    (\metronomeModel ->
                                        { model | status = Working { ws | actual = metronomeModel, stopped = False } }
                                    )

                        Reset ->
                            { model | status = Idle }
                                |> Return.singleton

                        Next ->
                            case ws.metronomes of
                                [] ->
                                    { model | status = Idle }
                                        |> Return.singleton

                                x :: xs ->
                                    { model | status = Working { ws | actual = x, metronomes = xs, stopped = True } }
                                        |> Return.singleton

                        _ ->
                            model
                                |> Return.singleton


view : Model -> Html Msg
view model =
    div []
        [ Html.text <| toString model
        , Html.p [] []
        , Html.button
            [ case model.status of
                Idle ->
                    Html.Events.onClick Start

                Working ws ->
                    case ws.stopped of
                        True ->
                            Html.Events.onClick Start

                        False ->
                            Html.Events.onClick Stop
            ]
            [ case model.status of
                Idle ->
                    Html.text "Start"

                Working ws ->
                    case ws.stopped of
                        True ->
                            Html.text "Resume"

                        False ->
                            Html.text "Stop"
            ]
        , Html.button
            [ case model.status of
                Idle ->
                    Html.Attributes.disabled True

                Working ws ->
                    case ws.stopped of
                        True ->
                            Html.Events.onClick Reset

                        False ->
                            Html.Attributes.disabled True
            ]
            [ Html.text "Reset" ]
        , Html.button
            [ case model.status of
                Idle ->
                    Html.Attributes.disabled True

                Working ws ->
                    Html.Events.onClick Next
            ]
            [ Html.text "Next" ]
        ]


init : Return Msg Model
init =
    { song = song
    , status = Idle
    }
        |> Return.singleton


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Idle ->
            Sub.none

        Working ws ->
            Sub.map MetronomeMsg <| Metronome.subscriptions ws.actual


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
