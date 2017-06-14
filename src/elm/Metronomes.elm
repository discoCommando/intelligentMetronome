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
import ViewBlock


type alias WorkingState =
    { paused : Bool
    , previous : List Metronome.Model
    , actual : Metronome.Model
    , next : List Metronome.Model
    }


type Status
    = Idle
    | Working WorkingState


type alias Model =
    { metronomes : List Metronome.Model
    , track : String
    , artist : String
    , status : Status
    }


mapBlock : Types.Block -> Metronome.Model
mapBlock block =
    { block = block
    , status = Metronome.Idle
    , temps =
        { tempo = Basics.toString block.tempo
        , count =
            case block.maybeCount of
                Just a ->
                    Basics.toString a

                Nothing ->
                    "1"
        }
    }


songToModel : Types.Song -> Model
songToModel song =
    { status = Idle
    , track = song.track
    , artist = song.artist
    , metronomes = List.map mapBlock song.blocks
    }


modelToSong : Model -> Types.Song
modelToSong model =
    { track = model.track
    , artist = model.artist
    , blocks = List.map .block model.metronomes
    }


exampleSong : Types.Song
exampleSong =
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
                  "maybeCount" : 3
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
    = MetronomeMsg Int Metronome.Msg
    | Start
    | Pause
    | Stop
    | Next
    | TickMsg Metronome.Msg


insert : Int -> a -> List a -> List a
insert index a list =
    case list of
        [] ->
            []

        x :: xs ->
            if index <= 0 then
                a :: xs
            else
                x :: insert (index - 1) a xs


at : Int -> List a -> Maybe a
at i list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if i <= 0 then
                Just x
            else
                at (i - 1) xs


remove : Int -> List a -> List a
remove i list =
    case list of
        [] ->
            []

        x :: xs ->
            if (i <= 0) then
                xs
            else
                x :: remove (i - 1) xs


insertWs : Int -> a -> { m | previous : List a, actual : a, next : List a } -> { m | previous : List a, actual : a, next : List a }
insertWs i a m =
    if (i < List.length m.previous) then
        { m | previous = insert i a m.previous }
    else if (i == List.length m.previous) then
        { m | actual = a }
    else
        { m | next = insert (i - 1 - List.length m.previous) a m.next }


atWs : Int -> { m | previous : List a, actual : a, next : List a } -> Maybe a
atWs i m =
    if (i < List.length m.previous) then
        at i m.previous
    else if (i == List.length m.previous) then
        Just m.actual
    else
        at (i - 1 - List.length m.previous) m.next


getActualIndex : WorkingState -> Int
getActualIndex ws =
    List.length ws.previous


update : Msg -> Model -> Return Msg Model
update msg model =
    case model.status of
        Idle ->
            case msg of
                Start ->
                    case model.metronomes of
                        [] ->
                            model |> Return.singleton

                        metronomeModel :: rest ->
                            Metronome.update Metronome.Start metronomeModel
                                |> Return.mapCmd (MetronomeMsg 0)
                                |> Return.map
                                    (\changedMetronomeModel ->
                                        { model | status = Working { previous = [], actual = changedMetronomeModel, next = List.map Metronome.makeFinished rest, paused = False } }
                                    )

                Next ->
                    case model.metronomes of
                        [] ->
                            model |> Return.singleton

                        metronomeModel :: rest ->
                            Metronome.update Metronome.Start metronomeModel
                                |> Return.dropCmd
                                |> Return.mapCmd (MetronomeMsg 0)
                                |> Return.map
                                    (\changedMetronomeModel ->
                                        { model | status = Working { previous = [], actual = changedMetronomeModel, next = List.map Metronome.makeFinished rest, paused = True } }
                                    )
                                |> Return.andThen (\m -> update Next m)

                MetronomeMsg i msg ->
                    case at i model.metronomes of
                        Nothing ->
                            model
                                |> Return.singleton

                        Just metronomeModel ->
                            case msg of
                                Metronome.ViewMsg (ViewBlock.RemoveAll) ->
                                    { model | metronomes = remove i model.metronomes }
                                        |> Return.singleton

                                _ ->
                                    let
                                        ( changedMetronomeModel, metronomeCmd ) =
                                            Metronome.update msg metronomeModel
                                    in
                                        { model | metronomes = insert i changedMetronomeModel model.metronomes }
                                            |> Return.singleton
                                            |> Return.command (Platform.Cmd.map (MetronomeMsg i) metronomeCmd)

                _ ->
                    model |> Return.singleton

        Working ws ->
            case ws.paused of
                False ->
                    case msg of
                        MetronomeMsg i msg ->
                            case atWs i ws of
                                Nothing ->
                                    model
                                        |> Return.singleton

                                Just metronomeModel ->
                                    let
                                        ( changedMetronomeModel, metronomeCmd ) =
                                            Metronome.update msg metronomeModel
                                    in
                                        { model | status = Working <| insertWs i changedMetronomeModel ws }
                                            |> Return.singleton
                                            |> Return.command (Platform.Cmd.map (MetronomeMsg i) metronomeCmd)

                        TickMsg msg ->
                            let
                                ( changedMetronomeModel, metronomeCmd ) =
                                    Metronome.update msg ws.actual
                            in
                                case changedMetronomeModel.status of
                                    Metronome.Finished ->
                                        case ws.next of
                                            [] ->
                                                { model | status = Idle }
                                                    |> Return.singleton
                                                    |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws) metronomeCmd)

                                            x :: xs ->
                                                let
                                                    ( changedX, xCmd ) =
                                                        Metronome.update Metronome.Start x
                                                in
                                                    { model | status = Working { previous = ws.previous ++ [ changedMetronomeModel ], actual = changedX, next = xs, paused = False } }
                                                        |> Return.singleton
                                                        |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws) metronomeCmd)
                                                        |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws + 1) xCmd)

                                    _ ->
                                        { model | status = Working { ws | actual = changedMetronomeModel } }
                                            |> Return.singleton
                                            |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws) metronomeCmd)

                        Pause ->
                            Metronome.update Metronome.Pause ws.actual
                                |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                |> Return.map
                                    (\metronomeModel ->
                                        { model | status = Working { ws | actual = metronomeModel, paused = True } }
                                    )

                        Next ->
                            case ws.next of
                                [] ->
                                    { model | status = Idle }
                                        |> Return.singleton

                                x :: xs ->
                                    Metronome.update Metronome.Start x
                                        |> Return.mapCmd (MetronomeMsg <| getActualIndex ws + 1)
                                        |> Return.map
                                            (\metronomeModel ->
                                                { model | status = Working { ws | previous = ws.previous ++ [ ws.actual ], actual = metronomeModel, next = xs, paused = False } }
                                            )

                        Stop ->
                            { model | status = Idle }
                                |> Return.singleton

                        _ ->
                            model
                                |> Return.singleton

                True ->
                    case msg of
                        Start ->
                            Metronome.update Metronome.Start ws.actual
                                |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                |> Return.map
                                    (\metronomeModel ->
                                        { model | status = Working { ws | actual = metronomeModel, paused = False } }
                                    )

                        Stop ->
                            { model | status = Idle }
                                |> Return.singleton

                        Next ->
                            case ws.next of
                                [] ->
                                    { model | status = Idle }
                                        |> Return.singleton

                                x :: xs ->
                                    { model | status = Working { ws | previous = ws.previous ++ [ Metronome.makeFinished ws.actual ], actual = Metronome.makePaused x, next = xs, paused = True } }
                                        |> Return.singleton

                        MetronomeMsg i msg ->
                            case atWs i ws of
                                Nothing ->
                                    model
                                        |> Return.singleton

                                Just metronomeModel ->
                                    let
                                        ( changedMetronomeModel, metronomeCmd ) =
                                            Metronome.update msg metronomeModel
                                    in
                                        { model | status = Working <| insertWs i changedMetronomeModel ws }
                                            |> Return.singleton
                                            |> Return.command (Platform.Cmd.map (MetronomeMsg i) metronomeCmd)

                        _ ->
                            model
                                |> Return.singleton


viewMetronomes : Model -> Html Msg
viewMetronomes model =
    div
        [ classList
            [ ( "metronomes", True )
            ]
        ]
        (case model.status of
            Idle ->
                List.indexedMap (\index metronome -> Metronome.view metronome |> Html.map (MetronomeMsg index)) model.metronomes

            Working ws ->
                let
                    actualIndex =
                        getActualIndex ws
                in
                    List.indexedMap (\index metronome -> Metronome.view metronome |> Html.map (MetronomeMsg index)) ws.previous
                        ++ [ (Metronome.view ws.actual |> Html.map (MetronomeMsg actualIndex)) ]
                        ++ List.indexedMap (\index metronome -> Metronome.view metronome |> Html.map (MetronomeMsg <| index + actualIndex)) ws.next
        )


view : Model -> Html Msg
view model =
    div []
        [ Html.div [ class "control-buttons" ]
            [ Html.button
                (case model.status of
                    Idle ->
                        [ Html.Events.onClick Start
                        , id "start-button"
                        , class "control-button"
                        ]

                    Working ws ->
                        case ws.paused of
                            True ->
                                [ Html.Events.onClick Start
                                , id "start-button"
                                , class "control-button"
                                ]

                            False ->
                                [ Html.Events.onClick Pause
                                , id "pause-button"
                                , class "control-button"
                                ]
                )
                []
            , Html.button
                [ case model.status of
                    Idle ->
                        Html.Attributes.disabled True

                    Working ws ->
                        Html.Events.onClick Stop
                , id "stop-button"
                , class "control-button"
                ]
                []
            , Html.button
                [ Html.Events.onClick Next
                , id "next-button"
                , class "control-button"
                ]
                []
            ]
        , viewMetronomes model
        ]


init : Return Msg Model
init =
    songToModel exampleSong
        |> Return.singleton


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Idle ->
            Sub.none

        Working ws ->
            Sub.map TickMsg <| Metronome.subscriptions ws.actual


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
