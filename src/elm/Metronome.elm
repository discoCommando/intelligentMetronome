port module Metronome exposing (..)

import Types exposing (..)
import Return exposing (..)
import Json.Decode exposing (field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Platform.Cmd
import ViewBlock
import String
import Task


type Click
    = High
    | Low


type alias TimeCorrection =
    { lastCorrectTickTime : Time.Time
    , timeOffset : Time.Time
    }


type alias WorkingState =
    { accents : List Int
    , maybeCount : Maybe Int
    , actual : List Int
    , highlightCount : Bool
    , lastClick : Click
    , passed : Int
    }


type Status
    = Working WorkingState TimeCorrection
    | Idle
    | Paused WorkingState
    | Finished


type alias Model =
    { block : Block
    , status : Status
    , temps : ViewBlock.Temps
    , initialTimeCorrection : Maybe TimeCorrection
    }


type Msg
    = Tick Time.Time
    | Start
    | StartWithTime Time.Time
    | Pause
    | Stop
    | ViewMsg ViewBlock.Msg


stripList : List Int -> ( Bool, Maybe (List Int) )
stripList list =
    case list of
        [] ->
            ( True, Nothing )

        [ 1 ] ->
            ( True, Nothing )

        x :: xs ->
            (if (x <= 1) then
                ( True, Just xs )
             else
                ( False, Just (x - 1 :: xs) )
            )


wsToViewBlockws : WorkingState -> ViewBlock.WorkingState
wsToViewBlockws ws =
    { maybeCount = ws.maybeCount, actual = ws.actual, highlightCount = ws.highlightCount }


wsFromIdle : Types.Block -> WorkingState
wsFromIdle block =
    { accents = block.accents
    , maybeCount = block.maybeCount
    , actual = [ 1 ]
    , highlightCount = False
    , lastClick = High
    , passed = 0
    }


makeFinished : Model -> Model
makeFinished model =
    { model | status = Finished }


makePaused : Model -> Model
makePaused model =
    case model.status of
        Idle ->
            { model | status = Paused (wsFromIdle model.block) }

        Working ws tc ->
            { model | status = Paused ws }

        Finished ->
            { model | status = Paused (wsFromIdle model.block) }

        _ ->
            model


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        beepToClick beep =
            if beep then
                High
            else
                Low

        addToLast list =
            case list of
                [] ->
                    []

                [ x ] ->
                    [ x + 1 ]

                x :: xs ->
                    x :: addToLast xs

        updateActual beep actual =
            if beep then
                actual ++ [ 1 ]
            else
                addToLast actual
    in
        case model.status of
            Idle ->
                case msg of
                    Start ->
                        case model.initialTimeCorrection of
                            Nothing ->
                                Return.return model
                                    (Time.now
                                        |> Task.perform StartWithTime
                                    )

                            Just timeCorrection ->
                                let
                                    ws =
                                        wsFromIdle model.block
                                in
                                    { model
                                        | status = Working ws timeCorrection
                                    }
                                        |> Return.singleton
                                        |> Return.command (click <| toString <| High)

                    StartWithTime now ->
                        let
                            ws =
                                wsFromIdle model.block
                        in
                            { model
                                | status = Working ws { lastCorrectTickTime = now, timeOffset = 0 }
                            }
                                |> Return.singleton
                                |> Return.command (click <| toString <| High)

                    ViewMsg (ViewBlock.Add i) ->
                        let
                            addToBlock block i =
                                if i > List.length block.accents then
                                    block
                                else if i == List.length block.accents then
                                    { block | accents = block.accents ++ [ 1 ] }
                                else
                                    { block
                                        | accents =
                                            List.indexedMap
                                                (\index a ->
                                                    if (index == i) then
                                                        a + 1
                                                    else
                                                        a
                                                )
                                                block.accents
                                    }
                        in
                            { model | block = addToBlock model.block i }
                                |> Return.singleton

                    ViewMsg (ViewBlock.Remove i) ->
                        let
                            removeFromBlock block i =
                                if i >= List.length block.accents then
                                    block
                                else
                                    { block
                                        | accents =
                                            List.indexedMap
                                                (\index a ->
                                                    if (index == i) then
                                                        if a == 1 then
                                                            []
                                                        else
                                                            [ a - 1 ]
                                                    else
                                                        [ a ]
                                                )
                                                block.accents
                                                |> List.concat
                                    }
                        in
                            { model | block = removeFromBlock model.block i }
                                |> Return.singleton

                    ViewMsg (ViewBlock.ChangeCount s) ->
                        (case String.toInt s of
                            Result.Ok i ->
                                if (i > 0) then
                                    let
                                        block =
                                            model.block

                                        updateMaybeCount maybeCount i =
                                            case maybeCount of
                                                Just c ->
                                                    Just i

                                                Nothing ->
                                                    Nothing
                                    in
                                        { model | block = { block | maybeCount = updateMaybeCount block.maybeCount i } }
                                            |> Return.singleton
                                else
                                    model
                                        |> Return.singleton

                            _ ->
                                model
                                    |> Return.singleton
                        )
                            |> Return.map
                                (\m ->
                                    let
                                        t =
                                            m.temps
                                    in
                                        { m | temps = { t | count = s } }
                                )

                    ViewMsg (ViewBlock.ClickCount) ->
                        let
                            block =
                                model.block

                            updateMaybeCount maybeCount =
                                case maybeCount of
                                    Just c ->
                                        Nothing

                                    Nothing ->
                                        Just 1
                        in
                            { model | block = { block | maybeCount = updateMaybeCount block.maybeCount } }
                                |> Return.singleton

                    ViewMsg (ViewBlock.ChangeTempo s) ->
                        (case String.toFloat s of
                            Result.Ok i ->
                                if (i > 0) then
                                    let
                                        block =
                                            model.block
                                    in
                                        { model | block = { block | tempo = i } }
                                            |> Return.singleton
                                else
                                    model
                                        |> Return.singleton

                            _ ->
                                model
                                    |> Return.singleton
                        )
                            |> Return.map
                                (\m ->
                                    let
                                        t =
                                            m.temps
                                    in
                                        { m | temps = { t | tempo = s } }
                                )

                    _ ->
                        model
                            |> Return.singleton

            Working ws tc ->
                case msg of
                    Tick time ->
                        let
                            newYs =
                                (let
                                    newCorrectTime =
                                        tc.lastCorrectTickTime + tempoToMs model.block.tempo
                                 in
                                    { lastCorrectTickTime = newCorrectTime
                                    , timeOffset = newCorrectTime - time
                                    }
                                )
                        in
                            case ws.maybeCount of
                                Just count ->
                                    if (count <= 0) then
                                        { model | status = Finished }
                                            |> Return.singleton
                                    else
                                        case stripList ws.accents of
                                            ( beep, Maybe.Nothing ) ->
                                                if count == 1 then
                                                    { model | status = Finished }
                                                        |> Return.singleton
                                                else
                                                    { model
                                                        | status =
                                                            Working
                                                                { ws
                                                                    | accents = model.block.accents
                                                                    , maybeCount = Just <| count - 1
                                                                    , actual = [ 1 ]
                                                                    , highlightCount = True
                                                                    , lastClick = beepToClick beep
                                                                    , passed = ws.passed + 1
                                                                }
                                                                newYs
                                                    }
                                                        |> Return.singleton
                                                        |> Return.command (click <| toString <| beepToClick beep)

                                            ( beep, Maybe.Just newAccents ) ->
                                                { model
                                                    | status =
                                                        Working
                                                            { ws
                                                                | accents = newAccents
                                                                , actual = updateActual beep ws.actual
                                                                , highlightCount = False
                                                                , lastClick = beepToClick beep
                                                            }
                                                            newYs
                                                }
                                                    |> Return.singleton
                                                    |> Return.command (click <| toString <| beepToClick beep)

                                Nothing ->
                                    case stripList ws.accents of
                                        ( beep, Maybe.Nothing ) ->
                                            { model
                                                | status =
                                                    Working
                                                        { ws
                                                            | accents = model.block.accents
                                                            , actual = [ 1 ]
                                                            , lastClick = beepToClick beep
                                                            , passed = ws.passed + 1
                                                        }
                                                        newYs
                                            }
                                                |> Return.singleton
                                                |> Return.command (click <| toString <| beepToClick beep)

                                        ( beep, Maybe.Just newAccents ) ->
                                            { model
                                                | status =
                                                    Working
                                                        { ws
                                                            | accents = newAccents
                                                            , actual = updateActual beep ws.actual
                                                            , lastClick = beepToClick beep
                                                        }
                                                        newYs
                                            }
                                                |> Return.singleton
                                                |> Return.command (click <| toString <| beepToClick beep)

                    Pause ->
                        { model | status = Paused ws }
                            |> Return.singleton

                    Stop ->
                        { model | status = Idle }
                            |> Return.singleton

                    _ ->
                        model
                            |> Return.singleton

            Paused ws ->
                case msg of
                    Start ->
                        case model.initialTimeCorrection of
                            Nothing ->
                                Return.return model
                                    (Time.now
                                        |> Task.perform StartWithTime
                                    )

                            Just timeCorrection ->
                                { model | status = Working ws timeCorrection }
                                    |> Return.singleton
                                    |> Return.command (click <| toString <| ws.lastClick)

                    StartWithTime now ->
                        { model | status = Working ws { lastCorrectTickTime = now, timeOffset = 0 } }
                            |> Return.singleton
                            |> Return.command (click <| toString <| ws.lastClick)

                    Stop ->
                        { model | status = Idle }
                            |> Return.singleton

                    _ ->
                        model
                            |> Return.singleton

            Finished ->
                case msg of
                    Start ->
                        case model.initialTimeCorrection of
                            Nothing ->
                                Return.return model
                                    (Time.now
                                        |> Task.perform StartWithTime
                                    )

                            Just timeCorrection ->
                                let
                                    ws =
                                        wsFromIdle model.block
                                in
                                    { model
                                        | status = Working ws timeCorrection
                                    }
                                        |> Return.singleton
                                        |> Return.command (click <| toString <| High)

                    StartWithTime now ->
                        let
                            ws =
                                wsFromIdle model.block
                        in
                            { model
                                | status = Working ws { lastCorrectTickTime = now, timeOffset = 0 }
                            }
                                |> Return.singleton
                                |> Return.command (click <| toString <| High)

                    _ ->
                        model
                            |> Return.singleton


view : Model -> Html Msg
view model =
    (case model.status of
        Idle ->
            ViewBlock.view { status = ViewBlock.Idle, block = model.block, temps = model.temps }

        Working ws _ ->
            ViewBlock.view { status = ViewBlock.Working <| wsToViewBlockws ws, block = model.block, temps = model.temps }

        Paused ws ->
            ViewBlock.view { status = ViewBlock.Paused <| wsToViewBlockws ws, block = model.block, temps = model.temps }

        Finished ->
            ViewBlock.view { status = ViewBlock.Finished, block = model.block, temps = model.temps }
    )
        |> Html.map ViewMsg


viewTest : Model -> Html Msg
viewTest model =
    div []
        [ Html.text <| toString model
        , Html.p [] []
        , Html.button
            [ case model.status of
                Idle ->
                    Html.Events.onClick Start

                Working ws _ ->
                    Html.Events.onClick Pause

                Paused ws ->
                    Html.Events.onClick Start

                Finished ->
                    Html.Events.onClick Start
            ]
            [ case model.status of
                Idle ->
                    Html.text "Start"

                Working ws _ ->
                    Html.text "Pause"

                Paused ws ->
                    Html.text "Resume"

                Finished ->
                    Html.text "Start"
            ]
        , Html.button
            [ case model.status of
                Idle ->
                    Html.Attributes.disabled True

                Working ws _ ->
                    Html.Events.onClick Stop

                Paused ws ->
                    Html.Events.onClick Stop

                Finished ->
                    Html.Events.onClick Stop
            ]
            [ Html.text "Stop" ]
        , Html.p [] []
        , view model
        ]


block : Block
block =
    case
        Json.Decode.decodeString decodeBlock """
    {
      "tempo" : 240,
      "accents": [
      4
      ],
      "maybeCount" : 5
    }
"""
    of
        Result.Ok a ->
            a

        Result.Err x ->
            Debug.crash x


init : Return Msg Model
init =
    Model block (Idle) { tempo = Basics.toString block, count = "5" } Nothing
        |> Return.singleton


tempoToMs : Float -> Time.Time
tempoToMs tempo =
    (60000 * Time.millisecond) / tempo


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Working ws tc ->
            Time.every (tempoToMs model.block.tempo + tc.timeOffset) Tick

        _ ->
            Sub.none


emptyModel : Model
emptyModel =
    { block = { tempo = 120, accents = [], maybeCount = Nothing }
    , status = Idle
    , temps = { tempo = "120", count = "1" }
    , initialTimeCorrection = Nothing
    }


blockToTime : Types.Block -> Maybe Time.Time
blockToTime block =
    case block.maybeCount of
        Nothing ->
            Nothing

        Just count ->
            ((count
                * List.sum block.accents
                * 60000
                |> Basics.toFloat
                |> (*) Time.millisecond
             )
                / (block.tempo)
            )
                |> Just


timeElapsed : Model -> Time.Time
timeElapsed model =
    let
        sumAccents ws =
            let
                temp =
                    List.sum model.block.accents
            in
                if temp == 0 then
                    ws.passed
                else
                    ws.passed * temp

        helper ws =
            tempoToMs model.block.tempo * (sumAccents ws + List.sum ws.actual - 1 |> Basics.toFloat)
    in
        case model.status of
            Idle ->
                0

            Working ws _ ->
                helper ws

            Paused ws ->
                helper ws

            Finished ->
                0


port click : String -> Platform.Cmd.Cmd msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = viewTest
        , update = update
        , subscriptions = subscriptions
        }
