port module Metronome exposing (..)

import Types exposing (..)
import Return exposing (..)
import Json.Decode exposing (field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Platform.Cmd
import Animation exposing (px, turn, percent)
import ViewBlock
import String


type Click
    = High
    | Low


type alias WorkingState =
    { accents : List Int
    , maybeCount : Maybe Int
    , actual : List Int
    , highlightCount : Bool
    , lastClick : Click
    }


type Status
    = Working WorkingState
    | Idle ViewBlock.IdleState
    | Paused ViewBlock.IdleState WorkingState


type alias Model =
    { block : Block
    , status : Status
    }


type Msg
    = Tick
    | Start
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


blockToIdleState : Types.Block -> ViewBlock.IdleState
blockToIdleState block =
    { tempo = Basics.toString block.tempo, count = Maybe.withDefault 1 block.maybeCount |> Basics.toString }


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
            Idle is ->
                case msg of
                    Start ->
                        let
                            ws =
                                { accents = model.block.accents, maybeCount = model.block.maybeCount, actual = [ 1 ], highlightCount = False, lastClick = High }
                        in
                            { model
                                | status = Working ws
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
                                    { m | status = Idle { is | count = s } }
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
                        (case String.toInt s of
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
                                (\m -> { m | status = Idle { is | tempo = s } })

                    _ ->
                        model
                            |> Return.singleton

            Working ws ->
                case msg of
                    Tick ->
                        case ws.maybeCount of
                            Just count ->
                                if (count <= 0) then
                                    { model | status = Idle <| blockToIdleState model.block }
                                        |> Return.singleton
                                else
                                    case stripList ws.accents of
                                        ( beep, Maybe.Nothing ) ->
                                            { model | status = Working { ws | accents = model.block.accents, maybeCount = Just <| count - 1, actual = [ 1 ], highlightCount = True, lastClick = beepToClick beep } }
                                                |> Return.singleton
                                                |> Return.command (click <| toString <| beepToClick beep)

                                        ( beep, Maybe.Just newAccents ) ->
                                            { model | status = Working { ws | accents = newAccents, actual = updateActual beep ws.actual, highlightCount = False, lastClick = beepToClick beep } }
                                                |> Return.singleton
                                                |> Return.command (click <| toString <| beepToClick beep)

                            Nothing ->
                                case stripList ws.accents of
                                    ( beep, Maybe.Nothing ) ->
                                        { model | status = Working { ws | accents = model.block.accents, actual = [ 1 ], lastClick = beepToClick beep } }
                                            |> Return.singleton
                                            |> Return.command (click <| toString <| beepToClick beep)

                                    ( beep, Maybe.Just newAccents ) ->
                                        { model | status = Working { ws | accents = newAccents, actual = updateActual beep ws.actual, lastClick = beepToClick beep } }
                                            |> Return.singleton
                                            |> Return.command (click <| toString <| beepToClick beep)

                    Pause ->
                        { model | status = Paused (blockToIdleState model.block) ws }
                            |> Return.singleton

                    Stop ->
                        { model | status = Idle (blockToIdleState model.block) }
                            |> Return.singleton

                    _ ->
                        model
                            |> Return.singleton

            Paused is ws ->
                case msg of
                    Start ->
                        { model | status = Working ws }
                            |> Return.singleton
                            |> Return.command (click <| toString <| ws.lastClick)

                    Stop ->
                        { model | status = Idle is }
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
                                        { model | status = Paused is { ws | maybeCount = updateMaybeCount ws.maybeCount i } }
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
                                    { m | status = Paused { is | count = s } ws }
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
                        (case String.toInt s of
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
                                (\m -> { m | status = Paused { is | tempo = s } ws })

                    _ ->
                        model
                            |> Return.singleton


view : Model -> Html Msg
view model =
    Html.div
        []
        [ (case model.status of
            Idle is ->
                ViewBlock.view { status = ViewBlock.Idle is, block = model.block }

            Working ws ->
                ViewBlock.view { status = ViewBlock.Working <| wsToViewBlockws ws, block = model.block }

            Paused is ws ->
                ViewBlock.view { status = ViewBlock.Paused is <| wsToViewBlockws ws, block = model.block }
          )
            |> Html.map ViewMsg
        ]


viewTest : Model -> Html Msg
viewTest model =
    div []
        [ Html.text <| toString model
        , Html.p [] []
        , Html.button
            [ case model.status of
                Idle is ->
                    Html.Events.onClick Start

                Working ws ->
                    Html.Events.onClick Pause

                Paused is ws ->
                    Html.Events.onClick Start
            ]
            [ case model.status of
                Idle is ->
                    Html.text "Start"

                Working ws ->
                    Html.text "Pause"

                Paused is ws ->
                    Html.text "Resume"
            ]
        , Html.button
            [ case model.status of
                Idle is ->
                    Html.Attributes.disabled True

                Working ws ->
                    Html.Events.onClick Stop

                Paused is ws ->
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
    Model block (Idle <| blockToIdleState block)
        |> Return.singleton


tempoToMs : Int -> Time.Time
tempoToMs tempo =
    (60000 * Time.millisecond) / Basics.toFloat tempo


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Working ws ->
            Time.every (tempoToMs model.block.tempo) <| Basics.always Tick

        _ ->
            Sub.none


port click : String -> Platform.Cmd.Cmd msg


main =
    Html.program
        { init = init
        , view = viewTest
        , update = update
        , subscriptions = subscriptions
        }
