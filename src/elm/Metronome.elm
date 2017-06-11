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


type alias WorkingState =
    { accents : List Int
    , maybeCount : Maybe Int
    , stopped : Bool
    , actual : List Int
    }


type Status
    = Working WorkingState
      -- List Int is a list of current accents to do, Int is a count left
    | Idle


type alias Model =
    { block : Block
    , status : Status
    }


type Msg
    = Tick
    | Start
    | Stop
    | Reset
    | ViewMsg ViewBlock.Msg


type Click
    = High
    | Low



-- bool is if to tick


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
    { maybeCount = ws.maybeCount, actual = ws.actual }


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
                        let
                            ws =
                                { accents = model.block.accents, maybeCount = model.block.maybeCount, stopped = False, actual = [ 1 ] }
                        in
                            { model
                                | status = Working ws
                            }
                                |> Return.singleton
                                |> Return.command (click <| toString <| High)

                    _ ->
                        model
                            |> Return.singleton

            Working ws ->
                case ws.stopped of
                    False ->
                        case msg of
                            Tick ->
                                case ws.maybeCount of
                                    Just count ->
                                        if (count <= 0) then
                                            { model | status = Idle }
                                                |> Return.singleton
                                        else
                                            case stripList ws.accents of
                                                ( beep, Maybe.Nothing ) ->
                                                    { model | status = Working { ws | accents = model.block.accents, maybeCount = Just <| count - 1, actual = [ 1 ] } }
                                                        |> Return.singleton
                                                        |> Return.command (click <| toString <| beepToClick beep)

                                                ( beep, Maybe.Just newAccents ) ->
                                                    { model | status = Working { ws | accents = newAccents, actual = updateActual beep ws.actual } }
                                                        |> Return.singleton
                                                        |> Return.command (click <| toString <| beepToClick beep)

                                    Nothing ->
                                        case stripList ws.accents of
                                            ( beep, Maybe.Nothing ) ->
                                                { model | status = Working { ws | accents = model.block.accents, actual = [ 1 ] } }
                                                    |> Return.singleton
                                                    |> Return.command (click <| toString <| beepToClick beep)

                                            ( beep, Maybe.Just newAccents ) ->
                                                { model | status = Working { ws | accents = newAccents, actual = updateActual beep ws.actual } }
                                                    |> Return.singleton
                                                    |> Return.command (click <| toString <| beepToClick beep)

                            Stop ->
                                { model | status = Working { ws | stopped = True } }
                                    |> Return.singleton

                            _ ->
                                model
                                    |> Return.singleton

                    True ->
                        case msg of
                            Start ->
                                { model | status = Working { ws | stopped = False } }
                                    |> Return.singleton
                                    |> Return.command (click <| toString <| High)

                            Reset ->
                                { model | status = Idle }
                                    |> Return.singleton

                            _ ->
                                model
                                    |> Return.singleton


view : Model -> Html Msg
view model =
    div []
        [ text <| Basics.toString model
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
        , Html.p [] []
        , Html.div
            []
            [ (case model.status of
                Idle ->
                    ViewBlock.view { status = ViewBlock.Idle, block = model.block }

                Working ws ->
                    ViewBlock.view { status = ViewBlock.Working <| wsToViewBlockws ws, block = model.block }
              )
                |> Html.map ViewMsg
            ]
        ]


block : Block
block =
    case
        Json.Decode.decodeString decodeBlock """
        {
          "tempo" : 200,
          "accents": [
            2, 3, 2, 4
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
    Model block Idle
        |> Return.singleton


tempoToMs : Int -> Time.Time
tempoToMs tempo =
    (60000 * Time.millisecond) / Basics.toFloat tempo


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Working ws ->
            case ws.stopped of
                False ->
                    Time.every (tempoToMs model.block.tempo) <| Basics.always Tick

                True ->
                    Sub.none

        _ ->
            Sub.none


port click : String -> Platform.Cmd.Cmd msg


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
