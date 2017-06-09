port module Metronome exposing (..)

import Types exposing (..)
import Return exposing (..)
import Json.Decode exposing (field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Platform.Cmd


type Status
    = Counting (List Int) (Maybe Int)
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


type Click
    = High
    | Low


initialModel : Block -> Model
initialModel block =
    { block = block
    , status = Idle
    }



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


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        beepToClick beep =
            if beep then
                High
            else
                Low
    in
        case msg of
            Tick ->
                case model.status of
                    Idle ->
                        Return.singleton model

                    Counting accents maybeCount ->
                        case maybeCount of
                            Just count ->
                                if (count <= 0) then
                                    { model | status = Idle }
                                        |> Return.singleton
                                else
                                    case stripList accents of
                                        ( beep, Maybe.Nothing ) ->
                                            { model | status = Counting model.block.accents (Just <| count - 1) }
                                                |> Return.singleton
                                                |> Return.command (click <| toString <| beepToClick beep)

                                        ( beep, Maybe.Just newAccents ) ->
                                            { model | status = Counting newAccents (Just count) }
                                                |> Return.singleton
                                                |> Return.command (click <| toString <| beepToClick beep)

                            Nothing ->
                                case stripList accents of
                                    ( beep, Maybe.Nothing ) ->
                                        { model | status = Counting model.block.accents Maybe.Nothing }
                                            |> Return.singleton
                                            |> Return.command (click <| toString <| beepToClick beep)

                                    ( beep, Maybe.Just newAccents ) ->
                                        { model | status = Counting newAccents Maybe.Nothing }
                                            |> Return.singleton
                                            |> Return.command (click <| toString <| beepToClick beep)

            Start ->
                { model | status = Counting model.block.accents model.block.count }
                    |> Return.singleton
                    |> Return.command (click <| toString <| High)

            Stop ->
                { model | status = Idle }
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

                _ ->
                    Html.Events.onClick Stop
            ]
            [ case model.status of
                Idle ->
                    Html.text "Start"

                _ ->
                    Html.text "Stop"
            ]
        ]


block : Block
block =
    case
        Json.Decode.decodeString decodeBlock """
        {
          "tempo" : 260,
          "accents": [
            2, 3, 2, 4
          ],
          "count" : 5
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
        Counting _ _ ->
            Time.every (tempoToMs model.block.tempo) <| Basics.always Tick

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
