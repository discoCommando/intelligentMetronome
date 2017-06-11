module ViewBlock exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types
import Return
import Json.Decode
import Time


type alias WorkingState =
    { maybeCount : Maybe Int
    , actual : List Int
    , stopped : Bool
    }


type Status
    = Idle
    | Working WorkingState


type alias Model =
    { block : Types.Block
    , status : Status
    }


type Msg
    = Add Int
    | Remove Int
    | ChangeTempo String
    | CheckInfinity Bool
    | ChangeCount String
      -- external messages
    | Tick WorkingState
    | New Types.Block


update : Msg -> Model -> Return.Return Msg Model
update msg model =
    case model.status of
        Idle ->
            case msg of
                New block ->
                    { model | block = block }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        Working ws ->
            case msg of
                Tick newWs ->
                    { model | status = Working newWs }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton


viewAddRemoveButtons : Bool -> Int -> Html Msg
viewAddRemoveButtons minusDisabled id =
    div [ class "add-remove-buttons" ]
        [ button
            [ class "add-button"
            , onClick <| Add id
            ]
            [ text "+" ]
        , button
            [ class "remove-button"
            , disabled minusDisabled
            , onClick <| Remove id
            ]
            [ text "-" ]
        ]


viewAccent : Bool -> String -> Int -> Html Msg
viewAccent highlight label tempo =
    let
        attributes =
            case highlight of
                True ->
                    [ class "accent highlight"
                    , style
                        [ ( "animation-duration"
                          , (Basics.toString <| Basics.floor ((60000 * Time.millisecond) / Basics.toFloat tempo)) ++ "ms"
                          )
                        ]
                    ]

                False ->
                    [ class "accent"
                    ]
    in
        div
            attributes
            [ text label ]


isJust : Maybe a -> Bool
isJust a =
    case a of
        Maybe.Nothing ->
            False

        _ ->
            True


toInt : Maybe Int -> Int
toInt m =
    case m of
        Maybe.Nothing ->
            1

        Maybe.Just a ->
            a


viewBlockIdle : Types.Block -> List (Html Msg)
viewBlockIdle block =
    [ div [ class "block-info" ]
        [ div []
            [ text "Tempo: "
            , input [ type_ "number", value <| Basics.toString block.tempo, onInput ChangeTempo ] [ text <| Basics.toString block.tempo ]
            ]
        , div []
            [ div []
                [ text "Count: "
                , input
                    [ type_ "number"
                    , value <| Basics.toString <| toInt block.maybeCount
                    , disabled <| Basics.not <| isJust block.maybeCount
                    , onInput ChangeCount
                    ]
                    [ text <| Basics.toString block.tempo ]
                , input
                    [ type_ "checkbox"
                    , checked (Basics.not <| isJust block.maybeCount)
                    , onCheck CheckInfinity
                    ]
                    []
                , text "Infinite"
                ]
            ]
        ]
    ]
        ++ (block.accents
                |> List.indexedMap
                    (\i accent ->
                        [ div [ class "accents" ] <|
                            List.map
                                (\index -> viewAccent False (toString index) block.tempo)
                                (List.range 1 accent)
                                ++ [ viewAddRemoveButtons False i ]
                        ]
                    )
                |> List.concat
           )
        ++ [ div
                [ class "accents" ]
                [ viewAddRemoveButtons True <| List.length block.accents ]
           ]


zipOnlyLast : List Int -> List Int -> List ( Int, Maybe Int )
zipOnlyLast accents actual =
    case ( accents, actual ) of
        ( x :: xs, [ y ] ) ->
            ( x, Just y ) :: zipOnlyLast xs []

        ( x :: xs, y :: ys ) ->
            ( x, Nothing ) :: zipOnlyLast xs ys

        ( x :: xs, [] ) ->
            ( x, Nothing ) :: zipOnlyLast xs []

        ( [], y :: ys ) ->
            ( 0, Nothing ) :: zipOnlyLast [] ys

        ( [], [] ) ->
            []


viewBlockWorking : Types.Block -> WorkingState -> List (Html Msg)
viewBlockWorking block ws =
    [ div [ class "block-info" ]
        [ div [] [ text "Tempo: ", input [ type_ "text", disabled True, value <| Basics.toString block.tempo ] [ text <| Basics.toString block.tempo ] ]
        ]
    ]
        ++ (zipOnlyLast block.accents ws.actual
                |> List.map
                    (\( accent, maybeInt ) ->
                        case maybeInt of
                            Nothing ->
                                [ div [ class "accents" ] <| List.map (\index -> viewAccent False (toString index) block.tempo) (List.range 1 accent) ]

                            Just x ->
                                [ div [ class "accents" ] <|
                                    List.map (\index -> viewAccent False (toString index) block.tempo) (List.range 1 (x - 1))
                                        ++ [ viewAccent (Basics.not ws.stopped) (toString x) block.tempo ]
                                        ++ List.map (\index -> viewAccent False (toString index) block.tempo) (List.range (x + 1) accent)
                                ]
                    )
                |> List.concat
           )


view : Model -> Html Msg
view model =
    case model.status of
        Idle ->
            div
                [ class "block idle"
                ]
                (viewBlockIdle model.block)

        Working ws ->
            div
                [ class "block working"
                ]
                (viewBlockWorking model.block ws)


block : Types.Block
block =
    case
        Json.Decode.decodeString Types.decodeBlock """
        {
          "tempo" : 160,
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


init : Return.Return Msg Model
init =
    Model block (Working { maybeCount = Just 5, actual = [ 2, 1 ], stopped = False })
        |> Return.singleton


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
