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
    , highlightCount : Bool
    }



-- values that may be not correct


type alias IdleState =
    { tempo : String
    , count : String
    }


type Status
    = Idle IdleState
    | Working WorkingState
    | Paused IdleState WorkingState
    | Finished IdleState


type alias Model =
    { block : Types.Block
    , status : Status
    }


type Msg
    = Add Int
    | Remove Int
    | ChangeTempo String
    | ClickCount
    | ChangeCount String
      -- external messages
    | Tick WorkingState
    | New Types.Block


update : Msg -> Model -> Return.Return Msg Model
update msg model =
    case model.status of
        Idle is ->
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


type AnimationType
    = Highlight
    | Glow
    | NoAnimation


viewAccent : AnimationType -> String -> Int -> Html Msg
viewAccent animation label tempo =
    let
        attributes =
            case animation of
                Highlight ->
                    [ class "accent highlight"
                    , style
                        [ ( "animation-duration"
                          , (Basics.toString <| Basics.floor ((60000 * Time.millisecond) / Basics.toFloat tempo)) ++ "ms"
                          )
                        ]
                    ]

                Glow ->
                    [ class "accent glow" ]

                NoAnimation ->
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


maybeMapWithDefault : Maybe a -> (a -> b) -> b -> b
maybeMapWithDefault maybe f default =
    case maybe of
        Just a ->
            f a

        Nothing ->
            default


viewBlockIdle : Types.Block -> IdleState -> List (Html Msg)
viewBlockIdle block is =
    [ div [ class "block-info" ]
        [ div [ class "tempo" ]
            [ div [ class "tempo-info" ] [ text "TEMPO" ]
            , div [ class "tempo-value" ]
                [ input [ type_ "text", value <| is.tempo, onInput ChangeTempo ] [ text <| is.tempo ]
                ]
            ]
        , div [ class "count" ]
            [ button
                [ classList
                    [ ( "count-info", True )
                    ]
                , onClick ClickCount
                ]
                [ text "COUNT" ]
            , div [ class "count-value" ]
                [ input
                    [ type_ "text"
                    , value <|
                        if (isJust block.maybeCount) then
                            is.count
                        else
                            "∞"
                    , disabled <| Basics.not <| isJust block.maybeCount
                    , onInput ChangeCount
                    ]
                    [ text <|
                        if (isJust block.maybeCount) then
                            is.count
                        else
                            "∞"
                    ]
                ]
            ]
        ]
    ]
        ++ (block.accents
                |> List.indexedMap
                    (\i accent ->
                        [ div [ class "accents" ] <|
                            List.map
                                (\index -> viewAccent NoAnimation (toString index) block.tempo)
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
        [ div [ class "tempo" ]
            [ div [ class "tempo-info" ] [ text "TEMPO" ]
            , div [ class "tempo-value" ]
                [ div
                    []
                    [ text <| Basics.toString block.tempo ]
                ]
            ]
        , div [ class "count" ]
            [ div [ class "count-info" ] [ text "COUNT" ]
            , div
                [ classList
                    [ ( "count-value", True )
                    , ( "animate bounce", ws.highlightCount )
                    ]
                , style
                    [ ( "animation-duration"
                      , (Basics.toString <| Basics.floor ((60000 * Time.millisecond) / Basics.toFloat block.tempo)) ++ "ms"
                      )
                    ]
                ]
                [ div
                    []
                    [ text <|
                        maybeMapWithDefault ws.maybeCount Basics.toString "∞"
                    ]
                ]
            ]
        ]
    ]
        ++ (zipOnlyLast block.accents ws.actual
                |> List.map
                    (\( accent, maybeInt ) ->
                        case maybeInt of
                            Nothing ->
                                [ div [ class "accents" ] <| List.map (\index -> viewAccent NoAnimation (toString index) block.tempo) (List.range 1 accent) ]

                            Just x ->
                                [ div [ class "accents" ] <|
                                    List.map (\index -> viewAccent NoAnimation (toString index) block.tempo) (List.range 1 (x - 1))
                                        ++ [ viewAccent Highlight (toString x) block.tempo ]
                                        ++ List.map (\index -> viewAccent NoAnimation (toString index) block.tempo) (List.range (x + 1) accent)
                                ]
                    )
                |> List.concat
           )


viewBlockPaused : Types.Block -> IdleState -> WorkingState -> List (Html Msg)
viewBlockPaused block is ws =
    [ div [ class "block-info" ]
        [ div [ class "tempo" ]
            [ div [ class "tempo-info" ] [ text "TEMPO" ]
            , div [ class "tempo-value" ]
                [ div
                    []
                    [ text <| Basics.toString block.tempo ]
                ]
            ]
        , div [ class "count" ]
            [ div [ class "count-info" ] [ text "COUNT" ]
            , div
                [ classList
                    [ ( "count-value", True )
                    , ( "animate bounce", ws.highlightCount )
                    ]
                , style
                    [ ( "animation-duration"
                      , (Basics.toString <| Basics.floor ((60000 * Time.millisecond) / Basics.toFloat block.tempo)) ++ "ms"
                      )
                    ]
                ]
                [ div
                    []
                    [ text <|
                        maybeMapWithDefault ws.maybeCount Basics.toString "∞"
                    ]
                ]
            ]
        ]
    ]
        ++ (zipOnlyLast block.accents ws.actual
                |> List.map
                    (\( accent, maybeInt ) ->
                        case maybeInt of
                            Nothing ->
                                [ div [ class "accents" ] <| List.map (\index -> viewAccent NoAnimation (toString index) block.tempo) (List.range 1 accent) ]

                            Just x ->
                                [ div [ class "accents" ] <|
                                    List.map (\index -> viewAccent NoAnimation (toString index) block.tempo) (List.range 1 (x - 1))
                                        ++ [ viewAccent Glow (toString x) block.tempo ]
                                        ++ List.map (\index -> viewAccent NoAnimation (toString index) block.tempo) (List.range (x + 1) accent)
                                ]
                    )
                |> List.concat
           )


viewBlockFinished : Types.Block -> IdleState -> List (Html Msg)
viewBlockFinished block is =
    [ div [ class "block-info" ]
        [ div [ class "tempo" ]
            [ div [ class "tempo-info" ] [ text "TEMPO" ]
            , div [ class "tempo-value" ]
                [ div
                    []
                    [ text <| Basics.toString block.tempo ]
                ]
            ]
        , div [ class "count" ]
            [ div [ class "count-info" ] [ text "COUNT" ]
            , div
                [ classList
                    [ ( "count-value", True )
                    ]
                , style
                    [ ( "animation-duration"
                      , (Basics.toString <| Basics.floor ((60000 * Time.millisecond) / Basics.toFloat block.tempo)) ++ "ms"
                      )
                    ]
                ]
                [ div
                    []
                    [ text <|
                        maybeMapWithDefault block.maybeCount Basics.toString "∞"
                    ]
                ]
            ]
        ]
    ]
        ++ (block.accents
                |> List.indexedMap
                    (\i accent ->
                        [ div [ class "accents" ] <|
                            List.map
                                (\index -> viewAccent NoAnimation (toString index) block.tempo)
                                (List.range 1 accent)
                        ]
                    )
                |> List.concat
           )


view : Model -> Html Msg
view model =
    case model.status of
        Idle is ->
            div
                [ class "block idle"
                ]
                (viewBlockIdle model.block is)

        Working ws ->
            div
                [ class "block working"
                ]
                (viewBlockWorking model.block ws)

        Paused is ws ->
            div [ class "block paused" ]
                (viewBlockPaused model.block is ws)

        Finished is ->
            div [ class "block finished" ]
                (viewBlockFinished model.block is)


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
    Model block (Working { maybeCount = Just 5, actual = [ 2, 1 ], highlightCount = True })
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
