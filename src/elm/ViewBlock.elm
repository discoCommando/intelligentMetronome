module ViewBlock exposing (..)

import Html
import Html.Attributes
import Html.Events
import Types
import Return
import Json.Decode
import Time


type alias WorkingState =
    { maybeCount : Maybe Int
    , actual : List Int
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


viewAccent : Bool -> String -> Int -> Html.Html Msg
viewAccent highlight label tempo =
    let
        attributes =
            case highlight of
                True ->
                    [ Html.Attributes.class "accent highlight"
                    , Html.Attributes.style [ ( "animation-duration", (Basics.toString <| Basics.floor ((60000 * Time.millisecond) / Basics.toFloat tempo)) ++ "ms" ) ]
                    ]

                False ->
                    [ Html.Attributes.class "accent"
                    ]
    in
        Html.div
            attributes
            [ Html.text label ]


viewBlockIdle : Types.Block -> List (Html.Html Msg)
viewBlockIdle block =
    block.accents
        |> List.map
            (\accent ->
                [ Html.div [] <| List.map (\index -> viewAccent False (toString index) block.tempo) (List.range 1 accent) ]
                    ++ [ Html.p [] [] ]
            )
        |> List.concat


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


viewBlockWorking : Types.Block -> WorkingState -> List (Html.Html Msg)
viewBlockWorking block ws =
    zipOnlyLast block.accents ws.actual
        |> List.map
            (\( accent, maybeInt ) ->
                case maybeInt of
                    Nothing ->
                        [ Html.div [ Html.Attributes.class "accents" ] <| List.map (\index -> viewAccent False (toString index) block.tempo) (List.range 1 accent) ]
                            ++ [ Html.p [] [] ]

                    Just x ->
                        [ Html.div [ Html.Attributes.class "accents" ] <|
                            List.map (\index -> viewAccent False (toString index) block.tempo) (List.range 1 (x - 1))
                                ++ [ viewAccent True (toString x) block.tempo ]
                                ++ List.map (\index -> viewAccent False (toString index) block.tempo) (List.range (x + 1) accent)
                        ]
                            ++ [ Html.p [] [] ]
            )
        |> List.concat


view : Model -> Html.Html Msg
view model =
    case model.status of
        Idle ->
            Html.div
                [ Html.Attributes.class "block idle"
                ]
                (viewBlockIdle model.block)

        Working ws ->
            Html.div
                [ Html.Attributes.class "block working"
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
    Model block (Working { maybeCount = Just 5, actual = [ 2, 1 ] })
        |> Return.singleton


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
