module View exposing (..)

import Types
import Animation exposing (px, turn, percent)
import Return
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Color exposing (rgb, rgba)
import Time exposing (..)


-- List Int is a index of Widget to Tick


type Msg
    = Tick (List Int)
    | Animate Animation.Msg


type alias Widget =
    { label : String
    , style : Animation.State
    , index : List Int
    }


type alias Model =
    { widgets : List (List Widget)
    }


onStyle : (Animation.State -> Animation.State) -> Widget -> Widget
onStyle styleFn widget =
    { widget | style = styleFn widget.style }


onIndex : List Int -> List (List a) -> (a -> a) -> List (List a)
onIndex ii listlist fn =
    case ii of
        [] ->
            listlist

        [ i ] ->
            case listlist of
                [] ->
                    []

                list :: lists ->
                    (List.indexedMap
                        (\j val ->
                            if i == j then
                                fn val
                            else
                                val
                        )
                        list
                    )
                        :: lists

        i :: is ->
            case listlist of
                [] ->
                    []

                list :: lists ->
                    list :: onIndex is lists fn


onWidgetStyle : Model -> List Int -> (Animation.State -> Animation.State) -> Model
onWidgetStyle model index fn =
    { model
        | widgets =
            onIndex index model.widgets <|
                onStyle fn
    }


update : Msg -> Model -> Return.Return Msg Model
update action model =
    case action of
        Tick list ->
            (onWidgetStyle model list <|
                (Animation.interrupt
                    --[ Animation.to
                    --    [ Animation.rotate (turn 1) ]
                    --, Animation.set
                    --    [ Animation.rotate (turn 0) ]
                    --]
                    [ Animation.to
                        [ Animation.translate (px 0) (px -10) ]
                      --, Animation.wait (1 * Time.millisecond)
                      --, Animation.to
                      --    [ Animation.translate (px 0) (px 0) ]
                    ]
                )
            )
                |> Return.singleton

        Animate time ->
            { model
                | widgets =
                    List.map
                        (List.map <| onStyle (Animation.update time))
                        model.widgets
            }
                |> Return.singleton


viewWidget : Widget -> Html.Html Msg
viewWidget widget =
    Html.div
        (Animation.render widget.style
            ++ [ Html.Attributes.style
                    [ ( "position", "relative" )
                    , ( "text-align", "center" )
                    , ( "cursor", "pointer" )
                    , ( "border-style", "solid" )
                    , ( "vertical-align", "middle" )
                    ]
               , Html.Events.onMouseOver (Tick [ 1 ])
               ]
        )
        [ Html.text widget.label ]


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color", "#f0f0f0" )
            , ( "padding", "0" )
            , ( "font-family", ("calibri, helvetica, arial, sans-serif") )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "left", "0px" )
                , ( "top", "0px" )
                , ( "width", "100%" )
                ]
            ]
            (List.map (\a -> [ Html.div [] (List.map viewWidget a) ]) model.widgets |> List.concat)
        ]


block : Types.Block
block =
    case
        Json.Decode.decodeString Types.decodeBlock """
        {
          "tempo" : 260,
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
    let
        initialWidgetStyle =
            Animation.style
                [ Animation.display Animation.inlineBlock
                , Animation.width (px 100)
                , Animation.height (px 100)
                , Animation.margin (px 5)
                , Animation.padding (px 2)
                , Animation.rotate (turn 0.0)
                , Animation.rotate3d (turn 0.0) (turn 0.0) (turn 0.0)
                , Animation.translate (px 0) (px 0)
                , Animation.opacity 1
                , Animation.backgroundColor Color.white
                , Animation.color Color.black
                , Animation.scale 1.0
                , Animation.borderColor Color.white
                , Animation.borderWidth (px 4)
                , Animation.borderRadius (px 8)
                , Animation.translate3d (percent 0) (percent 0) (px 0)
                , Animation.shadow
                    { offsetX = 0
                    , offsetY = 1
                    , size = 0
                    , blur = 2
                    , color = rgba 0 0 0 0.1
                    }
                ]
    in
        { widgets =
            block.accents
                |> List.foldl
                    (\accent ( widgets, leftAccents ) ->
                        (List.repeat accent 0
                            |> List.indexedMap
                                (\index _ ->
                                    { index = leftAccents ++ [ (index + 1) ]
                                    , label = toString <| index + 1
                                    , style = initialWidgetStyle
                                    }
                                )
                            |> (\f ->
                                    widgets
                                        ++ [ f ]
                               )
                            |> Basics.flip (,) (leftAccents ++ [ accent ])
                        )
                    )
                    ( [], [] )
                |> (\( a, b ) -> a)
        }
            |> Return.singleton


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate <|
        (List.map (List.map .style) model.widgets |> List.concat)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
