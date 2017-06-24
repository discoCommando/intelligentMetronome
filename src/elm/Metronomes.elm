port module Metronomes exposing (..)

import Metronome
import Types
import Return exposing (..)
import Json.Decode exposing (field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Platform.Cmd exposing (..)
import Platform.Sub exposing (..)
import ViewBlock
import Dom
import Task
import Regex
import Http


type WorkingStatus
    = Playing
    | Paused
    | WaitingForPlay


type alias WorkingState =
    { workingStatus : WorkingStatus
    , previous : List Metronome.Model
    , actual : Metronome.Model
    , next : List Metronome.Model
    }


type Status
    = Idle
    | Working WorkingState


type alias YoutubeState =
    { youtubeId : String
    , url : String
    , startFrom : Float
    , startFromString : String
    , cmdsAfterInit : List (Cmd Msg)
    }


type YoutubeStatus
    = NotExisting
    | Existing YoutubeState


type alias Model =
    { metronomes : List Metronome.Model
    , track : String
    , artist : String
    , status : Status
    , youtubeStatus : YoutubeStatus
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
    , initialTimeCorrection = Nothing
    }


songToModel : Types.Song -> Model
songToModel song =
    { status = Idle
    , track = song.track
    , artist = song.artist
    , metronomes = List.map mapBlock song.blocks
    , youtubeStatus =
        case song.youtube of
            Nothing ->
                NotExisting

            Just ytinfo ->
                Existing
                    { youtubeId = ytinfo.id
                    , startFrom = ytinfo.startFrom
                    , startFromString = ytinfo.startFrom |> Basics.toString
                    , url = "www.youtube.com/watch?v=" ++ ytinfo.id
                    , cmdsAfterInit = [ youtubeShow (), youtubeCueVideo ( ytinfo.id, ytInfor.startFrom ) ]
                    }
    }


modelToSong : Model -> Types.Song
modelToSong model =
    { track = model.track
    , artist = model.artist
    , blocks = List.map .block model.metronomes
    , youtube =
        case model.youtubeStatus of
            NotExisting ->
                Nothing

            Existing ys ->
                Just { id = ys.youtubeId, startFrom = ys.startFrom }
    }


initialSong : Json.Decode.Value -> Types.Song
initialSong v =
    case
        Json.Decode.decodeValue Types.decodeSong v
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
    | AddBlock Int
    | Focus
    | ChangeTrack String
    | ChangeArtist String
    | YoutubePlaying
    | YoutubePaused Time.Time
    | YoutubeButtonClick
    | YoutubeUrlChange String
    | YoutubeStartFromChange String
    | YoutubeReady


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


addAt : Int -> a -> List a -> List a
addAt i a list =
    case list of
        [] ->
            [ a ]

        x :: xs ->
            if (i <= 0) then
                a :: x :: xs
            else
                x :: addAt (i - 1) a xs


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


parseYoutubeId : String -> Maybe String
parseYoutubeId url =
    case (Regex.find (Regex.All) (Regex.regex "^.*((youtu.be\\/)|(v\\/)|(\\/u\\/\\w\\/)|(embed\\/)|(watch\\?))\\??v?=?([^#\\&\\?]*).*") url) of
        [] ->
            Nothing

        x :: _ ->
            case at 6 x.submatches of
                Nothing ->
                    Nothing

                Just x ->
                    case x of
                        Just x ->
                            if (String.length x == 11) then
                                Just x
                            else
                                Nothing

                        Nothing ->
                            Nothing


getNextTime : Float -> List Metronome.Model -> Maybe Time.Time
getNextTime startingTime list =
    let
        blockToTimes =
            list
                |> List.map .block
                |> List.map Metronome.blockToTime
    in
        case
            blockToTimes
                |> List.all ViewBlock.isJust
        of
            True ->
                blockToTimes
                    |> List.map
                        (\s ->
                            case s of
                                Just a ->
                                    a

                                Nothing ->
                                    0
                        )
                    |> List.sum
                    |> (+) (Time.second * startingTime)
                    |> Just

            False ->
                Nothing


update : Msg -> Model -> Return Msg Model
update msg model =
    let
        youtubeReady =
            case model.youtubeStatus of
                Existing ys ->
                    True

                _ ->
                    False

        youtubeCmd cmd =
            if youtubeReady then
                cmd
            else
                Platform.Cmd.none
    in
        case msg of
            ChangeTrack s ->
                { model | track = s }
                    |> Return.singleton

            ChangeArtist s ->
                { model | artist = s }
                    |> Return.singleton

            _ ->
                case model.status of
                    Idle ->
                        case msg of
                            Start ->
                                case model.metronomes of
                                    [] ->
                                        model
                                            |> Return.singleton

                                    metronomeModel :: rest ->
                                        case model.youtubeStatus of
                                            Existing ys ->
                                                { model | status = Working { workingStatus = WaitingForPlay, previous = [], actual = metronomeModel, next = List.map Metronome.makeFinished rest } }
                                                    |> Basics.flip Return.return (youtubePlay <| Just ys.startFrom)

                                            NotExisting ->
                                                Metronome.update Metronome.Start metronomeModel
                                                    |> Return.mapCmd (MetronomeMsg 0)
                                                    |> Return.map
                                                        (\changedMetronomeModel ->
                                                            { model | status = Working { workingStatus = Playing, previous = [], actual = changedMetronomeModel, next = List.map Metronome.makeFinished rest } }
                                                        )

                            YoutubePlaying ->
                                case model.metronomes of
                                    [] ->
                                        model
                                            |> Return.singleton

                                    metronomeModel :: rest ->
                                        Metronome.update Metronome.Start metronomeModel
                                            |> Return.mapCmd (MetronomeMsg 0)
                                            |> Return.map
                                                (\changedMetronomeModel ->
                                                    { model | status = Working { workingStatus = Playing, previous = [], actual = changedMetronomeModel, next = List.map Metronome.makeFinished rest } }
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
                                                    { model | status = Working { previous = [], actual = changedMetronomeModel, next = List.map Metronome.makeFinished rest, workingStatus = Paused } }
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

                            AddBlock i ->
                                { model | metronomes = addAt i Metronome.emptyModel model.metronomes }
                                    |> Return.singleton

                            YoutubeButtonClick ->
                                case model.youtubeStatus of
                                    NotExisting ->
                                        { model | youtubeStatus = Existing <| YoutubeState "" "" 0 "0" [] }
                                            |> Return.singleton
                                            |> Return.command (youtubeShow ())

                                    Existing ys ->
                                        { model | youtubeStatus = NotExisting }
                                            |> Return.singleton
                                            |> Return.command (youtubeHide ())

                            _ ->
                                case model.youtubeStatus of
                                    NotExisting ->
                                        model |> Return.singleton

                                    Existing ys ->
                                        case msg of
                                            YoutubeUrlChange url ->
                                                (case parseYoutubeId url of
                                                    Nothing ->
                                                        ys |> Return.singleton

                                                    Just id ->
                                                        { ys | youtubeId = id }
                                                            |> Return.singleton
                                                )
                                                    |> Return.andThen
                                                        (\ys ->
                                                            if (String.length ys.youtubeId == 11) then
                                                                Return.return ys <| youtubeCueVideo ( ys.youtubeId, ys.startFrom )
                                                            else
                                                                ys |> Return.singleton
                                                        )
                                                    |> Return.map
                                                        (\ys -> { model | youtubeStatus = Existing { ys | url = url } })

                                            YoutubeStartFromChange sf ->
                                                (case String.toFloat sf of
                                                    Ok startFrom ->
                                                        if (startFrom >= 0) then
                                                            { ys | startFrom = startFrom }
                                                        else
                                                            ys

                                                    _ ->
                                                        ys
                                                )
                                                    |> Return.singleton
                                                    |> Return.map
                                                        (\ys -> { model | youtubeStatus = Existing { ys | startFromString = sf } })

                                            YoutubeReady ->
                                                model
                                                    |> Return.singleton
                                                    |> Return.command (Cmd.batch ys.cmdsAfterInit)

                                            _ ->
                                                model |> Return.singleton

                    Working ws ->
                        case ws.workingStatus of
                            Playing ->
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
                                                                |> Return.command (youtubeCmd <| youtubeStop ())

                                                        x :: xs ->
                                                            let
                                                                ( changedX, xCmd ) =
                                                                    Metronome.update Metronome.Start x
                                                            in
                                                                { model | status = Working { ws | previous = ws.previous ++ [ changedMetronomeModel ], actual = changedX, next = xs } }
                                                                    |> Return.singleton
                                                                    |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws) metronomeCmd)
                                                                    |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws + 1) xCmd)
                                                                    |> Return.command (Task.attempt (Basics.always Focus) <| Dom.focus "actual")

                                                _ ->
                                                    { model | status = Working { ws | actual = changedMetronomeModel } }
                                                        |> Return.singleton
                                                        |> Return.command (Platform.Cmd.map (MetronomeMsg <| getActualIndex ws) metronomeCmd)

                                    Pause ->
                                        Metronome.update Metronome.Pause ws.actual
                                            |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                            |> Return.map
                                                (\metronomeModel ->
                                                    { model | status = Working { ws | actual = metronomeModel, workingStatus = Paused } }
                                                )
                                            |> Return.command (youtubeCmd <| youtubePause ())

                                    YoutubePaused time ->
                                        Metronome.update Metronome.Pause ws.actual
                                            |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                            |> Return.map
                                                (\metronomeModel ->
                                                    { ws | actual = metronomeModel, workingStatus = Paused }
                                                )
                                            |> Return.andThen
                                                (\ws ->
                                                    case model.youtubeStatus of
                                                        NotExisting ->
                                                            ws |> Return.singleton

                                                        Existing ys ->
                                                            case
                                                                ws.previous
                                                                    |> getNextTime ys.startFrom
                                                            of
                                                                Just x ->
                                                                    let
                                                                        timeToGo =
                                                                            ((time - x)
                                                                                / Metronome.tempoToMs ws.actual.block.tempo
                                                                            )
                                                                                |> Basics.floor
                                                                                |> Basics.toFloat
                                                                                |> (*) (Metronome.tempoToMs ws.actual.block.tempo)
                                                                    in
                                                                        x
                                                                            |> (+) timeToGo
                                                                            |> Time.inSeconds
                                                                            |> youtubeSeekTo
                                                                            |> Return.return ws

                                                                Nothing ->
                                                                    ws |> Return.singleton
                                                )
                                            |> Return.map
                                                (\ws ->
                                                    { model | status = Working ws }
                                                )

                                    Next ->
                                        case ws.next of
                                            [] ->
                                                { model | status = Idle }
                                                    |> Return.singleton
                                                    |> Return.command (youtubeCmd <| youtubeStop ())

                                            x :: xs ->
                                                Metronome.update Metronome.Start x
                                                    |> Return.mapCmd (MetronomeMsg <| getActualIndex ws + 1)
                                                    |> Return.map
                                                        (\metronomeModel ->
                                                            { ws | previous = ws.previous ++ [ Metronome.makeFinished ws.actual ], actual = metronomeModel, next = xs }
                                                        )
                                                    |> Return.command (Task.attempt (Basics.always Focus) <| Dom.focus "actual")
                                                    |> Return.andThen
                                                        (\ws ->
                                                            case model.youtubeStatus of
                                                                NotExisting ->
                                                                    ws |> Return.singleton

                                                                Existing ys ->
                                                                    case
                                                                        ws.previous
                                                                            |> getNextTime ys.startFrom
                                                                    of
                                                                        Just x ->
                                                                            x
                                                                                |> Time.inSeconds
                                                                                |> youtubeSeekTo
                                                                                |> Return.return ws

                                                                        Nothing ->
                                                                            ws |> Return.singleton
                                                        )
                                                    |> Return.map
                                                        (\ws ->
                                                            { model | status = Working ws }
                                                        )

                                    Stop ->
                                        { model | status = Idle }
                                            |> Return.singleton
                                            |> Return.command (youtubeCmd <| youtubeStop ())

                                    _ ->
                                        model
                                            |> Return.singleton

                            Paused ->
                                case msg of
                                    Start ->
                                        case model.youtubeStatus of
                                            Existing ys ->
                                                { model | status = Working { ws | workingStatus = WaitingForPlay } }
                                                    |> Basics.flip Return.return (youtubePlay Nothing)

                                            NotExisting ->
                                                Metronome.update Metronome.Start ws.actual
                                                    |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                                    |> Return.map
                                                        (\metronomeModel ->
                                                            { model | status = Working { ws | actual = metronomeModel, workingStatus = Playing } }
                                                        )

                                    YoutubePlaying ->
                                        Metronome.update Metronome.Start ws.actual
                                            |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                            |> Return.map
                                                (\metronomeModel ->
                                                    { model | status = Working { ws | actual = metronomeModel, workingStatus = Playing } }
                                                )

                                    Stop ->
                                        { model | status = Idle }
                                            |> Return.singleton
                                            |> Return.command (youtubeCmd <| youtubeStop ())

                                    Next ->
                                        case ws.next of
                                            [] ->
                                                { model | status = Idle }
                                                    |> Return.singleton
                                                    |> Return.command (youtubeCmd <| youtubeStop ())

                                            x :: xs ->
                                                { ws | previous = ws.previous ++ [ Metronome.makeFinished ws.actual ], actual = Metronome.makePaused x, next = xs, workingStatus = Paused }
                                                    |> Return.singleton
                                                    |> Return.command (Task.attempt (Basics.always Focus) <| Dom.focus "actual")
                                                    |> Return.andThen
                                                        (\ws ->
                                                            case model.youtubeStatus of
                                                                NotExisting ->
                                                                    ws |> Return.singleton

                                                                Existing ys ->
                                                                    case
                                                                        ws.previous
                                                                            |> getNextTime ys.startFrom
                                                                    of
                                                                        Just x ->
                                                                            x
                                                                                |> Time.inSeconds
                                                                                |> youtubeSeekTo
                                                                                |> Return.return ws

                                                                        Nothing ->
                                                                            ws |> Return.singleton
                                                        )
                                                    |> Return.map
                                                        (\ws ->
                                                            { model | status = Working ws }
                                                        )

                                    YoutubePaused time ->
                                        (case model.youtubeStatus of
                                            NotExisting ->
                                                ws |> Return.singleton

                                            Existing ys ->
                                                case
                                                    ws.previous
                                                        |> getNextTime ys.startFrom
                                                of
                                                    Just x ->
                                                        let
                                                            timeToGo =
                                                                ((time - x)
                                                                    / Metronome.tempoToMs ws.actual.block.tempo
                                                                )
                                                                    |> Basics.floor
                                                                    |> Basics.toFloat
                                                                    |> (*) (Metronome.tempoToMs ws.actual.block.tempo)
                                                        in
                                                            x
                                                                |> (+) timeToGo
                                                                |> Time.inSeconds
                                                                |> youtubeSeekTo
                                                                |> Return.return ws

                                                    Nothing ->
                                                        ws |> Return.singleton
                                        )
                                            |> Return.map
                                                (\ws ->
                                                    { model | status = Working ws }
                                                )

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

                            WaitingForPlay ->
                                case msg of
                                    YoutubePlaying ->
                                        Metronome.update Metronome.Start ws.actual
                                            |> Return.mapCmd (MetronomeMsg <| getActualIndex ws)
                                            |> Return.map
                                                (\metronomeModel ->
                                                    { model | status = Working { ws | actual = metronomeModel, workingStatus = Playing } }
                                                )

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
                (List.indexedMap
                    (\index metronome ->
                        [ div
                            [ class "add-block-button" ]
                            [ button [ onClick <| AddBlock index ] [ Html.text "+" ] ]
                        , Metronome.view metronome |> Html.map (MetronomeMsg index)
                        ]
                    )
                    model.metronomes
                    |> List.concat
                )
                    ++ [ div
                            [ class "add-block-button" ]
                            [ button [ onClick <| AddBlock <| List.length model.metronomes ] [ Html.text "+" ] ]
                       ]

            Working ws ->
                let
                    actualIndex =
                        getActualIndex ws
                in
                    [ div
                        [ class "past" ]
                      <|
                        List.indexedMap
                            (\index metronome -> Metronome.view metronome |> Html.map (MetronomeMsg index))
                            ws.previous
                    ]
                        ++ [ div
                                [ id "actual", Html.Attributes.tabindex 0 ]
                                [ (Metronome.view ws.actual |> Html.map (MetronomeMsg actualIndex)) ]
                           ]
                        ++ List.indexedMap (\index metronome -> Metronome.view metronome |> Html.map (MetronomeMsg <| index + actualIndex)) ws.next
        )


view : Model -> Html Msg
view model =
    div []
        [ Html.text <| Basics.toString model
        , Html.div [ class "title" ] [ text "Intelligent Metronome" ]
        , Html.div [ class "info" ]
            ([ text "Track"
             , p [] []
             , input [ type_ "text", onInput ChangeTrack, value model.track ] []
             , p [] []
             , text "Artist"
             , p [] []
             , input [ type_ "text", onInput ChangeArtist, value model.artist ] []
             ]
                ++ [ text "YouTube"
                   , p [] []
                   ]
                ++ (let
                        isIdle =
                            model.status == Idle
                    in
                        case model.youtubeStatus of
                            NotExisting ->
                                [ button [ class "youtube-button", onClick YoutubeButtonClick, disabled <| Basics.not isIdle ] [ text "Off" ]
                                ]

                            Existing ys ->
                                [ button [ class "youtube-button", onClick YoutubeButtonClick, disabled <| Basics.not isIdle ] [ text "On" ]
                                , p [] []
                                , text "YouTube Link"
                                , p [] []
                                , input [ type_ "text", value ys.url, onInput YoutubeUrlChange, disabled <| Basics.not isIdle ] []
                                , p [] []
                                , text "Start from (in seconds)"
                                , p [] []
                                , input [ type_ "text", value ys.startFromString, onInput YoutubeStartFromChange, disabled <| Basics.not isIdle ] []
                                , p [] []
                                , div [ class "player-empty-space" ] []
                                ]
                   )
            )
        , Html.div [ class "control-buttons" ]
            [ Html.button
                (case model.status of
                    Idle ->
                        [ Html.Events.onClick Start
                        , id "start-button"
                        , class "control-button"
                        , disabled <| List.length model.metronomes <= 0
                        ]

                    Working ws ->
                        if (ws.workingStatus == Paused) then
                            [ Html.Events.onClick Start
                            , id "start-button"
                            , class "control-button"
                            ]
                        else
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
                , disabled <| List.length model.metronomes <= 0
                ]
                []
            ]
        , viewMetronomes model
        , div [ class "footer" ]
            [ text <| "All code for Intelligent Metronome is open source. "
            , a [ href "https://github.com/discoCommando/intelligentMetronome" ] [ text <| "Check it out!" ]
            , text <| " — © 2017 discoCommando"
            ]
        ]


init : Json.Decode.Value -> Return Msg Model
init v =
    initialSong v
        |> songToModel
        |> Return.singleton


port youtubePlay : Maybe Float -> Platform.Cmd.Cmd msg


port youtubePause : () -> Platform.Cmd.Cmd msg


port youtubeStop : () -> Platform.Cmd.Cmd msg


port youtubePlaying : (() -> msg) -> Sub msg


port youtubePaused : (Float -> msg) -> Sub msg


port youtubeReady : (() -> msg) -> Sub msg


port youtubeCueVideo : ( String, Float ) -> Cmd msg


port youtubeShow : () -> Cmd msg


port youtubeHide : () -> Cmd msg


port youtubeSeekTo : Float -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.status of
        Idle ->
            Sub.none

        Working ws ->
            Sub.map TickMsg <| Metronome.subscriptions ws.actual
    , youtubePlaying <| Basics.always YoutubePlaying
    , youtubePaused <| (\float -> YoutubePaused <| Time.second * float)
    , youtubeReady <| Basics.always YoutubeReady
    ]
        |> Platform.Sub.batch


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
