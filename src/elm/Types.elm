module Types exposing (..)

import Json.Encode
import Json.Decode exposing (field)


type alias Block =
    { tempo : Int
    , accents : List Int
    , maybeCount : Maybe Int
    }


maybeEncode : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
maybeEncode encoder a =
    case a of
        Just x ->
            encoder x

        Nothing ->
            Json.Encode.null


decodeBlock : Json.Decode.Decoder Block
decodeBlock =
    Json.Decode.map3 Block
        (field "tempo" Json.Decode.int)
        (field "accents" <| Json.Decode.list Json.Decode.int)
        (field "maybeCount" <| Json.Decode.maybe Json.Decode.int)


encodeBlock : Block -> Json.Encode.Value
encodeBlock record =
    Json.Encode.object
        [ ( "tempo", Json.Encode.int <| record.tempo )
        , ( "accents", Json.Encode.list <| List.map Json.Encode.int <| record.accents )
        , ( "maybeCount", maybeEncode Json.Encode.int record.maybeCount )
        ]


type alias YoutubeInfo =
    { id : String
    , startFrom : Float
    }


type alias Song =
    { track : String
    , artist : String
    , blocks : List Block
    , youtube : Maybe YoutubeInfo
    }


decodeYoutubeInfo : Json.Decode.Decoder YoutubeInfo
decodeYoutubeInfo =
    Json.Decode.map2 YoutubeInfo
        (field "id" Json.Decode.string)
        (field "startFrom" Json.Decode.float)


decodeSong : Json.Decode.Decoder Song
decodeSong =
    Json.Decode.map4 Song
        (field "track" Json.Decode.string)
        (field "artist" Json.Decode.string)
        (field "blocks" <| Json.Decode.list decodeBlock)
        (field "youtube" <| Json.Decode.maybe decodeYoutubeInfo)


encodeYoutubeInfo : YoutubeInfo -> Json.Encode.Value
encodeYoutubeInfo ytinfo =
    Json.Encode.object
        [ ( "id", Json.Encode.string ytinfo.id )
        , ( "startFrom", Json.Encode.float ytinfo.startFrom )
        ]


encodeSong : Song -> Json.Encode.Value
encodeSong record =
    Json.Encode.object
        [ ( "track", Json.Encode.string <| record.track )
        , ( "artist", Json.Encode.string <| record.artist )
        , ( "blocks", Json.Encode.list <| List.map encodeBlock <| record.blocks )
        , ( "blocks", maybeEncode encodeYoutubeInfo record.youtube )
        ]


zip : List a -> List b -> List ( a, b )
zip l1 l2 =
    case ( l1, l2 ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        _ ->
            []


alreadyPassed : List Int -> List Int -> List Int
alreadyPassed accents passed =
    let
        passedWithZeros =
            List.repeat (List.length accents - List.length passed) 0 ++ passed
    in
        zip accents passedWithZeros
            |> List.map (Basics.uncurry (-))
            |> Debug.log "alreadyPassed"
