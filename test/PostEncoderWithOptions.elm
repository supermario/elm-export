module PostEncoderWithOptions exposing (..)

import CommentEncoder exposing (..)
import Json.Encode
import PostType exposing (..)


encodePost : Post -> Json.Encode.Value
encodePost x =
    Json.Encode.object
        [ ( "jsonPostId", Json.Encode.int x.elmPostId )
        , ( "jsonPostName", Json.Encode.string x.elmPostName )
        , ( "jsonPostAge", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.elmPostAge )
        , ( "jsonPostComments", (Json.Encode.list << List.map encodeComment) x.elmPostComments )
        , ( "jsonPostPromoted", (Maybe.withDefault Json.Encode.null << Maybe.map encodeComment) x.elmPostPromoted )
        , ( "jsonPostAuthor", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.elmPostAuthor )
        ]
