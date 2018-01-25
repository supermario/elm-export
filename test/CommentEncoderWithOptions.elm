module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "jsonCommentPostId", Json.Encode.int x.elmCommentPostId )
        , ( "jsonCommentText", Json.Encode.string x.elmCommentText )
        , ( "jsonCommentMainCategories", (Exts.Json.Encode.tuple2 Json.Encode.string Json.Encode.string) x.elmCommentMainCategories )
        , ( "jsonCommentPublished", Json.Encode.bool x.elmCommentPublished )
        , ( "jsonCommentCreated", (Json.Encode.string << Date.Extra.toUtcIsoString) x.elmCommentCreated )
        , ( "jsonCommentTags", (Exts.Json.Encode.dict Json.Encode.string Json.Encode.int) x.elmCommentTags )
        ]
