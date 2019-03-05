module MiniWorldWar.Request exposing
    ( OpenGame
    , OpenGameTable
    , Response(..)
    , RunningGameTable
    , dropOpenGameTable
    , dropRunningGameTable
    , httpDelete
    , httpPut
    , openGameDecoder
    , openGameEncoder
    , openGameRoute
    , runningGameRoute
    , runningGameTableDecoder
    , tableDecoder
    )

import Dict exposing (Dict)
import Http exposing (Body, Error, Expect)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Data.Game as Game exposing (Game)


url : String
url =
    "https://www.jsonstore.io/1f14b82b88eb39f996d8273444d16c26ad25a14b4cae764b3015b48d9e905a75"

type Response msg
    = Please msg
    | Exit --Savely Close current progess -> Then Reset
    | Reset --Restart Everything
    | Idle --Do Nothing
    | ResetWithError Error -- -> Restart
    | ExitWithError Error -- -> Close Process -> Then Reset
    | DropRunningGameTable Error -- -> Reset
    | DropOpenGameTable Error -- -> Reset

type alias ServerResponse =
    Response Never


openGameRoute : String
openGameRoute =
    url ++ "/openGame"


runningGameRoute : String
runningGameRoute =
    url ++ "/runningGame"


httpDelete : { url : String, expect : Expect msg } -> Cmd msg
httpDelete record =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = record.url
        , body = Http.emptyBody
        , expect = record.expect
        , timeout = Nothing
        , tracker = Nothing
        }


httpPut : { url : String, body : Body, expect : Expect msg } -> Cmd msg
httpPut record =
    Http.request
        { method = "POST"--"PUT"
        , headers = []
        , url = record.url
        , body = record.body
        , expect = record.expect
        , timeout = Nothing
        , tracker = Nothing
        }


tableDecoder : Decoder a -> Decoder (Dict String a)
tableDecoder decoder =
    D.field "result" <|
        D.map (Maybe.withDefault Dict.empty) <|
            D.nullable <|
                D.dict <|
                    decoder



{------------------------
   RunningGame
------------------------}


type alias RunningGameTable =
    Dict String Game


runningGameTableDecoder : Decoder RunningGameTable
runningGameTableDecoder =
    tableDecoder <| Game.decoder


dropRunningGameTable : Cmd ServerResponse
dropRunningGameTable =
    let
        response : Result Error () -> ServerResponse
        response =
            always Reset
    in
    httpDelete
        { url = runningGameRoute
        , expect = Http.expectWhatever response
        }



{------------------------
   OpenGame
------------------------}


type alias OpenGame =
    { date : Int }


type alias OpenGameTable =
    Dict String OpenGame


openGameDecoder : Decoder OpenGame
openGameDecoder =
    D.map
        (\date ->
            { date = date }
        )
        (D.field "date" D.int)


openGameEncoder : OpenGame -> Value
openGameEncoder { date } =
    E.object
        [ ( "date", E.int date )
        ]


dropOpenGameTable : Cmd ServerResponse
dropOpenGameTable =
    let
        response : Result Error () -> ServerResponse
        response =
            always Reset
    in
    httpDelete
        { url = openGameRoute
        , expect = Http.expectWhatever response
        }
