module MiniWorldWar.Server exposing( RunningGameTable
    , OpenGameTable
    , OpenGame
    , Response(..)
    , httpDelete
    , httpPut
    , openGameRoute
    , openGameDecoder
    , runningGameRoute
    , runningGameTableDecoder
    , tableDecoder
    )

import Http exposing (Body, Error, Expect)
import MiniWorldWar.Game as Game exposing (Game)
import Json.Decode as D exposing (Decoder)
import Dict exposing (Dict)

url : String
url =
    "https://www.jsonstore.io/1f14b82b88eb39f996d8273444d16c26ad25a14b4cae764b3015b48d9e905a75"

type Response msg =
  Do msg
  | Exit
  | Idle
  | DropRunningGameTable
  | DropOpenGameTable

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
        { method = "PUT"
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

{------------------------
   OpenGame
------------------------}

type alias OpenGame =
    { date : Int}

type alias OpenGameTable =
    Dict String OpenGame

openGameDecoder : Decoder OpenGame
openGameDecoder  =
     D.map
        (\date ->
            { date = date }
        )
        (D.field "date" D.int)