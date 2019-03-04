module Jsonstore exposing (delete, get, insert, update)

import Http exposing (Error, Resolver)
import Json.Decode as D exposing (Decoder)
import Json.Encode exposing (Value)
import Task exposing (Task)



-----------------------------------------
--
----------------------------------------


resolve : D.Decoder a -> Resolver Error a
resolve decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody err)


resolveWhatever : Resolver Error ()
resolveWhatever =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok ()


insert : String -> Value -> Task Error ()
insert url value =
    Http.task
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.jsonBody value
        , resolver = resolveWhatever
        , timeout = Nothing
        }


delete : String -> Task Error ()
delete url =
    Http.task
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = resolveWhatever
        , timeout = Nothing
        }


get : String -> Decoder a -> Task Error a
get url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = resolve <| D.field "result" <| decoder
        , timeout = Nothing
        }


update : { url : String, decoder : Decoder a, value : a -> Value } -> Task Error a
update { url, decoder, value } =
    get url decoder
        |> Task.andThen (value >> insert url)
