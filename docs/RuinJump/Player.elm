module RuinJump.Player exposing (FaceingDirection(..), Player, drop,PlayerAction(..),update, fall, jump, move)

import Dict exposing (Dict)
import RuinJump.Config as Config


type alias Map a =
    Dict ( Int, Int ) a


type alias Player =
    { pos : ( Int, Int )
    , action : PlayerAction
    , faceing : FaceingDirection
    }


type FaceingDirection
    = FaceingLeft
    | FaceingRight


type PlayerAction
    = Standing
    | Falling


update : Player -> Map a -> (Maybe a -> Bool) -> { newPlayer : Player, nextTick : Bool }
update ({ pos } as player) map isOccupied =
    let
        ( x, y ) =
            pos

        defaultCase : { newPlayer : Player, nextTick : Bool }
        defaultCase =
            { newPlayer = { player | action = Standing }
            , nextTick = False
            }
    in
    if
        (map |> Dict.get ( x, y + 2 ) |> isOccupied)
            || (map |> Dict.get ( x + 1, y + 2 ) |> isOccupied)
    then
        defaultCase

    else
        { newPlayer = player |> fall map
        , nextTick = True
        }


fall : Map a -> Player -> Player
fall map ({ pos, faceing } as player) =
    let
        ( x, y ) =
            pos
    in
    { player | pos = ( x, y + 1 ), action = Falling }
        |> (if
                case faceing of
                    FaceingLeft ->
                        x <= 0

                    FaceingRight ->
                        x >= Config.width - 2
            then
                identity

            else
                forwardByOne map
           )


upwardsByOne : Map a -> Player -> Player
upwardsByOne map ({ pos } as player) =
    let
        ( x, y ) =
            pos

        defaultCase : Player
        defaultCase =
            player
    in
    case map |> Dict.get ( x, y - 1 ) of
        Nothing ->
            case map |> Dict.get ( x + 1, y - 1 ) of
                Nothing ->
                    { player | pos = ( x, y - 1 ) }

                Just _ ->
                    defaultCase

        Just _ ->
            defaultCase

downByOne : Map a -> Player -> Player
downByOne map ({ pos} as player) =
    let
        ( x, y ) =
            pos
        
        defaultCase : Player
        defaultCase =
            player
    in
    case map |> Dict.get ( x , y + 3 ) of
        Nothing ->
            case map |> Dict.get ( x + 1 , y + 3 ) of
                Nothing ->
                     { player | pos = ( x, y + 3) }
                Just _ ->
                    defaultCase
        Just _ ->
            defaultCase

forwardByOne : Map a -> Player -> Player
forwardByOne map ({ pos, faceing } as player) =
    let
        ( x, y ) =
            pos

        dir : Int
        dir =
            case faceing of
                FaceingLeft ->
                    -1

                FaceingRight ->
                    2

        newX : Int
        newX =
            case faceing of
                FaceingLeft ->
                    x - 1

                FaceingRight ->
                    x + 1

        defaultCase : Player
        defaultCase =
            player
    in
    case map |> Dict.get ( x + dir, y ) of
        Nothing ->
            case map |> Dict.get ( x + dir, y + 1 ) of
                Nothing ->
                    { player | pos = ( newX, y ) }

                Just _ ->
                    case map |> Dict.get ( x + dir, y - 1 ) of
                        Nothing ->
                            { player | pos = ( newX, y - 1 ) }

                        Just _ ->
                            defaultCase

        Just _ ->
            case map |> Dict.get ( x + dir, y - 1 ) of
                Nothing ->
                    case map |> Dict.get ( x + dir, y - 2 ) of
                        Nothing ->
                            { player | pos = ( newX, y - 1 ) }

                        Just _ ->
                            defaultCase

                Just _ ->
                    defaultCase

drop : Map a -> Player -> Player
drop =
    downByOne

jump : Map a -> Player -> Player
jump map ({ action } as player) =
    let
        defaultCase : Player
        defaultCase =
            player
    in
    case action of
        Standing ->
            { player | action = Falling }
                |> upwardsByOne map
                |> upwardsByOne map
                |> upwardsByOne map
                |> upwardsByOne map
                |> upwardsByOne map

        Falling ->
            defaultCase


move : FaceingDirection -> Map a -> Player -> Player
move direction map ({ pos, action } as player) =
    let
        defaultCase : Player -> Player
        defaultCase =
            identity

        x : Int
        x =
            pos |> Tuple.first
    in
    { player | faceing = direction }
        |> (case action of
                Standing ->
                    if
                        case direction of
                            FaceingLeft ->
                                x <= 0

                            FaceingRight ->
                                x >= Config.width - 2
                    then
                        defaultCase

                    else
                        forwardByOne map
                            >> forwardByOne map

                Falling ->
                    defaultCase
           )
