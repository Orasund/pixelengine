module CultSim.Person exposing (Action(..), Person, Position, generate, move, pray, setPraying, tile, tile_bar)

import PixelEngine.Graphics.Tile as Tile exposing (Tile)
import Random


type alias Position =
    { x : Float, y : Float }


type Action
    = Walking
    | PendingPraying
    | Praying Int
    | Dying
    | None


type alias Skin =
    { head : Int, body : Int }


type alias Person =
    { position : Position
    , action : Action
    , skin : Skin
    , praying_duration : Int
    }


tile : Action -> Tile msg
tile action =
    case action of
        PendingPraying ->
            Tile.tile ( 0, 1 )

        Praying _ ->
            Tile.tile ( 0, 2 )

        _ ->
            Tile.tile ( 0, 0 )


tile_bar : Int -> Int -> Tile msg
tile_bar from amount =
    Tile.tile ( 8 * amount, from )


generateSkin : Random.Generator Skin
generateSkin =
    Random.map3
        (\isMale head body ->
            { head =
                if isMale then
                    head

                else
                    head + 50
            , body = body
            }
        )
        (Random.uniform True [False])
        (Random.int 1 12)
        (Random.int 0 7)


generatePosition : Float -> Random.Generator Position
generatePosition r =
    Random.float 0 (2 * pi)
        |> Random.map
            (\phi ->
                let
                    x : Float
                    x =
                        r * cos phi

                    y : Float
                    y =
                        r * sin phi
                in
                { x = x, y = y }
            )


move : Person -> Random.Seed -> ( Person, Random.Seed )
move person =
    Random.step (generatePosition 75)
        >> Tuple.mapFirst
            (\position ->
                { person
                    | position = position
                    , action = Walking
                }
            )


pray : Person -> Person
pray ({ praying_duration, skin } as person) =
    { person
        | action = Praying <| praying_duration + 1
        , praying_duration = praying_duration + 1
        , skin =
            case praying_duration of
                2 ->
                    { skin | body = 10 }

                4 ->
                    { skin | body = 100, head = 100 }

                _ ->
                    skin
    }


setPraying : Person -> Person
setPraying person =
    { person
        | action = PendingPraying
    }


generate : Random.Generator ( String, Person )
generate =
    Random.map3
        (\position float skin ->
            ( "person_" ++ String.fromFloat float
            , { position = position
              , action = None
              , skin = skin
              , praying_duration = 0
              }
            )
        )
        (generatePosition 135)
        (Random.float 0 1)
        generateSkin
