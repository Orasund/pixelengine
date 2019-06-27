module AsteroidMiner.Data.Item exposing (Item(..), fromInt, toInt)


type Item
    = Stone


toInt : Item -> Int
toInt item =
    case item of
        Stone ->
            1


fromInt : Int -> Item
fromInt int =
    case int of
        _ ->
            Stone
