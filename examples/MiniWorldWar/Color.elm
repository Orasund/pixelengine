module MiniWorldWar.Color exposing(
    Color(..),
    isBlue,
    fromBool,
    toInt
  )

type Color
    = Blue
    | Red

isBlue : Color -> Bool
isBlue color =
    color == Blue

fromBool : Bool -> Color
fromBool bool =
    if bool then
        Blue
    else
        Red

toInt : Color -> Int
toInt color =
    if color == Blue then
        0
    else
        1