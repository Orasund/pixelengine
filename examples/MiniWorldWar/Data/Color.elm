module MiniWorldWar.Data.Color exposing(
    Color(..),
    isBlue,
    fromBool,
    toInt,
    flip
  )

type Color
    = Blue
    | Red

flip : Color -> Color
flip color =
    case color of
        Blue -> Red
        Red -> Blue

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