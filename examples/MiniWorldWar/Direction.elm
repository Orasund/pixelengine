module MiniWorldWar.Direction exposing (Direction(..),fromString,toString)

type Direction
    = Up
    | Down
    | Left
    | Right

fromString : String -> Direction
fromString string =
    case string of
      "Up"-> Up
      "Down" -> Down
      "Left" -> Left
      "Right" -> Right
      _ -> Up

toString : Direction -> String
toString direction =
    case direction of
      Up -> "Up"
      Down -> "Down"
      Left -> "Left"
      Right -> "Right"