module Natural exposing (Natural16(..),toIntFrom16,fromIntTo16)

type Natural16 = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelfth
  | Thirdteen
  | Fourteen
  | Fifteen

fromIntTo16 : Int -> Maybe Natural16
fromIntTo16 int = case int of
  0 -> Just Zero
  1 -> Just One
  2 -> Just Two
  3 -> Just Three
  4 -> Just Four
  5 -> Just Five
  6 -> Just Six
  7 -> Just Seven
  8 -> Just Eight
  9 -> Just Nine
  10 -> Just Ten
  11 -> Just Eleven
  12 -> Just Twelfth
  13 -> Just Thirdteen
  14 -> Just Fourteen
  15 -> Just Fifteen
  _ -> Nothing

toIntFrom16 : Natural16 -> Int
toIntFrom16 nat16 = case nat16 of
  Zero -> 0
  One -> 1
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Eleven -> 11
  Twelfth -> 12
  Thirdteen -> 13
  Fourteen -> 14
  Fifteen -> 15