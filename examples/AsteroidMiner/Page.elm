module AsteroidMiner.Page exposing (GameMode(..))

import Random exposing (Seed)


type GameMode
    = Game Seed
    | Tutorial Seed
