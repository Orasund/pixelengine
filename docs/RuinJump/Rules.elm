module RuinJump.Rules exposing (parkour, placeDirt, removeGrass, placeGrass)

import CellAutomata exposing (Rule, RuleExpression(..),anyNeighborhood)
import RuinJump.MapElement exposing (Block(..))


parkour : List (Rule Block)
parkour =
    [ { from = Nothing
      , neighbors =
            { anyNeighborhood
            | northEast = Exactly <| Nothing
            , east = Exactly <| Just Stone
            , southEast = Exactly <| Nothing
            }
      , to = Just Stone
      }
    ]


placeGrass : List (Rule Block)
placeGrass =
    [ { from = Just Dirt
      , neighbors =
            { anyNeighborhood
            | north = Exactly <| Nothing
            }
      , to = Just Grass
      }
    , { from = Just Dirt
      , neighbors =
            { anyNeighborhood
            | north = Exactly <| Just Grass
            , northWest = Exactly <| Just Grass
            }
      , to = Just Grass
      }
    , { from = Just Dirt
      , neighbors =
            { anyNeighborhood
            | north = Exactly <| Just Grass
            , northEast = Exactly <| Just Grass
            }
      , to = Just Grass
      }
    , { from = Just Dirt
      , neighbors =
            { anyNeighborhood
            | north = Exactly <| Just Grass
            , northEast = Exactly <| Just Grass
            }
      , to = Just Grass
      }
    ]

removeGrass : List (Rule Block)
removeGrass =
    [ { from = Just Grass
      , neighbors =
            { anyNeighborhood
            | north = Exactly <| Nothing
            , east = Exactly <| Nothing
            , west = Exactly <| Nothing
            , south = Exactly <| Nothing
            }
      , to = Nothing
      }
    , { from = Just Grass
      , neighbors =
            { anyNeighborhood
            | east = Exactly <| Just Stone
            , south = Exactly <| Nothing
            }
      , to = Just Grass
      }
    , { from = Just Grass
      , neighbors =
            { anyNeighborhood
            | east = Exactly <| Just Dirt
            , south = Exactly <| Nothing
            }
      , to = Just Grass
      }
    , { from = Just Grass
      , neighbors =
            { anyNeighborhood
            | east = Exactly <| Just Grass
            , west = Exactly <| Just Grass
            , south = Exactly <| Nothing
            }
      , to = Just Grass
      }
    , { from = Just Grass
      , neighbors =
            { anyNeighborhood
            | south = Exactly <| Nothing
            }
      , to = Nothing
      }
    ]

placeDirt : List (Rule Block)
placeDirt =
    [ { from = Nothing
      , neighbors =
            { anyNeighborhood
            | east = Exactly <| Just Dirt
            , southEast = Exactly <| Just Dirt
            , south = Exactly <| Just Dirt
            , southWest = Exactly <| Just Dirt
            , west = Exactly <| Just Dirt
            }
      , to = Nothing
      }
    , { from = Nothing
      , neighbors =
            { anyNeighborhood
            | southEast = Exactly <| Just Dirt
            , south = Exactly <| Just Dirt
            , southWest = Exactly <| Just Dirt
            }
      , to = Just Dirt
      }
    , { from = Just Dirt
      , neighbors =
            { anyNeighborhood
            | east = Exactly <| Nothing
            , southEast = Exactly <| Just Dirt
            , south = Exactly <| Just Dirt
            , southWest = Exactly <| Just Dirt
            , west = Exactly <| Nothing
            }
      , to = Nothing
      }
    , { from = Just Stone
      , neighbors =
            { anyNeighborhood
            | east = Exactly <| Just Dirt
            }
      , to = Just Dirt
      }
    , { from = Just Stone
      , neighbors =
            { anyNeighborhood
            | west = Exactly <| Just Dirt
            }
      , to = Just Dirt
      }
    ]
