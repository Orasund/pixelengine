module AsteroidMiner.View.Tileset.Big exposing
    ( container
    , conveyorBelt
    , delete
    , floor
    , gameMenuButton
    , merger
    , mine
    , pickUp
    , sorter
    , tutorialMenuButton
    )

import AsteroidMiner.Data exposing (spriteSize)
import AsteroidMiner.View.Tileset as Tileset
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 16
        , spriteHeight = 16
        }


toImage : Tile msg -> Image msg
toImage tile =
    Image.fromTile tile tileset


defaultButton : Tile msg -> Image msg
defaultButton symbol =
    Image.multipleImages
        [ ( ( 0, 0 ), Tile.fromPosition ( 0, 4 ) |> toImage )
        , ( ( spriteSize / 2, spriteSize / 2 ), Image.fromTile symbol Tileset.tileset )
        ]


blackButton : Tile msg -> Image msg
blackButton symbol =
    Image.multipleImages
        [ ( ( 0, 0 ), Tile.fromPosition ( 1, 4 ) |> toImage )
        , ( ( spriteSize / 2, spriteSize / 2 ), Image.fromTile symbol Tileset.tileset )
        ]


blueButton : Tile msg -> Image msg
blueButton symbol =
    Image.multipleImages
        [ ( ( 0, 0 ), Tile.fromPosition ( 2, 4 ) |> toImage )
        , ( ( spriteSize / 2, spriteSize / 2 ), Image.fromTile symbol Tileset.tileset )
        ]


greenButton : Tile msg -> Image msg
greenButton symbol =
    Image.multipleImages
        [ ( ( 0, 0 ), Tile.fromPosition ( 3, 4 ) |> toImage )
        , ( ( spriteSize / 2, spriteSize / 2 ), Image.fromTile symbol Tileset.tileset )
        ]


delete : { image : Image msg, symobl : Image msg }
delete =
    { image =
        Tile.fromPosition ( 7, 1 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 11, 9 )
            |> blackButton
    }


pickUp : Maybe (Tile msg) -> { image : Image msg, symobl : Image msg }
pickUp maybeTile =
    { image =
        case maybeTile of
            Just tile ->
                Image.multipleImages
                    [ ( ( 0, 0 ), Tile.fromPosition ( 6, 1 ) |> toImage )
                    , ( ( spriteSize / 2, 2 + spriteSize / 2 )
                      , Image.fromTile tile Tileset.tileset
                      )
                    ]

            Nothing ->
                Tile.fromPosition ( 6, 1 )
                    |> toImage
    , symobl =
        Tile.fromPosition ( 10, 9 )
            |> blackButton
    }


mine : { image : Image msg, symobl : Image msg }
mine =
    { image =
        Tile.fromPosition ( 4, 1 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 8, 9 )
            |> blueButton
    }


conveyorBelt : { image : Image msg, symobl : Image msg }
conveyorBelt =
    { image =
        Tile.fromPosition ( 4, 0 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 8, 8 )
            |> defaultButton
    }


container : { image : Image msg, symobl : Image msg }
container =
    { image =
        Tile.fromPosition ( 5, 0 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 9, 8 )
            |> greenButton
    }


merger : { image : Image msg, symobl : Image msg }
merger =
    { image =
        Tile.fromPosition ( 6, 0 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 10, 8 )
            |> blueButton
    }


sorter : { image : Image msg, symobl : Image msg }
sorter =
    { image =
        Tile.fromPosition ( 7, 0 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 11, 8 )
            |> greenButton
    }


floor : { image : Image msg, symobl : Image msg }
floor =
    { image =
        Tile.fromPosition ( 5, 1 )
            |> toImage
    , symobl =
        Tile.fromPosition ( 9, 9 )
            |> defaultButton
    }


gameMenuButton : Image msg
gameMenuButton =
    Tile.fromPosition ( 12, 8 )
        |> defaultButton


tutorialMenuButton : Image msg
tutorialMenuButton =
    Tile.fromPosition ( 13, 8 )
        |> defaultButton
