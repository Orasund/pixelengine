module DigDigBoom.View.Screen exposing (death, menu, world)

import Color
import DigDigBoom.Cell as Cell exposing (Cell(..), EnemyType(..), ItemType(..), SolidType(..))
import DigDigBoom.Component.Inventory as Inventory
import DigDigBoom.Player exposing (PlayerData)
import DigDigBoom.View.Tile as TileView
import Grid as Grid exposing (Grid)
import Grid.Position exposing (Position)
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


logo : Tileset
logo =
    Tile.tileset { source = "title_image.png", spriteHeight = 128, spriteWidth = 128 }


death : List (Area msg)
death =
    let
        width : Int
        width =
            16
    in
    [ PixelEngine.tiledArea
        { rows = 2
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        []
    , PixelEngine.tiledArea
        { rows = 2
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        (List.concat
            [ ( 4, 0 ) |> TileView.text "You have" TileView.colorWhite
            , ( 6, 1 ) |> TileView.text "died" TileView.colorWhite
            ]
        )
    , PixelEngine.imageArea
        { height = toFloat <| 12 * 16
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        }
        [ ( ( toFloat <| (16 * width) // 2 - 64, toFloat <| (12 * width) // 2 - 64 )
          , Image.fromSrc "skull.png"
          )
        ]
    , PixelEngine.tiledArea
        { rows = 2
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        (List.concat
            [ ( 4, 0 ) |> TileView.text "Press any" TileView.colorWhite
            , ( 6, 1 ) |> TileView.text "button" TileView.colorWhite
            ]
        )
    , PixelEngine.tiledArea
        { rows = 2
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        []
    ]


menu : List (Area msg)
menu =
    let
        width : Int
        width =
            16

        tile : Tile msg
        tile =
            Tile.fromPosition ( 0, 0 ) |> Tile.animated 2
    in
    [ PixelEngine.tiledArea
        { rows = 2
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        []
    , PixelEngine.tiledArea
        { rows = 3
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        (List.concat
            [ ( 5, 0 ) |> TileView.text "DIG" TileView.colorWhite
            , ( 6, 1 ) |> TileView.text "DIG" TileView.colorWhite
            , ( 6, 2 ) |> TileView.text "BOOM" TileView.colorWhite
            ]
        )
    , PixelEngine.imageArea
        { height = toFloat <| 9 * 16
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        }
        [ ( ( toFloat <| (16 * width) // 2 - 64, 0 ), Image.fromTile tile logo )
        ]
    , PixelEngine.tiledArea
        { rows = 4
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        (List.concat
            [ ( 1, 0 ) |> TileView.text "a" TileView.colorWhite
            , [ ( ( 2, 0 ), TileView.arrow_left TileView.colorWhite )
              , ( ( 3, 0 ), TileView.arrow_right TileView.colorWhite )
              ]
            , ( 4, 0 ) |> TileView.text "d -Start" TileView.colorWhite
            , ( 8, 1 ) |> TileView.text "Game" TileView.colorWhite
            , ( 1, 3 ) |> TileView.text "w" TileView.colorWhite
            , [ ( ( 2, 3 ), TileView.arrow_up TileView.colorWhite )
              , ( ( 3, 3 ), TileView.arrow_down TileView.colorWhite )
              ]
            , ( 4, 3 ) |> TileView.text "s -Tutorial" TileView.colorWhite
            ]
        )
    , PixelEngine.tiledArea
        { rows = 2
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        []
    ]


world : Int -> Grid Cell -> PlayerData -> List ( Position, Tile msg ) -> List (Area msg)
world worldSeed map player hints =
    [ PixelEngine.tiledArea
        { rows = 1
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        (( 0, 0 )
            |> TileView.text
                ("X-exit score:"
                    ++ (if (worldSeed // abs worldSeed) == -1 then
                            "-"

                        else
                            " "
                       )
                    ++ String.fromInt (modBy 100 (abs worldSeed) // 10)
                    ++ String.fromInt (modBy 10 (abs worldSeed))
                )
                TileView.colorWhite
        )
    , PixelEngine.tiledArea
        { rows = 16
        , background =
            PixelEngine.imageBackground
                { source = "groundTile.png", width = 16, height = 16 }
        , tileset = TileView.tileset
        }
        (hints
            |> List.append
                (map
                    |> Grid.toList
                    |> List.map
                        (\( pos, cell ) -> ( pos, Cell.getImage cell ))
                )
        )
    , PixelEngine.tiledArea
        { rows = 3
        , background = PixelEngine.colorBackground (Color.rgb255 20 12 28)
        , tileset = TileView.tileset
        }
        (List.concat
            [ [ ( ( 4, 2 ), TileView.arrow_up TileView.colorWhite ) ]
            , ( 5, 2 ) |> TileView.text "SPACE-use" TileView.colorWhite
            , [ ( ( 0, 0 ), TileView.arrow_down TileView.colorWhite ) ]
            , ( 1, 0 ) |> TileView.text "floor" TileView.colorWhite
            , ( 2, 1 ) |> TileView.text "Q" TileView.colorWhite
            , [ ( ( 3, 1 ), TileView.arrow_left TileView.colorWhite ) ]
            , [ ( ( 12, 1 ), TileView.arrow_right TileView.colorWhite ) ]
            , ( 13, 1 ) |> TileView.text "E" TileView.colorWhite
            , case player.inventory |> Inventory.ground of
                Just a ->
                    [ ( ( 0, 1 ), Cell.getImage (Item a) ) ]

                Nothing ->
                    []
            , List.range 0 (player.lifes - 1)
                |> List.map (\i -> ( ( 15 - i, 0 ), TileView.heart TileView.colorRed ))
            , player.inventory
                |> Inventory.get
                |> List.indexedMap
                    (\i a ->
                        ( ( 4 + i, 1 ), Cell.getImage (Item a) )
                    )
            ]
        )
    ]
