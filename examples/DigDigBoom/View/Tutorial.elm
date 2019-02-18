module DigDigBoom.View.Tutorial exposing (view)

import DigDigBoom.View.Tile as TileView
import PixelEngine.Graphics.Tile exposing (Tile)
import PixelEngine.Grid.Position exposing (Position)
import PixelEngine.Graphics.Options as Options exposing (Options)
import PixelEngine.Graphics as Graphics exposing (Area)
import DigDigBoom.Player as Player exposing (PlayerData)
import PixelEngine.Grid as Grid exposing (Grid)
import DigDigBoom.Cell as Cell exposing ( Cell(..)  , EnemyType(..)  , ItemType(..)    , SolidType(..))
import DigDigBoom.View.Screen as Screen
import DigDigBoom.View.Transition as Transition

viewHint : Int -> List ( Position, Tile msg )
viewHint num =
    case num of
        5 ->
            List.concat
                [ [ ( ( 2, 5 ), TileView.arrow_left TileView.colorWhite )
                  , ( ( 3, 5 ), TileView.arrow_left TileView.colorWhite )
                  , ( ( 4, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 5, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 6, 5 ) |> TileView.text "!" TileView.colorWhite
                , [ ( ( 7, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 8, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 9, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 10, 5 ) |> TileView.text "!" TileView.colorWhite
                , [ ( ( 11, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                ]

        4 ->
            List.concat
                [ [ ( ( 2, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 3, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 4, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 5, 5 ) |> TileView.text "!" TileView.colorWhite
                , [ ( ( 6, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 7, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 8, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 9, 5 ) |> TileView.text "!" TileView.colorWhite
                , [ ( ( 10, 5 ), TileView.arrow_right TileView.colorWhite ) ]
                ]

        3 ->
            List.concat
                [ [ ( ( 2, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 3, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 4, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 5, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 6, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 7, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 8, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 9, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 10, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 11, 5 ) |> TileView.text "!" TileView.colorWhite
                ]

        2 ->
            List.concat
                [ [ ( ( 2, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 3, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 4, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 5, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 6, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 7, 5 ) |> TileView.text "!" TileView.colorWhite
                ]

        _ ->
            List.concat
                [ [ ( ( 2, 5 ), TileView.arrow_down TileView.colorWhite )
                  , ( ( 3, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 4, 5 ), TileView.arrow_right TileView.colorWhite )
                  , ( ( 5, 5 ), TileView.arrow_right TileView.colorWhite )
                  ]
                , ( 6, 5 ) |> TileView.text "!" TileView.colorWhite
                ]

view : Maybe (List (Area msg)) -> PlayerData -> Grid Cell -> Int -> ( Options msg -> Options msg, List (Area msg) )
view  oldScreen player map  num =
    let
        tutorialWorldScreen =
            Screen.world num
                map
                player
                (( 2, 4 )
                    |> TileView.text "hint:" TileView.colorWhite
                    |> List.append (viewHint num)
                )
    in
    case oldScreen of
        Just justOldScreen ->
            ( Transition.nextLevel justOldScreen
            , tutorialWorldScreen
            )

        Nothing ->
            if player.lifes > 0 then
                ( identity
                , tutorialWorldScreen
                )

            else
                ( Transition.death
                    tutorialWorldScreen
                    
                , Screen.death
                )