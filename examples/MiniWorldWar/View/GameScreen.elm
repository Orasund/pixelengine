module MiniWorldWar.View.GameScreen exposing (Msg, update, view)

import MiniWorldWar.Data.Board as Board exposing (Unit)
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Data.Direction exposing (Direction)
import MiniWorldWar.Data.Game exposing (GameState(..))
import MiniWorldWar.Role exposing (ClientModel)
import MiniWorldWar.View as View
import MiniWorldWar.View.Image.Card as Card
import MiniWorldWar.View.SelectGui as SelectGuiView
import MiniWorldWar.View.Supplys as SupplysView
import MiniWorldWar.View.Units as UnitsView
import PixelEngine.Image as Image exposing (Image)


type Msg
    = OpenSelectGui Continent
    | AddUnit
    | RemoveUnit
    | SwapUnits
    | ResetMove
    | SetMove Direction


update : Msg -> ClientModel -> ClientModel
update msg ({ game, select } as model) =
    let
        { unitBoard, moveBoard } =
            game
    in
    case msg of
        OpenSelectGui continent ->
            case unitBoard |> Board.get continent of
                Just { amount } ->
                    { model
                        | select =
                            Just
                                ( continent
                                , { remaining = amount - 1
                                  , selected = 1
                                  }
                                )
                    }

                Nothing ->
                    model

        AddUnit ->
            case select of
                Just ( continent, { selected, remaining } ) ->
                    if remaining > 1 then
                        { model
                            | select =
                                Just
                                    ( continent
                                    , { remaining = remaining - 1
                                      , selected = selected + 1
                                      }
                                    )
                        }

                    else
                        model

                Nothing ->
                    model

        RemoveUnit ->
            case select of
                Just ( continent, { selected, remaining } ) ->
                    if selected > 1 then
                        { model
                            | select =
                                Just
                                    ( continent
                                    , { remaining = remaining + 1
                                      , selected = selected - 1
                                      }
                                    )
                        }

                    else
                        model

                Nothing ->
                    model

        SwapUnits ->
            case select of
                Just ( continent, { selected, remaining } ) ->
                    if selected > 0 then
                        { model
                            | select =
                                Just
                                    ( continent
                                    , { remaining = selected
                                      , selected = remaining
                                      }
                                    )
                        }

                    else
                        model

                Nothing ->
                    model

        ResetMove ->
            case select of
                Just ( continent, _ ) ->
                    { model
                        | game =
                            { game
                                | moveBoard =
                                    moveBoard
                                        |> Board.set continent Nothing
                            }
                        , select = Nothing
                    }

                Nothing ->
                    model

        SetMove direction ->
            case select of
                Just ( continent, { selected } ) ->
                    { model
                        | game =
                            { game
                                | moveBoard =
                                    moveBoard
                                        |> Board.set
                                            continent
                                            (Just
                                                { amount = selected
                                                , direction = direction
                                                }
                                            )
                            }
                        , select = Nothing
                    }

                Nothing ->
                    model


drawCard : Continent -> Maybe Unit -> Maybe ( ( Float, Float ), Image msg )
drawCard continent maybeUnit =
    maybeUnit
        |> Maybe.map
            (\{ color } ->
                ( View.continentToPosition continent
                , Card.card continent color
                )
            )


view : { msgWrapper : Msg -> msg, close : msg, submit : msg } -> ClientModel -> List ( ( Float, Float ), Image msg )
view { msgWrapper, close, submit } { game, select, playerColor, ready } =
    List.concat
        [ Continent.list
            |> List.filterMap
                (\continent ->
                    game.unitBoard
                        |> Board.get continent
                        |> drawCard continent
                )
        , UnitsView.view
            playerColor
            { ready = ready }
            game.state
            (msgWrapper << OpenSelectGui)
            (\continent -> game.unitBoard |> Board.get continent)
            (\continent -> game.moveBoard |> Board.get continent)
        , SupplysView.view
            game.unitBoard
            game.supplyBoard
            [ ( [ Europe, Asia, NorthAmerica ], ( View.tileSize * 1 - 4, -8 ) )
            , ( [ Africa, SouthAmerica ], ( View.tileSize * 1 - 4, View.tileSize * 3 ) )
            ]
        , case select of
            Nothing ->
                []

            Just ( continent, selectGui ) ->
                SelectGuiView.view
                    { addUnit = msgWrapper AddUnit
                    , swapUnits = msgWrapper SwapUnits
                    , removeUnit = msgWrapper RemoveUnit
                    , resetMove = msgWrapper <| ResetMove
                    , setMove = msgWrapper << SetMove
                    }
                    continent
                    selectGui
        , [ ( ( View.tileSize * 3, View.tileSize * 4 )
            , case game.state of
                Win _ ->
                    Card.exit
                        |> Image.clickable close

                Draw ->
                    Card.exit
                        |> Image.clickable close

                _ ->
                    if ready then
                        Card.watch

                    else
                        Card.submit
                            |> Image.clickable submit
            )
          ]
        ]
