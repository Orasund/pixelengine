module MiniWorldWar.Main exposing (main)

import MiniWorldWar.Board as Board exposing (Unit)
import MiniWorldWar.Card as Card
import MiniWorldWar.Color as Color exposing (Color(..))
import MiniWorldWar.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Game as Game exposing (Game)
import MiniWorldWar.Gui as Gui exposing (SelectGui)
import MiniWorldWar.Server.Client as ClientServer exposing (ClientResponse(..))
import MiniWorldWar.Server.Guest as GuestServer exposing (GuestResponse(..))
import MiniWorldWar.Server.Host as HostServer exposing (HostResponse(..))
import MiniWorldWar.Server.WaitingHost as WaitingHostServer exposing (WaitingHostResponse(..))
import MiniWorldWar.Unit as Unit
import PixelEngine exposing (PixelEngine, game)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Image as Image exposing (Image, image)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix)


type alias Model =
    { game : Game
    , time : Posix
    , playerColor : Color
    , select : Maybe ( Continent, SelectGui )
    , id : String
    }


type State
    = Client Model
    | Host ( Model, Seed )
    | WaitingHost ( { time : Posix, id : String }, Seed )
    | Guest Posix
    | FetchingTime


type GuestEvent
    = HostGame Seed


type BoardEvent
    = OpenSelectGui Continent
    | AddUnit
    | RemoveUnit
    | SwapUnits


type SpecificMsg response event
    = Response response
    | Event event


type alias GuestMsg =
    SpecificMsg GuestResponse GuestEvent


type Msg
    = GuestSpecific GuestMsg
    | HostSpecific HostResponse
    | WaitingHostSpecific WaitingHostResponse
    | ClientSpecific ClientResponse
    | BoardSpecific BoardEvent
    | Reset Posix
    | Update Posix
    | None



{------------------------
   INIT
------------------------}


init : () -> ( State, Cmd Msg )
init _ =
    ( FetchingTime
    , Cmd.none
    )



{------------------------
   UPDATE
------------------------}


guestUpdate : GuestMsg -> Posix -> ( State, Cmd Msg )
guestUpdate msg time =
    let
        state : State
        state =
            Guest time

        defaultCase : ( State, Cmd Msg )
        defaultCase =
            ( state, Cmd.none )

        game : Game
        game =
            time |> Time.posixToMillis |> Game.empty
    in
    case msg of
        Response response ->
            case response of
                JoinGame id ->
                    ( Client
                        { game = game
                        , time = time
                        , id = id
                        , select = Nothing
                        , playerColor = Blue
                        }
                    , ClientServer.joinGame id game
                        |> Cmd.map ClientSpecific
                    )

                DropOpenGameTable ->
                    ( state
                    , GuestServer.dropOpenGameTable
                        |> Cmd.map (GuestSpecific << Response)
                    )

                DropRunningGameTable ->
                    ( state
                    , GuestServer.dropRunningGameTable
                        |> Cmd.map (GuestSpecific << Response)
                    )

                GameNotFound ->
                    defaultCase

                JoinOpenGame id ->
                    ( state
                    , GuestServer.joinOpenGame id
                        |> Cmd.map (GuestSpecific << Response)
                    )

                CloseGame id ->
                    ( state
                    , GuestServer.closeGame id
                        |> Cmd.map (GuestSpecific << Response)
                    )

                ReopenGame id ->
                    ( state
                    , GuestServer.reopenGame id
                        |> Cmd.map (GuestSpecific << Response)
                    )

                FindOldGame ->
                    ( state
                    , GuestServer.findOldGame time
                        |> Cmd.map (GuestSpecific << Response)
                    )

                CreateNewGame ->
                    ( state
                    , Random.generate
                        (GuestSpecific << Event << HostGame)
                        Random.independentSeed
                    )

                FindOpenGame ->
                    ( state
                    , GuestServer.findOpenGame time
                        |> Cmd.map (GuestSpecific << Response)
                    )

        Event event ->
            case event of
                HostGame seed ->
                    let
                        ( id, newSeed ) =
                            Random.step
                                (Random.int Random.minInt Random.maxInt
                                    |> Random.map String.fromInt
                                )
                                seed
                    in
                    ( WaitingHost
                        ( { time = time
                          , id = id
                          }
                        , newSeed
                        )
                    , WaitingHostServer.hostGame id time
                        |> Cmd.map WaitingHostSpecific
                    )


hostUpdate : HostResponse -> ( Model, Seed ) -> ( State, Cmd Msg )
hostUpdate msg (( { game, id, time } as model, seed ) as modelAndSeed) =
    let
        state : State
        state =
            Host modelAndSeed

        defaultCase : ( State, Cmd Msg )
        defaultCase =
            ( state, Cmd.none )
    in
    case msg of
        NewRound ->
            let
                ( newGame, newSeed ) =
                    seed
                        |> Random.step (game  |> Game.newRound time )
            in
            ( Host ( { model | game = newGame }, newSeed )
            , HostServer.newRound id newGame
                |> Cmd.map HostSpecific
            )

        IdleAsHost ->
            defaultCase

        EndGame ->
            ( state
            , HostServer.endGame id time
                |> Cmd.map HostSpecific
            )

        UpdateGameTable table ->
            ( state
            , HostServer.updateGameTable table
                |> Cmd.map HostSpecific
            )

        DropGameTable ->
            ( state
            , HostServer.dropGameTable
                |> Cmd.map HostSpecific
            )

        StopHosting ->
            init ()


waitingHostUpdate : WaitingHostResponse -> ( { time : Posix, id : String }, Seed ) -> ( State, Cmd Msg )
waitingHostUpdate msg (( { time, id }, seed ) as timeAndSeed) =
    let
        state : State
        state =
            WaitingHost timeAndSeed

        defaultCase : ( State, Cmd Msg )
        defaultCase =
            ( state, Cmd.none )
    in
    case msg of
        StopWaitingForOpponent ->
            ( state
            , WaitingHostServer.stopWaitingForOpponent id
                |> Cmd.map WaitingHostSpecific
            )

        WaitForOpponent ->
            ( state
            , WaitingHostServer.checkForOpponent id
                |> Cmd.map WaitingHostSpecific
            )

        FailedToHost ->
            init ()

        CreateBoard ({ board } as game) ->
            let
                ( newGame, newSeed ) =
                    seed
                        |> Random.step
                            ((Game.new time) |> Random.andThen (Game.newRound time)
                            )
            in
            ( Host
                ( { time = time
                  , id = id
                  , game = newGame
                  , select = Nothing
                  , playerColor = Red
                  }
                , newSeed
                )
            , HostServer.newRound id newGame
                |> Cmd.map HostSpecific
            )


clientUpdate : ClientResponse -> Model -> ( State, Cmd Msg )
clientUpdate msg ({time,id,game} as model) =
    let
        state : State
        state =
            Client model

        defaultCase : ( State, Cmd Msg )
        defaultCase =
            ( state, Cmd.none )
    in
    case msg of
        WaitingForHost ->
            ( state
            , ClientServer.waitingForHost id game.lastUpdated 
                |> Cmd.map ClientSpecific
            )

        UpdateGame newGame ->
            defaultCase --TODO
        
        UpdateAsClient newTime ->
            defaultCase --TODO
        
        ExitGame ->
            ( state
            , ClientServer.exitGame id
                |> Cmd.map ClientSpecific
            )
        
        DropRunningGameTableAsClient ->
            ( state
            , ClientServer.dropRunningGameTableAsClient id
                |> Cmd.map ClientSpecific
            )
        
        IdleAsClient ->
            defaultCase

        ResetAsClient ->
            init ()


updateBoard : BoardEvent -> Model -> Model
updateBoard msg ({ game, select } as model) =
    let
        { board } =
            game
    in
    case msg of
        OpenSelectGui continent ->
            case board |> Board.get continent of
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
                    if selected > 1 then
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


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        defaultCase : ( State, Cmd Msg )
        defaultCase =
            ( state, Cmd.none )

        resetCase : Posix -> ( State, Cmd Msg )
        resetCase time =
            ( Guest time, Cmd.none )
    in
    case state of
        Client model ->
            case msg of
                ClientSpecific clientMsg ->
                    clientUpdate clientMsg model

                BoardSpecific boardMsg ->
                    ( Client <| updateBoard boardMsg model, Cmd.none )

                _ ->
                    defaultCase

        Host (( model, seed ) as modelAndSeed) ->
            case msg of
                HostSpecific hostMsg ->
                    hostUpdate hostMsg modelAndSeed

                BoardSpecific boardMsg ->
                    ( Host <| ( updateBoard boardMsg model, seed ), Cmd.none )

                _ ->
                    defaultCase

        WaitingHost timeAndSeed ->
            case msg of
                WaitingHostSpecific waitingHostMsg ->
                    waitingHostUpdate waitingHostMsg timeAndSeed

                _ ->
                    defaultCase

        Guest time ->
            case msg of
                GuestSpecific guestMsg ->
                    guestUpdate guestMsg time

                _ ->
                    defaultCase

        FetchingTime ->
            case msg of
                Reset time ->
                    resetCase time

                _ ->
                    defaultCase



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : State -> Sub Msg
subscriptions state =
    let
        updateRate : Float
        updateRate = (5*1000)
    in
    case state of
        FetchingTime -> 
            Time.every updateRate Reset
        Guest _ ->
            Sub.none
        WaitingHost _ ->
            Time.every updateRate (WaitingHostSpecific << UpdateAsWaitingHost)
        Host _ ->
            Time.every updateRate (HostSpecific << UpdateAsHost)
        Client _ ->
            Time.every updateRate (ClientSpecific << UpdateAsClient)



{------------------------
   CONTROLS
------------------------}


controls : Input -> Msg
controls _ =
    None



{------------------------
   VIEW
------------------------}


tileSize : Float
tileSize =
    16


logoImage : ( ( Float, Float ), Image Msg )
logoImage =
    ( ( 0, tileSize * 1 ), image "logo.png" )


waitingScreen : List ( ( Float, Float ), Image Msg )
waitingScreen =
    [ logoImage
    , ( ( tileSize * 2, tileSize * 5 ), image "findingOpponent.png" )
    ]


titleScreen : List ( ( Float, Float ), Image Msg )
titleScreen =
    [ logoImage
    , ( ( tileSize * 2, tileSize * 5 )
      , image "newGame.png"
            |> Image.withAttributes
                [ Image.onClick (GuestSpecific <| Response FindOpenGame) ]
      )
    ]


view : State -> { title : String, options : Options Msg, body : List (Area Msg) }
view state =
    let
        size : Float
        size =
            tileSize * 8

        background : Background
        background =
            Graphics.imageBackground
                { height = size
                , width = size
                , source = "background.png"
                }

        drawUnit : ( Float, Float ) -> Unit -> ( ( Float, Float ), Image Msg )
        drawUnit pos unit =
            ( pos
            , Unit.unitImage unit { used = True }
            )

        drawUnitCenter : Continent -> { used : Bool } -> Unit -> ( ( Float, Float ), Image Msg )
        drawUnitCenter continent ({ used } as config) unit =
            let
                ( x, y ) =
                    continent |> continentToPosition
            in
            if used then
                unit |> drawUnit ( x + tileSize / 2, y + tileSize * 1 )

            else
                ( ( x + tileSize / 2, y + tileSize * 1 )
                , Unit.unitImage unit config
                    |> Image.withAttributes
                        [ Image.onClick (BoardSpecific <| OpenSelectGui continent) ]
                )

        drawUnitLeft : Continent -> Unit -> ( ( Float, Float ), Image Msg )
        drawUnitLeft continent =
            let
                ( x, y ) =
                    continent |> continentToPosition
            in
            drawUnit ( x - tileSize / 2, y + tileSize / 2 )

        drawUnitRight : Continent -> Unit -> ( ( Float, Float ), Image Msg )
        drawUnitRight continent =
            let
                ( x, y ) =
                    continent |> continentToPosition
            in
            drawUnit ( x + (3 * tileSize) / 2, y + (3 * tileSize) / 2 )

        drawUnitUp : Continent -> Unit -> ( ( Float, Float ), Image Msg )
        drawUnitUp continent =
            let
                ( x, y ) =
                    continent |> continentToPosition
            in
            drawUnit ( x, y - tileSize / 2 )

        drawUnitDown : Continent -> Unit -> ( ( Float, Float ), Image Msg )
        drawUnitDown continent =
            let
                ( x, y ) =
                    continent |> continentToPosition
            in
            drawUnit ( x + tileSize * 1, y + (5 * tileSize) / 2 )

        drawCard : Continent -> Maybe Unit -> Maybe ( ( Float, Float ), Image msg )
        drawCard continent maybeUnit =
            maybeUnit
                |> Maybe.map
                    (\{ color } ->
                        ( continentToPosition continent
                        , Card.card continent color
                        )
                    )

        continentToPosition : Continent -> ( Float, Float )
        continentToPosition continent =
            case continent of
                Europe ->
                    ( tileSize * 1, tileSize * 1 )

                NorthAmerica ->
                    ( tileSize * 5, tileSize * 1 )

                SouthAmerica ->
                    ( tileSize * 5, tileSize * 4 )

                Asia ->
                    ( tileSize * 3, tileSize * 1 )

                Africa ->
                    ( tileSize * 1, tileSize * 4 )

        drawModel : Model -> List ( ( Float, Float ), Image Msg )
        drawModel { game, select, playerColor } =
            let
                { europe, asia, africa, northAmerica, southAmerica } =
                    game.board
            in
            List.concat
                [ Continent.list
                    |> List.map
                        (\continent ->
                            game.board
                                |> Board.get continent
                                |> drawCard continent
                        )
                    |> List.filterMap identity
                , Continent.list
                    |> List.map
                        (\continent ->
                            game.board
                                |> Board.get continent
                                |> Maybe.map
                                    (\({ amount, color } as unit) ->
                                        unit
                                            |> drawUnitCenter
                                                continent
                                                { used = amount <= 1 || playerColor /= color }
                                    )
                        )
                    |> List.filterMap identity
                , case select of
                    Nothing ->
                        []

                    Just ( continent, { selected, remaining } as selectGui ) ->
                        let
                            ( x, y ) =
                                continent |> continentToPosition
                        in
                        List.concat
                            [ [ ( ( x, y )
                                , Gui.selectGui selectGui
                                )
                              ]
                            , case remaining of
                                1 ->
                                    []

                                _ ->
                                    [ ( ( x + 8 * 3, y + 3 + 8 * 0 )
                                      , Gui.addUnitButton
                                            |> Image.withAttributes
                                                [ Image.onClick <|
                                                    BoardSpecific AddUnit
                                                ]
                                      )
                                    ]
                            , 
                                    [ ( ( x + 8 * 3, y + 3 + 8 * 1 )
                                      , Gui.swapUnitsButton
                                            |> Image.withAttributes
                                                [ Image.onClick <|
                                                    BoardSpecific SwapUnits
                                                ]
                                      )
                                    ]
                            , case selected of
                                1 ->
                                    []

                                _ ->
                                    [ ( ( x + 8 * 3, y + 3 + 8 * 2 )
                                      , Gui.removeUnitButton
                                            |> Image.withAttributes
                                                [ Image.onClick <|
                                                    BoardSpecific RemoveUnit
                                                ]
                                      )
                                    ]
                            ]
                ]

        body : List (Area Msg)
        body =
            [ Graphics.imageArea
                { height = size
                , background = background
                }
                (case state of
                    Client model ->
                        model |> drawModel

                    Host ( model, _ ) ->
                        model |> drawModel

                    WaitingHost _ ->
                        waitingScreen

                    Guest _ ->
                        titleScreen

                    FetchingTime ->
                        titleScreen
                )
            ]
    in
    { title = "Mini World War"
    , options = Graphics.options { width = size, transitionSpeedInSec = 0.025 }
    , body = body
    }



{------------------------
   MAIN
------------------------}


main : PixelEngine () State Msg
main =
    game
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = controls
        }
