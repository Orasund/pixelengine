module MiniWorldWar.Main exposing (main)

import MiniWorldWar.Data.Board as Board exposing (Unit)
import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Data.Direction exposing (Direction(..))
import MiniWorldWar.Data.Game exposing (GameState(..))
import MiniWorldWar.Request as Request exposing (Response(..))
import MiniWorldWar.Request.Client as ClientRequest exposing (ClientMsg(..))
import MiniWorldWar.Request.Guest as GuestRequest exposing (GuestMsg(..))
import MiniWorldWar.Request.Host as HostRequest exposing (HostMsg(..))
import MiniWorldWar.Request.WaitingHost as WaitingHostRequest exposing ( WaitingHostMsg(..))
import MiniWorldWar.Role exposing (ClientModel, HostModel, WaitingHostModel)
import MiniWorldWar.Role.Client as Client
import MiniWorldWar.Role.Guest as Guest
import MiniWorldWar.Role.Host as Host
import MiniWorldWar.Role.WaitingHost as WaitingHost
import MiniWorldWar.View as View
import MiniWorldWar.View.Error as Error
import MiniWorldWar.View.Image.Card as Card
import MiniWorldWar.View.SelectGui as SelectGuiView
import MiniWorldWar.View.Supplys as SupplysView
import MiniWorldWar.View.TitleScreen as TitleScreenView
import MiniWorldWar.View.Units as UnitsView
import PixelEngine exposing (PixelEngine, gameWithNoControls)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Image as Image exposing (Image)
import Time exposing (Posix)


type State
    = Client ClientModel
    | Host HostModel
    | WaitingHost WaitingHostModel
    | Guest Posix
    | FetchingTime


type BoardEvent
    = OpenSelectGui Continent
    | AddUnit
    | RemoveUnit
    | SwapUnits
    | ResetMove
    | SetMove Direction


type Msg
    = GuestSpecific GuestMsg
    | HostSpecific HostMsg
    | WaitingHostSpecific WaitingHostMsg
    | ClientSpecific ClientMsg
    | BoardSpecific BoardEvent
    | RequestSpecific (Response Never)
    | Tick Posix
    | None


responseToMsg : (specificMsg -> Msg) -> Response specificMsg -> Msg
responseToMsg fun response =
    case response of
        Please msg ->
            fun msg

        Exit ->
            RequestSpecific Exit

        Reset ->
            RequestSpecific Reset

        Idle ->
            RequestSpecific Idle

        ExitWithError error ->
            RequestSpecific <| ExitWithError error

        ResetWithError error ->
            RequestSpecific <| ResetWithError error

        DropOpenGameTable error ->
            RequestSpecific <| DropOpenGameTable error

        DropRunningGameTable error ->
            RequestSpecific <| DropRunningGameTable error



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


updateBoard : BoardEvent -> ClientModel -> ClientModel
updateBoard msg ({ game, select } as model) =
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


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        defaultCase : ( State, Cmd Msg )
        defaultCase =
            ( state, Cmd.none )

        resetCase : ( State, Cmd Msg )
        resetCase =
            init ()

        exitCase : ( State, Cmd Msg )
        exitCase =
            ( state
            , (case state of
                Client { id } ->
                    ClientRequest.exit id

                Host ( { id }, _ ) ->
                    HostRequest.exit id

                WaitingHost ( { id }, _ ) ->
                    WaitingHostRequest.exit id

                Guest _ ->
                    GuestRequest.exit

                FetchingTime ->
                    GuestRequest.exit
              )
                |> Cmd.map RequestSpecific
            )
    in
    case msg of
        ClientSpecific clientMsg ->
            case state of
                Client model ->
                    Client.update clientMsg model (responseToMsg ClientSpecific)
                        |> Tuple.mapFirst Client

                _ ->
                    defaultCase

        HostSpecific hostMsg ->
            case state of
                Host modelAndSeed ->
                    Host.update hostMsg modelAndSeed (responseToMsg HostSpecific)
                        |> Tuple.mapFirst Host

                _ ->
                    defaultCase

        WaitingHostSpecific waitingHostMsg ->
            case state of
                WaitingHost timeAndSeed ->
                    WaitingHost.update
                        waitingHostMsg
                        timeAndSeed
                        WaitingHost
                        Host
                        (responseToMsg WaitingHostSpecific)
                        (responseToMsg HostSpecific)

                _ ->
                    defaultCase

        GuestSpecific guestMsg ->
            case state of
                Guest time ->
                    Guest.update
                        guestMsg
                        time
                        Guest
                        WaitingHost
                        Client
                        (responseToMsg GuestSpecific)
                        (responseToMsg WaitingHostSpecific)
                        (responseToMsg ClientSpecific)

                _ ->
                    defaultCase

        BoardSpecific boardMsg ->
            case state of
                Client ({ ready } as model) ->
                    if ready then
                        defaultCase

                    else
                        ( Client <| updateBoard boardMsg model, Cmd.none )

                Host ( { ready } as model, seed ) ->
                    if ready then
                        defaultCase

                    else
                        ( Host <| ( updateBoard boardMsg model, seed ), Cmd.none )

                _ ->
                    defaultCase

        RequestSpecific requestMsg ->
            case requestMsg of
                DropOpenGameTable error ->
                    ( state |> Error.log error
                    , Request.dropOpenGameTable |> Cmd.map RequestSpecific
                    )

                DropRunningGameTable error ->
                    ( state |> Error.log error
                    , Request.dropRunningGameTable |> Cmd.map RequestSpecific
                    )

                Exit ->
                    exitCase

                ExitWithError error ->
                    exitCase |> Error.log error

                Reset ->
                    resetCase

                ResetWithError error ->
                    resetCase |> Error.log error

                Please _ ->
                    defaultCase

                Idle ->
                    defaultCase

        Tick time ->
            case state of
                Client clientModel ->
                    time
                        |> Client.tick clientModel (responseToMsg ClientSpecific)
                        |> Tuple.mapFirst Client

                Host hostModel ->
                    time
                        |> Host.tick hostModel (responseToMsg HostSpecific)
                        |> Tuple.mapFirst Host

                WaitingHost waitingHostModel ->
                    time
                        |> WaitingHost.tick waitingHostModel (responseToMsg WaitingHostSpecific)
                        |> Tuple.mapFirst WaitingHost

                Guest _ ->
                    ( Guest time, Cmd.none )

                FetchingTime ->
                    ( Guest time, Cmd.none )

        None ->
            defaultCase



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : State -> Sub Msg
subscriptions state =
    let
        updateSub : Sub Msg
        updateSub =
            Time.every (1 * 1000) Tick
    in
    case state of
        FetchingTime ->
            updateSub

        Guest _ ->
            Sub.none

        WaitingHost _ ->
            updateSub

        Host _ ->
            updateSub

        Client _ ->
            updateSub



{------------------------
   CONTROLS
------------------------}


controls : Input -> Msg
controls _ =
    None



{------------------------
   VIEW
------------------------}


drawCard : Continent -> Maybe Unit -> Maybe ( ( Float, Float ), Image msg )
drawCard continent maybeUnit =
    maybeUnit
        |> Maybe.map
            (\{ color } ->
                ( View.continentToPosition continent
                , Card.card continent color
                )
            )


drawModel : Msg -> ClientModel -> List ( ( Float, Float ), Image Msg )
drawModel submitMsg { game, select, playerColor, ready } =
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
            (BoardSpecific << OpenSelectGui)
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
                    { addUnit = BoardSpecific AddUnit
                    , swapUnits = BoardSpecific SwapUnits
                    , removeUnit = BoardSpecific RemoveUnit
                    , resetMove = BoardSpecific <| ResetMove
                    , setMove = BoardSpecific << SetMove
                    }
                    continent
                    selectGui
        , [ ( ( View.tileSize * 3, View.tileSize * 4 )
            , case game.state of
                Win _ ->
                    Card.exit
                        |> Image.withAttributes [ Image.onClick <| RequestSpecific Reset ]

                Draw ->
                    Card.exit
                        |> Image.withAttributes [ Image.onClick <| RequestSpecific Reset ]

                _ ->
                    if ready then
                        Card.watch

                    else
                        Card.submit
                            |> Image.withAttributes [ Image.onClick submitMsg ]
            )
          ]
        ]


view : State -> { title : String, options : Options Msg, body : List (Area Msg) }
view state =
    let
        size : Float
        size =
            View.tileSize * 8

        background : Background
        background =
            Graphics.imageBackground
                { height = size
                , width = size
                , source = "background.png"
                }

        body : List (Area Msg)
        body =
            [ Graphics.imageArea
                { height = size
                , background = background
                }
                (case state of
                    Client model ->
                        model |> drawModel (ClientSpecific Ready)

                    Host ( model, _ ) ->
                        model |> drawModel (HostSpecific Submit)

                    WaitingHost _ ->
                        TitleScreenView.waiting

                    Guest _ ->
                        TitleScreenView.normal (GuestSpecific <| FindOpenGame)

                    FetchingTime ->
                        TitleScreenView.normal None
                )
            ]
    in
    { title = "Mini World War"
    , options = Graphics.options { width = size, transitionSpeedInSec = 0.012 }
    , body = body
    }



{------------------------
   MAIN
------------------------}


main : PixelEngine () State Msg
main =
    gameWithNoControls
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
