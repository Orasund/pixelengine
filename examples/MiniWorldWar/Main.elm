module MiniWorldWar.Main exposing (main)

import Action
import Either exposing (Either(..))
import Location exposing (Location)
import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Continent exposing (Continent(..))
import MiniWorldWar.Data.Direction exposing (Direction(..))
import MiniWorldWar.Data.Game exposing (GameState(..))
import MiniWorldWar.Request as Request exposing (Response(..))
import MiniWorldWar.Request.Client as ClientRequest exposing (Msg(..))
import MiniWorldWar.Request.Guest as GuestRequest exposing (GuestMsg(..))
import MiniWorldWar.Request.Host as HostRequest exposing (Msg(..))
import MiniWorldWar.Request.WaitingHost as WaitingHostRequest exposing (WaitingHostMsg(..))
import MiniWorldWar.Role exposing (ClientModel, HostModel, WaitingHostModel)
import MiniWorldWar.Role.Client as Client
import MiniWorldWar.Role.Guest as Guest
import MiniWorldWar.Role.Host as Host
import MiniWorldWar.Role.WaitingHost as WaitingHost
import MiniWorldWar.View as View
import MiniWorldWar.View.Error as Error
import MiniWorldWar.View.GameScreen as GameScreenView
import MiniWorldWar.View.Image.Card
import MiniWorldWar.View.Supplys as SupplysView
import MiniWorldWar.View.TitleScreen as TitleScreenView
import MiniWorldWar.View.Units
import PixelEngine exposing (Area, Background, Input(..), PixelEngine, gameWithNoControls)
import PixelEngine.Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import Time exposing (Posix)


type State
    = Client ClientModel
    | Host HostModel
    | WaitingHost WaitingHostModel
    | Guest Posix
    | FetchingTime


type Msg
    = GuestSpecific GuestMsg
    | HostSpecific HostRequest.Msg
    | WaitingHostSpecific WaitingHostMsg
    | ClientSpecific ClientRequest.Msg
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
    case ( msg, state ) of
        ( ClientSpecific clientMsg, Client model ) ->
            Client.update clientMsg model
                |> Action.config
                |> Action.withUpdate Client (responseToMsg ClientSpecific)
                |> Action.apply

        ( HostSpecific hostMsg, Host modelAndSeed ) ->
            Host.update hostMsg modelAndSeed
                |> Action.config
                |> Action.withUpdate Host (responseToMsg HostSpecific)
                |> Action.apply

        ( WaitingHostSpecific waitingHostMsg, WaitingHost timeAndSeed ) ->
            WaitingHost.update waitingHostMsg timeAndSeed
                |> Action.config
                |> Action.withUpdate WaitingHost (responseToMsg WaitingHostSpecific)
                |> Action.withTransition Host.initHost Host (responseToMsg HostSpecific)
                |> Action.apply

        ( GuestSpecific guestMsg, Guest time ) ->
            Guest.update guestMsg time
                |> Action.config
                |> Action.withUpdate Guest (responseToMsg GuestSpecific)
                |> Action.withCustomTransition
                    (\{ id, maybeSeed } ->
                        case maybeSeed of
                            Just seed ->
                                WaitingHost.init
                                    ( { time = time, id = id }, seed )
                                    |> Tuple.mapBoth
                                        WaitingHost
                                        (Cmd.map <|
                                            responseToMsg WaitingHostSpecific
                                        )

                            Nothing ->
                                Client.init { time = time, id = id }
                                    |> Tuple.mapBoth
                                        Client
                                        (Cmd.map (responseToMsg ClientSpecific))
                    )
                |> Action.apply

        ( RequestSpecific requestMsg, _ ) ->
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

        ( Tick time, _ ) ->
            case state of
                Client clientModel ->
                    time
                        |> Client.tick clientModel
                        |> Action.config
                        |> Action.withUpdate Client (responseToMsg ClientSpecific)
                        |> Action.apply

                Host hostModel ->
                    time
                        |> Host.tick hostModel
                        |> Action.config
                        |> Action.withUpdate Host (responseToMsg HostSpecific)
                        |> Action.apply

                WaitingHost waitingHostModel ->
                    time
                        |> WaitingHost.tick waitingHostModel
                        |> Action.config
                        |> Action.withUpdate WaitingHost (responseToMsg WaitingHostSpecific)
                        |> Action.apply

                Guest _ ->
                    ( Guest time, Cmd.none )

                FetchingTime ->
                    ( Guest time, Cmd.none )

        _ ->
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
   VIEW
------------------------}


size : Float
size =
    View.tileSize * 8


area : State -> List ( Location, Image Msg )
area state =
    case state of
        Client model ->
            GameScreenView.view
                { msgWrapper = ClientSpecific << ClientRequest.UISpecific
                , close = RequestSpecific Reset
                , submit = ClientSpecific Ready
                }
                model

        Host ( model, _ ) ->
            GameScreenView.view
                { msgWrapper = HostSpecific << HostRequest.UISpecific
                , close = RequestSpecific Reset
                , submit = HostSpecific Submit
                }
                model

        WaitingHost _ ->
            TitleScreenView.waiting

        Guest _ ->
            TitleScreenView.normal (GuestSpecific <| FindOpenGame)

        FetchingTime ->
            TitleScreenView.normal None


view : State -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view state =
    let
        background : Background
        background =
            PixelEngine.imageBackground
                { height = size
                , width = size
                , source = "background.png"
                }

        options : Options Msg
        options =
            Options.default
                |> Options.withAnimationFPS 4

        areas : List (Area Msg)
        areas =
            [ PixelEngine.imageArea
                { height = size
                , background = background
                }
              <|
                area state
            ]
    in
    { title = "Mini World War"
    , options =
        Just options
    , body = areas
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
        , width = size
        }
