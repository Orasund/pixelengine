module Index exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Nav exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Http
import Index.Example as Example
import Index.ExampleWithCode as ExampleWithCode
import Index.Home as Home
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))
import Framework.Typography as Typography

examplesWithCode =
    [ "Animations", "TicTacToe", "Snake" ]


examplesWithoutCode =
    [ "DigDigBoom", "MiniWorldWar", "RuinJump", "CultSim" ]


type alias ExampleModel =
    { src : String, code : String }


type Page
    = Home
    | ExampleWithCode ExampleModel
    | Example String


type alias Model =
    { key : Key
    , page : Page
    }


type Msg
    = ViewExampleWithCode ExampleModel
    | ViewExample String
    | GetCode String
    | ClickedLink UrlRequest
    | Back


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    update (url |> onUrlChange) { key = key, page = Home }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( { model | page = Home }, Cmd.none )
    in
    case msg of
        ViewExampleWithCode example ->
            ( { model | page = ExampleWithCode example }, Cmd.none )

        ViewExample string ->
            ( { model | page = Example string }, Cmd.none )

        GetCode name ->
            ( { model | page = Example name }
            , Http.get
                { url = "examples/"++name++"/Main.elm"
                , expect =
                    Http.expectString
                        (\result ->
                            case result of
                                Ok code ->
                                    ViewExampleWithCode { src = name, code = code }

                                Err error ->
                                    error
                                        |> Debug.log "Error:"
                                        |> always (ViewExample name)
                        )
                }
            )

        Back ->
            defaultCase

        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )


view : Model -> Document Msg
view ({ page } as model) =
    { title = "PixelEngine"
    , body =
        [ Element.layout
            [ Background.color <| Element.rgb255 222 238 214 ]
          <|
            case page of
                Example name ->
                    Example.view name

                ExampleWithCode example ->
                    ExampleWithCode.view example

                Home ->
                    Home.view { examples = examplesWithCode, games = examplesWithoutCode }
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


onUrlChange : Url -> Msg
onUrlChange url =
    let
        maybeName : Maybe String
        maybeName =
            url.fragment
    in
    case maybeName of
        Just name ->
            if examplesWithCode |> List.member name then
                GetCode name
                {-ViewExample name-}

            else if examplesWithoutCode |> List.member name then
                ViewExample name

            else
                Back

        Nothing ->
            Back


main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = onUrlChange
        }
