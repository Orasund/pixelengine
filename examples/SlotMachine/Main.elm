module SlotMachine exposing (main)

import Color
import Location exposing (Location, Vector)
import PixelEngine
    exposing
        ( Area
        , Background
        , Input(..)
        , PixelEngine
        , gameWithNoControls
        )
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile, tileset)
import Random



{------------------------
   TYPES
------------------------}


{-|


# Suits

The suites can be defined with a simple union type.

-}
type Suit
    = Heart
    | Spade
    | Diamond
    | Club


{-|


# Model

For the model we purposefully do not use a list, let the compiler know exactly
what is happening at a given moment.

-}
type Model
    = None
    | One Suit
    | Two Suit Suit
    | Three Suit Suit Suit


{-|


# Actions

  - The player can `Click` on the pile. In that case we request a random card.
  - The random card will be returned and we update the Model. `(`DealCard Suit\`)
  - Once the game is done, the player can press the `reset` button.

-}
type Msg
    = Click
    | DealCard Suit
    | Reset



{------------------------
   INIT
------------------------}


init : () -> ( Model, Cmd Msg )
init _ =
    ( None, Cmd.none )



{------------------------
   UPDATE
------------------------}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( model
            , Random.generate DealCard
                (Random.uniform Heart [ Spade, Diamond, Club ])
            )

        DealCard new ->
            ( case model of
                None ->
                    One new

                One a ->
                    Two a new

                Two a b ->
                    Three a b new

                Three _ _ _ ->
                    None
            , Cmd.none
            )

        Reset ->
            init ()



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{------------------------
   VIEW
------------------------}


{-|


# Images

We want to display three different things:

  - The background (`backgroundImage`)
  - The reset button (`resetButtonImage`)
  - The cards (`cardImage`)F

-}
backgroundImage : Image Msg
backgroundImage =
    Image.fromSrc "background.png"


resetButtonImage : Image Msg
resetButtonImage =
    Image.fromSrc "reset.png"
        |> Image.clickable Reset


{-|


## Cards

The function `Tile.animated` states how many steps the animation is doing.

we have 8 sprites and there for 7 steps.

-}
shufflingTile : Tile Msg
shufflingTile =
    Tile.fromPosition ( 0, 0 )
        |> Tile.animated 8
        |> Tile.clickable Click


cardImage : Maybe Suit -> Image Msg
cardImage suit =
    case suit of
        Just Heart ->
            Image.fromSrc "heart.png"

        Just Diamond ->
            Image.fromSrc "diamond.png"

        Just Spade ->
            Image.fromSrc "spade.png"

        Just Club ->
            Image.fromSrc "club.png"

        Nothing ->
            Image.fromTile shufflingTile <|
                tileset
                    { source = "shuffle.png"
                    , spriteWidth = 32
                    , spriteHeight = 32
                    }


{-|


# Text

The `text` uses a tileset that contains the font.
Negative spacing means that the letters overlap.

-}
text : String -> Image Msg
text string =
    Image.fromTextWithSpacing -2 string <|
        tileset
            { source = "NeoSans8x8.png"
            , spriteWidth = 8
            , spriteHeight = 8
            }


{-|


# Locations

-}
firstCardLocation : Location
firstCardLocation =
    ( 0 * 32, 0 * 32 ) |> Location.add offset


secondCardLocation : Location
secondCardLocation =
    ( 1 * 32, 0 * 32 ) |> Location.add offset


thirdCardLocation : Location
thirdCardLocation =
    ( 2 * 32, 0 * 32 ) |> Location.add offset


viewCards : Model -> List ( Location, Image Msg )
viewCards model =
    case model of
        None ->
            [ ( firstCardLocation, cardImage <| Nothing ) ]

        One first ->
            [ ( firstCardLocation, cardImage <| Just first )
            , ( secondCardLocation, cardImage Nothing )
            ]

        Two first second ->
            [ ( firstCardLocation, cardImage <| Just first )
            , ( secondCardLocation, cardImage <| Just second )
            , ( thirdCardLocation, cardImage Nothing )
            ]

        Three first second third ->
            [ ( firstCardLocation, cardImage <| Just first )
            , ( secondCardLocation, cardImage <| Just second )
            , ( thirdCardLocation, cardImage <| Just third )
            ]


{-|


# Offset

We want to define an offset, that will be added to every location.

-}
offset : Vector
offset =
    { x = (128 - 112) / 2 + 8, y = (128 - 48) / 2 + 8 }


{-|


# Areas

-}
size : Float
size =
    128


areas : Model -> List (Area Msg)
areas model =
    [ PixelEngine.imageArea
        { height = size
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 222 238 214
        }
        (List.concat
            [ [ ( ( -8, -8 ) |> Location.add offset, backgroundImage )
              , ( ( 3 * 32, 8 ) |> Location.add offset, resetButtonImage )
              , ( ( 0, 35 ) |> Location.add offset, text "SLOT-MACHINE" )
              ]
            , viewCards model
            ]
        )
    ]



{------------------------
   CONFIGURATION
------------------------}


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Slot Machine"
    , options =
        Just
            (Options.default
                |> Options.withAnimationFPS 8
            )
    , body = areas model
    }


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , width = size
        , subscriptions = subscriptions
        , view = view
        }
