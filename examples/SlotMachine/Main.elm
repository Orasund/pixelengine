module SlotMachine exposing (main)

import Array exposing (Array)
import Color
import PixelEngine exposing (PixelEngine, gameWithNoControls)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Image as Image exposing (Image, image)
import PixelEngine.Graphics.Options as Options exposing (Options)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset, tile, tileset)
import Random
import Time



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

  - The player can `Click` on the Pile. In that case we request a random card.
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

{-| # Images
We want to display three different things:
* The background (`backgroundImage`)
* The reset button (`resetButtonImage`)
* The cards (`cardImage`)
-}
backgroundImage : Image Msg
backgroundImage =
    image "background.png"

resetButtonImage : Image Msg
resetButtonImage =
    image "reset.png"
        |> Image.clickable Reset

{-| ## Cards
The function "animated" states how many steps the animation is doing.
8 Sprites -> 7 Steps.
-}
shufflingTile : Tile Msg
shufflingTile =
    tile ( 0, 0 )
        |> Tile.animated 7
        |> Tile.clickable Click


cardImage : Maybe Suit -> Image Msg
cardImage suit =
    case suit of
        Just Heart ->
            image "heart.png"

        Just Diamond ->
            image "diamond.png"

        Just Spade ->
            image "spade.png"

        Just Club ->
            image "club.png"

        Nothing ->
            Image.fromTile shufflingTile <|
                tileset
                    { source = "shuffle.png"
                    , spriteWidth = 32
                    , spriteHeight = 32
                    }

{-| # Text
The `text` uses a tileset that contains the font.
Negative spacing means that the letters overlap. This is only possible because
we are not using tiles.
-}
text : String -> Image Msg
text string =
    Image.fromTextWithSpacing -2 string <|
        tileset
            { source = "NeoSans8x8.png"
            , spriteWidth = 8
            , spriteHeight = 8
            }

{-| # Locations
-}
firstCardLocation : ( Float, Float )
firstCardLocation =
    ( 0 * 32, 0 * 32 ) |> addOffset offset


secondCardLocation : ( Float, Float )
secondCardLocation =
    ( 1 * 32, 0 * 32 ) |> addOffset offset


thirdCardLocation : ( Float, Float )
thirdCardLocation =
    ( 2 * 32, 0 * 32 ) |> addOffset offset


viewCards : Model -> List ( ( Float, Float ), Image Msg )
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


{-| #Offset

We want to define an offset, that means a value that will be added to every
location.
-}
offset : { left : Float, top : Float }
offset =
    { left = 50, top = 84 }

addOffset : { left : Float, top : Float } -> ( Float, Float ) -> ( Float, Float )
addOffset l ( left, top ) =
    ( l.left + left, l.top + top )






{-| # Viewing the Model
-}
view : Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view model =
    let
        size : Float
        size =
            200

        background : Background
        background =
            Graphics.colorBackground <|
                Color.rgb255 222 238 214
    in
    { title = "Slot Machine"
    , options =
        Options.fromWidth size
            |> Options.withAnimationFPS 8
    , body =
        [ Graphics.imageArea
            { height = size
            , background = background
            }
            (List.concat
                [ [ ( ( -8, -8 ) |> addOffset offset, backgroundImage )
                  , ( ( 3 * 32, 8 ) |> addOffset offset, resetButtonImage )
                  , ( ( 0, 35 ) |> addOffset offset, text "SLOT-MACHINE" )
                  ]
                , viewCards model
                ]
            )
        ]
    }



{------------------------
   MAIN
------------------------}


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
