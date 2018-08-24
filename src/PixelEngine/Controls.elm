module PixelEngine.Controls exposing (Input(..), basic, custom, defaultLayout, supportingMobile)

{-| The graphic engine provides a touch-controller for mobile devices. The controllers has 8 buttons:
Left (a key), Right (d key), Up (w key), Down (s key), A(Spacebar), B(x key), X(q key), Y(e key)


## Main Function

@docs supportingMobile


## Input

@docs Input,defaultLayout


## Subscriptions

@docs basic,custom

-}

import Char
import Keyboard
import PixelEngine.Graphics as Graphics exposing (Options)
import PixelEngine.Graphics.Abstract as Abstract
import Window


{-| all possible Inputs
-}
type Input
    = InputLeft
    | InputRight
    | InputUp
    | InputDown
    | InputA
    | InputB
    | InputX
    | InputY
    | InputNone


{-| adds mobile support to the options.
It needs the window size.

[PixelEngine](https://package.elm-lang.org/packages/Orasund/pixelengine/latest/PixelEngine) provides a fully wired document that takes care of everything.

-}
supportingMobile : { windowSize : Window.Size, controls : Input -> msg } -> Options msg -> Options msg
supportingMobile { windowSize, controls } (Abstract.Options options) =
    let
        convert : Abstract.AbstractInput -> Input
        convert input =
            case input of
                Abstract.AbstractInputA ->
                    InputA

                Abstract.AbstractInputB ->
                    InputB

                Abstract.AbstractInputX ->
                    InputX

                Abstract.AbstractInputY ->
                    InputY

                Abstract.AbstractInputUp ->
                    InputUp

                Abstract.AbstractInputLeft ->
                    InputLeft

                Abstract.AbstractInputRight ->
                    InputRight

                Abstract.AbstractInputDown ->
                    InputDown

                Abstract.AbstractInputNone ->
                    InputNone
    in
    Abstract.Options { options | controllerOptions = Just { windowSize = windowSize, controls = convert >> controls } }


{-| the default layout:

  - A - InputLeft
  - W - InputUp
  - D - InputRight
  - S - InputDown
  - ENTER - InputA
  - X - InputB
  - Q - InputX
  - E - InputY

-}
defaultLayout : Char -> Input
defaultLayout =
    \char ->
        case char of
            'w' ->
                InputUp

            'W' ->
                InputUp

            's' ->
                InputDown

            'S' ->
                InputDown

            'd' ->
                InputRight

            'D' ->
                InputRight

            'a' ->
                InputLeft

            'A' ->
                InputLeft

            ' ' ->
                InputA

            'q' ->
                InputX

            'Q' ->
                InputX

            'e' ->
                InputY

            'E' ->
                InputY

            'x' ->
                InputB

            'X' ->
                InputB

            _ ->
                InputNone


{-| subscribes to a keypress using custom key layouts
-}
custom : (Char -> Input) -> (Input -> msg) -> Sub msg
custom toInput fun =
    Keyboard.presses <|
        Char.fromCode
            >> toInput
            >> fun


{-| subscribes to a keypress and sends the corresponding msg. This Function uses the default key layout.
-}
basic : (Input -> msg) -> Sub msg
basic fun =
    Keyboard.presses <|
        Char.fromCode
            >> defaultLayout
            >> fun
