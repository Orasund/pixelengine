module PixelEngine.Controls exposing (Input(..), basic, custom, defaultLayout, supportingMobile)

import Char
import Keyboard
import PixelEngine.Graphics as Graphics exposing (Options)
import PixelEngine.Graphics.Abstract as Abstract
import Window


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


custom : (Char -> Input) -> (Input -> msg) -> Sub msg
custom toInput fun =
    Keyboard.presses <|
        Char.fromCode
            >> toInput
            >> fun


basic : (Input -> msg) -> Sub msg
basic fun =
    Keyboard.presses <|
        Char.fromCode
            >> defaultLayout
            >> fun
