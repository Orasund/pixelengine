module PixelEngine.Controls exposing
    ( supportingMobile
    , Input(..), defaultLayout
    , basic, custom
    )

{-| The graphic engine provides a touch-controller for mobile devices.

## Input

@docs Input, defaultLayout

# Advanced
Normally everything is already predefined in [PixelEngine](/PixelEngine)

## Mobile Support

@docs supportingMobile

## Subscriptions

@docs basic, custom

-}

import Browser.Events as Events
import PixelEngine.Graphics exposing (Options)
import PixelEngine.Graphics.Abstract as Abstract
import Json.Decode as Decode

{-| all possible Inputs.
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


{-| Adds mobile support to the options.
It needs the window size.

[PixelEngine](/PixelEngine) provides a fully wired program that takes care of everything.

-}
supportingMobile : { windowSize : {width:Float,height:Float}, controls : Input -> msg } -> Options msg -> Options msg
supportingMobile { windowSize, controls } (Abstract.Options options) =
    let
        {width,height} = windowSize

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
    Abstract.Options { options | controllerOptions = Just { windowSize = {width= width,height= height}, controls = convert >> controls } }


{-| The default layout:

  - A/ArrowLeft - `InputLeft`
  - W/ArrowUp - `InputUp`
  - D/ArrowRight - `InputRight`
  - S/ArrowDown - `InputDown`
  - Space/Enter - `InputA`
  - X/Backspace/Esc - `InputB`
  - Q - `InputX`
  - E - `InputY`

-}
defaultLayout : String -> Input
defaultLayout =
    \string ->
        case string of
            "w" ->
                InputUp

            "W" ->
                InputUp
            
            "ArrowUp" ->
                InputUp

            "s" ->
                InputDown

            "S" ->
                InputDown
            
            "ArrowDown" ->
                InputDown

            "d" ->
                InputRight

            "D" ->
                InputRight
            
            "ArrowRight" ->
                InputRight

            "a" ->
                InputLeft

            "A" ->
                InputLeft
            
            "ArrowLeft" ->
                InputLeft
            " " ->
                InputA
            
            "Enter" ->
                InputA

            "q" ->
                InputX

            "Q" ->
                InputX

            "e" ->
                InputY

            "E" ->
                InputY

            "x" ->
                InputB

            "X" ->
                InputB
            
            "Escape" ->
                InputB
            
            "Backspace" ->
                InputB

            _ ->
                InputNone


{-| Subscribes to a keypress using custom key layouts.

It uses [key values](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) for the String.
You can use this [website](http://keycode.info) to find out the key value.
-}
custom : (String -> Input) -> (Input -> msg) -> Sub msg
custom decoder fun =
    Events.onKeyDown <| Decode.map fun <| Decode.map decoder <| Decode.field "key" Decode.string


{-| Subscribes to a keypress and sends the corresponding msg. This Function uses the default key layout.
-}
basic : (Input -> msg) -> Sub msg
basic fun =
    custom defaultLayout fun
