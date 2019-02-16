# PixelEngine
PixelEngine is a graphic engine for rendering turn-based pixel games.

![Games made with pixelEngine](https://orasund.github.io/pixelengine/img1.png "Games made with pixelEngine")

Games can either be stand-alone documents or [imbedded in a website](https://orasund.github.io/pixelengine/)
## Tutorials
* [Tic Tac Toe](https://github.com/Orasund/pixelengine/wiki/Tutorial:-Tic-Tac-Toe) 
Beginner Tutorial, uses clickable Tiles.
* [Snake](https://github.com/Orasund/pixelengine/wiki/Tutorial:-Snake)
Beginner Tutorial, uses keyboard controls.

## Games made with this Engine

  * [Mini World War - Card Game](https://orasund.github.io/pixelengine/MiniWorldWar/)([src](https://github.com/Orasund/pixelengine/tree/master/examples/MiniWorldWar)) A Risc-inspired fast card game. Its a 2-Player online game written purely in Elm.
  *(Use the Mouse to play)*
  * [Dig Dig Boom - Rogue-like Game](https://orasund.itch.io/dig-dig-boom) ([src](https://github.com/Orasund/pixelengine/tree/master/examples/DigDigBoom)) The project that started everything. Mainly focusing on Tilesets
  * [Cult Simulator - Idle Game](https://orasund.github.io/pixelengine/CultSim/) ([src](https://github.com/Orasund/pixelengine/tree/master/examples/CultSim)) A not so intended usecase that tryed to figure out the edges of this library. It only uses the `MultipleImages` Type. *(Use the Mouse to play)*
  * [Ruin Jump - Platformer Game](https://orasund.github.io/pixelengine/RuinJump/) ([src](https://github.com/Orasund/pixelengine/tree/master/examples/RuinJump)) I wanted to have a plaformer with a jumping mechanic. I also intentionally made it a bit janky. *(Use the Arrow keys as well as SPACE to play)*

If you have created a game with this engine, please let me know.

## Should I use this Package?

This package follows a few philosophies:
  * **Let CSS handle all animations** - I feel like functional programming should describe *reactions* to some events. Animations are normally running all the time and not a reaction. Addionally by getting rid of an infinite game loop, we can use the **time-travel debugger**.
  * **Pixel games first** - There are a few optimizations to ensure crips pixels as well as good old low-frame animations.
  * **Demand driven features** - There is no roadmap. I intent to use this engine for all my games. If I feel like I can't make a game, because my engine is missing a feature, I will add it. (Please feel free to ask if you are missing a feature.)

When to use it:

  * The game is turned based. (Board Games, Rogue-likes games, Puzzle games, Turn based strategy games)
  * The game has an idle state. (Farming games, J-RPG games like Pokemon)
  * The game is tile based. (Tetris,Pack-man)

When not to use it:

  * The game is about speed or accuracy. (Racing games)
  * The game is physics based. (Flappy Birds)
  * The game has a continues gameloop. (Platformers, western RPGs like Zelda)

## Motivation
I had just watched a talk about the **time-travel debugger** of Elm. So I wanted to test it out for my next project: [A little game](https://orasund.itch.io/dig-dig-boom).

The graphics engines that I could choose from where
  * [elm-2d-game](https://package.elm-lang.org/packages/Zinggi/elm-2d-game/latest)
  * [elmo-8](https://package.elm-lang.org/packages/micktwomey/elmo-8/latest) (currently outdated)

Both used a loop to draw animations and therefore did not support time travelling and besides elmo-8 couldn't even render pixel graphics properly.

So I desided to try something different and to use HTML and CSS instead of WebGL. This way I could let CSS handle the animations. I knew that my little project was not a universal game engine, but for very specific games it might be just right. 

## Upcoming Features
  * **Console** - It would be great if all games written with this engine would have a common way how the interface looks like. I am mainly thinking of a [PICO-8-like](https://www.lexaloffle.com/pico-8.php) style, but i would like to provide a few different GUIs and Startscreen.
  * **Controls Rewrite** - I made the mistake of offering a `InputNone` instead of using something like `Maybe Input`. This is totally unnessery.
  * **MultipleTiles** - Similar to `MultipleImages` but for Tiles.
  * **Modular Transitions** - For now I do not expect this will ever be done, just because its a very complex problem.
  * **Particles** - Maybe using [BrianHicks/elm-particle](https://package.elm-lang.org/packages/BrianHicks/elm-particle/latest)?

## Upgrading
  * **To 4.0.0**
    * `program` and `programWithCustomControls` are now renamed to `game` and `gameWithCustomControls`.
    * This package now uses [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest) for colors.
    * For styling use `Html.Attributes.style`.
    * `renderToScale` replaced `usingScale`, but normally `Graphics.program` is the better option.
  * **To 3.0.0**
    * First follow the error messages of the compiler
    * Next set the `scale` of your game to `1`. In the new version the scale feature scales the entire game.
    * If you want keyboard support use `Graphics.program`. It will also take care of the new way scaling works.