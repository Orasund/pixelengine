# pixelEngine
PixelEngine is a graphic engine for rendering turn-based pixel games.

In future this libary may become a full game engine. But this is not a main goal; More over additional features will be added if they are usefull for [PixelEngine.Graphics](https://package.elm-lang.org/packages/Orasund/pixelengine/latest/PixelEngine-Graphics).

When to use it:

  * The game is turned based. (Board Games, Rogue-likes games, Puzzle games, Turn based strategy games)
  * The game has an idle state. (Farming games, J-RPG games like Pokemon)
  * The game is tile based. (Tetris,Pack-man)

When not to use it:

  * The game is about speed or accuracy. (Racing games)
  * The game is physics based. (Flappy Birds)
  * The game has a continues gameloop. (Platformers, western RPGs like Zelda)

## Motivation
I had just watched a talk about the **time-travel debugger** of elm. So i wanted to test it out for my next project: [a little game](https://orasund.itch.io/dig-dig-boom).

The graphics engines that i could choose of where
  * [elm-2d-game](https://package.elm-lang.org/packages/Zinggi/elm-2d-game/latest)
  * [elmo-8](https://package.elm-lang.org/packages/micktwomey/elmo-8/latest)

Both used a loop to draw animations and therefore did not support the time travelling. And besides elmo-8 couldn't even render pixel graphics properly.

So i desided to try something different and to use HTML and CSS instead of WebGL. This way i could let CSS handle the animations. I knew that my little project was not a universal game engine, but for very specific games it might be just right. 
## Games made with this Engine

  * [Dig Dig Boom - Rogue-like game](https://orasund.itch.io/dig-dig-boom) ([src](https://github.com/Orasund/pixelengine/tree/master/docs/DigDigBoom)) The project that started everything. Mainly focusing on Tilesets
  * [Cult Simulator - Idle game](https://orasund.github.io/pixelengine/CultSim/) ([src](https://github.com/Orasund/pixelengine/tree/master/docs/CultSim)) A not so intended usecase that tryed to figure out the edges of this library. It only uses the MultipleImages Type

## Upgrading
  * First follow the error messages of the compiler
  * Next set the scale of your game to 1. In the new version the scale feature scales the entire game.
  * If you want keyboard support use Graphics.program. It will also take care of the new way scaling works.

## Room for Improvement
  * There is a two second input delay on phones. This is actually a improvement to the 5 second delay from before, but i believe this can be further improved. - Maybe by **switching to Progressive Web App**.
  * The animations can sometimes be very janky. This is actually intended, but it might be better to give an alternative by **switching the animation off** for a few frames(to introduce clean skips or to let elm control the animation for a short period of ime.) 

## Upcoming Features
  * MultipleTiles - Similar to MultipleImages but for Tiles.
  * Particles - Maybe using SVG?
