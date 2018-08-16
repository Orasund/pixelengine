# pixelEngine
A graphic engine for rendering turn-based pixel games.

When to use it:

  * The game is turned based. (Board Games, Rogue-likes games, Puzzle games, Turn based strategy games)
  * The game has an idle state. (Farming games, Jump 'n' Run games, J-RPG games like Pokemon)
  * The game is tile based. (Tetris,Pack-man)

When not to use it:

  * The game is about speed or accuracy. (Racing games)
  * The game is physics based. (Flappy Birds)
  * The game has a continues gameloop. (Platformers, western RPGs like Zelda)

**Games made with this Engine**

  * [Dig Dig Boom - Rogue-like game](https://orasund.itch.io/dig-dig-boom) ([src](https://github.com/Orasund/elm-playground/tree/master/docs/DigDigBoom)) The project that started everything. Mainly focusing on Tilesets
  * [Cult Simulator - Idle game](https://orasund.github.io/pixelengine/CultSim/) ([src](https://github.com/Orasund/elm-playground/tree/master/docs/CultSim)) A not so intended usecase that tryed to figure out the edges of this library. It only uses MultipleImages
