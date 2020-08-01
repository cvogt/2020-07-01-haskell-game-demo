This is an amateur project exploring how convenient game development in Haskell can be.

A lot of the code is work in progress.

Presented at Haskell.Love 2020 https://haskell.love/jan-christopher-vogt/

Demo game: https://www.youtube.com/watch?v=KndOBmmuDQg

Started in June 2020 drawing inspiration from
https://www.youtube.com/watch?v=1MNTerD8IuI - "Writing a game in Haskell" by Elise Huard

Currently, this project is broken up into a shared engine named "Playtime" and
several executable demos.

Playtime takes care of interacting with openGL for rendering and GLFW-b for
capturing user inputs.

A game implementation has to provide a gameStep callback function, which knows
how to advance the game state. Playtime will call this function each time user
input events happen and once per frame. Playtime's EngineState keeps track of
things like time, pressed keys and cursor position for you.

A game implementation also has to provide a visualization callback function,
which translates from the final game state at each frame to something, which
Playtime knows how to pass to openGL. Playtime is called right before
rendering a frame and right after calling the gameStep function on a
RenderEvent

Playtime has built in support for
- Pre-loading textures from PNG files including alpha channel transparency

Playtime provides a few helpers for
- Geometry, in particular collision detection, see Geometry.hs
- saving the game state to disk and loading it, see SaveLoad.hs
- displaying custom debug information in the console while a game is running, see Debug.hs
- dynamically recompiling a game's implementation and injecting it into the running game
  for easy experimentation with a fast feedback loop, see LiveCode.hs
