# ElmLike

ElmLike is a GUI architecture framework modeled after The Elm Architecture
with the goal of being portable to many render targets.

The core of ElmLike is written in C++ and the runtime is written in Haskell.

> My use cases

I want to be able to build testable and performant interfaces for media
players/editors and games.

> Order of operations

- [x] Create a the window that uses skia renderer.
- [x] Define the lifecycle of the state in Haskell.
- [ ] Create an event queue that will inform the haskell runtime
      of the happenings of the application -- i.e. ProgramExit,
      ButtonClick, WindowMove, etc.
- [ ] Create a set of widgets and have the view function describe
      how these widgets are composed and to be used.
