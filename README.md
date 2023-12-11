# haskellGoL: An implementation of Conway's Game of Life in Haskell

## Introduction

This project implements Conway's Game of Life as a TUI application using the [Brick](https://hackage.haskell.org/package/brick) library.
It was written for the "Weitere Programmiersprache" course.

## Running

When running the application without any arguments, you will be presented with an empty board. There are instructions in the user interface on how to
use the program. You can also start the program with the `--help` flag to see further available options, those being:

- `--board <file>`: Import a file as the starting board
- `--rules-file`: Use a ruleset read from a file
- `--rules`: Use a ruleset directly from the command line

Board files are written with on line representing a living cell. Each line is written in the format `x,y`, where x and y specifiy the coordinates of that cell.
Rules are written in the format `x/y` with x being the amount of neighbours a living cell needs to have to survive, and y specifying the amount
of neighbours a dead cell needs to have to be bord. All other cases cause the cell to die. The default ruleset would be written as `23/3`:
With 2 or 3 neighbours a cell survives, with exactly 3 it gets born.

## Building

The build system used is cabal. To build the program from source, use `cabal build` in the project root.
The compiled executable will be created in the folder `dist-newstyle/build/<platform>/<ghc-version>/haskellGoL-<version>/x/haskellGoL`.

### Building tests and benchmarks

To build and execute tests, use `cabal test` in the project root. Use `cabal bench` to execute benchmarks.
The benchmarks only measure performance of the game of life simulation, not the rendering.

### Terminals

Since terminals sometimes behave differently, this application might not work perfectly on all of them. It has been tested on:

- alacritty, foot (Linux)
- Windows Terminal (Windows)

## Source outline

- app/: Contains the main file for the executable.
- lib/gol/: A library containing the core game of life rules and logic.
- lib/ui/: A library containing the terminal user interface.
- tests/: Unit tests
- bench/: Benchmarks