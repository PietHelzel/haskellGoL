module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Cell
import Board (Board(Board))
import BoardClass (getDefaultRules, getCells, update)
import Data.Set as DS (Set, fromList, map)

test2By2Square :: Test
test2By2Square = do
    let b = Board $ fromList [
            Cell {x = 1, y = 1},
            Cell {x = 1, y = 2},
            Cell {x = 2, y = 1},
            Cell {x = 2, y = 2}
            ]

    let b2 = update b getDefaultRules
    TestCase (assertEqual "Board should be equal after a generation" b b2)

testCreateCell :: Test
testCreateCell = do
    let b = Board $ fromList [
            Cell {x = 1, y = 1},
            Cell {x = 1, y = 2},
            Cell {x = 2, y = 1}
            ]

    let bCorrect = Board $ fromList [
            Cell {x = 1, y = 1},
            Cell {x = 1, y = 2},
            Cell {x = 2, y = 1},
            Cell {x = 2, y = 2}
            ]

    let b2 = update b getDefaultRules
    TestCase (assertEqual "Cell at (2, 2) should be created after a generation" bCorrect b2)

testGlider :: Test
testGlider = do
    let b = Board $ fromList [
            Cell {x = 0, y = 0},
            Cell {x = 1, y = 0},
            Cell {x = 2, y = 0},
            Cell {x = 2, y = 1},
            Cell {x = 1, y = 2}
            ]
    
    let b2 = iterate (\x -> update x getDefaultRules) b !! 4

    let bCorrect = Board $ DS.map (moveCell 1 (-1)) (getCells b)

    TestCase (assertEqual "Glider should have moved by 1 unit down-right" bCorrect b2)


tests :: Test
tests = TestList [
    TestLabel "2x2 constant Square" test2By2Square,
    TestLabel "Create cell" testCreateCell,
    TestLabel "Glider" testGlider
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
