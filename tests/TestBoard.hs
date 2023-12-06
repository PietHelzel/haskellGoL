module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Cell
import Board (Board(Board), update)
import BoardClass (getDefaultRules)

test2By2Square :: Test
test2By2Square = do
    let b = Board [
            Cell {x = 1, y = 1},
            Cell {x = 1, y = 2},
            Cell {x = 2, y = 1},
            Cell {x = 2, y = 2}
            ]

    let b2 = update b getDefaultRules
    TestCase (assertEqual "Board should be equal after a generation" b b2)

testCreateCell :: Test
testCreateCell = do
    let b = Board [
            Cell {x = 1, y = 1},
            Cell {x = 1, y = 2},
            Cell {x = 2, y = 1}
            ]

    let bCorrect = Board [
            Cell {x = 1, y = 1},
            Cell {x = 1, y = 2},
            Cell {x = 2, y = 1},
            Cell {x = 2, y = 2}
            ]

    let b2 = update b getDefaultRules
    TestCase (assertEqual "Cell at (2, 2) should be created after a generation" bCorrect b2)

tests :: Test
tests = TestList [
    TestLabel "2x2 constant Square" test2By2Square,
    TestLabel "Create cell" testCreateCell
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
