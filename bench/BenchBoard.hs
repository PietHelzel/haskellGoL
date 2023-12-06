module Main where

import Test.Tasty.Bench

import Cell
import Board (Board(Board))
import BoardClass (getDefaultRules, getCells, update)

-- iterate cannot be used for some reason here, it messes up benchmark times
applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

benchFPentomino :: Int -> [Cell]
benchFPentomino n = do
    let b = Board [
            Cell {x = 0, y = 0},
            Cell {x = 0, y = 1},
            Cell {x = (-1), y = 1},
            Cell {x = 0, y = 2},
            Cell {x = 1, y = 2}
            ]

    let b2 = applyNTimes n (\x -> update x getDefaultRules) b

    getCells b2


main :: IO ()
main = defaultMain [
        bgroup "f-Pentomino" [
            bench "100" $ whnf benchFPentomino 100,
            bench "500" $ whnf benchFPentomino 500
        ]
    ]
