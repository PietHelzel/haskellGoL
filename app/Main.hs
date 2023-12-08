module Main where

import Brick (customMain)

import UI (app, createTickChannel)
import AppState (AppState(..))

import Board (Board(..))

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import Data.Set as DS (fromList)

main :: IO (AppState Board)
main = do
    let builder = mkVty defaultConfig
    initialVty <- builder
    chan <- createTickChannel
    customMain
        initialVty
        builder
        (Just chan)
        app
        AppState {statePaused=False, stateBoard=Board $ fromList []}
