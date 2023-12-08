module UI where

import Brick (App(..), neverShowCursor)
import Brick.BChan (BChan)

import AppState

import UIController
import UIRenderer

import BoardClass

import Brick.BChan (writeBChan, newBChan)

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.Functor (void)

app :: BoardClass board => App (AppState board) CustomEvent ResourceName
app = App {
        appDraw = UIRenderer.drawUI,
        appChooseCursor = neverShowCursor,
        appHandleEvent = UIController.handleEvent,
        appStartEvent = return (),
        appAttrMap = getAttrMap
    }

createTickChannel :: IO (BChan CustomEvent)
createTickChannel = do
    chan <- newBChan 10
    let delay = 100000
    void . forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    return chan
