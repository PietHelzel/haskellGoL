module UIRenderer where

import BoardClass

import AppState

import Brick (Widget, str, AttrMap, attrMap)

import qualified Graphics.Vty as V

import UIController (ResourceName)

import BoardRenderer (renderBoard)

drawUI :: BoardClass board => AppState board -> [Widget ResourceName]
drawUI AppState {stateBoard=board} = [
        str $ renderBoard (-50) (-20) 100 40 board
    ]

getAttrMap :: BoardClass board => AppState board -> AttrMap
getAttrMap _ = attrMap V.defAttr []
