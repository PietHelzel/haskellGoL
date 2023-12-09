module UIRenderer where

import BoardClass

import AppState

import Brick (Widget, str, AttrMap, attrMap)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (withBorderStyle, (<+>))

import qualified Graphics.Vty as V

import UIController (ResourceName)

import BoardRenderer (renderBoard)

drawUI :: BoardClass board => AppState board -> [Widget ResourceName]
drawUI AppState {stateBoard=board} = [
        withBorderStyle unicodeRounded $ (<+>)
        (border $ str $ renderBoard (-50) (-20) 100 40 board)
        (border $ str "Hallo Welt")
    ]

getAttrMap :: BoardClass board => AppState board -> AttrMap
getAttrMap _ = attrMap V.defAttr []
