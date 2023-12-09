module UIRenderer where

import BoardClass

import AppState

import Brick (Widget, str, AttrMap, attrMap)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (withBorderStyle, (<+>), reportExtent, Padding(Max), padBottom, padRight)


import qualified Graphics.Vty as V

import UIController (ResourceName(..))

import BoardRenderer (renderBoard)

drawUI :: BoardClass board => AppState board -> [Widget ResourceName]
drawUI state =
    [
        withBorderStyle unicodeRounded $ (<+>)
        (padRight Max $ padBottom Max $ drawGameViewport state)
        (border $ str "Hallo Welt")
    ]

drawGameViewport :: BoardClass board => AppState board -> Widget ResourceName
drawGameViewport AppState {stateBoard=board, stateX=x, stateY=y, stateWidth=width, stateHeight=height} =
    reportExtent GameViewport (border $ str $ renderBoard x y width height board)

getAttrMap :: BoardClass board => AppState board -> AttrMap
getAttrMap _ = attrMap V.defAttr []
