module UIRenderer where

import BoardClass

import AppState

import Brick (Widget, str, strWrap, AttrMap, attrMap)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (withBorderStyle, (<+>), reportExtent, Padding(Max), padBottom, padRight, hLimitPercent)


import qualified Graphics.Vty as V

import UIController (ResourceName(..))

import BoardRenderer (renderBoard)

drawUI :: BoardClass board => AppState board -> [Widget ResourceName]
drawUI state =
    [
        withBorderStyle unicodeRounded $ (<+>)
        (drawGameViewport state)
        (drawHelperWindow state)
    ]

drawGameViewport :: BoardClass board => AppState board -> Widget ResourceName
drawGameViewport AppState {stateBoard=board, stateX=x, stateY=y, stateWidth=width, stateHeight=height} =
    reportExtent GameViewport $ border $ padRight Max $ padBottom Max $ str $ renderBoard x y width height board

drawHelperWindow :: BoardClass board => AppState board -> Widget ResourceName
drawHelperWindow _ = border $ hLimitPercent 15 $ strWrap "Help:\n- Use the arrow keys to move around the viewport.\n- Press q to quit."

getAttrMap :: BoardClass board => AppState board -> AttrMap
getAttrMap _ = attrMap V.defAttr []
