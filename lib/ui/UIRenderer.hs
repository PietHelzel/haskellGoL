module UIRenderer where

import BoardClass

import AppState

import Brick (Widget, str, AttrMap, attrMap)

import qualified Graphics.Vty as V

import UIController (ResourceName)

drawUI :: BoardClass board => AppState board -> [Widget ResourceName]
drawUI AppState {statePaused=statePaused} = [
        str $ show statePaused
    ]

getAttrMap :: BoardClass board => AppState board -> AttrMap
getAttrMap _ = attrMap V.defAttr []
