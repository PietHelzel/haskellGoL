module UIRenderer where

import BoardClass

import AppState

import Brick (Widget, str, strWrap, AttrMap, attrMap, AttrName, attrName)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (withBorderStyle, (<+>), (<=>), reportExtent, Padding(Max), padBottom, padRight, hLimitPercent, vBox, hBox, withAttr, clickable)
import Data.List.Split (splitOn)
import Brick.Util (fg)

import qualified Graphics.Vty as V

import UIController (ResourceName(..))

import BoardRenderer (renderBoard)

drawUI :: BoardClass board => AppState board -> [Widget ResourceName]
drawUI state =
    [
        withBorderStyle unicodeRounded $ (<+>)
        (drawGameViewport state)
        (hLimitPercent 15 $ (<=>) (drawHelperWindow state) (drawStatsWindow state))
    ]

drawGameViewport :: BoardClass board => AppState board -> Widget ResourceName
drawGameViewport AppState {stateBoard=board, stateX=x, stateY=y, stateWidth=width, stateHeight=height} = do
    let rBoard = renderBoard x y width height board
    let lines = enumerate $ splitOn "\n" rBoard
    reportExtent GameViewport $ clickable GameViewport $ border $ padRight Max $ padBottom Max $ vBox [
            if y' /= height `div` 2 then
                str line
            else
                hBox [
                    if x' == width `div` 2 && y' == height `div` 2 then
                        withAttr cursorAttr $ str [if c == '█' then '█' else '░']
                    else
                        str [c]
                    | (x', c) <- enumerate line
                    ]
            | (y', line) <- lines
        ]

enumerate :: [a] -> [(Integer, a)]
enumerate xs = enumerateHelper 0 xs

enumerateHelper :: Integer -> [a] -> [(Integer, a)]
enumerateHelper _ [] = []
enumerateHelper n (x:xs) = (n, x) : enumerateHelper (n + 1) xs

drawHelperWindow :: BoardClass board => AppState board -> Widget ResourceName
drawHelperWindow _ = border $ strWrap "\
\Help:\n\
\- Use the arrow keys to move around the viewport.\n\
\- Press q to quit.\n\
\- Press space to pause / unpause\n\
\- Press t to toggle a cell\n\
\- Press +/- to increase/decrease simulation speed"

drawStatsWindow :: BoardClass board => AppState board -> Widget ResourceName
drawStatsWindow state = do
    let speed = 10 - stateTicksBetweenUpdates state
    border $ strWrap ("Information:\n- Speed: " ++ (show speed))


cursorAttr :: AttrName
cursorAttr = attrName "cursorAttr"

getAttrMap :: BoardClass board => AppState board -> AttrMap
getAttrMap _ = attrMap V.defAttr [
        (cursorAttr, fg V.red)
    ]
