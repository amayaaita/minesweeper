module Main where

import Square
import Board
import Game

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad
import Data.IORef
import Control.Monad.Trans (liftIO)
import System.Random 

gridSize = 30
cellSize = 30
canvasSize = gridSize*gridSize
bombs = 80

data Modes = Reveal | Flag | Start


main :: IO ()
main = do
  rand<-newStdGen
  let game = createGame gridSize gridSize bombs rand
  startGUI defaultConfig (setup game)

setup :: Game -> Window -> UI ()
setup game window = do
  return window # set title "Minesweep"

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set UI.style [("border", "solid black 2px")]

  mode <- liftIO $ newIORef Reveal
  gameState <- liftIO $ newIORef game
  pos <- liftIO $ newIORef (0,0)

  startMode <- UI.button #+ [string "Start"]
  fillMode <- UI.button #+ [string "Uncover"]
  emptyMode <- UI.button #+ [string "Flag"]
  move <- UI.button #+ [string "Play Move"]

  getBody window #+
    [column [element canvas]
    , element startMode,element fillMode, element emptyMode, element move]

  on UI.click move $ const $
    do 
      liftIO $ writeIORef mode Reveal
      currentState <- liftIO $ readIORef gameState
      liftIO $ writeIORef gameState (makeMove currentState)
      newState <- liftIO $ readIORef gameState
      let rects = getDrawable newState
      if  _lost newState then
            forM_ rects $ \((x,y),color, num) -> do
                  canvas # set' UI.fillStyle (UI.htmlColor "red")
                  canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                  canvas # set' UI.textFont   "100px sans-serif"
                  canvas # set' UI.fillStyle   (UI.htmlColor "black")
                  canvas # UI.fillText  "Bombed" (250,460)
          else
            if  _won newState then
              forM_ rects $ \((x,y),color, num) -> do
                  canvas # set' UI.fillStyle (UI.htmlColor color)
                  canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                  canvas # set' UI.textFont   "100px sans-serif"
                  canvas # set' UI.fillStyle   (UI.htmlColor "Green")
                  canvas # UI.fillText  "You Won" (210,460)
            else
              forM_ rects $ \((x,y),color, num) -> do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                canvas # set' UI.textFont   "20px sans-serif"
                canvas # set' UI.fillStyle   (UI.htmlColor "black")
                canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

  on UI.click fillMode $ \_ ->
    do liftIO $ writeIORef mode Reveal
  on UI.click emptyMode $ \_ ->
      do liftIO $ writeIORef mode Flag
  on UI.mousemove canvas $ \xy ->
      do liftIO $ writeIORef pos xy

  on UI.click startMode $ \_ ->
    do 
      canvas # UI.clearCanvas
      liftIO $ writeIORef mode Reveal
      currentState <- liftIO $ readIORef gameState
      liftIO $ writeIORef gameState (refreshGame currentState gridSize gridSize bombs)
      newState <- liftIO $ readIORef gameState
      let rects = getDrawable newState
      forM_ rects $ \((x,y),color, num) -> do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                canvas # set' UI.textFont   "20px sans-serif"
                canvas # set' UI.fillStyle   (UI.htmlColor "black")
                canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

  on UI.click canvas $ \_  ->
    do (x,y) <- liftIO $ readIORef pos
       m <- liftIO $ readIORef mode
       case m of

        Reveal -> do
          currentState <- liftIO $ readIORef gameState
          liftIO $ writeIORef gameState (revealclick currentState ( ( x `div`30), ( y `div`30)))
          newState <- liftIO $ readIORef gameState
          let rects = getDrawable newState
          if  _lost newState then
            forM_ rects $ \((x,y),color, num) -> do
                  canvas # set' UI.fillStyle (UI.htmlColor "red")
                  canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                  canvas # set' UI.textFont   "100px sans-serif"
                  canvas # set' UI.fillStyle   (UI.htmlColor "black")
                  canvas # UI.fillText  "Bombed" (250,460)
          else
            if  _won newState then
              forM_ rects $ \((x,y),color, num) -> do
                  canvas # set' UI.fillStyle (UI.htmlColor color)
                  canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                  canvas # set' UI.textFont   "100px sans-serif"
                  canvas # set' UI.fillStyle   (UI.htmlColor "Green")
                  canvas # UI.fillText  "You Won" (210,460)
            else
              forM_ rects $ \((x,y),color, num) -> do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                canvas # set' UI.textFont   "20px sans-serif"
                canvas # set' UI.fillStyle   (UI.htmlColor "black")
                canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

        Flag -> do
          currentState <- liftIO $ readIORef gameState
          liftIO $ writeIORef gameState (flagclick currentState ( ( x `div`30), ( y `div`30)))
          newState <- liftIO $ readIORef gameState
          let rects = getDrawable newState
          if  _lost newState || _won newState then  
            canvas # UI.clearCanvas
          else
            forM_ rects $ \((x,y),color, num) -> do
                  canvas # set' UI.fillStyle (UI.htmlColor color)
                  canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
                  canvas # set' UI.textFont   "20px sans-serif"
                  canvas # set' UI.fillStyle   (UI.htmlColor "black")
                  canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

                   
                   

