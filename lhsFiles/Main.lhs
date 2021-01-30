> module Main where

> import Square
> import Board
> import Game

> import qualified Graphics.UI.Threepenny as UI
> import Graphics.UI.Threepenny.Core

> import Control.Monad
> import Data.IORef
> import Control.Monad.Trans (liftIO)
> import System.Random 


The main Function is responsible for drawing the game board to the browser and causing state changes of the board 
when events from the browser are triggered. I modified the Threepenny examples in the course to achieve this

> gridSize = 30
> cellSize = 30
> canvasSize = gridSize*gridSize


The dificulty is controled by setting the number of bombs
> bombs = 80

> data Modes = Reveal | Flag | Start


> main :: IO ()
> main = do

I create the original game state and pass it into the setup function

>   rand<-newStdGen
>   let game = createGame gridSize gridSize bombs rand
>   startGUI defaultConfig (setup game)

> setup :: Game -> Window -> UI ()
> setup game window = do
>   return window # set title "Minesweep"

>   canvas <- UI.canvas
>     # set UI.height canvasSize
>     # set UI.width canvasSize
>     # set UI.style [("border", "solid black 2px")]



>   mode <- liftIO $ newIORef Reveal

I lift the original game state so we can retrieve and modify game state in response to events

>   gameState <- liftIO $ newIORef game
>   pos <- liftIO $ newIORef (0,0)

Add buttons

>   startMode <- UI.button #+ [string "Start"]
>   fillMode <- UI.button #+ [string "Uncover"]
>   emptyMode <- UI.button #+ [string "Flag"]
>   move <- UI.button #+ [string "Play Move"]

>   getBody window #+
>     [column [element canvas]
>     , element startMode,element fillMode, element emptyMode, element move]

/The code for clicking the auto solver "Play Move" button

>   on UI.click move $ const $
>     do 

make the board clickable if you want to manually reveal fater a move
>       liftIO $ writeIORef mode Reveal

getting the gamestate
>       currentState <- liftIO $ readIORef gameState

This is where we call the autoslver and make the move
>       liftIO $ writeIORef gameState (makeMove currentState)

Save the resulting state
>       newState <- liftIO $ readIORef gameState

Getting the list of things to draw
>       let rects = getDrawable newState

Checking for win or loss or normal draw
>       if  _lost newState then
Function which takes a list of position ,color and text to write in a square and draws the squares 
Very similar functions used in each position where a draw is needed
>             forM_ rects $ \((x,y),color, num) -> do
>                   canvas # set' UI.fillStyle (UI.htmlColor "red")
>                   canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                   canvas # set' UI.textFont   "100px sans-serif"
>                   canvas # set' UI.fillStyle   (UI.htmlColor "black")
>                   canvas # UI.fillText  "Bombed" (250,460)
>           else
>             if  _won newState then
>               forM_ rects $ \((x,y),color, num) -> do
>                   canvas # set' UI.fillStyle (UI.htmlColor color)
>                   canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                   canvas # set' UI.textFont   "100px sans-serif"
>                   canvas # set' UI.fillStyle   (UI.htmlColor "Green")
>                   canvas # UI.fillText  "You Won" (210,460)
>             else
>               forM_ rects $ \((x,y),color, num) -> do
>                 canvas # set' UI.fillStyle (UI.htmlColor color)
>                 canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                 canvas # set' UI.textFont   "20px sans-serif"
>                 canvas # set' UI.fillStyle   (UI.htmlColor "black")
>                 canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

>   on UI.click fillMode $ \_ ->
>     do liftIO $ writeIORef mode Reveal
>   on UI.click emptyMode $ \_ ->
>       do liftIO $ writeIORef mode Flag
>   on UI.mousemove canvas $ \xy ->
>       do liftIO $ writeIORef pos xy


Click event for button start clears the canvas and draws the initial boardstate
>   on UI.click startMode $ \_ ->
>     do 
>       canvas # UI.clearCanvas
>       liftIO $ writeIORef mode Reveal
>       currentState <- liftIO $ readIORef gameState
>       liftIO $ writeIORef gameState (refreshGame currentState gridSize gridSize bombs)
>       newState <- liftIO $ readIORef gameState
>       let rects = getDrawable newState
>       forM_ rects $ \((x,y),color, num) -> do
>                 canvas # set' UI.fillStyle (UI.htmlColor color)
>                 canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                 canvas # set' UI.textFont   "20px sans-serif"
>                 canvas # set' UI.fillStyle   (UI.htmlColor "black")
>                 canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

Click event for the canvas checks if the uncover or flagging mode is selected.
It retrieves the mouse position and converts it to a grid position. Then either calls the reveal or flag function
of the game Module. 
>   on UI.click canvas $ \_  ->
>     do (x,y) <- liftIO $ readIORef pos
>        m <- liftIO $ readIORef mode
>        case m of

When revealing we check the game state to see if we have won or lost

>         Reveal -> do
>           currentState <- liftIO $ readIORef gameState

Here is the call to the reveal function ( x `div`30), ( y `div`30) is the conversion from mouse position to 
grid position
>           liftIO $ writeIORef gameState (revealclick currentState ( ( x `div`30), ( y `div`30)))
>           newState <- liftIO $ readIORef gameState
>           let rects = getDrawable newState
>           if  _lost newState then
>             forM_ rects $ \((x,y),color, num) -> do
>                   canvas # set' UI.fillStyle (UI.htmlColor "red")
>                   canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                   canvas # set' UI.textFont   "100px sans-serif"
>                   canvas # set' UI.fillStyle   (UI.htmlColor "black")
>                   canvas # UI.fillText  "Bombed" (250,460)
>           else
>             if  _won newState then
>               forM_ rects $ \((x,y),color, num) -> do
>                   canvas # set' UI.fillStyle (UI.htmlColor color)
>                   canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                   canvas # set' UI.textFont   "100px sans-serif"
>                   canvas # set' UI.fillStyle   (UI.htmlColor "Green")
>                   canvas # UI.fillText  "You Won" (210,460)
>             else
>               forM_ rects $ \((x,y),color, num) -> do
>                 canvas # set' UI.fillStyle (UI.htmlColor color)
>                 canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                 canvas # set' UI.textFont   "20px sans-serif"
>                 canvas # set' UI.fillStyle   (UI.htmlColor "black")
>                 canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)


If the game is in flag mode when we click on the canvas we draw the flag. We dont allow player flags to interfeer with revealing the
Square or with the autosolver
>         Flag -> do
>           currentState <- liftIO $ readIORef gameState
>           liftIO $ writeIORef gameState (flagclick currentState ( ( x `div`30), ( y `div`30)))
>           newState <- liftIO $ readIORef gameState
>           let rects = getDrawable newState
>           if  _lost newState || _won newState then  
>             canvas # UI.clearCanvas
>           else
>             forM_ rects $ \((x,y),color, num) -> do
>                   canvas # set' UI.fillStyle (UI.htmlColor color)
>                   canvas # UI.fillRect (fromIntegral (x*30)+2,fromIntegral (y*30)+2) 28 28
>                   canvas # set' UI.textFont   "20px sans-serif"
>                   canvas # set' UI.fillStyle   (UI.htmlColor "black")
>                   canvas # UI.fillText  num (fromIntegral (x*30)+10,fromIntegral (y*30)+22)

>                    
>                    

