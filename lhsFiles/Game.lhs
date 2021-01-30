{-# LANGUAGE TemplateHaskell #-}


This is the  Game module which sits at the top level and acts as an interface for the game control

> module Game where

> import Board
> import Square

> import Control.Lens
> import System.Random 
> import Debug.Trace
traceShowId(

represent board as a list of squares. 

We store the game state as a board, a total number of bombs, if the game is won or lost and the random seed used to generate the board 
> data Game = Game
>     { _gameBoard :: Board
>       , _numBombs :: Int
>       , _rndGen :: StdGen
>       , _lost :: Bool
>       , _won :: Bool
>     }

> makeLenses ''Game

Create a blank board

Creates a game 

> createGame :: Int -> Int -> Int-> StdGen -> Game
> createGame w h bombs rand  = game
>     where
>         board = createBoard w h bombs rand
>         game = Game board bombs rand False False
>         --game = revealclick game1 (10,10)

Function to restart a game
> refreshGame :: Game -> Int -> Int -> Int -> Game
> refreshGame g w h bombs = createGame w h bombs r
>     where
>         r = _rndGen g

when sombody clicks to flag we update the gamestate
> flagclick :: Game -> (Int,Int) -> Game
> flagclick g coord = game
>     where
The old board
>         board = _gameBoard g
the new board
>         flaggedBoard = flagCellCoord board coord
The update
>         game = g & (gameBoard) .~ flaggedBoard

when sombody clicks to reveal we update the gamestate
> revealclick :: Game -> (Int,Int) -> Game
> revealclick g coord = game
>     where
The old board
>         board = _gameBoard g
>         grd = _grid board
>         pos = coordsToListPosition board coord
The Square Clicked
>         sqr = grd!!pos

Did we lose. Returns t gamestate with the lost attribute updated
>         checkLost = updateLost g sqr
The new Board
>         revealedBoard = revealNeighbours board coord
Did we win.  Returns t gamestate with the won attribute updated
>         checkwon = updateWon checkLost revealedBoard
The updated game state with new board, lose and win attributes
>         game = checkwon & (gameBoard) .~ revealedBoard


Gets the board in a drawable state
> getDrawable :: Game -> [((Int, Int), String, String)]
> getDrawable game = drawableBoard  board
>     where
>         board = _gameBoard game

check if clicking the square lost the game
> updateLost:: Game ->Square-> Game
> updateLost g sqr= game
>     where
>         game = g & (lost) .~ sqr ^. hasMine

check if clicking the square won the game
> updateWon:: Game ->Board-> Game
> updateWon g board= game
>     where
>         revealed = getNumberRevealed board
>         isWon = revealed == _numBombs g
>         game = g & (won) .~ isWon


Solver based on Double set single point Algorithm
Fnctions more appropriate to the Board modle have been 
implemented there.

Solver attempt based on the algorithm found in
https://dash.harvard.edu/bitstream/handle/1/14398552/BECERRA-SENIORTHESIS-2015.pdf?sequence=1&isAllowed=y

> makeMove :: Game -> Game
> makeMove g = game
>     where
get the game state. 
>      board = _gameBoard g
Solve to find a move
>      move =  getMove board
Make the move. 
>      game = revealclick g move




