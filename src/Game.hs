{-# LANGUAGE TemplateHaskell #-}

module Game where

import Board
import Square

import Control.Lens
import System.Random 
import Debug.Trace
--traceShowId(

-- represent board as a list of squares. 
data Game = Game
    { _gameBoard :: Board
      , _numBombs :: Int
      , _rndGen :: StdGen
      , _lost :: Bool
      , _won :: Bool
    }

makeLenses ''Game

-- Create a blank board
createGame :: Int -> Int -> Int-> StdGen -> Game
createGame w h bombs rand  = game
    where
        board = createBoard w h bombs rand
        game = Game board bombs rand False False
        --game = revealclick game1 (10,10)

refreshGame :: Game -> Int -> Int -> Int -> Game
refreshGame g w h bombs = createGame w h bombs r
    where
        r = _rndGen g

flagclick :: Game -> (Int,Int) -> Game
flagclick g coord = game
    where
        board = _gameBoard g
        flaggedBoard = flagCellCoord board coord
        game = g & (gameBoard) .~ flaggedBoard

revealclick :: Game -> (Int,Int) -> Game
revealclick g coord = game
    where
        board = _gameBoard g
        grd = _grid board
        pos = coordsToListPosition board coord
        sqr = grd!!pos

        checkLost = updateLost g sqr
        revealedBoard = revealNeighbours board coord
        checkwon = updateWon checkLost revealedBoard

        game = checkwon & (gameBoard) .~ revealedBoard

getDrawable :: Game -> [((Int, Int), String, String)]
getDrawable game = drawableBoard  board
    where
        board = _gameBoard game

updateLost:: Game ->Square-> Game
updateLost g sqr= game
    where
        game = g & (lost) .~ sqr ^. hasMine

updateWon:: Game ->Board-> Game
updateWon g board= game
    where
        revealed = getNumberRevealed board
        isWon = revealed == _numBombs g
        game = g & (won) .~ isWon


-- Solver based on Double set single point Algorithm
-- Fnctions more appropriate to the Board modle have been 
-- implemented there

makeMove :: Game -> Game
makeMove g = game
    where
     board = _gameBoard g
     move =  getMove board--solver
     game = revealclick g move




