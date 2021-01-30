{-# LANGUAGE TemplateHaskell #-}
> module Square where

> import Control.Lens

This class is very simple. It is used to represent Squares on the grid in minesweeper
It stores the position in the game state list if the square has been opened, if it has been flagged,
if it is a mine, how many mines are adjacent and the mark which is applied by the solver 

> data Square = Square { _hasMine :: Bool
>                  , _revealed :: Bool
>                  , _flagged :: Bool
>                  , _minesAdjacent :: Int
>                  , _pos :: Int
>                  , _solverMark :: Bool
>                  }

> makeLenses ''Square


Show used for debugging
> instance Show Square where
>     show c
>         | _flagged c           = "[ F ]"
>         | _solverMark c        = "[ M ]"
>         | _minesAdjacent c > 0 = "[ * ]"
>         | otherwise            = "[   ]"

function to create the Square and store position

> createSquare :: Int -> Square
> createSquare x = Square { _hasMine = False
>                   , _revealed = False
>                   , _flagged = False
>                   , _minesAdjacent = 0
>                   , _pos = x-1
>                   , _solverMark = False
>                   }
