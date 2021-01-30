{-# LANGUAGE TemplateHaskell #-}
module Square where

import Control.Lens

data Square = Square { _hasMine :: Bool
                 , _revealed :: Bool
                 , _flagged :: Bool
                 , _minesAdjacent :: Int
                 , _pos :: Int
                 , _solverMark :: Bool
                 }

makeLenses ''Square

instance Show Square where
    show c
        | _flagged c           = "[ F ]"
        | _solverMark c        = "[ M ]"
        | _minesAdjacent c > 0 = "[ * ]"
        | otherwise            = "[   ]"

createSquare :: Int -> Square
createSquare x = Square { _hasMine = False
                  , _revealed = False
                  , _flagged = False
                  , _minesAdjacent = 0
                  , _pos = x-1
                  , _solverMark = False
                  }
