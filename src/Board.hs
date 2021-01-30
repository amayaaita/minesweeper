{-# LANGUAGE TemplateHaskell #-}

module Board where

import Square

import Control.Lens
import System.Random 
import Debug.Trace
--traceShowId(

-- represent board as a list of squares. 
data Board = Board
    { _grid :: [Square]
    , _width :: Int
    , _height :: Int
    }

makeLenses ''Board

--convert a list index to an (x,y) coordinate
listPositionToCoords :: Board -> Int -> (Int,Int)
listPositionToCoords board pos = ((pos `div` board ^. width), (pos `rem` board ^. width))


--convert a (x,y) coordinate to a list index
coordsToListPosition :: Board -> (Int,Int) -> Int 
coordsToListPosition board (x,y) = x*board ^. width + y

getSquare:: Board -> Int -> Square
getSquare board pos = sqr
    where
        g = _grid board
        sqr = g!!pos


-- Create a board with mines and updated adjency
createBoard :: Int -> Int -> Int-> StdGen -> Board
createBoard w h bombs rand = board
    where
        squares = map createSquare [1..w*h]         
        board1 = Board squares w h
        board = insertMines bombs rand board1

-- Inserts mines randomly untill count goes to 0.
-- If a mine already exists at the generated coordiante it will try again.
-- For densly populated grids this will be slow. It would be better to reimplement as 
-- a function which adds the required number of mines and shuffles the list
insertMines :: Int -> StdGen -> Board -> Board
insertMines 0 rand board = board
insertMines count rand board 
    | clash ==True  = insertMines (count) newRand newboard
    | clash ==False = insertMines (count-1) newRand newboard
    where
        (pos, newRand) = randomR (0, (board ^. height - 1)*(board ^. width - 1)) rand
        sqr = getSquare board pos
        clash = _hasMine sqr
        adjPos = adjacentPositions board pos
        adjBoard = updateAdjecency board adjPos
        newboard = adjBoard & (grid . element pos . hasMine) .~ True


updateAdjecency :: Board -> [Int] -> Board
updateAdjecency board [] = board
updateAdjecency board (pos:poss) = updateAdjecency newboard poss
  where
    newboard = incrementAdjacent board pos


incrementAdjacent :: Board -> Int -> Board
incrementAdjacent board pos = newBoard
  where
   sqr = getSquare board pos
   currentValue = sqr ^. minesAdjacent
   newBoard = board & (grid . element pos . minesAdjacent) .~ currentValue + 1


revealNeighbours :: Board -> (Int,Int) -> Board
revealNeighbours board coord = 
    if sqr ^. minesAdjacent > 0 || sqr ^. revealed then
        edge
    else
        middle
    where
        pos = coordsToListPosition board coord
        sqr = getSquare board pos
        edge = revealCoord board coord
        adj = adjacentCoordinates board coord
        middle = propogateFunction revealNeighbours edge adj


propogateFunction :: (Board -> (Int,Int) -> Board) -> Board -> [(Int,Int)] -> Board
propogateFunction _ board [] = board
propogateFunction f board (x:xs) = f newBoard x
  where newBoard = propogateFunction f board xs



revealCoord :: Board -> (Int,Int) -> Board
revealCoord board coord = revealCell board pos
    where
        pos = coordsToListPosition board coord

revealCell :: Board -> Int -> Board
revealCell board pos = 
    if sqr ^. hasMine then
        board
    else
        newBoard
    where
       sqr = getSquare board pos
       newBoard = board & (grid . element pos . revealed) .~ True

flagCellCoord :: Board -> (Int,Int) -> Board
flagCellCoord board coord = flagCellPos board pos
  where
   pos = coordsToListPosition board coord     

flagCellPos :: Board -> Int -> Board
flagCellPos board pos = newBoard
  where
   sqr = getSquare board pos
   newBoard = board & (grid . element pos . flagged) .~ True


--Dealing with adjacency

adjacentSquares ::  Board -> Int -> [Square]
adjacentSquares board pos = sqrs 
    where
        posList = adjacentPositions board pos
        mapF xs board   = map (\x -> getSquare board x) xs
        sqrs = mapF posList board



adjacentPositions ::  Board -> Int -> [Int]
adjacentPositions board pos = posList
  where
    coords = listPositionToCoords board pos
    ls = adjacentCoordinates board coords
    mapF xs board   = map (\x -> coordsToListPosition board x) xs
    posList = mapF ls board

adjacentCoordinates :: Board -> (Int, Int) -> [(Int, Int)]
adjacentCoordinates board (x, y) = coordList
  where   
    adjMatrix = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1)]
    topAndLeft = filter (\(i, j) -> i >= 0 && j >= 0) adjMatrix --- only access squares inside bounds of grid
    coordList = filter (\(i, j) -> i <  board ^. height && j < board ^. width) topAndLeft --- only access squares inside bounds of grid


-- Board to rectangles
drawableBoard :: Board -> [((Int, Int), String, String)]
drawableBoard board = rects
    where
        g = board ^. grid
        zl = zip [0..length g - 1] g
        mapF xs board   = map (\x -> drawableSquare board x) xs
        rects = mapF zl board



-- Square to Cordinates and color
drawableSquare :: Board -> (Int, Square) -> ((Int, Int), String, String)
drawableSquare board (pos, sqr) = 
        if sqr ^. revealed  then
            rev 
        else
            if sqr ^. flagged then
                flag
            else
                if sqr ^. hasMine then
                    rect
                else
                    rect
        where
         adj = sqr ^. minesAdjacent 
         str = show adj
         coords = listPositionToCoords board pos 
         rect = (coords,"grey","")
         rect1 = (coords,"grey",str)
         rev = (coords,"white", str)
         flag = (coords,"yellow", "F")     
         mine = (coords,"red","m")

getNumberRevealed:: Board -> Int
getNumberRevealed board = count
 where
    g = board ^. grid
    revealedList = filter (\square -> _revealed square ) g
    count =  (board ^. width* board ^. height)- (length revealedList)


-- Solver functions
-- Solver based on the double set single point Algorithm
-- https://dash.harvard.edu/bitstream/handle/1/14398552/BECERRA-SENIORTHESIS-2015.pdf?sequence=1&isAllowed=y

-- Main function of solver
getMove :: Board -> (Int,Int)
getMove board = move
    where
        markedB = markBoard board
        options = getAFNFrontier markedB
        pos =  getOption board options
        move = traceShowId(listPositionToCoords board pos)


getOption:: Board -> [Int] -> Int
getOption board [] = guessMove
getOption board (x:xs) = movePos
    where
        index = traceShowId( x  )
        moves = listOfUnrevealedUnmarked board index
        sqr =  head moves
        movePos =  traceShowId(sqr ^. pos )


--  should change so it guesses corners then edges then internal squares
guessMove :: Int
guessMove = 0

-- All revealed squares which have 1 or more adjecent bombs
getFrontier :: Board -> [Square]
getFrontier board = nonZeroRevList
    where
        g = board ^. grid
        revealedList = filter (\square -> _revealed square ) g
        nonZeroRevList = filter (\square -> _minesAdjacent square > 0) revealedList

-- All positions where all unmarked neighbours are valid moves
getAFNFrontier :: Board -> [Int]
getAFNFrontier board = list
    where
        front = getFrontier board
        posList = map (\square -> _pos square  ) front
        list = filter (\p -> isAFN board p) posList


-- All positions where uncovered neighbours need to be marked
getAMNFrontier :: Board -> [Int]
getAMNFrontier board = list
    where
        front = getFrontier board
        posList = map (\square -> _pos square ) front
        list = filter (\p -> isAMN board p) posList
        
-- Are all unmarked neighbours not bombs
isAFN :: Board -> Int -> Bool
isAFN board pos = result
    where
        sqr = getSquare board (pos )
        label = _minesAdjacent sqr
        marked = listOfMarked board (pos )
        result = length marked == label

-- Are all neighbours bombs
isAMN :: Board -> Int -> Bool
isAMN board pos = result
    where
        sqr = getSquare board (pos  )
        label = _minesAdjacent sqr
        unmarked = listOfUnrevealed board (pos )
        result = length unmarked == label


-- Lists the unrevealed and marked neighbours of a position
listOfMarked :: Board -> Int -> [Square]
listOfMarked board pos = marked
    where
        unrevealed =  listOfUnrevealed board pos
        marked = filter (\square -> _solverMark square == True ) unrevealed

-- Lists the unrevealed  of a position
listOfUnrevealedUnmarked :: Board -> Int -> [Square]
listOfUnrevealedUnmarked board pos = unrevealedUnmarked
    where
        unrevealed = listOfUnrevealed board pos
        unrevealedUnmarked = filter (\square -> _solverMark square == False ) unrevealed

-- Lists the unrevealed  of a position
listOfUnrevealed :: Board -> Int -> [Square]
listOfUnrevealed board pos = unrevealed
    where
        adjSqrs = adjacentSquares board pos
        unrevealed = filter (\square -> _revealed square == False ) adjSqrs


-- Marking Squares
-- mark the board
markBoard :: Board -> Board
markBoard board = newBoard
    where
        positions = getAMNFrontier board
        newBoard = markPositions board positions

-- mark the neighbours of relevant positions
markPositions:: Board -> [Int] -> Board
markPositions board [] = board
markPositions board (x:xs) = markPositions newboard xs
    where
        index = x -1
        newboard =  markNeighbours board index

-- find and mark the neighbours
markNeighbours :: Board -> Int -> Board
markNeighbours board pos = newboard
 where
    adjPos = adjacentPositions board pos
    newboard = updateMarked board adjPos

-- mark a list of positions
updateMarked :: Board -> [Int] -> Board
updateMarked board [] = board
updateMarked board (pos:poss) = updateMarked newboard poss
  where
    newboard = markSquare board pos

-- mark an individual position
markSquare :: Board -> Int -> Board
markSquare board pos = newBoard
  where
   newBoard = board & (grid . element pos . solverMark) .~ True
