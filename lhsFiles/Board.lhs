{-# LANGUAGE TemplateHaskell #-}
 The Board module is where the majorty of the code for this project is located


> module Board where

> import Square

I didnt discuss in the other files but I make use of the Lens package to
have easy updates and access of the data records
> import Control.Lens
> import System.Random 
> import Debug.Trace


Represent board as a list of squares with width and heigth.
Representing the board as a single list rather than a list of list or matrix.
This should make operations on the datastrucure easier.
I just need a helper function to convert coordinates to positions and the reverse

> data Board = Board
>     { _grid :: [Square]
>     , _width :: Int
>     , _height :: Int
>     }

> makeLenses ''Board

convert a list index to an (x,y) coordinate

> listPositionToCoords :: Board -> Int -> (Int,Int)
> listPositionToCoords board pos = ((pos `div` board ^. width), (pos `rem` board ^. width))


convert a (x,y) coordinate to a list index

> coordsToListPosition :: Board -> (Int,Int) -> Int 
> coordsToListPosition board (x,y) = x*board ^. width + y

Given a position return a Square

> getSquare:: Board -> Int -> Square
> getSquare board pos = sqr
>     where
>         g = _grid board
>         sqr = g!!pos


Create a board with mines and updated the number of mines for each mine adjacent square.
The function ensures all mines are placed

> createBoard :: Int -> Int -> Int-> StdGen -> Board
> createBoard w h bombs rand = board
>     where
create the list of squares
>         squares = map createSquare [1..w*h] 
create the board        
>         board1 = Board squares w h
place the mines
>         board = insertMines bombs rand board1

Inserts mines randomly untill count goes to 0.
If a mine already exists at the generated coordiante it will try again.
For densly populated grids this will be slow. It would be better to reimplement as 
a function which adds the required number of mines and shuffles the list, Or somthine else as 
shuffeling is a rather arbitary process as to where the function has to decides to stop.

> insertMines :: Int -> StdGen -> Board -> Board
> insertMines 0 rand board = board
> insertMines count rand board 
>     | clash ==True  = insertMines (count) newRand newboard
>     | clash ==False = insertMines (count-1) newRand newboard
>     where
Get random position in board
>         (pos, newRand) = randomR (0, (board ^. height - 1)*(board ^. width - 1)) rand
>         sqr = getSquare board pos
Check if there was already a mine
>         clash = _hasMine sqr
Get all the neighbours
>         adjPos = adjacentPositions board pos
Increment the mine count of all the neighbours
>         adjBoard = updateAdjecency board adjPos
updae the board with the square filled by a mine
>         newboard = adjBoard & (grid . element pos . hasMine) .~ True


function to pass over a list of positions and update the number of mines.

> updateAdjecency :: Board -> [Int] -> Board
> updateAdjecency board [] = board
> updateAdjecency board (pos:poss) = updateAdjecency newboard poss
>   where
>     newboard = incrementAdjacent board pos

Add one to the number of mines at a position

> incrementAdjacent :: Board -> Int -> Board
> incrementAdjacent board pos = newBoard
>   where
>    sqr = getSquare board pos
>    currentValue = sqr ^. minesAdjacent
>    newBoard = board & (grid . element pos . minesAdjacent) .~ currentValue + 1

Function which game calls to signify a click to reveal a square has happened

> revealNeighbours :: Board -> (Int,Int) -> Board
> revealNeighbours board coord = 
If we have revealed everything that we should for now
>     if sqr ^. minesAdjacent > 0 || sqr ^. revealed then
>         edge
>     else
>         middle
>     where
>         pos = coordsToListPosition board coord
>         sqr = getSquare board pos
Are we at the edge of the revealed area. Frontier might have 
been a better variable name. If we are at this edge stop revealing neighbours
>         edge = revealCoord board coord
>         adj = adjacentCoordinates board coord

If not at the frontier/edge keep revealing all the neighbours
>         middle = propogateFunction revealNeighbours edge adj

Function to help revealNeighbours above. It allows us to branch through the neighbours recursively
It is a type of graph traversal function.
> propogateFunction :: (Board -> (Int,Int) -> Board) -> Board -> [(Int,Int)] -> Board
> propogateFunction _ board [] = board
> propogateFunction f board (x:xs) = f newBoard x
>   where newBoard = propogateFunction f board xs


Given a coordinate mark Square as revealed
> revealCoord :: Board -> (Int,Int) -> Board
> revealCoord board coord = revealCell board pos
>     where
>         pos = coordsToListPosition board coord


Given a position mark Square as revealed
> revealCell :: Board -> Int -> Board
> revealCell board pos = 
>     if sqr ^. hasMine then
>         board
>     else
>         newBoard
>     where
>        sqr = getSquare board pos
>        newBoard = board & (grid . element pos . revealed) .~ True


Given a coordinate mark Square as Flagged
> flagCellCoord :: Board -> (Int,Int) -> Board
> flagCellCoord board coord = flagCellPos board pos
>   where
>    pos = coordsToListPosition board coord   
  
]Given a position mark Square as revealed
> flagCellPos :: Board -> Int -> Board
> flagCellPos board pos = newBoard
>   where
>    sqr = getSquare board pos
>    newBoard = board & (grid . element pos . flagged) .~ True


Dealing with adjacency

Get adjacent squares given a position
> adjacentSquares ::  Board -> Int -> [Square]
> adjacentSquares board pos = sqrs 
>     where
>         posList = adjacentPositions board pos
>         mapF xs board   = map (\x -> getSquare board x) xs
>         sqrs = mapF posList board


Get adjacent positions given a position
> adjacentPositions ::  Board -> Int -> [Int]
> adjacentPositions board pos = posList
>   where
>     coords = listPositionToCoords board pos
>     ls = adjacentCoordinates board coords
>     mapF xs board   = map (\x -> coordsToListPosition board x) xs
>     posList = mapF ls board

Neighbourhood Mask function which also makes sure when the mask is applied no values are outside of the game board

> adjacentCoordinates :: Board -> (Int, Int) -> [(Int, Int)]
> adjacentCoordinates board (x, y) = coordList
>   where   
>     adjMatrix = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1), (x+1, y), (x+1, y-1), (x, y-1)]
>     topAndLeft = filter (\(i, j) -> i >= 0 && j >= 0) adjMatrix --- only access squares inside bounds of grid
>     coordList = filter (\(i, j) -> i <  board ^. height && j < board ^. width) topAndLeft --- only access squares inside bounds of grid


Board state to drawable objects

> drawableBoard :: Board -> [((Int, Int), String, String)]
> drawableBoard board = rects
>     where
>         g = board ^. grid
>         zl = zip [0..length g - 1] g
Helper function for mapping
>         mapF xs board   = map (\x -> drawableSquare board x) xs
convert each position to a drawable object
>         rects = mapF zl board



Function to return a drawable object given a position and square
> drawableSquare :: Board -> (Int, Square) -> ((Int, Int), String, String)
> drawableSquare board (pos, sqr) = 
>         if sqr ^. revealed  then
>             rev 
>         else
>             if sqr ^. flagged then
>                 flag
>             else
>                 if sqr ^. hasMine then
>                     rect
>                 else
>                     rect
>         where
>          adj = sqr ^. minesAdjacent 
>          str = show adj
>          coords = listPositionToCoords board pos 
>          rect = (coords,"grey","")
>          rect1 = (coords,"grey",str)
>          rev = (coords,"white", str)
>          flag = (coords,"yellow", "F")     
>          mine = (coords,"red","m")


Function to get the number of revealed squares in a board
Used to check if the game has been won or lost

> getNumberRevealed:: Board -> Int
> getNumberRevealed board = count
>  where
>     g = board ^. grid
>     revealedList = filter (\square -> _revealed square ) g
>     count =  (board ^. width* board ^. height)- (length revealedList)


Solver functions
Solver based on the double set single point Algorithm
https://dash.harvard.edu/bitstream/handle/1/14398552/BECERRA-SENIORTHESIS-2015.pdf?sequence=1&isAllowed=y

Main function of solver

This  is the function which is called by the game to get a solved move. It is my attempt at the 
single point minsweeper solver. The solverMark attribute of the Square class is used by this solver
to keep track of positions which should not be clicked

The method is based on the idea of AMN and AFN

AMN = all neighbours must be mines. The label (number of mines shown in box) is equal to the number
of unopened positions adjecent.

AFN = the number of unopened and unflagged neighbours is equal to the label.

The algorithm of a single step is at its core

1 Get frontier squares i.e squares that are revealed and next to unopened squares
2 Check if frontier squares are AMN if so mark the unopened neighbours.
3 Check if frontier squares are now AFN If you find at least one click on
one of its unopened neighbours
4 If you dont find a solution you must guess
5 good guesses are corners which are not marked first, followed by
edges not marked and finally unopend squares not next to a frontier square

> getMove :: Board -> (Int,Int)
> getMove board = move
>     where
Mark the board
>         markedB = markBoard board
get the ANF squares
>         options = getAFNFrontier markedB
Pick an unmarked square from the neighbours of one of the ANF squares or guess
>         pos =  getOption board options
Return the move to be plyed
>         move = listPositionToCoords board pos



If there is a valid ANF square use one of its valid neighbours as a move
otherwise guess
> getOption:: Board -> [Int] -> Int
> getOption board [] = guessMove
> getOption board (x:xs) = movePos
>     where
>         index = traceShowId( x  )
Get all the revealed and unmarked neighbours of the first ANF in the list
>         moves = listOfUnrevealedUnmarked board index
Take one of those valid squares
>         sqr =  head moves
Return its position
>         movePos =  traceShowId(sqr ^. pos )


I have not had time to properly implement guess. Sorry
> guessMove :: Int
> guessMove = 0

All revealed squares which have 1 or more adjecent bombs
> getFrontier :: Board -> [Square]
> getFrontier board = nonZeroRevList
>     where
>         g = board ^. grid
get the revealed squares
>         revealedList = filter (\square -> _revealed square ) g
filter out squares with zero mines adjacent
>         nonZeroRevList = filter (\square -> _minesAdjacent square > 0) revealedList

All positions where all unmarked neighbours are valid moves
> getAFNFrontier :: Board -> [Int]
> getAFNFrontier board = list
>     where
revealed and greater than zero squares
>         front = getFrontier board
positions of those squares
>         posList = map (\square -> _pos square  ) front
Filter out non AFN positions
>         list = filter (\p -> isAFN board p) posList


All positions where uncovered neighbours need to be marked
> getAMNFrontier :: Board -> [Int]
> getAMNFrontier board = list
>     where
>         front = getFrontier board
>         posList = map (\square -> _pos square ) front
>         list = filter (\p -> isAMN board p) posList
>         
Are all unmarked neighbours not bombs

> isAFN :: Board -> Int -> Bool
> isAFN board pos = result
>     where
>         sqr = getSquare board (pos )
>         label = _minesAdjacent sqr
>         marked = listOfMarked board (pos )
check if the number of marked is equal to the label
>         result = length marked == label

Are all neighbours bombs
> isAMN :: Board -> Int -> Bool
> isAMN board pos = result
>     where
>         sqr = getSquare board (pos  )
>         label = _minesAdjacent sqr
>         unmarked = listOfUnrevealed board (pos )
Is the number of unrevealed squares the same as the label
>         result = length unmarked == label


Lists the unrevealed and marked neighbours of a positionneighbours of a position
> listOfMarked :: Board -> Int -> [Square]
> listOfMarked board pos = marked
>     where
>         unrevealed =  listOfUnrevealed board pos
>         marked = filter (\square -> _solverMark square == True ) unrevealed

Lists the unrevealed an unmarked of a position

> listOfUnrevealedUnmarked :: Board -> Int -> [Square]
> listOfUnrevealedUnmarked board pos = unrevealedUnmarked
>     where
>         unrevealed = listOfUnrevealed board pos
>         unrevealedUnmarked = filter (\square -> _solverMark square == False ) unrevealed

Lists the unrevealed neighbours of a position
> listOfUnrevealed :: Board -> Int -> [Square]
> listOfUnrevealed board pos = unrevealed
>     where
>         adjSqrs = adjacentSquares board pos
>         unrevealed = filter (\square -> _revealed square == False ) adjSqrs


Marking Squares
mark the board

> markBoard :: Board -> Board
> markBoard board = newBoard
>     where
>         positions = getAMNFrontier board
>         newBoard = markPositions board positions

mark the neighbours of a list of positions
> markPositions:: Board -> [Int] -> Board
> markPositions board [] = board
> markPositions board (x:xs) = markPositions newboard xs
>     where
The Square positions are 1 indexed while the board is zero indexed
This has been causing issues and I have not been able to debut the behaviour 
>         index = x - 1
>         newboard =  markNeighbours board index

find and mark the neighbours of a position
> markNeighbours :: Board -> Int -> Board
> markNeighbours board pos = newboard
>  where
>     adjPos = adjacentPositions board pos
>     newboard = updateMarked board adjPos

mark a list of positions

> updateMarked :: Board -> [Int] -> Board
> updateMarked board [] = board
> updateMarked board (pos:poss) = updateMarked newboard poss
>   where
>     newboard = markSquare board pos

mark an individual position

> markSquare :: Board -> Int -> Board
> markSquare board pos = newBoard
>   where
>    newBoard = board & (grid . element pos . solverMark) .~ True
