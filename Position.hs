module Position (Position(points, boards, moves, toText, 
                          fromData, fromRaw, evalBoard, 
                          toGame, present, presents), 
                 Player(Left, Right), 
                 Piece, left, rght, cell, wall,
                 makeTree, -- TODO: to remove?
                 makeGame,
                 opposite, 
                 getDir,
                 replace,
                 findCoord,
                 clearBoard,
                 readBoard,
                 help, commands
                ) where

import Prelude hiding (Left,Right)
import Data.List

import Scoring
import Help

------------------------------------

data Player = Left | Right
  deriving Eq

class Ord a => Position a where

  ----------------------------------------------
  -- abstract function, need implementation per game
  ----------------------------------------------

  points :: a -> NumberData
  boards :: a -> [String]
  moves  :: a -> Player -> [a]
  toText :: a -> String
  fromData :: Position a => NumberData -> [String] -> a
  
  ----------------------------------------------
  -- the remaining have standard implementations
  ----------------------------------------------
  
  toGame :: a -> Game
  toGame = canonize . makeGame . makeTree  -- it doesn't depend on specific rules!

  present :: a -> IO ()
  present position = 
      do
         putStr $ toText position
         putStrLn " points"  

  presents :: [a] -> IO ()
  presents [] = return ()
  presents (position:positions) =
      do
         present position
         presents positions
         
  evalBoard :: Position a => FilePath -> IO a
  evalBoard filePath = 
    do
      (n,rows) <- readBoard filePath
      let game = fromData n rows
      print $ toGame game
      return game

  fromRaw :: Position a => String -> a
  fromRaw description = fromData (read (head text)::NumberData) (tail text)
    where
      text = lines description
      
------------------------------------
  
data PositionTree a = Leaf NumberData
                    | Node { leftPos  :: [PositionTree a], 
                             value    :: NumberData,
                             rightPos :: [PositionTree a] }
    deriving (Show, Eq, Ord)

-- given a position create a tree with all possible moves
-- the "length lPos == 1 || length rPos == 1" option is for games like diskonnect
-- where exists an extra step to compute bonuses after the endgame. 
-- In these cases the board becomes empty
makeTree :: (Ord a, Position a) => a -> PositionTree a
makeTree pos
   | lPos == [] && rPos == [] = Leaf $ points pos -- endgame
   | lPos == [] = Node [] (points pos) [makeTree rp | rp <- rPos] -- left  does not have moves (no bonus)
   | rPos == [] = Node [makeTree lp | lp <- lPos] (points pos) [] -- right does not have moves (no bonus)
   | length lPos == 1 || length rPos == 1 
                              = case (emptyBoard (lPos!!0), emptyBoard (rPos!!0)) of
                                  (True,True)   -> Leaf $ points (lPos!!0)
                                  (True,False)  -> Node [] (points (lPos!!0)) [makeTree rp | rp <- rPos]
                                  (False,True)  -> Node [makeTree lp | lp <- lPos] (points (rPos!!0)) []
                                  (False,False) -> Node [makeTree lp | lp <- lPos] 
                                                        (points pos)
                                                        [makeTree rp | rp <- rPos]
   | otherwise                = Node [makeTree lp | lp <- lPos] 
                                     (points pos)
                                     [makeTree rp | rp <- rPos]
  where
     lPos = sort $ moves pos Left
     rPos = sort $ moves pos Right
     emptyBoard game = and (map (==cell) ((concat.boards) game))

-- translate the previous tree into Scoring Game format:
makeGame :: Position a => PositionTree a -> Game
makeGame (Leaf n)           = Nu n
makeGame (Node [] n rPos)   = LE             n            [makeGame p | p <- rPos]
makeGame (Node lPos n [])   = RE [makeGame p | p <- lPos]             n
makeGame (Node lPos n rPos) = Op [makeGame p | p <- lPos] [makeGame p | p <- rPos]

------------------------------------

type Piece = Char

left :: Piece
left = 'l'

rght :: Piece
rght = 'r'

cell :: Piece
cell = '.'

wall :: Piece
wall = 'x'

opposite :: Piece -> Piece
opposite piece
  | piece == left = rght
  | piece == rght = left
  | otherwise     = error "Piece does not have opposite"

-- given a direction, the position relative to a given one
-- pre: assume that it's a valid position (ie, non negative outputs)
getDir :: (Int,Int) -> Char -> (Int,Int)
getDir (i,j) dir
  |  dir == 'n' = (i-1,j)
  |  dir == 's' = (i+1,j)
  |  dir == 'w' = (i,j-1)
  |  dir == 'e' = (i,j+1)

------------------------------------
-- replace a char at position (i,j) with a new char
replace :: [String] -> (Int,Int) -> Char -> [String]
replace position (i,j) newchar = rep position (i,j) newchar 0
  where
    rep (row:rows) (i,j) newchar k
      | i == k    = (take j row ++ [newchar] ++ drop (j+1) row) : rows
      | otherwise = row : rep rows (i,j) newchar (k+1)

------------------------------------
-- find the coordinates where a given piece type are
findCoord :: Piece -> [String] -> [(Int,Int)]
findCoord piece rows = find piece rows 0
  where
      find   _       []     _ = []
      find piece (row:rows) i = [ (i,j) | j <- indexes piece row 0 ] ++ find piece rows (i+1)
      indexes   _     []   _ = []
      indexes piece (c:cs) j
         | piece == c = j : indexes piece cs (j+1)
         | otherwise  =     indexes piece cs (j+1)
    
------------------------------------
-- clear the board
clearBoard :: [String] -> [String]
clearBoard board = replicate (length board) row
  where
    row = replicate (length (board!!0)) cell
    
------------------------------------
-- Read the raw data from the file presenting some useful information
readBoard :: FilePath -> IO (NumberData, [String])
readBoard filePath =
  do
    contents <- readFile filePath
    putStr "-- Read: "
    print $ filter (/=' ') contents
    putStrLn "-- Board"
    putStrLn contents
    putStrLn "-- Position Value:"
    let text = lines $ filter (/=' ') contents
    return (read (head text)::NumberData, tail text)
