module Kobber(Kobber(P), evalKobber) where
                
import Prelude hiding (Left,Right)
import qualified Prelude hiding (Left,Right)

import Position
import Scoring

------------------------------------
-- Kobber game

-- Pieces move orthogonally. 
-- Can capture by replacement an adjacent enemy stone (earning a point)
-- or capture by jumping over (not earning a point)

-- A Position is the amount of points already gained (positive for Left, negative for Right)
-- and a matrix of cells defining the current position
------------------------------------

data Kobber = P { pts :: NumberData, board :: [String] }
  deriving (Eq, Ord, Show) -- TODO: show only needed for testing, consider removing

instance Position Kobber where
  points   = pointsKobber
  boards   = boardKobber
  moves    = movesKobber
  toText   = showKobber
  fromData = fromKobberData
  
------------------------------------

pointsKobber :: Kobber -> NumberData
pointsKobber (P n _) = n

boardKobber :: Kobber -> [String]
boardKobber (P _ b) = b

------------------------------------

showKobber :: Kobber -> String
showKobber (P n ls) = unlines ls ++ showNu n

------------------------------------

fromKobberData :: NumberData -> [String] -> Kobber
fromKobberData n rows = P n rows

------------------------------------
-- which positions are possible given a position?

movesKobber :: Kobber -> Player -> [Kobber]
movesKobber position Left  = mv position left
movesKobber position Right = mv position rght

------------------------------------
-- the next functions are needed to define movesKobber:

mv :: Kobber -> Char -> [Kobber]
mv game piece = concat [useOnePiece game coord | coord <- findCoord piece (board game)]

-- find all Kobber games due to one piece
useOnePiece :: Kobber -> (Int,Int) -> [Kobber]
useOnePiece game coord = [move game coord d | d <- dirs, canMove coord (board game) d] ++
                         concat [jump game coord d | d <- dirs, canJump coord (board game) d]
  where
    dirs = "nswe"

------------------------------------

canJump :: (Int,Int) -> [String] -> Char -> Bool
canJump (i,j) rows dir
  | dir == 'n' = i > 1                   && rows!!(i-1)!!j == adversary && rows!!(i-2)!!j == cell
  | dir == 's' = i < -2+length rows      && rows!!(i+1)!!j == adversary && rows!!(i+2)!!j == cell
  | dir == 'w' = j > 1                   && rows!!i!!(j-1) == adversary && rows!!i!!(j-2) == cell
  | dir == 'e' = j < -2+length (rows!!0) && rows!!i!!(j+1) == adversary && rows!!i!!(j+2) == cell
    where
      adversary = opposite $ rows!!i!!j

-- Orthogonal draught-like jump, can be multiple
-- pre: canJump 
jump :: Kobber -> (Int,Int) -> Char -> [Kobber]
jump (P n rows) (i,j) dir 
   | not $ canJump (i,j) rows dir = []
   | otherwise                    = newposition : jump newposition coord2 dir
     where
      myPiece   = rows!!i!!j
      newvalue  = if myPiece == left then n+1 else n-1
      adversary = opposite myPiece
      rows1     = replace rows (i,j) cell
      coord1    = getDir (i,j) dir
      rows2     = replace rows1 coord1 cell
      coord2    = getDir coord1 dir
      newrows   = replace rows2 coord2 myPiece
      newposition = P newvalue newrows
      
canMove :: (Int, Int) -> [String] -> Char -> Bool    
canMove (i,j) rows dir
  | dir == 'n' = i > 0                   && rows!!(i-1)!!j == adversary
  | dir == 's' = i < -1+length rows      && rows!!(i+1)!!j == adversary      
  | dir == 'w' = j > 0                   && rows!!i!!(j-1) == adversary
  | dir == 'e' = j < -1+length (rows!!0) && rows!!i!!(j+1) == adversary
    where
      adversary = opposite $ rows!!i!!j  

-- orthogonal moves to adjacent cell  
-- pre: canMove
move :: Kobber -> (Int,Int) -> Char -> Kobber
move (P n rows) (i,j) dir = (P n newrows)
     where
      myPiece   = rows!!i!!j
      rows1     = replace rows (i,j) cell
      pos1      = getDir (i,j) dir
      newrows   = replace rows1 pos1 myPiece
      
------------------------------------

evalKobber :: FilePath -> IO ()
evalKobber filePath = 
  do
    _ <- evalBoard filePath :: IO Kobber -- don't print the internal representation
    return ()
    
------------------------------------
------------------------------------
-- empty = P 0 [[]]

-- test  = P 0 [[left,rght]]

-- test1 = P 0 [[left,rght,cell]]

-- test2 = P 3 [[left,rght,cell],
             -- [rght,left,cell],
             -- [left,cell,cell]]

-- test3 = P 0 [[cell,left,cell,left,rght,cell,rght,cell]]
             
-- zugzwang = P 0 [[cell,left,left,rght,rght,cell]]

-- endgame = P 2 [[left,cell,rght]]
             
-- showTest2 = presents $ movesKobber test2 Left
