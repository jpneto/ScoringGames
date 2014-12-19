module Diskonnect(Diskonnect(P), evalDiskonnect) where
                
import Prelude hiding (Left,Right)
import qualified Prelude hiding (Left,Right)

import Position
import Scoring

------------------------------------
-- Diskonnect game

-- Each move must capture stones
-- Stones are captured by jumping orthogonally over enemy stones, checkers-like
-- Captures can be multiple, but cannot change the initial direction

-- When a player cannot move, she takes the final penalty equal to the number
-- of her own dead stones (ie, a dead stone is a stone that can still be captured)

-- A Position is the amount of points already gained (positive for Left, negative for Right)
-- and a matrix of cells defining the current position
------------------------------------

data Diskonnect = P { pts :: NumberData, board :: [String] }
  deriving (Eq, Ord, Show) -- TODO: show only needed for testing, consider removing

instance Position Diskonnect where
  points   = pointsDiskonnect
  boards   = boardDiskonnect
  moves    = movesDiskonnect
  toText   = showDiskonnect
  fromData = fromDiskonnectData
  
------------------------------------

pointsDiskonnect :: Diskonnect -> NumberData
pointsDiskonnect (P n _) = n

boardDiskonnect :: Diskonnect -> [String]
boardDiskonnect(P _ b) = b

------------------------------------

showDiskonnect :: Diskonnect -> String
showDiskonnect (P n ls) = unlines ls ++ showNu n

------------------------------------

fromDiskonnectData :: NumberData -> [String] -> Diskonnect
fromDiskonnectData n rows = P n rows

------------------------------------
-- which positions are possible given a position?

movesDiskonnect :: Diskonnect -> Player -> [Diskonnect]
movesDiskonnect position Left  = mv position left
movesDiskonnect position Right = mv position rght

------------------------------------
-- the next functions are needed to define movesDiskonnect:

-- If there are no available moves for the player which has 'piece' type,
-- then the game has ended, and she receives the negative points equal to the number
-- of dead stones

mv :: Diskonnect -> Char -> [Diskonnect]
mv game piece = 
   if (emptyBoard game)  -- the game already ended
     then []
     else if nextMvs /= [] then nextMvs -- there are still moves to do
                           else [P (computeScore game piece) (clearBoard $ board game)] -- endgame
   where
     nextMvs = nextMoves game piece
                                 
nextMoves game piece = concat [useOnePiece game coord | coord <- findCoord piece (board game)]

emptyBoard game = and (map (==cell) ((concat.board) game))

-- find all Diskonnect games due to one piece
useOnePiece :: Diskonnect -> (Int,Int) -> [Diskonnect]
useOnePiece game coord = concat [jump game coord d | d <- dirs, canJump coord (board game) d]
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

-- Orthogonal draught-like jump, can be multiple but does not change direction
-- pre: canJump 
jump :: Diskonnect -> (Int,Int) -> Char -> [Diskonnect]
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

---------------------------
-- if the player does not have more moves left, she must take the penalty of her
-- dead stones

-- to implement this, we make all possible captures with adversary stones
-- and collect the positions arising from those
-- then we merge all those positions, where if 'cell' appears in just one
-- board, then the final board will have a cell in that coordinate 
-- (this means that there were at least one path where the piece was captured
-- and so that removed piece was dead)

computeScore :: Diskonnect -> Piece -> NumberData
computeScore game@(P n pos) piece = 
     n + isLeft*((count piece $ concat finalBoard) - (count piece $ concat $ pos))
  where
      adversary = opposite piece    
      getBoard = collapseBoards $ captureFill game adversary
      -- if empty, no captures occoured, so leave everything untouched
      finalBoard = if getBoard == ["NA"] then board game else getBoard
      count c = fromIntegral . length . filter (\x -> x == c)
      isLeft = if piece==left then 1 else -1

-- make all possible captures with piece type
-- to test use: presents $ map (\p -> P 0 p) $ captureFill disk2 left
captureFill :: Diskonnect -> Char -> [[String]]
captureFill game piece =
    (map board games) ++ gamess
  where
    games  = nextMoves game piece
    gamess = concat [ captureFill gs piece | gs <- games ]
    
-- merge boards eliminating pieces when there are cells in the same coordinates    
-- on a different board
collapseBoards :: [[String]] -> [String]
collapseBoards (p1:p2:ps) = collapseBoards ((collapse p1 p2):ps)
   where
     collapse :: [String] -> [String] -> [String]
     collapse [] [] = []
     collapse (row1:rows1) (row2:rows2) = mergeRows row1 row2 : collapse rows1 rows2
     
     mergeRows :: String -> String -> String
     mergeRows [] [] = []
     mergeRows (c1:row1) (c2:row2) = (if c1==cell || c2==cell then cell else c1) : mergeRows row1 row2

collapseBoards [] = ["NA"]
collapseBoards [p] = p

------------------------------------

evalDiskonnect :: FilePath -> IO ()
evalDiskonnect filePath = 
  do
    _ <- evalBoard filePath :: IO Diskonnect -- don't print the internal representation
    return ()
    
------------------------------------
------------------------------------
-- empty = P 0 [[]]

-- test  = P 0 [[left,rght]]

-- test1 = P 0 [[left,rght,cell]]

disk0 = P 0 [[left,rght,cell,cell,cell]]

disk1 = P 0 [[left,cell,left,rght,cell]]

disk2 = P 0 [[left,rght,cell],
             [rght,left,cell],
             [cell,rght,cell],
             [rght,left,cell],
             [cell,cell,cell]]
             
term1 = P 0 [[cell,cell,cell,left],
             [left,left,cell,cell],
             [cell,cell,cell,cell],
             [rght,left,cell,cell],
             [cell,cell,left,rght]]
             

-- test3 = P 0 [[cell,left,cell,left,rght,cell,rght,cell]]
             
-- zugzwang = P 0 [[cell,left,left,rght,rght,cell]]

-- endgame = P 2 [[left,cell,rght]]
             
-- showTest2 = presents $ Diskonnect test2 Left
