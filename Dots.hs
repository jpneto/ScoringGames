module Dots(Dots(P), evalDots) where

import Prelude hiding (Left,Right)
import qualified Prelude hiding (Left,Right)
import Data.List

import Position
import Scoring

------------------------------------
-- Dots and Boxes

-- Starting with an empty grid of dots, players take turns, adding a single horizontal or vertical line between two unjoined adjacent dots. A player who completes the fourth side of a 1×1 box earns one point and takes another turn. (The points are typically recorded by placing in the box an identifying mark of the player, such as an initial). The game ends when no more lines can be placed. The winner of the game is the player with the most points. [wikipedia]

------------------------------------

data Dots = P { pts :: NumberData, board :: [String] }
  deriving (Eq, Ord, Show) -- TODO: show only needed for testing, consider removing

instance Position Dots where
  points   = pointsDots
  boards   = boardPoints
  moves    = movesDots
  toText   = showDots2
  fromData = fromDotsData
  
-- This position:

-- +-+ + + +-+-+
-- |L|   | |R|R| 
-- +-+-+-+ +-+-+ 

-- will be represented by the following [String]:

--  x . . . x x 
-- x x . x x x x
--  x x x . x x 
 
-- where the markings/spaces are just there for presentation, so the true representation is
 
-- x...xx 
-- xx.xxxx
-- xxx.xx 
  
------------------------------------

pointsDots :: Dots -> NumberData
pointsDots (P n _) = n

boardPoints :: Dots -> [String]
boardPoints (P _ b) = b

------------------------------------
-- > putStrLn $ showDots (P (-1) ["x...xx","xx.xxxx","xxx.xx"])    
--  x . . . x x
-- x x . x x x x
--  x x x . x x
-- -1

showDots :: Dots -> String
showDots (P n ls) = present ls 1 ++ showNu n
  where
    present [] _ = []
    present (l:ls) r
     | odd r     =        foldr (\x xs -> ' ':x:xs) "\n" l ++ present ls (r+1)
     | otherwise = tail $ foldr (\x xs -> ' ':x:xs) "\n" l ++ present ls (r+1)
    
------------------------------------------
-- Alternative presentation

-- > putStrLn $ showDots2 (P (-1) ["x...xx","xx.xxxx","xxx.xx"]) 
-- +-+ + + +-+-+
-- |#|   | |#|#|
-- +-+-+-+ +-+-+
-- -1

-- > putStrLn $ showDots2 (P (-3) ["x...xx","xx.xxxx","xxx.xx","xx...xx","xxx..x"]) 
-- +-+ + + +-+-+
-- |#|   | |#|#|
-- +-+-+-+ +-+-+
-- |#|       |#|
-- +-+-+-+ + +-+
-- -3

-- > putStrLn $ showDots2 (P 1 ["x...xx","xx.xxxx","xxx.xx","x.xx.xx","xxx..x","xx.xxxx","xxxxxx"])
-- +-+ + + +-+-+
-- |#|   | |#|#|
-- +-+-+-+ +-+-+
-- |   |#|   |#|
-- +-+-+-+ + +-+
-- |#|   | | |#|
-- +-+-+-+-+-+-+
-- 1

-- odd  lines have the format like "+-+-+-+ + +-+"
-- even lines have vertical walls, '|', or spaces

showDots2 :: Dots -> String
showDots2 (P n ls) = placeMarkers (lines (present ls 1)) 1 n ++ "\n" ++ showNu n
  where
    present [] _ = []
    present (row:rows) r
      | odd r     = presentOddLine  row ++ present rows (r+1)
      | otherwise = presentEvenLine row ++ present rows (r+1)
    presentOddLine []     = "+\n"
    presentOddLine (c:cs)
      | c == 'x'  = "+-" ++ presentOddLine cs
      | otherwise = "+ " ++ presentOddLine cs
    presentEvenLine []     = "\n"
    presentEvenLine [c]
      | c == 'x'  = "|\n" 
      | otherwise = " \n" 
    presentEvenLine (c:cs)
      | c == 'x'  = "| " ++ presentEvenLine cs
      | otherwise = "  " ++ presentEvenLine cs

-- this function place markers inside the squares w.r.t the actual scoring
-- there are only replacements in the even lines, odd lines are not affected
placeMarkers [row] _ _ = row
placeMarkers (row1:row2:row3:rows) r n
  | even r    = fst (processEvenLine row1 row2 row3 [] n) ++ "\n" ++
                placeMarkers (row3:rows) (r+1) (snd (processEvenLine row1 row2 row3 [] n)) 
  | otherwise = row1 ++ "\n" ++ placeMarkers (row1:row2:row3:rows) (r+1) n -- do nothing, goto next
    where
       -- each even line needs the previous (r1s) and the next (r3s) odd lines to
       -- determine if there are made squares, in order to place markers
       processEvenLine [_] [r2] [_] row2 n = (row2++[r2],n) -- last column (has space or |)
       processEvenLine (r11:r12:r13:r1s) (r21:r22:r23:r2s) (r31:r32:r33:r3s) newrow2 n
        | r21 == '|' && r23 == '|' && 
          r12 == '-' && r32 == '-'  = 
            if (n>0) then processEvenLine (r13:r1s) (r23:r2s) (r33:r3s) (newrow2 ++ "|#") (n-1)  -- or "|L"
                     else processEvenLine (r13:r1s) (r23:r2s) (r33:r3s) (newrow2 ++ "|#") (n+1)  -- or "|R"
        | otherwise =     processEvenLine (r13:r1s) (r23:r2s) (r33:r3s) (newrow2 ++ r21:r22:[]) n

------------------------------------

fromDotsData :: NumberData -> [String] -> Dots
fromDotsData n rows = P n rows  

------------------------------------
-- which positions are possible given a position?

movesDots :: Dots -> Player -> [Dots]
movesDots position player  = concat [useOnePiece position player coord | coord <- possibleMoves position]

------------------------------------
-- find all positions due to one move
-- pre: board at coord must be empty

useOnePiece :: Dots -> Player -> (Int,Int) -> [Dots]
useOnePiece position player coord
  | closedSqs == 0 = [newPosition] -- single move, no squares closed
                      -- otherwise, same player continues playing, after the wall is placed
  | otherwise      =  if endgame then [newPosition]
                                 else concat [ useOnePiece newPosition player coord2 | coord2 <- possibleMoves newPosition ] 
    where
      closedSqs = closeSquare position coord  -- how many squares were closed?
      newBoard  = replace (board position) coord 'x'
      newScore  = if player==Left then pts position + fromIntegral closedSqs 
                                  else pts position - fromIntegral closedSqs
      newPosition = P newScore newBoard
      endgame     = possibleMoves newPosition == []

----------------------------------
-- possible moves

possibleMoves :: Dots -> [(Int,Int)]
possibleMoves position = concat $ zipWith (\x y -> zip x y) infs dotIdxs
  where
    infs    = [ [i,i..i] | i <- [0..]] :: [[Int]] -- [[0,0,...],[1,1,...],[2,2,...],...] only in Haskell :-)
    dotIdxs = map (elemIndices '.') (board position)

------------------------------------
-- place a wall in the board (does not change scoring)
-- replace function is available at Position module

placeWall :: Dots -> (Int,Int) -> Dots
placeWall (P n brd) coord = P  n (replace brd coord 'x')

------------------------------------
-- playing at coord, how much square it closes? (returns -1 if cell is occupied
-- coordinates start at zero here!
closeSquare :: Dots -> (Int,Int) -> Int
closeSquare position (row,col) 
   | brd!!row!!col /= '.' = -1
   | odd row   = leftOddSquare brd (row,col) + rightOddSquare brd (row,col)
   | otherwise = upEvenSquare  brd (row,col) + downEvenSquare brd (row,col)
    where
      brd = board position
      leftOddSquare brd (row,col) = if col>0 &&                               -- x
                                       brd!!(row-1)!!(col-1) == 'x' &&        -- x .
                                       brd!!(row+1)!!(col-1) == 'x' &&        -- x
                                       brd!!row!!(col-1)     == 'x' then 1 else 0
      rightOddSquare brd (row,col) = if col<length (brd!!0) &&                -- x
                                       brd!!(row-1)!!col == 'x' &&            -- . x
                                       brd!!(row+1)!!col == 'x' &&            -- x
                                       brd!!row!!(col+1) == 'x' then 1 else 0
      downEvenSquare brd (row,col) = if row+1<length brd &&                   -- .
                                       brd!!(row+1)!!col     == 'x' &&        -- x x
                                       brd!!(row+1)!!(col+1) == 'x' &&        -- x
                                       brd!!(row+2)!!col     == 'x' then 1 else 0
      upEvenSquare brd (row,col) = if  row>1 &&                               -- x
                                       brd!!(row-1)!!col     == 'x' &&        -- x x
                                       brd!!(row-1)!!(col+1) == 'x' &&        -- .
                                       brd!!(row-2)!!col     == 'x' then 1 else 0


-- returns a list of list with the amount of closed squares each wall placing
-- originates (-1 if the cell is occupied)
-- note: function mainly for testing purposes
testClosings :: Dots -> [[Int]]
testClosings position = check 0
     where
      brd   = board position
      nrows = length brd
      check i 
        | i == nrows = []
        | otherwise  = [ closeSquare position (i,j-1) |  j <- [1..length (brd!!i)] ] : check (i+1)
 
------------------------------------

evalDots :: FilePath -> IO ()
evalDots filePath = 
  do
    _ <- evalBoard filePath :: IO Dots -- don't print the internal representation
    return ()
     
------------------------------------
------------------------------------
------------------------------------

dots1 = P 1 ["x...xx","xx.xxxx","xxx.xx","x.xx.xx","xxx...","xx.xxxx","xxxxxx"]

-- use:
-- > putStrLn $ showDots2 dots1

-- +-+ + + +-+-+
-- |#|   | |#|#|
-- +-+-+-+ +-+-+
-- |   |#|   | |
-- +-+-+-+ + + +
-- |#|   | | | |
-- +-+-+-+-+-+-+
-- 1 points

dzzwang = P 0 [".","xx",".","xx","."]

