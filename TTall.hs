module TTall(TTall(P), evalTTall) where
                
import Prelude hiding (Left,Right)
import qualified Prelude hiding (Left,Right)

import Data.Char

import Position
import Scoring

-------------------------------------
-- TakeTall game
-- Pieces are 0s to 9s (in general any real numbers but for now we do not need the general setting)
-- Left moves to the right, and Right moves to the left. 
-- A player can capture by replacement an adjacent number if it is not smaller than the moved piece.
-- A move always requires a capture.
-- If the captured number is a n then the player earns n points (if a 0 then 0 points).

-- When a player does not have moves, the other gets a bonus that is equal to all possible captures
-- he could still make

-- A Position contains the amount of points already gained (positive for Left, negative for Right)
-- and also a row of cells defining the current position
------------------------------------

data TTall = P { pts :: NumberData, board :: [String] }
  deriving (Eq, Ord, Show) -- TODO: show only needed for testing, consider removing

instance Position TTall where
  points   = pointsTTall
  boards   = boardTTall
  moves    = movesTTall
  toText   = showTTall
  fromData = fromTTallData
  
------------------------------------

pointsTTall :: TTall -> NumberData
pointsTTall (P n _) = n

boardTTall :: TTall -> [String]
boardTTall (P _ b) = b

showTTall :: TTall -> String
showTTall (P n ls) = unlines ls ++ showNu n

fromTTallData :: NumberData -> [String] -> TTall
fromTTallData n rows = P n rows

------------------------------------
-- which positions are possible given a position?

movesTTall :: TTall -> Player -> [TTall]
movesTTall position@(P n [row]) Left  = 
   if (emptyBoard position)  -- the game already ended
     then []
     else if nextMvs /= [] then nextMvs -- there are still moves to do
                           else [P (computeScore position Right) (clearBoard $ board position)] -- endgame
   where 
     nextMvs = concat [useIthPieceLeft  position i | i <- [0..(length row - 2)]]
    
    
movesTTall position@(P n [row]) Right = 
   if (emptyBoard position)  -- the game already ended
     then []
     else if nextMvs /= [] then nextMvs -- there are still moves to do
                           else [P (computeScore position Left) (clearBoard $ board position)] -- endgame
   where 
     nextMvs = concat [useIthPieceRight position i | i <- [1..(length row - 1)]]

useIthPieceLeft (P n [row]) i =
    if row!!i /= cell && row!!(i+1) /= cell && row!!i <= row!!(i+1) 
      then [P newvalue newposition] 
      else []
  where
    newvalue    = n + (read [row!!(i+1)])::NumberData
    newposition = [movePiece row i (i+1)]

useIthPieceRight (P n [row]) i =
    if row!!i /= cell && row!!(i-1) /= cell && row!!(i-1) >= row!!i 
      then [P newvalue newposition] 
      else []
  where  
    newvalue    = n - (read [row!!(i-1)])::NumberData
    newposition = [movePiece row i (i-1)]

-- move piece from i to j (indexes i and j must be adjacent)
movePiece string i j = 
  if i>j then take (i-1) string ++ [string!!i] ++ [cell] ++ drop (i+1) string    
         else take   i   string ++ [cell] ++ [string!!i] ++ drop (i+2) string    

-- check is board is empty (ie, full of cells)
emptyBoard position = and (map (==cell) ((concat.board) position))
         
------------------------------------

-- here the board is a sequence of decreasing clusters (if Left  cannot move)
--                or a sequence of increasing clusters (if Right cannot move)
-- the other player scores the sum of all possible captures
computeScore :: TTall -> Player -> NumberData
computeScore position@(P n [row]) Left =   -- points for Left,  ie, Right cannot move
  n +       (fromIntegral $ sum $ concat $ map (map digitToInt) $ map tail $ wordsWhen (=='.') row)
computeScore position@(P n [row]) Right =  -- points for Right, ie, Left cannot move
  n - 1.0 * (fromIntegral $ sum $ concat $ map (map digitToInt) $ map init $ wordsWhen (=='.') row)
  
-- split a string into tokens given a separator  
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'  

------------------------------------

evalTTall :: FilePath -> IO ()
evalTTall filePath = 
  do
    _ <- evalBoard filePath :: IO TTall -- don't print the internal representation
    return ()
    
