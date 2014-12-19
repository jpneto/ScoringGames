-- make executable with: ghc --make run_disk.hs
-- run  executable with: run_disk <diskonnect file>

import Prelude hiding (Left,Right)

import System.Environment

import Scoring
import Position
import Diskonnect

main =  
  do
    args <- getArgs
    if length args == 0
     then do
            putStr "=============================================================\n"
            putStr "This program computes scoring values of diskonnect positions.\n"
            putStr "Run again but include a filename with the diskonnect position\nyou wish to analyse.\n\nYour file should be something like this:\n\n0\n.....\n..l..\n.....\n..l..\n.l.lr\n"
            putStr "\nThe first line is the current score.\n"
            putStr "\nChars 'l', 'r' and '.' represent Left pieces, Right pieces, and empty cells.\n"
            putStr "\nSave it on a file named, say, disconnect.txt and then execute"
            putStr "\nrun_disk disconnect.txt\n"
            putStr "=============================================================\n"
     else evalDiskonnect (args!!0)
