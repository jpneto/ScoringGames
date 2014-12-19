module Scoring (NumberData,
                Game(..), isNu, isBE, isLE, isRE, isOp,
                leftOp, rightOp, lop, rop, goto, remLop, remRop, addLop, addRop,
                (#), canonize, --dominance, reversibility,  
                conjugate, guaranteed, stable, hot, zugzwang, tepid, rank, invertible,
                (>=.), (<=.), (<.), (>.), (==.), (>==), (<==), (===), (>=?), (<=?), (==?), 
                --sameGame, sameTree, isGreaterTree, atomicSubstitution, monotonePrinciple,
                lrp, ls, ls_, rs, rs_, 
                down, star, up, scgDiatic, scgInt, hat, scgStar, zeta, star2, star3,
                showNu, showRaw, latex,
                test, test2, testn, test2n, getCanonize, getCanonizeList,             
                --canonizedSample, guaranteedSample, unguaranteedSample,
                help, commands) where

import Data.List   -- nub
import Text.Printf -- printf
import Test.QuickCheck
import Test.QuickCheck.Gen -- ungen
import System.Random
import Control.Monad

import Help -- load help descriptions

--------------------------------
-- associate number type for atoms (uncomment the one you need)

-- DOUBLE:
--type NumberData = Double
--signif = "%.2f"  -- using n decimal places 

-- INT
type NumberData = Int

-- need to uncomment appropriate functions for showNu and sowNuLatex (search below)
--------------------------------

data Game = Nu NumberData            -- endgame, Nu n == {^n | ^n}
          | LE NumberData [Game]     -- left empty
          | RE [Game] NumberData     -- right empty
          | BE NumberData NumberData -- end game, BE n m == {^n | ^m} with n < m
          | Op [Game] [Game]         -- game with options on both sides
     deriving (Eq, Ord, Read)

isNu (Nu _)   = True
isNu   _      = False
isLE (LE _ _) = True
isLE   _      = False
isRE (RE _ _) = True
isRE   _      = False
isBE (BE _ _) = True
isBE   _      = False
isOp (Op _ _) = True
isOp   _      = False

-- NB: BE n m == Op [Nu n + scgInt (-1)] [Nu m + scgInt 1]   with n < m 
-- This equivalence is used for the computation of ls_ and rs_
makeBE n m = Op [Nu n + scgInt (-1)] [Nu m + scgInt 1]

--------------------------------
leftOp :: Game -> [Game]
leftOp (Nu _)    = []
leftOp (LE _ _)  = []
leftOp (RE gL _) = gL
leftOp (BE _ _)  = []
leftOp (Op gL _) = gL          

rightOp :: Game -> [Game]
rightOp (Nu _)    = []
rightOp (LE _ gR) = gR
rightOp (RE _ _)  = []
rightOp (BE _ _)  = []
rightOp (Op _ gR) = gR          

-- get n-th left/right option
lop :: Int -> Game -> Game 
lop n = (!!(n-1)) . leftOp

rop :: Int -> Game -> Game
rop n = (!!(n-1)) . rightOp

-- op: travel thru the tree to get a game
-- positive goes to the Left, negative goes to the right (cannot be zero)

goto :: [Int] -> Game -> Game
goto [n] g
  | n>0 = lop   n  g
  | n<0 = rop (-n) g
goto (n:ns) g
  | n>0 = goto ns $ lop   n  g
  | n<0 = goto ns $ rop (-n) g 

-- remove n-th left option  
-- pre: n>0 && n<=length (leftOp g)
remLop :: Int -> Game -> Game
remLop n (RE gL  m)
    | null newgL = error "Cannot delete only game option"
    | otherwise  = RE newgL m
  where
    newgL = deleteNth n gL
    
remLop n (Op gL gR)
    | null newgL = error "Cannot delete only game option"
    | otherwise  = Op newgL gR
  where
    newgL = deleteNth n gL

remLop _ _ = error "Cannot remove from atoms"

-- remove n-th right option  
-- pre: n>0 && n<=length (rightOp g)
remRop :: Int -> Game -> Game
remRop n (LE m gR)
    | null newgR = error "Cannot delete only game option"
    | otherwise  = LE m newgR
  where
    newgR = deleteNth n gR
    
remRop n (Op gL gR)
    | null newgR = error "Cannot delete only game option"
    | otherwise  = Op gL newgR
  where
    newgR = deleteNth n gR

remRop _ _ = error "Cannot remove from atoms"

deleteNth n xs 
  | n > length xs  = error "Game option does not exist"
  | n <=0          = error "Need a positive index to remove"
  | otherwise      = take (n-1) xs ++ drop n xs
  
-- add game into the n-th left option  
addLop :: Int -> Game -> Game -> Game
addLop i (RE gL n)  g = RE (insertNth i g gL) n
addLop i (Op gL gR) g = Op (insertNth i g gL) gR
addLop _ _ _ = error "Cannot insert games in atoms"

addRop :: Int -> Game -> Game -> Game
addRop i (LE n  gR) g = LE n  (insertNth i g gR)
addRop i (Op gL gR) g = Op gL (insertNth i g gR)
addRop _ _ _ = error "Cannot insert games in atoms"
  
insertNth :: Int -> a -> [a] -> [a]
insertNth n x xs
  | n > length xs = xs ++ [x]
  | n <= 0        = error "Need a positive index to insert"
  | otherwise     = take (n-1) xs ++ [x] ++ drop (n-1) xs
  
--------------------------------
canonize :: Game -> Game
canonize g
  | guaranteed g = applyCanons g (canon g)
  | otherwise    = error $ "Game " ++ show g ++ " is not guaranteed"
  where
    applyCanons g h = if g==h then g else applyCanons h (canon h)

canon :: Game -> Game
canon (Nu n)     = Nu n
canon (LE n gR)  = reversibility $ dominance $ LE         n         (simplify 'r' gR)
canon (RE gL n)  = reversibility $ dominance $ RE (simplify 'l' gL)         n
canon (BE n  m)  = if (n==m) then Nu n else BE n m
canon (Op gL gR) = reversibility $ dominance $ Op (simplify 'l' gL) (simplify 'r' gR)
 
-- apply domination reduction
dominance (Nu n)     = Nu n
dominance (LE n gR)  = LE n (dominanceRight [canonize giR | giR <- gR])
dominance (RE gL n)  = RE (dominanceLeft [canonize giL | giL <- gL]) n
dominance (BE n  m)  = if (n==m) then Nu n else BE n m
dominance (Op gL gR) = Op (dominanceLeft  [canonize giL | giL <- gL]) 
                          (dominanceRight [canonize giR | giR <- gR])
                         
-- if there's a G^L_i >= G^L_1 then we can remove G^L_1
dominanceLeft :: [Game] -> [Game]
dominanceLeft [g] = [g]
dominanceLeft gL  = [head gs | gs <- gss, not $ findGeq gs]
  where
    -- remove all equal games among themselves except one
    gLNotEquals = removeEqs gL
    -- return a list of list of games, where the i-ith entry has G^L_i at its head
    gss = [swapElts 0 i gLNotEquals | i <- [0..length gLNotEquals-1]]
    -- check if there's a game at the tail bigger than the game at the head of the list
    findGeq (g:gs) = any (\x -> x >== g) gs 

-- if there's a G^R_i <= G^R_1 then we can remove G^R_1 (symmetric function of dominanceLeft)
dominanceRight :: [Game] -> [Game]
dominanceRight [g] = [g]
dominanceRight gR  = [head gs | gs <- gss, not $ findLeq gs]
  where
    gRNotEquals = removeEqs gR
    gss = [swapElts 0 i gRNotEquals | i <- [0..length gRNotEquals-1]]
    findLeq (g:gs) = any (\x -> g >== x) gs 
    
-- aux function: swaps indexes i and j of list ls   
swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x

-- remove all games but one that are equal (so to prevent dominance to remove all equal games)
removeEqs:: [Game] -> [Game]
removeEqs [] = []
removeEqs [g] = [g]
removeEqs (g:gs) = g : removeEqs (removeEqsAux g gs)
  where
    removeEqsAux g [] = []
    removeEqsAux g (g1:gs) = if g===g1 then removeEqsAux g gs else g1:removeEqsAux g gs
                  
-- remove irrelevant numbers, duplicates, and order games (using Haskell Ord)
simplify :: Char -> [Game] -> [Game]
simplify   _    [] = []
simplify player gs = sort $ nub $ dominantNumber player [canon g | g <- gs]

-- checks for dominant numbers and removes the others (go away scum!)
dominantNumber :: Char -> [Game] -> [Game]
dominantNumber _ [] = []

dominantNumber 'l' pos = keepMax (-10000000::NumberData) pos
  where
    keepMax m []         = if m /= (-10000000::NumberData) then [Nu m] else []
    keepMax m (Nu n:pos) = keepMax (max n m) pos
    keepMax m (p:pos)    = p : keepMax m pos
    
dominantNumber 'r' pos = keepMin (10000000::NumberData) pos
  where
    keepMin m []         = if m /= (10000000::NumberData) then [Nu m] else []
    keepMin m (Nu n:pos) = keepMin (min n m) pos
    keepMin m (p:pos)    = p : keepMin m pos
 
--------------------------------
--------------------------------
-- Perform atomic and non-atomic reversibility at the same time
reversibility :: Game -> Game
reversibility (Nu n)   = Nu n
reversibility (BE n m) = if (n==m) then Nu n else BE n m
reversibility g@(LE n gR)
  | isN && n==m = Nu n
  | isN && n/=m = BE n m 
  | otherwise   = LE n new_gR
  where
    (isN, m, new_gR) = reversibilityRight g [reversibility giR | giR <- gR]
    
reversibility g@(RE gL n)
  | isN && n==m = Nu n
  | isN && n/=m = BE m n 
  | otherwise   = RE new_gL n 
  where
    (isN, m, new_gL) = reversibilityLeft g [reversibility giL | giL <- gL]
    
reversibility g@(Op gL gR)
  | isNL && isNR && mL==mR = Nu mL
  | isNL && isNR && mL/=mR = BE mL     mR 
  | isNL                   = LE mL     new_gR
  |         isNR           = RE new_gL mR
  | otherwise              = Op new_gL new_gR
  where
    (isNL, mL, new_gL) = reversibilityLeft  g [reversibility giL | giL <- gL]
    (isNR, mR, new_gR) = reversibilityRight g [reversibility giR | giR <- gR]

-- reversibilityLeft/Right return a triple. The boolean flag is true when the
-- game reverts to 0^n. The NumberData is that n. Otherwise, it returns the
-- final list of games after reversibility does its magic
reversibilityLeft :: Game -> [Game] -> (Bool, NumberData, [Game])
reversibilityLeft g gL
  | length gL == 1 = reversibilityLeftEach g (head gL)
  | otherwise      = compact [ reversibilityLeftEach g giL | giL <- gL ]
  where
    compact triples = (False, 0, concat $ map (\(_,_,g) -> g) triples)

reversibilityLeftEach :: Game -> Game -> (Bool, NumberData, [Game])
reversibilityLeftEach g giL
  | null gijLRs        = (False, 0, [giL])           -- nothing changes
  | leftOp gijLR /= [] = (False, 0, leftOp gijLR) -- non-atomic reversibility
  -- if leftOp gijLR == 0^r we might have atomic reversibility:
  | otherwise          = checkAtomicRev g gijLR
  where
    -- we will process only the first game that satisfies the next condition
    gijLRs = [ gijLR | gijLR <- rightOp giL, gijLR <== g]
    -- in the next function, last game is select, since it's the place of the biggest one
    -- (in the case where they are comparable)
    gijLR  = if null gijLRs then Nu 0 else last gijLRs -- then part returns a dummy game
    checkAtomicRev g gijLR 
      | length (leftOp g) == 1 && guaranteed new_g = (True, r, []) 
      | otherwise                                  = test g r 0      
 
    r     = getN gijLR
    new_g = replaceLeft g r  
    getN (Nu n)   = n
    getN (LE n _) = n
    getN (BE n _) = n
    replaceLeft (Nu n)        r = if r==n then Nu n else BE r n
    replaceLeft (RE _ n)      r = if r==n then Nu n else BE r n
    replaceLeft (Op _ gRight) r = LE r gRight
    test g r m = if g >== Nu r + scgInt (-m) then (False, 0, [Nu r + scgInt (-m-1)])
                                             else test g r (m+1)


reversibilityRight :: Game -> [Game] -> (Bool, NumberData, [Game])
reversibilityRight g gR
  | length gR == 1 = reversibilityRightEach g (head gR)
  | otherwise      = compact [ reversibilityRightEach g giR | giR <- gR ]
  where
    compact triples = (False, 0, concat $ map (\(_,_,g) -> g) triples)

reversibilityRightEach :: Game -> Game -> (Bool, NumberData, [Game])
reversibilityRightEach g giR
  | null gijRLs         = (False, 0, [giR])            -- nothing changes
  | rightOp gijRL /= [] = (False, 0, rightOp gijRL) -- non-atomic reversibility
  -- if rightOp gijRL == 0^r we might have atomic reversibility:
  | otherwise          = checkAtomicRev g gijRL
  where
    -- we will process only the first game that satisfies the next condition
    gijRLs = [ gijRL | gijRL <- leftOp giR, gijRL >== g]
    gijRL  = if null gijRLs then Nu 0 else last gijRLs  -- then part returns a dummy game
    checkAtomicRev g gijRL 
      | length (rightOp g) == 1 && guaranteed new_g = (True, r, []) 
      | otherwise                                   = test g r 0      
 
    r     = getN gijRL
    new_g = replaceRight g r  
    getN (Nu n)   = n
    getN (RE _ n) = n
    getN (BE _ n) = n
    replaceRight (Nu n)        r = if r==n then Nu n else BE n r
    replaceRight (LE n _)      r = if r==n then Nu n else BE n r
    replaceRight (Op gLeft _)  r = RE gLeft r
    test g r m = if Nu r + scgInt (m) >== g then (False, 0, [Nu r + scgInt (m+1)])
                                            else test g r (m+1)

-- game samples to test reversibility

-- <<<5|6>|<^1|1>>|5>
gr0 = Op [Op [Op [Nu 5][Nu 6]][LE 1 [Nu 1]]] [Nu 5]

-- <<2|2>,<1|1,<1|1>>|<<<^4|4>|<4|^4>>|-5>>
gr1 = Op [Op [Nu 2][Nu 2], Op [Nu 1][Nu 1, Op[Nu 1][Nu 1]]] 
         [Op [Op[LE 4 [Nu 4]][RE [Nu 4] 4]][Nu (-5)]]
              
-- <<2|2>,<1|<1|1>>|1>
gr2 = Op [Op [Nu 2][Nu 2], Op [Nu 1][Op [Nu 1][Nu 1]]] [Nu 1]   
       
-- <<4|<<^1|<^1|1>>|<^1|1>>>|<^1|1>>
gr3 = Op [Op [Nu 4][Op [LE 1 [LE 1 [Nu 1]]][LE 1 [Nu 1]]]] [LE 1 [Nu 1]]
        
-- <<2|2,<2|2>>,<1|1>|<<<^4|4>|<4|^4>>|-5>>
gr4 = Op [Op [Nu 2] [Nu 2, Op [Nu 2][Nu 2]], Op[Nu 1][Nu 1]] 
         [Op [Op [LE 4 [Nu 4]] [RE [Nu 4] 4]] [Nu (-5)]]

-- <<^0|<-1|-2>>|<<-1|-2>,<-1|^2>|<-2|^1>>> eg from the paper         
gr5 = Op [LE 0 [Op [Nu (-1)] [Nu (-2)]] ] [Op [Op[Nu (-1)][Nu (-2)],RE[Nu (-1)] 2][RE [Nu (-2)] 1]]

--------------------------------
-- stable games

stable :: Game -> Bool
stable   (Nu _)   = True
stable   (BE n m) = n <= m
stable g@(LE n _) = n <= rs g
stable g@(RE _ n) = ls g <= n
stable _ = error "This game is not inside the function's domain"

-- other properties:

hot :: Game -> Bool
hot g = hotcan $ canonize g
  where
    hotcan g = ls g > rs g

zugzwang :: Game -> Bool
zugzwang g = zugzwangcan $ canonize g
  where
    zugzwangcan g = ls g < rs g

tepid :: Game -> Bool
tepid g = tepidcan $ canonize g
  where
    tepidcan (Nu _) = False
    tepidcan g      = ls g == rs g

--------------------------------
-- check if a game value is indeed a valid game, ie, it satisfies the void rule

guaranteed :: Game -> Bool
guaranteed (Nu _)     = True
guaranteed (LE n g)   = n <= checkVal min g && all id [guaranteed gi | gi <- g]
guaranteed (RE g n)   = checkVal max g <= n && all id [guaranteed gi | gi <- g]
guaranteed (BE n m)   = n <= m
guaranteed (Op gL gR) = all id [guaranteed gi | gi <- gL] && 
                        all id [guaranteed gi | gi <- gR]
  
checkVal :: (NumberData -> NumberData -> NumberData) -> [Game] -> NumberData
checkVal f [g]    = val f g
checkVal f (g:gs) = f (val f g) (checkVal f gs)

val :: (NumberData -> NumberData -> NumberData) -> Game -> NumberData
val _ (Nu n)     = n
val f (LE n g)   = f (checkVal f g) n
val f (RE g n)   = f (checkVal f g) n
val f (BE n m)   = f n m
val f (Op gL gR) = f (checkVal f gL) (checkVal f gR)

--------------------------------
-- rank of a game, ie, which day it was born

rank :: Game -> Int
rank (Nu _)     = 0
rank (BE _ _)   = 0
rank (LE _ gR)  = 1 + maximum (map rank gR)
rank (RE gL n)  = 1 + maximum (map rank gL)
rank (Op gL gR) = 1 + max (maximum $ map rank gR) (maximum $ map rank gL)

--------------------------------
-- left stop
ls :: Game -> NumberData
ls (Nu n)    = n
ls (LE  n _) = n
ls (RE gL _) = maximum [rs giL | giL <- gL]
ls (BE  n _) = n
ls (Op gL _) = maximum [rs giL | giL <- gL]

-- right stop
rs :: Game -> NumberData
rs (Nu n)    = n
rs (RE _ n)  = n
rs (LE _ gR) = minimum [ls giR | giR <- gR]
rs (BE _ n)  = n
rs (Op _ gR) = minimum [ls giR | giR <- gR]

--------------------------------
-- Cf. disjunctive sum definition

(#) :: Game -> Game -> Game
(#) g1 g2 
  |     gg1 &&     gg2 = g1 ## g2
  | not gg1 && not gg2 = error $ "Both games " ++ show g1 ++ " and " ++ show g2 ++ 
                                 " are not guaranteed"
  | not gg1            = error $ "First game "  ++ show g1 ++ " is not guaranteed"
  | not gg2            = error $ "Second game " ++ show g2 ++ " is not guaranteed"
  where
    gg1 = guaranteed g1
    gg2 = guaranteed g2

-- eq.1
(##) (Nu n)     (Nu m)     = Nu (n+m)          
(##) (Nu m)     (BE n1 n2) = BE (n1+m)  (n2+m)
(##) (BE n1 n2) (Nu m)     = BE (n1+m)  (n2+m)
(##) (BE n1 n2) (BE m1 m2) = BE (n1+m1) (n2+m2)

-- eq.2
(##) (Nu n)     (LE m gR)  = (#) (LE m gR) (Nu n)     -- commutative  
(##) (BE n1 n2) (LE m gR)  = (#) (LE m gR) (BE n1 n2) -- commutative

(##) (LE m  gR)  (Nu n)      = LE (m+n)   (getRightSumList (LE m gR)   (Nu n))     
(##) (LE m  gR)  (BE n1 n2)  = LE (m+n1)  (getRightSumList (LE m gR)   (BE n1 n2))
(##) (LE m1 gR1) (LE m2 gR2) = LE (m1+m2) (getRightSumList (LE m1 gR1) (LE m2 gR2))

-- eq.3
(##) (Nu n)     (RE gL m)  = (#) (RE gL m) (Nu n)     -- commutative
(##) (BE n1 n2) (RE gL m)  = (#) (RE gL m) (BE n1 n2) -- commutative

(##) (RE gL  m)  (Nu n)      = RE (getLeftSumList (RE gL m)   (Nu n))      (m+n)   
(##) (RE gL  m)  (BE n1  n2) = RE (getLeftSumList (RE gL m)   (BE n1 n2))  (m+n2)
(##) (RE gL1 m1) (RE gL2 m2) = RE (getLeftSumList (RE gL1 m1) (RE gL2 m2)) (m1+m2) 

-- eq.4
(##) gGame hGame = Op (getLeftSumList gGame hGame) (getRightSumList gGame hGame)

-- getRightSumList gs hs = [gs^R + hs, gs + hs^R]
getRightSumList :: Game -> Game -> [Game]
getRightSumList gGame hGame = 
    nub([g ## hGame | g <- rightOp gGame] ++ [gGame ## h | h <- rightOp hGame])

-- getLeftSumList gs hs = [gs^L + hs, gs + hs^L]
getLeftSumList :: Game -> Game -> [Game]
getLeftSumList gGame hGame = 
    nub([g ## hGame | g <- leftOp gGame] ++ [gGame ## h | h <- leftOp hGame])

--------------------------------
-- conjugate -G = { -g^R | -g^L }

conjugate :: Game -> Game
conjugate (Nu n)     = Nu (-n)
conjugate (LE n g)   = RE (map conjugate g)         (-n)
conjugate (RE g n)   = LE        (-n)        (map conjugate g)
conjugate (BE n m)   = BE        (-m)               (-n)
conjugate (Op gL gR) = Op (map conjugate gR) (map conjugate gL)

--------------------------------
-- making Games instances of class Number

instance Num Game where
  g1 + g2       = canonize $ canonize g1 # canonize g2  
  g1 - g2       = canonize $ canonize g1 # negate g2
  negate        = canonize . conjugate
  fromInteger a = Nu (fromIntegral a)
  -- not defined functions
  g1 * g2  = error "Multiplication not implemented for games"
  abs g    = error "Absolute Value not implemented for games"
  signum g = error "Signum not implemented for games"

--------------------------------
-- ls_ left stop pass allowed

ls_ :: Game -> NumberData
ls_ (Nu n)     = n  -- a)
ls_ (LE n _)   = n  -- a)
ls_ (RE gL _)  = maximum [rs_ g | g <- gL]  -- b)
ls_ (BE n m)   = ls_ $ makeBE n m
ls_ (Op gL gR) = maximum [rs_ g | g <- gL]  -- b)

-- rs_ right stop pass allowed

rs_ :: Game -> NumberData
rs_ (Nu n)     = n -- a)
rs_ (RE gL n)  = minimum $ [n, ls_ (RE gL n)] -- c)
rs_ (LE n gR)  = minimum $ ls_ (LE n gR)  : [ls_ g | g <- gR]  -- d)
rs_ (BE n m)   = rs_ $ makeBE n m
rs_ (Op gL gR) = minimum $ ls_ (Op gL gR) : [ls_ g | g <- gR]  -- d)

--------------------------------
-- left-r-protected

lrp :: NumberData -> Game -> Bool
lrp r g = ls_ g >= r && 
          for_all [ for_any [ lrp r gRL | gRL <- leftOp gR ] | gR <- rightOp g ]
    where
      for_all = all id
      for_any = any id

--------------------------------
-- Relational operators
      
(>=.) :: Game -> NumberData -> Bool
game >=. n = lrp n game    
  
-- G >= n <=> -G <= -n
(<=.) :: Game -> NumberData -> Bool
(<=.) g n = conjugate g >=. (-n) 

(>.) :: Game -> NumberData -> Bool
(>.) g n = g >=. n && not (g <=. n)

(<.) :: Game -> NumberData -> Bool
(<.) g n = g <=. n && not (g >=. n)

(==.) :: Game -> NumberData -> Bool
(==.) g n = g >=. n && g <=. n

(/=.) :: Game -> NumberData -> Bool
(/=.) g n = not $ (==.) g n

infixl 4 <=.  -- these operators have low priority
infixl 4 >=.
infixl 4 >.
infixl 4 <.
infixl 4 ==.
infixl 4 /=.

invertible :: Game -> Bool
invertible g = (gsub >=. 0) && (gsub <=. 0) 
  where
    gsub = g # conjugate g  -- (#) is (waaay) faster than (+)

-- G1 >= G2 
(>==) :: Game -> Game -> Bool
g1 >== g2 = isGreaterTree g1 g2 -- Comparing game trees (includes G>=G)
          || (guaranteed g1 && atomicSubstitution g1 g2)
          || (guaranteed g1 && monotonePrinciple  g1 g2)
          || (invertible g2 && (g1 # conjugate g2 >=. 0))
          || (invertible g1 && (g2 # conjugate g1 <=. 0))

(<==) :: Game -> Game -> Bool
(<==) = flip (>==)    
  
(===) :: Game -> Game -> Bool
g1 === g2 = (g1 >== g2) && (g2 >== g1)
  
-- version with maybe

(>=?) :: Game -> Game -> Maybe Bool
g1 >=? g2 
  | isGreaterTree g1 g2                            = Just True  -- Compare game trees (includes G>=G)
  | isGreaterTree g2 g1                            = Just False -- Compare game trees (includes G>=G)
  | guaranteed g1 && atomicSubstitution g1 g2      = Just True
  | guaranteed g2 && atomicSubstitution g2 g1      = Just False
  | guaranteed g1 && monotonePrinciple  g1 g2      = Just True
  | guaranteed g2 && monotonePrinciple  g2 g1      = Just False
  | invertible g2 && g1 # conjugate g2 >=. 0       = Just True
  | invertible g1 && g2 # conjugate g1 <=. 0       = Just True
  | invertible g2 && not (g1 # conjugate g2 >=. 0) = Just False
  | invertible g1 && not (g2 # conjugate g1 <=. 0) = Just False
  | otherwise                                      = Nothing

(<=?) :: Game -> Game -> Maybe Bool
(<=?) = flip (>=?)      

(==?) :: Game -> Game -> Maybe Bool
g1 ==? g2 = check (g1 >=? g2) (g2 >=? g1)
  where
    check Nothing _ = Nothing
    check _ Nothing = Nothing
    check (Just x) (Just y) = Just (x&&y)

infixl 4 <==
infixl 4 >==   
infixl 4 ===
infixl 4 <=?
infixl 4 >=?   
infixl 4 ==?

-- checks if it is exactly the same game
sameGame :: Game -> Game -> Bool
sameGame g1 g2 = showRaw g1 == showRaw g2

-- checks if two games have the same tree
sameTree :: Game -> Game -> Bool
sameTree g1 g2 = sameGame zero1 zero2
  where
    zero1 = zeroTree g1
    zero2 = zeroTree g2  
    zeroTree (Nu _)     = Nu 0
    zeroTree (BE _ _)   = BE 0 0
    zeroTree (LE _ gR)  = LE 0 [zeroTree giR | giR <- gR]
    zeroTree (RE gL _)  = RE [zeroTree giL | giL <- gL] 0
    zeroTree (Op gL gR) = Op [zeroTree giL | giL <- gL] [zeroTree giR | giR <- gR]

-- check if G >= H by checking their tree nodes
-- returning False only means that this criteria failed, not that G<H
isGreaterTree g1 g2
  | sameTree g1 g2 = compareTree (>=) g1 g2
  | otherwise      = False
  
-- pre: g1 and g2 share the same tree  
compareTree :: (NumberData -> NumberData -> Bool) -> Game -> Game -> Bool
compareTree f (Nu n1)      (Nu n2)      = f n1 n2
compareTree f (BE n1 m1)   (BE n2 m2)   = f n1 n2 && f m1 m2
compareTree f (LE n1 gR1)  (LE n2 gR2)  = f n1 n2 && 
                                          all id [compareTree f giR1 giR2 | (giR1,giR2) <- zip gR1 gR2]
compareTree f (RE gL1 n1)  (RE gL2 n2)  = f n1 n2 && 
                                          all id [compareTree f giL1 giL2 | (giL1,giL2) <- zip gL1 gL2]
compareTree f (Op gL1 gR1) (Op gL2 gR2) = all id [compareTree f giL1 giL2 | (giL1,giL2) <- zip gL1 gL2] &&
                                          all id [compareTree f giR1 giR2 | (giR1,giR2) <- zip gR1 gR2]
  
-- pre: first game must be guaranteed
atomicSubstitution :: Game -> Game -> Bool  -- Lemma of Comparisation
atomicSubstitution g1@(LE _ gR1) (LE n2 gR2)
  | same_gR gR1 gR2 && ls_ g1 == n2 = guaranteed $ LE n2 gR2
  | otherwise                       = False
atomicSubstitution g1@(Op _ gR1) (LE n2 gR2)
  | same_gR gR1 gR2 && ls_ g1 == n2 = guaranteed $ LE n2 gR2
  | otherwise                       = False
atomicSubstitution _ _ = False

same_gR gR1 gR2 = all id [sameTree giR1 giR2 | (giR1,giR2) <- zip sort_gR1 sort_gR2]
  where
    sort_gR1 = sort gR1
    sort_gR2 = sort gR2

-- pre: first game must be guaranteed
monotonePrinciple :: Game -> Game -> Bool  -- Theorem Monotone Principle
monotonePrinciple g1 g2
  | length (leftOp g1) < 2 = False
  | otherwise              = map showRaw sort_gR1 == map showRaw sort_gR2 -- gR1 == gR2
                          && removeEachOption (length (leftOp g1)) g1 g2
  where
    sort_gL1 = sort $ leftOp  g1  -- sort games, so that 'map showRaw' can compare both
    sort_gR1 = sort $ rightOp g1
    sort_gL2 = sort $ leftOp  g2
    sort_gR2 = sort $ rightOp g2
    -- this principle states that if the g2 is equal to g1 except for
    -- one game in the left option, then g1 >= g2
    removeEachOption 0 _  _  = False
    removeEachOption i g1 g2 = map showRaw (deleteNth i sort_gL1) == map showRaw sort_gL2 
                            || removeEachOption (i-1) g1 g2
    
--------------------------------
-- short conway games (scg) to scoring games

-- integer version
-- scgInt 6 = {{{{{{0|^0}|^0}|^0}|^0}|^0}|^0}
scgInt :: (Ord a, Num a) => a -> Game
scgInt 0 = Nu 0
scgInt n 
  | n > 0 = RE [scgInt (n-1)] 0
  | n < 0 = LE 0 [scgInt (n+1)]

hat = scgInt
  
-- diatic version
---- eg 5/8 = {{0|{0|^0}}|{{0|{0|^0}}|{0|^0}}}
---- call: scgDiatic (5,8)
scgDiatic :: (Int,Int) -> Game
scgDiatic (n,d) = scgD (n,d)
  where
     scgD (n,1) = scgInt n
     scgD (n,d) = Op [scgD (preDiatic n d)] [scgD (postDiatic n d)]
     preDiatic n d  = simplify (n-1) d
     postDiatic n d = simplify (n+1) d
     simplify n d = (quot n mdc, quot d mdc)
       where
         mdc = gcd n d
 
-- convert conway's *n to scoring format 
scgStar :: Int -> Game
scgStar 0 = 0
scgStar n = Op (map scgStar [0..(n-1)]) (map scgStar [0..(n-1)])
         
-- conway embebbing for numbers
-- pre: den == 2^pot, pot >= 0
zeta :: Int -> Int -> Game
zeta num den 
   | den == 1  = scgInt $ fromIntegral num  -- an integer
   | otherwise = scgDiatic (num,den)        -- a diatic
  
-- special games
star  = scgStar 1
star2 = scgStar 2
star3 = scgStar 3
up    = Op [Nu 0] [star]
down  = Op [star] [Nu 0]  

--------------------------------
--------------------------------
--------------------------------
-- Printing facilities
       
instance Show Game where
    show = remBrackets . showG
    
showG (Nu n)     = showNuLatex n
showG (LE n g)   = "<^" ++ showNuLatex n ++ "|"  ++ show g        ++ ">"
showG (RE g n)   = "<"  ++ show g        ++ "|^" ++ showNuLatex n ++ ">"
showG (BE n m)   = "<^" ++ showNuLatex n ++ "|^" ++ showNuLatex m ++ ">"
showG (Op gL gR) = "<"  ++ show gL        ++ "|"  ++ show gR      ++ ">"
    
remBrackets = filter . flip notElem $ "[]" -- remove the [...] from the lists

-- check if a double/float is an integer
isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

-- use this for doubles
-- remove the fractional part if the double/float is an integer
-- showNu :: NumberData -> String
-- showNu x
  -- | isInt x   = if x>=0 then show $ round x  else "(" ++ (show $ round x)  ++ ")"
  -- | otherwise = if x>=0 then printf signif x else "(" ++ (printf signif x) ++ ")"
  
-- use this for Ints  
showNu :: NumberData -> String
showNu x = if x>=0 then show x else "(" ++ show x ++ ")"
  
showRaw (Nu n)     = "Nu "  ++ showNu n
showRaw (LE n g)   = "LE "  ++ showNu n    ++ " ["  ++ showRaws g  ++ "]"
showRaw (RE g n)   = "RE [" ++ showRaws g  ++ "] "  ++ showNu n
showRaw (BE n m)   = "BE "  ++ showNu n    ++ " "   ++ showNu m
showRaw (Op gL gR) = "Op [" ++ showRaws gL ++ "] [" ++ showRaws gR ++ "]"

showRaws [g]    = showRaw g
showRaws (g:gs) = showRaw g ++ "," ++ showRaws gs

-- use this for doubles
-- show negative numbers without parenthesis
-- showNuLatex :: NumberData -> String
-- showNuLatex x
  -- | isInt x   = show $ round x
  -- | otherwise = printf signif x 

-- use this for ints
showNuLatex = show
  
latex :: Game -> String
latex g = "$$" ++ (remBrackets.toLaTeXAux) g ++ "$$"
  where
    toLaTeXAux(Nu n)     = showNuLatex n
    toLaTeXAux(LE n g)   = "<\\emptyset^{" ++ showNuLatex n ++ "}|" ++ toLaTeXList g ++ ">"
    toLaTeXAux(RE g n)   = "<" ++ toLaTeXList g ++ "|\\emptyset^{" ++ showNuLatex n ++ "}>"
    toLaTeXAux(BE n m)   = "<\\emptyset^{" ++ showNuLatex n ++ "\\}|\\emptyset^{" ++ showNuLatex m ++ "}>"
    toLaTeXAux(Op gL gR) = "<" ++ toLaTeXList gL ++ "|" ++ toLaTeXList gR ++ ">"
    toLaTeXList [] = ""
    toLaTeXList [g]    = toLaTeXAux g
    toLaTeXList (g:gs) = toLaTeXAux g ++ "," ++ toLaTeXList gs

--------------------------------
--------------------------------
--------------------------------
-- Testing facilities

---- About QuickCheck:
-- http://tab.snarc.org/posts/haskell/2010-12-02-using_quickcheck.html
-- http://jasani.org/2008/01/03/testing-haskell-with-quickcheck/
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

instance Arbitrary Game where
   arbitrary = sized arbGame

arbGame :: Int -> Gen Game
arbGame 0 = liftM Nu arbitrary
arbGame n = do
              m <- choose (2, min (n+1) 6)
              let n' = div n m
              oneof [liftM  Nu (choose (-10, 10)),
                     liftM2 LE (choose (-10, 10)) (replicateM m $ arbGame n'),
                     liftM2 RE (replicateM m $ arbGame n') (choose (-10, 10)),
                     liftM2 BE (choose (-10, 10)) (choose (-10, 10)),
                     liftM2 Op (replicateM m $ arbGame n') (replicateM m $ arbGame n')]

{----------------
-- creating properties
prop_gminusg_ls_eq0 g = guaranteed g ==> (ls(g - g) == 0)
prop_gminusg_rs_eq0 g = guaranteed g ==> (rs(g - g) == 0)
   
gameType :: Game -> String
gameType (Nu _)   = "NU"
gameType (LE _ _) = "LE"
gameType (RE _ _) = "RE"
gameType (BE _ _) = "BE"
gameType     _    = "OP"

prop_conjugate g = 
    classify (gameType g=="NU") "Numbers" $
    classify (gameType g=="LE") "Left Empty" $
    classify (gameType g=="RE") "Right Empty" $
    classify (gameType g=="BE") "Both Empty" $
    classify (gameType g=="OP") "Options" $
    (conjugate.conjugate) g == g

-- quickCheck prop_gminusg_eq0    -- check property
-- verboseCheck prop_gminusg_eq0  -- check and see case by case:
-- sample' arbitrary :: IO [Game] -- see a list of random games
----------------}
 
-- to create a Game generator
gameGen = arbitrary :: Gen Game
gameNum = arbitrary :: Gen NumberData
-- sample gameGen -- to see some examples

{---------------- -- TODO: these next three lists seem to only output Nu and BE games...
-- generate random canonized games
canonizedSample :: Int -> Int -> [Game]
canonizedSample seed size = 
   take size $ map canonize $ filter guaranteed $ unGen arbitrary (mkStdGen seed) (1000*size)

-- generate random guaranteed games
guaranteedSample :: Int -> Int -> [Game]
guaranteedSample seed size = 
   take size $ filter guaranteed $ unGen arbitrary (mkStdGen seed) (1000*size)
   
-- generate random games
unguaranteedSample :: Int -> Int -> [Game]
unguaranteedSample seed size = 
   unGen arbitrary (mkStdGen seed) size
----------------}

--------------------------------
-- checking unary and binary propositions

-- rundExpr (arbitrary::Gen Game) produces a new random scoring game 
rundExpr :: Gen a -> IO a
rundExpr gen = fmap (flip (unGen gen) 5) newStdGen

-- get a random canonize game
getCanonize :: IO Game
getCanonize = do
                  g <- rundExpr (arbitrary::Gen Game)
                  if guaranteed g
                     then return (canonize g)
                     else getCanonize
  
getCanonizeList n = forM [1..n] (\_ -> getCanonize)
  
gameTest :: (Game -> Bool) -> IO Bool
gameTest f =
    do
      g <- getCanonize
      if not (f g) 
        then 
          do 
            putStrLn $ "Found a counter example:"
            putStrLn $ "Game: " ++ (show $ g)
            return False
        else return True
        
-- check unary propositions     
test :: (Game -> Bool) -> Int -> IO ()
test f  0   = putStrLn $ "Tests finished!"                            
test f runs =
    do
      result <- gameTest f
      putStr "."
      if result then test f (runs-1)
                else putStrLn "" >> return ()
     
gameTest2 :: (Game -> Game -> Bool) -> IO Bool
gameTest2 f =
    do
      g1 <- getCanonize
      g2 <- getCanonize
      if not (f g1 g2) 
        then 
          do 
            putStrLn $ "Found a counter example:"
            putStrLn $ "Game 1: " ++ (show $ g1)
            putStrLn $ "Game 2: " ++ (show $ g2)
            return False
        else return True
       
-- check binary propositions     
test2 :: (Game -> Game  -> Bool) -> Int -> IO ()
test2 f  0   = putStrLn $ "Tests finished!"                            
test2 f runs =
    do
      result <- gameTest2 f
      putStr "."
      if result then test2 f (runs-1)
                else putStrLn "" >> return ()

-- check unary proposition using also a random number
gameTestn :: (Game -> NumberData -> Bool) -> IO Bool
gameTestn f =
    do
      g <- getCanonize
      n <- sample' arbitrary :: IO [NumberData]
      if not (f g $ n!!1) 
        then 
          do 
            putStrLn $ "Found a counter example:"
            putStrLn $ "Game: " ++ (show g)
            putStrLn $ "Number: " ++ (show $ n!!1)
            return False
        else return True

testn :: (Game -> NumberData -> Bool) -> Int -> IO ()
testn f  0   = putStrLn $ "Tests finished!"                            
testn f runs =
    do
      result <- gameTestn f
      putStr "."
      if result then testn f (runs-1)
                else putStrLn "" >> return ()

 -- check unary proposition using also a random number
gameTest2n :: (Game -> Game -> NumberData -> Bool) -> IO Bool
gameTest2n f =
    do
      g1 <- getCanonize
      g2 <- getCanonize
      n <- sample' arbitrary :: IO [NumberData]
      if not (f g1 g2 $ n!!1) 
        then 
          do 
            putStrLn $ "Found a counter example:"
            putStrLn $ "Game1: " ++ (show g1)
            putStrLn $ "Game2: " ++ (show g2)
            putStrLn $ "Number: " ++ (show $ n!!1)
            return False
        else return True

test2n :: (Game -> Game -> NumberData -> Bool) -> Int -> IO ()
test2n f  0   = putStrLn $ "Tests finished!"                            
test2n f runs =
    do
      result <- gameTest2n f
      putStr "."
      if result then test2n f (runs-1)
                else putStrLn "" >> return ()
               
--------------------------------