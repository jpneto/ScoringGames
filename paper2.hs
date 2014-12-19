import Scoring

import Test.QuickCheck

(-->) a b = not a || b
  -- | not a     = True
  -- | otherwise = b
infixr 3 -->

prop12 g = (rank g <= 4) --> (ls_ g == ls (g - hat 4))

-------------------------------------------
prop16 g1 g2 = (g1 >. 0 && g2 >=. 0) --> (g1+g2 >. 0)

-------------------------------------------
prop18 g n = 
     ls_ (g+h) == ls_ g+n
   where 
     h = LE n [Nu $ n+1]
             
-------------------------------------------             
prop25 g n =
      ls (g+x) >= ls_ g + n
    where  
      x = LE n [Nu $ n+1]
   
-------------------------------------------
prop27 g =
      isOp g --> (length (leftOp g) >= 2 --> g >== h)
    where
      h = remLop 1 g

-------------------------------------------      
prop28 g =
       (length (rightOp g)>0 && not (sameGame g h) && guaranteed h)--> g >== h
     where
       r = ls_ g
       h = LE r (rightOp g)     
      
-------------------------------------   
-- test sum   
-- test2 (\g h -> (invertible g && invertible h) --> (g+h === h+g)) 1000

-- let g= Op [Op[1][2]][LE 1 [BE 1 2, RE [1] 2]]
-- g+g demora 44 segundos no PC de casa; 31 segundos do PC do trabalho