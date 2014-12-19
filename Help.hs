module Help (help, commands,
             help_pos, commands_pos) 
             where

-- Help facilities

separator = "----------------------------------------------------------\n"

help :: String -> IO ()
help "+" = putStr $ separator ++
                    "Disjuctive sum of two or more games." ++
                    "Use: G1 + G2 + ...\n" ++
                    "Egs:\n" ++
                    "> let g1 = Nu 5\n" ++
                    "> g1\n" ++
                    "5\n" ++
                    "> let g2 = LE (-1) [Nu 3]\n" ++
                    "> g2\n" ++
                    "<^-1|3>\n" ++
                    "> let g3 = RE [Op [Nu 1, Op [Nu 3] [Nu (-1)]] [Nu (-11)]] (-2)\n" ++
                    "> g3" ++
                    "<<1,<3|-1>|-11>|^-2>\n" ++
                    "> g1+g2+g3\n" ++
                    "<<<^5|9>,<<^7|11>|<^3|7>,<11|7>>|<^-7|-3>,<9,<11|7>|-3>>|<<9,<11|7>|-3>|^6>>\n" ++
                    separator

help "-" = putStr $ separator ++
                    "Two possible uses:\n" ++
                    "\tAs a unary function of a game -G means the conjugate of game G\n" ++
                    "\tAs a binary function of two games means G1 + (-G2)\n" ++
                    "Egs:\n" ++
                    "> let g1 = Nu 5\n" ++
                    "> g1\n" ++
                    "5\n" ++
                    "> let g2 = LE (-1) [Nu 3]\n" ++
                    "> g2\n" ++
                    "<^-1|3>\n" ++
                    "> -g2\n" ++
                    "<-3|^1>\n" ++
                    "> conjugate g2\n" ++
                    "<-3|^1>\n" ++
                    "> g1-g2\n" ++
                    "<2|^6>\n" ++
                    "> g2-g1\n" ++
                    "<^-6|-2>\n" ++
                    separator

help "conjugate" = putStr $ separator ++
                            "The conjugate of game G = < -gR | -gL > \n" ++
                            "Eg:\n" ++
                            "> conjugate (Op [Nu (-1)] [LE 5 [Nu 10]])\n" ++
                            "<<-10|^-5>|1>\n" ++
                            separator

help "canonize" = putStr $ separator ++
                            "Returns the canonical expression of a game\n" ++
                            "Eg:\n" ++
                            "> let g = Op [Nu 3, Op [Nu 5] [Op [Nu 2] [Nu 0]]] [Nu 2]\n" ++
                            "> g\n" ++
                            "<3,<5|<2|0>>|2>\n" ++
                            "> canonize g\n" ++
                            "<3|2>\n" ++
                            separator

help "followsVoidRule" = putStr $ separator ++
                                  "Checks if the game follows the void rule\n" ++
                                  "Eg:\n" ++
                                  "> followsVoidRule (RE [Op [Nu 4] [Nu (-1)]] (-2))\n" ++
                                  "False\n" ++
                                  "> followsVoidRule (RE [Op [Nu 4] [Nu (-3)]] (-2))\n" ++
                                  "True\n" ++
                                  separator

help "latex" = putStr $ separator ++
                        "Convert a game into a latex description\n" ++
                        "Eg:\n" ++
                        "> latex (RE [Op [Nu 4] [Nu (-3)]] (-2))\n" ++
                        "\"$$\\<\\<4|-3\\>|\\emptyset^<-2>\\>$$\"\n" ++
                        separator

help "leftOp" = putStr $ separator ++
                        "Left options of game G. It returns the respective list of games\n" ++
                        "Eg:\n" ++
                        "> leftOp (RE [Op [Nu 4] [Nu (-3)]] (-2))\n" ++
                        "[<4|-3>]\n" ++
                        "> leftOp (LE 5 [Op [Nu 4] [Nu (-3)]])\n" ++
                        "[]\n" ++
                        separator

help "rightOp" = putStr $ separator ++
                          "Right options of game G. It returns the respective list of games\n" ++
                          "Eg:\n" ++
                          "> rightOp (LE 5 [Op [Op [Nu 10] [Nu 1]] [Nu (-3)]])\n" ++
                          "[<<10|1>|-3>]\n" ++
                          "> rightOp (RE [Op [Nu 4] [Nu (-3)]] (-2))\n" ++
                          "[]\n" ++
                          separator

help "ls" = putStr $ separator ++
                        "Left Stop of game G\n" ++
                        "Eg:\n" ++
                        "> let g = LE 5 [Op [Op [Nu 10] [Nu 1]] [Nu (-3)]]\n" ++
                        "> g\n<^5|<<10|1>|-3>>\n" ++
                        "> ls g\n" ++
                        "5.0\n" ++
                        separator

help "rs" = putStr $ separator ++
                        "Right Stop of game G\n" ++
                        "Eg:\n" ++
                        "> let g = LE 5 [Op [Op [Nu 10] [Nu 1]] [Nu (-3)]]\n" ++
                        "> g\n<^5|<<10|1>|-3>>\n" ++
                        "> rs g\n" ++
                        "1.0\n" ++
                        separator
help "ls_" = putStr $ separator ++
                        "Left Stop pass-allowed of game G\n" ++
                        "Eg:\n" ++
                        "> let g = LE 5 [Op [Op [Nu 10] [Nu 1]] [Nu (-3)]]\n" ++
                        "> g\n<^5|<<10|1>|-3>>\n" ++
                        "> ls_ g\n" ++
                        "5.0\n" ++
                        separator

help "rs_" = putStr $ separator ++
                        "Right Stop pass-allowed of game G\n" ++
                        "Eg:\n" ++
                        "> let g = LE 5 [Op [Op [Nu 10] [Nu 1]] [Nu (-3)]]\n" ++
                        "> g\n<^5|<<10|1>|-3>>\n" ++
                        "> rs_ g\n" ++
                        "1.0\n" ++
                        separator

                        
help "translate" = putStr $ separator ++
                          "Translate a game by the given number\n" ++
                          "Eg:\n" ++
                          "> let g = Op [Nu pi] [Nu (exp (1))]\n" ++
                          "<3.1416|2.7183>\n" ++
                          "> translate 5 g\n" ++
                          "<8.1416|7.7183>\n" ++
                          "> translate (-1e-3) g\n" ++
                          "<3.1406|2.7173>\n" ++
                          separator
                        
help "scgDiatic" = putStr $ separator ++
                        "Transform a diatic Conway's game into a scoring game\n" ++
                        "Notice that a diatic is a fraction like numerador/2^exponent\n" ++
                        "Egs: 1/4, 5/16, -7/32, 1/1, etc.\n" ++
                        "Everything else will produce an infinite Conway's game.\n" ++
                        "note: SCG stands for Short (ie, finite) Conway Game.\n" ++
                        "Eg:\n" ++
                        "> scgDiatic (2,8)\n" ++
                        "<<0|<0|<0|<0|^0>>>>|<<0|<0|<0|^0>>>|<0|<0|^0>>>>\n" ++
                        "> scgDiatic (0,1)\n" ++
                        "0\n" ++
                        separator

help "scgInt" = putStr $ separator ++
                        "Transform a integer Conway's game into a scoring game\n" ++
                        "Notice that a integer n is also the diatic n/2^0\n" ++
                        "note: SCG stands for Short (ie, finite) Conway Game.\n" ++
                        "Eg:\n" ++
                        "> scgInt 0\n" ++
                        "0\n" ++
                        "> scgInt (-5)\n" ++
                        "<^0|<^0|<^0|<^0|<^0|0>>>>>\n" ++
                        separator
                        
help "scgStar" = putStr $ separator ++
                        "Transform a Conway's star game *n into a scoring game\n" ++
                        "Def: * = *1 = <0|0> && *n = <*,*2,...,*(n-1)|*,*2,...,*(n-1)>, n>1\n" ++
                        "note: SCG stands for Short (ie, finite) Conway Game.\n" ++
                        "Eg:\n" ++
                        "> scgStar 1\n" ++
                        "<0|0>\n" ++
                        "> scgStar 2\n" ++
                        "<<0|0>|<0|0>>\n" ++
                        separator

help "zeta" = putStr $ separator ++
                        "Order preserving embedding for numbers (integers & diatics)\n" ++
                        "This can be used instead of scgInt and scgDiatic.\n" ++
                        "\'zeta n d\' is the representation of number n/d (d>0)\n" ++
                        "Eg:\n" ++
                        "> zeta 1 1\n" ++
                        "<0|^0>\n" ++
                        "> zeta 1 4\n" ++
                        "<0|<0|<0|^0>>>\n" ++
                        "> zeta (-1) 32\n" ++
                        "<<<<<<^0|0>|0>|0>|0>|0>|0>\n" ++
                        separator

help "invertible" = putStr $ separator ++
                        "Checks if a game G is invertible (G-G=0)\n" ++
                        "Eg:\n" ++
                        "> invertible (Nu 3)\n" ++
                        "True\n" ++
                        separator

help "lrp" = putStr $ separator ++
                        "Checks if a game is left-r-protected\n" ++
                        "Eg:\n" ++
                        "> lrp 2 (Nu 3)\n" ++
                        "True\n" ++
                        separator

help "down" = putStr $ separator ++
                       "Down, ie <*|0>\n" ++
                       separator

help "up"  = putStr $ separator ++
                      "Up, ie <0|*>\n" ++
                      separator

help "star"  = putStr $ separator ++
                        "Star, ie <0|0>\n" ++
                        separator
                        
help ">=." = putStr $ separator ++
                      "Checks if a game >= number\n" ++
                      "Eg:\n" ++
                      "> up >=. 0\n" ++
                      "True\n" ++
                      "> not (Nu 5 >=. 0)\n" ++
                      "False\n" ++
                      separator

help "<=." = putStr $ separator ++
                      "Checks if a game <= number\n" ++
                      "Eg:\n" ++
                      "> down <=. 0\n" ++
                      "True\n" ++
                      separator

help "<==" = putStr $ separator ++
                      "Checks if game1 <= game2 (returns false if not comparable)\n" ++
                      "Eg:\n" ++
                      "> up <== down\n" ++
                      "False\n" ++
                      "> Nu 5 <== LE (-4) [Nu 6]\n" ++
                      "False\n" ++
                      separator

help ">==" = putStr $ separator ++
                      "Checks if game1 >= game2 (returns false if not comparable)\n" ++
                      "Eg:\n" ++
                      "> up >== down\n" ++
                      "True\n" ++
                      "> Nu 5 >== LE (-4) [Nu 6]\n" ++
                      "False\n" ++
                      separator

help "===" = putStr $ separator ++
                      "Checks if game1 == game2 (returns false if not comparable)\n" ++
                      "Eg:\n" ++
                      "> up === up\n" ++
                      "True\n" ++
                      "> Nu 5 === LE (-4) [Nu 6]\n" ++
                      "False\n" ++
                      separator
                      
help ">=?" = putStr $ separator ++
                      "Tries to check if game1 >= game2, returning Nothing if not comparable\n" ++
                      "Eg:\n" ++
                      "> up >=? down\n" ++
                      "Just True\n" ++
                      "> Nu 5 >=? LE (-4) [Nu 6]\n" ++
                      "Nothing\n" ++
                      separator
                      
help "<=?" = putStr $ separator ++
                      "Tries to check if game1 <= game2, returning Nothing if not comparable\n" ++
                      "Eg:\n" ++
                      "> up <=? down\n" ++
                      "Just False\n" ++
                      "> Nu 5 <=? LE (-4) [Nu 6]\n" ++
                      "Nothing\n" ++
                      separator
                      
help "==?" = putStr $ separator ++
                      "Tries to check if game1 == game2, returning Nothing if not comparable\n" ++
                      "Eg:\n" ++
                      "> up ==? up\n" ++
                      "Just True\n" ++
                      "> Nu 5 ==? LE (-4) [Nu 6]\n" ++
                      "Nothing\n" ++
                      separator
                    
help "lop" = putStr $ separator ++
                        "Extract n-th left/right game\n" ++
                        "Eg:\n" ++
                        "> g\n" ++
                        "<<1|1,<1|1>>,<2|2>|<<<^4|4>|<4|^4>>|-5>>\n" ++
                        "> lop 1 $ g\n" ++
                        "<1|1,<1|1>>\n" ++
                        "> rop 2 . lop 1 $ g\n" ++
                        "<1|1>\n" ++
                        "> rop 1 . lop 2 $ g\n" ++
                        "2\n" ++
                        separator
                        
help "rop" = putStr $ separator ++
                        "Extract n-th left/right game\n" ++
                        "Eg:\n" ++
                        "> g\n" ++
                        "<<1|1,<1|1>>,<2|2>|<<<^4|4>|<4|^4>>|-5>>\n" ++
                        "> lop 1 $ g\n" ++
                        "<1|1,<1|1>>\n" ++
                        "> rop 2 . lop 1 $ g\n" ++
                        "<1|1>\n" ++
                        "> rop 1 . lop 2 $ g\n" ++
                        "2\n" ++
                        separator

help "showRaw" = putStr $ separator ++
                      "Show the raw Haskell structure of a game\n" ++
                      "Eg:\n" ++
                      "> showRaw up\n" ++
                      "Op [Nu 0] [Op [Nu 0] [Nu 0]]\n" ++
                      "> let pos = fromData (-1) [\"lr\",\"rl\"]::Kobber\n" ++
                      "> let g = toGame pos\n" ++
                      "> g\n" ++
                      "<<-1|<-1|-1>>|<<-1|-1>|-1>>\n" ++
                      "> showRaw g\n" ++
                      "Op [Op [Nu (-1)] [Op [Nu (-1)] [Nu (-1)]]] [Op [Op [Nu (-1)] [Nu (-1)]] [Nu (-1)]]\n" ++
                      separator

                      
help "test" = putStr $ separator ++
                        "Test an unary proposition using random generated games\n" ++
                        "The first argument is a lambda expression, while the 2nd is the required number of tests\n" ++
                        "Eg:\n" ++
                        "> test (\\x -> x >=. 0 || (-x) >=. 0) 40\n" ++
                        "Found a counter example:\n" ++
                        "Game: <^4.7568|<^2.3046|-6.9642>,<^3.8723|-3.4964>>\n\n" ++
                        "> test (\\x -> ls x == ls_ x) 100\n" ++
                        ".............................Found a counter example:\n" ++
                        "Game: <<^-0.8732|-0.0251>,<4.8932|^-1.8782>|^-2.9849>\n\n" ++
                        "> test (\\x -> ls x >= ls_ x) 75\n" ++
                        "...........................................................................Tests finished!\n" ++
                        "\nMore on lambda-expressions: www.cs.bham.ac.uk/~vxs/teaching/Haskell/handouts/lambda.pdf\n" ++
                        separator
                        
help "test2" = putStr $ separator ++
                        "Test a binary proposition using random generated games\n" ++
                        "The first two arguments are lambda expressions, while the 3rd is the required number of tests\n" ++
                        "Eg:\n" ++
                        "> test2 (\\x y -> y <=. ls_ (x+x)) 20\n" ++
                        "Found a counter example:\n" ++
                        "Game 1: <4.5708|^-0.7437>\n" ++
                        "Game 2: <4.8928|-11.9910>\n\n" ++
                        "> test2 (\\x y -> ls_ x >= ls_ y || not(ls_ x >= ls_ y)) 20\n" ++
                        "....................Tests finished\n" ++
                        "\nMore on lambda-expressions: www.cs.bham.ac.uk/~vxs/teaching/Haskell/handouts/lambda.pdf\n" ++
                        separator

-- help "" = putStr $ separator ++
                        -- "\n" ++
                        -- "Eg:\n" ++
                        -- "> \n" ++
                        -- "\n" ++
                        -- separator

help _ = putStr $ "Unkown command. Please check \'commands\'\n"
                
commands :: IO ()
commands = do
            putStr $ separator ++
                     "commands available:\n" ++
                     "\t+, -, >=., <=., >==, <==, ===, >=?, <=?, ==?, canonize, conjugate, down, followsVoidRule,\n" ++
                     "\tinvertible, latex, leftOp, lop, lrp, ls, ls_, rightOp, rop, rs, rs_, translate, scgDiatic,\n" ++
                     "\tscgInt, scgStar, showRaw, star, test, test2, up, zeta\n\n" ++
                     "For more information use: help \"command\". Egs:\n" ++
                     "> help \"zeta\"\n" ++
                     "> help \"+\"\n" ++
                     separator     
                     
                     
--------------------------------
--------------------------------
-- Help facilities for Position (abstract module for games)

help_pos "points" = putStr $ 
                      separator ++
                      "Returns the current points of a position\n" ++
                      "Eg:\n" ++
                      "> let pos = fromData 1 [\"lr.\",\"rl.\",\"l.r\"]::Kobber\n"++
                      "> points pos\n" ++
                      "1.0\n" ++
                      separator
                      
help_pos "toText" = putStr $ 
                      separator ++
                      "Returns an one line description of a position\n" ++
                      "Eg:\n" ++
                      "> let pos = fromData 1 [\"lr.\",\"rl.\",\"l.r\"]::Kobber\n"++
                      "> toText pos\n" ++
                      "\"lr.\\nrl.\\nl.r\\n1\"\n" ++
                      separator

help_pos "toGame" = putStr $ 
                      separator ++
                      "Returns the game value of a position\n" ++
                      "Eg:\n" ++
                      "> let pos = fromData (-1) [\"lr\",\"rl\"]::Kobber\n"++
                      "> toGame pos\n" ++
                      "{{-1|{-1|-1}}|{{-1|-1}|-1}}\n" ++
                      separator

help_pos "fromRaw" = putStr $ 
                      separator ++
                      "Parse a string from an eval report and make the respective position\n" ++
                      "Eg:\n" ++
                      "> evalKobber \"kobber1.txt\"\n"++
                      "-- Read: \"1\\nlr.\\nrl.\\nl.r\"\n...\n"++
                      "> let pos = fromRaw \"1\\nlr.\\nrl.\\nl.r\"::Kobber\n"++
                      "> present pos\n" ++
                      "lr.\nrl.\nl.r\n1 points\n" ++
                      "> let g = toGame pos\n" ++
                      "> ls_ g\n" ++
                      "1.0\n" ++
                      separator
                      
help_pos "fromData" = putStr $ 
                      separator ++
                      "Given a value and a list of rows, create the respective game\n" ++
                      "Eg:\n" ++
                      "> let pos = fromData (-1) [\"lr\",\"rl\"]::Kobber\n"++
                      "> toGame pos\n" ++
                      "{{-1|{-1|-1}}|{{-1|-1}|-1}}\n" ++
                      separator
                      
help_pos "moves" = putStr $ 
                      separator ++
                      "Returns the next moves given a position and a player\n" ++
                      "Eg:\n" ++
                      "> let pos = fromData 0 [\"lr\",\"rl\",\"..\"]::Kobber\n"++
                      "> presents $ moves pos Left\n" ++
                      ".r\nll\n..\n0 points\n.l\nrl\n..\n0 points\n.r\n.l\nl.\n1 points\nll\nr.\n..\n0 points\nlr\nl.\n..\n0 points\n" ++
                      separator
 
help_pos "presents" = putStr $ 
                      separator ++
                      "Shows a list of positions\n" ++
                      "Eg:\n" ++
                      "> let pos1 = fromData 0 [\"lr\",\"rl\",\"..\"]::Kobber\n"++
                      "> let pos2 = fromData 2 [\"rr\",\"xx\"]::Kobber\n"++
                      "> presents $ [pos1,pos2]\n" ++
                      "lr\nrl\n..\n0 points\nrr\nxx\n2 points\n" ++
                      separator
                      
help_pos "present" = putStr $ 
                      separator ++
                      "Shows a position\n" ++
                      "Eg:\n" ++
                      "> let pos = fromData 0 [\"lr\",\"rl\",\"..\"]::Kobber\n"++
                      "> present pos\n" ++
                      "lr\nrl\n..\n0 points\n" ++
                      separator

help_pos "eval" = putStr $ 
                      separator ++
                      "Eval<G>: Reads a position from game G from a text file and shows a report\n" ++
                      "Eg:\n" ++
                      "> evalKobber \"kobber1.txt\"\n"++
                      "-- Read: \"1\\nlr.\\nrl.\\nl.r\"\n-- Board\n1\nlr.\nrl.\nl.r\n-- Position Value:\n<<2|<1|<2|1>>>,<3|<<1|0>|1>>|<0,<<1|0>,<1|<2|1>>|<0|-1>,<<-1|-2>|-1>>|<<0|-1>|0>>,<<1|0>|<<0|-1>|0>>>\n" ++
                      "\nIn this eg, the text file \'kobber1.txt\' includes the following:\n \n" ++
                      "   1\n   lr.\n   rl.\n   l.r\n\n" ++
                      "All text files start with a number representing the current points, while the remaining file has a position row for each line.\n" ++
                      "There's also available evalDots for Dots'n'Boxes, with the same functionality. For an eg type\n \n> evalDots \"dots1.txt\"\n  (the report will appear...)\n" ++
                      "> let dots = fromRaw \"1\\nx...xx\\nxx.xxxx\\nxxx.xx\" :: Dots\n" ++
                      "> present dots\n" ++
                      "+-+ + + +-+-+\n|#|   | |#|#|\n+-+-+-+ +-+-+\n1 points\n" ++
                      "> ls (toGame dots)\n2.0\n" ++
                      separator
                      
help_pos "readBoard" = putStr $ separator ++
                        "Read the raw data from the file presenting some useful information\n" ++
                        "Eg:\n" ++
                        "> readBoard \"dots1.txt\"\n" ++
                        "-- Read: \"1\\nx...xx\\nxx.xxxx\\nxxx.xx\"\n-- Board\n1\n x . . . x x \nx x . x x x x\n x x x . x x \n-- Position Value:\n(1.0,[\"x...xx\",\"xx.xxxx\",\"xxx.xx\"])\n" ++
                        separator
               
-- help_pos "" = putStr $ separator ++
                        -- "\n" ++
                        -- "Eg:\n" ++
                        -- "> \n" ++
                        -- "\n" ++
                        -- separator
                        
commands_pos :: IO ()
commands_pos = do
            putStr $ separator ++
                     "commands available for positions:\n" ++
                     "\teval, fromData, fromRaw, moves, points, present, presents, readBoard, toGame, toText\n" ++
                     "For more information use: help \"command\". Egs:\n" ++
                     "> help_pos \"points\"\n" ++
                     separator                              