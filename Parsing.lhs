Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


> module Parsing where
> 
> import Data.Char
> import Control.Monad
> import Control.Applicative hiding (many)

> infixr 5 +++

The monad of parsers
--------------------

> newtype Parser a              =  P (String -> [(a,String)])
>
> instance Monad Parser where
>    return v                   =  P (\inp -> [(v,inp)])
>    p >>= f                    =  P (\inp -> case parse p inp of
>                                                  []        -> []
>                                                  [(v,out)] -> parse (f v) out)
> 
> instance MonadPlus Parser where
>    mzero                      =  P (\inp -> [])
>    p `mplus` q                =  P (\inp -> case parse p inp of
>                                                []        -> parse q inp
>                                                [(v,out)] -> [(v,out)])

-- necessary, unless it would become an error in GHC 7.10, under the Applicative-Monad Proposal

> instance Functor Parser where  
>    fmap f p = P (\inp -> case parse p inp of
>                            []        -> []
>                            [(v,out)] -> [(f v, out)])

-- TODO: how to make this? Anyway I don't use Parsers as Applicatives or Alternatives
    
> instance Applicative Parser where
>   pure v                     = P (\inp -> [(v,inp)])
>   f <*> p                    = P (\inp -> case parse p inp of
>                                                  []        -> []
>                                                  [(v,out)] -> [])

> instance Alternative Parser where
>   empty                      = P (\inp -> [])
>   (<|>) p1 p2                = p1 >> p2

Basic parsers
-------------

> failure                       :: Parser a
> failure                       =  mzero
>
> item                          :: Parser Char
> item                          =  P (\inp -> case inp of
>                                                []     -> []
>                                                (x:xs) -> [(x,xs)])
> 
> parse                         :: Parser a -> String -> [(a,String)]
> parse (P p) inp               =  p inp

Choice
------

> (+++)                         :: Parser a -> Parser a -> Parser a
> p +++ q                       =  p `mplus` q

Derived primitives
------------------

> sat                           :: (Char -> Bool) -> Parser Char
> sat p                         =  do x <- item
>                                     if p x then return x else failure
> 
> digit                         :: Parser Char
> digit                         =  sat isDigit
> 
> lower                         :: Parser Char
> lower                         =  sat isLower
> 
> upper                         :: Parser Char
> upper                         =  sat isUpper
> 
> letter                        :: Parser Char
> letter                        =  sat isAlpha
> 
> alphanum                      :: Parser Char
> alphanum                      =  sat isAlphaNum
> 
> char                          :: Char -> Parser Char
> char x                        =  sat (== x)
> 
> string                        :: String -> Parser String
> string []                     =  return []
> string (x:xs)                 =  do char x
>                                     string xs
>                                     return (x:xs)
> 
> many                          :: Parser a -> Parser [a]
> many p                        =  many1 p +++ return []
> 
> many1                         :: Parser a -> Parser [a]
> many1 p                       =  do v  <- p
>                                     vs <- many p
>                                     return (v:vs)
> 
> ident                         :: Parser String
> ident                         =  do x  <- lower
>                                     xs <- many alphanum
>                                     return (x:xs)
> 
> nat                           :: Parser Int
> nat                           =  do xs <- many1 digit
>                                     return (read xs)
>
> int                           :: Parser Int
> int                           =  do 
>                                     do
>                                       char '-'
>                                       n <- nat
>                                       return (-n)
>                                     +++
>                                     nat
>
> space                         :: Parser ()
> space                         =  do many (sat isSpace)
>                                     return ()


> comment                       = do
>                                   string "--"
>                                   many (sat (/= '\n'))
>                                   return ()

Ignoring spacing
----------------

> token                         :: Parser a -> Parser a
> token p                       =  do space
>                                     v <- p
>                                     space
>                                     return v
> 
> identifier                    :: Parser String
> identifier                    =  token ident
> 
> natural                       :: Parser Int
> natural                       =  token nat
> 
> integer                       :: Parser Int
> integer                       =  token int
>
> symbol                        :: String -> Parser String
> symbol xs                     =  token (string xs)

------------------------------------

{- eg (Hutton, chp.8):
  expr   ::= term (+ expr | e)
  term   ::= factor (* term | e)
  factor ::= (expr) | nat
  nat    ::= 0 | 1 | 2 | · · ·
-}
 
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             +++ 
             return t
          
term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
             +++ 
             return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ 
         natural
         
eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [ ])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [ ]        -> error "invalid input"         