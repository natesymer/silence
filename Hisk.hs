{-# LANGUAGE OverloadedStrings #-}

module Hisk (hiskRepl, hiskEval) where
  
import Data.Maybe

data HiskObject = String | Integer | Double | HiskCell
type HiskCell = (Maybe HiskObject, Maybe HiskObject)

-------------------------------------------------------------------------------
-- | helpers

-- from http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Haskell
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

-------------------------------------------------------------------------------
-- | Repl 
  
hiskRepl :: String -> IO ()
hiskRepl prompt = do
  putStrLn prompt
  input <- getLine
  putStrLn $ hiskEval input
  hiskRepl prompt
  
-------------------------------------------------------------------------------
-- | Core language functions

-- RETURNS: evaluated result
hiskEval :: String -> String
hiskEval input = show $ hiskRead input

hiskRead :: String -> HiskObject
hiskRead input = hiskRead' input 1 -- (tail $ init input) would work for readExprToplevel

hiskRead' :: String -> Integer -> HiskObject
hiskRead' input level = (carExpr, Just $ hiskRead' remainder)
  where
    expr = readExpr input level
    carExpr = readCar expr 0
    remainder = (drop (length carExpr) expr)

-------------------------------------------------------------------------------
-- | parseValue - parse a Lisp value.

parseValue :: String -> Maybe HiskObject
parseValue "()" = Nothing
parseValue expr -- = (Just 0) :: HiskObject -- TODO: write this
  | isDouble expr      = Just (read expr) :: Double
  | isInteger expr     = Just (read expr) :: Integer
  | (head expr) == "'" = Just (Just "quote", Just $ tail expr)
  | otherwise          = Just expr
  
-------------------------------------------------------------------------------
-- | readCar - reads the car part of a form expression in string form

readCar' :: String -> Integer -> String -> Maybe HiskCell
readCar' "" _ accum = Just (parseValue accum, Nothing)
readCar' (x:xs) 0 accum -- breakable
  | x == "("  = readCar' xs 1 accum
  | x == ")"  = Just (parseValue accum, readCar' xs 0 "")
  | x == " "  = Just (parseValue accum, readCar' xs 0 "")
  | otherwise = readCar' xs 0 (accum ++ x)
readCar (x:xs) depth accum -- non-breakable
  | x == "("  = readCar' xs (depth + 1)
  | x == ")"  = readCar' xs (depth - 1)
  | otherwise = readCar' xs depth (accum ++ x)

-------------------------------------------------------------------------------
-- | readExpr - Collects chars after depth open parens until a paren that closes
-- |            the first paren or the end of the string is reached.
-- |            
-- |            Depth is numbered from 0 to infinity. A depth of 0 is the toplevel,
-- |            a depth of 1 in "(func arg)" is "func arg", level two
-- |            of (func (funcTwo arg)) is "funcTwo arg", etc

readExpr :: String -> Integer -> String
readExpr input depth = readExpr' input depth 0

readExpr' :: String -> Integer -> Integer -> String
readExpr' "" _ _ = "" -- heh. Empty strings are easy :P
readExpr' (x:xs) targetDepth depth
  | x == ")" && depth == targetDepth = ""                                    -- we stop before the ) at a depth targetDepth
  | depth == targetDepth             = x:(readExpr' xs targetDepth depth)    -- accumulate characters
  | x == ")"                         = readExpr' xs targetDepth (depth - 1)  -- not the last quote on this level
  | x == "("                         = readExpr' xs targetDepth (depth + 1)  -- open a new level
  | otherwise                        = readExpr' xs targetDepth depth        -- skip characters
  
-------------------------------------------------------------------------------
-- | Lisp primitive functions 

cons :: Maybe HiskObject -> Maybe HiskObject -> HiskCell
cons one two = (one, two) :: HiskCell

car :: HiskCell -> Maybe HiskObject
car cell = (fst cell)

cdr :: HiskCell -> Maybe HiskObject
cdr cell = (snd cell)