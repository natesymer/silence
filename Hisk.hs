{-# LANGUAGE OverloadedStrings #-}

module Hisk (hiskRepl, hiskEval) where

data HiskObject = String | Integer | HiskCell
type HiskCell = (HiskObject, Maybe HiskObject)

-- data HiskObject = String | Integer | HiskCell {
--    car :: HiskObject,
--    cdr :: HiskObject
-- }
  
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

-- hiskRead :: String -> HiskObject
-- hiskRead input = splitExpr $ readExpr input 1 -- (tail $ init input) would work for readExprToplevel

hiskRead :: String -> HiskObject
hiskRead input = hiskRead' input 1 -- (tail $ init input) would work for readExprToplevel

hiskRead' :: String -> Integer -> HiskObject
hiskRead' input level = (carExpr, hiskRead' remainder)
  where
    expr = readExpr input level
    carExpr = readCar expr 0
    remainder = (drop (length carExpr) expr)

--
-- hiskRead' :: String -> Integer -> HiskObject
-- hiskRead' input level = (fst expr, hiskRead' $ snd expr)
--   where
--     let expr = splitExpr $ readExpr input level

-------------------------------------------------------------------------------
-- | helper functions

parseExpr :: String -> HiskObject
parseExpr expr = 0 :: HiskObject -- TODO: write this

-------------------------------------------------------------------------------
-- | readCar - reads the cons part of a form expression

readCar' :: String -> Integer -> String -> HiskCell
readCar' "" _ accum = (accum, Nothing)
readCar' (x:xs) 0 accum -- breakable
  | x == "("  = readCar' xs 1 accum
  | x == ")"  = (accum, Just $ readCar' xs 0 "")
  | x == " "  = (accum, Just $ readCar' xs 0 "")
  | otherwise = readCar' xs 0 (accum ++ x)
readCar (x:xs) depth accum -- non-breakable
  | x == "("  = readCar' xs (depth + 1)
  | x == ")"  = readCar' xs (depth - 1)
  | otherwise = readCar' xs depth (accum ++ x)

-- readCar :: String -> Integer -> String
-- readCar "" _ = ""
-- readCar (x:xs) 0 -- breakable
--   | x == "("  = readCar xs 1
--   | x == ")"  = ""
--   | x == " "  = ""
--   | otherwise = x:(readCar xs 0)
-- readCar (x:xs) depth -- non-breakable
--   | x == "("  = readCar xs (depth + 1)
--   | x == ")"  = readCar xs (depth - 1)
--   | otherwise = x:(readCar xs depth)

-------------------------------------------------------------------------------
-- | splitExpr - Splits a line of code into a series of cons cells

-- splitExpr :: String -> HiskCell
-- splitExpr str = splitExpr str ""
--
-- splitExpr :: String -> String -> HiskCell
-- splitExpr "" str = str
-- splitExpr (x:xs) accum
--   | x == " " = ("", splitExpr xs 1)
--   | otherwise = do
--       let split = (splitExpr xs)
--       (x:(fst split), snd split)

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

cons :: HiskObject -> HiskObject -> HiskCell
cons one two = (one, Just two) :: HiskCell

car :: HiskCell -> Maybe HiskObject
car cell = (fst cell)

cdr :: HiskCell -> HiskObject
cdr cell = (snd cell)