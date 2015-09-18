{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Felony.Expression
(
  Expression(..),
  Environment,
  isConsList,
  fromConsList,
  toConsList,
  showExpr
)
where

import Data.HashMap.Strict (HashMap)

type Environment = [HashMap String Expression] -- deriving (Eq, Show)

data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Procedure Environment [String] [Expression]
                | Null
                | Cell Expression Expression deriving (Eq, Show)
                
-- TODO: Rewrite show as as builders
                
-- TODO: Uncomment
-- instance Show Expression where
--   show = showExpr

showExpr :: Expression -> String
showExpr (Cell (Atom "quote") (Cell e Null)) = "'" ++ show e
showExpr (Atom x) = x
showExpr (String x) = "\"" ++ x ++ "\""
showExpr (Integer x) = show x
showExpr (Real x) = show x
showExpr Null = showCell Null
showExpr c@(Cell _ _) = showCell c
showExpr (Bool True)  = "#t"
showExpr (Bool False) = "#f"
showExpr (Procedure _ argnames _) = mconcat $ "<<procedure: ":argnames

showCell :: Expression -> String
showCell x = "(" ++ f x ++ ")"
  where
    f Null = "" 
    f (Cell a Null) = showExpr a
    f (Cell a b@(Cell _ _)) = mconcat [showExpr a, " ", f b]
    f (Cell a b) = mconcat [showExpr a, " . ", showExpr b]
    f _ = error "Invalid list."
    

toConsList :: [Expression] -> Expression
toConsList [] = Null
toConsList (x:xs) = foldl append (Cell x Null) xs

fromConsList :: Expression -> [Expression]
fromConsList e = f e []
  where
    f Null accum = accum
    f (Cell a b) accum = f b (a:accum)
    f s _ = error $ (show s) ++ " is not a cons list."

isConsList :: Expression -> Bool
isConsList Null = True
isConsList (Cell _ b) = isConsList b
isConsList _ = False

append :: Expression -> Expression -> Expression
append a b = f a b
  where
    f Null xpr = Cell xpr Null
    f (Cell x xs) bxpr = Cell x (append xs bxpr)
    f axpr bxpr = Cell axpr (Cell bxpr Null)