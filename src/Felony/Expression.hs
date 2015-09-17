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

type Environment = [HashMap String Expression]
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
showExpr Null = showCell True Null
showExpr c@(Cell _ _) = showCell True c
showExpr (Bool True)  = "#t"
showExpr (Bool False) = "#f"
showExpr (Procedure _ argnames expressions) = mconcat $ "<<procedure: ":argnames

-- TODO: This sucks
-- Bool is hasLeadingParen
showCell :: Bool -> Expression  -> String
showCell True    Null                  = "()"
showCell False   Null                  = ")"
showCell True  c@(Cell a Null)         = '(':(showCell False c)
showCell False   (Cell a Null)         = mconcat [show a, showCell False Null]
showCell True  c@(Cell a b@(Cell _ _)) = '(':(showCell False c)
showCell False   (Cell a b@(Cell _ _)) = mconcat [show a, " ", showCell False b]
showCell _       (Cell a b)            = mconcat ["(", show a, " . ", show b, ")"]
showCell _       _                     = error "Invalid cons cell."

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
isConsList (Cell a b) = isConsList b
isConsList _ = False

append :: Expression -> Expression -> Expression
append a b = f a b
  where
    f Null b = Cell b Null
    f (Cell x xs) b = Cell x (append xs b)
    f a b = Cell a (Cell b Null)