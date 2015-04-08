{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--module Hisk (Expression (),cons) where
  
import           Control.Applicative
import qualified Data.Text as T
import           Data.Char

import System.IO.Unsafe

data Expression = LispSymbol String
                | LispString String
                | LispInteger Integer
                | LispReal Double
                | LispNull
                | LispCell Expression Expression deriving (Show)

-- -- uncomment to have an AST be turned back into code
-- instance Show Expression where
--   show = lispShow

-------------------------------------------------------------------------------
-- | Lisp AST

isParen :: Char -> Bool
isParen '(' = True
isParen ')' = True
isParen _   = False

splitList :: (Eq a) => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep lst
  | (head lst) == sep = splitList sep $ tail lst
  | otherwise = part:(splitList sep $ drop (1 + (length part)) lst)
                  where
                    part = (takeWhile ((/=) sep) lst)

lispShow :: Expression -> String
lispShow (LispSymbol x) = x
lispShow (LispString x) = "\"" ++ x ++ "\""
lispShow (LispInteger x) = show x
lispShow (LispReal x) = show x
lispShow LispNull = "'()"
lispShow (LispCell a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"

trim :: String -> String
trim s = reverse $ dropWhile (\c -> c == ' ') $ reverse s

preproc :: String -> String
preproc ('(':xs) = " ( " ++ (preproc xs)
preproc (')':xs) = " ) " ++ (preproc xs)
preproc (x:xs) = x:preproc xs
preproc a = a

tokenize :: String -> [String]
tokenize str = splitList ' ' $ trim $ preproc str

-- group :: [String] -> Expression
-- group ("(":xs) = map (coerce . group) $ takeWhile (\t -> t /= ")") xs
-- group ("(":"(":xs) = append (group (takeWhile (\t -> t /= ")" xs)) (group (dropWhile (\t -> t /= ")" xs))
-- group (")":xs) = lispError "unexpected )"
-- group (x:xs) = coerce x:group xs

lispRead :: [String] -> Expression
lispRead [] = lispError "No tokens to read"
lispRead ("(":xs) = if null remaining then (toConsList coerced) else (toConsList (coerced ++ [lispRead remaining])) 
                      where
                        raw = takeWhile (\t -> t /= "(") xs
                        coerced = map coerce raw
                        remaining = init $ drop (length raw) raw --tail $ dropWhile (not . isParen) (init xs)     
lispRead x = toConsList $ map coerce x

coerce :: String -> Expression
coerce "" = error "Cannot parse empty token."
coerce "'()" = LispNull
coerce s@(x:xs)
  | (last s) == x && x == '"' = LispString $ tail $ init s
  | x == '\'' = LispCell (LispSymbol "quote") (lispRead $ tokenize xs)
  | isInteger s = LispInteger $ read s
  | isDouble s = LispReal $ read s
  | otherwise = LispSymbol s
  
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

-------------------------------------------------------------------------------
-- | Lisp evaluator

-------------------------------------------------------------------------------
-- | Lisp primitive functions

cons :: Expression -> Expression -> Expression
cons a b = LispCell a b

car :: Expression -> Expression
car (LispCell x y) = x
car x              = error $ "Cannot take the car of " ++ lispShow x

cdr :: Expression -> Expression
cdr (LispCell x y) = y
cdr x              = error $ "Cannot take the cdr of " ++ lispShow x

append :: Expression -> Expression -> Expression
append _ LispNull = error "Cannot append null to list."
append LispNull b = cons b LispNull
append (LispCell x xs) b = cons x (append xs b)
append a b = cons a (cons b LispNull)

toConsList :: [Expression] -> Expression
toConsList (x:xs) = foldl append (cons x LispNull) xs

lispError :: [Char] -> t
lispError a = error a

main = do
  let code = "(this is (that foo))"
  putStrLn $ "Code: " ++ code
  putStrLn $ "Eval'd: " ++ (show $ lispRead $ tokenize code)
  --print $ lispRead "(this 1)"
  --print $ lispRead "(this '(shits real))"

-------------------------------------------------------------------------------
-- | Repl

repl :: String -> IO ()
repl prompt = do
  putStrLn prompt
  input <- getLine
  putStrLn input -- TODO: Evaluate
  hiskRepl prompt
