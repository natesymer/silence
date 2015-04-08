{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--module Hisk (Expression (),cons) where
  
--import           Control.Applicative
import qualified Data.Text as T
import           Data.Char

import System.IO.Unsafe
import Control.Monad.IO.Class

import Text.ParserCombinators.Parsec as Parsec hiding (spaces)

data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Null
                | Cell Expression Expression deriving (Show)

-- uncomment to have an AST be turned back into code
-- instance Show Expression where
--   show = lispShow

-- -- properly parsed AST
-- LispCell
--   (LispSymbol "this")
--   (LispCell
--     (LispSymbol "is")
--     (LispCell
--       (LispCell
--         (LispSymbol "that")
--         (LispCell
--           (LispSymbol "foo")
--           LispNull))
--       LispNull))

--
-- Cell
--   (Atom "this")
--   (Cell
--     (Atom "is")
--     (Cell
--       (Cell
--         (Atom "that")
--         (Cell
--           (Atom "foo")
--           Null))
--     Null))

-------------------------------------------------------------------------------
-- | Lisp AST

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser Expression
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser Expression
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom
                          
parseInteger :: Parser Expression
parseInteger = fmap (Integer . read) $ many1 digit

parseReal :: Parser Expression
parseReal = fmap (Real . read) $ many1 digit

parseList :: Parser Expression
parseList = fmap toConsList $ sepBy parseExpr spaces

parseDottedList :: Parser Expression
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ cons (head h) t
   -- return $ cons (Atom "asdf") (Integer 1)

parseQuoted :: Parser Expression
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ Cell (Atom "quote") x

parseExpr :: Parser Expression
parseExpr = parseAtom
        <|> parseString
        <|> parseInteger
        <|> parseReal
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
               
lispRead :: String -> Expression
lispRead input = case parse parseExpr "lisp" input of
    Left err -> lispError $ "No match: " ++ show err
    Right val -> val
    
lispShow :: Expression -> String
lispShow (Atom x) = x
lispShow (String x) = "\"" ++ x ++ "\""
lispShow (Integer x) = show x
lispShow (Real x) = show x
lispShow Null = "'()"
lispShow (Bool True) = "#t"
lispShow (Bool False) = "#f"
lispShow (Cell a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"

-------------------------------------------------------------------------------
-- | Lisp evaluator

-------------------------------------------------------------------------------
-- | Lisp primitive functions

cons :: Expression -> Expression -> Expression
cons a b = Cell a b

car :: Expression -> Expression
car (Cell x y) = x
car x          = error $ "Cannot take the car of " ++ lispShow x

cdr :: Expression -> Expression
cdr (Cell x y) = y
cdr x          = error $ "Cannot take the cdr of " ++ lispShow x

append :: Expression -> Expression -> Expression
append _ Null = error "Cannot append null to list."
append Null b = cons b Null
append (Cell x xs) b = cons x (append xs b)
append a b = cons a (cons b Null)

toConsList :: [Expression] -> Expression
toConsList (x:xs) = foldl append (cons x Null) xs
toConsList [] = Null

lispError :: [Char] -> t
lispError a = error a

main = do
  let code = "(this is (that foo))"
  print $ lispRead "(fuck . '())"

-------------------------------------------------------------------------------
-- | Repl

repl :: String -> IO ()
repl prompt = do
  putStrLn prompt
  input <- getLine
  putStrLn input -- TODO: Evaluate
  repl prompt
