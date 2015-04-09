{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--module Lispell (Expression (),cons) where
import qualified Data.Map as M
import           Control.Monad.IO.Class
import           Control.Monad.State as Monad.State
import           Control.Concurrent
import           System.IO.Unsafe
import           Text.ParserCombinators.Parsec as Parsec hiding (spaces)

data Procedure = Procedure {
  environment :: Environment,
  parameterNames :: [String],
  name :: String
}

data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Null
                | Cell Expression Expression deriving (Show)

data Environment = Environment {
  bindings :: M.Map String Expression,
  parent :: Maybe Environment
}

type EnvironmentState = Monad.State.State Environment

-------------------------------------------------------------------------------
-- | Environment

setEnvironment :: String -> Expression -> Environment -> Environment
setEnvironment key expr (Environment bindings_ parent_) = Environment (M.insert key expr bindings_) parent_

getEnvironment :: String -> Environment -> Maybe Expression
getEnvironment key (Environment bindings_ parent_) = case M.lookup key bindings_ of
                                                       Just expr -> Just expr
                                                       Nothing -> case parent_ of
                                                                    Nothing -> Nothing
                                                                    Just p -> getEnvironment key p

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty Nothing

-------------------------------------------------------------------------------
-- | Lisp AST parsers

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

parseQuoted :: Parser Expression
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ Cell (Atom "quote") x

parseExpr :: Parser Expression
parseExpr = parseAtom <|> parseString <|> parseInteger <|> parseReal <|> parseQuoted
                      <|> do char '('
                             x <- (try parseList) <|> parseDottedList
                             char ')'
                             return x

-------------------------------------------------------------------------------
-- | Lisp core

-- reads lisp code into an AST
lispRead :: String -> Expression
lispRead input = case parse parseExpr "lisp" input of
    Left err -> lispError $ "No match: " ++ show err
    Right val -> val

-- evaluates lisp expressions
lispEval :: Expression -> Expression
lispEval expr = evalState (lispEval' expr) emptyEnvironment

lispEval' :: Expression -> EnvironmentState Expression
lispEval' expr = do
  env <- get
  case expr of
    (Atom a) -> case getEnvironment a env of
                  Just lkup -> return $ lispEval $ cons lkup $ cdr expr
                  Nothing -> lispError "Atom not found in environment."
    (Integer a) -> return (Integer a)
    (String a) -> return (String a)
    (Real a) -> return (Real a)
    (Bool a) -> return (Bool a)
    (Cell (Atom "define") e) -> return $ car e -- TODO: modify env
    -- (Cell "begin" e) ->
    -- (Cell "fork" e) ->
    -- (Cell "lambda" e) ->
    -- (Cell a e) -> -- TODO: Look up in primitive and in the env
    e -> do
      return e
      
repl :: String -> IO ()
repl prompt = do
  putStrLn prompt
  input <- getLine
  putStrLn $ show $ lispEval $ lispRead input
  repl prompt
  
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

-- returns true if the expression is a cons list
isConsList :: Expression -> Bool
isConsList Null = True
isConsList (Cell a Null) = True
isConsList (Cell _ b) = isConsList b
isConsList a = False

lispError :: [Char] -> t
lispError a = error a

-- evals each expression in a Lisp list and returns the result of the last eval
lispBegin :: Expression -> Expression
lispBegin (Cell a Null) = lispEval a
lispBegin (Cell a b) = seq (lispEval a) (lispBegin b)
lispBegin a = lispEval a

-- evals the first/only argument on a new native OS thread, not waiting for 
-- the thread to exit
-- lispFork :: Expression -> ThreadId
-- lispFork a = unsafePerformIO $ forkOS $ do
--   (lispBegin a)
--   yield

-- evals the first/only argument on a new native OS thread, waiting for it to
-- finish
lispForkWait :: Expression -> Expression
lispForkWait a = unsafePerformIO $ do
    m <- newEmptyMVar
    forkOS $ putMVar m $ lispBegin a
    r <- takeMVar m
    return r

-- (apply + 1 2 3 4 5)
-- lispApply :: Expression -> Expression -> Expression -> Expression
-- lispApply (Atom exprName)

-------------------------------------------------------------------------------
-- | Main (temporary)
  
main = do
  let code = "(this)"
  print $ lispRead code
