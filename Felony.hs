{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--module Lispell (Expression (),cons) where
import qualified Data.Map as M
import           Control.Monad.IO.Class
import           Control.Monad.State as Monad.State
import           Control.Concurrent
import           System.IO.Unsafe
import           Text.ParserCombinators.Parsec as Parsec hiding (spaces)

data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Procedure Environment [String] [Expression]
                | Null
                | Cell Expression Expression deriving (Show)

data Environment = Environment {
  bindings :: M.Map String Expression,
  parent :: Maybe Environment
}

type EnvironmentState = Monad.State.State Environment

-------------------------------------------------------------------------------
-- | Environment

setEnvironment :: String -> Environment -> Expression -> Environment
setEnvironment key (Environment bindings_ parent_) expr = Environment (M.insert key expr bindings_) parent_

getEnvironment :: String -> Environment -> Maybe Expression
getEnvironment key (Environment bindings_ parent_) = case M.lookup key bindings_ of
                                                       Just expr -> Just expr
                                                       Nothing -> case parent_ of
                                                                    Nothing -> Nothing
                                                                    Just p -> getEnvironment key p
                                                                    
zipEnvironment :: [String] -> [Expression] -> Environment
zipEnvironment keys exprs = Environment (fromList $ zip keys exprs) Nothing

extendEnvironment :: Environment -> Environment -> Environment
extendEnvironment parent (Environment bindings Nothing) = Environment bindings $ Just parent
extendEnvironment _ _ = lispError "Cannot extend environment with an environment that already has a parent."

childEnvironment :: Environment -> [String] -> [Expression] -> Environment
childEnvironment env keys values = extendEnvironment env $ zipEnvironment keys values

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

-- print a lisp expression as-is to the console
lispShow :: Expression -> String
lispShow (Atom x) = x
lispShow (String x) = "\"" ++ x ++ "\""
lispShow (Integer x) = show x
lispShow (Real x) = show x
lispShow Null = "'()"
lispShow (Cell a b) = "(" ++ (lispShow a) ++ " " ++ (lispShow b) ++ ")"

-- reads lisp code into an AST
lispRead :: String -> Expression
lispRead input = case parse parseExpr "lisp" input of
    Left err -> lispError $ "No match: " ++ show err
    Right val -> val

-- evaluates lisp expressions
lispEval :: Environment -> Expression -> (Expression, Environment)
lispEval env expr = do
  case expr of
    (Atom a) -> case getEnvironment a env of
                    Just lkup -> lkup
                    Nothing -> lispError "Atom not found in environment."
    (Integer a) -> ((Integer a), env)
    (String a) -> ((String a), env)
    (Real a) -> ((Real a), env)
    (Bool a) -> ((Bool a), env)
    (Cell (Atom "+") e) -> case car e of
                             Real value = (value + (lispEval (Cell (Atom "+") (cdr e))), env)
                             Integer value = (value + (lispEval (Cell (Atom "+") (cdr e))), env)
                             _ -> lispError "Cannot add non-number value"
    (Cell (Atom "*") e) -> case car e of
                             Real value = value * (lispEval (Cell (Atom "*") (cdr e)))
                             Integer value = value * (lispEval (Cell (Atom "*") (cdr e)))
                             _ -> lispError "Cannot add non-number value"
    (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) -> lispEval isfalse env
    (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) -> lispEval iftrue env
    (Cell (Atom "if") e) -> lispError "Invalid special form: if."
    (Cell (Atom "cons") (Cell a (Cell b Null))) = Cell a b
    (Cell (Atom "cons") (Cell a (Cell b _))) = lispError "cons: Too many arguments."
    (Cell (Atom "cons") a) = lispError "cons: Too few arguments."
    (Cell (Atom "car") (Cell a _)) = (a, env)
    (Cell (Atom "car") e) = lispError "car: Cannot take the car of: " ++ (show e)
    (Cell (Atom "cdr") (Cell _ b)) = (b, env)
    (Cell (Atom "cdr") e) = lispError "car: Cannot take the cdr of: " ++ (lispShow (Cell (Atom "display") (Cell (Atom "quote") e)))
    (Cell (Atom "display") e) = seq (unsafePerformIO $ putStrLn $ lispShow e) Null
    (Cell (Atom "bind") (Cell (Atom key) value)) -> (Atom key, setEnvironment key env value)
    (Cell (Atom "begin") e) -> (lispBegin env e, env)
    (Cell (Atom "fork") e) -> seq (unsafePerformIO $ forkOS $ lispBegin env a) (Null, env)
    (Cell (Atom "lambda") (Cell argNamesList bodyList)) -> (Procedure env (fromConsList argNamesList) (fromConsList bodyList), env)
    (Cell (Atom "lambda") e) -> lispError "Invalid lambda: " ++ (lispShow e)
    (Cell (Procedure procenv argNamesList bodies) e) = (lispBegin (childEnvironment env (map (\(Atom key) -> key) (fromConsList argNamesList)) (fromConsList e)) (toConsList bodies), env)
    (Cell a e) -> case getEnvironment a env of
                    Just lkup -> return $ lispEvalEnv (cons lkup e) env
                    Nothing -> lispError "Atom not found in environment."
    e -> lispError $ "Invalid form: " ++ lispShow e
    
repl :: String -> IO ()
repl prompt = do
  putStrLn prompt
  input <- getLine
  putStrLn $ show $ lispBegin emptyEnvironment $ lispRead input
  repl prompt
  
evalProgram :: String -> String
evalProgram code = lispBegin emptyEnvironment $ Cell (Atom "begin") $ lispRead code
  
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

fromConsList :: Expression -> [Expression]
fromConsList Null = []
fromConsList (Cell a b) = a:fromConsList b

-- returns true if the expression is a cons list
isConsList :: Expression -> Bool
isConsList Null = True
isConsList (Cell a Null) = True
isConsList (Cell _ b) = isConsList b
isConsList a = False

lispError :: [Char] -> t
lispError a = error a

-- evals each expression in a Lisp list and returns the result of the last eval
lispBegin :: Environment -> Expression -> (Expression, Environment)
lispBegin env (Cell a Null) = lispEval env a
lispBegin env (Cell a b) = lispBegin (snd $ lispEval env a) b
lispBegin env a = lispEval env a

-- (apply + 1 2 3 4 5)
lispApply :: Expression -> Expression -> Expression -> Expression
lispApply (Atom exprName)

-------------------------------------------------------------------------------
-- | Main (temporary)
  
main = do
  let code = "(this)"
  print $ lispEval $ lispRead code
