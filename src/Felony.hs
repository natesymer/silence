{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Felony where
import qualified Data.Map as M
import           Control.Monad.IO.Class
import qualified Control.Applicative as A
import           Control.Concurrent
import           System.IO.Unsafe
import           System.IO
import           Text.ParserCombinators.Parsec as Parsec hiding (spaces)

data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Procedure Environment [String] [Expression]
                | Null
                | Cell Expression Expression deriving (Eq, Show)

getAtom :: Expression -> String
getAtom (Atom a) = a
getAtom e = error $ "Not an atom: " ++ (show e)

data Environment = Environment {
  bindings :: M.Map String Expression,
  parent :: Maybe Environment
} deriving (Eq, Show)

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
zipEnvironment keys exprs = Environment (M.fromList $ zip keys exprs) Nothing

extendEnvironment :: Environment -> Environment -> Environment
extendEnvironment parent (Environment bindings Nothing) = Environment bindings $ Just parent
extendEnvironment _ _ = error "Cannot extend environment with an environment that already has a parent."

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
    return $ Cell (head h) t

parseQuoted :: Parser Expression
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ Cell (Atom "quote") (Cell x Null)

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
    Left err -> error $ "No match: " ++ show err
    Right val -> val

-- evaluates lisp expressions
lispEval :: Environment -> Expression -> (Expression, Environment)
lispEval env expr = do
  case expr of
    (Atom a) -> case getEnvironment a env of
                    Just lkup -> (lkup, env)
                    Nothing -> error $ "Variable not found: " ++ a
    (Integer a) -> (expr, env)
    (String a) -> (expr, env)
    (Real a) -> (expr, env)
    (Bool a) -> (expr, env)
    -- (Cell (Atom "+") e) -> (lispMath "+" env e, env)
    -- (Cell (Atom "-") e) -> (lispMath "-" env e, env)
    -- (Cell (Atom "*") e) -> (lispMath "*" env e, env)
    -- (Cell (Atom "/") e) -> (lispMath "/" env e, env)
    (Cell (Atom "quote") (Cell e Null)) -> (e, env)
    (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) -> lispEval env iffalse 
    (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) -> lispEval env iftrue
    (Cell (Atom "if") e) -> error "Invalid special form: if."
    (Cell (Atom "cons") (Cell a (Cell b Null))) -> (Cell a b, env)
    (Cell (Atom "cons") (Cell a (Cell b _))) -> error "cons: Too many arguments."
    (Cell (Atom "cons") a) -> error "cons: Too few arguments."
    (Cell (Atom "car") (Cell a _)) -> (a, env)
    (Cell (Atom "car") e) -> error $ "car: Cannot take the car of: " ++ (show e)
    (Cell (Atom "cdr") (Cell _ b)) -> (b, env)
    (Cell (Atom "cdr") e) -> error $ "car: Cannot take the cdr of: " ++ (lispShow (Cell (Atom "display") (Cell (Atom "quote") e)))
    (Cell (Atom "display") (Cell e Null)) -> seq (print e) $ seq (unsafePerformIO $ putStrLn $ show $ fst $ lispEval env e) (Null, env)
    (Cell (Atom "bind") (Cell e (Cell value _))) -> (Atom key, setEnvironment key env value) 
                                                      where 
                                                        key = getAtom $ fst $ lispEval env e
    (Cell (Atom "begin") e) -> lispBegin env e
--    (Cell (Atom "fork") e) -> seq (unsafePerformIO $ forkOS $ lispBegin env e) (Null, env)
    (Cell (Atom "lambda") (Cell argNamesList bodyList)) -> (Procedure env (map (\(Atom x) -> x) (fromConsList argNamesList)) (fromConsList bodyList), env)
    (Cell (Atom "lambda") e) -> error $ "Invalid lambda: " ++ (lispShow e)
    (Cell (Procedure procenv argNamesList bodies) e) -> lispBegin (childEnvironment env argNamesList (fromConsList e)) (toConsList bodies)
    -- (Cell (Atom a) e) -> case getEnvironment a env of
    --                       Just lkup -> lispEval env $ Cell lkup e
    --                       Nothing -> error $ "Atom not found in environment: " ++ a
    e -> error $ "Invalid form: " ++ show expr
    
repl :: String -> IO ()
repl prompt = do
  putStr prompt
  hFlush stdout
  code <- getLine
  print $ fst $ lispEval emptyEnvironment $ lispRead code
  repl prompt
  
evalProgram :: String -> Expression
evalProgram code = fst $ lispBegin emptyEnvironment $ lispRead code
  
-------------------------------------------------------------------------------
-- | Lisp primitive functions

append :: Expression -> Expression -> Expression
append _ Null = error "Cannot append null to list."
append Null b = Cell b Null
append (Cell x xs) b = Cell x (append xs b)
append a b = Cell a (Cell b Null)

toConsList :: [Expression] -> Expression
toConsList (x:xs) = foldl append (Cell x Null) xs
toConsList [] = Null

fromConsList :: Expression -> [Expression]
fromConsList Null = []
fromConsList (Cell a b) = a:fromConsList b

-- lispFoldl :: () -> Expression -> Expression -> Expression
-- lispFoldl f z Null = z
-- lispFoldl f z (Cell x xs) = lispFoldl f (f z x) xs

-- returns true if the expression is a cons list
isConsList :: Expression -> Bool
isConsList Null = True
isConsList (Cell a Null) = True
isConsList (Cell _ b) = isConsList b
isConsList a = False

-- evals each expression in a Lisp list and returns the result of the last eval
lispBegin :: Environment -> Expression -> (Expression, Environment)
lispBegin env (Cell a Null) = lispEval env a
lispBegin env (Cell a b) = lispBegin (snd $ lispEval env a) b
lispBegin env a = lispEval env a

-- (apply + 1 2 3 4 5)
-- lispApply :: Expression -> Expression -> Expression -> Expression
-- lispApply (Atom exprName)

-- recursively applies mathmematical operations over a cons list of arguments
-- lispMath :: String -> Environment -> Expression -> Expression
-- lispMath _      env (Cell a Null)          = (fst $ lispEval env a)
-- lispMath op@"+" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((+) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath op@"-" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((-) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath op@"*" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((*) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath op@"/" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((/) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath _      _   expr                   = error $ "Invalid expression: " ++ (lispShow expr)
