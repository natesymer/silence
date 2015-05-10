{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Felony where
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map as M
-- import           Control.Monad.IO.Class
import qualified Control.Applicative as A
import           Control.Concurrent
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

-- instance Num Expression where
--   Integer v1 + Integer v2 = Integer (v1 + v2)
--   Real v1 + Integer v2 = Real (v1 + v2)
--   Integer v1 + Real v2 = Real (v1 + v2)
--   Real v1 + Real v2 = Real (v1 + v2)
--   (+) _ _ = error "Mathematical operations not allowed"

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
zipEnvironment k e = Environment (M.fromList $ zipEnvironment' k e) Nothing
  
zipEnvironment' :: [String] -> [Expression] -> [(String, Expression)]
zipEnvironment' _ [] = []
zipEnvironment' [] _ = []
zipEnvironment' ("*":ks) es = [("*", toConsList es)]
zipEnvironment' (k:ks) (e:es) = [(k, e)] ++ zipEnvironment' ks es
  
extendEnvironment :: Environment -> Environment -> Environment
extendEnvironment prnt (Environment bndngs Nothing) = Environment bndngs $ Just prnt
extendEnvironment _ _ = error "Cannot extend environment with an environment that already has a parent."

childEnvironment :: Environment -> [String] -> [Expression] -> Environment
childEnvironment env keys values = extendEnvironment env $ zipEnvironment keys values

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty Nothing

-------------------------------------------------------------------------------
-- | Lisp AST parsers

eol :: Parser ()
eol = (void newline) <|> eof

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

skipComments :: Parser ()
skipComments = optional $ do
  char ';'
  void $ manyTill anyToken eol
  
skipWSAndNewline :: Parser ()
skipWSAndNewline = skipMany $ (newline <|> space)

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser Expression
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
  char '"'
  return (String x)
  
parseAtom :: Parser Expression
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
 -- return $ case [first] ++ rest of
  return $ case first:rest of
             "#t" -> Bool True
             "#f" -> Bool False
             atom -> Atom atom
                          
parseInteger :: Parser Expression
parseInteger = fmap (Integer . read) $ many1 digit

-- TODO: invalid
parseReal :: Parser Expression
parseReal = fmap (Real . read) $ many1 (digit <|> char '.')

parseQuoted :: Parser Expression
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ Cell (Atom "quote") (Cell x Null)

-- TODO: problematic
parseList :: Parser Expression
parseList = between (char '(') (char ')') (parseProperList <|> parseDottedList)

parseProperList :: Parser Expression
parseProperList = fmap toConsList $ sepBy parseExpr (spaces <|> (skipMany newline) <|> skipComments)

-- TODO: Improve this dramatically
-- multiple elements on either side of dot
parseDottedList :: Parser Expression
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ Cell (head h) t

parseExpr :: Parser Expression
parseExpr = (parseAtom <|> parseString <|> parseInteger <|> parseReal <|> parseQuoted <|> parseList <|> fail "invalid syntax.")

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
lispShow (Bool True) = "#t"
lispShow (Bool False) = "#f"
lispShow (Procedure _ _ _) = "procedure"

-- reads lisp code into an AST
lispRead :: String -> Expression
lispRead "" = Null
lispRead input = case parse parseExpr "" input of
    Left err -> error $ "Invalid syntax " ++ show err
    Right val -> val
    
type Felony a = StateT Environment IO a

felonyIO :: IO a -> Felony a
felonyIO = liftIO
  
-- Runs lisp code. For external use
lispRun :: Expression -> IO Expression
lispRun expr = lispEval expr emptyEnvironment
  
-- used internally
lispEval :: Expression -> Environment -> IO Expression -- used to return Felony expression
lispEval expr env = evalStateT (lispEvalM expr) env

lispEvalM :: Expression -> Felony Expression
lispEvalM expr = do
  env <- get
  case expr of
    -- if
    (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) -> lispEvalM iffalse 
    (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) -> lispEvalM iftrue
    (Cell (Atom "if") _) -> error "Invalid special form: if."
      
    -- cons & car
    (Cell (Atom "cons") (Cell a (Cell b Null))) -> Cell <$> (lispEvalM a) <*> (lispEvalM b) -- TODO: Fix cons lists
    (Cell (Atom "cons") _) -> error "cons: Incorrect number of arguments."
    (Cell (Atom "car") (Cell a _)) -> return a
    (Cell (Atom "car") e) -> error $ "car: Cannot take the car of: " ++ (show e)
    (Cell (Atom "cdr") (Cell _ b)) -> return b
    (Cell (Atom "cdr") e) -> error $ "car: Cannot take the cdr of: " ++ (show e)
      
    -- exprs that get translated into other exprs
    (Cell (Atom "begin") e) -> lispEvalM (Cell (Atom "lambda") (Cell Null e)) >>= lispEvalM -- just returns proc, need to evaluate it
    (Cell (Atom "quote") (Cell e Null)) -> return e
    
    -- lambda
    (Cell (Atom "lambda") (Cell argnames bodies)) -> return $ Procedure env (map getAtom $ fromConsList argnames) (fromConsList bodies)
    (Cell (Atom "lambda") e) -> error $ "Invalid lambda: " ++ (show e)
    
    -- TODO: `apply` function
    
    -- threaded execution
    (Cell (Atom "fork") e) -> do
      liftIO $ void $ forkOS $ void $ lispEval e env
      return Null 

    -- environment operations
    (Cell (Atom "bind!") (Cell e (Cell value Null))) -> do
      case e of
        Atom key -> do
          put $ setEnvironment key env value
          return $ Atom key
        bindexpr -> lispEvalM bindexpr >>= \e -> case e of
          Atom key -> do
            put $ setEnvironment key env value
            return $ Atom key
          _ -> error $ "Invalid binding."
    (Atom a) -> case getEnvironment a env of -- environment lookup
                    Just lkup -> return lkup
                    Nothing -> error $ "Variable not found: " ++ a
                    
    -- TODO: (func asdf (arg1 arg2) (display arg1) (display arg2))
    
    -- misc
    (Cell (Atom "display") (Cell e Null)) -> do
      lispEvalM e >>= liftIO . print
      return Null
    
    -- TODO: Evaluate procedures properly
    
    -- evaluation
    (Cell (Procedure procenv argNames bodies) e) -> do
      liftIO $ last <$> mapM f bodies
        where
          args = fromConsList e
          newEnv = (childEnvironment procenv argNames args)
          f a = lispEval a newEnv
    (Cell a e) -> lispEvalM a >>= \a' -> lispEvalM $ Cell a' e
    
    -- fall through for literals
    (Integer _) -> return expr
    (String _) -> return expr
    (Real _) -> return expr
    (Bool _) -> return expr
    (Procedure _ _ _) -> return expr
    Null -> return expr
    
    -- invalid form catchall
    e -> error $ "Invalid form: " ++ (show e)
    
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
fromConsList e = error $ "Not a cons list: " ++ (show e)

-- -------------------------------------------------------------------------------
-- -- | Lisp primitive functions
--
--
-- lispFork :: Environment -> Expression -> Expression
-- lispFork env e = seq (unsafePerformIO $ forkOS $ seq (lispBegin env e) (return ())) Null
--
-- -- evals each expression in a Lisp list and returns the result of the last eval
-- -- This function is a giant fucking ugly hack
-- -- Haskell doesn't compute things it doesn't need. When evaling a non-last expression
-- -- in a lambda or begin, the first half of the tuple would not be calculated (it was
-- -- not referenced), so the unsafe side effects would not happen.
-- lispBegin :: Environment -> Expression -> Expression
-- lispBegin env (Cell a Null) = fst $ lispEval env a
-- lispBegin env (Cell a b) = seq (fst e) lispBegin (snd e) b
--                             where
--                               e = lispEval env a
-- lispBegin env a = fst $ lispEval env a
--
-- -- (apply + 1 2 3 4 5)
-- lispApply :: Expression -> Expression -> Expression
-- lispApply (Procedure env argslist bodies) args = lispBegin (zipEnvironment argslist (fromConsList args)) (toConsList bodies)
-- lispApply _ _ = error "Cannot evaluate non-procedure."
--
-- lispMap :: (Expression -> Expression) -> Expression -> Expression
-- lispMap f Null = Null
-- lispMap f (Cell a b)  = (Cell (f a) (lispMap f b))
-- lispMap f _ = error "Cannot map a non-conslist."
--
-- lispFoldl :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
-- lispFoldl f z Null = z
-- lispFoldl f z (Cell x xs) = lispFoldl f (f z x) xs
--
-- lispFoldr :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
-- lispFoldr f z Null = z
-- lispFoldr f z (Cell x xs) = f x (lispFoldr f z xs)

-- recursively applies mathmematical operations over a cons list of arguments
-- lispMath :: String -> Environment -> Expression -> Expression
-- lispMath _      env (Cell a Null)          = (fst $ lispEval env a)
-- lispMath op@"+" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((+) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath op@"-" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((-) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath op@"*" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((*) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath op@"/" env (Cell a (Cell b rest)) = lispMath op env $ Cell ((/) A.<$> (fst $ lispEval env a) A.<*> (fst $ lispEval env b)) rest
-- lispMath _      _   expr                   = error $ "Invalid expression: " ++ (lispShow expr)
