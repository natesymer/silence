{-# LANGUAGE OverloadedStrings, DeriveGeneric, NoMonomorphismRestriction #-}

module Felony where
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Concurrent
import qualified Data.Map as M
import           System.IO
import           Text.ParserCombinators.Parsec as Parsec hiding (spaces)

type Felony a = StateT Environment IO a

data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Procedure Environment [String] [Expression]
                | Null
                | Cell Expression Expression deriving (Eq, Show)

-- TODO: Fix issue with parsing Reals vs Integers

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
parseReal = fmap (Real . read) $ many1 (digit <|> (char '.'))

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
parseExpr = (parseAtom <|> parseString <|> parseReal <|> parseInteger <|> parseQuoted <|> parseList <|> fail "invalid syntax.")

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
  
-- Runs lisp code. For external use
lispRun :: Expression -> IO Expression
lispRun expr = lispEval expr emptyEnvironment
  
-- used internally
lispEval :: Expression -> Environment -> IO Expression -- used to return Felony expression
lispEval expr env = evalStateT (lispEvalM expr) env

lispMath :: (Double -> Double -> Double) -> Expression -> Expression -> Expression
lispMath f (Integer o) (Integer p) = Integer $ truncate (f (fromIntegral o) (fromIntegral p))
lispMath f (Integer a) (Real b)    = Real (f (fromIntegral a) b)
lispMath f (Real a)    (Integer b) = Real (f a (fromIntegral b))
lispMath f (Real a)    (Real b)    = Real (f a b)
lispMath _ _        _              = error "Cannot perform math with non-numeric expression."

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
    
    -- type checking
    (Cell (Atom "integer?") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
      (Integer _) -> return $ Bool True
      _           -> return $ Bool False
    (Cell (Atom "real?") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
      (Real _) -> return $ Bool True
      _        -> return $ Bool False
    (Cell (Atom "string?") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
      (String _) -> return $ Bool True
      _          -> return $ Bool False
    (Cell (Atom "atom?") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
      (Atom _) -> return $ Bool True
      _        -> return $ Bool False
    (Cell (Atom "null?") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
      Null       -> return $ Bool True
      _          -> return $ Bool False
    (Cell (Atom "pair?") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
      (Cell _ _) -> return $ Bool True
      _          -> return $ Bool False
      
    -- exprs that get translated into other exprs
    --(Cell (Atom "begin") e) -> lispEvalM (Cell (Atom "lambda") (Cell Null e)) >>= lispEvalM -- just returns proc, need to evaluate it
    (Cell (Atom "quote") (Cell e Null)) -> return e
    
    -- lambda
    (Cell (Atom "lambda") (Cell argnames bodies)) -> return $ Procedure env (map getAtom $ fromConsList argnames) (fromConsList bodies)
    (Cell (Atom "lambda") e) -> error $ "Invalid lambda: " ++ (show e)
    
    -- TODO: `apply` function
    
    -- math
    (Cell (Atom "+") (Cell h t)) -> lispFoldl (lispMath (+)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM (fromConsList t)))
    (Cell (Atom "-") (Cell h t)) -> lispFoldl (lispMath (-)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM (fromConsList t)))
    (Cell (Atom "*") (Cell h t)) -> lispFoldl (lispMath (*)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM (fromConsList t)))
    (Cell (Atom "/") (Cell h t)) -> lispFoldl (lispMath (/)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM (fromConsList t)))
    
    -- equality
    (Cell (Atom "==") e) -> Bool <$> (infiniteBicompare (==) <$> (mapM lispEvalM (fromConsList e)))
    
    -- IO-related
    (Cell (Atom "fork") e) -> (liftIO $ void $ forkOS $ void $ lispEval e env) >> return Null
    (Cell (Atom "display") (Cell e Null)) -> (lispEvalM e >>= liftIO . print) >> return Null

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
                    Nothing -> error $ "Binding not found: " ++ a
    
    (Cell (Atom a) e) -> lispEvalM (Atom a) >>= \a' -> lispEvalM $ Cell a' e

    (Cell (Procedure procenv argNames bodies) e) -> do
      liftIO $ last <$> mapM f bodies
        where
          args = fromConsList e
          newEnv = (childEnvironment procenv argNames args)
          f a = lispEval a newEnv
          
    -- general evaluation
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

numerify :: Expression -> Double
numerify (Integer v) = fromIntegral v
numerify (Real v) = v
numerify expr = error $ "Invalid number: " ++ (show expr)

-------------------------------------------------------------------------------
-- | higher order functions implemented using Expression

lispFoldl :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
lispFoldl f z Null = z
lispFoldl f z (Cell x xs) = lispFoldl f (f z x) xs

infiniteBicompare :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool
infiniteBicompare _ [] = True
infiniteBicompare _ [x] = True
infiniteBicompare f (x:y:rest)
 | f x y = infiniteBicompare f (y:rest)
 | otherwise = False

-- lispFoldr :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
-- lispFoldr f z Null = z
-- lispFoldr f z (Cell x xs) = f x (lispFoldr f z xs)
--
-- lispMap :: (Expression -> Expression) -> Expression -> Expression
-- lispMap f Null = Null
-- lispMap f (Cell a b)  = (Cell (f a) (lispMap f b))
-- lispMap f e = error $ "Not a list: " ++ (show e)

--
-- -- (apply + 1 2 3 4 5)
-- lispApply :: Expression -> Expression -> Expression
-- lispApply (Procedure env argslist bodies) args = lispBegin (zipEnvironment argslist (fromConsList args)) (toConsList bodies)
-- lispApply _ _ = error "Cannot evaluate non-procedure."
--

