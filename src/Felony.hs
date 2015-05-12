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
                | Cell Expression Expression deriving (Eq)

data Environment = Environment {
  bindings :: M.Map String Expression,
  parent :: Maybe Environment
} deriving (Eq, Show)

-------------------------------------------------------------------------------
-- | General

infiniteBicompare :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool
infiniteBicompare _ [] = True
infiniteBicompare _ [x] = True
infiniteBicompare f (x:y:rest)
 | f x y = infiniteBicompare f (y:rest)
 | otherwise = False

-------------------------------------------------------------------------------
-- | Environment

setEnvironment :: String -> Environment -> Expression -> Environment
setEnvironment key (Environment bindings_ parent_) expr = Environment (M.insert key expr bindings_) parent_

setParentEnvironment :: String -> Environment -> Expression -> Environment
setParentEnvironment key (Environment b (Just (Environment pb pp))) expr = Environment b (Just $ Environment (M.insert key expr pb) pp)
setParentEnvironment key env _ = env

-- unsetEnvironment :: [String] -> Environment -> Environment
-- unsetEnvironment [] env = env
-- unsetEnvironment (key:ks) (Environment bindings_ parent_) = unsetEnvironment ks $ Environment (M.delete key bindings_) parent_

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

parseNumber :: Parser Expression
parseNumber = (many1 digit) `sepBy1` (char '.') >>= \halves -> case halves of
  [x,y] -> return $ Real $ read $ x ++ "." ++ y
  [x] -> return $ Integer $ read x
  _ -> error "Invalid number."

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
parseExpr = (parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseList <|> fail "invalid syntax.")

-------------------------------------------------------------------------------
-- | Display

instance Show Expression where
  show (Atom x) = x
  show (String x) = "\"" ++ x ++ "\""
  show (Integer x) = show x
  show (Real x) = show x
  show Null = "'()"
  show c@(Cell a b) = showCell True c
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Procedure _ argsnames expressions) = show (Cell (Atom "procedure") (Cell (toConsList (map Atom argsnames)) (toConsList expressions)))

-- Bool is hasLeadingParen
showCell :: Bool -> Expression  -> String
showCell True  (Cell a Null)         = "(" ++ (show a) ++ ")"
showCell False (Cell a Null)         = (show a) ++ ")"
showCell True  (Cell a b@(Cell _ _)) = "(" ++ (show a) ++ " " ++ (showCell False b)
showCell False (Cell a b@(Cell _ _)) = (show a) ++ " " ++ (showCell False b)
showCell _     (Cell a b)            = "(" ++ (show a) ++ " . " ++ (show b) ++ ")"
showCell _     _                     = error "Invalid cons cell."

-------------------------------------------------------------------------------
-- | Felony core

-- reads lisp code into an AST
lispRead :: String -> Expression
lispRead "" = Null
lispRead input = case parse parseExpr "" input of
  Left err -> error $ "Invalid syntax " ++ show err
  Right val -> val

lispEvalToplevel :: Expression -> IO Expression
lispEvalToplevel expr = lispEval (Cell (Atom "begin") (Cell expr Null)) emptyEnvironment

lispEval :: Expression -> Environment -> IO Expression
lispEval expr env = evalStateT (lispEvalM expr) env

lispEvalEnvironment :: Expression -> Environment -> IO (Expression, Environment)
lispEvalEnvironment expr env = runStateT (lispEvalM expr) env

lispEvalM :: Expression -> Felony Expression
lispEvalM expr = do
  env <- get
  case expr of
    -- import
    (Cell (Atom "import") e) -> do
      codes <- (liftIO $ mapM readFile (map lispStringValue (fromConsList e)))
      let exprs = map lispRead codes
      mapM_ lispEvalM exprs
      return Null
    
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
      (Cell _ (Cell _ _)) -> return $ Bool False
      (Cell _ _)          -> return $ Bool True
      _                   -> return $ Bool False
    (Cell (Atom "list?") (Cell lst Null)) -> Bool <$> (lispEvalM lst >>= return . isConsList)
    
    -- atom to string
    -- TODO: evaluate!!!
    --(Cell (Atom "a2s") (Cell (Atom "quote") (Cell (Atom s) _))) -> return $ String s
    (Cell (Atom "a2s") (Cell (Cell (Atom "quote") (Cell (Atom a) Null)) Null)) -> return $ String a -- ((quote asdf))
    (Cell (Atom "s2a") (Cell (String s) _)) -> return $ Atom s
      
    -- exprs that get translated into other exprs    (lambda (() . ))
    (Cell (Atom "begin") e) -> lispEvalM (Cell (Cell (Atom "lambda") (Cell Null e)) Null) -- just returns proc, need to evaluate it
    (Cell (Atom "list") e) -> return e
    (Cell (Atom "quote") (Cell e Null)) -> return e
    
    -- lambda
    (Cell (Atom "lambda") (Cell argnames bodies)) -> return $ Procedure env (map lispAtomValue $ fromConsList argnames) (fromConsList bodies)
    (Cell (Atom "lambda") e) -> error $ "Invalid lambda: " ++ (show e)
    
    -- apply
    (Cell (Atom "apply") (Cell a (Cell e@(Cell _ _) Null))) -> lispEvalM e >>= \args -> lispEvalM (Cell a args)
    (Cell (Atom "apply") _) -> error "Invalid special form: apply."
    
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
          put $ setParentEnvironment key env value
          return $ Atom key
        bindexpr -> lispEvalM bindexpr >>= \e -> case e of
          Atom key -> do
            put $ setParentEnvironment key env value
            return $ Atom key
          _ -> error $ "Invalid binding."
    (Atom a) -> case getEnvironment a env of -- environment lookup
                    Just lkup -> return lkup
                    Nothing -> error $ "Binding not found: " ++ a ++ " in: " ++ (show env)
    
    (Cell (Atom a) e) -> lispEvalM (Atom a) >>= \a' -> lispEvalM $ Cell a' e

    (Cell (Procedure procenv argNames bodies) e) -> do
      evaledArgs <- (mapM lispEvalM (fromConsList e))
      oldenv <- get
      put procenv
      evaluation <- last <$> mapM (f evaledArgs) bodies
      put oldenv
      return evaluation
      
      where
        f :: [Expression] -> Expression -> Felony Expression
        f args a = do
          env <- get
          (ret, renv) <- liftIO $ lispEvalEnvironment a $ childEnvironment env argNames args
          case parent renv of
            Just parentEnv -> put parentEnv
            Nothing -> return ()
          return ret
          
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

-------------------------------------------------------------------------------
-- | higher order functions implemented using Expression

lispFoldl :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
lispFoldl f z Null = z
lispFoldl f z (Cell x xs) = lispFoldl f (f z x) xs

-------------------------------------------------------------------------------
-- | Felony helper functions

-- I know how hacky this is. Shut up.
lispMath :: (Double -> Double -> Double) -> Expression -> Expression -> Expression
lispMath f (Integer o) (Integer p) = Integer $ truncate (f (fromIntegral o) (fromIntegral p))
lispMath f (Integer a) (Real b)    = Real (f (fromIntegral a) b)
lispMath f (Real a)    (Integer b) = Real (f a (fromIntegral b))
lispMath f (Real a)    (Real b)    = Real (f a b)
lispMath _ _        _              = error "Cannot perform math with non-numeric expression."

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

isConsList :: Expression -> Bool
isConsList Null = True
isConsList (Cell a b) = True == (isConsList b)
isConsList _ = False

lispAtomValue :: Expression -> String
lispAtomValue (Atom a) = a
lispAtomValue e = error $ "Not an atom: " ++ (show e)

lispStringValue :: Expression -> String
lispStringValue (String s) = s
lispStringValue e = error $ "Not a string: " ++ (show e)
