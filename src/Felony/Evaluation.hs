module Felony.Evaluation
(
  lispEval,
  lispEvalEnvironment
)
where
  
import Felony.Types
import Felony.Environment
import Felony.Expression
import Felony.Parser
import Control.Concurrent

import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Exception.Base
import System.Environment

import Data.Default

import Debug.Trace

infiniteBicompare :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool
infiniteBicompare _ [] = True
infiniteBicompare _ [x] = True
infiniteBicompare f (x:y:rest)
 | f x y = infiniteBicompare f (y:rest)
 | otherwise = False

-------------------------------------------------------------------------------
-- | Felony core

evalProgram :: String -> IO Expression
evalProgram str = catch act onErr
  where
    act = lispEval (lispReadFile str) def
    onErr e = flip lispEval $ def $ Cell (Atom "display") (Cell (String $ show (e :: ErrorCall)) Null)

-- reads lisp code into an AST
lispReadFile :: String -> Expression
lispReadFile "" = Null
lispReadFile input = case parseFelony input of
  [e] -> e
  es -> (Cell (Atom "begin") (toConsList es))

-------------------------------------------------------------------------------
-- | higher order functions implemented using Expression

lispFoldl :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
lispFoldl f z Null = z
lispFoldl f z (Cell x xs) = lispFoldl f (f z x) xs

-------------------------------------------------------------------------------
-- | Felony helper functions

lispSequence :: [Expression] -> Felony Expression
lispSequence bodies = last <$> mapM step bodies
  where
    step a = do
      env <- get
      (ret, renv) <- liftIO $ lispEvalEnvironment a $ mkChildEnv env
      case envParent renv of
        Just e -> put e
        Nothing -> return ()
      return ret

-- I know how hacky this is. Shut up.
lispMath :: (Double -> Double -> Double) -> Expression -> Expression -> Expression
lispMath f (Integer o) (Integer p) = Integer $ truncate (f (fromIntegral o) (fromIntegral p))
lispMath f (Integer a) (Real b)    = Real (f (fromIntegral a) b)
lispMath f (Real a)    (Integer b) = Real (f a (fromIntegral b))
lispMath f (Real a)    (Real b)    = Real (f a b)
lispMath _ _        _              = error "Cannot perform math with non-numeric expression."

lispAtomValue :: Expression -> String
lispAtomValue (Atom a) = a
lispAtomValue e = error $ "Not an atom: " ++ (show e)

lispStringValue :: Expression -> String
lispStringValue (String s) = s
lispStringValue e = error $ "Not a string: " ++ (show e)


lispEval :: Expression -> Environment -> IO Expression
lispEval expr env = evalStateT (lispEvalM expr) env

lispEvalEnvironment :: Expression -> Environment -> IO (Expression, Environment)
lispEvalEnvironment expr env = runStateT (lispEvalM expr) env

lispChildEvalM :: Expression -> Felony Expression
lispChildEvalM expr = do
  state (\s -> (def { envParent = (Just s) }, s))
  lispEvalM expr

lispEvalM :: Expression -> Felony Expression
lispEvalM expr = traceShow expr >> ((flip eval' $ expr) =<< get)
  where
    lispRead = parseFelony
    eval' :: Environment -> Expression -> Expression
    eval' expr env = case expr of
        -- import
        -- TODO: these two are horribly broken
        -- (begin (eval "(bind! a 1) (display a)") (display a)) 
        -- the second display statement doesn't work
        (Cell (Atom "eval") (Cell (String code) Null)) -> (lispSequence $ lispRead code)
        (Cell (Atom "import") (Cell (String a) Null)) -> (liftIO $ readFile a) >>= lispSequence . lispRead
        
        
       -- (lispSequence [] []) <$> lispRead <$> (liftIO $ readFile a)
        --(Cell (Atom "import") e) -> lispSequence [] [] $ (concat . (map lispRead)) <$> (liftIO $ mapM readFile (map lispStringValue $ fromConsList e))
        
        -- if
        (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) -> lispEvalM iffalse 
        (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) -> lispEvalM iftrue
        (Cell (Atom "if") _) -> error "Invalid special form: if."
        (Cell (Atom "not") (Cell e Null)) -> lispEvalM e >>= \ev -> case ev of
          (Bool True)  -> return $ Bool False
          (Bool False) -> return $ Bool True
          _            -> error $ "Expression is not a bool."
          
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
        
        -- atom <-> string
        -- TODO: evaluate!!!
        --(Cell (Atom "a2s") (Cell (Atom "quote") (Cell (Atom s) _))) -> return $ String s
        
        
        -- (Cell (Atom "a2s") (Cell (Cell (Atom "quote") (Cell (Atom a) Null)) Null)) -> return $ String a -- ((quote asdf))
--         (Cell (Atom "s2a") (Cell (String s) _)) -> return $ Atom s
          
        -- exprs that get translated into other exprs    (lambda (() . ))
        (Cell (Atom "begin") e) -> lispEvalM (Cell (Cell (Atom "lambda") (Cell Null e)) Null) -- begins a new scope
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
              put $ (Environment (Just $ envParent env) ) -- setParentEnvironment key env value
              get >>= liftIO . print 
              return $ Cell (Atom "quote") (Cell e Null)
            raw@(Cell _ _) -> lispEvalM raw >>= \e -> lispEvalM $ Cell (Atom "bind!") (Cell e (Cell value Null))
            _ -> error $ "Invalid binding."
        (Atom a) -> do
          get >>= liftIO . print . ((++) "Looking up: ") . show
          case getEnvironment a env of -- environment lookup
                        Just lkup -> return lkup
                        Nothing -> error $ "Binding not found: " ++ a
        
        (Cell (Atom a) e) -> lispEvalM (Atom a) >>= \a' -> lispEvalM $ Cell a' e
      
        (Cell (Procedure procenv argNames bodies) e) -> do
          evaledArgs <- (mapM lispChildEvalM (fromConsList e))
          oldenv <- get
          put $ mkEnv (Just procenv) argNames evaledArgs
          evaluation <- lispSequence bodies
          put oldenv
          return evaluation
              
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