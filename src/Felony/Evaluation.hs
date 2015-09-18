module Felony.Evaluation
(
  lispEval,
  lispEvalEnvironment,
  evalProgram
)
where
  
import Felony.Expression
import Felony.Parser
import Felony.Monad
import Control.Concurrent

import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Exception.Base

import qualified Data.HashMap.Strict as H

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
    act = lispEval (lispReadFile str) [H.empty]
    onErr e = lispEval (Cell (Atom "display") (Cell (String (show (e :: ErrorCall))) Null)) ([H.empty] :: Environment)

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
lispFoldl f z e = error $ "Invalid list: " ++ show e

-------------------------------------------------------------------------------
-- | Helpers


-- I know how hacky this is. Shut up.
lispMath :: (Double -> Double -> Double) -> Expression -> Expression -> Expression
lispMath f (Integer o) (Integer p) = Integer $ truncate (f (fromIntegral o) (fromIntegral p))
lispMath f (Integer a) (Real b)    = Real (f (fromIntegral a) b)
lispMath f (Real a)    (Integer b) = Real (f a (fromIntegral b))
lispMath f (Real a)    (Real b)    = Real (f a b)
lispMath _ _        _              = error "Cannot perform math with non-numeric expression."

atomValue :: Expression -> String
atomValue (Atom a) = a
atomValue e = error $ "Not an atom: " ++ (show e)

lispEval :: Expression -> Environment -> IO Expression
lispEval expr env = evalStateT (lispEvalM expr) env

lispEvalEnvironment :: Expression -> Environment -> IO (Expression, Environment)
lispEvalEnvironment expr env = runStateT (lispEvalM expr) env

-- TODO: atom to string conversion
--       import functionality
lispEvalM :: Expression -> Felony Expression
lispEvalM expr = ((flip eval' $ expr) =<< get)
  where
    eval' env (Cell (Atom "eval") (Cell (String code) Null)) = lispEvalM $ Procedure env [] (mconcat $ map fromConsList $ parseFelony code)
    eval' env (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) = lispEvalM iffalse 
    eval' env (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) = lispEvalM iftrue
    eval' env (Cell (Atom "if") _) = error "Invalid special form: if."
    eval' env (Cell (Atom "not") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
          (Bool True)  -> return $ Bool False
          (Bool False) -> return $ Bool True
          _            -> error $ "Expression is not a bool."
    eval' env (Cell (Atom "cons") (Cell a (Cell b Null))) = Cell <$> (lispEvalM a) <*> (lispEvalM b) -- TODO: Fix cons lists
    eval' env (Cell (Atom "cons") _) = error "cons: Incorrect number of arguments."
    eval' env (Cell (Atom "car") (Cell a _)) = return a
    eval' env (Cell (Atom "car") e) = error $ "car: Cannot take the car of: " ++ (show e)
    eval' env (Cell (Atom "cdr") (Cell _ b)) = return b
    eval' env (Cell (Atom "cdr") e) = error $ "car: Cannot take the cdr of: " ++ (show e)
    eval' env (Cell (Atom "integer?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Integer _) -> return $ Bool True
      _           -> return $ Bool False
    eval' env (Cell (Atom "real?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Real _) -> return $ Bool True
      _        -> return $ Bool False
    eval' env (Cell (Atom "string?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (String _) -> return $ Bool True
      _          -> return $ Bool False
    eval' env (Cell (Atom "atom?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Atom _) -> return $ Bool True
      _        -> return $ Bool False
    eval' env (Cell (Atom "null?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      Null       -> return $ Bool True
      _          -> return $ Bool False
    eval' env (Cell (Atom "pair?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Cell _ (Cell _ _)) -> return $ Bool False
      (Cell _ _)          -> return $ Bool True
      _                   -> return $ Bool False
    eval' env (Cell (Atom "list?") (Cell lst Null)) = Bool <$> (lispEvalM lst >>= return . isConsList)
    -- exprs that get translated into other exprs    (lambda (() . ))
    eval' env (Cell (Atom "begin") e) = lispEvalM (Cell (Cell (Atom "lambda") (Cell Null e)) Null) -- begins a new scope
    eval' env (Cell (Atom "quote") (Cell e Null)) = return e
    eval' env (Cell (Atom "lambda") (Cell keys vals)) = return $ Procedure env (map atomValue $ fromConsList keys) (fromConsList vals)
    eval' env (Cell (Atom "lambda") e) = error $ "Invalid lambda: " ++ (show e)
    eval' env (Cell (Atom "apply") (Cell a (Cell e@(Cell _ _) Null))) = lispEvalM e >>= \args -> lispEvalM (Cell a args)
    eval' env (Cell (Atom "apply") (Cell a e)) = lispEvalM e >>= \args -> lispEvalM (Cell a args)
    eval' env (Cell (Atom "apply") _) = error "Invalid special form: apply."
    eval' env (Cell (Atom "+") (Cell h t)) = lispFoldl (lispMath (+)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' env (Cell (Atom "-") (Cell h t)) = lispFoldl (lispMath (-)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' env (Cell (Atom "*") (Cell h t)) = lispFoldl (lispMath (*)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' env (Cell (Atom "/") (Cell h t)) = lispFoldl (lispMath (/)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' env (Cell (Atom "==") e) = Bool <$> (infiniteBicompare (==) <$> (mapM lispEvalM $ fromConsList e))
    -- IO-related
    eval' env (Cell (Atom "fork") e) = (liftIO $ void $ forkOS $ void $ lispEval e env) >> return Null
    eval' env (Cell (Atom "display") (Cell e Null)) = (lispEvalM e >>= liftIO . print) >> return Null
    -- Environment
    eval' env (Cell (Atom "bind!") (Cell (Atom key) (Cell value Null))) = do
      put $ (H.insert key value $ head env):(tail env) -- originally set (k, v) in parent env
      get >>= liftIO . print 
      return $ value
    eval' env (Cell (Atom "bind!") (Cell raw@(Cell _ _) (Cell v Null))) = lispEvalM raw >>= \e -> lispEvalM $ Cell (Atom "bind!") (Cell e (Cell v Null))
    eval' env (Cell (Atom "bind!") (Cell _ (Cell value Null))) = error $ "Invalid special form: bind!"
    -- env lookup
    eval' env (Atom key) = case H.lookup key $ head env of
      Just lkup -> return lkup
      Nothing -> error $ "Binding not found: " ++ key
    -- Procedure calling
    eval' env (Cell fname@(Atom a) args) = do
      proc <- lispEvalM fname
      case proc of
        Procedure _ _ _ -> lispEvalM $ Cell proc args
        _ -> error $ "First argument " ++a ++ " is not a procedure."
    -- Procedure evaluation
    eval' env (Cell (Procedure procenv argNames bodies) e) = do
      newenv <- fmap (H.fromList . zip argNames) . liftIO . mapM evalProcenv . fromConsList $ e
     --  get >>= put . ((:) newenv)
      evaluation <- mapM lispEvalM bodies
      --state $ statePopChild 
      return $ last evaluation
      where
        evalProcenv = flip lispEvalEnvironment $ procenv
        statePushChild env s = env:s
        statePopChild [] = [H.empty]
        statePopChild [x] = [x]
        statePopChild (_:xs) = xs
    
    eval' env (Integer _)       = return expr
    eval' env (String _)        = return expr
    eval' env (Real _)          = return expr
    eval' env (Bool _)          = return expr
    eval' env (Procedure _ _ _) = return expr
    eval' env Null              = return expr
    eval' env e = error $ "Invalid form: " ++ (show e)