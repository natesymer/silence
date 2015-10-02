{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Felony.Evaluation
(
  lispEval,
  evalProgram,
  evalCode
)
where
  
import Felony.Expression
import Felony.Parser

import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Control.Exception.Base

import qualified Data.Vector.Mutable as V
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)

import Debug.Trace

evalCode :: String -> IO Expression
evalCode code = lispEval $ Cell (Procedure unsafeCreateEnv [] $ parseFelony code) Null

evalProgram :: FilePath -> IO Expression
evalProgram fp = (readFile fp) >>= evalCode

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

-- (define asdf (a b) (display a) (display b))
-- becomes:
-- ((lambda () (define asdf (a b) (display a) (display b))))
-- which is consequently NOT MATCHED

{-

(
  (lambda ()
          (define asdf
                  (a b)
                  (display a)
                  (display b)
          )
  )
)

-}

-- TODO: Fix environments and toplevel evaluation
-- TODO TODO TODO: CURRYING!!!!!
-- TODO: function composition
-- TODO: Pattern matching:
--         (lambda (a (b (c '())) (display a) (display b) (display c))
-- TODO: atom to string conversion
-- TODO: import functionality

lispEval :: Expression -> IO Expression
lispEval x = do
  v <- createEnv >>= liftIO . newTVarIO
  catch (runReaderT (evalM x) v) onErr
  where
    onErr e = print (e :: SomeException) >> return Null
    evalM :: Expression -> StateM Expression
    evalM (Cell (Atom "eval") (Cell (String code) Null)) = do
      env <- getEnv
      evalM $ Procedure env [] (mconcat $ map fromConsList $ parseFelony code)
    evalM (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) = evalM iffalse 
    evalM (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) = evalM iftrue
    evalM (Cell (Atom "if") _) = error "Invalid special form: if."
    evalM (Cell (Atom "not") (Cell e Null)) = evalM e >>= \ev -> case ev of
      (Bool True)  -> return $ Bool False
      (Bool False) -> return $ Bool True
      _            -> error $ "Expression is not a bool."
    evalM (Cell (Atom "cons") (Cell a (Cell b Null))) = Cell <$> (evalM a) <*> (evalM b) -- TODO: Fix cons lists
    evalM (Cell (Atom "cons") _) = error "cons: Incorrect number of arguments."
    evalM (Cell (Atom "car") (Cell a _)) = return a
    evalM (Cell (Atom "car") e) = error $ "car: Cannot take the car of: " ++ (show e)
    evalM (Cell (Atom "cdr") (Cell _ b)) = return b
    evalM (Cell (Atom "cdr") e) = error $ "car: Cannot take the cdr of: " ++ (show e)
    evalM (Cell (Atom "integer?") (Cell e Null)) = evalM e >>= \ev -> case ev of
      (Integer _) -> return $ Bool True
      _           -> return $ Bool False
    evalM (Cell (Atom "real?") (Cell e Null)) = evalM e >>= \ev -> case ev of
      (Real _) -> return $ Bool True
      _        -> return $ Bool False
    evalM (Cell (Atom "string?") (Cell e Null)) = evalM e >>= \ev -> case ev of
      (String _) -> return $ Bool True
      _          -> return $ Bool False
    evalM (Cell (Atom "atom?") (Cell e Null)) = evalM e >>= \ev -> case ev of
      (Atom _) -> return $ Bool True
      _        -> return $ Bool False
    evalM (Cell (Atom "null?") (Cell e Null)) = evalM e >>= \ev -> case ev of
      Null       -> return $ Bool True
      _          -> return $ Bool False
    evalM (Cell (Atom "pair?") (Cell e Null)) = evalM e >>= \ev -> case ev of
      (Cell _ (Cell _ _)) -> return $ Bool False
      (Cell _ _)          -> return $ Bool True
      _                   -> return $ Bool False
    evalM (Cell (Atom "list?") (Cell lst Null)) = Bool <$> (evalM lst >>= return . isConsList)
    -- exprs that get translated into other exprs    (lambda (() . ))
    evalM (Cell (Atom "quote") (Cell e Null)) = return e
    evalM (Cell (Atom "lambda") (Cell keys vals)) = do
      env <- getEnv
      return $ Procedure env (map atomValue $ fromConsList keys) (fromConsList vals)
    evalM (Cell (Atom "lambda") e) = error $ "Invalid lambda: " ++ (show e)
    evalM (Cell (Atom "apply") (Cell a (Cell e@(Cell _ _) Null))) = evalM e >>= \args -> evalM (Cell a args)
    evalM (Cell (Atom "apply") (Cell a e)) = evalM e >>= \args -> evalM (Cell a args)
    evalM (Cell (Atom "apply") _) = error "Invalid special form: apply."
--------------------------------------------------------------------------------
    evalM (Cell (Atom "+") (Cell a (Cell b Null))) = lispMath (+) <$> (evalM a) <*> (evalM b)
    evalM (Cell (Atom "-") (Cell a (Cell b Null))) = lispMath (-) <$> (evalM a) <*> (evalM b)
    evalM (Cell (Atom "*") (Cell a (Cell b Null))) = lispMath (*) <$> (evalM a) <*> (evalM b)
    evalM (Cell (Atom "/") (Cell a (Cell b Null))) = lispMath (/) <$> (evalM a) <*> (evalM b)
    evalM (Cell (Atom "==") (Cell a (Cell b Null))) = return . Bool $ a == b
    -- IO-related
    evalM (Cell (Atom "display") (Cell e Null)) = (evalM e >>= liftIO . print) >> return Null
    -- Environment
    evalM (Cell (Atom "bind!") (Cell (Atom key) (Cell value Null))) = envInsert key value
    evalM (Cell (Atom "bind!") (Cell raw@(Cell _ _) (Cell v Null))) = do
      e <- evalM raw
      evalM $ Cell (Atom "bind!") (Cell e (Cell v Null)) -- FIXME: avoid extra eval
    evalM (Cell (Atom "bind!") _) = error $ "Invalid special form: bind!"
    -- Environment Lookup
    evalM (Atom key) = do
      lkup <- lookupEnv key
      case lkup of
        Just xpr -> return xpr
        Nothing -> error $ "Binding not found: " ++ key
    -- Procedure calling
    evalM (Cell fname@(Atom a) args) = do
      proc <- evalM fname
      case proc of
        Procedure _ _ _ -> evalM $ Cell proc args
        _ -> error $ "First argument " ++ a ++ " is not a procedure."
    -- Procedure evaluation
    evalM (Cell p@(Procedure procenv argNames bodies) e) = do
      args <- mapM evalM $ fromConsList e
      oldEnv <- getEnv
      putEnv procenv
      appendChildEnv $ H.fromList $ zip argNames args
      evaluation <- evalBodies bodies
      popChildEnv
      putEnv oldEnv
      return evaluation
      where
        evalBodies = foldM (\a b -> evalM b) Null -- TODO: backwards.....
    evalM (Cell expr xs) = do
      e <- evalM expr
      evalM $ Cell e xs
    -- primitive pass-throughs
    evalM expr@(Integer _)       = return expr
    evalM expr@(String _)        = return expr
    evalM expr@(Real _)          = return expr
    evalM expr@(Bool _)          = return expr
    evalM expr@(Procedure _ _ _) = return expr
    evalM expr@Null              = return expr
    evalM e = error $ "Invalid form: " ++ (show e)
    
{-
    
  State
  
-}

type StateM a = ReaderT (TVar Environment) IO a

getEnv :: StateM Environment
getEnv = ask >>= liftIO . readTVarIO

putEnv :: Environment -> StateM ()
putEnv v = ask >>= liftIO . atomically . flip writeTVar v

modifyEnv :: (Environment -> Environment) -> StateM ()
modifyEnv f = ask >>= liftIO . atomically . flip modifyTVar' f 

envInsert :: String -> Expression -> StateM Expression
envInsert k v = do
  vec <- getEnv
  V.modify vec f 0
  return v
  where
    f = H.insert k v
    
lookupEnv :: String -> StateM (Maybe Expression)
lookupEnv k = do
  vec <- getEnv
  if V.null vec
    then return Nothing
    else fmap (H.lookup k) $ V.read vec 0
  
popChildEnv :: StateM ()
popChildEnv = do
  vec <- getEnv
  putEnv $ V.slice 0 (V.length vec -1) vec
  
appendChildEnv :: HashMap String Expression -> StateM ()
appendChildEnv child = do
  vec <- getEnv
  liftIO $ print $ V.length vec
  bigger <- V.grow vec 1 -- we can do this because 1 is a constant we know to be positive.
  liftIO $ print $ V.length bigger
  V.write bigger (V.length vec) child
  putEnv bigger
  