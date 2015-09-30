{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Felony.Evaluation
(
  lispEval,
  lispEval',
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

infiniteBicompare :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool
infiniteBicompare _ [] = True
infiniteBicompare _ [_] = True
infiniteBicompare f (x:y:rest)
 | f x y = infiniteBicompare f (y:rest)
 | otherwise = False

-------------------------------------------------------------------------------
-- | Felony core

evalCode :: String -> IO Expression
evalCode code = do
  env <- createEnv
  lispEval env $ Procedure env [] $ parseFelony code

evalProgram :: FilePath -> IO Expression
evalProgram fp = (readFile fp) >>= evalCode

-------------------------------------------------------------------------------
-- | higher order functions implemented using Expression

lispFoldl :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
lispFoldl _ z Null = z
lispFoldl f z (Cell x xs) = lispFoldl f (f z x) xs
lispFoldl _ _ e = error $ "Invalid list: " ++ show e

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

lispEval' :: Expression -> IO Expression
lispEval' expr = do
  env <- createEnv
  lispEval env expr

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

lispEval :: Environment -> Expression -> IO Expression
lispEval v x = do
  v' <- liftIO $ newTVarIO v
  catch (runReaderT (lispEvalM x) v') onErr
  where
    lispEvalM :: Expression -> StateM Expression
    lispEvalM expr = getEnv >>= (flip eval' $ expr)
    onErr e = lispVoid $ print (e :: SomeException)
    lispVoid a = a >> return Null
    eval' env (Cell (Atom "eval") (Cell (String code) Null)) = lispEvalM $ Procedure env [] (mconcat $ map fromConsList $ parseFelony code)
    eval' _   (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell iffalse _)))) = lispEvalM iffalse 
    eval' _   (Cell (Atom "if") (Cell (Bool True) (Cell iftrue _))) = lispEvalM iftrue
    eval' _   (Cell (Atom "if") _) = error "Invalid special form: if."
    eval' _   (Cell (Atom "not") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
          (Bool True)  -> return $ Bool False
          (Bool False) -> return $ Bool True
          _            -> error $ "Expression is not a bool."
    eval' _   (Cell (Atom "cons") (Cell a (Cell b Null))) = Cell <$> (lispEvalM a) <*> (lispEvalM b) -- TODO: Fix cons lists
    eval' _   (Cell (Atom "cons") _) = error "cons: Incorrect number of arguments."
    eval' _   (Cell (Atom "car") (Cell a _)) = return a
    eval' _   (Cell (Atom "car") e) = error $ "car: Cannot take the car of: " ++ (show e)
    eval' _   (Cell (Atom "cdr") (Cell _ b)) = return b
    eval' _   (Cell (Atom "cdr") e) = error $ "car: Cannot take the cdr of: " ++ (show e)
    eval' _   (Cell (Atom "integer?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Integer _) -> return $ Bool True
      _           -> return $ Bool False
    eval' _   (Cell (Atom "real?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Real _) -> return $ Bool True
      _        -> return $ Bool False
    eval' __  (Cell (Atom "string?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (String _) -> return $ Bool True
      _          -> return $ Bool False
    eval' _   (Cell (Atom "atom?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Atom _) -> return $ Bool True
      _        -> return $ Bool False
    eval' _   (Cell (Atom "null?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      Null       -> return $ Bool True
      _          -> return $ Bool False
    eval' _   (Cell (Atom "pair?") (Cell e Null)) = lispEvalM e >>= \ev -> case ev of
      (Cell _ (Cell _ _)) -> return $ Bool False
      (Cell _ _)          -> return $ Bool True
      _                   -> return $ Bool False
    eval' _   (Cell (Atom "list?") (Cell lst Null)) = Bool <$> (lispEvalM lst >>= return . isConsList)
    -- exprs that get translated into other exprs    (lambda (() . ))
    eval' _   (Cell (Atom "quote") (Cell e Null)) = return e
    eval' env (Cell (Atom "lambda") (Cell keys vals)) = return $ Procedure env (map atomValue $ fromConsList keys) (fromConsList vals)
    eval' _   (Cell (Atom "lambda") e) = error $ "Invalid lambda: " ++ (show e)
    eval' _   (Cell (Atom "apply") (Cell a (Cell e@(Cell _ _) Null))) = lispEvalM e >>= \args -> lispEvalM (Cell a args)
    eval' _   (Cell (Atom "apply") (Cell a e)) = lispEvalM e >>= \args -> lispEvalM (Cell a args)
    eval' _   (Cell (Atom "apply") _) = error "Invalid special form: apply."
    eval' _   (Cell (Atom "+") (Cell h t)) = lispFoldl (lispMath (+)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' _   (Cell (Atom "-") (Cell h t)) = lispFoldl (lispMath (-)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' _   (Cell (Atom "*") (Cell h t)) = lispFoldl (lispMath (*)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' _   (Cell (Atom "/") (Cell h t)) = lispFoldl (lispMath (/)) <$> (lispEvalM h) <*> (toConsList <$> (mapM lispEvalM $ fromConsList t))
    eval' _   (Cell (Atom "==") e) = Bool <$> (infiniteBicompare (==) <$> (mapM lispEvalM $ fromConsList e))
    -- IO-related
    eval' _   (Cell (Atom "display") (Cell e Null)) = (lispEvalM e >>= liftIO . print) >> return Null
    -- Environment
    eval' _   (Cell (Atom "bind!") (Cell (Atom key) (Cell value Null))) = envInsert key value
    eval' _   (Cell (Atom "bind!") (Cell raw@(Cell _ _) (Cell v Null))) = do
      e <- lispEvalM raw
      lispEvalM $ Cell (Atom "bind!") (Cell e (Cell v Null)) -- FIXME: avoid extra eval
    eval' _   (Cell (Atom "bind!") _) = error $ "Invalid special form: bind!"
    -- Environment Lookup
    eval' _   (Atom key) = do
      lkup <- lookupEnv key
      case lkup of
        Just xpr -> return xpr
        Nothing -> error $ "Binding not found: " ++ key
    -- Procedure calling
    eval' _   (Cell fname@(Atom a) args) = do
      proc <- lispEvalM fname
      case proc of
        Procedure _ _ _ -> lispEvalM $ Cell proc args
        _ -> error $ "First argument " ++ a ++ " is not a procedure."
    -- Procedure evaluation
    eval' _   (Cell (Procedure procenv argNames bodies) e) = do
      args <- mapM lispEvalM $ fromConsList e
      
      oldEnv <- getEnv
      putEnv procenv
      appendChildEnv $ H.fromList $ zip argNames args
      evaluation <- evalBodies bodies
      popChildEnv
      putEnv oldEnv
      
      return evaluation
      where
        evalBodies [] = return Null
        evalBodies [x] = lispEvalM x
        evalBodies (x:xs) = lispEvalM x >> evalBodies xs
    -- primitive pass-throughs
    eval' _   expr@(Integer _)       = return expr
    eval' _   expr@(String _)        = return expr
    eval' _   expr@(Real _)          = return expr
    eval' _   expr@(Bool _)          = return expr
    eval' _   expr@(Procedure _ _ _) = return expr
    eval' _   expr@Null              = return expr
    eval' _   e = error $ "Invalid form: " ++ (show e)
    
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
  V.unsafeGrow vec 1 -- we can do this because 1 is a constant we know to be positive.
  V.write vec ((V.length vec) - 1) child
  putEnv vec
  