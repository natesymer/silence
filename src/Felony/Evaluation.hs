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
    
--------------------------------------------------------------------------------
evalM :: Expression -> StateM Expression
evalM (Cell (Atom "eval") (Cell (String code) Null)) = do
  env <- getEnv
  evalM $ Procedure env [] (mconcat $ map fromConsList $ parseFelony code)
evalM (Cell (Atom "if") (Cell (Bool False) (Cell _ (Cell x Null)))) = evalM x
evalM (Cell (Atom "if") (Cell (Bool True) (Cell x (Cell _ Null)))) = evalM x
evalM (Cell (Atom "if") _) = error "Invalid special form: if"
evalM (Cell (Atom "not") (Cell e Null)) = evalM e >>= f
  where f (Bool True)  = return $ Bool False
        f (Bool False) = return $ Bool True
        f _            = error $ "Expression is not a bool."
evalM (Cell (Atom "cons") (Cell a (Cell b Null))) = Cell <$> (evalM a) <*> (evalM b) -- TODO: Fix cons lists
evalM (Cell (Atom "cons") _) = error "cons: Incorrect number of arguments."
evalM (Cell (Atom "car") (Cell a _)) = return a
evalM (Cell (Atom "car") e) = error $ "car: Cannot take the car of: " ++ (show e)
evalM (Cell (Atom "cdr") (Cell _ b)) = return b
evalM (Cell (Atom "cdr") e) = error $ "car: Cannot take the cdr of: " ++ (show e)
evalM (Cell (Atom "integer?") (Cell e Null)) = f <$> evalM e
  where f (Integer _) = Bool True
        f _             = Bool False
evalM (Cell (Atom "real?") (Cell e Null)) = f <$> evalM e
  where f (Real _) = Bool True
        f _        = Bool False
evalM (Cell (Atom "string?") (Cell e Null)) = f <$> evalM e
  where f (String _) = Bool True
        f _          = Bool False
evalM (Cell (Atom "atom?") (Cell e Null)) = f <$> evalM e
  where f (Atom _) = Bool True
        f _        = Bool False
evalM (Cell (Atom "null?") (Cell e Null)) = f <$> evalM e
  where f Null = Bool True
        f _    = Bool False
evalM (Cell (Atom "pair?") (Cell e Null)) = f <$> evalM e
  where f (Cell _ (Cell _ _)) = Bool False
        f (Cell _ _)          = Bool True
        f _                   = Bool False
evalM (Cell (Atom "list?") (Cell e Null)) = evalM e >>= return . Bool . isConsList
-- exprs that get translated into other exprs    (lambda (() . ))
evalM (Cell (Atom "quote") (Cell e Null)) = return e
evalM (Cell (Atom "lambda") (Cell args@(Cell _ _) bodies@(Cell _ _))) = do -- (lambda (arg1 arg2) (display (+ arg1 arg2))
  env <- getEnv
  return $ Procedure env (map atomValue $ fromConsList args) (fromConsList bodies)
evalM (Cell (Atom "lambda") e) = error $ "Invalid lambda: " ++ (show e)
evalM (Cell (Atom "apply") (Cell a (Cell e@(Cell _ _) Null))) = evalM e >>= evalM . Cell a -- (apply 'funcname '(1 2 3))
evalM (Cell (Atom "apply") (Cell a e@(Cell _ _))) = evalM e >>= evalM . Cell a -- (apply 'funcname 1 2 3)
evalM (Cell (Atom "apply") _) = error "Invalid special form: apply."
-- math
evalM (Cell (Atom "+") (Cell a (Cell b Null))) = lispMath (+) <$> (evalM a) <*> (evalM b)
evalM (Cell (Atom "-") (Cell a (Cell b Null))) = lispMath (-) <$> (evalM a) <*> (evalM b)
evalM (Cell (Atom "*") (Cell a (Cell b Null))) = lispMath (*) <$> (evalM a) <*> (evalM b)
evalM (Cell (Atom "/") (Cell a (Cell b Null))) = lispMath (/) <$> (evalM a) <*> (evalM b)
evalM (Cell (Atom "==") (Cell a (Cell b Null))) = return . Bool $ a == b
-- IO-related
evalM (Cell (Atom "display") (Cell e Null)) = (evalM e >>= liftIO . print) >> return Null
-- Environment
evalM (Cell (Atom "let!") (Cell (Atom k) (Cell v Null))) = envInsert k v
evalM (Cell (Atom "let!") (Cell raw@(Cell _ _) (Cell v Null))) = evalM raw >>= f
  where f (Atom k) = envInsert k v
        f _ = error $ mconcat ["Invalid expression (first argument must be an atom): ", show raw]
evalM (Cell (Atom "let!") _) = error "Invalid special form: let!"
-- Environment Lookup
evalM (Atom k) = lookupEnv k >>= f
  where f (Just xpr) = return xpr
        f Nothing    = error $ "Binding not found: " ++ k
-- Procedure calling
evalM (Cell name@(Atom _) args) = evalM name >>= f
  where
    f proc@(Procedure _ _ _) = evalM $ Cell proc args
    f xpr = error $ mconcat ["Not a procedure: ", show xpr]
-- Procedure evaluation
evalM (Cell (Procedure procenv argNames bodies) e) = do
  args <- mapM evalM $ fromConsList e
  oldEnv <- getEnv
  putEnv procenv
  appendChildEnv $ H.fromList $ zip argNames args
  evaluation <- evalBodies bodies
  popChildEnv
  putEnv oldEnv
  return evaluation
  where
    evalBodies = foldM (\_ b -> evalM b) Null -- TODO: backwards.....
-- primitive pass-throughs
evalM x@(Integer _)       = return x
evalM x@(String _)        = return x
evalM x@(Real _)          = return x
evalM x@(Bool _)          = return x
evalM x@(Procedure _ _ _) = return x
evalM x@Null              = return x
evalM x = error $ "Invalid form: " ++ (show x)
    
{-
    
  State
  
-}

type StateM a = ReaderT (TVar Environment) IO a

getEnv :: StateM Environment
getEnv = ask >>= liftIO . readTVarIO

putEnv :: Environment -> StateM ()
putEnv v = ask >>= liftIO . atomically . flip writeTVar v

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
  bigger <- V.grow vec 1 -- we can do this because 1 is a constant we know to be positive.
  V.write bigger (V.length vec) child
  putEnv bigger
  