{-# LANGUAGE OverloadedStrings, TupleSections, GADTs, Rank2Types #-}

module Felony.Lisp
(
  LispM(..),
  Expression(..),
  Environment,
  evalExpressions,
  evaluate,
  toConsList,
  mkLambda
) where

import Control.Monad.IO.Class
  
import Data.Monoid

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
  
newtype LispM a = LispM {
  runLispM :: Environment -> IO (a,Environment,Expression)
}

evalExpressions :: [Expression] -> IO Expression
evalExpressions e = thrd <$> runLispM (evaluate e') Empty
  where
    e' = Cell (mkLambda [] e) Null -- Wrap with lambda evaluation
    thrd (_,_,v) = v

instance Functor LispM where
  fmap f m = LispM $ \env -> fmap f' $ runLispM m env
    where f' (a,env',expr') = (f a,env',expr')
  
instance Applicative LispM where
  pure a = LispM $ \env -> return (a,env,Null)
  m1 <*> m2 = LispM $ \env -> do
    (f,env',_) <- runLispM m1 env
    (a,env'',expr) <- runLispM m2 env'
    return (f a, env'', expr)

instance Monad LispM where
  fail msg = LispM $ \_ -> fail msg
  m >>= k = LispM $ \env -> do
    (a,env',_) <- runLispM m env
    runLispM (k a) env'

instance MonadIO LispM where
  liftIO io = LispM $ \env -> fmap (,env,Null) io

data Expression = Atom ByteString
                | String ByteString
                | Integer Integer
                | Real Double
                | LispTrue
                | LispFalse
                | Procedure ([Expression] -> LispM ())
                | Null
                | Cell Expression Expression 

instance Show Expression where
  show = B.unpack . showExpr

instance Eq Expression where
  (Atom a) == (Atom b) = a == b
  (String a) == (String b) = a == b
  (Real a) == (Real b) = a == b
  (Integer a) == (Real b) = (fromInteger a) == b
  (Real b) == (Integer a) = b == (fromInteger a)
  (Integer a) == (Integer b) = a == b
  LispTrue == LispTrue = True
  LispFalse == LispFalse = True
  Null == Null = True
  (Cell a as) == (Cell b bs) = a == b && as == bs
  _ == _ = False

showExpr :: Expression -> ByteString
showExpr (Cell (Atom "quote") (Cell e Null)) = "'" <> showExpr e
showExpr (Atom x) = x
showExpr (String x) = x
showExpr (Integer x) = B.pack $ show x
showExpr (Real x) = B.pack $ show x
showExpr Null = "()"
showExpr LispTrue  = "#t"
showExpr LispFalse = "#f"
showExpr (Procedure _) = "<<procedure>>"
showExpr c@(Cell _ _) = "(" <> f c <> ")"
  where f Null = "" 
        f (Cell a Null) = showExpr a
        f (Cell a b@(Cell _ _)) = showExpr a <> " " <> f b
        f (Cell a b) = showExpr a <> " . " <> showExpr b
        f _ = error "invalid cons list."
      
invalidForm :: String -> LispM ()
invalidForm = lispError . (++) "invalid special form: "
        
-- |Primitive procedures.
primitives :: EnvFrame
primitives = H.fromList [
  ("if",Procedure ifE),
  ("not",Procedure notE),
  ("cons",Procedure consE),
  ("car",Procedure carE),
  ("cdr",Procedure cdrE),
  ("==",Procedure eqlE),
  ("+",Procedure addE),
  ("-", Procedure subE),
  ("*", Procedure mulE),
  ("/", Procedure divE),
  ("display", Procedure displayE),
  ("let!", Procedure letBangE),
  ("integer?", Procedure isIntegerE),
  ("real?", Procedure isRealE),
  ("string?", Procedure isStringE),
  ("atom?", Procedure isAtomE),
  ("null?", Procedure isNullE),
  ("list?", Procedure isListE),
  ("pair?", Procedure isPairE)
  ]
  where
    ifE (LispTrue:expr:_)  = evaluate expr
    ifE [LispFalse,_,expr] = evaluate expr
    ifE _                  = invalidForm "if"
    notE [LispFalse] = returnExpr LispTrue
    notE [LispTrue]  = returnExpr LispFalse
    notE _           = invalidForm "not"
    consE [a,b] = returnExpr $ Cell a b
    consE _     = invalidForm "cons"
    carE [Cell v _] = returnExpr v
    carE _          = invalidForm "car"
    cdrE [Cell _ v] = returnExpr v
    cdrE _          = invalidForm "cdr"
    displayE = mapM_ (liftIO . print)
    letBangE [Atom k, v] = insertEnv k v
    letBangE _           = invalidForm "let!"
    isIntegerE [Integer _] = returnExpr LispTrue
    isIntegerE [_]         = returnExpr LispFalse
    isIntegerE _           = invalidForm "integer?"
    isRealE [Real _] = returnExpr LispTrue
    isRealE [_]      = returnExpr LispFalse
    isRealE _        = invalidForm "real?"
    isStringE [String _] = returnExpr LispTrue
    isStringE [_]        = returnExpr LispFalse
    isStringE _          = invalidForm "string?"
    isAtomE [Atom _] = returnExpr LispTrue
    isAtomE [_]      = returnExpr LispFalse
    isAtomE _        = invalidForm "atom?"
    isNullE [Null] = returnExpr LispTrue
    isNullE [_]    = returnExpr LispFalse
    isNullE _      = invalidForm "null?"
    isListE [Cell _ xs] = isListE [xs]
    isListE [Null]      = returnExpr LispTrue
    isListE _           = invalidForm "list?"
    isPairE [Cell _ (Cell _ _)] = returnExpr LispFalse -- TODO: verify this
    isPairE [Cell _ _]          = returnExpr LispTrue
    isPairE _                   = invalidForm "pair?"
    addE [Integer a, Integer b] = returnExpr $ Integer $ a + b
    addE [Integer a, Real b]    = returnExpr $ Real $ (fromInteger a) + b
    addE [Real a, Integer b]    = returnExpr $ Real $ a + (fromInteger b)
    addE [Real a, Real b]       = returnExpr $ Real $ a + b
    addE _                      = invalidForm "+"
    subE [Integer a, Integer b] = returnExpr $ Integer $ a - b
    subE [Integer a, Real b]    = returnExpr $ Real $ (fromInteger a) - b
    subE [Real a, Integer b]    = returnExpr $ Real $ a - (fromInteger b)
    subE [Real a, Real b]       = returnExpr $ Real $ a - b
    subE _                      = invalidForm "-"
    mulE [Integer a, Integer b] = returnExpr $ Integer $ a * b
    mulE [Integer a, Real b]    = returnExpr $ Real $ (fromInteger a) * b
    mulE [Real a, Integer b]    = returnExpr $ Real $ a * (fromInteger b)
    mulE [Real a, Real b]       = returnExpr $ Real $ a * b
    mulE _                      = invalidForm "*"
    divE [Integer a, Integer b] = returnExpr $ Real $ (fromInteger a) / (fromInteger b)
    divE [Integer a, Real b]    = returnExpr $ Real $ (fromInteger a) / b
    divE [Real a, Integer b]    = returnExpr $ Real $ a / (fromInteger b)
    divE [Real a, Real b]       = returnExpr $ Real $ a / b
    divE _                      = invalidForm "/"
    eqlE [a,b]                  = returnExpr $ if a == b then LispTrue else LispFalse
    eqlE _                      = invalidForm "=="

-- |Throw an error.
lispError :: String -> LispM ()
lispError = error

-- |Evaluate an expression
evaluate :: Expression -> LispM ()
evaluate (Cell (Atom "quote") (Cell v Null)) = returnExpr v
evaluate (Cell (Atom "quote") _) = invalidForm "quote"
evaluate (Cell (Atom "lambda") (Cell bindings bodies)) = do
  case (fromConsList bindings >>= fromAtoms, fromConsList bodies) of
    (Just bindings', Just bodies') -> returnExpr $ mkLambda bindings' bodies'
    _ -> invalidForm "lambda"
  where
    fromAtoms = foldr f (Just [])
      where f (Atom a) b = ((:) a) <$> b
            f _        _ = Nothing
evaluate (Cell (Atom "lambda") _) = invalidForm "lambda"
evaluate (Cell x xs) = (getReturnedExpr $ evaluate x) >>= f
  where f (Procedure act) = do
          case fromConsList xs of
            Nothing -> error "invalid s-expression: cdr not a cons list."
            Just xs' -> mapM (getReturnedExpr . evaluate) xs' >>= act
        f _ = error "invalid s-expression: car not a procedure."
evaluate (Atom a) = do
  LispM $ \env -> f env $ Frame env primitives
  where
    f env (Frame xs x) = maybe (f env xs) (return . ((),env,)) $! H.lookup a x
    f _ Empty = error $ "cannot find " ++ B.unpack a
evaluate x = returnExpr x

returnExpr :: Expression -> LispM ()
returnExpr e = LispM $ \env -> return ((),env,e)

getReturnedExpr :: LispM () -> LispM Expression
getReturnedExpr (LispM f) = LispM $ \env -> (\(_,env',e) -> (e,env',Null)) <$> f env

-- |Transform a cons list into a haskell list.
fromConsList :: Expression -> Maybe [Expression]
fromConsList = f $ Just []
  where f acc Null = acc
        f acc (Cell x xs) = f (fmap (flip (++) [x]) acc) xs
        f _ _ = Nothing

-- |Transform a Haskell list into a cons list.
toConsList :: [Expression] -> Expression
toConsList = foldr Cell Null

-- |Construct a lambda from bindings and bodies.
mkLambda :: [ByteString] -> [Expression] -> Expression
mkLambda bindings bodies = Procedure $ \args -> do
  pushEnvFrame $ H.fromList $ zip bindings args
  rets <- mapM (getReturnedExpr . evaluate) bodies
  popEnvFrame
  returnExpr $ last rets

type EnvFrame = HashMap ByteString Expression
data Environment = Frame Environment EnvFrame | Empty deriving (Show)

-- |Pop a "stack frame".
popEnvFrame :: LispM ()
popEnvFrame = LispM f
  where f Empty = error "Cannot pop empty stack."
        f (Frame xs _) = return ((),xs,Null)

-- |Push a "stack frame"
pushEnvFrame :: EnvFrame -> LispM ()
pushEnvFrame child = LispM $ \env -> return ((),Frame env child,Null)

-- |Insert a value into the environment.
insertEnv :: ByteString -> Expression -> LispM ()
insertEnv k v = LispM f
  where f Empty = error "No stack frame!"
        f (Frame xs x) = return ((),Frame xs (H.insert k v x),v)