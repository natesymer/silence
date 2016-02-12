{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Felony.Types
(
  Expression(..),
  LispM(..),
  EnvFrame,
  Environment(..)
)
where
  
import Control.Monad.IO.Class

import Data.Monoid
  
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
  
-- TODO: rewrite in terms of RWST or StateT
newtype LispM a = LispM {
  runLispM :: Environment -> IO (a,Environment,Expression)
}

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

type EnvFrame = HashMap ByteString Expression
data Environment = Frame Environment EnvFrame | Empty deriving (Show)

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