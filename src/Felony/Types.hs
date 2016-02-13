{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Felony.Types
(
  Expression(..),
  LispM(..),
  EnvFrame,
  Environment(..)
)
where
  
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Monoid
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

newtype LispM a = LispM {
  runLispM :: StateT Environment IO a
} deriving (Functor,Applicative,Monad,MonadIO,MonadState Environment)

type EnvFrame = HashMap ByteString Expression
data Environment = Frame Environment EnvFrame | Empty deriving (Show)

-- TODO: List-based string implementation

data Expression = Atom ByteString
                | String ByteString
                | Integer Integer
                | Real Double
                | LispTrue
                | LispFalse
                | Procedure Int ([Expression] -> LispM Expression)
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
showExpr (Procedure argc _) = "<<procedure arity:" <> (B.pack $ show argc) <> ">>"
showExpr c@(Cell _ _) = "(" <> f c <> ")"
  where f Null = "" 
        f (Cell a Null) = showExpr a
        f (Cell a b@(Cell _ _)) = showExpr a <> " " <> f b
        f (Cell a b) = showExpr a <> " . " <> showExpr b
        f _ = error "invalid cons list."