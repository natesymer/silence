{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, MagicHash #-}
module Felony.Types
(
  PrimFunc,
  Expression(..),
  LispM(..),
  EnvFrame,
  Environment(..),
  toLispStr,
  fromLispStr,
  fromConsList,
  fromExpr,
  invalidForm
)
where
  
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Monoid
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import GHC.Integer.GMP.Internals
import GHC.Types
import GHC.Prim

newtype LispM a = LispM {
  runLispM :: StateT Environment IO a
} deriving (Functor,Applicative,Monad,MonadIO,MonadState Environment)

type EnvFrame = HashMap ByteString Expression
data Environment = Frame Environment EnvFrame | Empty deriving (Show)

type PrimFunc = [Expression] -> LispM Expression

data Expression = Atom ByteString
                | Integer Integer
                | Real Double
                | Bool Bool
                | Procedure Int PrimFunc
                | Null
                | Cell Expression Expression 

instance Show Expression where
  show = B.unpack . showExpr

instance Eq Expression where
  (Atom a) == (Atom b) = a == b
  (Real a) == (Real b) = a == b
  (Integer a) == (Real b) = (fromInteger a) == b
  (Real b) == (Integer a) = b == (fromInteger a)
  (Integer a) == (Integer b) = a == b
  (Bool a) == (Bool b) = a == b
  Null == Null = True
  (Cell a as) == (Cell b bs) = a == b && as == bs
  _ == _ = False

showExpr :: Expression -> ByteString
showExpr (Cell (Atom "quote") (Cell e Null)) = "'" <> showExpr e
showExpr (Atom x) = x
showExpr (Integer x) = B.pack $ show x -- TODO: better means of showing 'Integers's
showExpr (Real x) = B.pack $ show x -- TODO: better means of showing 'Double's (that doesn't use sci notation)
showExpr Null = "()"
showExpr (Bool True)  = "#t"
showExpr (Bool False) = "#f"
showExpr (Procedure argc _) = "<procedure with arity " <> (B.pack $ show argc) <> ">"
showExpr c@(Cell _ _) = "(" <> f "" c <> ")"
  where f acc Null = acc <> ""
        f acc (Cell a Null) = acc <> showExpr a
        f acc (Cell a b@(Cell _ _)) = f (acc <> showExpr a <> " ") b
        f acc (Cell a b) = acc <> showExpr a <> " . " <> showExpr b
        f _ _ = error "invalid cons list."

-- |Lisp equivalent of Haskell's 'show'.
toLispStr :: Expression -> Expression
toLispStr = toIntList . showExpr
  where toIntList = B.foldr (Cell . Integer . fastOrdInteger) Null
        fastOrdInteger (C# c) = S# (ord# c)
   
-- |Turns a lisp string into a Haskell 'String'.     
fromLispStr :: Expression -> Maybe String
fromLispStr = fromExpr integer
  where integer (Integer x) = Just $ fastChr x
        integer _           = Nothing
        fastChr (S# i) = C# (chr# i)
        fastChr x = chr $ fromInteger x
        
-- |Transform a cons list into a haskell list. It builds a function 
-- that takes an empty list and returns a list of expressions.
-- eg: (@(Just $ id . (:) x . (:) x) <*> (Just [])@)
fromConsList :: Expression -> Maybe [Expression]
fromConsList = f (Just id)
  where f acc Null = acc <*> Just []
        f acc (Cell x xs) = f (fmap (. (:) x) acc) xs
        f _ _ = Nothing
  
-- |Coerce lisp cons list into a homogeneous haskell
-- list of haskell values.
fromExpr :: (Expression -> Maybe a) -> Expression -> Maybe [a]
fromExpr f = (>>= (foldr f' (Just []))) . fromConsList
  where f' = (<*>) . fmap (:) . f

invalidForm :: String -> LispM a
invalidForm = error . (++) "invalid special form: "