{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, MagicHash #-}
module Silence.Expression
(
  Scope,
  PrimFunc,
  Expression(..),
  LispM(..),
  -- * Evaluation
  evaluate,
  apply,
  -- * 'Expression' Transforms
  toLispStr,
  fromLispStr,
  fromConsList,
  toConsList,
  fromExpr,
  fromAtoms,
  envToAssoc,
  -- * Misc
  invalidForm,
  showExpr
)
where
  
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Monoid
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import GHC.Integer.GMP.Internals
import GHC.Types
import GHC.Prim
-- import System.Posix.IO
import Debug.Trace

-- |Monad in which Lisp expressions are evaluated
newtype LispM a = LispM {
  runLispM :: StateT [Scope] IO a
} deriving (Functor,Applicative,Monad,MonadIO,MonadState [Scope])

type Scope = HashMap ByteString Expression
type PrimFunc = [Expression] -> LispM Expression -- TODO: Make this a state action: [Expression] -> [Scope] -> IO (Expression,[Scope])
          

-- |A lisp expression.
data Expression = Atom ByteString
                | Integer Integer
                | Real Double
                | Bool Bool
                -- | FD Fd
                | Environment [Scope]
                | Procedure
                    Bool -- @'True'@ if arguments should be evaluated in 'evaluate'
                    Int -- arity
                    PrimFunc -- state action
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
  (Environment a) == (Environment b) = a == b
  _ == _ = False

showExpr :: Expression -> ByteString
showExpr (Atom x) = x
showExpr (Integer x) = B.pack $ show x -- TODO: better means of showing 'Integers's
showExpr (Real x) = B.pack $ show x -- TODO: better means of showing 'Double's (that doesn't use sci notation)
showExpr Null = "()"
showExpr (Bool True)  = "#t"
showExpr (Bool False) = "#f"
showExpr (Environment e) = showExpr $ envToAssoc e
-- TODO: print if the procedure evals args
showExpr (Procedure _ (-1) _) = "<procedure with indefinite arity>"
showExpr (Procedure _ argc _) = "<procedure with arity " <> (B.pack $ show argc) <> ">"
showExpr c@(Cell _ _) = "(" <> f "" c <> ")"
  where f acc Null = acc <> ""
        f acc (Cell a Null) = acc <> showExpr a
        f acc (Cell a b@(Cell _ _)) = f (acc <> showExpr a <> " ") b
        f acc (Cell a b) = acc <> showExpr a <> " . " <> showExpr b
        f _ _ = error "invalid cons list."
        
-- |Turn an environment (@['Scope']@) into a lisp assoc list.
envToAssoc :: [Scope] -> Expression
envToAssoc = foldr f Null
  where f = Cell . foldr Cell Null . map (uncurry (Cell . Atom)) . H.toList

-- |Lisp equivalent of Haskell's 'show'.
toLispStr :: Expression -> Expression
toLispStr = toIntList . showExpr
  where toIntList = B.foldr (Cell . Integer . fastOrdInteger) Null
        fastOrdInteger (C# c) = S# (ord# c)

-- |Turns a lisp string into a Haskell 'String'.     
fromLispStr :: Expression -> Maybe String
fromLispStr = fromExpr integer
  where integer (Integer x) = Just $ chr $ fromInteger x
        integer _           = Nothing
        
-- |Transform a cons list into a haskell list. It builds a function 
-- that takes an empty list and returns a list of expressions.
-- eg: (@(Just $ id . (:) x . (:) x) \<*\> (Just [])@)
fromConsList :: Expression -> Maybe [Expression]
fromConsList = f (Just id)
  where f acc Null = acc <*> Just []
        f acc (Cell x xs) = f (fmap (. (:) x) acc) xs
        f _ _ = Nothing
    
-- |Make a cons list out of a Haskell list.    
toConsList :: [Expression] -> Expression
toConsList = foldr Cell Null
  
-- |Coerce lisp cons list into a homogeneous haskell
-- list of haskell values. For example: to turn a cons list
-- of 'Atom's into a haskell list of 'ByteStrings', see 'fromAtoms'.
fromExpr :: (Expression -> Maybe a) -- ^ function to unbox an expression
         -> Expression -- ^ hopefully a cons list
         -> Maybe [a]
fromExpr f = (>>= (foldr f' (Just []))) . fromConsList
  where f' = (<*>) . fmap (:) . f
  
-- |Turn a cons list into a list of bytestrings, if each element is an Atom.
fromAtoms :: Expression -> Maybe [ByteString]
fromAtoms = fromExpr f
  where f (Atom a) = Just a
        f _ = Nothing

-- |Specialized error message.
invalidForm :: String -> LispM a
invalidForm = error . (++) "invalid form: "

-- TODO: Fix @evaluate@ procedure when used like @(evaluate (evaluate '+))@.
-- That returns @(Atom "+")@ whereas @((. evaluate evaluate) '+)@ evaluates to
-- a @Procedure@. This is probably a bug in evaluating nested proc calls.
        
{-|Evaluate an 'Expression'.

* Atoms are evaluated by looking their string value up in the environment.

* Values of all other "types" are not modified.

* Cells are assumed to be s-expressions. They are evaluated as follows:

    * Convert the cell into a list

    * Evaluate each member of the list if the procedure requires it

    * Apply the procedure in the head of the list to the tail of the list.

         * Procedure application is curried if the arity is positive

If an expression doesn't match any of the preceding rules, it is invalid.
The language will detect this and error out, providing an error message.
-}
evaluate :: Expression -> LispM Expression
evaluate (Cell x xs) = evaluate x >>= f
  where f p@(Procedure _ _ _) = maybe err (apply p) (fromConsList xs)
        f _ = error "invalid expression: car is not a procedure"
        err = error "invalid expression: cdr is not a cons list"
evaluate (Atom a) = get >>= f
  where f (x:xs) = maybe (f xs) return $ H.lookup a x
        f [] = error $ "cannot find " ++ B.unpack a
evaluate x = return x

-- |Apply a procedure over arguments. Honors evalargs flag.
-- Allows partial application.
apply :: Expression -> [Expression] -> LispM Expression
apply    (Procedure False (-1) act) as  = act as
apply    (Procedure True (-1) act)  as  = mapM evaluate as >>= act
apply    (Procedure _ 0 act) []         = act []
apply    (Procedure _ 0 _) _            = error "procedure applied too many times"
apply p'@(Procedure _ _ _) []           = return p'
apply    (Procedure False c act) (a:as) = apply (Procedure False (c-1) (act . (:) a)) as
apply    (Procedure True c act)  (a:as) = evaluate a >>= \a' -> apply (Procedure True (c-1) (act . (:) a')) as
apply    _                       _      = error "invalid procedure"