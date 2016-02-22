{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Silence.Semantics (evaluate) where

import Silence.Expression

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import Control.Monad.State.Strict

{- |Evaluate an 'Expression'.
a. Cells are assumed to be s-expressions:
   1. Convert the cell into a list
   2. Evaluate each member of the list if the procedure requires it
   3. Apply the procedure in the head of the list to the tail of the list.
      • Procedure application is curried if the arity is positive
   If any of these assumptions are wrong, the expression is invalid and
   'evaluate' will error out with an appropriate error message.
b. Atoms are evaluated by looking their string value up in the environment.
c. Values of all other "types" are not modified.
-}
evaluate :: Expression -> LispM Expression
evaluate (Cell x xs) = evaluate x >>= f
  where f p@(Procedure True _ _) = maybe err (((apply p) =<<) . mapM evaluate) (fromConsList xs)
        f p@(Procedure False _ _) = maybe err (apply p) (fromConsList xs)
        f _ = error "invalid expression: car not a procedure"
        err = error "invalid expression: cdr not a cons list"
        apply    (Procedure _ (-1) act) as  = act as
        apply    (Procedure _ 0 act) []     = act []
        apply    (Procedure _ 0 _) _        = error "procedure applied too many times"
        apply p'@(Procedure _ _ _) []       = return p'
        apply    (Procedure e c act) (a:as) = apply (Procedure e (c-1) $ act . (:) a) as
        apply    _                 _      = error "invalid procedure"
evaluate (Atom a) = get >>= f
  where f (x:xs) = maybe (f xs) return $ H.lookup a x
        f [] = error $ "cannot find " ++ B.unpack a
evaluate x = return x