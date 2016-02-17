{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Felony.Semantics
(
  evaluate,
  mkLambda
)
where
  
import Felony.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import Control.Monad.State.Strict

-- |Evaluate an 'Expression'.
-- Cells are assumed to be s-expressions:
-- 1. Convert the cell into a list
-- 2. Evaluate each member of the list
-- 3. Apply the procedure in the head of the list to the tail of the list.
-- If either of these assumptions are wrong, the expression is invalid and
-- 'evaluate' will error out with an appropriate error message.
--
-- Certain forms do not follow this pattern, namely:
-- • @if@: evaluate the first argument and evaluate the next two accordingly.
-- • @quote@: don't evaluate any arguments, just return the first one.
-- • @unquote@: cancel out quote so that the expression behaves like an unquoted value.
-- • @define@: @(define atom args bodies)@ becomes @(bind! atom (lambda args bodies))@
-- • @lambda@: don't evaluate anything; lisp wrapper around mkLambda
--
-- Atoms are evaluated by looking their string value up in the environment.
--
-- Values of all other "types" are not modified.
evaluate :: Expression -> LispM Expression
evaluate (Cell (Atom "if") (Cell x (Cell t (Cell f Null)))) = evaluate x >>= fn
  where fn (Bool b) = evaluate (if b then t else f)
        fn _ = invalidForm "if"
evaluate (Cell (Atom "quote") (Cell v Null)) = return v
evaluate (Cell (Atom "quote") _) = invalidForm "quote"
evaluate (Cell (Atom "unquote") (Cell (Atom "quote") (Cell v Null))) = evaluate v
evaluate (Cell (Atom "unquote") _) = invalidForm "unquote"
evaluate (Cell (Atom "define") (Cell a (Cell car cdr))) =
  evaluate (Cell (Atom "bind!") (Cell a (Cell (Cell (Atom "lambda") (Cell car cdr)) Null)))
evaluate (Cell (Atom "lambda") (Cell car cdr)) = maybe (invalidForm "lambda") return lambda
    where lambda = mkLambda <$> (fromExpr atom car) <*> (fromConsList cdr)
          atom (Atom a) = Just a
          atom _        = Nothing
evaluate (Cell (Atom "lambda") _) = invalidForm "lambda"
evaluate (Cell x xs) = evaluate x >>= f
  where f p@(Procedure _ _) = maybe err (((apply p) =<<) . mapM evaluate) (fromConsList xs)
          where err = error "invalid expression: cdr not a cons list"
                apply    (Procedure 0 act) []     = act []
                apply    (Procedure 0 _) _        = error "procedure applied too many times"
                apply p'@(Procedure _ _) []       = return p'
                apply    (Procedure c act) (a:as) = apply (Procedure (c-1) $ act . (:) a) as
                apply    _                 _      = error "invalid procedure"
        f _ = error "invalid expression: car not a procedure"
evaluate (Atom a) = get >>= f
  where f (Frame xs x) = maybe (f xs) return $ H.lookup a x
        f Empty = error $ "cannot find " ++ B.unpack a
evaluate x = return x

-- |Construct a lambda from bindings and bodies.
mkLambda :: [ByteString] -- ^ bindings
         -> [Expression] -- ^ bodies
         -> Expression
mkLambda bindings bodies = Procedure (length bindings) $ \args -> do
  modify (push args)
  rets <- mapM evaluate bodies
  modify pop
  return $ last rets
  where push args xs = Frame xs $ H.fromList $ zip bindings args
        pop Empty = error "empty stack"
        pop (Frame xs _) = xs