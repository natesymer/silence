{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Silence.Expression
(
  Scope,
  PrimFunc,
  PtrFinalizer,
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
  fromNumber,
  car,
  cdr,
  compose,
  mkLambda,
  -- * Type Predicates
  isProc,
  isNumber,
  isString,
  isAtom,
  isNull,
  isList,
  isPair,
  isBool,
  isTruthy,
  -- * Misc
  invalidForm,
  lispVoid,
  showExpr
)
where
  
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Monoid
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char hiding (isNumber)
import Data.Ratio

import Foreign.Ptr

-- |Monad in which Lisp expressions are evaluated
newtype LispM a = LispM {
  runLispM :: StateT [Scope] IO a
} deriving (Functor,Applicative,Monad,MonadIO,MonadState [Scope])

type Scope = HashMap ByteString Expression
type PrimFunc = [Expression] -> LispM Expression
type PtrFinalizer = Ptr () -> IO ()

-- |A lisp expression.
data Expression = Atom ByteString
                | Number Rational
                | Bool Bool
                | Procedure
                    Bool -- @'True'@ if arguments should be evaluated in 'evaluate'
                    Int -- arity
                    PrimFunc -- state action
                | Null
                | Cell Expression Expression
                | Pointer (Ptr ()) PtrFinalizer

instance Show Expression where
  show = B.unpack . showExpr

instance Eq Expression where
  (Atom a) == (Atom b) = a == b
  (Number a) == (Number b) = a == b
  (Bool a) == (Bool b) = a == b
  Null == Null = True
  (Cell a as) == (Cell b bs) = a == b && as == bs
  (Pointer a _) == (Pointer b _) = a == b
  _ == _ = False

showExpr :: Expression -> ByteString
showExpr (Atom x) = x
showExpr (Number x) = B.pack $ either show show $ fromNumber x
showExpr Null = "()"
showExpr (Bool True)  = "#t"
showExpr (Bool False) = "#f"
-- TODO: print if the procedure evals args
showExpr (Procedure _ (-1) _) = "<procedure with indefinite arity>"
showExpr (Procedure _ argc _) = "<procedure with arity " <> (B.pack $ show argc) <> ">"
showExpr c@(Cell _ _) = "(" <> f "" c <> ")"
  where f acc Null = acc <> ""
        f acc (Cell a Null) = acc <> showExpr a
        f acc (Cell a b@(Cell _ _)) = f (acc <> showExpr a <> " ") b
        f acc (Cell a b) = acc <> showExpr a <> " . " <> showExpr b
        f _ _ = error "invalid cons list."
showExpr (Pointer p _) = B.pack $ show p

-- |Convert a rational into either an 'Integer' or a 'Double'        
fromNumber :: Rational -> Either Integer Double
fromNumber r
  | denominator r == 1 = Left $ numerator r
  | otherwise = Right $ fromRational r
  
-- |Lisp equivalent of Haskell's 'show'.
toLispStr :: Expression -> Expression
toLispStr = toIntList . showExpr
  where toIntList = B.foldr (Cell . Number . toRational . ord) Null

-- |Turns a lisp string into a Haskell 'String'.     
fromLispStr :: Expression -> Maybe String
fromLispStr = fromExpr integer
  where integer (Number x) = either (Just . chr . fromInteger) (const Nothing) $ fromNumber x
        integer _          = Nothing
        
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
        
-- |lisp @car@.
car :: Expression -> Maybe Expression
car (Cell v _) = Just v
car _          = Nothing

-- |lisp @cdr@.
cdr :: Expression -> Maybe Expression
cdr (Cell _ v) = Just v
cdr _          = Nothing

-- |compose two procedures of arbitrary arities. The result of 
-- second proc gets passed to the first. Does not matter if
-- this fully applies the first.
compose :: Expression -> Expression -> Maybe Expression
compose (Procedure _ barity b) (Procedure eargs aarity a) = 
  Just $ Procedure eargs aarity ((apply procb . pure =<<) . a)
  where procb = Procedure False barity b
compose _ _ = Nothing

-- |construct a lambda expression.
mkLambda :: Bool -> [Scope] -> Expression -> Expression -> Maybe Expression
mkLambda evalArgs cap a@(Atom _) body = mkLambda evalArgs cap (Cell a Null) body
mkLambda evalArgs cap args body = maybe Nothing (Just . lambda) (fromAtoms args)
  where lambda ["*"] = scoped (-1) (H.singleton "args" . toConsList)
        lambda xs = scoped (length xs) (H.fromList . zip xs)
        scoped arity f = Procedure evalArgs arity $ \as -> do
          modify' ((:) (mconcat $ (f as):cap))
          evaluate body <* modify' tail

-- |Specialized error message.
invalidForm :: String -> a
invalidForm = error . (++) "invalid form: "

lispVoid :: LispM a -> LispM Expression
lispVoid = (<$) Null

-- |Type predicates

isProc :: Expression -> Bool
isProc (Procedure _ _ _) = True
isProc _                 = False

isNumber :: Expression -> Bool
isNumber (Number _) = True
isNumber _          = False

isString :: Expression -> Bool
isString = isJust . fromLispStr

isAtom ::  Expression -> Bool
isAtom (Atom _) = True
isAtom _        = False

isNull :: Expression -> Bool
isNull Null = True
isNull _    = False

isList :: Expression -> Bool
isList Null        = True
isList (Cell _ xs) = isList xs
isList _           = False

isPair :: Expression -> Bool
isPair (Cell _ _) = True
isPair _          = False

isBool :: Expression -> Bool
isBool (Bool _) = True
isBool _        = False

isTruthy :: Expression -> Bool
isTruthy (Bool False) = False
isTruthy _            = True

{-|Evaluate an 'Expression'.

* Atoms are evaluated by looking their string value up in the environment.

* Cells are assumed to be s-expressions. They are evaluated as follows:

    * Evaluate the @car@ of the list

    * Use the haskell 'apply' function to evaluate the procedure with
      arguments given as a cons list in the @cdr@ of the cell.

* Values of all other "types" are not modified.

If an expression doesn't match any of the preceding rules, it is invalid.
The language will detect this and error out, providing an error message.
-}
evaluate :: Expression -> LispM Expression
evaluate (Cell x xs) = evaluate x >>= flip (maybe err . apply) (fromConsList xs)
  where err = error "invalid arguments: not a cons list"
evaluate (Atom a) = fromMaybe err . foldl f Nothing <$> get
  where f z x = maybe (H.lookup a x) Just z
        err = error $ "cannot find " ++ B.unpack a
evaluate x = return x

-- |Apply a procedure over arguments.
-- Evaluates arguments according to the evalargs flag.
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