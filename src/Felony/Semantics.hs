{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Felony.Semantics
(
  LispM,
  Environment(..),
  evalExpressions,
  evalExpressions'
)
where

{- TODO:
* FIXME quotes in strings
* vectors?
* call/cc & concurrency
* first class environments
* standard library
* rethink strings (list of atoms?, integers?)
* more printing facilities involving strings
* I/O (console, files)
* math ops (trig, rounding)
* maybe pattern matching
-}
  
import Felony.Types
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
 
-- |Evaluate a list of 'Expression's in a new environment.
evalExpressions' :: [Expression] -> IO (Expression,Environment)
evalExpressions' = evalExpressions Empty
 
-- |Evaluate a list of 'Expression's in a given environment.
evalExpressions :: Environment -> [Expression] -> IO (Expression,Environment)
evalExpressions env es = runStateT (runLispM $ evaluate l) (Frame env primitives)
  where l = Cell (mkLambda [] es) Null -- wrap expressions in a lambda

invalidForm :: String -> LispM a
invalidForm = error . (++) "invalid special form: "

lispBool :: Bool -> Expression
lispBool True = LispTrue
lispBool False = LispFalse

-- |Evaluate an 'Expression'.
evaluate :: Expression -> LispM Expression
evaluate (Cell (Atom "if") (Cell x (Cell t (Cell f Null)))) = evaluate x >>= fn
  where fn LispTrue = evaluate t
        fn LispFalse = evaluate f
        fn _ = invalidForm "if"
evaluate (Cell (Atom "quote") (Cell v Null)) = return v
evaluate (Cell (Atom "quote") _) = invalidForm "quote"
evaluate (Cell (Atom "define") (Cell a (Cell car cdr))) =
  evaluate (Cell (Atom "bind!") (Cell a (Cell (Cell (Atom "lambda") (Cell car cdr)) Null)))
evaluate (Cell (Atom "lambda") (Cell car cdr)) = maybe (invalidForm "lambda") return lambda
    where lambda = mkLambda <$> (fromConsList car >>= fromAtoms) <*> (fromConsList cdr)
          fromAtoms = foldr ((<*>) . fmap (:) . fromAtom) (Just [])
          fromAtom (Atom a) = Just a
          fromAtom _        = Nothing
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

-- |Transform a cons list into a haskell list. It builds a function 
-- that takes an empty list and returns a list of expressions.
-- eg: (@(Just $ id . (:) x . (:) x) <*> (Just [])@)
fromConsList :: Expression -> Maybe [Expression]
fromConsList = f (Just id)
  where f acc Null = acc <*> Just []
        f acc (Cell x xs) = f (fmap (. (:) x) acc) xs
        f _ _ = Nothing

-- |Construct a lambda from bindings and bodies.
mkLambda :: [ByteString] -> [Expression] -> Expression
mkLambda bindings bodies = Procedure (length bindings) $ \args -> do
  modify (push args)
  rets <- mapM evaluate bodies
  modify pop
  return $ last rets
  where push args xs = Frame xs $ H.fromList $ zip bindings args
        pop Empty = error "empty stack"
        pop (Frame xs _) = xs
    
-- |Primitive 'Procedure' 'Expression's that cannot be implemented in lisp.
-- These procedures are not "special" by nature and are evaluated identically
-- to procedures implemented in Lisp. Forms like "if", "quote", "lambda", and
-- "define" which break the expression evaluation rules are implemented
-- directly in 'evaluate'.
primitives :: EnvFrame
primitives = H.fromList [
  ("not",Procedure 1 notE),
  ("cons",Procedure 2 consE),
  ("car",Procedure 1 carE),
  ("cdr",Procedure 1 cdrE),
  ("=",Procedure 2 eqlE),
  (">",Procedure 2 gtE),
  (">=",Procedure 2 gteE),
  ("<",Procedure 2 ltE),
  ("<=",Procedure 2 lteE),
  ("+",Procedure 2 addE),
  ("-", Procedure 2 subE),
  ("*", Procedure 2 mulE),
  ("/", Procedure 2 divE),
  ("display", Procedure 1 displayE), -- print a value
  ("bind!", Procedure 2 letBangE), -- bind a value to an atom in the root/global env
  ("integer?", Procedure 1 isIntegerE),
  ("real?", Procedure 1 isRealE),
  ("string?", Procedure 1 isStringE),
  ("atom?", Procedure 1 isAtomE),
  ("null?", Procedure 1 isNullE),
  ("list?", Procedure 1 isListE),
  ("pair?", Procedure 1 isPairE),
  (".", Procedure 2 composeE) -- function composition, result of 2nd proc gets passed to 1st proc.
  ]
  where
    notE [LispFalse] = return LispTrue
    notE [LispTrue]  = return LispFalse
    notE _           = invalidForm "not"
    consE [a,b] = return $ Cell a b
    consE _     = invalidForm "cons"
    carE [Cell v _] = return v
    carE _          = invalidForm "car"
    cdrE [Cell _ v] = return v
    cdrE _          = invalidForm "cdr"
    displayE xs = mapM_ (liftIO . print) xs >> return Null
    letBangE [Atom k, v] = modify f >> return v
      where f Empty = error "empty stack"
            f (Frame Empty x) = Frame Empty (H.insert k v x)
            f (Frame xs x) = Frame (f xs) x
    letBangE _           = invalidForm "let!"
    isIntegerE [Integer _] = return LispTrue
    isIntegerE [_]         = return LispFalse
    isIntegerE _           = invalidForm "integer?"
    isRealE [Real _] = return LispTrue
    isRealE [_]      = return LispFalse
    isRealE _        = invalidForm "real?"
    isStringE [String _] = return LispTrue
    isStringE [_]        = return LispFalse
    isStringE _          = invalidForm "string?"
    isAtomE [Atom _] = return LispTrue
    isAtomE [_]      = return LispFalse
    isAtomE _        = invalidForm "atom?"
    isNullE [Null] = return LispTrue
    isNullE [_]    = return LispFalse
    isNullE _      = invalidForm "null?"
    isListE [Cell _ xs] = isListE [xs]
    isListE [Null]      = return LispTrue
    isListE _           = invalidForm "list?"
    isPairE [Cell _ (Cell _ _)] = return LispFalse
    isPairE [Cell _ _]          = return LispTrue
    isPairE _                   = invalidForm "pair?"
    addE [Integer a, Integer b] = return $ Integer $ a + b
    addE [Integer a, Real b]    = return $ Real $ (fromInteger a) + b
    addE [Real a, Integer b]    = return $ Real $ a + (fromInteger b)
    addE [Real a, Real b]       = return $ Real $ a + b
    addE _                      = invalidForm "+"
    subE [Integer a, Integer b] = return $ Integer $ a - b
    subE [Integer a, Real b]    = return $ Real $ (fromInteger a) - b
    subE [Real a, Integer b]    = return $ Real $ a - (fromInteger b)
    subE [Real a, Real b]       = return $ Real $ a - b
    subE _                      = invalidForm "-"
    mulE [Integer a, Integer b] = return $ Integer $ a * b
    mulE [Integer a, Real b]    = return $ Real $ (fromInteger a) * b
    mulE [Real a, Integer b]    = return $ Real $ a * (fromInteger b)
    mulE [Real a, Real b]       = return $ Real $ a * b
    mulE _                      = invalidForm "*"
    divE [Integer a, Integer b] = return $ Real $ (fromInteger a) / (fromInteger b)
    divE [Integer a, Real b]    = return $ Real $ (fromInteger a) / b
    divE [Real a, Integer b]    = return $ Real $ a / (fromInteger b)
    divE [Real a, Real b]       = return $ Real $ a / b
    divE _                      = invalidForm "/"
    eqlE [a,b]                  = return $ lispBool $ a == b
    eqlE _                      = invalidForm "="
    gtE  [Integer a, Integer b] = return $ lispBool $ a > b
    gtE  [Real a, Integer b]    = return $ lispBool $ a > (fromInteger b)
    gtE  [Integer a, Real b]    = return $ lispBool $ (fromInteger a) > b
    gtE  [Real a, Real b]       = return $ lispBool $ a > b
    gtE  _                      = invalidForm ">"
    gteE  [Integer a, Integer b] = return $ lispBool $ a >= b
    gteE  [Real a, Integer b]    = return $ lispBool $ a >= (fromInteger b)
    gteE  [Integer a, Real b]    = return $ lispBool $ (fromInteger a) >= b
    gteE  [Real a, Real b]       = return $ lispBool $ a >= b
    gteE  _                      = invalidForm ">="
    ltE  [Integer a, Integer b] = return $ lispBool $ a < b
    ltE  [Real a, Integer b]    = return $ lispBool $ a < (fromInteger b)
    ltE  [Integer a, Real b]    = return $ lispBool $ (fromInteger a) < b
    ltE  [Real a, Real b]       = return $ lispBool $ a < b
    ltE  _                      = invalidForm "<"
    lteE  [Integer a, Integer b] = return $ lispBool $ a <= b
    lteE  [Real a, Integer b]    = return $ lispBool $ a <= (fromInteger b)
    lteE  [Integer a, Real b]    = return $ lispBool $ (fromInteger a) <= b
    lteE  [Real a, Real b]       = return $ lispBool $ a <= b
    lteE  _                      = invalidForm "<="
    composeE [Procedure 1 b, Procedure argc a] = return $ Procedure argc $ \args -> (a args) >>= b . (: [])
    -- TODO: proper error message for incorrect arities?
    composeE _ = invalidForm "!"