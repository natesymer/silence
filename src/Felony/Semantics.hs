{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Felony.Semantics
(
  LispM,
  evalExpressions
)
where
  
import Felony.Types
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
 
evalExpressions :: [Expression] -> IO Expression
evalExpressions e = evalStateT (runLispM $ evaluate e') (Frame Empty primitives)
  where e' = Cell (mkLambda [] e) Null -- wrap expressions in a lambda

invalidForm :: String -> LispM a
invalidForm = error . (++) "invalid special form: "

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
  where f (Procedure act) = maybe (error "invalid s-expression: cdr not a cons list.")
                                  ((act =<<) . mapM evaluate)
                                  (fromConsList xs)
        f _ = error "invalid s-expression: car not a procedure."
evaluate (Atom a) = get >>= f
  where
    f (Frame xs x) = maybe (f xs) return $! H.lookup a x
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
mkLambda bindings bodies = Procedure $ \args -> do
  modify (flip Frame $ bind args)
  rets <- mapM evaluate bodies
  modify pop
  return $ last rets
  where
    bind = H.fromList . zip bindings
    pop Empty = error "Cannot pop empty stack."
    pop (Frame xs _) = xs
    
-- |Primitive 'Procedure' 'Expression's that cannot be implemented in lisp.
primitives :: EnvFrame
primitives = H.fromList [
  ("not",Procedure notE),
  ("cons",Procedure consE),
  ("car",Procedure carE),
  ("cdr",Procedure cdrE),
  ("=",Procedure eqlE),
  (">",Procedure gtE),
  (">=",Procedure gteE),
  ("<",Procedure ltE),
  ("<=",Procedure lteE),
  ("+",Procedure addE),
  ("-", Procedure subE),
  ("*", Procedure mulE),
  ("/", Procedure divE),
  ("display", Procedure displayE), -- print a value
  ("bind!", Procedure letBangE), -- bind a value to an atom in the root/global env
  ("integer?", Procedure isIntegerE),
  ("real?", Procedure isRealE),
  ("string?", Procedure isStringE),
  ("atom?", Procedure isAtomE),
  ("null?", Procedure isNullE),
  ("list?", Procedure isListE),
  ("pair?", Procedure isPairE)]
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
      where f Empty = error "No stack frame!"
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
    isPairE [Cell _ (Cell _ _)] = return LispFalse -- TODO: verify this
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
    
lispBool :: Bool -> Expression
lispBool True = LispTrue
lispBool False = LispFalse