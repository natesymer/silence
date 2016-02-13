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

-- |Primitive procedures that cannot be implemented in lisp.
primitives :: EnvFrame
primitives = H.fromList [
  ("not",Procedure notE),
  ("cons",Procedure consE),
  ("car",Procedure carE),
  ("cdr",Procedure cdrE),
  ("=",Procedure eqlE),
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
    letBangE [Atom k, v] = insertGlobalEnv k v
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
    eqlE [a,b]                  = return $ if a == b then LispTrue else LispFalse
    eqlE _                      = invalidForm "=="
    
evalExpressions :: [Expression] -> IO Expression
evalExpressions e = (evalStateT (runLispM $ evaluate e') Empty)
  where e' = Cell (mkLambda [] e) Null -- wrap expressions in a lambda

invalidForm :: String -> LispM a
invalidForm = error . (++) "invalid special form: "

-- |Evaluate an expression
evaluate :: Expression -> LispM Expression
evaluate (Cell (Atom "if") (Cell x (Cell t (Cell f Null)))) = evaluate x >>= fn
  where fn LispTrue = evaluate t
        fn LispFalse = evaluate f
        fn _ = invalidForm "if"
evaluate (Cell (Atom "quote") (Cell v Null)) = return v
evaluate (Cell (Atom "quote") _) = invalidForm "quote"
evaluate (Cell (Atom "lambda") (Cell car cdr)) =
  maybe (invalidForm "lambda") return $ maybeLambda car cdr
  where
    maybeLambda bindings bodies = mkLambda
                                  <$> (fromConsList bindings >>= fromAtoms)
                                  <*> (fromConsList bodies)
    fromAtoms = foldr ((<*>) . fmap (:) . fromAtom) (Just [])
      where fromAtom (Atom a) = Just a
            fromAtom _        = Nothing
evaluate (Cell (Atom "lambda") _) = invalidForm "lambda"
evaluate (Cell x xs) = evaluate x >>= f
  where f (Procedure act) = maybe
                              (error "invalid s-expression: cdr not a cons list.")
                              (\xs' -> mapM evaluate xs' >>= act)
                              (fromConsList xs)
        f _ = error "invalid s-expression: car not a procedure."
evaluate (Atom a) = get >>= f . flip Frame primitives
  where
    f (Frame xs x) = maybe (f xs) return $! H.lookup a x
    f Empty = error $ "cannot find " ++ B.unpack a
evaluate x = return x

-- TODO: this is a huge bottleneck - O(n^2) due to @(++ [x])@
-- |Transform a cons list into a haskell list.
fromConsList :: Expression -> Maybe [Expression]
fromConsList = f (Just [])
  where f acc Null = acc
        f acc (Cell x xs) = f ((++ [x]) <$> acc) xs
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

-- |Insert a value into the global environment.
insertGlobalEnv :: ByteString -> Expression -> LispM Expression
insertGlobalEnv k v = modify f >> return v
  where f Empty = error "No stack frame!"
        f (Frame Empty x) = Frame Empty (H.insert k v x)
        f (Frame xs x) = Frame (f xs) x