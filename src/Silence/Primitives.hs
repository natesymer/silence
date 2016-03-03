{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Silence.Primitives
(
  primitives,
  primitiveConstants
)
where
  
import Silence.Syntax
import Silence.Expression

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Maybe
import Data.Ratio
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B

{- TODO
* Rewrite math to use Rational rather than Integer/Double
* Real world features
  * randomness
  * file descrptors
    * files
    * consoles
    * sockets (allows for networking!)
  * import code (depends on file descriptors)
  * concurrency
  * system programming
  * c interface
* get-env & put-env
* number equality vs naive equality
-}

primitiveConstants :: Scope
primitiveConstants = H.fromList []

-- |Primitive procedures that cannot be implemented in lisp. Most of them
-- behave just like procedures defined in lisp, but are defined in Haskell.
-- There are some procedures which inhibit parameter evaluation. These procedures
-- (such as @lambda@) evaluate parameters internally according to their own logic.
primitives :: Scope
primitives = H.fromList [
  mkProc "." True 2 composeE, -- function composition, result of 2nd proc gets passed to 1st proc.
  mkProc "=" True 2 eqlE,
  mkProc ">" True 2 $ compE (>),
  mkProc ">=" True 2 $ compE (>=),
  mkProc "<" True 2 $ compE (<),
  mkProc "<=" True 2 $ compE (<=),
  mkProc "+" True 2 $ mathE (+),
  mkProc "-" True 2 $ mathE (-),
  mkProc "*" True 2 $ mathE (*),
  mkProc "/" True 2 $ mathE (/),
  mkProc "^" True 2 expoE,
  mkProc "%" True 2 $ integralMathE rem,-- moduloE,
  mkProc "&&" True 2 andE,
  mkProc "||" True 2 orE,
  mkProc "quot" True 2 $ integralMathE quot,-- quotE,
  mkProc "round" True 1 $ roundingMathE round,--roundE,
  mkProc "ceil" True 1 $ roundingMathE ceiling, -- ceilE,
  mkProc "floor" True 1 $ roundingMathE floor, -- floorE,
  mkProc "sin" True 1 $ realMathUnaryE sin,
  mkProc "cos" True 1 $ realMathUnaryE cos,
  mkProc "tan" True 1 $ realMathUnaryE tan,
  mkProc "asin" True 1 $ realMathUnaryE asin,
  mkProc "acos" True 1 $ realMathUnaryE acos,
  mkProc "atan" True 1 $ realMathUnaryE atan,
  mkProc "to-int" True 1 toIntegerE,
  mkProc "to-real" True 1 toRealE,
  mkProc "to-str" True 1 toStrE,
  mkProc "cons" True 2 consE,
  mkProc "car" True 1 carE,
  mkProc "cdr" True 1 cdrE,
  mkProc "print" True 1 printE, -- print a string
  mkProc "proc?" True 1 isProcE,
  mkProc "integer?" True 1 isIntegerE,
  mkProc "real?" True 1 isRealE,
  mkProc "string?" True 1 isStringE,
  mkProc "atom?" True 1 isAtomE,
  mkProc "null?" True 1 isNullE,
  mkProc "list?" True 1 isListE,
  mkProc "pair?" True 1 isPairE,
  mkProc "read" True 1 readE, -- parse a string of code
  mkProc "let!" True 2 letBangE,
  mkProc "let-parent!" True 2 letParentBangE,
  mkProc "if" False 3 ifE,
  mkProc "quote" False 1 quoteE,
  mkProc "lambda" False 2 $ lambdaE True, -- this one evaluates arguments
  mkProc "lambda!" False 2 $ lambdaE False, -- this one *doesn't* evaluate arguments
  mkProc "mk-lambda" True 2 $ lambdaE True, -- like lambda, but it's args are evaluated
  mkProc "mk-lambda!" True 2 $ lambdaE False, -- like lambda, but it's args are evaluated
  mkProc "evaluate" False 1 evaluateE,
  mkProc "import" True 1 importE,
  mkProc "begin" True (-1) (const $ return . last)]

mkProc :: B.ByteString -> Bool -> Int -> (String -> PrimFunc) -> (B.ByteString, Expression)
mkProc name eval arity body = (name, Procedure eval arity $ body $ B.unpack name)

evaluateE :: String -> PrimFunc
evaluateE _ [x] = evaluate x
evaluateE n _ = invalidForm n

ifE :: String -> PrimFunc
ifE _ [x,t,f] = evaluate x >>= fn
  where fn (Bool False) = evaluate f
        fn _ = evaluate t
ifE n _ = invalidForm n

-- |Standard scheme-esque quote.
quoteE :: String -> PrimFunc
quoteE _ [v] = return v
quoteE n _ = invalidForm n

-- |Takes:
-- args -> list of atoms to which arguments will be bound
-- body -> a *single* expression that serves as the procedure's body.
-- Returns: function with arity @-1@ that evaluates args given key=val.
-- example: @(lambda (a b c) (+ a (+ b c)))@ returns a procedure
-- that adds three numbers.
lambdaE :: Bool -> String -> PrimFunc
lambdaE evalArgs n [args,body] = maybe argErr (lambda body) (fromAtoms args)
  where argErr = invalidForm $ n ++ ": invalid argument names"
        lambda bdy ["*"] = do
          cap <- get -- capture env it was defined in
          return $ Procedure evalArgs (-1) (scoped bdy cap . H.singleton "args" . toConsList)
        lambda bdy xs = do
          cap <- get -- capture env it was defined in
          return $ Procedure evalArgs (length xs) $ scoped bdy cap . H.fromList . zip xs
        scoped bdy cap env = liftIO (putStrLn "scoping") *> modify' ((:) (mconcat $ env:cap)) *> evaluate bdy <* modify' tail
lambdaE _ n _ = invalidForm n

-- |Takes:
-- key -> what to bind the variable to
-- val -> value to bind
-- Does: Assigns val to key in the current environment scope.
letBangE :: String -> PrimFunc
letBangE _ [Atom k,v] = modify' add >> return v
  where add [] = error "empty stack"
        add (e:es) = (H.insert k v e):es
letBangE n _ = invalidForm n

-- |Like letBangE, except it first pops the environment.
letParentBangE :: String -> PrimFunc
letParentBangE _ [Atom k,v] = modify' add >> return v
  where add [] = error "empty stack"
        add [_] = error "no parent scope"
        add (e:e':es) = e:(H.insert k v e'):es
letParentBangE n as = (liftIO $ print as) >> invalidForm n

-- |Compose two functions of arbitrary arities. If @barity@ is > 1,
-- this will return a procedure. @((. b a)<args>)@ = @(b (a <args>))@
composeE :: String -> PrimFunc
composeE _ [Procedure _ barity b, Procedure eargs aarity a] = 
  return $ Procedure eargs aarity ((apply procb . pure =<<) . a)
    where procb = Procedure False barity b
composeE n _ = invalidForm n

consE :: String -> PrimFunc
consE _ [a,b] = return $ Cell a b
consE n _     = invalidForm n

carE :: String -> PrimFunc
carE _ [Cell v _] = return v
carE n _          = invalidForm n

cdrE :: String -> PrimFunc
cdrE _ [Cell _ v] = return v
cdrE n _          = invalidForm n

printE :: String -> PrimFunc
printE n [x] = maybe (invalidForm n) (liftIO . putStr) (fromLispStr x) >> return Null
printE n _   = invalidForm n

isProcE :: String -> PrimFunc
isProcE _ [Procedure _ _ _] = return $ Bool True
isProcE _ [_]               = return $ Bool False
isProcE n _                 = invalidForm n

isIntegerE :: String -> PrimFunc
isIntegerE _ [Integer _] = return $ Bool True
isIntegerE _ [_]         = return $ Bool False
isIntegerE n _           = invalidForm n

isRealE :: String -> PrimFunc
isRealE _ [Real _] = return $ Bool True
isRealE _ [_]      = return $ Bool False
isRealE n _        = invalidForm n

isStringE :: String -> PrimFunc
isStringE _ [xs] = return $ Bool $ isJust $ fromExpr integer xs
  where integer (Integer x) = Just x
        integer _           = Nothing
isStringE n _    = invalidForm n

isAtomE :: String -> PrimFunc
isAtomE _ [Atom _] = return $ Bool True
isAtomE _ [_]      = return $ Bool False
isAtomE n _        = invalidForm n

isNullE :: String -> PrimFunc
isNullE _ [Null] = return $ Bool True
isNullE _ [_]    = return $ Bool False
isNullE n _      = invalidForm n

isListE :: String -> PrimFunc
isListE n [Cell _ xs] = isListE n [xs]
isListE _ [Null]      = return $ Bool True
isListE n _           = invalidForm n

isPairE :: String -> PrimFunc
isPairE _ [Cell _ _] = return $ Bool True
isPairE _ [_]        = return $ Bool False
isPairE n _          = invalidForm n

eqlE :: String -> PrimFunc
eqlE _ [a,b]                  = return $ Bool $ a == b
eqlE n _                      = invalidForm n

andE :: String -> PrimFunc
andE _ [Bool a,Bool b] = return $ Bool (a && b)
andE n _ = invalidForm n

orE :: String -> PrimFunc
orE _ [Bool a,Bool b] = return $ Bool (a || b)
orE n _ = invalidForm n

readE :: String -> PrimFunc
readE n [x] = maybe (invalidForm n) f $ fromLispStr x
  where f = return . parseSilence . B.pack
readE n _ = invalidForm n

importE :: String -> PrimFunc
importE n [x] = maybe (invalidForm n) f $ fromLispStr x
  where f pth = (liftIO $ B.readFile pth) >>= evaluate . parseSilence
importE n _ = invalidForm n

toStrE :: String -> PrimFunc
toStrE _ [x] = return $ toLispStr x
toStrE n _ = invalidForm n

toIntegerE :: String -> PrimFunc
toIntegerE _ [Real v]    = return $ Integer $ truncate v
toIntegerE _ [Integer v] = return $ Integer v
toIntegerE n _           = invalidForm n

toRealE :: String -> PrimFunc
toRealE _ [Integer v] = return $ Real $ fromInteger v
toRealE _ [Real v]    = return $ Real v
toRealE n _           = invalidForm n

compE :: (Ordering -> Ordering -> Bool) -> String -> PrimFunc
compE p _ [Integer a, Integer b] = return $ Bool $ p (compare a b)               EQ
compE p _ [Real a, Integer b]    = return $ Bool $ p (compare a (fromInteger b)) EQ
compE p _ [Integer a, Real b]    = return $ Bool $ p (compare (fromInteger a) b) EQ
compE p _ [Real a, Real b]       = return $ Bool $ p (compare a b)               EQ
compE _ n _ = invalidForm n

mathE :: (Rational -> Rational -> Rational) -> String -> PrimFunc
mathE f _ [Integer a, Integer b] = return $ rat2expr $ f (toRational a) (toRational b)
mathE f _ [Integer a, Real b]    = return $ rat2expr $ f (toRational a) (toRational b)
mathE f _ [Real a, Integer b]    = return $ rat2expr $ f (toRational a) (toRational b)
mathE f _ [Real a, Real b]       = return $ rat2expr $ f (toRational a) (toRational b)
mathE _ n _ = invalidForm n

integralMathE :: (Integer -> Integer -> Integer) -> String -> PrimFunc
integralMathE f _ [Integer a, Integer b] = return $ Integer $ f a b
integralMathE _ n _ = invalidForm n

realMathUnaryE :: (Double -> Double) -> String -> PrimFunc
realMathUnaryE f _ [Real v] = return $ rat2expr $ toRational $ f v
realMathUnaryE _ n _ = invalidForm n

roundingMathE :: (Double -> Integer) -> String -> PrimFunc
roundingMathE f _ [Real v] = return $ Integer $ f v
roundingMathE f _ [Integer v] = return $ Integer $ f $ fromInteger v
roundingMathE _ n _ = invalidForm n

rat2expr :: Rational -> Expression
rat2expr x = if denominator x == 1 then (Integer $ numerator x) else (Real $ fromRational x)

expoE :: String -> PrimFunc
expoE _ [Integer a, Integer b] = return $ Integer $ a ^ b
expoE _ [Real a, Integer b]    = return $ Real $ a ^ b
expoE n _                      = invalidForm n