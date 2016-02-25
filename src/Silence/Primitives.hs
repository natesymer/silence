{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B

{- TODO
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
* higher-order operators: ($)
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
  -- (">",  Procedure True 2 $ compE ">" (>)),
--   (">=", Procedure True 2 $ compE ">=" (>=)),
--   ("<",  Procedure True 2 $ compE "<" (<)),
--   ("<=", Procedure True 2 $ compE "<=" (<=)),
--   ("+", Procedure True 2 addE),
--   ("-", Procedure True 2 subE),
--   ("*", Procedure True 2 mulE),
--   ("/", Procedure True 2 divE),
--   ("^", Procedure True 2 expoE),
--   ("%", Procedure True 2 moduloE),
--   ("&&", Procedure True 2 andE),
--   ("||", Procedure True 2 orE),
--   ("nroot", Procedure True 2 nrootE),
--   ("quot", Procedure True 2 quotE),
--   ("round", Procedure True 1 roundE),
--   ("ceil", Procedure True 1 ceilE),
--   ("floor", Procedure True 1 floorE),
--   ("sin", Procedure True 1 (trig "sin" sin)),
--   ("cos", Procedure True 1 (trig "cos" cos)),
--   ("tan", Procedure True 1 (trig "tan" tan)),
--   ("asin", Procedure True 1 (trig "asin" asin)),
--   ("acos", Procedure True 1 (trig "acos" acos)),
--   ("atan", Procedure True 1 (trig "atan" atan)),
--   ("to-int", Procedure True 1 toIntegerE),
--   ("to-real", Procedure True 1 toRealE),
--   ("to-str", Procedure True 1 toStrE),
--   (".", Procedure True 2 composeE), -- function composition, result of 2nd proc gets passed to 1st proc.
--   ("=", Procedure True 2 eqlE),
--   ("cons", Procedure True 2 consE),
--   ("car", Procedure True 1 carE),
--   ("cdr", Procedure True 1 cdrE),
--   ("print", Procedure True 1 printE), -- print a string
--   ("integer?", Procedure True 1 isIntegerE),
--   ("real?", Procedure True 1 isRealE),
--   ("string?", Procedure True 1 isStringE),
--   ("atom?", Procedure True 1 isAtomE),
--   ("null?", Procedure True 1 isNullE),
--   ("list?", Procedure True 1 isListE),
--   ("pair?", Procedure True 1 isPairE),
--   ("read", Procedure True 1 readE), -- parse a string of code
--   ("let", Procedure True 2 letE),
--   ("let!", Procedure True 2 letBangE),
--   ("begin", Procedure True (-1) beginE), -- function sequencing
--   ("if", Procedure False 3 ifE),
--   ("quote", Procedure False 1 quoteE),
--   ("lambda", Procedure False 2 lambdaE),
--   ("func", Procedure False 3 funcE)
  mkProc ">" True 2 $ compE (>),
  mkProc ">=" True 2 $ compE (>=),
  mkProc "<" True 2 $ compE (<),
  mkProc "<=" True 2 $ compE (<=),
  mkProc "+" True 2 addE,
  mkProc "-" True 2 subE,
  mkProc "*" True 2 mulE,
  mkProc "/" True 2 divE,
  mkProc "^" True 2 expoE,
  mkProc "%" True 2 moduloE,
  mkProc "&&" True 2 andE,
  mkProc "||" True 2 orE,
  mkProc "nroot" True 2 nrootE,
  mkProc "quot" True 2 quotE,
  mkProc "round" True 1 roundE,
  mkProc "ceil" True 1 ceilE,
  mkProc "floor" True 1 floorE,
  mkProc "sin" True 1 $ trig sin,
  mkProc "cos" True 1 $ trig cos,
  mkProc "tan" True 1 $ trig tan,
  mkProc "asin" True 1 $ trig asin,
  mkProc "acos" True 1 $ trig acos,
  mkProc "atan" True 1 $ trig atan,
  mkProc "to-int" True 1 toIntegerE,
  mkProc "to-real" True 1 toRealE,
  mkProc "to-str" True 1 toStrE,
  mkProc "." True 2 composeE, -- function composition, result of 2nd proc gets passed to 1st proc.
  mkProc "=" True 2 eqlE,
  mkProc "cons" True 2 consE,
  mkProc "car" True 1 carE,
  mkProc "cdr" True 1 cdrE,
  mkProc "print" True 1 printE, -- print a string
  mkProc "integer?" True 1 isIntegerE,
  mkProc "real?" True 1 isRealE,
  mkProc "string?" True 1 isStringE,
  mkProc "atom?" True 1 isAtomE,
  mkProc "null?" True 1 isNullE,
  mkProc "list?" True 1 isListE,
  mkProc "pair?" True 1 isPairE,
  mkProc "read" True 1 readE, -- parse a string of code
  mkProc "let" True 2 letE,
  mkProc "let!" True 2 letBangE,
  mkProc "begin" True (-1) beginE, -- function sequencing
  mkProc "if" False 3 ifE,
  mkProc "quote" False 1 quoteE,
  mkProc "lambda" False 2 lambdaE,
  mkProc "func" False 3 funcE
  ]
  
mkProc :: B.ByteString -> Bool -> Int -> (String -> PrimFunc) -> (B.ByteString, Expression)
mkProc name eval arity body = (name, Procedure eval arity $ body $ B.unpack name)

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
lambdaE :: String -> PrimFunc
lambdaE n [args,body] = maybe argErr (return . lambda) (fromExpr atom args)
  where argErr = invalidForm $ n ++ ": invalid argument names"
        atom (Atom a) = Just a
        atom _        = Nothing
        lambda xs = Procedure True (length xs) $ \as ->
          modify (push as) *> evaluate body <* modify tail
          where push = (:) . H.fromList . zip xs
lambdaE n _ = invalidForm n

-- |Takes:
-- key -> what to bind the variable to
-- val -> value to bind
-- Returns: function with arity @-1@ that evaluates args given key=val.
-- example: @((let 'a 1) (+ 1 a))@ returns @2@
letE :: String -> PrimFunc
letE n [k@(Atom _),v] =
  return $ Procedure False (-1) $ \args -> do
    l <- lambdaE n [Cell k Null, Cell (Atom "begin") (toConsList args)]
    evaluate (Cell l (Cell v Null))
letE n _ = invalidForm n

-- |Takes:
-- key -> what to bind the variable to
-- val -> value to bind
-- Does: Assigns val to key in the current environment scope.
letBangE :: String -> PrimFunc
letBangE _ [Atom k,v] = modify add >> return v
  where add [] = error "empty stack"
        add (e:es) = (H.insert k v e):es
letBangE n _ = invalidForm n

funcE :: String -> PrimFunc
funcE n (k:xs) = evaluate k >>= f
  where
    f k'@(Atom _) = lambdaE n xs >>= letBangE n . (:) k' . pure
    f _ = invalidForm $ n ++ ": function name must be an atom"
funcE n _ = invalidForm n

-- |Compose two functions of arbitrary arities. If @barity@ is > 1,
-- this will return a procedure. @((. b a)<args>)@ = @(b (a <args>))@
composeE :: String -> PrimFunc
composeE _ [Procedure _ barity b, Procedure eargs aarity a]
  | barity < 2 = return $ Procedure eargs aarity ((b . return =<<) . a)
  | otherwise = return $ Procedure eargs aarity ((apply procb . pure =<<) . a)
    where procb = Procedure False barity b 
composeE n _ = invalidForm n

beginE :: String -> PrimFunc
beginE = const $ return . last . (:) Null

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

compE :: (Ordering -> Ordering -> Bool) -> String -> PrimFunc
compE p _ [Integer a, Integer b] = return $ Bool $ p (compare a b)               EQ
compE p _ [Real a, Integer b]    = return $ Bool $ p (compare a (fromInteger b)) EQ
compE p _ [Integer a, Real b]    = return $ Bool $ p (compare (fromInteger a) b) EQ
compE p _ [Real a, Real b]       = return $ Bool $ p (compare a b)               EQ
compE _ n _ = invalidForm n

addE :: String -> PrimFunc
addE _ [Integer a, Integer b] = return $ Integer $ a + b
addE _ [Integer a, Real b]    = return $ Real $ (fromInteger a) + b
addE _ [Real a, Integer b]    = return $ Real $ a + (fromInteger b)
addE _ [Real a, Real b]       = return $ Real $ a + b
addE n _                      = invalidForm n

subE :: String -> PrimFunc
subE _ [Integer a, Integer b] = return $ Integer $ a - b
subE _ [Integer a, Real b]    = return $ Real $ (fromInteger a) - b
subE _ [Real a, Integer b]    = return $ Real $ a - (fromInteger b)
subE _ [Real a, Real b]       = return $ Real $ a - b
subE n _                      = invalidForm n

mulE :: String -> PrimFunc
mulE _ [Integer a, Integer b] = return $ Integer $ a * b
mulE _ [Integer a, Real b]    = return $ Real $ (fromInteger a) * b
mulE _ [Real a, Integer b]    = return $ Real $ a * (fromInteger b)
mulE _ [Real a, Real b]       = return $ Real $ a * b
mulE n _                      = invalidForm n

divE :: String -> PrimFunc
divE _ [Integer a, Integer b] = return $ Real $ (fromInteger a) / (fromInteger b)
divE _ [Integer a, Real b]    = return $ Real $ (fromInteger a) / b
divE _ [Real a, Integer b]    = return $ Real $ a / (fromInteger b)
divE _ [Real a, Real b]       = return $ Real $ a / b
divE n _                      = invalidForm n

expoE :: String -> PrimFunc
expoE _ [Integer a, Integer b] = return $ Integer $ a ^ b
expoE _ [Real a, Integer b]    = return $ Real $ a ^ b
expoE n _                      = invalidForm n

nrootE :: String -> PrimFunc
nrootE _ [Integer n, Integer x] = return $ Real $ xroot (fromInteger n) (fromInteger x)
nrootE _ [Integer n, Real x]    = return $ Real $ xroot (fromInteger n) x
nrootE _ [Real n, Integer x]    = return $ Real $ xroot n (fromInteger x)
nrootE _ [Real n, Real x]       = return $ Real $ xroot n x
nrootE n _                      = invalidForm n
    
-- based on http://www.rohitab.com/discuss/topic/35165-nth-root-function/
xroot :: Floating a => a -> a -> a
xroot n x = f (16 :: Int) x
  where
    f 0 xn = xn
    f d xn = f ((-) d 1)
               ((-) xn ((/) ((-) ((**) xn n) x) ((*) ((**) xn ((-) n 1)) n)))

quotE :: String -> PrimFunc
quotE _ [Integer a, Integer b] = return $ Integer $ quot a b
quotE n _                      = invalidForm n

moduloE :: String -> PrimFunc
moduloE _ [Integer a, Integer b] = return $ Integer $ rem a b
moduloE n _                      = invalidForm n

roundE :: String -> PrimFunc
roundE _ [Real v] = return $ Integer $ round v
roundE n _        = invalidForm n

ceilE :: String -> PrimFunc
ceilE _ [Real v]  = return $ Integer $ ceiling v
ceilE n _         = invalidForm n

floorE :: String -> PrimFunc
floorE _ [Real v] = return $ Integer $ floor v
floorE n _        = invalidForm n

trig :: (Double -> Double) -> String -> PrimFunc
trig f _ [Real v] = return $ Real $ f v
trig _ n _        = invalidForm n

toIntegerE :: String -> PrimFunc
toIntegerE _ [Real v]    = return $ Integer $ truncate v
toIntegerE _ [Integer v] = return $ Integer v
toIntegerE n _           = invalidForm n

toRealE :: String -> PrimFunc
toRealE _ [Integer v] = return $ Real $ fromInteger v
toRealE _ [Real v]    = return $ Real v
toRealE n _           = invalidForm n

toStrE :: String -> PrimFunc
toStrE _ [x] = return $ toLispStr x
toStrE n _ = invalidForm n

andE :: String -> PrimFunc
andE _ [Bool a,Bool b] = return $ Bool (a && b)
andE n _ = invalidForm n

orE :: String -> PrimFunc
orE _ [Bool a,Bool b] = return $ Bool (a || b)
orE n _ = invalidForm n

readE :: String -> PrimFunc
readE _ [x] = maybe (invalidForm "read") f $ fromLispStr x
  where f = return . parseSilence . B.pack
readE n _ = invalidForm n