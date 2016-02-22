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
import Data.Bits
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B

{- TODO
* I/O (console, files)
* import code (depends on I/O)
* math ops:
  -> trig (sin,tan,cos,asin,atan,acos)
  -> division (%,quot,rem)
  -> roots & exponentiation (nroot,^)
  -> rounding (round,ceiling,floor)
* higher-order operators: ($)
* randomness
* get-env & put-env
* improve lambda consistency:
  • @begin@ procedure to handle sequential evaluation
  • @lambda@ is no longer of arity @-1@ (it's now @2@, a list of arg names and a body)
-}

primitiveConstants :: Scope
primitiveConstants = H.fromList []

-- |Primitive 'Procedure' 'Expression's that cannot be implemented in lisp.
-- They behave like any other @'Procedure'@ 'Expression', except they may inhibit
-- evaluation of arguments by 'evaluate'.
primitives :: Scope
primitives = H.fromList [
  ("not",Procedure True 1 notE),
  ("cons",Procedure True 2 consE),
  ("car",Procedure True 1 carE),
  ("cdr",Procedure True 1 cdrE),
  (".", Procedure True 2 composeE), -- function composition, result of 2nd proc gets passed to 1st proc.
  ("=",Procedure True 2 eqlE),
  (">",Procedure True 2 gtE),
  (">=",Procedure True 2 gteE),
  ("<",Procedure True 2 ltE),
  ("<=",Procedure True 2 lteE),
  ("+",Procedure True 2 addE),
  ("-", Procedure True 2 subE),
  ("*", Procedure True 2 mulE),
  ("/", Procedure True 2 divE),
  ("&&", Procedure True 2 andE),
  ("||", Procedure True 2 orE),
  ("xor", Procedure True 2 xorE),
  ("to-str", Procedure True 1 toStrE),
  ("display", Procedure True 1 displayE), -- print a value
  ("print", Procedure True 1 printE), -- print a string
  ("bind!", Procedure True 2 bindE), -- bind a value to an atom in the root/global env
  ("integer?", Procedure True 1 isIntegerE),
  ("real?", Procedure True 1 isRealE),
  ("string?", Procedure True 1 isStringE),
  ("atom?", Procedure True 1 isAtomE),
  ("null?", Procedure True 1 isNullE),
  ("list?", Procedure True 1 isListE),
  ("pair?", Procedure True 1 isPairE),
  ("evaluate", Procedure True 1 evaluateE),
  ("if",Procedure False 3 ifE),
  ("quote",Procedure False 1 quoteE),
  ("lambda",Procedure False (-1) lambdaE),
  ("define",Procedure False (-1) defineE),
  ("let",Procedure True 2 letE),
  ("let!",Procedure True 2 letBangE)
  ]

ifE :: PrimFunc
ifE [x,t,f] = evaluate x >>= fn
  where fn (Bool b) = evaluate (if b then t else f)
        fn _ = invalidForm "if"
ifE _ = invalidForm "if"

-- |Standard scheme-esque quote.
quoteE :: PrimFunc
quoteE [v] = return v
quoteE _ = invalidForm "quote"

-- |Standard scheme-esque lambda.
lambdaE :: PrimFunc
lambdaE (_:[]) = invalidForm "lambda: cannot create a body-less lambda"
lambdaE (car:cdr) = maybe (invalidForm "lambda") return lambda
  where lambda = mkLambda <$> (fromExpr atom car) <*> (Just cdr)
        atom (Atom a) = Just a
        atom _        = Nothing
lambdaE _ = invalidForm "lambda"

-- |Takes:
-- key -> what to bind the variable to
-- val -> value to bind
-- Returns: function with arity @-1@ that evaluates args given key=val.
-- example: @((let 'a 1) (+ 1 a))@ returns @2@
letE :: PrimFunc
letE [k@(Atom _),v] =
  return $ Procedure False (-1) $ \args -> do
    l <- lambdaE ((Cell k Null):args)
    evaluate (Cell l (Cell v Null))
letE [_,_] = invalidForm "let: first argument must be an atom"
letE _ = invalidForm "let"

-- |Takes:
-- key -> what to bind the variable to
-- val -> value to bind
-- Does: Assigns val to key in the current environment scope.
letBangE :: PrimFunc
letBangE [Atom k,v] = modify add >> return v
  where add [] = error "empty stack"
        add (e:es) = (H.insert k v e):es
letBangE [_,_] = invalidForm "let!: first argument must be an atom"
letBangE _ = invalidForm "let"

-- |like letE, except for global (root) env.
bindE :: PrimFunc
bindE [Atom k, v] = modify f >> return v
  where f [] = error "empty stack"
        f [x] = [H.insert k v x]
        f (x:xs) = x:f xs
bindE _ = invalidForm "bind!"

defineE :: PrimFunc
defineE (k:xs) = evaluate k >>= f
  where
    f k'@(Atom _) = lambdaE xs >>= bindE . (:) k' . pure
    f _ = invalidForm "define: procedure name must be an atom"
defineE _ = invalidForm "define"

-- |Compose two functions of arbitrary arities. If @barity@ is > 1,
-- this will return a procedure. @((. b a)<args>)@ = @(b (a <args>))@
composeE :: PrimFunc
composeE [Procedure _ barity b, Procedure eargs aarity a]
  | barity < 2 = return $ Procedure eargs aarity ((b . return =<<) . a)
  | otherwise = return $ Procedure eargs aarity ((apply procb . pure =<<) . a)
    where procb = Procedure False barity b 
composeE _ = invalidForm "."

notE :: PrimFunc
notE [Bool b] = return $ Bool $ not b
notE _           = invalidForm "not"

consE :: PrimFunc
consE [a,b] = return $ Cell a b
consE _     = invalidForm "cons"

carE :: PrimFunc
carE [Cell v _] = return v
carE _          = invalidForm "car"

cdrE :: PrimFunc
cdrE [Cell _ v] = return v
cdrE _          = invalidForm "cdr"

displayE :: PrimFunc
displayE = (>> return Null) . mapM_ (liftIO . print)

printE :: PrimFunc
printE [x] = maybe (invalidForm "print") (liftIO . putStr) (fromLispStr x) >> return Null
printE _   = invalidForm "print"

isIntegerE :: PrimFunc
isIntegerE [Integer _] = return $ Bool True
isIntegerE [_]         = return $ Bool False
isIntegerE _           = invalidForm "integer?"

isRealE :: PrimFunc
isRealE [Real _] = return $ Bool True
isRealE [_]      = return $ Bool False
isRealE _        = invalidForm "real?"

isStringE :: PrimFunc
isStringE [xs] = return $ Bool $ isJust $ fromExpr integer xs
  where integer (Integer x) = Just x
        integer _           = Nothing
isStringE _    = invalidForm "string?"

isAtomE :: PrimFunc
isAtomE [Atom _] = return $ Bool True
isAtomE [_]      = return $ Bool False
isAtomE _        = invalidForm "atom?"

isNullE :: PrimFunc
isNullE [Null] = return $ Bool True
isNullE [_]    = return $ Bool False
isNullE _      = invalidForm "null?"

isListE :: PrimFunc
isListE [Cell _ xs] = isListE [xs]
isListE [Null]      = return $ Bool True
isListE _           = invalidForm "list?"

isPairE :: PrimFunc
isPairE [Cell _ _] = return $ Bool True
isPairE [_]        = return $ Bool False
isPairE _          = invalidForm "pair?"

addE :: PrimFunc
addE [Integer a, Integer b] = return $ Integer $ a + b
addE [Integer a, Real b]    = return $ Real $ (fromInteger a) + b
addE [Real a, Integer b]    = return $ Real $ a + (fromInteger b)
addE [Real a, Real b]       = return $ Real $ a + b
addE _                      = invalidForm "+"

subE :: PrimFunc
subE [Integer a, Integer b] = return $ Integer $ a - b
subE [Integer a, Real b]    = return $ Real $ (fromInteger a) - b
subE [Real a, Integer b]    = return $ Real $ a - (fromInteger b)
subE [Real a, Real b]       = return $ Real $ a - b
subE _                      = invalidForm "-"

mulE :: PrimFunc
mulE [Integer a, Integer b] = return $ Integer $ a * b
mulE [Integer a, Real b]    = return $ Real $ (fromInteger a) * b
mulE [Real a, Integer b]    = return $ Real $ a * (fromInteger b)
mulE [Real a, Real b]       = return $ Real $ a * b
mulE _                      = invalidForm "*"

divE :: PrimFunc
divE [Integer a, Integer b] = return $ Real $ (fromInteger a) / (fromInteger b)
divE [Integer a, Real b]    = return $ Real $ (fromInteger a) / b
divE [Real a, Integer b]    = return $ Real $ a / (fromInteger b)
divE [Real a, Real b]       = return $ Real $ a / b
divE _                      = invalidForm "/"

eqlE :: PrimFunc
eqlE [a,b]                  = return $ Bool $ a == b
eqlE _                      = invalidForm "="

gtE :: PrimFunc
gtE  [Integer a, Integer b] = return $ Bool $ a > b
gtE  [Real a, Integer b]    = return $ Bool $ a > (fromInteger b)
gtE  [Integer a, Real b]    = return $ Bool $ (fromInteger a) > b
gtE  [Real a, Real b]       = return $ Bool $ a > b
gtE  _                      = invalidForm ">"

gteE :: PrimFunc
gteE  [Integer a, Integer b] = return $ Bool $ a >= b
gteE  [Real a, Integer b]    = return $ Bool $ a >= (fromInteger b)
gteE  [Integer a, Real b]    = return $ Bool $ (fromInteger a) >= b
gteE  [Real a, Real b]       = return $ Bool $ a >= b
gteE  _                      = invalidForm ">="

ltE :: PrimFunc
ltE  [Integer a, Integer b] = return $ Bool $ a < b
ltE  [Real a, Integer b]    = return $ Bool $ a < (fromInteger b)
ltE  [Integer a, Real b]    = return $ Bool $ (fromInteger a) < b
ltE  [Real a, Real b]       = return $ Bool $ a < b
ltE  _                      = invalidForm "<"

lteE :: PrimFunc
lteE  [Integer a, Integer b] = return $ Bool $ a <= b
lteE  [Real a, Integer b]    = return $ Bool $ a <= (fromInteger b)
lteE  [Integer a, Real b]    = return $ Bool $ (fromInteger a) <= b
lteE  [Real a, Real b]       = return $ Bool $ a <= b
lteE  _                      = invalidForm "<="

toStrE :: PrimFunc
toStrE [x] = return $ toLispStr x
toStrE _ = invalidForm "to-str"

andE :: PrimFunc
andE [Bool a,Bool b] = return $ Bool (a && b)
andE _ = invalidForm "&&"

orE :: PrimFunc
orE [Bool a,Bool b] = return $ Bool (a || b)
orE _ = invalidForm "||"

xorE :: PrimFunc
xorE [Bool a,Bool b] = return $ Bool $ xor a b
xorE _ = invalidForm "xor"

evaluateE :: PrimFunc
evaluateE [x] = maybe (invalidForm "evaluate") eval $ fromLispStr x
  where eval = fmap last . mapM evaluate . parseSilence . B.pack
evaluateE _ = invalidForm "evaluate"