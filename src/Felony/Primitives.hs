{-# LANGUAGE OverloadedStrings #-}
module Felony.Primitives
(
  primitives
)
where
  
import Felony.Syntax
import Felony.Semantics
import Felony.Types

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
-}

-- |Primitive 'Procedure' 'Expression's that cannot be implemented in lisp.
-- These procedures are not "special" by nature and are evaluated identically
-- to procedures implemented in Lisp. Forms like "if", "quote", "lambda", and
-- "define" which break the expression evaluation rules are implemented
-- directly in 'evaluate'.
primitives :: EnvFrame
primitives = H.fromList [
  (".", Procedure 2 composeE), -- function composition, result of 2nd proc gets passed to 1st proc.
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
  ("to-str", Procedure 1 toStrE),
  ("display", Procedure 1 displayE), -- print a value
  ("print", Procedure 1 printE), -- print a string
  ("bind!", Procedure 2 bindE), -- bind a value to an atom in the root/global env
  ("integer?", Procedure 1 isIntegerE),
  ("real?", Procedure 1 isRealE),
  ("string?", Procedure 1 isStringE),
  ("atom?", Procedure 1 isAtomE),
  ("null?", Procedure 1 isNullE),
  ("list?", Procedure 1 isListE),
  ("pair?", Procedure 1 isPairE),
  ("&&", Procedure 2 andE),
  ("||", Procedure 2 orE),
  ("xor", Procedure 2 xorE),
  ("evaluate", Procedure 1 evaluateE)]

-- TODO: proper error message for incorrect arities?
composeE :: PrimFunc
composeE [Procedure 1 b, Procedure argc a] = return $ Procedure argc $ (b . return =<<) . a
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

bindE :: PrimFunc
bindE [Atom k, v] = modify f >> return v
  where f Empty = error "empty stack"
        f (Frame Empty x) = Frame Empty (H.insert k v x)
        f (Frame xs x) = Frame (f xs) x
bindE _           = invalidForm "bind!"

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
  where eval = fmap last . mapM evaluate . parseFelony . B.pack
evaluateE _ = invalidForm "evaluate"