{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Silence.Primitives
(
  primitives,
  primitiveConstants
)
where
  
import Silence.Syntax
import Silence.Expression
import Silence.FFI

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Data.Bool
import Data.List
import Data.Ratio
import Data.Maybe
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B

{- TODO
* Real world features
  * randomness
  * file descrptors - see system programming
    * files
    * consoles
    * sockets (allows for networking!)
  * concurrency - see system programming
  * system programming - primitive procedure that takes a list of args and a system call number
  * c interface
* catch errors
-}

-- |Primitive constants that cannot be implemented in lisp.
primitiveConstants :: Scope
primitiveConstants = H.fromList []

-- |Primitive procedures that cannot be implemented in lisp.
primitives :: Scope
primitives = H.fromList [
  mkProc "."           True  2 $ binaryMaybe compose,
  mkProc "="           True  2 $ binary $ ((return . Bool) .) . (==),
  mkProc ">"           True  2 $ compE (>),
  mkProc ">="          True  2 $ compE (>=),
  mkProc "<"           True  2 $ compE (<),
  mkProc "<="          True  2 $ compE (<=),
  mkProc "+"           True  2 $ mathBinaryE (+),
  mkProc "-"           True  2 $ mathBinaryE (-),
  mkProc "*"           True  2 $ mathBinaryE (*),
  mkProc "/"           True  2 $ mathBinaryE (/),
  mkProc "log"         True  2 $ mathBinaryE $ wrapBinReal logBase,
  mkProc "exp"         True  1 $ mathUnaryE $ wrapReal exp,
  mkProc "sin"         True  1 $ mathUnaryE $ wrapReal sin,
  mkProc "cos"         True  1 $ mathUnaryE $ wrapReal cos,
  mkProc "tan"         True  1 $ mathUnaryE $ wrapReal tan,
  mkProc "asin"        True  1 $ mathUnaryE $ wrapReal asin,
  mkProc "acos"        True  1 $ mathUnaryE $ wrapReal acos,
  mkProc "atan"        True  1 $ mathUnaryE $ wrapReal atan,
  mkProc "numerator"   True  1 $ mathUnaryE $ (% 1) . numerator,
  mkProc "denominator" True  1 $ mathUnaryE $ (% 1) . denominator,
  mkProc "print"       True  1 $ stringArg $ lispVoid . liftIO . putStr,
  mkProc "proc?"       True  1 $ typePredE isProc,
  mkProc "number?"     True  1 $ typePredE isNumber,
  mkProc "string?"     True  1 $ typePredE isString,
  mkProc "atom?"       True  1 $ typePredE isAtom,
  mkProc "null?"       True  1 $ typePredE isNull,
  mkProc "list?"       True  1 $ typePredE isList,
  mkProc "pair?"       True  1 $ typePredE isPair,
  mkProc "read"        True  1 $ stringArg $ return . parseSilence . B.pack,
  mkProc "show"        True  1 $ unary $ return . toLispStr,
  mkProc "mk-atom"     True  1 $ stringArg $ return . Atom . B.pack,
  mkProc "cons"        True  2 $ binary $ (return .) . Cell,
  mkProc "car"         True  1 $ unaryMaybe car,
  mkProc "cdr"         True  1 $ unaryMaybe cdr,
  mkProc "if"          False 3 $ ternary $ \x t f -> evaluate . bool f t . isTruthy =<< evaluate x,
  mkProc "let!"        True  2 letBangE, -- set a key/value pair in the current env.
  mkProc "let-parent!" True  2 letParentBangE, -- like @let!@, except it modifies the parent env.
  mkProc "lambda"      False 2 $ lambdaE True, -- this one evaluates arguments
  mkProc "lambda!"     False 2 $ lambdaE False, -- this one *doesn't* evaluate arguments
  mkProc "mk-lambda"   True  2 $ lambdaE True, -- like lambda, but its args are evaluated
  mkProc "mk-lambda!"  True  2 $ lambdaE False, -- like lambda, but its args are evaluated
  mkProc "evaluate"    True  1 $ const $ evaluate . head,
  mkProc "quote"       False 1 $ const $ return . head,
  mkProc "import"      True  1 $ stringArg $ (=<<) (evaluate . parseSilence) . liftIO . B.readFile,
  mkProc "begin"       True  (-1) $ const $ return . last,
  mkProc "foreign"     True  2 foreignE,
  mkProc "free"        True  1 freeE
  ]

-- TODO: make error handling not require a parameter to be passed
mkProc :: B.ByteString -> Bool -> Int -> (String -> PrimFunc) -> (B.ByteString, Expression)
mkProc name eval arity body = (name, Procedure eval arity $ body $ B.unpack name)

lambdaE :: Bool -> String -> PrimFunc
lambdaE evalArgs n = (>>=) get . flip (flip binaryMaybe n . mkLambda evalArgs)

letBangE :: String -> PrimFunc
letBangE _ [Atom k,v] = v <$ modify' add
  where add = maybe (error "empty stack") (uncurry ((:) . H.insert k v)) . uncons
letBangE n _ = invalidForm n

letParentBangE :: String -> PrimFunc
letParentBangE n as = get >>= maybe (error "empty stack") (uncurry f) . uncons
  where f e es = put es *> letBangE n as <* (get >>= put . (:) e)

foreignE :: String -> PrimFunc
foreignE n [fp,Atom funcname] = return $ Procedure True (-1) $ fromMaybe (invalidForm n) $ do
  fp' <- fromLispStr fp
  return $ loadForeignProcedure fp' (B.unpack funcname)
foreignE n _ = invalidForm n

freeE :: String -> PrimFunc
freeE _ [Pointer p fin] = lispVoid $ liftIO $ fin p
freeE n _ = invalidForm n

unary :: (Expression -> LispM Expression) -> String -> PrimFunc
unary f n = maybe (invalidForm n) f . fmap fst . uncons

binary :: (Expression -> Expression -> LispM Expression) -> String -> PrimFunc
binary f _ [a,b] = f a b
binary _ n _ = invalidForm n

ternary :: (Expression -> Expression -> Expression -> LispM Expression) -> String -> PrimFunc
ternary f _ [a,b,c] = f a b c
ternary _ n _ = invalidForm n

unaryMaybe :: (Expression -> Maybe Expression) -> String -> PrimFunc
unaryMaybe f n = unary (maybe (invalidForm n) return . f) n

binaryMaybe :: (Expression -> Expression -> Maybe Expression) -> String -> PrimFunc
binaryMaybe f n = binary (\a b -> maybe (invalidForm n) return $ f a b) n

stringArg :: (String -> LispM Expression) -> String -> PrimFunc
stringArg f n = unary (maybe (invalidForm n) f . fromLispStr) n

typePredE :: (Expression -> Bool) -> String -> PrimFunc
typePredE p = unary (return . Bool . p)

coerceNumber :: String -> Expression -> Rational
coerceNumber _ (Number v) = v
coerceNumber n _          = invalidForm n

compE :: (Rational -> Rational -> Bool) -> String -> PrimFunc
compE p n = binary (\a b -> return $ Bool $ p (coerceNumber n a) (coerceNumber n b)) n

mathUnaryE :: (Rational -> Rational) -> String -> PrimFunc
mathUnaryE f n = unary (return . Number . f . coerceNumber n) n

mathBinaryE :: (Rational -> Rational -> Rational) -> String -> PrimFunc
mathBinaryE f n = binary (\a b -> return $ Number $ f (coerceNumber n a) (coerceNumber n b)) n

wrapBinReal :: (Double -> Double -> Double) -> Rational -> Rational -> Rational
wrapBinReal f a b = toRational $ f (fromRational a) (fromRational b)

wrapReal :: (Double -> Double) -> Rational -> Rational
wrapReal f = toRational . f . fromRational