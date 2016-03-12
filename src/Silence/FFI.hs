{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Silence.FFI
(
  loadForeignProcedure,
  Expression()
)
where
  
import Silence.Expression
  
import System.Posix.DynamicLinker hiding (Null)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Control.Exception.Base

import Data.Ratio
import Data.Int
import qualified Data.ByteString.Internal as B

-- TODO: 
-- C compilation unit for manipulating Expressions in C

intSize :: Int
intSize = sizeOf (0 :: Int)

ptrSize :: Int
ptrSize = sizeOf (nullPtr :: Ptr ())

ratioSize :: Int
ratioSize = sizeOf ((1 % 1) :: Ratio Int64)

boolSize :: Int
boolSize = sizeOf (True :: Bool)

pokeTypecode :: Ptr Expression -> Int -> IO ()
pokeTypecode ptr = pokeByteOff ptr 0

toInt64Ratio :: Ratio Integer -> Ratio Int64
toInt64Ratio r = (fromInteger $ numerator r) % (fromInteger $ denominator r)

fromInt64Ratio :: Ratio Int64 -> Ratio Integer
fromInt64Ratio r = (toInteger $ numerator r) % (toInteger $ denominator r)

instance Storable Expression where
  alignment _ = 4
  sizeOf (Atom _) = (3*intSize) + ptrSize
  sizeOf (Number _) = intSize + ratioSize
  sizeOf (Bool _) = intSize + boolSize
  sizeOf (Procedure _ _ _) = intSize + boolSize + ptrSize
  sizeOf Null = intSize
  sizeOf (Cell a b) = intSize + sizeOf a + sizeOf b
  sizeOf (Pointer _) = intSize + ptrSize
  peek ptr = do
    tcode <- peekByteOff ptr 0
    case (tcode :: Int) of
      0 -> Atom <$> (B.fromForeignPtr
           <$> (newForeignPtr_ =<< peekByteOff ptr intSize)
           <*> peekByteOff ptr (2*intSize)
           <*> peekByteOff ptr (3*intSize))
      1 -> Number . fromInt64Ratio <$> peekByteOff ptr intSize
      2 -> Bool <$> peekByteOff ptr intSize
      3 -> Procedure
           <$> peekByteOff ptr intSize
           <*> peekByteOff ptr (intSize + boolSize)
           <*> (fromCSig . mkFun <$> peekByteOff ptr ((3*intSize) + boolSize))
      4 -> return Null
      5 -> do
        a <- peekByteOff ptr intSize
        b <- peekByteOff ptr $ intSize + sizeOf a
        return $ Cell a b
      6 -> Pointer <$> peekByteOff ptr intSize
      _ -> return Null
  poke ptr (Atom x) = do
    pokeTypecode ptr 0
    withForeignPtr fptr $ \bptr -> pokeByteOff ptr (intSize) bptr
    pokeByteOff ptr (2*intSize) off
    pokeByteOff ptr (3*intSize) len
    where (fptr,off,len) = B.toForeignPtr x
  poke ptr (Number r) = do
    pokeTypecode ptr 1
    pokeByteOff ptr intSize (toInt64Ratio r)
  poke ptr (Bool b) = do
    pokeTypecode ptr 2
    pokeByteOff ptr intSize b
  poke ptr (Procedure e a b) = do
    pokeTypecode ptr 3
    pokeByteOff ptr intSize e
    pokeByteOff ptr (intSize + sizeOf e) a
    mkFunPtr (toCSig b) >>= pokeByteOff ptr (intSize + sizeOf e + sizeOf a)
  poke ptr Null = pokeByteOff ptr 0 (4 :: Int)
  poke ptr (Cell a b) = do
    pokeTypecode ptr 5
    pokeByteOff ptr intSize a
    pokeByteOff ptr (intSize + sizeOf a) b
  poke ptr (Pointer p) = do
    pokeTypecode ptr 6
    pokeByteOff ptr intSize p

-- |C LISP function signature. Returned 'Expression' pointer
-- must be malloc'd (seek 'fromCSig' for how the pointer is handled.)
type CSig = Ptr Expression -> Int -> IO (Ptr Expression)

foreign import ccall "dynamic"
  mkFun :: FunPtr (CSig) -> CSig
  
foreign import ccall "wrapper"
  mkFunPtr :: CSig -> IO (FunPtr CSig)
  
-- |Wrap a C LISP function in a Haskell LISP function.
-- the following rule holds: @'toCSig' '.' 'fromCSig' = 'id'@
fromCSig :: CSig -> PrimFunc
fromCSig cf es = liftIO $ do
  eptr <- bracket allocArgs free $ \args -> cf args (length es)
  v <- peek eptr
  free eptr
  return v
  where
    allocArgs = do
      ptr <- mallocBytes $ foldl (\z x -> (sizeOf x) + z) 0 es
      pokeArray ptr es
      return ptr

-- TODO: environment?
-- |Wrap a Haskell LISP function in a C LISP function.
toCSig :: PrimFunc -> CSig
toCSig f = \esptr count -> do
  es <- peekArray count esptr
  (expr,_) <- runStateT (runLispM $ f es) []
  ptr <- malloc
  poke ptr expr
  return ptr

-- |Load a foreign procedure.
loadForeignProcedure :: FilePath -- |path of dylib to load
                     -> String -- |symbol str of function to call
                     -> PrimFunc
loadForeignProcedure file func as = do
  dl <- liftIO $ dlopen file [RTLD_NOW]
  f <- liftIO $ dlsym dl func
  res <- (fromCSig $ mkFun f) as
  liftIO $ dlclose dl
  return res