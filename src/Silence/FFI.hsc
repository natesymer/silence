{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables #-}

module Silence.FFI
(
  loadForeignProcedure,
  Expression()
)
where
  
#include "Expression.h"
  
import Silence.Expression
  
import System.Posix.DynamicLinker hiding (Null)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Control.Exception.Base

import Data.Ratio
import Data.Word
import Data.Int
import qualified Data.ByteString.Internal as B

instance Storable Expression where
  sizeOf    _ = #const sizeof(Expression)
  alignment _ = alignment (undefined :: Ptr ())
  peek ptr = do
    (typecode,_,ptrs) <- peekExpr ptr
    -- TODO: proper bounds checking for @ptrs@
    case typecode of
      0 -> do
        buf <- peekPtrs 0 ptrs >>= newForeignPtr_
        (CInt len) <- peekPtrs 1 ptrs >>= peek
        return $ Atom $ B.fromForeignPtr buf 0 (fromIntegral len)
      1 -> do
        (num :: Int64) <- peekPtrs 0 ptrs >>= peek
        (denom :: Int64) <- peekPtrs 1 ptrs >>= peek
        return $ Number $ (toInteger num) % (toInteger denom)
      2 -> Bool <$> (peekPtrs 0 ptrs >>= peekByteBool)
      3 -> do
        evalArgs <- peekPtrs 0 ptrs >>= peekByteBool
        (arity :: Word8) <- peekPtrs 1 ptrs >>= peek
        f <- fromCSig . mkFun <$> (peekPtrs 2 ptrs >>= peek)
        return $ Procedure evalArgs (fromIntegral arity) f
      4 -> return Null
      5 -> Cell
           <$> (peekPtrs 0 ptrs >>= peek)
           <*> (peekPtrs 1 ptrs >>= peek)
      6 -> Pointer <$> (peekPtrs 0 ptrs >>= peek)
      _ -> error "FFI: invalid typecode"
  poke ptr (Atom x) = do
      lenP <- mallocSingleton $ CInt (fromIntegral len)
      buf' <- mallocArray len'
      withForeignPtr fptr $ \buf -> copyBytes buf' buf len'
      ptrs <- mallocN [castPtr buf',lenP]
      pokeExpr ptr 0 2 ptrs
    where (fptr,_,len) = B.toForeignPtr x
          len' = fromIntegral len
  poke ptr (Number r) = do
    numPtr <- mallocSingleton ((fromIntegral (numerator r)) :: Int64)
    denPtr <- mallocSingleton ((fromIntegral (denominator r)) :: Int64)
    ptrs <- mallocN [numPtr,denPtr]
    pokeExpr ptr 1 2 ptrs
  poke ptr (Bool True) = pokeExpr ptr 2 1 =<< mallocN . pure =<< mallocSingleton (1 :: Word8)
  poke ptr (Bool False) = pokeExpr ptr 2 1 =<< mallocN . pure =<< mallocSingleton (0 :: Word8)
  poke ptr (Procedure e a b) = do
    e' <- mallocSingleton ((if e then 1 else 0) :: Word8)
    a' <- mallocSingleton ((fromIntegral a) :: Word8)
    b' <- mkFunPtr (toCSig b) >>= mallocSingleton
    pokeExpr ptr 3 3 =<< mallocN [e',a',b']
  poke ptr Null = pokeExpr ptr 4 0 nullPtr
  poke ptr (Cell a b) = do
    a' <- mallocSingleton a
    b' <- mallocSingleton b
    pokeExpr ptr 5 2 =<< mallocN [a',b']
  poke ptr (Pointer p) = do
    p' <- mallocSingleton p
    pokeExpr ptr 6 1 =<< mallocN [p']

mallocSingleton :: (Storable a) => a -> IO (Ptr ())
mallocSingleton v = do
  ptr <- malloc -- Bytes $ sizeOf v -- TODO: use 'malloc'
  poke ptr v
  return $ castPtr ptr
  
mallocN :: (Storable a) => [a] -> IO (Ptr a)
mallocN xs = do
  ptrs <- mallocBytes ((length xs)*(sizeOf (undefined :: Ptr ())))
  pokeArray ptrs xs
  return ptrs
  
pokeExpr :: Ptr Expression -> Word8 -> Word8 -> Ptr (Ptr ()) -> IO ()
pokeExpr ptr typecode numPtrs ptrs = do
  (#poke Expression, typecode) ptr typecode
  (#poke Expression, num_ptrs) ptr numPtrs
  (#poke Expression, ptrs) ptr ptrs

peekExpr :: Ptr Expression -> IO (Word8,Word8,Ptr (Ptr ()))
peekExpr ptr = do
  typecode <- (#peek Expression, typecode) ptr
  numPtrs  <- (#peek Expression, num_ptrs) ptr
  ptrs     <- (#peek Expression, ptrs) ptr
  return (typecode,numPtrs,ptrs)
  
peekPtrs :: (Storable a) => Int -> Ptr (Ptr ()) -> IO (Ptr a)
peekPtrs idx ptrs = castPtr <$> peekByteOff ptrs (idx * (sizeOf (undefined :: Ptr ())))
  
peekByteBool :: Ptr Word8 -> IO Bool
peekByteBool = fmap f . peek
  where f 0 = False; f _ = True

-- |C LISP function signature. Returned 'Expression' pointer
-- must be malloc'd (seek 'fromCSig' for how the pointer is handled.)
type CSig = Int -> Ptr (Ptr Expression) -> IO (Ptr Expression)

foreign import ccall "dynamic"
  mkFun :: FunPtr (CSig) -> CSig
  
foreign import ccall "wrapper"
  mkFunPtr :: CSig -> IO (FunPtr CSig)
  
foreign import ccall "freeExpression"
  freeExpression :: Ptr Expression -> IO ()

-- |Wrap a C LISP function in a Haskell LISP function.
-- the following rule holds: @'toCSig' '.' 'fromCSig' = 'id'@
fromCSig :: CSig -> PrimFunc
fromCSig cf es = liftIO $ bracket (withCArgs $ cf $ length es) free peek
  where withCArgs = bracket allocArgs freeArgs
        allocArgs = mapM (fmap castPtr . mallocSingleton) es >>= mallocN
        freeArgs ptr = (peekArray (length es) ptr >>= mapM_ freeExpression) >> free ptr

-- TODO: environment?
-- |Wrap a Haskell LISP function in a C LISP function.
toCSig :: PrimFunc -> CSig
toCSig f = \n args -> peekArray n args >>= mapM peek >>= runHLisp >>= castMallocS
  where castMallocS = fmap castPtr . mallocSingleton
        runHLisp args = evalStateT (runLispM $ f args) []

-- |Load a foreign procedure.
loadForeignProcedure :: FilePath -- |path of dylib to load
                     -> String -- |symbol str of function to call
                     -> PrimFunc
loadForeignProcedure file func as = do
  dl <- liftIO $ dlopen file [RTLD_NOW]
  f <- fromCSig . mkFun <$> (liftIO $ dlsym dl func)
  res <- f as
  liftIO $ dlclose dl
  return res