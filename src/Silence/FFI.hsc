{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables, TupleSections #-}

module Silence.FFI
(
  loadForeignCode,
  loadForeignProcedure
)
where
  
#include "Expression.h"

import Silence.Expression

import System.Posix.DynamicLinker.Prim hiding (Null)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable (peek,peekByteOff)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr

import Control.Monad.IO.Class
import Control.Monad.State.Strict

import Control.Exception.Base

import Data.Ratio
import Data.Word
import Data.Int
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import GHC.Real (Ratio(..))
import System.IO.Unsafe

-- | C Expression "memory" Struct Interop
-- These foreign imports use the C implementation of expression memory
-- layout to guarantee the (hard to debug) layout of expressions in memory.

foreign import ccall "mkAtom"
  c_mkAtom :: CString -> CInt -> IO (Ptr Expression)
  
foreign import ccall "mkNumber"
  c_mkNumber :: Int64 -> Int64 -> IO (Ptr Expression)

foreign import ccall "mkBoolFalse"
  c_mkBoolTrue :: IO (Ptr Expression)
  
foreign import ccall "mkBoolFalse"
  c_mkBoolFalse :: IO (Ptr Expression)
  
foreign import ccall "mkProcedure"
  c_mkProcedure :: Word8 -> Int8 -> FunPtr CSig -> IO (Ptr Expression)
  
foreign import ccall "mkCell"
  c_mkCell :: Ptr Expression -> Ptr Expression -> IO (Ptr Expression)

foreign import ccall "mkPointer"
  c_mkPointer :: Ptr () -> FunPtr PtrFinalizer -> IO (Ptr Expression)

-- These functions "dereference" (read from memory)
-- the layout of expressions. They are much easier to
-- debug and cause less subtle bugs. This is because 
-- Haskell is more picky about memory ownership/layout than C is.

derefAtomMem :: Ptr () -> IO Expression
derefAtomMem ptr = do
  buf <- (#peek struct Atom, buf) ptr
  CInt len <- (#peek struct Atom, len) ptr
  Atom <$> B.packCStringLen (buf,fromIntegral len)
  
derefNumberMem :: Ptr () -> IO Expression
derefNumberMem ptr = do
  (n :: Int64) <- (#peek struct Number, numerator) ptr
  (d :: Int64) <- (#peek struct Number, denominator) ptr
  return $ Number $ (toInteger n) % (toInteger d)
  
derefCellMem :: Ptr () -> IO Expression
derefCellMem ptr = Cell
                   <$> (derefExpression =<< (#peek struct Cell, car) ptr)
                   <*> (derefExpression =<< (#peek struct Cell, cdr) ptr)

derefPointerMem :: Ptr () -> IO Expression
derefPointerMem ptr = Pointer
                      <$> ((#peek struct Pointer, ptr) ptr)
                      <*> (unwrapFinalizer <$> ((#peek struct Pointer, finalizer) ptr))
  
derefProcMem :: Ptr () -> IO Expression
derefProcMem ptr = do
  (ea :: Word8) <- ((#peek struct Procedure,evalArgs) ptr)
  (arity :: Int8) <- ((#peek struct Procedure,arity) ptr)
  (bdy :: FunPtr CSig) <- ((#peek struct Procedure,body) ptr)
  return $ Procedure (ea == 1) (fromIntegral arity) (fromCSig $ unwrapCSig bdy)

-- Your meat and potatoes of expression marshalling:

derefExpression :: Ptr Expression -> IO Expression
derefExpression ptr
  | ptr == nullPtr = return Null
  | otherwise = ((#peek Expression, typecode) ptr) >>= f
    where f :: Word8 -> IO Expression
          f 0 = derefAtomMem =<< ((#peek Expression, memory) ptr)
          f 1 = derefNumberMem =<< ((#peek Expression, memory) ptr)
          f 2 = return $ Bool True
          f 3 = return $ Bool False
          f 4 = derefProcMem =<< ((#peek Expression, memory) ptr)
          f 5 = return Null
          f 6 = derefCellMem =<< ((#peek Expression, memory) ptr)
          f 7 = derefPointerMem =<< ((#peek Expression, memory) ptr)
          f _ = error "FFI: invalid C Expression."
      
refExpression :: Expression -> IO (Ptr Expression)
refExpression (Atom (PS fp o l)) = withForeignPtr fp $ \fp' ->
  c_mkAtom (plusPtr fp' o) (fromIntegral l)
refExpression (Number (n :% d)) = c_mkNumber (fromIntegral n) (fromIntegral d)
refExpression (Bool True) = c_mkBoolTrue
refExpression (Bool False) = c_mkBoolFalse
refExpression (Procedure e a b) =
  c_mkProcedure (if e then 1 else 0) (fromIntegral a) =<< wrapCSig (toCSig b)
refExpression Null = return nullPtr
refExpression (Cell a b) = do
  a' <- refExpression a
  b' <- refExpression b
  c_mkCell a' b'
refExpression (Pointer p fin) = wrapFinalizer fin >>= c_mkPointer p

--
-- FFI logic
--

-- |C LISP function signature. Returned 'Expression' pointer
-- must be malloc'd (seek 'fromCSig' for how the pointer is handled.)
type CSig = Int -> Ptr (Ptr Expression) -> IO (Ptr Expression)

foreign import ccall "dynamic"
  unwrapFinalizer :: FunPtr (PtrFinalizer) -> PtrFinalizer
  
foreign import ccall "wrapper"
  wrapFinalizer :: PtrFinalizer -> IO (FunPtr PtrFinalizer)

foreign import ccall "dynamic"
  unwrapCSig :: FunPtr (CSig) -> CSig
  
foreign import ccall "wrapper"
  wrapCSig :: CSig -> IO (FunPtr CSig)
  
foreign import ccall
  freeExpression :: Ptr Expression -> IO ()

-- |Wrap a C LISP function in a Haskell LISP function.
-- the following rule holds: @'toCSig' '.' 'fromCSig' = 'id'@
fromCSig :: CSig -> PrimFunc
fromCSig cf = \es -> liftIO $ bracket (withCArgs es) free derefExpression
  where withCArgs args = bracket
                           (mapM refExpression args)
                           (mapM_ freeExpression)
                           (($!) (flip withArray $ cf $ length args))

-- |Wrap a Haskell LISP function in a C LISP function.
toCSig :: PrimFunc -> CSig
toCSig f = \n args -> peekArgs [] args n >>= runHLisp >>= refExpression
  where runHLisp hargs = evalStateT (runLispM $ f hargs) []
        peekArgs acc _ 0 = return acc
        peekArgs acc ptr n = unsafeInterleaveIO $ do
          v <- derefExpression =<< peek (advancePtr ptr (n-1))
          v `seq` (peekArgs (v:acc) ptr (n-1))

-- |Load a foreign procedure.
loadForeignProcedure :: Ptr () -- ^ pointer to dynamically linked code
                     -> String -- ^ symbol str of function to call
                     -> IO (Either String PrimFunc)
loadForeignProcedure ptr sym = c_dlerror *> (withCString sym $ \sym' -> do
  fptr <- c_dlsym ptr sym'
  if fptr == nullFunPtr
    then c_dlerror >>= fmap Left . peekCString
    else return $ Right $ fromCSig $ unwrapCSig fptr)

-- |Load a foreign binary.
loadForeignCode :: FilePath -> IO (Either String Expression)
loadForeignCode fp = c_dlerror *> (withCString fp $ \fp' -> do
  ptr <- c_dlopen fp' 2 -- RTLD_NOW
  if ptr == nullPtr
    then c_dlerror >>= fmap Left . peekCString
    else return $ Right $ Pointer ptr $ void <$> c_dlclose)