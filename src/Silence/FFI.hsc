{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables #-}

module Silence.FFI
(
  loadForeignCode,
  loadForeignProcedure,
  Expression()
)
where
  
#include "Expression.h"
#include <stdalign.h>
  
import Silence.Expression
  
-- import System.Posix.DynamicLinker hiding (Null)
import System.Posix.DynamicLinker.Prim hiding (Null)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

{-
TODO:
1. Finish Cell-to-CharBuffer C function (and char* to cell function)
-}

data CAtom = CAtom CString CInt
data CNumber = CNumber Int64 Int64
data CProcedure = CProcedure Word8 Int8 (FunPtr CSig)
data CCell = CCell (Ptr Expression) (Ptr Expression)
data CPointer = CPointer (Ptr ()) (FunPtr PtrFinalizer)

instance Storable CAtom where
  sizeOf    _ = #const sizeof(struct Atom)
  alignment _ = #alignment struct Atom
  peek ptr = CAtom <$> ((#peek struct Atom, buf) ptr) <*> ((#peek struct Atom, len) ptr)
  poke ptr (CAtom cstr len) = ((#poke struct Atom, buf) ptr cstr) >> ((#poke struct Atom, len) ptr len)

instance Storable CNumber where
  sizeOf _ = #const sizeof(struct Number)
  alignment _ = #alignment struct Number
  peek ptr = CNumber <$> ((#peek struct Number, numerator) ptr) <*> ((#peek struct Number, denominator) ptr)
  poke ptr (CNumber n d) = ((#poke struct Number, numerator) ptr n) >> ((#poke struct Number, denominator) ptr d)
  
instance Storable CCell where
  sizeOf _ = #const sizeof(struct Cell)
  alignment _ = #alignment struct Cell
  peek ptr = CCell <$> ((#peek struct Cell,car) ptr) <*> ((#peek struct Cell, cdr) ptr)
  poke ptr (CCell a b) = ((#poke struct Cell,car) ptr a) >> ((#poke struct Cell,cdr) ptr b)
  
instance Storable CProcedure where
  sizeOf _ = #const sizeof(struct Procedure)
  alignment _ = #alignment struct Procedure
  peek ptr = CProcedure
             <$> ((#peek struct Procedure,evalArgs) ptr)
             <*> ((#peek struct Procedure,arity) ptr)
             <*> ((#peek struct Procedure,body) ptr)
  poke ptr (CProcedure ea a bdy) = do
    ((#poke struct Procedure,evalArgs) ptr ea)
    ((#poke struct Procedure,arity) ptr a)
    ((#poke struct Procedure,body) ptr bdy)

instance Storable CPointer where
  sizeOf _ = #const sizeof(struct Pointer)
  alignment _ = #alignment struct Pointer
  peek ptr = CPointer <$> ((#peek struct Pointer, ptr) ptr) <*> ((#peek struct Pointer, finalizer) ptr)
  poke ptr (CPointer p f) = ((#poke struct Pointer, ptr) ptr p) >> ((#poke struct Pointer, finalizer) ptr f)

instance Storable Expression where
  sizeOf    _ = #const sizeof(Expression)
  alignment _ = #alignment Expression
  peek ptr = do
    (tc,eptr) <- peekExpr ptr
    case tc of
      0 -> do
        (CAtom buf (CInt len)) <- peek ((castPtr eptr) :: Ptr CAtom)
        Atom <$> B.packCStringLen (buf,fromIntegral len)
      1 -> do
        (CNumber n d) <- peek ((castPtr eptr) :: Ptr CNumber)
        return $ Number $ (toInteger n) % (toInteger d)
      2 -> return $ Bool True
      3 -> return $ Bool False
      4 -> do
        (CProcedure ea arity bdy) <- peek ((castPtr eptr) :: Ptr CProcedure)
        return $ Procedure (ea == 1) (fromIntegral arity) (fromCSig $ unwrapCSig bdy)
      5 -> return Null
      6 -> do
        (CCell a b) <- peek ((castPtr eptr) :: Ptr CCell)
        Cell <$> (peek a) <*> (peek b)
      7 -> do
        (CPointer p f) <- peek ((castPtr eptr) :: Ptr CPointer)
        return $ Pointer p (unwrapFinalizer f)
      _ -> error "FFI: invalid C Expression."
  poke ptr (Atom (PS fp o l)) = do
    (p :: Ptr CChar) <- mallocBytes l
    withForeignPtr fp $ \fp' -> copyArray p (fp' `plusPtr` o) l
    pokeExpr ptr 0 $ CAtom p (fromIntegral l)
  poke ptr (Number r) =
    pokeExpr ptr 1 $ CNumber (fromIntegral $ numerator r) (fromIntegral $ denominator r)
  poke ptr (Bool True) = pokeExpr ptr 2 nullPtr
  poke ptr (Bool False) = pokeExpr ptr 3 nullPtr
  poke ptr (Procedure e a b) = do
    cs <- wrapCSig $ toCSig b
    pokeExpr ptr 3 $ CProcedure ((if e then 1 else 0) :: Word8) (fromIntegral a) cs
  poke ptr Null = pokeExpr ptr 5 nullPtr
  poke ptr (Cell a b) = do
    a' <- mallocSingleton a
    b' <- mallocSingleton b
    pokeExpr ptr 6 (CCell (castPtr a') (castPtr b'))
  poke ptr (Pointer p fin) = wrapFinalizer fin >>= pokeExpr ptr 7 . CPointer p
    
mallocSingleton :: (Storable a) => a -> IO (Ptr ())
mallocSingleton v = do
  ptr <- malloc
  poke ptr v
  return $ castPtr ptr

mallocN :: (Storable a) => [a] -> IO (Ptr a)
mallocN [] = return nullPtr
mallocN xs = do
  ptrs <- mallocBytes ((length xs)*(sizeOf (undefined :: Ptr ())))
  pokeArray ptrs xs
  return ptrs
  
pokeExpr :: (Storable a) => Ptr Expression -> Word8 -> a -> IO ()
pokeExpr ptr typecode memory = do
  memory' <- mallocSingleton memory
  (#poke Expression, typecode) ptr typecode
  (#poke Expression, memory) ptr memory'

peekExpr :: Ptr Expression -> IO (Word8,Ptr ())
peekExpr ptr = do
  tc <- ((#peek Expression, typecode) ptr)
  memptr <- ((#peek Expression, memory) ptr)
  return (tc,memptr)

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
fromCSig cf es = liftIO $ bracket (withCArgs $ cf $ length es) free peek
  where withCArgs = bracket allocArgs freeArgs
        allocArgs = mapM (fmap castPtr . mallocSingleton) es >>= mallocN
        freeArgs ptr = (peekArray (length es) ptr >>= mapM_ freeExpression) >> free ptr

-- TODO: environment?
-- |Wrap a Haskell LISP function in a C LISP function.
toCSig :: PrimFunc -> CSig
toCSig f n args = peekArray n args >>= mapM peek >>= runHLisp >>= castMallocS
  where castMallocS = fmap castPtr . mallocSingleton
        runHLisp hargs = evalStateT (runLispM $ f hargs) []

-- |Load a foreign procedure.
loadForeignProcedure :: Ptr () -- |pointer to dynamically linked code
                     -> String -- |symbol str of function to call
                     -> IO (Either String PrimFunc)
loadForeignProcedure ptr sym = withCString sym $ \sym' -> do
  c_dlerror -- clear error
  func <- liftIO $ c_dlsym ptr sym'
  err <- c_dlerror
  if err == nullPtr
    then return $ Right $ fromCSig $ unwrapCSig func
    else Left <$> peekCString err

loadForeignCode :: FilePath -> IO (Either String Expression)
loadForeignCode fp = withCString fp $ \fp' -> do
  c_dlerror -- clear error
  ptr <- c_dlopen fp' 2 -- RTLD_NOW
  if ptr == nullPtr
    then c_dlerror >>= fmap Left . peekCString
    else return $ Right $ Pointer ptr (fmap void c_dlclose)