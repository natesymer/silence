{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables, TupleSections #-}

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
import Foreign.Marshal.Utils
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
1. Finish Cell<->CharBuffer C functions
-}

--
-- C Expression "memory" Struct Interop
--

-- TODO: Avoid round trips through data structures;
--       write functions to marshall each individual struct
--       this way, explicitly designating types in code can be avoided!

data CAtom = CAtom CString CInt
data CNumber = CNumber Int64 Int64
data CProcedure = CProcedure Word8 Int8 (FunPtr CSig)
data CCell = CCell (Ptr Expression) (Ptr Expression)
data CPointer = CPointer (Ptr ()) (FunPtr PtrFinalizer)

instance Storable CAtom where
  alignment _ = #{alignment struct Atom}
  sizeOf _ = #{size struct Atom}
  peek ptr = CAtom <$> ((#peek struct Atom, buf) ptr) <*> ((#peek struct Atom, len) ptr)
  poke ptr (CAtom cstr len) = ((#poke struct Atom, buf) ptr cstr) >> ((#poke struct Atom, len) ptr len)

instance Storable CNumber where
  alignment _ = #{alignment struct Number}
  sizeOf _ = #{size struct Number}
  peek ptr = CNumber <$> ((#peek struct Number, numerator) ptr) <*> ((#peek struct Number, denominator) ptr)
  poke ptr (CNumber n d) = ((#poke struct Number, numerator) ptr n) >> ((#poke struct Number, denominator) ptr d)
  
instance Storable CCell where
  alignment _ = #{alignment struct Cell}
  sizeOf _ = #{size struct Cell}
  peek ptr = CCell <$> ((#peek struct Cell,car) ptr) <*> ((#peek struct Cell, cdr) ptr)
  poke ptr (CCell a b) = ((#poke struct Cell,car) ptr a) >> ((#poke struct Cell,cdr) ptr b)
  
instance Storable CProcedure where
  alignment _ = #{alignment struct Procedure}
  sizeOf _ = #{size struct Procedure}
  peek ptr = CProcedure
             <$> ((#peek struct Procedure,evalArgs) ptr)
             <*> ((#peek struct Procedure,arity) ptr)
             <*> ((#peek struct Procedure,body) ptr)
  poke ptr (CProcedure ea a bdy) = do
    ((#poke struct Procedure,evalArgs) ptr ea)
    ((#poke struct Procedure,arity) ptr a)
    ((#poke struct Procedure,body) ptr bdy)

instance Storable CPointer where
  alignment _ = #{alignment struct Pointer}
  sizeOf _ = #{size struct Pointer}
  peek ptr = CPointer <$> ((#peek struct Pointer, ptr) ptr) <*> ((#peek struct Pointer, finalizer) ptr)
  poke ptr (CPointer p f) = ((#poke struct Pointer, ptr) ptr p) >> ((#poke struct Pointer, finalizer) ptr f)

--
-- Expression Marshalling
--

derefExpression :: Ptr Expression -> IO Expression
derefExpression ptr
  | ptr == nullPtr = return Null
  | otherwise = do
    (tc :: Word8) <- ((#peek Expression, typecode) ptr)
    eptr <- ((#peek Expression, memory) ptr) -- TODO: don't load this unless it's necessary
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
        Cell <$> (derefExpression a) <*> (derefExpression b)
      7 -> do
        (CPointer p f) <- peek ((castPtr eptr) :: Ptr CPointer)
        return $ Pointer p (unwrapFinalizer f)
      _ -> error "FFI: invalid C Expression."
      
refExpression :: Expression -> IO (Ptr Expression)
refExpression (Atom (PS fp o l)) = do
    (p :: Ptr CChar) <- mallocBytes (l * sizeOf (undefined :: CChar))
    withForeignPtr fp $ \fp' -> copyArray p (fp' `plusPtr` o) l
    mkExpression 0 $ CAtom p $ fromIntegral l
refExpression (Number r) = mkExpression 1 $ CNumber num denom
  where num = fromIntegral $ numerator r
        denom = fromIntegral $ denominator r
refExpression (Bool True) = mkExpression 2 nullPtr
refExpression (Bool False) = mkExpression 3 nullPtr
refExpression (Procedure e a b) = do
    cs <- wrapCSig $ toCSig b
    mkExpression 3 $ CProcedure ((if e then 1 else 0) :: Word8) (fromIntegral a) cs
refExpression Null = return nullPtr
refExpression (Cell a b) = mkExpression 6 =<< (CCell <$> (refExpression a) <*> (refExpression b))
refExpression (Pointer p fin) = wrapFinalizer fin >>= mkExpression 7 . CPointer p

mkExpression :: Storable a => Word8 -> a -> IO (Ptr Expression)
mkExpression typecode memory = do
  ptr <- mallocBytes #{size Expression}
  memory' <- new memory
  (#poke Expression, typecode) ptr typecode
  (#poke Expression, memory) ptr memory'
  return ptr

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

-- FIXME null ptr handling
-- |Wrap a C LISP function in a Haskell LISP function.
-- the following rule holds: @'toCSig' '.' 'fromCSig' = 'id'@
fromCSig :: CSig -> PrimFunc
fromCSig cf es = liftIO $ bracket (withCArgs $ cf $ length es) free derefExpression
  where withCArgs = bracket allocArgs freeArgs
        allocArgs = mapM (refExpression) es >>= newArray
        freeArgs ptr = (peekArray (length es) ptr >>= mapM_ freeExpression) *> free ptr

-- |Wrap a Haskell LISP function in a C LISP function.
toCSig :: PrimFunc -> CSig
toCSig f n args = peekArray n args >>= mapM derefExpression >>= runHLisp >>= refExpression
  where runHLisp hargs = evalStateT (runLispM $ f hargs) []

-- |Load a foreign procedure.
loadForeignProcedure :: Ptr () -- |pointer to dynamically linked code
                     -> String -- |symbol str of function to call
                     -> IO (Either String PrimFunc)
loadForeignProcedure ptr sym = withCString sym $ \sym' -> do
  c_dlerror -- clear error
  fptr <- c_dlsym ptr sym'
  if fptr == nullFunPtr
    then c_dlerror >>= fmap Left . peekCString
    else return $ Right $ fromCSig $ unwrapCSig fptr

loadForeignCode :: FilePath -> IO (Either String Expression)
loadForeignCode fp = withCString fp $ \fp' -> do
  c_dlerror -- clear error
  ptr <- c_dlopen fp' 2 -- RTLD_NOW
  if ptr == nullPtr
    then c_dlerror >>= fmap Left . peekCString
    else return $ Right $ Pointer ptr $ void <$> c_dlclose