{-# LANGUAGE OverloadedStrings, TupleSections, Rank2Types, GADTs #-}

module Felony.Lisp
(
  LispT(..),
  Expression,
  setEnv,
  getEnv
) where
  
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
  
import Data.Monoid
import Data.Maybe
  
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
  
newtype LispT m a = LispT {
  runLispT :: Environment -> m (a,Environment,Expression)
}

instance (Functor m) => Functor (LispT m) where
  fmap f m = LispT $ \env -> fmap f' $ runLispT m env
    where f' (a,env',expr') = (f a,env',expr')
  
instance (Monad m) => Applicative (LispT m) where
  pure a = LispT $ \env -> return (a,env,Null)
  (<*>) = ap

instance (Monad m) => Monad (LispT m) where
  fail msg = LispT $ \_ -> fail msg
  m >>= k = LispT $ \env -> do
    -- TODO: evaluate this
    (a,env',_) <- runLispT m env
    runLispT (k a) env'

instance MonadTrans LispT where
  lift m = LispT $ \env -> m >>= return . (,env,Null)

instance (MonadIO m) => MonadIO (LispT m) where
  liftIO = lift . liftIO
  
data Expression = Atom ByteString
                | String ByteString
                | Integer Integer
                | Real Double
                | LispTrue
                | LispFalse
                | Procedure ((Monad m) => Expression -> LispT m ())
                | Null
                | Cell Expression Expression
                
instance Show Expression where
  show = B.unpack . showExpr

instance Eq Expression where
  (Atom a) == (Atom b) = a == b
  (String a) == (String b) = a == b
  (Real a) == (Real b) = a == b
  (Integer a) == (Real b) = (fromInteger a) == b
  (Real b) == (Integer a) = b == (fromInteger a)
  (Integer a) == (Integer b) = a == b
  LispTrue == LispTrue = True
  LispFalse == LispFalse = True
  Null == Null = True
  (Cell a as) == (Cell b bs) = a == b && as == bs
  _ == _ = False 
  
showExpr :: Expression -> ByteString
showExpr (Cell (Atom "quote") (Cell e Null)) = "'" <> showExpr e
showExpr (Atom x) = x
showExpr (String x) = x
showExpr (Integer x) = B.pack $ show x
showExpr (Real x) = B.pack $ show x
showExpr Null = "()"
showExpr LispTrue  = "#t"
showExpr LispFalse = "#f"
showExpr (Procedure _) = "<<procedure>>"
showExpr c@(Cell _ _) = "(" <> f c <> ")"
  where f Null = "" 
        f (Cell a Null) = showExpr a
        f (Cell a b@(Cell _ _)) = showExpr a <> " " <> f b
        f (Cell a b) = showExpr a <> " . " <> showExpr b
        f _ = error "Invalid list."

-- |Evaluate an expression
evaluate :: (Monad m) => Expression -> LispT m ()
evaluate x@(Cell (Atom a) xs) = do
  case fromConsList xs of
    Just xs' -> do
      args <- mapME (getReturnedExpr . evaluate) xs
      expr <- primitive a args
      case expr of
        Just expr' -> returnExpr expr'
        Nothing -> do
          v <- lookupEnv a
          case v of
            Just (Procedure act) -> act args
            Nothing -> error $ "Procedure '" ++ B.unpack a ++ " doesn't exist."
    Nothing -> error $ "Invalid S-Expression: " ++ show x
evaluate (Cell (Procedure act) xs) = do
  v <- mapME evaluate xs
  case v of
    Just v' -> act v'
    Nothing -> error "Invalid S-Expression."
evaluate x@(Atom a)        = lookupEnv a >>= fromMaybe Null
evaluate x@(Integer _)     = return x
evaluate x@(String _)      = return x
evaluate x@(Real _)        = return x
evaluate x@LispTrue        = return x
evaluate x@LispFalse       = return x
evaluate x@(Procedure _)   = return x
evaluate x@Null            = return x

-- TODO: 'primitive' should be implemented by adding primitive actions to the global environment.

-- |logic such as @if@, @lambda@, among others. Does not evaluate
-- the expression on which it operates & does not cause side effects.
primitive :: (MonadIO m) => ByteString -> Expression -> LispT m (Maybe Expression)
primitive "if"       (Cell LispTrue (Cell expr _))              = return $ Just expr
primitive "if"       (Cell LispFalse (Cell _ (Cell expr Null))) = return $ Just expr
primitive "not"      (Cell LispFalse Null)                      = return $ Just LispTrue
primitive "not"      (Cell LispTrue Null)                       = return $ Just LispFalse
primitive "cons"     (Cell a (Cell b Null))                     = return $ Just $ Cell a b
primitive "car"      (Cell v _)                                 = return $ Just v
primitive "cdr"      (Cell _ v)                                 = return $ Just v
primitive "integer?" (Cell (Integer _) _)                       = return $ Just LispTrue
primitive "integer?" (Cell _ _)                                 = return $ Just LispFalse
primitive "real?"    (Cell (Real _) _)                          = return $ Just LispTrue
primitive "real?"    (Cell _ _)                                 = return $ Just LispFalse
primitive "string?"  (Cell (String _) _)                        = return $ Just LispTrue
primitive "string?"  (Cell _ _)                                 = return $ Just LispFalse
primitive "atom?"    (Cell (Atom _) _)                          = return $ Just LispTrue
primitive "atom?"    (Cell _ _)                                 = return $ Just LispFalse
primitive "null?"    (Cell Null _)                              = return $ Just LispTrue
primitive "null?"    (Cell _ _)                                 = return $ Just LispFalse
primitive "list?"    (Cell Null _)                              = return $ Just LispTrue
primitive "list?"    (Cell (Cell _ a) _)                        = primitive "list?" a
primitive "list?"    (Cell _ _)                                 = return $ Just LispFalse
primitive "pair?"    (Cell (Cell _ _) _)                        = return $ Just LispTrue
primitive "pair?"    (Cell _ _)                                 = return $ Just LispFalse
primitive "quote"    (Cell v _)                                 = return $ Just v
primitive "lambda"   (Cell bindings bodies)                     = return $ mkLambda bindings bodies
primitive "=="       (Cell a (Cell b Null))                     = return $ Just $ if a == b then LispTrue else LispFalse
primitive "+"        x                                          = return $ math "+" x
primitive "-"        x                                          = return $ math "-" x
primitive "*"        x                                          = return $ math "*" x
primitive "/"        x                                          = return $ math "/" x
primitive "let!"     (Cell (Atom k) (Cell v Null))              = Just <$> insertEnv k v
primitive "display"  (Cell a Null)                              = (liftIO $ putStrLn "TODO: print") >> (return $ Just Null)
primitive _          _                                          = return $ Nothing

math :: ByteString -> Expression -> Maybe Expression
math "+" (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Integer $ a + b
math "+" (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) + b
math "+" (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a + (fromInteger b)
math "+" (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a + b
math "-" (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Integer $ a - b
math "-" (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) - b
math "-" (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a - (fromInteger b)
math "-" (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a - b
math "*" (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Integer $ a * b
math "*" (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) * b
math "*" (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a * (fromInteger b)
math "*" (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a * b
math "/" (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Real $ (fromInteger a) / (fromInteger b)
math "/" (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) / b
math "/" (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a / (fromInteger b)
math "/" (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a / b
math op  (Cell a (Cell b xs))                       = math op x >>= math op where x = (Cell a (Cell b Null))
math _   _                                          = Nothing

returnExpr :: (Monad m) => Expression -> LispT m ()
returnExpr e = LispT $ \env -> return ((),env,e)

getReturnedExpr :: (Monad m) => LispT m () -> LispT m Expression
getReturnedExpr (LispT f) = LispT $ \env -> (\(_,_,e) -> (e,env,Null)) <$> f env

-- Lists

-- |Map a function over an expression, failing if it's not a list.
mapE :: (Expression -> Expression) -> Expression -> Maybe Expression
mapE f = runIdentity . mapME (return . f)

-- |Map a monadic function over an 'Expression'. Returns 'Nothing'
-- if the expression turns out to not be a cons list.
mapME :: (Monad m) => (Expression -> m Expression) -> Expression -> m (Maybe Expression)
mapME _ Null = return $ Just Null
mapME f e = maybe (return Nothing) (fmap (Just . toConsList) . mapM f) (fromConsList e)

-- |Transform a cons list into a haskell list.
fromConsList :: Expression -> Maybe [Expression]
fromConsList = f $ Just []
  where
    f acc Null = acc
    f acc (Cell x xs) = f (fmap ((:) x) acc) xs 
    f acc _ = Nothing
    
toConsList :: [Expression] -> Expression
toConsList = foldr Cell Null


-- Procedures

-- |Construct a lambda from bindings and bodies.
mkLambda :: Expression -> Expression -> Maybe Expression
mkLambda bindings bodies = do
  bindings' <- fromAtoms (Just []) bindings
  bodies' <- fromConsList bodies
  if null bodies'
    then Nothing
    else Just $ Procedure $ proc bindings' bodies'
      
  where
    fromAtoms acc Null = acc
    fromAtoms acc (Cell (Atom a) xs) = fromAtoms (fmap ((:) a) acc) xs
    fromAtoms acc _ = Nothing
    proc :: (Monad m) => [ByteString] -> [Expression] -> Expression -> LispT m ()
    proc bindings bodies expr = do
      case fromConsList expr of
        Nothing -> error $ "Invalid arguments (not a cons list)." -- TODO: print @expr@
        Just args -> do
          pushEnvFrame (H.fromList $ zip bindings args)
          ret <- getReturnedExpr $ last $ map evaluate bodies
          popEnvFrame
          returnExpr ret

-- Environment

type EnvFrame = HashMap ByteString Expression
data Environment = Frame Environment EnvFrame | Empty

createEnv :: Environment
createEnv = Empty

getEnv :: (Monad m) => LispT m Environment
getEnv = LispT $ \env -> return (env,env,Null)

-- |Pop a "stack frame".
popEnvFrame :: (Monad m) => LispT m ()
popEnvFrame = LispT f
  where f Empty = error "Cannot pop empty stack."
        f (Frame xs _) = return ((),xs,Null)

-- |Push a "stack frame"
pushEnvFrame :: (Monad m) => EnvFrame -> LispT m ()
pushEnvFrame child = LispT $ \env -> return ((),Frame env child,Null)

-- |Insert a value into the environment.
insertEnv :: (Monad m) => ByteString -> Expression -> LispT m Expression
insertEnv k v = LispT f
  where f Empty = error "No stack frame!"
        f (Frame xs x) = return (v,Frame xs (H.insert k v x),Null)

-- |Lookup a value in the environment.
lookupEnv :: (MonadIO m) => ByteString -> LispT m (Maybe Expression)
lookupEnv k = LispT $ \env -> return (f env,env,Null)
  where f Empty = Nothing
        f (Frame xs x) = maybe (f xs) Just (H.lookup k x)