{-# LANGUAGE OverloadedStrings, TupleSections, GADTs, Rank2Types #-}

module Felony.Lisp
(
  LispM(..),
  Expression(..),
  Environment,
  createEnv,
  evaluate,
  toConsList,
  mkLambda
) where
  
import Control.Monad
import Control.Monad.IO.Class
  
import Data.Monoid
import Data.Maybe
  
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
  
newtype LispM a = LispM {
  runLispM :: Environment -> IO (a,Environment,Expression)
}

instance Functor LispM where
  fmap f m = LispM $ \env -> fmap f' $ runLispM m env
    where f' (a,env',expr') = (f a,env',expr')
  
instance Applicative LispM where
  pure a = LispM $ \env -> return (a,env,Null)
  (<*>) = ap

instance Monad LispM where
  fail msg = LispM $ \_ -> fail msg
  m >>= k = LispM $ \env -> do
    -- TODO: evaluate this
    (a,env',_) <- runLispM m env
    runLispM (k a) env'

instance MonadIO LispM where
  liftIO io = LispM $ \env -> fmap (,env,Null) io

data Expression = Atom ByteString
                | String ByteString
                | Integer Integer
                | Real Double
                | LispTrue
                | LispFalse
                | Procedure (Expression -> LispM ())
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

-- (car '(1 2))
-- Cell (Atom "car")
--  (Cell
--    (Cell 1 (Cell 2 Null))
--   Null)

-- |Evaluate an expression
evaluate :: Expression -> LispM ()
-- S-Expr evaluation
evaluate (Cell x xs) = do
  x' <- getReturnedExpr $ evaluate x -- TODO: fix list arguments
  case x' of
    Procedure act -> do
      xs' <- mapME (getReturnedExpr . evaluate) xs
      act $ fromMaybe Null xs'
    _ -> error "Invalid S-Expression"
-- environment lookup
evaluate (Atom a) = lookupEnv a >>= maybe (error $ "Can't find " ++ B.unpack a) returnExpr
-- literal passthroughs
evaluate x = returnExpr x

-- |Primitive procedures.
primitives :: EnvFrame
primitives = H.fromList [
  ("if",Procedure ifE),
  ("quote",Procedure quoteE),
  ("'",Procedure quoteE),
  ("not",Procedure notE),
  ("cons",Procedure consE),
  ("car",Procedure carE),
  ("cdr",Procedure cdrE),
  ("==",Procedure $ mathE "=="),
  ("+",Procedure $ mathE "+"),
  ("-", Procedure $ mathE "-"),
  ("*", Procedure  $ mathE "*"),
  ("/", Procedure  $ mathE "/"),
  ("lambda", Procedure lambdaE),
  ("display", Procedure displayE),
  ("let!", Procedure letBangE),
  ("integer?", Procedure isIntegerE),
  ("real?", Procedure isRealE),
  ("string?", Procedure isStringE)
  ("atom?", Procedure isAtomE),
  ("null?", Procedure isNullE),
  ("list?", Procedure isListE),
  ("pair?", Procedure isPairE)
  ]
  where
    invalidForm :: String -> LispM ()
    invalidForm = error . (++) "invalid special form: "
    ifE (Cell LispTrue (Cell expr _)) = evaluate expr
    ifE (Cell LispFalse (Cell _ (Cell expr Null))) = evaluate expr
    ifE _ = invalidForm "if"
    quoteE (Cell x Null) = returnExpr x
    quoteE _ = invalidForm "quote"
    notE (Cell LispFalse Null) = returnExpr LispTrue
    notE (Cell LispTrue Null) = returnExpr LispFalse
    notE _ = invalidForm "not"
    consE (Cell a (Cell b Null)) = returnExpr $ Cell a b
    consE _ = invalidForm "cons"
    carE (Cell v _) = returnExpr v
    carE _ = invalidForm "car"
    cdrE (Cell _ v) = returnExpr v
    cdrE _ = invalidForm "cdr"
    mathE op = maybe (invalidForm op) returnExpr . math op
    lambdaE (Cell bindings bodies) = maybe (invalidForm "lambda") returnExpr $ mkLambda bindings bodies
    lambdaE _ = invalidForm "lambda"
    displayE (Cell x xs) = (liftIO $ print x) >> displayE xs
    displayE Null = (liftIO $ print Null) >> returnExpr Null
    displayE _ = invalidForm "display"
    letBangE (Cell (Atom k) (Cell v Null)) = insertEnv k v
    letBangE _ = invalidForm "let!"
    isIntegerE (Cell (Integer _) Null) = returnExpr LispTrue
    isIntegerE (Cell _ Null)           = returnExpr LispFalse
    isIntegerE _                       = invalidForm "integer?"
    isRealE    (Cell (Real _) Null)    = returnExpr LispTrue
    isRealE    (Cell _ Null)           = returnExpr LispFalse
    isRealE    _                       = invalidForm "real?"
    isStringE  (Cell (String _) Null)  = returnExpr LispTrue
    isStringE  (Cell _ Null)           = returnExpr LispFalse
    isStringE  _                       = invalidForm "string?"
    isAtomE    (Cell (Atom _) Null)    = returnExpr LispTrue
    isAtomE    (Cell _ Null)           = returnExpr LispFalse
    isAtomE    _                       = invalidForm "atom?"
    isNullE    (Cell Null Null)        = returnExpr LispTrue
    isNullE    (Cell _ Null)           = returnExpr LispFalse
    isNullE    _                       = invalidForm "null?"
    isListE    (Cell (Cell _ xs) Null) = isListE $ Cell xs Null
    isListE    (Cell Null Null)        = returnExpr LispTrue
    isListE    (Cell _ _)              = returnExpr LispFalse
    isPairE    (Cell (Cell _ _) _)     = returnExpr LispTrue
    isPairE    (Cell _ _)              = returnExpr LispFalse

math :: ByteString -> Expression -> Maybe Expression
math "+"  (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Integer $ a + b
math "+"  (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) + b
math "+"  (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a + (fromInteger b)
math "+"  (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a + b
math "-"  (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Integer $ a - b
math "-"  (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) - b
math "-"  (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a - (fromInteger b)
math "-"  (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a - b
math "*"  (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Integer $ a * b
math "*"  (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) * b
math "*"  (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a * (fromInteger b)
math "*"  (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a * b
math "/"  (Cell (Integer a) (Cell (Integer b) Null)) = Just $ Real $ (fromInteger a) / (fromInteger b)
math "/"  (Cell (Integer a) (Cell (Real b) Null))    = Just $ Real $ (fromInteger a) / b
math "/"  (Cell (Real a) (Cell (Integer b) Null))    = Just $ Real $ a / (fromInteger b)
math "/"  (Cell (Real a) (Cell (Real b) Null))       = Just $ Real $ a / b
math "==" (Cell a (Cell b Null))                     = Just $ if a == b then LispTrue else LispFalse
math op   (Cell a (Cell b xs))                       = math op x >>= math op . flip Cell xs where x = (Cell a (Cell b Null))
math _    _                                          = Nothing

returnExpr :: Expression -> LispM ()
returnExpr e = LispM $ \env -> return ((),env,e)

getReturnedExpr :: LispM () -> LispM Expression
getReturnedExpr (LispM f) = LispM $ \env -> (\(_,_,e) -> (e,env,Null)) <$> f env

-- Lists

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
    f _ _ = Nothing
    
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
    else Just $ Procedure $ \expr -> do
      case fromConsList expr of
        Nothing -> error $ "Invalid arguments (not a cons list)." -- TODO: print @expr@
        Just args -> do
          pushEnvFrame $ H.fromList $ zip bindings' args
          ret <- getReturnedExpr $ last $ map evaluate bodies'
          popEnvFrame
          returnExpr ret
  where
    fromAtoms acc Null = acc
    fromAtoms acc (Cell (Atom a) xs) = fromAtoms (fmap ((:) a) acc) xs
    fromAtoms _ _ = Nothing

-- Environment

type EnvFrame = HashMap ByteString Expression
data Environment = Frame Environment EnvFrame | Empty

createEnv :: Environment
createEnv = Empty

-- |Pop a "stack frame".
popEnvFrame :: LispM ()
popEnvFrame = LispM f
  where f Empty = error "Cannot pop empty stack."
        f (Frame xs _) = return ((),xs,Null)

-- |Push a "stack frame"
pushEnvFrame :: EnvFrame -> LispM ()
pushEnvFrame child = LispM $ \env -> return ((),Frame env child,Null)

-- |Insert a value into the environment.
insertEnv :: ByteString -> Expression -> LispM ()
insertEnv k v = LispM f
  where f Empty = error "No stack frame!"
        f (Frame xs x) = return ((),Frame xs (H.insert k v x),v)

-- |Lookup a value in the environment.
lookupEnv :: ByteString -> LispM (Maybe Expression)
lookupEnv k = LispM $ \env -> return (f $ Frame env primitives ,env,Null)
  where f Empty = Nothing
        f (Frame xs x) = maybe (f xs) Just (H.lookup k x)