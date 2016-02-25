{-# LANGUAGE OverloadedStrings #-}
module Silence
(
  module Silence.Expression,
  module Silence.Syntax,
  evalExpression',
  evalExpression
)
where
  
{- TODO:
* certainly
  * call/cc & concurrency
  * first class environments
  * standard library
  * quasiquoter
  * vectors
* pipe dreams
  * infix syntax
  * pattern matching
-}

import Silence.Syntax
import Silence.Expression
import Silence.Primitives
import Control.Monad.State.Strict

-- |Evaluate an 'Expression' in a new environment.
evalExpression' :: Expression -> IO (Expression,[Scope])
evalExpression' = evalExpression []
 
-- |Evaluate an  'Expression' in a given environment.
evalExpression :: [Scope] -> Expression -> IO (Expression,[Scope])
evalExpression env ex = runStateT (runLispM $ evaluate ex) ((primitives `mappend` primitiveConstants):env)