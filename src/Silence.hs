module Silence
(
  module Silence.Expression,
  module Silence.Syntax,
  evalExpressions',
  evalExpressions
)
where
  
{- TODO:
* infix syntax
* vectors?
* call/cc & concurrency
* first class environments
* standard library
* quasiquoter
* pattern matching
-}

import Silence.Syntax
import Silence.Expression
import Silence.Primitives
import Control.Monad.State.Strict

-- |Evaluate a list of 'Expression's in a new environment.
evalExpressions' :: [Expression] -> IO (Expression,[Scope])
evalExpressions' = evalExpressions []
 
-- |Evaluate a list of 'Expression's in a given environment.
evalExpressions :: [Scope] -> [Expression] -> IO (Expression,[Scope])
evalExpressions env es = runStateT (runLispM $ evaluate l) ((primitives `mappend` primitiveConstants):env)
  where l = Cell (mkLambda [] es) Null -- wrap expressions in a lambda