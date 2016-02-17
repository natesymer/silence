module Felony
(
  module Felony.Semantics,
  module Felony.Syntax,
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
  
import Felony.Semantics
import Felony.Syntax
import Felony.Types
import Felony.Primitives
import Control.Monad.State.Strict

-- |Evaluate a list of 'Expression's in a new environment.
evalExpressions' :: [Expression] -> IO (Expression,Environment)
evalExpressions' = evalExpressions Empty
 
-- |Evaluate a list of 'Expression's in a given environment.
evalExpressions :: Environment -> [Expression] -> IO (Expression,Environment)
evalExpressions env es = runStateT (runLispM $ evaluate l) (Frame env primitives)
  where l = Cell (mkLambda [] es) Null -- wrap expressions in a lambda