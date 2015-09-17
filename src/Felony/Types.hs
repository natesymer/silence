module Felony.Types
(
  Expression(..),
  Environment(..),
  Felony
)
where
  
{-
  
  Why is this file here?

  Cyclic dependency hell
  
  -}
  
import Data.HashMap.Strict (HashMap)
import Control.Monad.State.Strict (StateT)
  
data Environment = Environment {
  envBindings :: HashMap String Expression,
  envParent :: Maybe Environment
} deriving (Eq, Show)

type Felony a = StateT Environment IO a
  
data Expression = Atom String
                | String String
                | Integer Integer
                | Real Double
                | Bool Bool
                | Procedure Environment [String] [Expression]
                | Null
                | Cell Expression Expression deriving (Eq, Show)