module Felony.Monad
(
  Felony
)
where

import Felony.Expression
import Control.Monad.State.Strict (StateT)

type Felony a = StateT Environment IO a