module Felony.Environment
(
  envInsert,
  envDelete,
  envLookup,
  envExtend,
  mkEnv,
  mkChildEnv,
  module Felony.Types
)
where
 
import Felony.Expression
import Felony.Types

import Data.Default

import qualified Data.HashMap.Strict as H

instance Default Environment where
  def = Environment H.empty Nothing

mkEnv :: Maybe Environment -> [String] -> [Expression] -> Environment
mkEnv mparent keys values = (zipEnv keys values) { envParent = mparent }
  where
    zipEnv key expr = Environment (H.fromList $ f key expr) Nothing
      where
        f _ [] = []
        f [] _ = []
        f ("*":_) exs = [("*", toConsList exs)]
        f (k:ks) (ex:exs) = (k, ex):(f ks exs)

mkChildEnv :: Environment -> Environment
mkChildEnv env = def { envParent = (Just env) }

envInsert :: String -> Expression -> Environment -> Environment
envInsert key expr (Environment bindings parent) = Environment (H.insert key expr bindings) parent

envDelete :: String -> Environment -> Environment
envDelete k (Environment bindings parent) = Environment (H.delete k bindings) parent

envLookup :: String -> Environment -> Maybe Expression
envLookup k (Environment bindings parent) = f $ H.lookup k bindings
  where f Nothing = parent >>= envLookup k
        f v@(Just _) = v
        
envExtend :: Environment -> Environment -> Environment
envExtend parent (Environment bindings _) = Environment bindings (Just parent)

-- TODO: See what's up
-- setParentEnvironment :: String -> Environment -> Expression -> Environment
-- setParentEnvironment key (Environment b (Just (Environment pb pp))) expr = Environment b (Just $ Environment (H.insert key expr pb) pp)
-- setParentEnvironment key env _ = env