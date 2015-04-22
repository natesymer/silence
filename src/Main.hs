{-# LANGUAGE OverloadedStrings #-}

import Felony
import System.IO
import System.Environment

-- TODO: work as shell
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r"] -> Felony.repl "𝝺 "
    [fp] -> readFile fp >>= \code -> print $ evalProgram code
    _ -> putStrLn "Invalid arguments"
  
  