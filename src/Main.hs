{-# LANGUAGE OverloadedStrings #-}

import Felony
import Felony.Repl
import System.IO
import System.Environment
import Control.Monad

main :: IO ()
main = getArgs >>= procArgs

procArgs :: [String] -> IO ()
procArgs [] = procArgs ["-r"]
procArgs ["-r"] = Felony.Repl.terminalRepl "𝝺 "
procArgs ["-ep", code] = evalProgram code >>= print
procArgs ["-e", code] = void $ evalProgram code
procArgs ["-f", fp] = readFile fp >>= \c -> procArgs ["-e", c]
procArgs ["-fp", fp] = readFile fp >>= \c -> procArgs ["-ep", c]
procArgs _ = (putStrLn "Invalid arguments") >> printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "This is the help message... Kinda sucks, right?"