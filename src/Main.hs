{-# LANGUAGE OverloadedStrings #-}

import Felony
import Felony.Repl
import System.IO
import System.Environment

main :: IO ()
main = getArgs >>= procArgs

evalProgram :: String -> IO Expression
evalProgram = lispEvalToplevel . lispRead

procArgs :: [String] -> IO ()
procArgs ["-r"] = Felony.Repl.terminalRepl "𝝺 "
procArgs ["-ep", code] = evalProgram code >>= print
procArgs ["-e", code] = seq (evalProgram code) return ()
procArgs ["-f", fp] = readFile fp >>= \c -> procArgs ["-e", c]
procArgs ["-fp", fp] = readFile fp >>= \c -> procArgs ["-ep", c]
procArgs _ = (putStrLn "Invalid arguments") >> printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "This is the help message... Kinda sucks, right?"