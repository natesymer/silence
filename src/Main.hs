{-# LANGUAGE OverloadedStrings #-}

import Felony
import System.Environment
import System.IO
import Control.Exception.Base hiding (evaluate)
import GHC.IO.Exception 
import Control.Monad
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = getArgs >>= procArgs

-- TODO: Optparse-Applicative

procArgs :: [String] -> IO ()
procArgs [] = procArgs ["-r"]
procArgs ["-r"] = repl "𝝺 "
procArgs ["-ep", code] = (evalExpressions $ parseFelony $ B.pack code) >>= print
procArgs ["-e", code] = void $ evalExpressions $ parseFelony $ B.pack code
procArgs ["-f", fp] = readFile fp >>= \c -> procArgs ["-e", c]
procArgs ["-fp", fp] = readFile fp >>= \c -> procArgs ["-ep", c]
procArgs _ = putStrLn "Invalid arguments" >> printHelp

printHelp :: IO ()
printHelp = putStrLn "This is the help message... Kinda sucks, right?"

repl :: String -> IO ()
repl prompt = do
  putStr prompt >> hFlush stdout
  (try $ B.hGetLine stdin) >>= \i -> case i of
    Left (IOError _ EOF _ _ _ _) -> return ()
    Left (IOError _ _ _ errormessage _ _) -> putStrLn $ "Error: " ++ errormessage
    Right str -> do
      (evalExpressions $ parseFelony str) >>= print
      repl prompt