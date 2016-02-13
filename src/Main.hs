{-# LANGUAGE OverloadedStrings #-}

import Felony
import System.Environment
import System.IO
import Control.Exception.Base hiding (evaluate)
import GHC.IO.Exception 
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Options.Applicative hiding (str)

main :: IO ()
main = getArgs >>= getCommandArgs >>= evalCmd

repl :: Bool -> String -> IO ()
repl silent prompt = do
  putStr prompt >> hFlush stdout
  (try $ B.hGetLine stdin) >>= \i -> case i of
    Left (IOError _ EOF _ _ _ _) -> return ()
    Left (IOError _ _ _ msg _ _) -> putStrLn $ "IOError: " ++ msg
    Right str -> do
      (evalExpressions $ parseFelony str) >>= if silent then (const $ return ()) else print
      repl silent prompt
      
data Cmd = Cmd {
  cmdFilePath :: Maybe FilePath,
  cmdSource :: Maybe String,
  cmdSilent :: Bool,
  cmdRepl :: Bool,
  cmdPrompt :: String
}

evalCmd :: Cmd -> IO ()
evalCmd (Cmd Nothing (Just src) False False _) = (evalExpressions $ parseFelony $ B.pack src) >>= print
evalCmd (Cmd Nothing (Just src) True False _) = void $ evalExpressions $ parseFelony $ B.pack src
evalCmd (Cmd (Just fp) Nothing silent False _) = do
  src <- readFile fp
  evalCmd $ Cmd Nothing (Just src) silent False ""
evalCmd (Cmd Nothing Nothing silent True prompt) = repl silent prompt
evalCmd _ = error "invalid arguments."

getCommandArgs :: [String] -> IO Cmd
getCommandArgs = handleParseResult . execParserPure pprefs parser
  where
    pprefs = ParserPrefs "" False True True 80
    parser = info (helper <*> parser') (fullDesc <> header "Felony Lisp")
    parser' = Cmd
      <$> (optional $ strOption $ opt "file" 'f' "FILEPATH" Nothing "source file to evaluate")
      <*> (optional $ strOption $ opt "evaluate" 'e' "CODE" Nothing "source code to evaluate")
      <*> (flag False True $ short 's' <> long "silent" <> help "don't print return values")
      <*> (flag False True $ short 'r' <> long "repl" <> help "run a REPL")
      <*> (strOption $ opt "prompt" 't' "PROMPT" (Just "𝝺 ") "what to print at the start of each line")
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)