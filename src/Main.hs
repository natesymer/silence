{-# LANGUAGE OverloadedStrings #-}

import Silence

import System.Environment
import System.IO
import Control.Exception.Base hiding (evaluate)
import GHC.IO.Exception 
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Options.Applicative hiding (str)

main :: IO ()
main = getArgs >>= getCommandArgs >>= evalCmd

repl :: String -> IO ()
repl p = repl' p []
  where repl' prompt env = do
          putStr prompt >> hFlush stdout
          (try $ B.hGetLine stdin) >>= \i -> case i of
            Left (IOError _ EOF _ _ _ _) -> return ()
            Left (IOError _ _ _ msg _ _) -> putStrLn $ "IOError: " ++ msg
            Right str -> do
              -- TODO: catch errors & make prompt an error prompt
              (res,env') <- evalExpressions env $ parseSilence str
              print res
              repl' prompt env'

data Cmd = Cmd (Maybe FilePath) (Maybe String) Bool String

evalCmd :: Cmd -> IO ()
evalCmd (Cmd Nothing (Just src) False _) = void $ evalExpressions' $ parseSilence $ B.pack src
evalCmd (Cmd (Just fp) Nothing False _) = do
  src <- readFile fp
  evalCmd $ Cmd Nothing (Just src) False ""
evalCmd (Cmd Nothing Nothing True prompt) = repl prompt
evalCmd _ = error "invalid arguments."

getCommandArgs :: [String] -> IO Cmd
getCommandArgs = handleParseResult . execParserPure pprefs parser
  where
    pprefs = ParserPrefs "" False True True 80
    parser = info (helper <*> parser') (fullDesc <> header "Silence Lisp")
    parser' = Cmd
      <$> (optional $ strOption $ opt "file" 'f' "FILEPATH" Nothing "source file to evaluate")
      <*> (optional $ strOption $ opt "evaluate" 'e' "CODE" Nothing "source code to evaluate")
      <*> (flag False True $ short 'r' <> long "repl" <> help "run a REPL")
      <*> (strOption $ opt "prompt" 't' "PROMPT" (Just "𝝺 ") "what to print at the start of each line")
    opt lng shrt mvar (Just defVal) hlp = (long lng <> short shrt <> metavar mvar <> value defVal <> help hlp)
    opt lng shrt mvar Nothing       hlp = (long lng <> short shrt <> metavar mvar <> help hlp)