{-# LANGUAGE OverloadedStrings #-}

module Felony.Repl where
import           Felony
import           Control.Monad
import           System.IO
import           Control.Exception.Base
import           System.IO.Error
import           GHC.IO.Exception

repl :: Handle -> Handle -> String -> IO ()
repl outp inp prompt = do
  hPutStr outp prompt
  hFlush outp
  (try $ hGetLine inp) >>= \i -> case i of
    Left (IOError _ EOF _ _ _ _) -> return ()-- do nothing, exit
    Left (IOError _ _ _ errormessage _ _) -> hPutStrLn outp $ "Error: " ++ errormessage
    Right str -> do
      ((flip (>>=) $ (hPrint outp)) . lispRun . lispRead) str -- TODO: catch language errors
      repl outp inp prompt
      
terminalRepl :: String -> IO ()
terminalRepl prompt = repl stdout stdin prompt
