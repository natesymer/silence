{-# LANGUAGE OverloadedStrings #-}

module Felony.Repl
(
  repl,
  terminalRepl,
  evalCode
)
where

import Felony.Lisp
import Felony.Parser
import System.IO
import Control.Exception.Base hiding (evaluate)
import GHC.IO.Exception 
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

repl :: Handle -> Handle -> String -> IO ()
repl outp inp prompt = do
  hPutStr outp prompt
  hFlush outp
  (try $ B.hGetLine inp) >>= \i -> case i of
    Left (IOError _ EOF _ _ _ _) -> return ()
    Left (IOError _ _ _ errormessage _ _) -> hPutStrLn outp $ "Error: " ++ errormessage
    Right str -> do
      evalCode str >>= (hPrint outp)
      repl outp inp prompt
      
terminalRepl :: String -> IO ()
terminalRepl prompt = repl stdout stdin prompt

evalCode :: ByteString -> IO Expression
evalCode = evalExpressions . parseFelony