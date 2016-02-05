{-# LANGUAGE OverloadedStrings #-}

module Felony.Repl where
import           Felony.Lisp
import           Felony.Parser
import           System.IO
import           Control.Exception.Base hiding (evaluate)
import           GHC.IO.Exception 

catcher :: String -> IO Expression
catcher errmsg = putStrLn errmsg >> return Null

repl :: Handle -> Handle -> String -> IO ()
repl outp inp prompt = do
  hPutStr outp prompt
  hFlush outp
  (try $ hGetLine inp) >>= \i -> case i of
    Left (IOError _ EOF _ _ _ _) -> return ()
    Left (IOError _ _ _ errormessage _ _) -> hPutStrLn outp $ "Error: " ++ errormessage
    Right str -> do
      evalCode str >>= (hPrint outp)
      repl outp inp prompt
      
terminalRepl :: String -> IO ()
terminalRepl prompt = repl stdout stdin prompt

evalCode :: String -> IO Expression
evalCode code = thrd <$> runLispM e createEnv
  where f [x] = x
        f x = toConsList x
        e = evaluate $ f $ parseFelony code
        thrd (_,_,v) = v