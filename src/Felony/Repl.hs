{-# LANGUAGE OverloadedStrings #-}

module Felony.Repl where
import           Felony
import           Control.Monad
import           System.IO
import           Control.Exception.Base
import           System.IO.Error
import           GHC.IO.Exception

evalProgram :: String -> IO Expression
evalProgram str = catch
                    (lispEval (lispRead str) emptyEnvironment)
                    (\e -> (flip lispEval $ emptyEnvironment) $ Cell (Atom "display") (Cell (String (show (e :: ErrorCall))) Null))

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
      evalProgram str >>= (hPrint outp)
      repl outp inp prompt
      
terminalRepl :: String -> IO ()
terminalRepl prompt = repl stdout stdin prompt