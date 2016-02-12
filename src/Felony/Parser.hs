{-# LANGUAGE OverloadedStrings #-}
module Felony.Parser
(
  parseFelony
)
where
  
import Felony.Lisp
import Data.Char
import Text.Parsec
import Control.Monad (void)
import Data.Monoid

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

{-

  General
                
-}
   
parseFelony :: ByteString -> [Expression]
parseFelony = f . parse parseCode ""
  where f (Left err) = error $ "Invalid syntax: " ++ show err 
        f (Right e) = e

parseCode :: Parsec ByteString () [Expression]
parseCode = manyTill (between skipWS skips parseExpr) eof
  where skips = try $ skipMany $ choice [void whitespace, void comment]
        skipWS = try $ skipMany $ void whitespace
        -- skipCmnts = try $ skipMany $ void comment
    
-- FIXME: Avoid parsing two atoms on one line next to eachother, outside an expr          
parseExpr :: Parsec ByteString () Expression
parseExpr = choice [parseQuoted, parseNumber, parseBool, parseAtom, parseString, parseList]

{-

  DRY
  
-}
     
symbol :: Parsec ByteString () Char
symbol = satisfy p
  where p '!' = True
        p '$' = True
        p '%' = True
        p '&' = True
        p '|' = True
        p '*' = True
        p '+' = True
        p '-' = True
        p '/' = True
        p ':' = True
        p '<' = True
        p '=' = True
        p '>' = True
        p '?' = True
        p '@' = True
        p '^' = True
        p '_' = True
        p '~' = True
        p _   = False

whitespace :: Parsec ByteString () Char
whitespace = newline <|> space

comment :: Parsec ByteString () String
comment = char ';' *> (manyTill anyToken $ (void newline) <|> eof)

parseAtom :: Parsec ByteString () Expression
parseAtom = fmap (Atom . B.pack) $ (:)
            <$> (letter <|> symbol)
            <*> (many $ letter <|> digit <|> symbol)

parseBool :: Parsec ByteString () Expression
parseBool = true <|> false
  where true = try $ string "#t" >> return LispTrue
        false = try $ string "#f" >> return LispFalse

parseString :: Parsec ByteString () Expression
parseString = String . B.pack <$> between q q (many accepted)
  where q = char '"'
        accepted = noneOf "\"" <|> (char '\\' >> char '\"')

parseQuoted :: Parsec ByteString () Expression
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ Cell (Atom "quote") (Cell x Null)

-- TODO: allow for syntax like @'(1 2 . 3)@
-- TODO: ensure evaluated syntax (like quote wrapping) doesn't turn into part of the list
parseList :: Parsec ByteString () Expression
parseList = between lp rp $ list id
  where
    lp = char '('
    rp = char ')'
    skipw = skipMany whitespace
    parseExpr' = skipw *> parseExpr <* skipw
    item = optionMaybe $ skipw *> parseExpr'
    dot = optionMaybe $ skipw *> char '.'
    list acc = dot >>= maybe (acc <$> parseExpr') (const readItem)
      where readItem = item >>= maybe (return $ acc Null) (\v -> p $ acc . Cell v)

parseNumber :: Parsec ByteString () Expression
parseNumber = real <|> dec <|> hex <|> binary <|> octal
  where
    real = try $ do
      s <- sign
      x <- many digit
      char '.'
      y <- many1 digit
      let base = length y
          signfcnd = fromRational . toRational $ str2dec $ B.pack $ x <> y
      return $ Real $ s * signfcnd / (10.0^base)
    dec = try $ do
      s <- sign
      i <- str2dec . B.pack <$> many1 digit
      return $ Integer $ s * i
    hex = try $ string "#x" *> ((Integer . hex2dec . B.pack) <$> many1 (digit <|> oneOf "abcdefABCDEF"))
    binary = try $ string "#b" *> ((Integer . bin2dec . B.pack) <$> many1 digit)
    octal = try $ string "#o" *> ((Integer . oct2dec . B.pack) <$> many1 digit)
    
sign :: (Num a) => Parsec ByteString () a
sign = f <$> (optionMaybe $ char '-')
  where f Nothing = 1
        f (Just _) = -1

bin2dec :: ByteString -> Integer
bin2dec = baseNToDec 2 -- Little Endian

oct2dec :: ByteString -> Integer
oct2dec = baseNToDec 8

str2dec :: ByteString -> Integer
str2dec = baseNToDec 10

hex2dec :: ByteString -> Integer
hex2dec = baseNToDec 16

baseNToDec :: Integer -> ByteString -> Integer
baseNToDec base str = f ((B.length str) - 1) str 0
  where f pos bs ttl = case B.uncons bs of
          Nothing -> ttl
          Just (x,xs) -> if int >= base -- Prevent invalid values!
                           then position (base-1)
                           else position int
            where int = toInteger $ digitToInt x
                  position v = f (pos-1) xs $ ttl + ((base^pos) * v)
