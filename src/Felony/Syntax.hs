{-# LANGUAGE OverloadedStrings #-}

module Felony.Syntax
(
  Expression(..),
  parseFelony
)
where
  
import Felony.Types (Expression(..))
import Control.Monad
import Text.Parsec
import Data.Char
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

parseFelony :: ByteString -> [Expression]
parseFelony = f . parse parseCode ""
  where f = either (error . mappend "Invalid syntax: " . show) id

parseCode :: Parsec ByteString () [Expression]
parseCode = manyTill (skips *> parseExpr <* skips) eof
  where skips = try $ skipMany $ (void whitespace <|> void comment)
      
parseExpr :: Parsec ByteString () Expression
parseExpr = choice [parseQuoted, parseList, parseAtom, parseBool, parseNumber, parseString]
  
symbol :: Parsec ByteString () Char
symbol = satisfy p <?> "symbol"
  where p '!' = True; p '$' = True; p '%' = True; p '&' = True; p '|' = True
        p '*' = True; p '+' = True; p '-' = True; p '/' = True; p ':' = True
        p '<' = True; p '=' = True; p '>' = True; p '?' = True; p '@' = True
        p '^' = True; p '_' = True; p '~' = True; p _   = False

whitespace :: Parsec ByteString () Char
whitespace = newline <|> space <?> "whitespace"

comment :: Parsec ByteString () String
comment = char ';' *> (manyTill anyToken $ (void newline) <|> eof) <?> "comment"

parseAtom :: Parsec ByteString () Expression
parseAtom = (fmap (Atom . B.pack) $ (:) <$> ls <*> (many $ ls <|> digit)) <?> "atom"
  where ls = letter <|> symbol

parseBool :: Parsec ByteString () Expression
parseBool = true <|> false <?> "bool"
  where true  = try $ string "#t" *> return LispTrue
        false = try $ string "#f" *> return LispFalse

parseString :: Parsec ByteString () Expression
parseString = String . B.pack <$> (q *> many accepted <* q) <?> "string"
  where q = char '"'
        accepted = noneOf "\"" <|> (char '\\' *> char '"')

parseQuoted :: Parsec ByteString () Expression
parseQuoted = char '\'' *> (wrap <$> parseExpr) <?> "quote"
  where wrap x = Cell (Atom "quote") (Cell x Null)

parseList :: Parsec ByteString () Expression
parseList = char '(' *> list id <* char ')' <?> "list"
  where skipw = skipMany whitespace
        dotted acc = try $ char '.' *> skipw *> (acc <$> parseExpr)
        proper acc = option (acc Null) (parseExpr >>= \v -> list $ acc . Cell v)
        list acc = skipw *> (dotted acc <|> proper acc) <* skipw

parseNumber :: Parsec ByteString () Expression
parseNumber = real <|> dec <|> hex <|> binary <|> octal <?> "number"
  where real = try $ f <$> (fromIntegral <$> dec' <* char '.')
                       <*> (length <$> lookAhead (many1 digit))
                       <*> (fromIntegral <$> dec')
          where f x base y = Real . (+) x . (/) y . (^) 10.0 $ base
        dec'   = (*) <$> sign <*> baseN 10
        dec    = try $ fmap Integer $ (*) <$> sign <*> baseN 10
        hex    = try $ string "#x" *> (Integer <$> baseN 16)
        binary = try $ string "#b" *> (Integer <$> baseN 2)
        octal  = try $ string "#o" *> (Integer <$> baseN 8)
    
sign :: (Num a) => Parsec ByteString () a
sign = option 1 (char '-' *> return (-1)) <?> "sign"

baseN :: Integer -> Parsec ByteString () Integer
baseN n = many1 alphaNum >>= f 0
  where f acc [] = return acc
        f acc (x:xs) 
          | int >= n = unexpected "digit is larger than the base."
          | otherwise = f ((+) acc . (*) int . (^) n $ length xs) xs
          where int = toInteger $ digitToInt x