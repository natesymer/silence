module Felony.Parser
(
  parseFelony
)
where
  
import Felony.Lisp
import Data.Char
import Text.Parsec
import Control.Monad (void)

{-

  General
                
-}
   
parseFelony :: String -> [Expression]
parseFelony = f . parse parseCode ""
  where f (Left err) = error $ "Invalid syntax: " ++ show err 
        f (Right e) = e

parseCode :: Parsec String () [Expression]
parseCode = manyTill (between skipWS skips parseExpr) eof
  where skips = try $ skipMany $ choice [void whitespace, void comment]
        skipWS = try $ skipMany $ void whitespace
        skipCmnts = try $ skipMany $ void comment
    
-- FIXME: Avoid parsing two atoms on one line next to eachother, outside an expr          
parseExpr :: Parsec String () Expression
parseExpr = choice [parseQuoted, parseNumber, parseBool, parseAtom, parseString, parseList]

{-

  DRY
  
-}
     
symbol :: Parsec String () Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

whitespace :: Parsec String () Char
whitespace = newline <|> space

comment :: Parsec String () String
comment = do
  string ";"
  manyTill anyToken $ choice [void newline, eof]
      
{-

  Atoms
        
-}

parseAtom :: Parsec String () Expression
parseAtom = (fmap f . (:)) <$> fstChar <*> rest
  where
    f xs = Atom xs
    fstChar = choice [letter, symbol]
    rest = many $ choice [letter, digit, symbol]

parseBool :: Parsec String () Expression
parseBool = choice [string "#t", string "#f"] >>= f
  where f "#t" = return $ Bool True
        f "#f" = return $ Bool False
        f e = unexpected $ "unexpetected:" ++ e

{-
              
  Strings

-} 

parseString :: Parsec String () Expression
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
  char '"'
  return $ String x
      
{-
              
  Numbers

-}          

parseNumber :: Parsec String () Expression
parseNumber = real <|> dec <|> hex <|> binary <|> octal
  where
    real = try $ do
      s <- sign
      x <- many digit
      char '.'
      y <- many1 digit
      let base = length y
          signfcnd = fromRational . toRational $ str2dec $ x ++ y
      return $ Real $ s * signfcnd / (10.0^base)
    dec = try $ do
      s <- sign
      i <- str2dec <$> many1 digit
      return $ Integer $ s * i
    hex = try $ string "#x" *> ((Integer . hex2dec) <$> many1 (digit <|> oneOf "abcdefABCDEF"))
    binary = try $ string "#b" *> ((Integer . bin2dec) <$> many1 digit)
    octal = try $ string "#o" *> ((Integer . oct2dec) <$> many1 digit)
    
sign :: (Num a) => Parsec String () a
sign = f <$> (optionMaybe $ char '-')
  where f Nothing = 1
        f (Just _) = -1
    
{-
              
  Quotation

-}          
   
parseQuoted :: Parsec String () Expression
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ Cell (Atom "quote") (Cell x Null)
    
{-

  Lists

-}
    
parseList :: Parsec String () Expression
parseList = between lp rp (properList <|> dottedList)
  where
    lp = char '('
    rp = char ')'
    properList = try $ do
      skipMany whitespace
      lst <- sepBy parseExpr (skipMany1 whitespace)
      skipMany whitespace
      return $ toConsList lst
    dottedList = try $ do
      skipMany whitespace
      h <- parseExpr
      skipMany whitespace
      char '.'
      skipMany whitespace
      t <- parseExpr
      skipMany whitespace
      return $ Cell h t
    
{-
    
  String Coercion
  
-}

bin2dec :: String -> Integer
bin2dec = baseNToDec 2 -- Little Endian

oct2dec :: String -> Integer
oct2dec = baseNToDec 8

str2dec :: String -> Integer
str2dec = baseNToDec 10

hex2dec :: String -> Integer
hex2dec = baseNToDec 16

baseNToDec :: Integer -> String -> Integer
baseNToDec base str = f ((length str) - 1) str 0
  where
    f _     []     ttl = ttl
    f pos (x:xs) ttl = f (pos-1) xs $ ttl + ((base^pos) * (toInteger $ digitToInt x))
