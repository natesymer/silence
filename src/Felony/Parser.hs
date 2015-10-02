module Felony.Parser
(
  parseFelony
)
where
  
import Felony.Expression
import Data.Char
import Text.Parsec

{-

  General
                
-}
   
parseFelony :: String -> [Expression]
parseFelony "" = []
parseFelony input = f $ parse parseCode "" input
  where f (Left err) = error $ "Invalid syntax: " ++ show err 
        f (Right e) = e

parseCode :: Parsec String () [Expression]
parseCode = manyTill p eof
  where p = do
          skipMany whitespace
          e <- parseExpr
          skipMany whitespace
          return e
              
parseExpr :: Parsec String () Expression
parseExpr = choice [parseQuoted, parseNumber, parseBool, parseAtom, parseString, parseList]

{-

  DRY
  
-}
     
symbol :: Parsec String () Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

whitespace :: Parsec String () Char
whitespace = newline <|> space
                
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
  
-- TODO: Rewrite
-- Take into account escaping  
parseString :: Parsec String () Expression
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
  char '"'
  return $ toConsList $ map (\c -> Atom [c]) x
      
{-
              
  Numbers

-}          
       
-- TODO: Fix inaccuracies in parsing Reals         
parseNumber :: Parsec String () Expression
parseNumber = choice [real, dec, notatedDec, hex, binary, octal]
  where
    toDbl = fromRational . toRational
    real = try $ do
      x <- many digit
      char '.'
      y <- many1 digit
      return $ Real $ (toDbl $ str2dec x) + ((toDbl $ str2dec y)/((toDbl $ toInteger $ length y)*10.0))
    dec = try $ (Integer . str2dec) <$> many1 digit
    notatedDec = try $ string "#d" >> ((Integer . str2dec) <$> many1 digit)
    hex = try $ string "#x" >> ((Integer . hex2dec) <$> many1 (digit <|> oneOf "abcdefABCDEF"))
    binary = try $ string "#b" >> ((Integer . bin2dec) <$> many1 digit)
    octal = try $ string "#o" >> ((Integer . oct2dec) <$> many1 digit)
    
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

str2dec :: String -> Integer
str2dec = baseNToDec 1

bin2dec :: String -> Integer
bin2dec = baseNToDec 2 . reverse -- Little Endian

oct2dec :: String -> Integer
oct2dec = baseNToDec 8

hex2dec :: String -> Integer
hex2dec = baseNToDec 16

baseNToDec :: Integer -> String -> Integer
baseNToDec base str = f base 0 str 0
  where
    f :: Integer -> Integer -> String -> Integer -> Integer
    f _ _     []     accum = accum
    f n lspos (x:xs) accum = f n (lspos+1) xs (accum+((n^lspos)*(toInteger $ digitToInt x)))
           
-- eol :: Parser ()
-- eol = (void newline) <|> eof
--
-- skipComments :: Parser ()
-- skipComments = optional $ do
--   char ';'
--   void $ manyTill anyToken eol
