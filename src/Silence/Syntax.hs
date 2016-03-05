{-# LANGUAGE OverloadedStrings #-}

module Silence.Syntax
(
  Expression(..),
  parseSilence
)
where
  
import Silence.Expression

import Control.Monad
import Text.Parsec
import Data.Char
import Data.Ratio
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

skipLine :: Parsec ByteString () ()
skipLine = void newline <|> void crlf <|> eof <|> (anyToken *> skipLine)

-- |Parse code an evaluateable expression.
parseSilence :: ByteString -> Expression
parseSilence = wrap . either (error . show) id . parse code ""
  where wrap = Cell (Atom "begin") . toConsList

-- |Parser for code (list of expressions)
code :: Parsec ByteString () [Expression]
code = shebang *> exprs
  where exprs = (many $ ignored *> expr <* ignored)
        shebang = optional (try $ string "#!" *> skipLine) <?> "shebang"
  
-- |Parser for an expression.
expr :: Parsec ByteString () Expression
expr = choice [equoted,elist,ebool,enumber,eatom,estring]

-- |Skip comments & whitespace
ignored :: Parsec ByteString () ()
ignored = skipMany $ whitespace <|> comment
  where whitespace = try (void (space <|> tab <|> newline <|> crlf)) <?> "whitespace"
        comment = try (char ';' *> skipLine)                         <?> "comment"
        
eatom :: Parsec ByteString () Expression
eatom = fmap (Atom . B.pack) ident <?> "atom"
  where ident = ((:) <$> initial <*> many subseq) <|> (pure <$> exc)
        initial = toLower <$> (letter <|> symbol)
        subseq = toLower <$> (initial <|> digit <|> exc)
        exc = oneOf ".+-"
        symbol = oneOf "?!$%&*/|:<=>~_^"

ebool :: Parsec ByteString () Expression
ebool = true <|> false <?> "bool"
  where true  = try (string "#t" *> return (Bool True))
        false = try (string "#f" *> return (Bool False))

estring :: Parsec ByteString () Expression
estring = wrap . toIntList <$> str <?> "string"
  where toIntList = foldr Cell Null . map (Number . toRational . ord)
        wrap = Cell (Atom "quote") . flip Cell Null
        str = q *> many accepted <* q
        q = char '"'; bs = char (chr 92)
        accepted = escaped <|> satisfy (liftM2 (&&) (chr 34 /=) (chr 92 /=))
        escaped = bs *> (choice [
          eseq "NUL" 0,eseq "SOH" 1,eseq "STX" 2,eseq "ETX" 3,eseq "EOT" 4,
          eseq "ENQ" 5,eseq "ACK" 6,eseq' "BEL" 'a' 7,eseq' "BS" 'b' 8,
          eseq' "HT" 't' 9,eseq' "LF" 'n' 10,eseq' "VT" 'v' 11,eseq' "FF" 'f' 12,
          eseq' "CR" 'r' 13,eseq "SO" 14,eseq "SI" 15,eseq "DLE" 16,eseq "DC1" 17,
          eseq "DC2" 18,eseq "DC3" 19,eseq "DC4" 20,eseq "NAK" 21,eseq "SYN" 22,
          eseq "ETB" 23,eseq "CAN" 24,eseq "EM" 25,eseq "SUB" 26,eseq "ESC" 27,
          eseq "FS" 28,eseq "GS" 29,eseq "RS" 30,eseq "US" 31,bs,
          chr . fromInteger <$> baseN 10] <?> "escape sequence")
        eseq :: String -> Int -> Parsec ByteString () Char
        eseq a c = try $ string a *> return (chr c)
        eseq' :: String -> Char -> Int -> Parsec ByteString () Char
        eseq' a b c = eseq a c <|> (char b *> return (chr c))

equoted :: Parsec ByteString () Expression
equoted = char '\'' *> (wrap <$> expr) <?> "quote"
  where wrap = Cell (Atom "quote") . flip Cell Null

elist :: Parsec ByteString () Expression
elist = char '(' *> list' id <* char ')' <?> "list"
  where list' acc = option (acc Null) $ do
          a <- ignored *> expr <* ignored
          choice [char '.' *> ignored *> (acc . Cell a <$> expr), -- dotted
                  list' $ acc . Cell a] -- proper

enumber :: Parsec ByteString () Expression
enumber = choice [fractional,dec,nonBaseTen] <?> "number"
  where fractional = try $ do
          s <- sign
          whole <- option 0 (baseN 10)
          fracDigits <- char '.' *> many1 digit
          let f = return . Number . mkRat s whole (length fracDigits)
          either fail f $ str2int 10 fracDigits
          where mkRat s whole len v = ((s*whole) % 1) + (v % (10^len))
        sign = option 1 $ char '-' *> return (-1)
        dec  = try $ Number . toRational <$> ((*) <$> sign <*> baseN 10)
        nonBaseTen = char '#' *> choice [
          char 'x' *> intN 16,
          char 'b' *> intN 2,
          char 'o' *> intN 8,
          baseN 10 <* char '|' >>= intN . fromInteger]
        intN = fmap (Number . toRational) . baseN
          
baseN :: Int -> Parsec ByteString () Integer
baseN n
  | n > 255 = fail "impossibly large base (greater than 255)"
  | otherwise = (many1 alphaNum >>= either fail return . str2int n) <?> "based number"
                
str2int :: Int -> String -> Either String Integer
str2int n s = f 0 s
  where f :: Integer -> String -> Either String Integer
        f acc [] = Right acc
        f acc (x:xs)
          | dec   < 10 = if dec   < n then g dec   else err
          | alphu < 36 = if alphu < n then g alphu else err
          | alphl < 36 = if alphl < n then g alphl else err
          | otherwise = Left "unexpected numeral out of base"
          where dec = ord x - ord '0'
                alphl = ord x - ord 'a' + 10
                alphu = ord x - ord 'A' + 10
                err = Left $ x : " is larger than base " ++ show n
                g y = f ((+) acc . toInteger . (*) y . (^) n . length $ xs) xs