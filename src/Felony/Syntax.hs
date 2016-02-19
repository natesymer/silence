{-# LANGUAGE OverloadedStrings #-}

module Felony.Syntax
(
  Expression(..),
  parseFelony
)
where
  
import Felony.Expression

import Control.Monad
import Text.Parsec
import Data.Char
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

skipTill :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m () -> ParsecT s u m ()
skipTill p end = (try end) <|> (p *> skipTill p end)

parseFelony :: ByteString -> [Expression]
parseFelony = either (error . show) id . parse code ""

code :: Parsec ByteString () [Expression]
code = skipw *> endBy expr skipw-- many $ skipw *> expr <* skipw
  where skipw = try $ skipMany $ void whitespace <|> void comment
  
expr :: Parsec ByteString () Expression
expr = choice [equoted,elist,enumber,eatom,estring,ebool]

whitespace :: Parsec ByteString () Char
whitespace = (space <|> tab <|> newline <|> crlf) <?> "whitespace"

comment :: Parsec ByteString () ()
comment = char ';' *> skipTill anyToken lineEnd <?> "comment"

lineEnd :: Parsec ByteString () ()
lineEnd = void newline <|> void crlf <|> eof

eatom :: Parsec ByteString () Expression
eatom = fmap (Atom . B.pack . map toLower) ident <?> "atom"
  where ident = ((:) <$> initial <*> many subseq) <|> (pure <$> exc)
        initial = letter <|> symbol
        subseq = initial <|> digit <|> exc
        exc = oneOf ".+-"
        symbol = oneOf "?!$%&*/|:<=>~_^"

ebool :: Parsec ByteString () Expression
ebool = true <|> false <?> "bool"
  where true  = try $ string "#t" *> return (Bool True)
        false = try $ string "#f" *> return (Bool False)

estring :: Parsec ByteString () Expression
estring = wrap . toIntList <$> str <?> "string"
  where toIntList = foldr Cell Null . map (Integer . fromIntegral . ord)
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
          eseq "FS" 28,eseq "GS" 29,eseq "RS" 30,eseq "US" 31,bs *> return (chr 92),
          chr . fromInteger <$> baseN 10] <?> "escape sequence")
        eseq :: String -> Int -> Parsec ByteString () Char
        eseq a c = try $ string a *> return (chr c)
        eseq' :: String -> Char -> Int -> Parsec ByteString () Char
        eseq' a b c = try $ (void (string a) <|> void (char b)) *> return (chr c)

equoted :: Parsec ByteString () Expression
equoted = char '\'' *> (wrap <$> expr) <?> "quote"
  where wrap = Cell (Atom "quote") . flip Cell Null

elist :: Parsec ByteString () Expression
elist = char '(' *> list' id <* char ')' <?> "list"
  where skipw = skipMany (void whitespace <|> void comment)
        list' acc = option (acc Null) $ do
          a <- skipw *> expr <* skipw
          choice [
            char '.' *> skipw *> (acc . Cell a <$> expr), -- dotted
            list' $ acc . Cell a] -- proper

enumber :: Parsec ByteString () Expression
enumber = choice [real,dec,hex,binary,octal,ntal] <?> "number"
  where real = try $ real' <$> (option 0 dec') <*> (char '.' *> (withLength $ baseN 10))
        real' x (base,y) = real'' (fromInteger x) (fromInteger y) base
        real'' x y = Real . (+) x . (/) y . (^) 10.0
        dec'   = (*) <$> sign <*> baseN 10
        dec    = try $ fmap Integer dec'
        hex    = try $ string "#x" *> (Integer <$> baseN 16)
        binary = try $ string "#b" *> (Integer <$> baseN 2)
        octal  = try $ string "#o" *> (Integer <$> baseN 8)
        ntal   = try $ char '#' *> baseN 10 <* char '|' >>= f >>= fmap Integer . baseN
          where f n = if n <= 36 then return n else fail "unsupported base greater than 36"
        sign   = option 1 $ char '-' *> return (-1)
        getCol = sourceColumn <$> getPosition
        withLength p = do
          c <- getCol
          p' <- p
          c' <- getCol
          return ((c'-c),p')

-- TODO: ensure doesn't parse chars that don't make sense given @n@.
-- eg: @baseN 10@ on @"12a"@ shouldn't parse the @'a'@.
baseN :: Integer -> Parsec ByteString () Integer
baseN n = many1 alphaNum >>= f 0
  where f acc [] = return acc
        f acc (x:xs)
          | dec < 10 = g dec
          | hxu < 36 = g hxu
          | hxl < 36 = g hxl
          | otherwise = unexpected "non-numeric digit"
          where dec = fromIntegral $ ord x - ord '0'
                hxl = fromIntegral $ ord x - ord 'a' + 10
                hxu = fromIntegral $ ord x - ord 'A' + 10
                g y
                  | y >= n = fail "digit is larger than the base"
                  | otherwise = f ((+) acc . (*) y . (^) n . length $ xs) xs