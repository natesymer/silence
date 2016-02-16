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
parseFelony = either (error . show) id . parse code ""

code :: Parsec ByteString () [Expression]
code = manyTill (skipw *> expr <* skipw) eof
  where skipw = try $ skipMany $ void whitespace <|> void comment
      
expr :: Parsec ByteString () Expression
expr = choice [equoted,elist,enumber,eatom,estring,ebool]

whitespace :: Parsec ByteString () Char
whitespace = space <|> tab <|> newline <|> crlf <?> "whitespace"

comment :: Parsec ByteString () String
comment = char ';' *> (manyTill anyToken $ (void $ newline <|> crlf) <|> eof) <?> "comment"

eatom :: Parsec ByteString () Expression
eatom = fmap (Atom . B.pack . map toLower) ident <?> "atom"
  where ident = ((:) <$> initial <*> many subseq) <|> (pure <$> exc)
        initial = letter <|> symbol
        subseq = initial <|> digit <|> exc
        exc = oneOf ".+-"
        symbol = oneOf "?!$%&*/:<=>~_^"

ebool :: Parsec ByteString () Expression
ebool = true <|> false <?> "bool"
  where true  = try $ string "#t" *> return LispTrue
        false = try $ string "#f" *> return LispFalse

estring :: Parsec ByteString () Expression
estring = String . B.pack <$> (q *> many accepted <* q) <?> "string"
  where q = char '"'; bs = char (chr 92)
        accepted = (bs *> (q <|> bs)) <|> satisfy ((/=) '"')

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