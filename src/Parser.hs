module Parser (
  JsonValue(JsonInt, JsonString),
  apply,
  jint, jstring, jvalue,
  jlist,
  one,
  getValue
) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (MonadPlus, mplus, mzero, liftM, ap)
import Data.Char (ord, isDigit)

newtype Parser a = Parser (String -> [(a, String)])

getValue [(a, b)] = a
getValue ((a, b):_) = a

parse (Parser p) = p

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

--https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative
instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c, cs)])

p :: Parser (Char, Char)
p = do { c <- item; item; d <- item; return (c, d) }

-- https://stackoverflow.com/questions/36311809/haskell-parser-monad-and-monadplus

instance MonadPlus Parser where
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)
    mzero = Parser (\cs -> [])

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

--instance Show Parser where

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c ==)

anychar2 = do { c <- item; return c }

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do { char c; string cs; return (c:cs) }

-- msum
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
  [] -> []
  (x:cs) -> [x])

mani :: Parser a -> Parser [a]
mani p = many1 p +++ return []

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]

p `sepby1` sep =
  do a <- p
     as <- many (do {sep; p})
     return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                   rest a = (do f <- op
                                b <- p
                                rest (f a b))
                            +++ return a

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

isSpace :: Char -> Bool
isSpace c = c == ' '

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

data JsonValue = JsonInt Int | JsonString [Char] | JsonList [JsonValue] | JsonMap [(JsonValue, JsonValue)]
  deriving (Show, Eq)

anychar = sat (\c -> True)
jstring = do { char '"'; a <- many (sat ('"' /=)); char '"'; return $ JsonString a }

droplast [] = []
droplast [_] = []
droplast (x:xs) = x : droplast xs

jstring2 = do { char '"'; a <- unescape; char '"'; return $ JsonString a } where
  unescape = do
    t <- many $ sat (/= '\"')
    if last t == '\\' then do { rest <- unescape; return $ droplast t ++ rest }
                      else return t

{-
jstring2 = do { char '"'; a <- unescape; char '"'; return $ JsonString a } where

jstring2 = do { char '"'; a <- unescape; char '"'; return $ JsonString a } where
  unescape = do { char '\''; char '"'; return $ item } <|>
    t <- many $ sat (/= '\"')
    if last t == '\\' then do { rest <- unescape; return $ droplast t ++ rest }
                      else return t
-}
--blabla = do { char '\\'; char '"'; blabla; return '"' } <|> do { char '"' } <|> do { i <- item; blabla; return i }
--blabla = do { string "\\\""; blabla; return "\"" } <|> do { string "\"" } <|> do { i <- many item; blabla; return i }
--blabla = do { string "\\\""; blabla; return "\"" } <|> do { s <- many $ sat (/= '\"'); char '"'; return s }

--'' <|> do { char '"'; return mzero } <|> do { return item }

jint = do { int <- many1 (sat isDigit); return $ JsonInt (strToInt int) }

jmap = do
  symb "{";
  a <- sepby jkvp (symb ":")
  symb "}";
  return $ JsonMap a

jkvp = do
  key <- jstring2
  symb ":"
  value <- jvalue
  return (key, value)

jlist = do
  symb "["
  a <- sepby jvalue (symb ",")
  symb "]"
  return $ JsonList a

jvalue = jstring2 <|> jint <|> jmap

charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

strToInt str = strToInt' str 0
strToInt' (l:"") n = n * 10 + (charToInt l)
strToInt' (l:ls) n = strToInt' ls (n * 10 + (charToInt l))
strToInt' "" n = n


expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (sat isDigit); return (ord x - ord '0')}

--Parser (Int -> Int -> Int)
addop = do {symb "+"; return (+)}
mulop = do {symb "*"; return (*)}

one = JsonInt 5

{-
main :: IO ()
main = do
  print $ apply (symb "*") "*"
  print $ apply expr " 1 + 2 * 3"
  print $ apply expr " (1 + 2) * 3"
  print $ apply jstring "\"abc\""
  print $ apply jvalue "123"
  print $ apply jvalue "{\"a\":2}"
  print $ apply jvalue "{\"a\":2,\"b\":3}"
  print $ apply jlist "[1]"
  print $ apply jlist "[1,2]"
  print $ apply jlist "[1,2,\"a\"]"
  print $ apply (string "YOU") "YOU"
  print $ apply jstring "\"abc\\\"bcd\""
  print $ apply blabla "nesamone"
  print $ apply blabla "abc\""
  print $ apply blabla "abc\\\"def\""
  print $ apply jmap "{\"a\":\"b\"}"
  print $ apply jmap "{\"a\":2}"
  print $ apply jmap "{\"a\": 2 }"
  print $ apply jvalue "123"
-}
