module Parser (
  JsonValue(JsonInt, JsonString, JsonList, JsonMap),
  apply,
  jint, jstring, jvalue,
  jlist,
  getValue
) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (MonadPlus, mplus, mzero, liftM, ap)
import Data.Char (ord, isDigit)

data JsonValue = JsonInt Int | JsonString [Char] | JsonList [JsonValue] | JsonMap [(JsonValue, JsonValue)]
  deriving (Show, Eq)

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

anychar = sat (\c -> True)
jstring = do { char '"'; a <- many (sat ('"' /=)); char '"'; return $ JsonString a }

jint = do { int <- many1 (sat isDigit); return $ JsonInt (strToInt int) }

jmap = do
  symb "{";
  a <- sepby jkvp (symb ",")
  symb "}";
  return $ JsonMap a

jkvp = do
  key <- jstring
  symb ":"
  value <- jvalue
  return (key, value)

jlist = do
  symb "["
  a <- sepby jvalue (symb ",")
  symb "]"
  return $ JsonList a

jvalue = jstring <|> jint <|> jmap <|> jlist <|> jmap

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
