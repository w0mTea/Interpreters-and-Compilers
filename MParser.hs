{-
module MParser
       ( a
       , b
       , c
       ) where
-}

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser { parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \x -> [(f a, y) | (a, y) <- p x]

instance Applicative Parser where
  pure = return
  cs1 <*> cs2  = Parser $ \s -> [(f a, s2) | (f, s1) <- parse cs1 s, (a, s2) <- parse cs2 s1]

instance Monad Parser where
  return a = Parser $ \x -> [(a, x)]
  m >>= k = Parser $ \x -> [(b, z) | (a, y) <- parse m x, (b, z) <- parse (k a) y]

instance MonadPlus Parser where
  mzero = Parser $ \x -> []
  mplus p1 p2 = Parser $ \x -> parse p1 x ++ parse p2 x

instance Alternative Parser where
  empty = mzero
  p1 <|> p2 = Parser $ \x ->
    case parse p1 x of
    [] -> parse p2 x
    res -> res

  some v = some_v
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v
  many v = many_v
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v


(|>) :: Parser a -> (a -> Bool) -> Parser a
m |> p = m >>= \a -> if p a then return a else mzero

runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(res, [])] -> res
  [(_, rs)] -> error "Parser failed"
  _ -> error "Parser Error"

item :: Parser Char
item = Parser func
  where func [] = []
        func (x:xs) = [(x, xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy = (item |>)

char :: Char -> Parser Char
char c = satisfy (c == )

