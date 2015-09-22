module MParser
       ( a
       , b
       , c
       ) where


newtype Parser a = Parser { parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser p) x = [(f a, y) | (a, y) <- p x]

instance Applicative Parser where
  pure = return
  (cs1 <*> cs2) s = [(f a, s2) | (f, s1) <- parse cs1 s, (a, s2) <- parse cs2 s1]

instance Monad Parser where
  return a x = [(a, x)]
  (m >>= k) x = [(b, z) | (a, y) <- m x, (b, z) <- k a y]

instance MonadPlus Parser where
  mzero x = []
  mplus p1 p2 x = parse p1 x ++ parse p2 x

instance Alternative Parser where
  empty = mzero
  (p1 <|> p2) x = if r \= [] then r else parse p2 x
    where r = parse p1 x
  some = some_v
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v
  many = many_v
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v


(|>) :: M a -> (a -> Bool) -> M a
m |> p = m >>= \a -> if p a then return a else mzero

runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(res, [])] -> res
  [(_, rs)] -> error "Parser failed"
  _ -> error "Parser Error"

item :: Parser Char
item [] = []
item [a:x] = [(a, x)]


