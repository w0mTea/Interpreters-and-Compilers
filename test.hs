import MParser

{-
term = var | abs | app
var ::= [A-Za-z_][_A-Za-z0-9]*
abs ::= \var.term
app ::= (term) term | (term) (term)
-}

data Term = Var String
          | Abs String Term
          | App Term Term
          deriving Show

name :: Parser String
name = oneOf n >>= \x -> fmap ([x] ++) $ many $ oneOf n'
  where n = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
        n' = n ++ ['0' .. '9']

term :: Parser Term
term = pVar `mplus` pAbs `mplus` pApp1 `mplus` pApp2
  where pVar = do
          n <- name
          return  $ Var n
        pAbs = do
          char '\\'
          spaces
          n <- name
          spaces
          char '.'
          spaces
          t <- term
          return $ Abs n t
        pApp1 = do
          t1 <- parens term
          spaces
          t2 <- term
          return $ App t1 t2
        pApp2 = do
          t1 <- parens term
          spaces
          t2 <- parens term
          return $ App t1 t2
