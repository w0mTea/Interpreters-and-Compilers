module Parser where

import Context
import Syntax
import Text.Parsec
import Text.Parsec.Combinator (chainl1)

type LCParser = Parsec String Context Term

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

name :: Parsec String u String
name = do
    let fLetters = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_"
        iLetters = fLetters ++ ['0' .. '9']
    f <- oneOf fLetters
    r <- many $ oneOf iLetters
    t <- many $ char '\''
    spaces
    return $ (f : r) ++ t

parseVar :: LCParser
parseVar = do
    n <- name
    ctx <- getState
    findVar n ctx

findVar :: String -> Context -> LCParser
findVar v ctx = case indexInCtx v ctx of
    Nothing -> fail ("Variable \"" ++ v ++ "\" cannot be found") <?> "a bouned variable"
    Just n  -> do
        pos <- getPosition
        return $ TmVar (infoFrom pos) n (length ctx)

indexInCtx :: String -> Context -> Maybe Int
indexInCtx _ [] = Nothing
indexInCtx n ctx = func n ctx 0
  where func _ [] _ = Nothing
        func s ((s', _) : xs) i | s == s' = Just i
                                | otherwise = func s xs (i+1)

parseBoolType :: Parsec String u TmType
parseBoolType = do
    _ <- string "Bool"
    spaces
    return TyBool

parseType :: Parsec String u TmType
parseType = parseBoolType
-- parseType = msum [parseBoolType]

parseAbs :: LCParser
parseAbs = do
    pos <- getPosition
    _ <- char '\\'
    n <- name
    _ <- char ':'
    spaces
    ty <- parseType
    _ <- char '.'
    modifyState ((n, VarBind ty) :)
    t <- parseTerm
    modifyState tail
    return $ TmAbs (infoFrom pos) n ty t

parseIf :: LCParser
parseIf = do
    pos <- getPosition
    _ <- string "if"
    c <- parseTerm
    _ <- string "then"
    t1 <- parseTerm
    _ <- string "else"
    t2 <- parseTerm
    return $ TmIf (infoFrom pos) c t1 t2

parseBool :: LCParser
parseBool = do
    pos <- getPosition
    s <- string "True" <|> string "False"
    spaces
    if s == "True"
    then return $ TmTrue (infoFrom pos)
    else return $ TmFalse (infoFrom pos)

parens :: Parsec String u a -> Parsec String u a
parens p = do
    _ <- char '('
    t <- p
    _ <- char ')'
    spaces
    return t

parseNonApp :: LCParser
parseNonApp =  parens parseTerm
           <|> parseIf
           <|> parseBool
           <|> parseAbs
           <|> try parseVar

parseTerm :: LCParser
parseTerm = chainl1 (spaces >> parseNonApp) $ do
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "Untyped Lambda Calculus"

lcParse :: String -> Either ParseError Term
lcParse = parseWith parseTerm
