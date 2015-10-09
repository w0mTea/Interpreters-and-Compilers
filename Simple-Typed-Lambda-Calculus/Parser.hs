module Parser (lcParse) where

import Context
import Syntax
import Text.Parsec
import Data.Functor.Identity (Identity)
import qualified Text.Parsec.Token as T

type LCParser = ParsecT String Context Identity Term

langDef :: T.LanguageDef st
langDef = T.LanguageDef
        { T.commentStart = "/*"
        , T.commentEnd = "*/"
        , T.commentLine = "//"
        , T.nestedComments = False
        , T.identStart = letter <|> char '_'
        , T.identLetter = alphaNum <|> char '_' <|> char '\''
        , T.opStart = letter
        , T.opLetter = alphaNum <|> oneOf "*#&?^$"
        , T.reservedNames = ["if", "then", "else", "Bool", "True", "False"]
        , T.reservedOpNames = []
        , T.caseSensitive = True
        }

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser langDef

ident :: ParsecT String u Identity String
ident = T.identifier lexer

blank :: ParsecT String u Identity ()
blank = T.whiteSpace lexer

reserved :: String -> ParsecT String u Identity ()
reserved = T.reserved lexer

symbol :: String -> ParsecT String u Identity String
symbol = T.symbol lexer

colon :: ParsecT String u Identity String
colon = T.colon lexer

dot :: ParsecT String u Identity String
dot = T.dot lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = T.parens lexer

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseVar :: LCParser
parseVar = do
    n <- ident
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
    reserved "Bool"
--    blank
    return TyBool

parseType :: Parsec String u TmType
parseType = parseBoolType
-- parseType = msum [parseBoolType]

parseAbs :: LCParser
parseAbs = do
    pos <- getPosition
    _ <- symbol "\\"
    n <- ident
    _ <- colon
--    blank
    ty <- parseType
    _ <- dot
    modifyState ((n, VarBind ty) :)
    t <- parseTerm
    modifyState tail
    return $ TmAbs (infoFrom pos) n ty t

parseIf :: LCParser
parseIf = do
    pos <- getPosition
    _ <- reserved "if"
    c <- parseTerm
    _ <- reserved "then"
    t1 <- parseTerm
    _ <- reserved "else"
    t2 <- parseTerm
    return $ TmIf (infoFrom pos) c t1 t2

parseBool :: LCParser
parseBool = parseTrue <|> parseFalse
    where parseTrue = do {pos <- getPosition; reserved "True"; return $ TmTrue (infoFrom pos)}
          parseFalse = do {pos <- getPosition; reserved "False"; return $ TmFalse (infoFrom pos)}

parseNonApp :: LCParser
parseNonApp =  parens parseTerm
           <|> parseIf
           <|> parseBool
           <|> parseAbs
           <|> try parseVar

parseTerm :: LCParser
parseTerm = chainl1 (blank >> parseNonApp) $ do
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "Untyped Lambda Calculus"

lcParse :: String -> Either ParseError Term
lcParse = parseWith parseTerm
