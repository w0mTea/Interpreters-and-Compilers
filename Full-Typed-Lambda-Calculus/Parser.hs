module Parser (lcParse) where

import Context
import Syntax
import Text.Parsec
import Data.Functor.Identity (Identity)
import Control.Monad (msum)
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
        , T.reservedNames = ["if", "then", "else", "Bool", "True", "False", "succ", "pred", "iszero"]
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
    return TyBool

parseNatType :: Parsec String u TmType
parseNatType = do
    reserved "Nat"
    return TyNat

parseType :: Parsec String u TmType
parseType = parseBoolType <|> parseNatType
-- parseType = msum [parseBoolType]

parseAbs :: LCParser
parseAbs = do
    pos <- getPosition
    _ <- symbol "\\"
    n <- ident
    _ <- colon
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

unitParser :: LCParser
unitParser = msum [parseZero, parseVar, parseBool] -- Parse a single unit such zero and a var

parseZero :: LCParser
parseZero = do
    pos <- getPosition
    _ <- char '0'
    spaces
    return $ TmZero (infoFrom pos)

parseSucc :: LCParser
parseSucc = do
    pos <- getPosition
    reserved "succ"
    t <- unitParser <|> parens parseTerm -- succ unit or succ (...) is legal where unit is a var or 0
    return $ TmSucc (infoFrom pos) t

parsePred :: LCParser
parsePred = do
    pos <- getPosition
    reserved "pred"
    t <- unitParser <|> parens parseTerm -- pred unit or pred (...) is legal where unit is a var or 0
    return $ TmPred (infoFrom pos) t

parseNat :: LCParser
parseNat = parsePred <|> parseSucc <|> parseZero

parseIsZero :: LCParser
parseIsZero = do
    pos <- getPosition
    reserved "iszero"
    t <- unitParser <|> parens parseTerm -- iszero unit or iszero (...) is legal where unit is a var or 0
    return $ TmIsZero (infoFrom pos) t

parseNonApp :: LCParser
parseNonApp =  parens parseTerm
           <|> parseIf
           <|> parseBool
           <|> parseAbs
           <|> parseNat
           <|> parseIsZero
           <|> try parseVar

parseTerm :: LCParser
parseTerm = chainl1 (blank >> parseNonApp) $ do
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "Untyped Lambda Calculus"

lcParse :: String -> Either ParseError Term
lcParse = parseWith (parseTerm <* eof)
