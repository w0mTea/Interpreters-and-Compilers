import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)
import System.IO
import System.Environment
import Data.List (isPrefixOf)

data Info = Info {row :: Int, col :: Int}
          deriving Show
type LCParser = Parsec String Context Term

-- Use De Bruijin index to represent lambda calculus
data Term = TmVar Info Int Int -- the second Int is a debug info which stores the current context's length
          | TmAbs Info String Term
          | TmApp Info Term Term
          deriving Show

 -- left for extend
data Binding = EmptyBinding deriving (Show, Eq)

type Context = [(String, Binding)]


-- { Parse functions
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "Untyped Lambda Calculus"

name :: Parsec String u String
name = do
  let firstLetters = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['_']
      identLetters = firstLetters ++ ['0' .. '9']
  spaces
  fst <- oneOf firstLetters
  rest <- many $ oneOf identLetters
  tail <- many $ char '\''
  spaces
  return $ (fst : rest) ++ tail

parseAbs :: LCParser
parseAbs = do
  spaces
  pos <- getPosition
  char '\\'
  n <- name
  modifyState ((n, EmptyBinding) :)
  char '.'
  t <- parseTerm
  modifyState tail
  return $ TmAbs (infoFrom pos) n t

parseVar :: LCParser
parseVar = do
  n <- name
  ctx <- getState
  findVar n ctx

findVar :: String -> Context -> LCParser
findVar v ctx = case indexInCtx v ctx of
  Nothing -> (fail $ "Variable " ++ v ++ " cannot be found") <?> "a bouned variable"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length ctx)

indexInCtx :: String -> Context -> Maybe Int
indexInCtx name [] = Nothing
indexInCtx name ctx = func name ctx 0
  where func n [] i = Nothing
        func n ((n', _) : xs) i | n == n' = Just i
                                | otherwise = func n xs (i+1)

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

parseNonApp :: LCParser
parseNonApp =  parens parseTerm
      <|> parseAbs
      <|> parseVar


parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
  spaces
  pos <- getPosition
  return $ TmApp (infoFrom pos)

lcParse :: String -> Either ParseError Term
lcParse = parseWith parseTerm

--}

--{ Core functions

tmMap :: (Int -> Term -> Term) -> Term -> Term
tmMap f t = func 0 t
  where func c t1@(TmVar _ _ _) = f c t1
        func c (TmAbs info name t1) = TmAbs info name $ func (c + 1) t1
        func c (TmApp info t1 t2) = TmApp info (func c t1) (func c t2)

tmShift :: Int -> Term -> Term
tmShift step = tmMap f
  where f c (TmVar info n len) = TmVar info (n + step) (len + step)
        f _ _ = error "Error in tmShift"

tmSubst :: Int -> Term -> Term -> Term
tmSubst j s = tmMap f
  where f c t@(TmVar _ n _) | n == c + j = tmShift c s
                            | otherwise = t
        f _ _ = error "Error in tmSubst"

tmAppAbs :: Term -> Term -> Term
tmAppAbs t1 t2 = tmShift (-1) $ tmSubst 0 (tmShift 1 t1) t2

--}

--{ Evaluatino functions

isVal :: Term -> Bool
isVal (TmAbs _ _ _) = True
isVal _ = False

eval1 :: Term -> Maybe Term
eval1 (TmApp info t@(TmAbs _ _ _) v2)
  | isVal v2 = return $ tmAppAbs t v2
  | otherwise = let v2' = eval1 v2 in
    v2' >>= return . TmApp info t
eval1 (TmApp info t1 t2) = let t1' = eval1 t1 in
  t1' >>= return . flip (TmApp info) t2
eval1 _ = Nothing

eval :: Term -> Term
eval t = let t' = eval1 t in
  case t' of
    (Just term) -> eval term
    Nothing -> t

--}

--{ Printing functions

-- rebuild context and print
printTerm :: Context -> Term -> String
printTerm ctx (TmAbs _ name t1) = let (ctx', name') = pickFreshName ctx name in
  "(λ" ++ name' ++ ". " ++ printTerm ctx' t1 ++ ")"
printTerm ctx (TmApp _ t1 t2) = "(" ++ printTerm ctx t1 ++ printTerm ctx t2 ++ ")"
printTerm ctx (TmVar info x len) = if length ctx == len
                                  then indexToName info ctx x
                                  else "[bad index]"

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx name = (ctx', name')
  where (nc, ns) = foldr func (0, []) ctx
        name' = newName nc ns
        ctx' = (name, EmptyBinding) : ctx
        func (n, _) (c, lst) = if n == name
                               then (c+1, lst)
                               else if name `isPrefixOf` n
                                    then (c, n : lst)
                                    else (c, lst)
        newName c ns = let n' = name ++ (show c) in
          if n' `elem` ns then newName (c+1) ns else n'
    
indexToName :: Info -> Context -> Int -> String
indexToName info ctx index = fst $ f ctx index
  where f (x:xs) 0 = x
        f (x:xs) n = f xs (n-1)

--}
