import Text.Parsec
import Text.Parsec.Combinator (between, chainl1)
import Data.List (isPrefixOf)
import System.IO
import System.Environment
import Control.Monad (liftM)

data Info = Info {row :: Int, col :: Int}
          deriving Show
type LCParser = Parsec String Context Term

-- Use De Bruijin index to represent lambda calculus
data Term = TmVar Info Int Int -- the second Int is a debug info which stores the current context's length
          | TmAbs Info String Term
          | TmApp Info Term Term

 -- left for extend
data Binding = EmptyBinding deriving (Show, Eq)

type Context = [(String, Binding)]

-- pretty print function
instance Show Term where
    show = pprint 0

indentBy :: Int -> String
indentBy = flip replicate ' '

pprint :: Int -> Term -> String
pprint i (TmVar _ n l) = indentBy i ++ "(TmVar " ++ show n ++ " " ++ show l ++ ")"
pprint i (TmAbs _ n t) = indentBy i ++ "(TmAbs " ++ n ++ "\n" ++ pprint (i + 2) t ++ ")"
pprint i (TmApp _ t1 t2) = indentBy i ++ "(TmApp\n" ++ pprint (i + 2) t1 ++ "\n" ++ pprint (i + 2) t2 ++ ")"

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
  firstL <- oneOf firstLetters
  restL <- many $ oneOf identLetters
  tailL <- many $ char '\''
  spaces
  return $ (firstL : restL) ++ tailL

parseAbs :: LCParser
parseAbs = do
  spaces
  pos <- getPosition
  _ <- char '\\'
  n <- name
  modifyState ((n, EmptyBinding) :)
  _ <- char '.'
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
  Nothing -> fail ("Variable \"" ++ v ++ "\" cannot be found") <?> "a bouned variable"
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
parens p = do
    spaces
    _ <- char '('
    t <- p
    _ <- char ')'
    spaces
    return t

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
-- apply a function on Each subterm of the given term
tmMap :: (Int -> Term -> Term) -> Term -> Term
tmMap f = func 0
  where func c t1@(TmVar {}) = f c t1
        func c (TmAbs info name t1) = TmAbs info name $ func (c + 1) t1
        func c (TmApp info t1 t2) = TmApp info (func c t1) (func c t2)

tmShift :: Int -> Term -> Term
tmShift step = tmMap f
  where f c t@(TmVar info n len)
            | n < c = t
            | otherwise = TmVar info (n + step) len
        f _ _ = error "Error in tmShift"

tmSubst :: Int -> Term -> Term -> Term
tmSubst j s = tmMap f
  where f c t@(TmVar _ n _) | n == c + j = tmShift c s
                            | otherwise = t
        f _ _ = error "Error in tmSubst"

tmAppAbs :: Term -> Term -> Term
tmAppAbs (TmAbs _ _ t) v = tmShift (-1) $ tmSubst 0 (tmShift 1 v) t

--}

--{ Evaluatino functions

isVal :: Term -> Bool
isVal (TmAbs {}) = True
isVal _ = False

eval1 :: Term -> Maybe Term
eval1 (TmApp info t@(TmAbs {}) v2)
  | isVal v2 = return $ tmAppAbs t v2
  | otherwise = let v2' = eval1 v2 in
    liftM (TmApp info t) v2'
eval1 (TmApp info t1 t2) = let t1' = eval1 t1 in
  liftM (flip (TmApp info) t2) t1'
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
  "(λ" ++ name' ++ "." ++ printTerm ctx' t1 ++ ")"
printTerm ctx (TmApp _ t1 t2) = "(" ++ printTerm ctx t1 ++ " " ++ printTerm ctx t2 ++ ")"
printTerm ctx (TmVar info x len) = if length ctx == len
                                  then indexToName info ctx x
                                  else "[bad index: " ++ show ctx ++ " len is: " ++ show len ++ "]"

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx name = (ctx', name')
  where (nc, ns) = foldr func (0, []) ctx
        name' = newName nc ns
        ctx' = (name, EmptyBinding) : ctx
        func (n, _) (c, lst)
            | n == name = (c + 1, lst)
            | name `isPrefixOf` n = (c, n : lst)
            | otherwise = (c, lst)
        newName c ns' = let n' = if c > 0 then name ++ show c else name in
          if n' `elem` ns' then newName (c+1) ns' else n'

indexToName :: Info -> Context -> Int -> String
indexToName _ ctx index = fst $ f ctx index
  where f (x:_) 0 = x
        f (x:xs) n = f xs (n-1)

--}

main :: IO [()]
main = do
  args <- getArgs
  mapM runFile args

runFile :: String -> IO ()
runFile path = do
  s <- readFile path
  let term = lcParse s
  case term of
    Left e -> print e
    Right t -> evalTerm t

evalTerm :: Term -> IO ()
evalTerm t = let t' = eval t in
  if isVal t'
  then putStrLn $ printTerm [] t'
  else putStrLn "Evaluating Error"
