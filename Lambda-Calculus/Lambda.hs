-- File: Lambda.hs
-- Description: An simple lambda calculus interpreter

import MParser
import System.IO
import System.Environment

data Info = Info {line_no :: Int, column_no :: Int} deriving (Show, Eq)

-- Use De Bruijin index to represent lambda calculus
data Term = TmVar Info Int Int -- the second Int is a debug info which stores the current context's length
          | TmAbs Info String Term
          | TmApp Info Term Term
          deriving (Show, Eq)

 -- left for extend
data Binding = EmptyBinding deriving (Show, Eq)

type Context = [(String, Binding)]

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

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp info t@(TmAbs _ _ _) v2) | isVal v2 = return $ tmAppAbs t v2
                                          | otherwise = let v2' = eval1 ctx v2 in
                                            v2' >>= return . TmApp info t
eval1 ctx (TmApp info t1 t2) = let t1' = eval1 ctx t1 in
  t1' >>= return . flip (TmApp info) t2
eval1 _ _ = Nothing

eval :: Context -> Term -> Term
eval ctx t = let t' = eval1 ctx t in
  case t' of
  (Just term) -> eval ctx term
  Nothing -> t

--}

--{ Parse functions

{-
 term = var | abs | app
 var ::= [A-Za-z_][_A-Za-z0-9]*
 abs ::= \var.term
 app ::= (term) term | (term) (term) | var (term) | var var
-}

data PTerm = PVar String
           | PAbs String PTerm
           | PApp PTerm PTerm
           deriving Show

name :: Parser String
name = oneOf n >>= \x -> fmap ([x] ++) $ many $ oneOf n'
  where n = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
        n' = n ++ ['0' .. '9']

term :: Parser PTerm
term = pApp1 <|> pApp2 <|> pApp3 <|> pApp4 <|> pVar <|> pAbs
  where pVar = do
          spaces
          n <- name
          return  $ PVar n
        pAbs = do
          spaces
          char '\\'
          spaces
          n <- name
          spaces
          char '.'
          spaces
          t <- term
          return $ PAbs n t
        pApp1 = do
          spaces
          t1 <- parens term
          spaces
          t2 <- term
          return $ PApp t1 t2
        pApp2 = do
          spaces
          t1 <- parens term
          spaces
          t2 <- parens term
          return $ PApp t1 t2
        pApp3 = do
          spaces
          var <- pVar
          spaces
          t <- parens term
          return $ PApp var t
        pApp4 = do
          spaces
          v1 <- pVar
          spaces
          v2 <- pVar
          return $ PApp v1 v2
          
--}

--{ Convert a lambda calculus into the nameless form

removeName :: Context -> PTerm -> (Term, Context)
removeName ctx (PVar name) = let index = indexOfName name ctx
                                 len = length ctx
                             in case index of
                                  (Just index') -> (TmVar dummyinfo index' len, ctx)
                                  Nothing -> error $ "\"" ++ name ++ "\" is a free variable"
removeName ctx (PAbs name t) = let (t', ctx') = removeName (ctx ++ [(name, EmptyBinding)]) t
                               in (TmAbs dummyinfo name t', ctx')
removeName ctx (PApp pt1 pt2) = let (t1, ctx1) = removeName ctx pt1
                                    (t2, ctx2) = removeName ctx pt2
                                in if length ctx1 > length ctx2 -- in this situation, choose a longger ctx
                                   then (TmApp dummyinfo t1 t2, ctx1)
                                   else (TmApp dummyinfo t1 t2, ctx2)

--}

--{ Printing functions

printTerm :: Context -> Term -> String
printTerm ctx (TmAbs _ name t1) = let (ctx', name') = pickFreshName ctx name in
  "(λ" ++ name' ++ ". " ++ printTerm ctx' t1 ++ ")"
printTerm ctx (TmApp _ t1 t2) = "(" ++ printTerm ctx t1 ++ printTerm ctx t2 ++ ")"
printTerm ctx (TmVar info x len) = if ctxLength ctx == len
                                  then indexToName info ctx x
                                  else "[bad index]"

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx name = (ctx', name')
  where n = foldr (\ (c, _) count -> if c == name then count + 1 else count) 0 ctx
        name' = if n > 1 then name ++ show (n - 1) else name
        func pre (c:cs) | fst c == name = pre ++ [(name', snd c)] ++ cs
        ctx' = reverse $ func [] ctx

indexToName :: Info -> Context -> Int -> String
indexToName info ctx index = fst $ f (reverse ctx) index
  where f (x:xs) 0 = x
        f (x:xs) n = f xs (n-1)

--}

--{Auxilary functions

ctxLength :: Context -> Int
ctxLength = length

indexOfName :: String -> Context -> Maybe Int
indexOfName name ctx = func name 0 $ reverse ctx
  where func n i ((s, _):cs) = if n == s then Just i else func n (i+1) cs
        func _ _ _ = Nothing
--}

dummyinfo = Info (-1) (-1)

lambdaParser :: String -> String
lambdaParser s = let l = parse term s in
  case l of
    [] -> "Empty file"
    [(t, "")] -> interprete t
    _ -> "Parse error"

interprete :: PTerm -> String
interprete t = let (t', ctx) = removeName [] t
                   t'' = eval ctx t' in
               printTerm ctx t''


main = do
  lst <- getArgs
  case lst of
    [] -> putStrLn "A input file is needed"
    xs -> do
      sequence $ map parseFile xs
      return ()

parseFile :: String -> IO ()
parseFile filename = do
  s <- readFile filename
  putStrLn $ lambdaParser s
