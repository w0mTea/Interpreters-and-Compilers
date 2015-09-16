-- File: Lambda.hs
-- Description: An simple lambda calculus interpreter

data Info = Info {line_no :: Int, column_no :: Int} deriving (Show, Eq)

-- Use De Bruijin index to represent lambda calculus
data Term = TmVar Info Int Int -- the second Int is a debug info which stores the current context's length
          | TmAbs Info String Term
          | TmApp Info Term Term
          deriving (Show, Eq)

 -- left for extend
data Binding = EmptyBinding deriving (Show, Eq)

type Context = [(String, Binding)]

{-
LP ::= (
LAMBDA ::= \
ID ::= [a-zA-Z_][a-zA-Z0-9_]*
RP ::= )
-}

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

lexer :: Char -> 

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

ctxLength :: Context -> Int
ctxLength = length

indexToName :: Info -> Context -> Int -> String
indexToName info ctx index = fst $ f (reverse ctx) index
  where f (x:xs) 0 = x
        f (x:xs) n = f xs (n-1)

--}


dummyinfo = Info (-1) (-1)
