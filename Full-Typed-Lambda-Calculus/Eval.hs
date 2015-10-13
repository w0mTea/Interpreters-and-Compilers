module Eval where
import Syntax
import Context
import Data.List (isPrefixOf)
import Control.Monad (liftM)

-- apply a function on Each subterm of the given term
tmMap :: (Int -> Term -> Term) -> Term -> Term
tmMap f = func 0
  where func c t1@(TmVar {}) = f c t1
        func c (TmAbs info name ty t1) = TmAbs info name ty $ func (c + 1) t1
        func c (TmApp info t1 t2) = TmApp info (func c t1) (func c t2)
        func _ t@(TmTrue {}) = t
        func _ t@(TmFalse {}) = t
        func c (TmIf info t0 t1 t2) = TmIf info (func c t0) (func c t1) (func c t2)

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

-- E-APPABS
tmAppAbs :: Term -> Term -> Term
tmAppAbs (TmAbs _ _ _ t) v = tmShift (-1) $ tmSubst 0 (tmShift 1 v) t

eval1 :: Term -> Maybe Term
eval1 (TmApp info t@(TmAbs {}) v2)
    | isVal v2 = return $ tmAppAbs t v2
    | otherwise = let v2' = eval1 v2 in -- E-APP2
        liftM (TmApp info t) v2'
eval1 (TmApp info t1 t2) = let t1' = eval1 t1 in -- E-APP1
    liftM (flip (TmApp info) t2) t1'
eval1 (TmIf _ (TmTrue _) t1 _) = return t1 -- E-IFTRUE
eval1 (TmIf _ (TmFalse _) _ t2) = return t2 -- E-IFFALSE
eval1 (TmIf info c t1 t2) = do -- E-IF
    c' <- eval1 c
    return $ TmIf info c' t1 t2
eval1 (TmSucc info t) = do -- E-SUCC
    t' <- eval1 t
    return $ TmSucc info t'
eval1 (TmPred _ z@(TmZero {})) = return z -- E-PREDZERO
eval1 (TmPred info t) | not $ isVal t = do {t' <- eval1 t; return $ TmPred info t'} -- E-PRED
eval1 (TmPred _ (TmSucc _ nv)) = return nv -- E-PREDSUCC
eval1 (TmIsZero info (TmZero {})) = return $ TmTrue info -- E-ISZEROZERO
eval1 (TmIsZero info t) | not $ isVal t = do {t' <- eval1 t; return $ TmIsZero info t'} -- E-ISZERO
eval1 (TmIsZero info (TmSucc {})) = return $ TmFalse info -- E-ISZEROSUCC
eval1 _ = Nothing

eval :: Term -> Term
eval t = let t' = eval1 t in
  case t' of
    (Just term) -> eval term
    Nothing -> t

-- printing
-- rebuild context and print
printTerm :: Context -> Term -> String
printTerm ctx (TmAbs _ name ty t1) = let (ctx', name') = pickFreshName ctx name in
  "(λ" ++ name' ++ ": " ++ show ty ++ "." ++ printTerm ctx' t1 ++ ")"
printTerm ctx (TmApp _ t1 t2) = "(" ++ printTerm ctx t1 ++ " " ++ printTerm ctx t2 ++ ")"
printTerm ctx (TmVar info x len) = if length ctx == len
                                  then indexToName info ctx x
                                  else "[bad index: " ++ show ctx ++ " len is: " ++ show len ++ "]"
printTerm _ t = show t

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx name = (ctx', name')
  where (nc, ns) = foldr func (0, []) ctx
        name' = newName nc ns
        ctx' = (name, NameBind) : ctx
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
