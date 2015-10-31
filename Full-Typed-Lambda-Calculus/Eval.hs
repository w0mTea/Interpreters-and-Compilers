module Eval where
import Syntax
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
        func _ t@(TmZero {}) = t
        func c (TmIsZero info t) = TmIsZero info $ func c t
        func c (TmSucc info t) = TmSucc info $ func c t
        func c (TmPred info t) = TmPred info $ func c t
        func _ t@(TmUnit {}) = t
        func c (TmAscrip info t ty) = TmAscrip info (func c t) ty
        -- Treat let x = t1 in t2 as (\x:T.t2) t1 and then the rule for let is clear
        func c (TmLet info s t1 t2) = TmLet info s (func c t1) $ func (c + 1) t2
        func c (TmTuple info ts) = TmTuple info ts'
            where ts' = map (func c) ts
        func c (TmTupleProj info t n) = TmTupleProj info (func c t) n

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
tmMaintainLength :: Term -> Term
tmMaintainLength = tmMap f
    where f c (TmVar i n _) = TmVar i n c
          f _ _ = error "Error in tmMaintainLength"

tmAppSubst :: Term -> Term -> Term
tmAppSubst term v = tmMaintainLength $ tmShift (-1) $ tmSubst 0 (tmShift 1 v) term

tmAppAbs :: Term -> Term -> Term
tmAppAbs (TmAbs _ _ _ t) = tmAppSubst t

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
eval1 (TmAscrip info t ty) | isVal t = return t -- E-ASCRIBE
                           | otherwise = do {t' <- eval1 t; return $ TmAscrip info t' ty} -- E-ASCRIBE1
eval1 (TmLet info s t1 t2) | isVal t1 = return $ tmAppSubst t2 t1 -- E-LETV
                           | otherwise = do {t1' <- eval1 t1; return $ TmLet info s t1' t2} -- E-LET
eval1 (TmTupleProj info t@(TmTuple _ ts) n)
    | isVal t = return $ ts !! (n - 1)-- E-PROJTUPLE
    | otherwise = do {t' <- eval1 t; return $ TmTupleProj info t' n} -- E-PROJ
eval1 (TmTuple info t) =
    let (vs, ts) = span isVal t
    in case ts of
        [] -> Nothing -- evaluate complete
        (x:xs) -> do {x' <- eval1 x; return $ TmTuple info $ vs ++ [x'] ++ xs} -- E-TUPLE

eval1 _ = Nothing

eval :: Term -> Term
eval t = let t' = eval1 t in
  case t' of
    (Just term) -> eval term
    Nothing -> t
