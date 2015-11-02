module TypeChecker (typeOf) where

import Syntax
import Context
import Data.List (partition)
import Control.Arrow (second)

typeOf :: Context -> Term -> Either String TmType
typeOf _ (TmTrue {}) = Right TyBool -- T-TRUE
typeOf _ (TmFalse {}) = Right TyBool -- T-FALSE
typeOf ctx (TmIf info c t1 t2) = do -- T-IF
    tyC <- typeOf ctx c
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyC of
        TyBool | tyT1 == tyT2 -> Right tyT1
               | otherwise -> Left $ show info ++ ":" ++
                              "\n    Two branches are expected to have a same type." ++
                              "\n    " ++ printTerm ctx t1 ++ " : " ++ show tyT1 ++
                              "\n    " ++ printTerm ctx t2 ++ " : " ++ show tyT2
        _ -> Left $ show info ++ ":" ++
                    "\n    Expect type 'Bool' as if expression's condition" ++
                    "\n    " ++ printTerm ctx c ++ " : " ++ show tyC
typeOf ctx (TmVar info i _) = getTypeFromContext info ctx i -- T-VAR
typeOf ctx (TmAbs _ s ty t) = do -- T-ABS
    let ctx' = addBinding ctx s (VarBind ty)
    tyT <- typeOf ctx' t
    Right $ TyArrow ty tyT
typeOf ctx (TmApp info t1 t2) = do -- T-APP
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyT1 of
        (TyArrow tyT11 tyT12) | tyT11 === tyT2 -> Right tyT12 -- Notice: t1:Unit->t t2:t1 is allowed so here use === instead of ==
                              | otherwise      -> Left $ show info ++ ":" ++
                                                  "\n    Expect type '" ++ show tyT11 ++ "' but the actual type is '" ++ show tyT2 ++ "'" ++
                                                  "\n    " ++ printTerm ctx t1 ++ " : " ++ show tyT1 ++
                                                  "\n    " ++ printTerm ctx t2 ++ " : " ++ show tyT2
        _ -> Left $ show info ++ ":" ++
                    "\n    Arrow type expected" ++
                    "\n    " ++ printTerm ctx t1 ++ " : " ++ show tyT1
typeOf _ (TmZero {}) = Right TyNat -- T-ZERO
typeOf ctx (TmSucc info t) = do -- T-SUCC
    ty <- typeOf ctx t
    case ty of
        TyNat -> Right ty
        _ -> Left $ show info ++ ":" ++
                    "\n    Expect a nature number as succ's parameter" ++
                    "\n    " ++ printTerm ctx t ++ " : " ++ show ty
typeOf ctx (TmPred info t) = do -- T-PRED
    ty <- typeOf ctx t
    case ty of
        TyNat -> Right ty
        _ -> Left $ show info ++ ":" ++
                    "\n    Except a nature number as pred's parameter" ++
                    "\n    " ++ printTerm ctx t ++ " : " ++ show ty
typeOf ctx (TmIsZero info t) = do -- T-ISZERO
    ty <- typeOf ctx t
    case ty of
        TyNat -> Right TyBool
        _ -> Left $ show info ++ ":" ++
                    "\n    Except a nature number as iszero's parameter" ++
                    "\n    " ++ printTerm ctx t ++ " : " ++ show ty
typeOf _ (TmUnit {}) = return TyUnit
typeOf ctx (TmAscrip info t ty) = do -- T-ASCRIBE
    ty' <- typeOf ctx t
    if ty == ty'
    then Right ty
    else Left $ show info ++ ":" ++
                "\n    Type dismatch in as-expression" ++
                "\n    " ++ printTerm ctx t ++ " : " ++ show ty' ++
                "\n    but the type given in as-expression is " ++ show ty
typeOf ctx (TmLet _ s t1 t2) = do -- T-LET
    ty1 <- typeOf ctx t1
    let ctx' = addBinding ctx s (VarBind ty1)
    ty2 <- typeOf ctx' t2
    Right ty2
typeOf ctx (TmTuple _ ts) = do -- T-TUPLE
    tys <- mapM (typeOf ctx) ts
    return $ TyTuple tys $ length tys
typeOf ctx (TmTupleProj info t n) = do -- T-PROJTUPLE
    ty <- typeOf ctx t
    case ty of
        TyTuple tys len
            | n <= len && n > 0 -> Right $ tys !! (n - 1)
            | otherwise -> Left $ show info ++ ":" ++
                                  "\n    Invalid index of tuple" ++
                                  "\n    The index " ++ show n ++ " is not valid"
        _ -> Left $ show info ++ ":" ++
                    "\n    Projection on a non-tuple term" ++
                    "\n    Except a tuple but given " ++ show ty
typeOf ctx (TmRecord _ ts) = -- T-RECORD
    let mapUntilEither = mapUntilEither' []
        mapUntilEither' r _ [] = ([], r)
        mapUntilEither' r f (x:xs) = let (l, t') = f x
                                     in case t' of
                                         Left s -> ([(l, s)], r)
                                         Right t -> mapUntilEither' (r ++ [(l, t)]) f xs
        (e, ts') = mapUntilEither (second $ typeOf ctx) ts
    in case e of
       [] -> Right $ TyRecord ts'
       (x : _) -> Left $ snd x
typeOf ctx (TmRecordProj info t l) = do -- T-PROJRCD
    ty <- typeOf ctx t
    case ty of
        TyRecord ts -> let ty' = lookup l ts
                      in case ty' of
                          Just ty'' -> Right ty''
                          Nothing   -> Left $ show info ++ ":" ++
                                              "\n    The key \"" ++ l ++ "\" doesn't exist in the record"
        _ -> Left $ show info ++ ":" ++
                    "\n    Projection on a non-record term" ++
                    "\n    Except a record but given " ++ show ty
