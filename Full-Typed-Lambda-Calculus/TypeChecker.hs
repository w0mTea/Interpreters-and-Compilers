module TypeChecker where

import Syntax
import Context

typeOf :: Context -> Term -> Either String TmType
typeOf _ (TmTrue {}) = Right TyBool -- T-TRUE
typeOf _ (TmFalse {}) = Right TyBool -- T-FALSE
typeOf ctx (TmIf info c t1 t2) = do -- T-IF
    tyC <- typeOf ctx c
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyC of
        TyBool | tyT1 == tyT2 -> Right tyT1
               | otherwise -> Left $ show info ++ ": Two branches have different types"
        _ -> Left $ show info ++ ": Except Bool type for the condition"
typeOf ctx (TmVar info i _) = getTypeFromContext info ctx i -- T-VAR
typeOf ctx (TmAbs _ s ty t) = do -- T-ABS
    let ctx' = addBinding ctx s (VarBind ty)
    tyT <- typeOf ctx' t
    Right $ TyArrow ty tyT
typeOf ctx (TmApp info t1 t2) = do -- T-APP
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyT1 of
        (TyArrow tyT11 tyT12) | tyT11 == tyT2 -> Right tyT12
                              | otherwise     -> Left $ show info ++ ": Parameter type mismatch"
        _ -> Left $ show info ++ ": Arrow type expected"
typeOf _ (TmZero {}) = Right TyNat -- T-ZERO
typeOf ctx (TmSucc info t) = let ty = typeOf ctx t in -- T-SUCC
    case ty of
        Right TyNat -> ty
        _ -> Left $ show info ++ ": Except a nature number as succ's parameter"
typeOf ctx (TmPred info t) = let ty = typeOf ctx t in -- T-PRED
    case ty of
        Right TyNat -> ty
        _ -> Left $ show info ++ ": Except a nature number as pred's parameter"
typeOf ctx (TmIsZero info t) = let ty = typeOf ctx t in -- T-ISZERO
    case ty of
        Right TyNat -> Right TyBool
        _ -> Left $ show info ++ ": Except a nature number as iszero's parameter"
