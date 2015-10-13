module TypeChecker where

import Syntax
import Context

typeOf :: Context -> Term -> Either String TmType
typeOf _ (TmTrue {}) = Right TyBool
typeOf _ (TmFalse {}) = Right TyBool
typeOf ctx (TmIf info c t1 t2) = do
    tyC <- typeOf ctx c
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyC of
        TyBool | tyT1 == tyT2 -> Right tyT1
               | otherwise -> Left $ show info ++ ": Two branches have different types"
        _ -> Left $ show info ++ ": Except Bool type for the condition"
typeOf ctx (TmVar info i _) = getTypeFromContext info ctx i
typeOf ctx (TmAbs _ s ty t) = do
    let ctx' = addBinding ctx s (VarBind ty)
    tyT <- typeOf ctx' t
    Right $ TyArrow ty tyT
typeOf ctx (TmApp info t1 t2) = do
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyT1 of
        (TyArrow tyT11 tyT12) | tyT11 == tyT2 -> Right tyT12
                              | otherwise     -> Left $ show info ++ ": Parameter type mismatch"
        _ -> Left $ show info ++ ": Arrow type expected"
