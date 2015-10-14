module Syntax where

import Context

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmVar Info Int Int -- the second Int means the length of context
          | TmAbs Info String TmType Term
          | TmApp Info Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term


instance Show Term where
    show = pprint 0

indentBy :: Int -> String
indentBy = flip replicate ' '

pprint :: Int -> Term -> String
pprint i (TmTrue {}) = indentBy i ++ "True"
pprint i (TmFalse {}) = indentBy i ++ "False"
pprint i (TmIf _ c t1 t2) = indentBy i ++ "if\n" ++ pprint (i + 2) c ++ "\nthen\n" ++ pprint (i + 2) t1 ++ "\nelse\n" ++ pprint (i + 2) t2
pprint i (TmVar _ n l) = indentBy i ++ "(TmVar " ++ show n ++ " " ++ show l ++ ")"
pprint i (TmAbs _ n _ t) = indentBy i ++ "(TmAbs " ++ n ++ "\n" ++ pprint (i + 2) t ++ ")"
pprint i (TmApp _ t1 t2) = indentBy i ++ "(TmApp\n" ++ pprint (i + 2) t1 ++ "\n" ++ pprint (i + 2) t2 ++ ")"
pprint i (TmZero _) = indentBy i ++ "0"
pprint i (TmSucc _ t) = indentBy i ++ "succ " ++ show t
pprint i (TmPred _ t) = indentBy i ++ "pred " ++ show t
pprint i (TmIsZero _ t) = indentBy i ++ "iszero " ++ show t

isVal :: Term -> Bool
isVal (TmAbs {}) = True
isVal (TmTrue {}) = True
isVal (TmFalse {}) = True
isVal t = isNv t

isNv :: Term -> Bool
isNv (TmZero {}) = True
isNv (TmSucc _ t) = isNv t
isNv _ = False
