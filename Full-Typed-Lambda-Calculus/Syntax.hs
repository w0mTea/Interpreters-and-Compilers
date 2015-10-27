module Syntax where

import Context
import Data.List (isPrefixOf)

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
          | TmUnit Info


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
isVal (TmUnit {}) = True
isVal t = isNv t

isNv :: Term -> Bool
isNv (TmZero {}) = True
isNv (TmSucc _ t) = isNv t
isNv _ = False

-- printing
-- rebuild context and print
printTerm :: Context -> Term -> String
printTerm ctx (TmAbs _ name ty t1) = let (ctx', name') = pickFreshName ctx name in
  "λ" ++ name' ++ ": " ++ show ty ++ "." ++ printTerm ctx' t1
printTerm ctx (TmApp _ t1 t2) =
    let s1 = case t1 of
                (TmAbs {}) -> "(" ++ printTerm ctx t1 ++ ")"
                _ -> printTerm ctx t1
        s2 = case t2 of
                (TmApp {}) -> "(" ++ printTerm ctx t2 ++ ")"
                _ -> printTerm ctx t2
    in s1 ++ " " ++ s2
printTerm ctx (TmVar info x len) = if length ctx == len
                                  then indexToName info ctx x
                                  else "[bad index: " ++ show ctx ++ " len is: " ++ show len ++ "]"
printTerm ctx (TmIf _ c t1 t2) = "if " ++ printTerm ctx c ++ " then " ++ printTerm ctx t1 ++ " else " ++ printTerm ctx t2
printTerm ctx (TmIsZero _ t) = "iszero " ++ printTerm ctx t
printTerm ctx (TmSucc _ t) = "succ " ++ printTerm ctx t
printTerm ctx (TmPred _ t) = "pred " ++ printTerm ctx t
printTerm _ t = show t

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx name = (ctx', name')
  where (nc, ns) = foldr func (0, []) ctx
        name' = newName nc ns
        ctx' = (name', NameBind) : ctx
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
