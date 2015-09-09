{-
  File: Simple1.hs
  Terms:
    t  ::= 0 | succ t | pred t | iszero t | true | false | if t then t else t
    v  ::= true | false | nv
    nv ::= 0 | succ nv
  Evaluation:
    if true then t2 else t3 -> t2
    if false then t2 else t3 -> t3

                      t1 -> t1'
    -----------------------------------------------
    if t1 then t2 else t3 -> if t1' then t2 else t3

                  t1 -> t1'
    ------------------------------------
            succ t1 -> succ t1'

    pred 0 -> 0
    pred (succ nv1) -> nv1

                  t1 -> t1'
    ------------------------------------
            pred t1 -> pred t1'

    iszero 0 -> true
    iszero (succ nv1) -> false

                  t1 -> t1'
    ------------------------------------
          iszero t1 -> iszero t1'

-}

data SourceInfo = SourceInfo {line_no :: Int, column_no :: Int}

data Term = TmTrue SourceInfo
          | TmFalse SourceInfo
          | TmIf SourceInfo Term Term Term
          | TmZero SourceInfo
          | TmSucc SourceInfo Term
          | TmPred SourceInfo Term
          | TmIsZero SourceInfo Term

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t) = isNumericVal t
isNumericVal _ = False
