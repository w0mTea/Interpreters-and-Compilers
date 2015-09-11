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

data SourceInfo = SourceInfo {line_no :: Int, column_no :: Int} deriving (Eq, Show)

data Term = TmTrue SourceInfo
          | TmFalse SourceInfo
          | TmIf SourceInfo Term Term Term
          | TmZero SourceInfo
          | TmSucc SourceInfo Term
          | TmPred SourceInfo Term
          | TmIsZero SourceInfo Term
          deriving (Eq, Show)

dummyinfo = SourceInfo (-1) (-1)

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t = isNumericVal t

oneStepEval :: Term -> Maybe Term
oneStepEval (TmIf _ (TmTrue _) t _) = return t
oneStepEval (TmIf _ (TmFalse _) _ f) = return f
oneStepEval (TmIf info c t f) = c' >>= (\x -> return $ TmIf info x t f)
  where c' = oneStepEval c
oneStepEval (TmSucc info t) = t' >>= return . TmSucc info
  where t' = oneStepEval t
oneStepEval (TmPred _ (TmZero _)) = return $ TmZero dummyinfo
oneStepEval (TmPred _ (TmSucc _ nv)) | isNumericVal nv = return nv
oneStepEval (TmPred info t) = t' >>= return . TmPred info
  where t' = oneStepEval t
oneStepEval (TmIsZero _ (TmZero _)) = return $ TmTrue dummyinfo
oneStepEval (TmIsZero _ (TmSucc _ nv)) | isNumericVal nv = return $ TmFalse dummyinfo
oneStepEval (TmIsZero info t) = t' >>= return . TmIsZero info
  where t' = oneStepEval t
oneStepEval _ = Nothing

eval :: Term -> Term
eval t = let t' = oneStepEval t
         in case t' of (Just term) -> eval term
                       Nothing -> t
