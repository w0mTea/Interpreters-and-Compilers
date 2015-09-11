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

-- the implementaion below uses big-step evaluation
{- v means evaluate to a value
  v1 v v1

  t1 v true t2 v v2
  -------------------------------
  if t1 then t2 else t3 v v2

  t1 v false t2 v v2
  -------------------------------
  if t1 then t2 else t3 v v3

  t1 v nv
  -------------------------------
  succ t1 v succ nv1

  t1 v 0
  -------------------------------
  pred t1 v 0

  t1 v succ nv1
  -------------------------------
  pred t1 v nv1

  t1 v 0
  -------------------------------
  iszero t1 v true

  t1 v succ nv1
  -------------------------------
  iszero t1 v false

-}

bigStepEval :: Term -> Maybe Term
bigStepEval t | isVal t = return t
bigStepEval (TmIf info c t f) = let c' = bigStepEval c in
  case c' of (Just (TmTrue _)) -> bigStepEval t
             (Just (TmFalse _)) -> bigStepEval f
             otherwise -> Nothing
bigStepEval (TmSucc info t) = let t' = bigStepEval t in
  case t' of (Just term) | isNumericVal term -> return $ TmSucc info term
             otherwise -> Nothing
bigStepEval (TmPred _ t) = let t' = bigStepEval t in
  case t' of (Just (TmZero _)) -> return $ TmZero dummyinfo
             (Just (TmSucc _ nv)) -> return nv
             otherwise -> Nothing
bigStepEval (TmIsZero _ t) = let t' = bigStepEval t in
  case t' of (Just (TmZero _)) -> return $ TmTrue dummyinfo
             (Just (TmSucc _ _)) -> return $ TmFalse dummyinfo
             otherwise -> Nothing
