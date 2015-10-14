module Context where

type Context = [(String, Binding)]

data Binding = NameBind
             | VarBind TmType
             deriving Show

data TmType = TyBool
            | TyNat
            | TyArrow TmType TmType
            deriving Eq

data Info = Info {row :: Int, col :: Int}

instance Show TmType where
    show TyBool = "Bool"
    show TyNat = "Nat"
    show (TyArrow t1 t2) = case t1 of
        TyArrow _ _ -> "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
        _ -> show t1 ++ " -> " ++ show t2

instance Show Info where
    show (Info r c) = show r ++ ":" ++ show c

addBinding :: Context -> String -> Binding -> Context
addBinding ctx var bind = (var, bind) : ctx

getTypeFromContext :: Info -> Context -> Int -> Either String TmType
getTypeFromContext fi ctx i =
    case c of
        Nothing -> Left $ show fi ++ ": Invalid index " ++ show i
        Just (_, VarBind ty) -> Right ty
        _ -> Left $ show fi ++ ": Not a VarBind"
    where bindAt [] _ = Nothing
          bindAt (x:xs) n | n == 0 = Just x
                          | n  > 0 = bindAt xs (n - 1)
          bindAt _ _ = Nothing
          c = bindAt ctx i
