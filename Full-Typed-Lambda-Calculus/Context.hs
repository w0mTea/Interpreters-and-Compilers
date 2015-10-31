module Context where
import Data.List (intercalate)

type Context = [(String, Binding)]

data Binding = NameBind
             | VarBind TmType
             deriving Show

data TmType = TyBool
            | TyNat
            | TyArrow TmType TmType
            | TyUnit
            | TyTuple [TmType] Int -- Empty tuple is not allowed. Int represents length of [TmType]

data Info = Info {row :: Int, col :: Int}

dummyinfo :: Info
dummyinfo = Info 0 0

instance Show TmType where
    show TyBool = "Bool"
    show TyNat = "Nat"
    show (TyArrow t1 t2) = case t1 of
        TyArrow _ _ -> "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
        _ -> show t1 ++ " -> " ++ show t2
    show TyUnit = "Unit"
    show (TyTuple ts _) = case ts of
        [] -> "Error: empty tuple occured"
        [t] -> "{" ++ show t ++ "}"
        _ -> "{" ++ s ++ "}"
            where s = intercalate ", " $ map show ts

instance Eq TmType where
    TyUnit == _ = True
    _ == TyUnit = True
    TyBool == TyBool = True
    TyNat == TyNat = True
    (TyArrow t1 t2) == (TyArrow t1' t2') = t1 == t1' && t2 == t2'
    (TyTuple ts l) == (TyTuple ts' l') = l == l' && and2 ts ts'
        where and2 [] [] = True
              and2 [x] [y] = x == y
              and2 (x:xs) (y:ys) = x == y && and2 xs ys
              and2 _ _ = False
    _ == _ = False

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
