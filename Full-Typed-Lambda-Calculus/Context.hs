module Context where
import Data.List (intercalate)
import qualified Data.Map as Map

type Context = [(String, Binding)]

data Binding = NameBind
             | VarBind TmType
             deriving Show

data TmType = TyBool
            | TyNat
            | TyArrow TmType TmType
            | TyUnit
            | TyTuple [TmType] Int -- Empty tuple is not allowed. Int represents length of [TmType]
            | TyRecord (Map.Map String TmType)
            deriving Eq

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
        [] -> "Error: Empty tuple occurs"
        [t] -> "{" ++ show t ++ "}"
        _ -> "{" ++ s ++ "}"
            where s = intercalate ", " $ map show ts
    show (TyRecord m) | Map.null m = "Error: Empty record occurs"
                      | Map.size m == 1 = let [(l, t)] = Map.toList m
                                          in "{" ++ l ++ " = " ++ show t ++ "}"
                      | otherwise = let ts = Map.toList m
                                        show' (l, t) = l ++ " = " ++ show t
                                        s = intercalate ", " $ map show' ts
                                    in "{" ++ s ++ "}"

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

(===) :: TmType -> TmType -> Bool
TyUnit === _ = True
_ === TyUnit = True
a === b = a == b
