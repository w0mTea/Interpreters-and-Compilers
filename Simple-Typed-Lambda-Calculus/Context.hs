module Context where

type Context = [(String, Binding)]

data Binding = NameBind
             | VarBind TmType
             deriving Show

data TmType = TyBool
            | TyArrow TmType TmType
            deriving (Show, Eq)

data Info = Info {row :: Int, col :: Int} deriving Show

addBinding :: Context -> String -> Binding -> Context
addBinding ctx var bind = (var, bind) : ctx

getTypeFromContext :: Info -> Context -> Int -> Either String TmType
getTypeFromContext fi ctx i = let (_, b) = (reverse ctx) !! i in
    case b of
        (VarBind ty) -> Right ty
        _ -> Left $ show fi
