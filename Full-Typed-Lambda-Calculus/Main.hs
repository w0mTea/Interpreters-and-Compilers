import Syntax
import Parser
import TypeChecker
import Eval
import System.Environment

main :: IO [()]
main = do
  args <- getArgs
  mapM runFile args

runFile :: String -> IO ()
runFile path = do
  s <- readFile path
  let term = lcParse s
  case term of
    Left e -> print e
    Right t -> checkType t

checkType :: Term -> IO()
checkType t = let ty = typeOf [] t in
    case ty of
        Left e -> print e
        Right _ -> evalTerm t

evalTerm :: Term -> IO ()
evalTerm t = let t' = eval t in
  if isVal t'
  then putStrLn $ printTerm [] t'
  else putStrLn $ "Evaluating Error: " ++ show t'
