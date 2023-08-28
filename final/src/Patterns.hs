-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

module Patterns where

import Control.Exception (throw, catch)
import Types
import Parser

execFile :: FilePath -> IO Value
execFile f = (readFile f >>= execString) `catch` exitError

execString :: String -> IO Value
execString s = execExpr (parseExpr s) `catch` exitError

execExpr :: Expr -> IO Value
execExpr e = return (eval [] e) `catch` exitError

parse :: String -> Expr
parse = parseExpr

parseFile :: FilePath -> IO ()
parseFile f = readFile f >>= (print . parse)

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

-------------------------------------------------------------------------------
-- Task 3.1: Monad Instance for Result
-------------------------------------------------------------------------------

-- | Result of a computation that can fail
data Result a
  = Fail
  | Success a
  deriving (Eq, Show)

instance Monad Result where
  return x  =   Success x

  res >>= process  =   
    case res of
      Fail -> Fail
      Success a -> process a

-- | Used for testing
ret3 :: Result Int
ret3 = return 3

-------------------------------------------------------------------------------
-- Task 3.2: Pattern matching
-------------------------------------------------------------------------------

-- | Match value against a pattern;
-- if no match, return Fail
-- if match, return bindings for pattern variables  
match :: Pattern -> Value -> Result Env
match (PVar x) v = Success [(x, v)]
match (PCons p1 p2) (VCons v1 v2) =
  do
    env1 <- match p1 v1
    env2 <- match p2 v2
    return (env1 ++ env2)
match PNil VNil = Success []
match _ _ = Fail

-- | Used for testing
consPat = PCons (PVar "y") (PVar "ys")
l123    = VCons (VInt 1) (VCons (VInt 2) (VCons (VInt 3) VNil))
e123    = EBin Cons (EInt 1) (EBin Cons (EInt 2) (EBin Cons (EInt 3) ENil))

-------------------------------------------------------------------------------
-- Task 3.3: Case expression
-------------------------------------------------------------------------------

eval :: Env -> Expr -> Value
eval env (EInt n)        = VInt n
eval env (EVar x)        = case lookupId x env of
                            v@(VClos clEnv y e) -> VClos (extend x v clEnv) y e 
                            v -> v
eval _   ENil            = VNil                            
eval env (EBin op e1 e2) = evalOp op (eval env e1) (eval env e2)
eval env (ELet x e1 e2)  = eval env1 e2
  where
    v1                   = eval env e1
    env1                 = extend x v1 env
eval env (EApp e1 e2)    = evalApp (eval env e1) (eval env e2)
eval env (ELam x e)      = VClos env x e
eval env (ECase e cases) =  
    let eValue = eval env e
  in
    case matchPattern eValue cases of
      Just (p, c) -> eval (extendPattern env p eValue) c
      Nothing         -> throw (Error "non-exhaustive patterns")

-- helper function that matches the expression value with the cases
matchPattern :: Value -> [(Pattern, Expr)] -> Maybe (Pattern, Expr)
matchPattern _ [] = Nothing
matchPattern eValue ((p, c) : rest) =
  case match p eValue of
    Success _ -> Just (p, c)
    Fail      -> matchPattern eValue rest

-- helper function that extends the environment with pattern bindings 
extendPattern :: Env -> Pattern -> Value -> Env
extendPattern env (PVar x) v = extend x v env
extendPattern env (PCons p1 p2) (VCons v1 v2) =
  extendPattern (extendPattern env p1 v1) p2 v2
extendPattern env PNil VNil = env

-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

evalOp :: Binop -> Value -> Value -> Value
evalOp Plus   (VInt n)     (VInt m)     = VInt  (n + m)
evalOp Cons   v1           v2           = VCons v1 v2
evalOp _      _        _                = throw (Error "type error: binop")

evalApp :: Value -> Value -> Value
evalApp (VClos env x e) v = eval (extend x v env) e
evalApp _               _ = throw (Error "type error: closure")

instance Functor Result where
  fmap f (Success v) = Success (f v)
  fmap f Fail        = Fail

instance Applicative Result where
  pure v = Success v

  (Success f) <*> (Success x) = Success (f x)
  Fail        <*> _           = Fail
  _           <*> Fail        = Fail
  