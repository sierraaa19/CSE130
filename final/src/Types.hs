module Types where

import           Control.Exception

data Error = Error {errMsg :: String}
             deriving (Show)

instance Exception Error

data Binop
  = Plus
  | Cons
  deriving (Eq, Show)

type Id = String

-- | Expressions
data Expr
  = EInt  Int
  | ENil
  | EVar Id
  | EBin Binop Expr Expr
  | ELet Id   Expr  Expr
  | EApp Expr Expr
  | ELam Id   Expr
  | ECase Expr [(Pattern, Expr)]
  deriving (Eq, Show)

-- | Values
data Value
  = VInt  Int
  | VClos Env Id Expr
  | VNil
  | VCons Value Value
  | VErr  String
  deriving (Eq, Show)

-- | Patterns
data Pattern
  = PVar Id                -- Variable
  | PInt Int               -- Integer constant
  | PNil                   -- Nil
  | PCons Pattern Pattern  -- Cons applied to patterns
  deriving (Eq, Show)

type Env = [(Id, Value)]

lookupId :: Id -> Env -> Value
lookupId x ((y, v) : env)
  | x == y    = v
  | otherwise = lookupId x env
lookupId x [] = throw (Error ("unbound variable: " ++ x))

extend :: Id -> Value -> Env -> Env
extend x v env = (x, v) : env

exprList :: [Expr] -> Expr
exprList = foldr (EBin Cons) ENil

valueList :: [Value] -> Value
valueList = foldr VCons VNil
