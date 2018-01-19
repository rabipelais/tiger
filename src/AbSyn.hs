{-# LANGUAGE DuplicateRecordFields #-}

module AbSyn where

import Symbol (Symbol)

import Protolude hiding (Symbol)

data Var =
  SimpleVar Symbol
  | FieldVar Var Symbol
  | SubscriptVar Var Exp
  deriving (Show)

data Exp =
  VarExp Var
  | NilExp
  | IntExp Int
  | StringExp Text
  | CallExp Symbol [Exp]
  | OpExp Exp Oper Exp
  | RecordExp [(Symbol, Exp)] Symbol
  | SeqExp [Exp]
  | AssignExp Var Exp
  | IfExp Exp Exp (Maybe Exp)
  | WhilExp Exp Exp
  | ForExp Symbol {- escape :: bool ref -} Exp Exp Exp -- var, lo, hi, body
  | BreakExp
  | LetExp [Dec] Exp
  | ArrayExp Symbol Exp Exp
  deriving (Show)

data Dec =
  FunctionDec [FunDec]
  | VarDec Symbol {- escape :: bool ref -} (Maybe Symbol) Exp -- name, typ, init
  | TypeDec Symbol Ty
  deriving (Show)

data Ty =
  NameTy Symbol
  | RecordTy [Field]
  | ArrayTy Symbol
  deriving (Show)

data Oper =
  PlusOp | MinusOp | TimesOp | DivideOp | EqOp
  | NeqOp | LtOp | LeOp | GtOp | GeOp
  deriving (Show)

data Field =
  Field { name :: Symbol
        {-, escape :: bool ref -}
        , typ :: Symbol}
  deriving (Show)

data FunDec =
  FunDec { name :: Symbol
         , params :: [Field]
         , result :: Maybe Symbol
         , body :: Exp}
  deriving (Show)
