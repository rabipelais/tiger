module Env where

import           Protolude hiding (empty)

import           Symbol    (Table, empty)
import           Types     (Type)

type TEnv = Table Type

baseTEnv ::TEnv
baseTEnv = empty

data EnvEntry =
  VarEntry Type
  | FunEntry {formals :: [Type], result :: Type}
  deriving (Show, Eq)

type VEnv = Table EnvEntry

baseVEnv :: VEnv
baseVEnv = empty
