module Env where

import           Protolude hiding (empty)

import           Symbol    (Symbol (..), Table, empty, enter)
import           Types     (Type (..))

type TEnv = Table Type

baseTEnv ::TEnv
baseTEnv = enter (Symbol "string") Types.String $ enter (Symbol "int") Types.Int empty

data EnvEntry =
  VarEntry Type
  | FunEntry {formals :: [Type], result :: Type}
  deriving (Show, Eq)

type VEnv = Table EnvEntry

baseVEnv :: VEnv
baseVEnv = empty
