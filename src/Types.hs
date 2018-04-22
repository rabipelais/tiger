module Types where

import           Protolude hiding (Symbol)

import           Symbol    (Symbol)

newtype Unique = Unique Int
  deriving (Eq, Show)

uniqueSupply :: [Unique]
uniqueSupply = map Unique [0..]

data Type =
  Int
  | String
  | Record [(Symbol, Type)] Unique
  | Array Type Unique
  | Nil
  | Unit
  | Name Symbol (Maybe Type) -- this is a ref in the book
  deriving (Show, Eq)
