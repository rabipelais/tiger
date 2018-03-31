module Symbol (Symbol(..), symbol, name
              , StringTable, emptyStringTable
              , Table, Symbol.empty
              , look, enter) where

import           Data.HashMap.Strict as M hiding (size)
import           Protolude           hiding (Symbol)

data Symbol =
  Symbol { name     :: Text
         , uniqueId :: Int}
  deriving (Show)

instance Eq Symbol where
  (==) = (==) `on` uniqueId

instance Ord Symbol where
  compare = compare `on` uniqueId

instance Hashable Symbol where
  hashWithSalt s = hashWithSalt s . uniqueId

-- Computing the size of a HashMap takes O(n) time, so we cache it.
data StringTable =
  StringTable { hashMap :: HashMap Text Symbol
              , size    :: Int}
  deriving (Show)

emptyStringTable :: StringTable
emptyStringTable = StringTable M.empty 0

lookupStringTable :: Text -> StringTable -> Maybe Symbol
lookupStringTable n = lookup n . hashMap

insertStringTable :: Text -> StringTable -> (Symbol, StringTable)
insertStringTable n t = (s, StringTable (insert n s (hashMap t)) (size t + 1))
   where s = Symbol n (size t)

-- Note: Different signature, because we want to be pure.
symbol :: Text -> StringTable -> (Symbol, StringTable)
symbol n t =
  maybe (insertStringTable n t) (\s -> (s, t)) (lookupStringTable n t)

--------------------------------------------------------------------------------

newtype Table a = Table { unTable :: HashMap Symbol a }
  deriving ( Show )

empty :: Table a
empty = Table M.empty

look :: Symbol -> Table a -> Maybe a
look s =  lookup s . unTable

enter :: Symbol -> a -> Table a -> Table a
enter s v = Table . insert s v . unTable
