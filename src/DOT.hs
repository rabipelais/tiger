{-# LANGUAGE OverloadedStrings #-}

module DOT ( Tree(..), toDOT ) where

import           Protolude as P

data Tree a = Tree a [Tree a] deriving Show

data Node = Node Id Label
newtype Id = Id Text
newtype Label = Label Text
data Edge = Edge Id Id

renderAsDOT :: Tree Node -> Text
renderAsDOT t = showText "graph g {\n" .
                showSepByLines showNode (nodes t) .
                showSepByLines showEdge (edges t) .
                showText "}" $ ""
  where nodes (Tree n ts) = n : concatMap nodes ts
        edges (Tree (Node i1 _) ts) =
          [ Edge i1 i2 | Tree (Node i2 _) _ <- ts ] ++ concatMap edges ts
        showNode (Node i l) =
          showId i . showText " [ label=\"" . showLabel l . showText "\" ]"
        showId (Id i) = showText i
        showLabel (Label l) = showText l
        showEdge (Edge a b) = showId a . showText " -- " . showId b
        showSepByLines f = foldr (\x s -> f x . showText "\n" . s) id
        showText = (<>)
        id a = a

addIds :: Tree Text -> Tree Node
addIds = flip evalState (0 :: Int) . go
  where go (Tree s ts) = do
          i <- get
          put (i + 1)
          ns <- mapM go ts
          return $ Tree (Node (Id ("n" <> P.show i)) (Label s)) ns

toDOT :: Tree Text -> Text
toDOT = renderAsDOT . addIds
