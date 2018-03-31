{-# LANGUAGE OverloadedStrings #-}

module DOT ( Tree(..), toDOT ) where

import qualified Data.Text as T
import           Protolude as P

data Tree a = Tree a [Tree a] deriving Show

data Node = Node Id Label
newtype Id = Id Text
newtype Label = Label Text
data Edge = Edge Id Id

renderAsDOT :: Tree Node -> Text
renderAsDOT t = drawTree t

addIds :: Tree Text -> Tree Node
addIds = flip evalState (0 :: Int) . go
  where go (Tree s ts) = do
          i <- get
          put (i + 1)
          ns <- mapM go ts
          return $ Tree (Node (Id ("n" <> P.show i)) (Label s)) ns

toDOT :: Tree Text -> Text
toDOT = renderAsDOT . addIds


-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree Node -> Text
drawTree  = T.unlines . draw

draw :: Tree Node -> [Text]
draw (Tree n ts0) =  (showNode n) : (drawSubTrees ts0)
  where
    showNode (Node i l) =
      showLabel l
    showId (Id i) = i
    showLabel (Label l) = l
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) <> drawSubTrees ts

    shift first other = zipWith (<>) (first : repeat other)
