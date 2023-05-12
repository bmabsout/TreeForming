{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module TreeShaping.Lib where

import Common
import Diagrams.Prelude
import Data.Functor.Foldable.TH
import Data.Functor.Foldable

data Square a = Sq {topl :: a, topr :: a, botl :: a, botr :: a}
  deriving (Show, Eq, Functor, Foldable, Traversable)

pattern Square :: Shapes -> Shapes -> Shapes -> Shapes -> Shapes
pattern Square{topl, topr, botl, botr} = S (Sq {topl, topr, botl, botr})

newtype Circle a = Circ a
  deriving (Show, Eq, Functor, Foldable, Traversable)

pattern Circle :: Shapes -> Shapes
pattern Circle a = C (Circ a)

data Shapes =
  S (Square Shapes) | C (Circle Shapes) | Leaf
makeBaseFunctor ''Shapes


class Drawable f where
  draw :: (Renderable (Path V2 Double) a) => f (Diag a) -> Diag a

instance Drawable Square where
  draw sq = subDiagram # center <> square (maximum $ size subDiagram) # themed
    where
      paddedSq = padSubDiagsAndResize sq
      subDiagram =
            paddedSq.topl ||| paddedSq.topr
        === paddedSq.botl ||| paddedSq.botr

instance Drawable Circle where
  draw :: Renderable (Path V2 Double) a => Circle (Diag a) -> Diag a
  draw (Circ subDiagram) = subDiagram <> circle (norm (size subDiagram) / 2) # themed


drawShape :: _ => Diag a -> Shapes -> Diag a
drawShape leaf = cata \case
  SF s -> draw s
  CF c -> draw c
  LeafF -> leaf

example :: _
example =
  Circle $
    Circle $
      Square
        (Circle (Circle Leaf))
        (Circle Leaf)
        Leaf
        (Circle Leaf)

example2 :: _
example2 = Square example example example Leaf

example2Diag :: _ => Diag a -> Diag a
example2Diag leafDiagram = drawShape leafDiagram example2

exampleDiag :: _ => Diag a -> Diag a
exampleDiag leafDiagram = drawShape leafDiagram example

main :: IO ()
main = runMain example2Diag