{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module TreeShaping.Lib(
  Square(..),
  pattern Square,
  Circle(..),
  pattern Circle,
  Drawable(..),
  Shapes(..),
  ShapesF(..),
  drawShape,
  drawShapeF,
  example,
  example2
) where

import Common
import Diagrams.Prelude
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Text.Show.Deriving

data Square a = Sq {topl :: a, topr :: a, botl :: a, botr :: a}
  deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveShow1 ''Square)

newtype Circle a = Circ a
  deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveShow1 ''Circle)



data Shapes =
  S (Square Shapes) | C (Circle Shapes) | Leaf
  -- deriving (Show)
makeBaseFunctor ''Shapes
$(deriveShow1 ''ShapesF)

pattern Square{topl, topr, botl, botr} = S (Sq {topl, topr, botl, botr})

pattern Circle a = C (Circ a)

class Drawable f where
  draw :: (Renderable (Path V2 Double) a) => f (Diag a) -> Diag a

instance Drawable Square where
  draw sq = subDiagram # center <> square (maximum $ size subDiagram) # themed
    where
      paddedSq = padSubDiagsAndResize sq
      subDiagram =
            (paddedSq.topl ||| paddedSq.topr)
        === (paddedSq.botl ||| paddedSq.botr)

instance Drawable Circle where
  draw :: Renderable (Path V2 Double) a => Circle (Diag a) -> Diag a
  draw (Circ subDiagram) = subDiagram <> circle (norm (size subDiagram) / 2) # themed

drawShapeF :: _ => Diag a -> ShapesF (Diag a) -> Diag a
drawShapeF leaf = \case
  SF s  -> draw s
  CF s  -> draw s
  LeafF -> leaf

drawShape :: _ => Diag a -> Shapes -> Diag a
drawShape leaf = cata (drawShapeF leaf)

example :: _
example =
  Circle $
    Circle $
      Square
        (Circle (Circle Leaf)) (Circle Leaf)
        Leaf                   (Circle Leaf)

example2 :: _
example2 = Square
  example example
  example Leaf


main :: IO ()
main = runMain (`drawShape` example2)