{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecordWildCards #-}

module Typeclasses.Lib (Square(..), Circle(..), Leaf(..), Drawable(..), example, example2) where

import Diagrams.Prelude
import Common
import Linear

data Square a b c d = Square {topl :: a, topr :: b, botl :: c, botr :: d}
    deriving (Show, Eq, Functor, Foldable)
newtype Circle a = Circle a
    deriving (Show, Eq, Functor)
data Leaf = Leaf


class Drawable a where
    draw :: (Renderable (Path V2 Double) b) => Diag b -> a -> Diag b

instance (Drawable a, Drawable b, Drawable c, Drawable d) => Drawable (Square a b c d) where
    draw leaf (Square {..}) =
        let V4 toplShape toprShape botlShape botrShape =
                padSubDiagsAndResize (V4 (draw leaf topl) (draw leaf topr) (draw leaf botl) (draw leaf botr))
            subDiagram =
                    (toplShape ||| toprShape)
                === (botlShape ||| botrShape)
        in subDiagram # center <> square (maximum $ size subDiagram) # themed

instance Drawable a => Drawable (Circle a) where
    draw leaf (Circle subshape) = subDiagram <> circle (norm (size subDiagram) / 2) # themed
        where subDiagram = draw leaf subshape


instance Drawable Leaf where
    draw leaf Leaf = leaf

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
example2Diag leafDiagram = draw leafDiagram example2

exampleDiag :: _ => Diag a -> Diag a
exampleDiag leafDiagram = draw leafDiagram example

main = runMain example2Diag