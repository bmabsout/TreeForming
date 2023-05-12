{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Typeclasses.Lib (module Typeclasses.Lib, module Common) where


import Linear
import Utils.Theme qualified as Theme

data Square a b c d = Square {topl :: a, topr :: b, botl :: c, botr :: d}
    deriving (Show, Eq, Functor, Foldable)
newtype Circle a = Circle a
    deriving (Show, Eq, Functor)
newtype Leaf a = Leaf (Diag a)



class Drawable a where
    draw :: a -> Diag b

instance (Drawable a, Drawable b, Drawable c, Drawable d) => Drawable (Square a b c d) where
    draw sq =
        let Square toplShape toprShape botlShape botrShape =
                padSubDiagsAndResize (draw <$> sq)
            subDiagram =
                    (toplShape ||| toprShape)
                === (botlShape ||| botrShape)
        in subDiagram # center <> square (maximum $ size subDiagram) # themed

instance Drawable a => Drawable (Circle a) where
    draw (Circle subshape) = subDiagram <> circle (norm (size subDiagram) / 2) # themed
        where subDiagram = draw subshape


instance Drawable (Leaf a) where
    draw (Leaf diag) = diag

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
example2Diag leafDiagram = shapeToDiagram leafDiagram example2

exampleDiag :: _ => Diag a -> Diag a
exampleDiag leafDiagram = shapeToDiagram leafDiagram example
