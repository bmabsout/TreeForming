{-# LANGUAGE TemplateHaskell #-}


module TreeShaping.WithPoly where

import Common
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Foldable.TH
import Diagrams
import TreeShaping.Lib hiding (pattern Circle, pattern Square, Leaf)
import Diagrams.Prelude (norm)

newtype Poly a = PolyC [a]
  deriving (Show, Eq, Functor, Foldable, Traversable)

data WithPoly
  = Orig (ShapesF WithPoly)
  | Pol (Poly WithPoly)
--   deriving (Show)
makeBaseFunctor ''WithPoly

pattern Square a b c d = Orig (SF (Sq a b c d))
pattern Circle a = Orig (CF (Circ a))
pattern Leaf = Orig LeafF
pattern Poly l = Pol (PolyC l)

instance Drawable Poly where
  draw (PolyC subShapes) = subDiagram2 <> polyDag # themed
    where
        shapes = padSubDiagsAndResize subShapes
        numShapes :: Num b => b
        numShapes = fromIntegral (length subShapes)
        r = norm $ size (head shapes)
        theta = pi / numShapes
        l = r/(2*sin theta)
        directions = iterate (rotateBy (1 / numShapes)) (V2 0 l)
        subDiagram2 = zipWith translate directions shapes # mconcat
        polyDag = regPoly numShapes (r * tan theta + r)

drawWithPolyF leaf (OrigF shapeF) = drawShapeF leaf shapeF
drawWithPolyF _ (PolF subShapes) = draw subShapes

drawWithPoly :: _ => Diag a -> WithPoly -> Diag a
drawWithPoly leaf = cata (drawWithPolyF leaf)

convertExample :: Shapes -> WithPoly
convertExample = hoist OrigF

example3 :: WithPoly
example3 = Poly [e, e, e, Leaf, Circle Leaf, e, Square e Leaf Leaf Leaf]
    where e = convertExample example2

main :: IO ()
main = runMain (`drawWithPoly` example3)