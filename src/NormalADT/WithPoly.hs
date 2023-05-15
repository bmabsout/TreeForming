module NormalADT.WithPoly where

import Diagrams
import Common
import Linear

data Shapes
  = Square {topl :: Shapes, topr :: Shapes, botl :: Shapes, botr :: Shapes}
  | Circle Shapes
  | Poly [Shapes]
  | Leaf

example :: Shapes
example =
  Circle $
    Circle $
      Square
        (Circle (Circle Leaf))
        (Circle Leaf)
        Leaf
        (Circle Leaf)

example2 :: Shapes
example2 = Square example example example Leaf


example3 :: Shapes
example3 = Circle (Poly [sq, Circle sq, triLeaf, sq, triLeaf])
    where sq = Square (Circle Leaf) triLeaf (Poly (replicate 8 Leaf)) Leaf
          triLeaf = Poly [Leaf, Leaf, Leaf]

fromSlide :: Shapes
fromSlide = Poly
    [Leaf, Leaf, Leaf, Circle Leaf,
      Circle
        (Square
          Leaf (Circle Leaf)
          Leaf Leaf
        )
    ]



circularEnvelope :: forall a f. _ => f (Diag a) -> f (Diag a)
circularEnvelope shapes = withEnvelope (circle maxDim :: Diag a) <$> shapes
    where maxDim = maximum $ norm $ size <$> shapes

shapeToDiagram :: forall a. Renderable (Path V2 Double) a => Diag a -> Shapes -> Diag a
shapeToDiagram leafDiagram = recurse
  where
    recurse :: Shapes -> Diag a
    recurse = \case
        Square {..} ->
            let V4 toplShape toprShape botlShape botrShape =
                    padSubDiagsAndResize ( recurse <$> V4 topl topr botl botr)
                subDiagram =
                        (toplShape ||| toprShape)
                    === (botlShape ||| botrShape)
            in subDiagram # center <> rect (width subDiagram) (height subDiagram) # themed
        Circle subshape ->
            let subDiagram = recurse subshape
            in subDiagram <> circle (norm (size subDiagram) /1.9) # themed
        Poly subShapes ->
            let shapes = padSubDiagsAndResize (recurse <$> subShapes)
                numShapes :: Num b => b
                numShapes = fromIntegral (length subShapes)
                r = norm $ size (head shapes)
                theta = pi / numShapes
                l = r/(2*sin theta)
                directions = iterate (rotateBy (1 / numShapes)) (V2 0 l)
                subDiagram2 = zipWith translate directions shapes # mconcat
                polyDag = regPoly numShapes (r * tan theta + r)
            in subDiagram2 <> polyDag # themed
        Leaf -> leafDiagram


main :: IO ()
main = runMain (`shapeToDiagram` fromSlide)