module Tagless.WithPoly where
import Tagless.Lib
import Common
import Data.Functor.Identity
import Diagrams.Prelude

class WithPoly repr where
    poly :: (Renderable (Path V2 Double) a) => [repr (Diag a)] -> repr (Diag a)

instance WithPoly Identity where
    poly subShapes = Identity $ subDiagram2 <> polyDag # themed
      where
        shapes = padSubDiagsAndResize (runIdentity <$> subShapes)
        numShapes :: Num b => b
        numShapes = fromIntegral (length subShapes)
        r = norm $ size (head shapes)
        theta = pi / numShapes
        l = r/(2*sin theta)
        directions = iterate (rotateBy (1 / numShapes)) (V2 0 l)
        subDiagram2 = zipWith translate directions shapes # mconcat
        polyDag = regPoly numShapes (r * tan theta + r)

example3 leafDiag = poly [example2 leafDiag, example2 leafDiag, example2 leafDiag, l, l]
  where l = leaf leafDiag

main = runMain (runIdentity . example3)