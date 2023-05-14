module TreesThatGrow.WithPoly where
import TreesThatGrow.Lib hiding (main)
import Data.Void
import Common
import Diagrams.Prelude

data WithPoly
type instance XCirc WithPoly = Void
type instance XSq WithPoly = Void
type instance Xl WithPoly = Void
type instance XShapes WithPoly  = [Shapes WithPoly]


instance PostProcess [Shapes WithPoly] where
    post subShapes leafDiag = subDiagram2 <> polyDag # themed
      where        
        shapes = padSubDiagsAndResize ((`post` leafDiag) <$> subShapes)
        numShapes :: Num b => b
        numShapes = fromIntegral (length subShapes)
        r = norm $ size (head shapes)
        theta = pi / numShapes
        l = r/(2*sin theta)
        directions = iterate (rotateBy (1 / numShapes)) (V2 0 l)
        subDiagram2 = zipWith translate directions shapes # mconcat
        polyDag = regPoly numShapes (r * tan theta + r)


pattern Poly :: XShapes WithPoly -> Shapes WithPoly
pattern Poly x = Shapes x

example3 = Poly [example2, Leaf, example2, Circle Leaf, Leaf]

main = runMain (post example3)