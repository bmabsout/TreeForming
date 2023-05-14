{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TreesThatGrow.WithDepthAndPoly where
import Common
import TreesThatGrow.WithPoly hiding (main)
import TreesThatGrow.WithDepth hiding (main)
import TreesThatGrow.Lib hiding (main)
import Diagrams.Prelude

data WithDPoly
type instance XCirc WithDPoly = XCirc (WithDepth WithPoly)
type instance XSq WithDPoly = XSq (WithDepth WithPoly)
type instance Xl WithDPoly = Xl (WithDepth WithPoly)
type instance XShapes WithDPoly  = (Depth, [Shapes WithDPoly])


instance PostProcess [Shapes WithDPoly] where
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

instance Depther (Shapes WithPoly) WithDPoly where
    depther :: Depth -> Shapes WithPoly -> Shapes WithDPoly
    depther depth (Lf x) = Lf (depth, x)
    depther depth (Circ e c) = Circ (depth, e) (depther (depth+1) c)
    depther depth (Sq e tr tl br bl) = Sq (depth, e) rtr rtl rbr rbl
      where
        rtr = depther (depth + 1)  tr
        rtl = depther (depth + 1)  tl
        rbr = depther (depth + 1)  br
        rbl = depther (depth + 1)  bl
    depther depth (Shapes e) = depther depth e

instance Depther [Shapes WithPoly] WithDPoly where
  depther :: Depth -> [Shapes WithPoly] -> Shapes WithDPoly
  depther depth e= Shapes (depth, depther (depth+1) <$> e)

main = runMain (post (depther 0 example3 :: Shapes WithDPoly))