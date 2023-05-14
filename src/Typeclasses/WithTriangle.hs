module Typeclasses.WithTriangle where

import Diagrams
import Linear
import Common
import Typeclasses.Lib

data Triangle a b c = Triangle {top::a, botl::b , botr::c}

instance (Drawable a, Drawable b, Drawable c) => Drawable (Triangle a b c) where
  draw leaf (Triangle {..}) =
    let subShapes = padSubDiagsAndResize [draw leaf top, draw leaf botl, draw leaf botr]
        r = norm $ size (head subShapes)
        theta = pi / 3
        l = r/(2*sin theta)
        directions = iterate (rotateBy (1 / 3)) (V2 0 l)
        subDiagram2 = zipWith translate directions subShapes # mconcat
        polyDag = triangle (r * tan theta + r)
    in subDiagram2 <> polyDag # themed


example3 = Circle (Triangle sq (Circle sq) triLeaf)
  where
    sq = Square (Circle Leaf) triLeaf (Triangle triLeaf Leaf (Circle Leaf)) Leaf
    triLeaf = Triangle Leaf Leaf Leaf


main :: IO ()
main = runMain (`draw` example3)