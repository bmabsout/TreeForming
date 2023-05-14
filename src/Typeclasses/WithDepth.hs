module Typeclasses.WithDepth where

import Diagrams
import Diagrams.Prelude
import Linear
import Common
import Typeclasses.Lib

class WithDepth a where
  withDepth :: (Renderable (Path V2 Double) b) => Int -> Diag b -> a -> Diag b

instance (WithDepth a, WithDepth b, WithDepth c, WithDepth d) => WithDepth (Square a b c d) where
  withDepth depth leaf (Square {..}) =
    let V4 toplShape toprShape botlShape botrShape =
          padSubDiagsAndResize (V4 
            (withDepth (depth+1) leaf topl) (withDepth (depth+1) leaf topr)
            (withDepth (depth+1) leaf botl) (withDepth (depth+1) leaf botr))
        subDiagram =
              (toplShape ||| toprShape)
          === (botlShape ||| botrShape)
     in subDiagram # center <> square (maximum $ size subDiagram) # depthToTheme depth

instance WithDepth a => WithDepth (Circle a) where
  withDepth depth leaf (Circle subshape) = subDiagram <> circle (norm (size subDiagram) / 2) # depthToTheme depth
    where
      subDiagram = withDepth (depth+1) leaf subshape

instance WithDepth Leaf where
  withDepth _ leaf Leaf = leaf


main :: IO ()
main = runMain (\l -> withDepth 0 l example2)