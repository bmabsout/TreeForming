module NormalADT.WithDepth where
import Diagrams
import Diagrams.Prelude
import Linear
import Common

import NormalADT.Lib


shapeToDiagramWithDepth :: forall a. Renderable (Path V2 Double) a => Diag a -> Shapes -> Diag a
shapeToDiagramWithDepth leafDiagram = recurse 0
  where
    recurse :: Int -> Shapes -> Diag a
    recurse depth = \case
      Square {..} ->
        let V4 toplShape toprShape botlShape botrShape =
              padSubDiagsAndResize (recurse (depth + 1) <$> V4 topl topr botl botr)
            subDiagram =
              (toplShape ||| toprShape)
                === (botlShape ||| botrShape)
        in subDiagram # center <> square (maximum $ size subDiagram) # depthToTheme depth
      Circle subshape ->
        let subDiagram = recurse (depth + 1) subshape
        in subDiagram <> circle (norm (size subDiagram) / 2) # depthToTheme depth
      Leaf -> leafDiagram


example2Diag :: _ => Diag a -> Diag a
example2Diag leafDiag = shapeToDiagramWithDepth leafDiag example2

main :: IO ()
main = runMain example2Diag