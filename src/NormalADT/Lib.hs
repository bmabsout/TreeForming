{-# LANGUAGE RecordWildCards #-}


module NormalADT.Lib (Shapes(..), shapeToDiagram, example, example2) where

import Diagrams.Prelude
import Common
import Linear

data Shapes =
    Square {topl:: Shapes, topr:: Shapes, botl:: Shapes, botr:: Shapes}
    | Circle Shapes
    | Leaf

example :: Shapes
example = Circle $ Circle $
    Square (Circle (Circle Leaf)) (Circle Leaf)
           Leaf                   (Circle Leaf)
example2 :: Shapes
example2 = Square example example example Leaf


shapeToDiagram :: forall a. Renderable (Path V2 Double) a => Diag a -> Shapes -> Diag a
shapeToDiagram leafDiagram = recurse
  where
    recurse :: Shapes -> Diag a
    recurse = \case
        Square {..} ->
            let V4 toplShape toprShape botlShape botrShape =
                    padSubDiagsAndResize (recurse <$> V4 topl topr botl botr)
                subDiagram =
                        (toplShape ||| toprShape)
                    === (botlShape ||| botrShape)
            in subDiagram # center <> square (maximum $ size subDiagram) # themed
        Circle subshape ->
            let subDiagram = recurse subshape
            in subDiagram <> circle (norm (size subDiagram) /2) # themed
        Leaf -> leafDiagram


example2Diag :: _ => Diag a -> Diag a
example2Diag leafDiagram = shapeToDiagram leafDiagram example2
exampleDiag :: _ => Diag a -> Diag a
exampleDiag leafDiagram = shapeToDiagram leafDiagram example

main :: IO ()
main = runMain example2Diag