{-# LANGUAGE DuplicateRecordFields #-}

import Data.Void
import Common
import Diagrams.Prelude
import Linear

data Shapes x
    = Circ (XCirc x) (Shapes x)
    | Sq {ext :: XSq x, topl :: Shapes x, topr :: Shapes x, botl :: Shapes x, botr :: Shapes x}
    | Lf (Xl x)

type family XCirc x
type family XSq x
type family Xl x


data UD
type instance XCirc UD = Void
type instance XSq UD = Void
type instance Xl UD = Void

pattern Circle :: Shapes UD -> Shapes UD
pattern Circle c <- Circ _ c
    where Circle c = Circ undefined c


pattern Square :: Shapes UD -> Shapes UD -> Shapes UD -> Shapes UD -> Shapes UD
pattern Square{topl, topr, botl, botr} <- Sq {ext=_, topl=topl, topr=topr, botl=botl, botr=botr}
  where
    Square c = Sq undefined c

pattern Leaf <- Lf _
    where Leaf = Lf undefined


drawShape :: Renderable (Path V2 Double) a =>
    Diag a -> (Shapes x -> Diag a) -> Shapes x -> Diag a
drawShape l d (Circ _ subShape) = subDiagram <> circle (norm (size subDiagram) / 2) # themed
  where subDiagram = drawShape l d subShape
drawShape l d (Sq {..}) =
    subDiagram # center <> square (maximum $ size subDiagram) # themed
  where
    V4 toplShape toprShape botlShape botrShape =
            padSubDiagsAndResize (drawShape l d <$> V4 topl topr botl botr)
    subDiagram =
            (toplShape ||| toprShape)
        === (botlShape ||| botrShape)
drawShape leafDiag _ (Lf _) = leafDiag

example :: Shapes UD
example =
  Circle $
    Circle $
      Square
        (Circle (Circle Leaf))
        (Circle Leaf)
        Leaf
        (Circle Leaf)

example2 :: Shapes UD
example2 = Square example example example Leaf

main = runMain (\l -> drawShape l undefined example2)