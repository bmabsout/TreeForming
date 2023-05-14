{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module TreesThatGrow.Lib where
import Data.Void
import Common
import Diagrams.Prelude
import Linear
import Data.Kind

data Shapes ξ
    = Circ (XCirc ξ) (Shapes ξ)
    | Sq {ext :: XSq ξ, topl :: Shapes ξ, topr :: Shapes ξ, botl :: Shapes ξ, botr :: Shapes ξ}
    | Lf (Xl ξ)
    | Shapes (XShapes ξ)

type family XCirc ξ
type family XSq ξ
type family Xl ξ
type family XShapes ξ


data UD
type instance XCirc UD = Void
type instance XSq UD = Void
type instance Xl UD = Void
type instance XShapes UD = Void

pattern Circle c <- Circ _ c
    where Circle c = Circ undefined c


pattern Square{topl, topr, botl, botr} <- Sq {ext=_, topl=topl, topr=topr, botl=botl, botr=botr}
  where
    Square c = Sq undefined c

pattern Leaf <- Lf _
    where Leaf = Lf undefined


{-# COMPLETE Circle, Square, Leaf, Shapes #-}

class PostProcess x where
  post :: Renderable (Path V2 Double) a=>
    x -> Diag a-> Diag a

instance PostProcess Void where
  post _ = id

type Forall (φ :: Type -> Constraint) ξ
  = (φ (XCirc ξ), φ (XSq ξ), φ (Xl ξ), φ (XShapes ξ))


instance Forall PostProcess ξ => PostProcess (Shapes ξ) where
  post = drawShape

drawShape :: (Renderable (Path V2 Double) b, Forall PostProcess ξ) =>
    Shapes ξ -> Diag b -> Diag b
drawShape (Circ ext subShape) l = post ext $ subDiagram <> circle (norm (size subDiagram) / 2) # themed
  where subDiagram = drawShape subShape l
drawShape (Sq {..}) l = post ext $
    subDiagram # center <> square (maximum $ size subDiagram) # themed
  where
    V4 toplShape toprShape botlShape botrShape =
            padSubDiagsAndResize ((`drawShape` l) <$> V4 topl topr botl botr)
    subDiagram =
            (toplShape ||| toprShape)
        === (botlShape ||| botrShape)
drawShape (Lf ext) leafDiag = post ext leafDiag
drawShape (Shapes ext) l = post ext l

example :: Shapes ξ
example =
  Circle $
    Circle $
      Square
        (Circle (Circle Leaf))
        (Circle Leaf)
        Leaf
        (Circle Leaf)

example2 :: Shapes ξ
example2 = Square example example example Leaf

main = runMain (post (example2 @UD))