module Tagless.Lib (Shapes(..), example2) where
import Common
import Diagrams.Prelude hiding (square, circle, apply)
import qualified Diagrams.Prelude as D
import Linear

class Shapes repr where
    square :: (Renderable (Path V2 Double) a) => V4 (repr (Diag a)) -> repr (Diag a)
    circle :: (Renderable (Path V2 Double) a) => repr (Diag a) -> repr (Diag a)
    leaf :: Diag a -> repr (Diag a)

instance Shapes Identity where
    square subDiagrams = Identity $
        insideDiagram # center <> D.square (maximum $ size insideDiagram) # themed
        where
            V4 topl topr botl botr = padSubDiagsAndResize (runIdentity <$> subDiagrams)
            insideDiagram =
                    (topl ||| topr)
                === (botl ||| botr)
    circle subDiagram = Identity $ runIdentity subDiagram <> D.circle (norm (size $ runIdentity subDiagram) /2) # themed
    leaf = Identity


example2 :: (Shapes repr, Renderable (Path V2 Double) a) =>
    Diag a -> repr (Diag a)
example2 leafDiag = square $ V4
    (circle example) l
    l                (circle l)
  where
    l = leaf leafDiag
    example = square $ V4
        l                   (circle l)
        (circle $ circle l) l

main :: IO ()
main = runMain (runIdentity . example2)
