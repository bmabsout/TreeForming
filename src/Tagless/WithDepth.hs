module Tagless.WithDepth where
import Tagless.Lib
import Data.Function
import Diagrams hiding (circle, square)
import Debug.Trace (traceShowId)
import Common
import Data.Functor.Identity

newtype WithDepth a = WithDepth (Int -> a)
depth :: Int -> WithDepth a -> a
depth d (WithDepth f) = f d

instance Shapes WithDepth where
    square subDiagrams = WithDepth (\d -> runIdentity $ square $ Identity . depthToTheme (traceShowId d+1) . depth (d+1) <$> subDiagrams )
    circle subDiagram = WithDepth (\d -> subDiagram & depth (d+1) & Identity & circle & runIdentity & depthToTheme (d+1))
    leaf diag = WithDepth $ \d -> diag # depthToTheme (traceShowId d)

example2Diag :: Renderable (Path V2 Double) a => Diag a -> Diag a
example2Diag leafDiag = depth 0 (example2 leafDiag)

main :: IO ()
main = runMain example2Diag