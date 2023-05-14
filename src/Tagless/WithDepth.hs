module Tagless.WithDepth where
import Tagless.Lib
import Data.Function
import Diagrams hiding (circle, square)
import Common
import Data.Functor.Identity

newtype WithDepth a = WithDepth (Int -> a)
depth :: Int -> WithDepth a -> a
depth d (WithDepth f) = f d

instance Shapes WithDepth where
    square subDiagrams = WithDepth (\d -> depthToTheme d  $ runIdentity $ square $ Identity . depth (d + 1) <$> subDiagrams)
    circle subDiagram = WithDepth (\d -> subDiagram & depth (d+1) & Identity & circle & runIdentity & depthToTheme d)
    leaf diag = WithDepth $ \d -> diag # depthToTheme d

main :: IO ()
main = runMain (depth 0 . example2)