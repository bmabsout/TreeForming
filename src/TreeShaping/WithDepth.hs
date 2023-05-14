module TreeShaping.WithDepth(addCoDepth, addDepth, drawWithDepth) where
import Common
import TreeShaping.Lib
import Data.Functor.Foldable hiding (fold)
import Data.Semigroup
import Control.Comonad.Cofree
import Control.Comonad
import qualified Control.Comonad.Trans.Cofree as F


addCoDepth :: (Foldable (Base t), Recursive t) =>
    t -> Cofree (Base t) (Max Word)
addCoDepth = cata \c -> 1 + foldMap extract c :< c

addDepth :: (Recursive t) => t -> Word -> Cofree (Base t) Word
addDepth = cata \s w -> w :< (($ w+1) <$> s)

drawWithDepth :: (Integral n, Functor f) =>
    (Diag a -> f (Diag a) -> Diag a) -> Cofree f n -> Diag a -> Diag a
drawWithDepth diagAlg withDepth leaf =
    cata (\(d F.:< w) -> depthToTheme d (diagAlg leaf w)) withDepth


main :: IO ()
main = runMain (drawWithDepth drawShapeF (addDepth example2 0))
-- main = runMain (drawWithDepth drawShapeF (getMax <$> addCoDepth example2))