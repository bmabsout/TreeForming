{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module TreesThatGrow.WithDepth where
import TreesThatGrow.Lib hiding (main)
import Common
import Data.Void

type Depth = Word
data WithDepth e
type instance XCirc (WithDepth e) = (Depth, XCirc e)
type instance XSq (WithDepth e) = (Depth, XSq e)
type instance Xl (WithDepth e) = (Depth, Xl e)
type instance XShapes (WithDepth e) = (Depth, XShapes e)


class Depther i o where
    depther :: Depth -> i -> Shapes o

instance Depther Void (WithDepth UD) where
    depther = undefined

instance (Depther (XShapes ξ) (WithDepth ξ)) => Depther (Shapes ξ) (WithDepth ξ) where
    depther depth (Lf x) = Lf (depth, x)
    depther depth (Circ e c) = Circ (depth, e) (depther (depth+1) c)
    depther depth (Sq e tr tl br bl) = Sq (depth, e) rtr rtl rbr rbl
      where
        rtr = depther (depth + 1)  tr
        rtl = depther (depth + 1)  tl
        rbr = depther (depth + 1)  br
        rbl = depther (depth + 1)  bl
    depther depth (Shapes e) = depther depth e


instance PostProcess e => PostProcess (Depth, e) where
    post (d, e) diag = depthToTheme d (post e diag)

ex :: Shapes UD
ex = example2

depthed :: Shapes (WithDepth UD)
depthed = depther 0 ex

main = runMain (post depthed)