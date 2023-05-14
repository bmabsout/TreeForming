{-# OPTIONS_GHC -Wno-orphans #-}
module Tagless.WithDepthAndPoly where
import Tagless.WithDepth (WithDepth(..), depth)
import Tagless.WithPoly (WithPoly(..), example3)
import Common
import Data.Function
import Data.Functor.Identity

instance WithPoly WithDepth where
    poly subDiagrams = WithDepth (\d -> Identity . depth (d+1) <$> subDiagrams & poly & runIdentity & depthToTheme d)


main = runMain (depth 0 . example3)