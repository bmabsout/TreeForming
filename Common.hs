{-# LANGUAGE OverloadedRecordDot #-}

module Common (module Common, Theme.color) where
import Diagrams
import Diagrams.Prelude
import qualified Common.Theme as Theme

type Diag a = QDiagram a V2 Double Any

padSubDiagsAndResize :: (Functor f, Foldable f) => f (Diag a) -> f (Diag a)
padSubDiagsAndResize l = setDiagToMaxSize <$> diagrams
  where
    n = maximum (norm . size <$> diagrams)
    setDiagToMaxSize = sized (dims (V2 n n))
    diagrams = themed . pad 1.2 <$> l


themed :: Diag a -> Diag a
themed = lc Theme.color.base03 . fc Theme.color.base3

depthToTheme :: Integral n => n -> Diag a -> Diag a
depthToTheme level = depthColor . lc Theme.color.base03
  where
    normalizedDepth = (5 / (5 + fromIntegral level)) ** 5
    depthColor = fc (blend normalizedDepth Theme.color.base3 Theme.color.base03)