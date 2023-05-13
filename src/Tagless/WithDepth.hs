{-# LANGUAGE OverloadedRecordDot #-}

module Tagless.WithDepth where
import Tagless.Lib
import Control.Monad.Reader
import Control.Comonad.Identity
import NormalADT.Lib (depthToTheme)
import Data.Function
import Diagrams.Backend.SVG
import Diagrams hiding (circle, square)
import Debug.Trace (traceShowId)
import Common
import Data.Colour

newtype WithDepth a = WithDepth (Int -> a)
depth d (WithDepth f) = f d

instance Shapes WithDepth where
    square subDiagrams = WithDepth (\d -> runIdentity $ square $ Identity . depthToTheme (traceShowId d+1) . depth (d+1) <$> subDiagrams )
    circle subDiagram = WithDepth (\d -> subDiagram & depth (d+1) & Identity & circle & runIdentity & depthToTheme (d+1))
    leaf diag = WithDepth $ \d -> diag # depthToTheme (traceShowId d)

example2Diag leafDiag = depth 0 (example2 leafDiag)

main :: IO ()
main = do
  leaf <- loadImageSVG "leaf.png"
  -- let diag =  frame 100 $ NormalADT.Lib.example2Diag leaf
  -- let diag =  frame 100 $ NormalADT.WithDepth.example2Diag leaf
  let diag =  frame 100 $ example2Diag leaf
  -- let diag =  frame 100 $ Typeclasses.Lib.example2Diag leaf
  renderSVG "diagram.svg" (mkSizeSpec (V2 (Just 400) Nothing)) diag