{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where
import Diagrams.Backend.SVG
import Diagrams.Prelude

import qualified NormalADT.Lib
import qualified NormalADT.WithDepth
import qualified NormalADT.WithPoly

import qualified Typeclasses.Lib
import qualified Typeclasses.WithDepth
import qualified Typeclasses.WithTriangle



main :: IO ()
main = do
  leaf <- loadImageSVG "leaf.png"
  -- let diag =  frame 100 $ NormalADT.Lib.example2Diag leaf
  -- let diag =  frame 100 $ NormalADT.WithDepth.example2Diag leaf
  -- let diag =  frame 100 $ NormalADT.WithPoly.example2Diag leaf
  -- let diag =  frame 100 $ Typeclasses.Lib.example2Diag leaf
  -- let diag =  frame 100 $ Typeclasses.WithDepth.example2Diag leaf
  let diag =  frame 100 $ Typeclasses.WithTriangle.example3Diag leaf
  renderSVG "diagram.svg" (mkSizeSpec (V2 (Just 400) Nothing)) diag
