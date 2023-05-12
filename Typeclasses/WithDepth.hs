{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
import Diagrams
import Typeclasses.Lib
import Diagrams.Prelude
import Linear

data WithDepth a = WithDepth Int a


instance (Drawable a) => Drawable (WithDepth a) where
    draw leaf (WithDepth depth shape) = draw leaf shape # depthToTheme depth

class GiveDepth a where
    withDepth :: Int -> a -> WithDepth a

instance GiveDepth (Circle a) where
    withDepth depth (Circle subStuff) = Circle (WithDepth depth (withDepth (depth+1) subStuff))

instance GiveDepth Leaf where
    withDepth depth Leaf = WithDepth depth Leaf

-- instance GiveDepth (Square a b c d)

-- shapeToDiagramWithDepth :: forall a. Renderable (Path V2 Double) a => Diag a -> Shapes -> Diag a
-- shapeToDiagramWithDepth leafDiagram = recurse 0
--   where
--     recurse :: Int -> Shapes -> Diag a
--     recurse depth = \case
--       Square {..} ->
--         let V4 toplShape toprShape botlShape botrShape =
--               padSubDiagsAndResize (recurse (depth + 1) <$> V4 topl topr botl botr)
--             subDiagram =
--               (toplShape ||| toprShape)
--                 === (botlShape ||| botrShape)
--         in subDiagram # center <> square (maximum $ size subDiagram) # depthToTheme depth
--       Circle subshape ->
--         let subDiagram = recurse (depth + 1) subshape
--         in subDiagram <> circle (norm (size subDiagram) / 2) # depthToTheme depth
--       Leaf -> leafDiagram


-- example2Diag :: _ => Diag a -> Diag a
-- example2Diag leafDiag = shapeToDiagramWithDepth leafDiag example2