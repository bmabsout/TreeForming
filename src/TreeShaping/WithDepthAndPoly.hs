module TreeShaping.WithDepthAndPoly where
import TreeShaping.WithDepth
import TreeShaping.WithPoly hiding (main)
import Common ( runMain )

main = runMain (drawWithDepth drawWithPolyF (addDepth example3 0))