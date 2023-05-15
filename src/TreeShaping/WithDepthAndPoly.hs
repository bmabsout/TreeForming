module TreeShaping.WithDepthAndPoly where
import TreeShaping.WithDepth
import TreeShaping.WithPoly hiding (main)
import Common ( runMain )

fromSlide = Poly
    [Leaf, Leaf, Leaf, Circle Leaf,
      Circle
        (Square
          Leaf (Circle Leaf)
          Leaf Leaf
        )
    ]


main = runMain (drawWithDepth drawWithPolyF (addDepth fromSlide 0))