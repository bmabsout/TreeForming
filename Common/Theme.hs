module Common.Theme where
import Data.Colour
import Data.Colour.SRGB (toSRGB, sRGB24read)

data Theme a = Theme {
    base03:: a,
    base02:: a,
    base01:: a,
    base00:: a,
    base0:: a,
    base1:: a,
    base2:: a,
    base3:: a,
    yellow:: a,
    orange:: a,
    red:: a,
    magenta:: a,
    violet:: a,
    blue:: a,
    cyan:: a,
    green:: a
}
    deriving (Functor, Eq, Show)


solarized :: Theme String
solarized = Theme {
    base03 = "#002b36",
    base02 = "#073642",
    base01 = "#586e75",
    base00 = "#657b83",
    base0 = "#839496",
    base1 = "#93a1a1",
    base2 = "#eee8d5",
    base3 = "#fdf6e3",
    yellow = "#b58900",
    orange = "#cb4b16",
    red = "#dc322f",
    magenta = "#d33682",
    violet = "#6c71c4",
    blue = "#268bd2",
    cyan = "#2aa198",
    green = "#859900"
}

color :: Theme (Colour Double)
color = fmap sRGB24read solarized
