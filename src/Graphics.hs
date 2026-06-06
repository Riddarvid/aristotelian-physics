{-# LANGUAGE LambdaCase #-}
module Graphics (
    renderGrid
) where
import Types (Grid (Grid), Cell (..), Element (..))
import Graphics.Gloss (Picture, pictures, rectangleSolid, translate, color, Color, makeColor, mixColors)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

renderGrid :: (Fractional a, Real a) => Grid a -> Picture
renderGrid (Grid columns) = pictures $ zipWith renderColumns [0 ..] columns

renderColumns :: (Fractional a, Real a) => Int -> [Cell a] -> Picture
renderColumns x column = pictures $ zipWith (renderCell x) [0 ..] column

renderCell :: (Fractional a, Real a) => Int -> Int -> Cell a -> Picture
renderCell x y cell = color (mixCellColor cell)
    $ translate (fromIntegral x) (fromIntegral y)
    $ rectangleSolid 1 1

mixCellColor :: (Fractional a, Real a) => Cell a -> Color
mixCellColor cell = mixColorList colorIntensities
    where
        colorIntensities = NE.fromList [
            (cEarthPart cell, elementColor Earth), 
            (cWaterPart cell, elementColor Water),
            (cAirPart cell, elementColor Air),
            (cFirePart cell, elementColor Fire)
            ]

-- The first parameter represents how large of a portion the color should have in the final image
-- I think a reasonable way of doing this is mixing the first two colors using the mix function,
-- and then recording how big a part this new color should play by recording the average.
mixColorList :: (Fractional a, Real a) => NonEmpty (a, Color) -> Color
mixColorList ((intensity, c) :| ics) = case ics of
    [] -> c
    ic' : ics' -> mixColorList (mixTwoColors (intensity, c) ic' :| ics')

mixTwoColors :: (Fractional a, Real a) => (a, Color) -> (a, Color) -> (a, Color)
mixTwoColors (i1, c1) (i2, c2) = (i, c)
    where
        c = mixColors (realToFrac i1) (realToFrac i2) c1 c2
        i = (i1 + i2) / 2

elementColor :: Element -> Color
elementColor = \case
    Earth -> makeColor 0.4 0.3 0.2 1
    Water -> makeColor 0 0 1 1
    Air -> makeColor 0.51 0.86 1 1
    Fire -> makeColor 0.85 0.83 0.1 1
