module Generate (
    generateGrid
) where

import Types (Grid (Grid), Cell (Cell, cEarthPart, cWaterPart, cAirPart, cFirePart, cWaitTime))
import System.Random (RandomGen)
import Control.Monad (replicateM)
import System.Random.Stateful (runStateGen_, UniformRange (uniformRM), StatefulGen)

generateGrid :: (RandomGen g, UniformRange a, Fractional a, Num b) => Int -> Int -> g -> Grid a b
generateGrid width height g = runStateGen_ g (gridGenerator width height)

gridGenerator :: (StatefulGen g m, UniformRange a, Fractional a, Num b) => Int -> Int -> g -> m (Grid a b)
gridGenerator width height = (Grid <$>) . replicateM width . replicateM height . cellGenerator

cellGenerator :: (StatefulGen g m, UniformRange a, Fractional a, Num b) => g -> m (Cell a b)
cellGenerator g = do
    earthVal <- uniformRM (0, 1) g
    waterVal <- uniformRM (0, 1) g
    airVal <- uniformRM (0, 1) g
    fireVal <- uniformRM (0, 1) g
    let elementSum = earthVal + waterVal + airVal + fireVal
    return $ Cell {
        cEarthPart = earthVal / elementSum,
        cWaterPart = waterVal / elementSum,
        cAirPart = airVal / elementSum,
        cFirePart = fireVal / elementSum,
        cWaitTime = 0
    }
