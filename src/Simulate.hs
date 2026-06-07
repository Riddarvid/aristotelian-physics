{-# LANGUAGE LambdaCase #-}
module Simulate (stepGrid) where
import Types (Grid (Grid), Cell (..), Element (..))

stepGrid :: (Real b, Fractional a, Ord a) => b -> Grid a b -> Grid a b
stepGrid = naturalMovementGrid

naturalMovementGrid :: (Real b, Fractional a, Ord a) => b -> Grid a b -> Grid a b
naturalMovementGrid deltaTime (Grid columns) = Grid $ map (naturalMovementColumn deltaTime) columns

naturalMovementColumn :: (Real b, Fractional a, Ord a) => b -> [Cell a b] -> [Cell a b]
naturalMovementColumn deltaTime = go
    where
        go (cTop : cBottom : cs)
            | shouldCellsSwap cTop cBottom = resetWaitTime cBottom : go (resetWaitTime cTop : cs)
            | otherwise = incWaitTime cTop : go (incWaitTime cBottom : cs)
        go cs = cs
        resetWaitTime c = c{cWaitTime = 0}
        incWaitTime c = c{cWaitTime = cWaitTime c + deltaTime}

shouldCellsSwap :: (Fractional a, Ord a, Real b) => Cell a b -> Cell a b -> Bool
shouldCellsSwap cTop cBottom
    | bottomWeight > topWeight = False
    | minWaitTime == 0 = False
    | otherwise = speed > 1 / realToFrac minWaitTime
    where
        speed = topWeight / bottomWeight
        minWaitTime = min (cWaitTime cTop) (cWaitTime cBottom)
        topWeight = cellWeight cTop
        bottomWeight = cellWeight cBottom

elementDensity :: Num a => Element -> a
elementDensity = \case
    Earth -> 4
    Water -> 3
    Air -> 2
    Fire -> 1

cellWeight :: Num a => Cell a b -> a
cellWeight cell = 
    cEarthPart cell * elementDensity Earth +
    cWaterPart cell * elementDensity Water +
    cAirPart cell * elementDensity Air +
    cFirePart cell * elementDensity Fire