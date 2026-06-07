module Types (
    Element(..),
    Cell(..),
    Grid(..)
) where

-- Grid represents a list of columns, not a list of rows
newtype Grid a b = Grid [[Cell a b]] 
    deriving (Show)

data Cell a b = Cell {
    cEarthPart :: a,
    cWaterPart :: a,
    cAirPart :: a,
    cFirePart :: a,
    cWaitTime :: b
} 
    deriving (Show) 

data Element = Earth | Water | Air | Fire