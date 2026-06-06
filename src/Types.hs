module Types (
    Element(..),
    Cell(..),
    Grid(..)
) where

-- Grid represents a list of columns, not a list of rows
newtype Grid a = Grid [[Cell a]] 
    deriving (Show)

data Cell a = Cell {
    cEarthPart :: a,
    cWaterPart :: a,
    cAirPart :: a,
    cFirePart :: a
} 
    deriving (Show) 

data Element = Earth | Water | Air | Fire