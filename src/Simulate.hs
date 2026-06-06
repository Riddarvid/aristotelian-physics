module Simulate (stepGrid) where
import Types (Grid)

stepGrid :: b -> Grid a -> Grid a
stepGrid _ = id