module LibMain (main) where
import Generate (generateGrid)
import System.Random (newStdGen)
import Graphics.Gloss (simulate, white, Color, Display (FullScreen))
import Simulate (stepGrid)
import Graphics (renderGrid)
import Types (Grid)

display :: Display
display = FullScreen --InWindow "Test" (100, 100) (0, 0)

backgroundColor :: Color
backgroundColor = white

framesPerSecond :: Int
framesPerSecond = 60

main :: IO ()
main = do
    gen <- newStdGen
    let grid = generateGrid 2 500 gen :: Grid Double Float
    simulate display backgroundColor framesPerSecond grid renderGrid (const stepGrid)
