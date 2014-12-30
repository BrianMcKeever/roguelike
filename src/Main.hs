import Game
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import Miscellaneous

main :: IO ()
main = do
    gameData <- bouncingCirclesInitialData --initialGameData
    play (InWindow windowTitle (displayWidth, displayHeight) (50, 50)) white 30 gameData draw handleInput update

windowTitle :: String
windowTitle = "Rogue Bard"
