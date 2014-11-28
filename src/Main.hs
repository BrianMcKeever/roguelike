--import qualified Data.Map as Map
--import Data.Maybe
--import qualified Data.Set as Set
--import EntityComponentSystem
--import GHC.Float
import GameState
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game

draw :: GameData -> Picture
draw _ = undefined

handleInput :: Event -> GameData -> GameData
handleInput _ gameData = gameData

main :: IO ()
main = do
    play (InWindow windowTitle (windowWidth, windowHeight) (50, 50)) white 30 initialGameData draw handleInput update

update :: Float -> GameData -> GameData
update _ gameData = gameData

windowTitle :: String
windowTitle = "Rogue Bard"

windowWidth :: Int
windowWidth = 704

windowHeight :: Int
windowHeight = 704
