module GameState (
    GameState(..),
    initialGameState,
    updateFunctions
)
where
import Components.PositionBase
import Components.Renderable
import qualified Data.Map as Map
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random

data GameState = GameState {
    entitySerial :: Int, 
    entities :: (Map.Map Serial Entity), 
    positionState :: PositionState, 
    randomState :: StdGen, 
    renderFunctions :: (Map.Map Serial (Float -> GameState -> Entity -> GameState)),
    tiles :: (Map.Map String Picture),
    toBeRendered :: [Picture]}

initialGameState :: GameState
initialGameState = GameState{
    entitySerial = 0, 
    entities = Map.empty, 
    randomState = mkStdGen 0, 
    positionState = initialPositionState,
    renderFunctions = Map.empty,
    tiles = Map.empty,
    toBeRendered =  []}

updateFunctions :: Map.Map Component (Float -> GameState -> Entity -> GameState)
updateFunctions = Map.fromList  [
    (positionComponent, pass),
    (renderableComponent, pass)
    ]
    where
    pass _ gameState _ = gameState
-- I decided to store my update functions in this dictionary instead of each
-- component because storing them in each component would cause circular imports
