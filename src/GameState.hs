module GameState (
    addComponent,
    createEntity,
    GameState(..),
    initialGameState,
    updateFunctions
)
where
import Components.PositionBase
import Components.RenderableBase
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random

addComponent :: GameState -> Entity -> Component -> (Entity, GameState)
addComponent gameState (Entity serial kind components) component = (entity, gameState')
    where
    entity = Entity serial kind $ Set.insert component components
    entities' = Map.insert serial entity $ entities gameState
    gameState' = gameState{entities = entities'}
  
createEntity :: GameState -> Kind -> (Entity, GameState)
createEntity gameState kind = (entity, gameState') 
    where
    serial = entitySerial gameState
    entity = Entity serial kind emptyComponents
    entities' = Map.insert serial entity $ entities gameState
    gameState' = gameState {entitySerial = serial + 1, entities = entities'}

data GameState = GameState {
    entitySerial :: Integer, 
    entities :: (Map.Map Serial Entity), 
    positionState :: PositionState, 
    randomState :: StdGen, 
    renderFunctions :: (Map.Map Serial (Float -> GameState -> Entity -> GameState)),
    tiles :: (Map.Map String Picture),
    toBeRendered :: [RenderData]}

initialGameState :: GameState
initialGameState = GameState{
    entitySerial = 0, 
    entities = Map.empty, 
    randomState = mkStdGen 0, 
    positionState = initialPositionState,
    renderFunctions = Map.empty,
    tiles = Map.empty,
    toBeRendered = []}

updateFunctions :: Map.Map Component (Float -> GameState -> Entity -> GameState)
updateFunctions = Map.fromList  [
    (positionComponent, pass),
    (renderableComponent, pass)
    ]
    where
    pass _ gameState _ = gameState
-- I decided to store my update functions in this dictionary instead of each
-- component because storing them in each component would cause circular imports
