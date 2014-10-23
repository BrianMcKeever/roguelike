module GameState (
    addComponent,
    createEntity,
    GameState(..),
    initialGameState,
    removeComponent
)
where
import Components.PhysicsBase
import Components.RenderableBase
import Components.TransformBase
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import EntityComponentSystem
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H
import System.Random

addComponent :: Component -> GameState -> Entity -> (Entity, GameState)
addComponent component gameState (Entity serial kind components) = (entity, gameState')
    where
    entity = Entity serial kind $ Set.insert component components
    entities' = Map.insert serial entity $ entities gameState
    gameState' = gameState{entities = entities'}
    --addComponent is here because it was causing circular imports in
    --entitycomponentSystem
  
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
    physicsState :: PhysicsState,
    transformState :: TransformState, 
    randomState :: StdGen, 
    renderFunctions :: (Map.Map Serial (Float -> GameState -> Entity -> IO GameState)),
    space :: H.Space,
    tiles :: (Map.Map String Picture),
    toBeRendered :: [RenderData]}

initialGameState :: IO GameState
initialGameState = do
    space' <- H.newSpace
    return $ GameState{
        entitySerial = 0, 
        entities = Map.empty, 
        physicsState = initialPhysicsState,
        randomState = mkStdGen 1, 
        transformState = initialTransformState,
        renderFunctions = Map.empty,
        space = space',
        tiles = Map.empty,
        toBeRendered = []}

removeComponent :: Component -> GameState -> Entity -> (Entity, GameState)
removeComponent component gameState (Entity serial kind components) = (entity', gameState')
    where
    entity' = Entity serial kind $ Set.delete component components
    entities' = Map.insert serial entity' $ entities gameState
    gameState' = gameState{entities = entities'}
