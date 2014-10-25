module Components.SimpleMovement (
    addSimpleMovement,
    Destination(..),
    getDestination,
    setDestination,
    simpleMovementComponent,
    updateSimpleMovement
)
where
import Components.Physics
import Components.SimpleMovementBase
import Components.Transform
--import Components.Transform
import qualified Data.Map.Lazy as Map
import Data.Maybe
import EntityComponentSystem
import GameState
import GHC.Float
import qualified Physics.Hipmunk as H

-- SimpleMovement moves the entity in a straight line towards the destination
-- and stops if it arrives or it collides with something.

addSimpleMovement :: GameState -> Entity -> (Entity, GameState)
addSimpleMovement = addComponent simpleMovementComponent

deceleration :: Double
deceleration = 10
--lower decelerates slower over a greater period of time
--don't do 0

distance :: H.Position -> H.Position -> H.Distance
distance position1 position2 = H.len $ position1 - position2

getDestination :: GameState -> Entity -> Destination
getDestination gameState (Entity serial _ _) = destination
    where
    maybeDestination = Map.lookup serial $ simpleMovementState gameState
    destination = fromMaybe Nowhere maybeDestination

maxSpeed :: Double
maxSpeed = 400

setDestination :: Destination -> GameState -> Entity -> GameState
setDestination destination gameState (Entity serial _ _) = gameState {simpleMovementState = state}
    where
    state = Map.insert serial destination $ simpleMovementState gameState

updateSimpleMovement :: Float -> GameState -> Entity -> IO GameState
updateSimpleMovement tick gameState entity = do
--This is more or less the "Arrive" steering behavior from Programming Game AI
--by Example. 
    let destination = getDestination gameState entity
    if destination == Nowhere 
    then return gameState 
    else do
        position <- getPosition gameState entity
        let goal = case destination of
                (Location location) -> location
                Nowhere -> error "should be impossible"
        let distance' = distance position goal 
        if distance' <= 0.1
        then return $ setDestination Nowhere gameState entity
        else do
            --velocity <- getVelocity gameState entity
            let speed' = distance' * deceleration
            let speed'' = min speed' maxSpeed 
            let velocity' = H.scale (position - goal) $ (float2Double tick)*(-speed'')/distance'
            setVelocity velocity' gameState entity
            return gameState
