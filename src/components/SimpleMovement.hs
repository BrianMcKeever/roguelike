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
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.Maybe
import EntityComponentSystem
import GameState
import GHC.Float
import qualified Physics.Hipmunk as H

-- SimpleMovement moves the entity in a straight line towards the destination
-- and stops if it arrives or it collides with something.

addSimpleMovement :: Entity -> GameState Entity
addSimpleMovement = addComponent simpleMovementComponent

deceleration :: Double
deceleration = 10
--lower decelerates slower over a greater period of time
--don't do 0

distance :: H.Position -> H.Position -> H.Distance
distance position1 position2 = H.len $ position1 - position2

getDestination :: GameData -> Entity -> Destination
getDestination gameData (Entity serial _ _) = destination
    where
    maybeDestination = Map.lookup serial $ simpleMovementState gameData
    destination = fromMaybe Nowhere maybeDestination

maxSpeed :: Double
maxSpeed = 400

setDestination :: Destination -> Entity -> GameState ()
setDestination destination (Entity serial _ _) = do
    gameData <- get
    let movementState = Map.insert serial destination $ simpleMovementState gameData
    put gameData {simpleMovementState = movementState}

updateSimpleMovement :: Float -> Entity -> GameState ()
updateSimpleMovement tick entity = do
--This is more or less the "Arrive" steering behavior from Programming Game AI
--by Example. 
    gameData <- get
    let destination = getDestination gameData entity
    if destination == Nowhere 
    then return () 
    else do
        position <- getPosition entity
        let goal = case destination of
                (Location location) -> location
                Nowhere -> error "should be impossible"
        let distance' = distance position goal 
        if distance' <= 0.1
        then setDestination Nowhere entity
        else do
            --velocity <- getVelocity gameData entity
            let speed' = distance' * deceleration
            let speed'' = min speed' maxSpeed 
            let velocity' = H.scale (position - goal) $ float2Double tick *(-speed'')/distance'
            setVelocity velocity' entity