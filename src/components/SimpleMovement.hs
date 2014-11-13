module Components.SimpleMovement (
    addSimpleMovement,
    Destination(..),
    getDestination,
    setDestination,
    updateSimpleMovement
)
where
import Components.SimpleMovementBase
import Components.TransformAndPhysics
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

addSimpleMovement :: Entity -> GameState ()
addSimpleMovement entity = do
    genericAddComponent simpleMovementComponent entity
    setDestination Nowhere entity

deceleration :: Double
deceleration = 100
--lower decelerates slower over a greater period of time
--don't do 0

distance :: H.Position -> H.Position -> H.Distance
distance position1 position2 = H.len $ position1 - position2

getDestination :: GameData -> Entity -> Destination
getDestination gameData entity = destination
    where
    maybeDestination = Map.lookup entity $ simpleMovementState gameData
    destination = fromMaybe Nowhere maybeDestination

maxSpeed :: Double
maxSpeed = 6000

removeSimpleMovement :: Entity -> GameState ()
removeSimpleMovement entity = do
    genericRemoveComponent simpleMovementComponent entity
    gameData <- get
    let state' = Map.delete entity $ simpleMovementState gameData

    put gameData{ simpleMovementState = state'}

setDestination :: Destination -> Entity -> GameState ()
setDestination destination entity = do
    gameData <- get
    let movementState = Map.insert entity destination $ simpleMovementState gameData
    put gameData {simpleMovementState = movementState}

simpleMovementComponent :: Component
simpleMovementComponent = Component{
    hasComponent = genericHasComponent simpleMovementComponent,
    nameComponent = createComponentName "simpleMovement",
    removeComponent = removeSimpleMovement
    }

updateEntityMovement :: Float -> Entity -> GameState()
updateEntityMovement tick entity = do
--This is more or less the "Arrive" steering behavior from Programming Game AI
--by Example. 
    gameData <- get
    let destination = getDestination gameData entity
    if destination == Nowhere 
    then return () 
    else do
        position <- liftIO $ getPosition entity gameData
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
            let velocity' = H.scale (position - goal) $ float2Double tick * (-speed'')/distance'
            setVelocity velocity' entity

updateSimpleMovement :: Float -> [Entity] -> GameState ()
updateSimpleMovement tick entities' = 
-- We could to better by implementing this as a merge like in merge sort
    foldState (updateEntityMovement tick) entities'
