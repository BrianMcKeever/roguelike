module EntityComponentSystem (
    Component, 
    Components, 
    createEntity, 
    Entity
)
where
import qualified Data.HashSet as Set
import Control.Monad.State.Lazy

data Component = Component String (Maybe Entity -> Entity)
type Components = Set.HashSet Component

data Entity = Entity Int Components 
type EntityManagement = State Int

generateEntityId :: EntityManagement Int
generateEntityId = do
    n <- get
    put (n+1)
    return n

createEntity :: EntityManagement Entity
createEntity = do
    serial <- generateEntityId
    return $ Entity serial Set.empty
