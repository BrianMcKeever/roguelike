module EntityComponentSystem (
    Component, 
    ComponentData,
    createComponent,
    Entity,
    initialComponentData
)
where
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import StringTable.Atom

type Component = Atom    

type ComponentData = Map.Map Entity (Set.Set Component)

createComponent :: String -> Component
createComponent = toAtom

type Entity = Integer

initialComponentData :: ComponentData
initialComponentData = Map.empty
