module EntityComponentSystem (
    addComponent,
    Component(..), 
    componentFoldl,
    componentsToList,
    Components, 
    emptyComponents,
    Entity(..),
    hasComponent,
    Kind,
    Serial
)
where
import qualified Data.Set as Set
import StringTable.Atom

addComponent :: Entity -> Component -> Entity
addComponent (Entity serial kind components) component = Entity serial kind $ Set.insert component components

data Component = Component 
    Float                             --Priority
    Atom                              --Name

componentFoldl :: (a -> Component -> a) -> a -> Components -> a
componentFoldl = Set.foldl'

componentsToList :: Components -> [Component]
componentsToList = Set.toAscList

instance Eq Component where
    (Component priority1 name1) == (Component priority2 name2) = priority1 == priority2 && name1 == name2

instance Ord Component where
    compare (Component priority1 name1) (Component priority2 name2) 
        | priority1 > priority2 = GT 
        | priority1 < priority2 = LT 
        | name1 > name2 = GT 
        | name1 < name2 = LT 
        | otherwise = EQ

type Components = Set.Set Component

emptyComponents :: Components
emptyComponents = Set.empty

data Entity = Entity Serial Kind Components 

hasComponent :: Entity -> Component -> Bool
hasComponent (Entity _ _ components) component = Set.member component components

type Kind = Atom
type Serial = Int
