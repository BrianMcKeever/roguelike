module EntityComponentSystem (
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

data Component = Component 
    Float                             --Priority
    Atom                              --Name

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

data Entity = Entity Serial Kind Components 

type Kind = Atom
type Serial = Integer

componentFoldl :: (a -> Component -> a) -> a -> Components -> a
componentFoldl = Set.foldl'

componentsToList :: Components -> [Component]
componentsToList = Set.toAscList

emptyComponents :: Components
emptyComponents = Set.empty

hasComponent :: Entity -> Component -> Bool
hasComponent (Entity _ _ components) component = Set.member component components
