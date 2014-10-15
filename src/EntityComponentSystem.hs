module EntityComponentSystem (
    Component(..), 
    componentFoldl,
    componentInsert,
    componentsToList,
    Components, 
    emptyComponents,
    Entity(..),
    Serial
)
where
import qualified Data.Set as Set

data Component = Component 
    Float                             --Priority
    String                            --Name

componentFoldl :: (a -> Component -> a) -> a -> Components -> a
componentFoldl = Set.foldl'

componentInsert :: Component -> Components -> Components
componentInsert component components = Set.insert component components

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

data Entity = Entity Serial Components 

type Serial = Int
