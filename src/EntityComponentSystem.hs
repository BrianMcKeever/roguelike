module EntityComponentSystem (
    Component(..), 
    componentInsert,
    Components, 
    componentsFromList,
    Entity(..),
    Serial
)
where
import qualified Data.Set as Set

data Component = Component 
    Float                             --Priority
    String                            --Name

componentInsert :: Component -> Components -> Components
componentInsert component components = Set.insert component components

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

componentsFromList :: [Component] -> Components
componentsFromList = Set.fromList

data Entity = Entity Serial Components 

type Serial = Int
