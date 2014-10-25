module EntityComponentSystem (
    Component(..), 
    componentFoldl,
    componentFoldM,
    componentsToList,
    Components, 
    emptyComponents,
    Entity(..),
    getComponents,
    getSerial,
    hasComponent,
    Kind,
    Serial
)
where
import Control.Monad
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

getComponents :: Entity -> Components
getComponents (Entity _ _ components) = components

getSerial :: Entity -> Serial
getSerial (Entity serial _ _) = serial

type Kind = Atom
type Serial = Integer

componentFoldl :: (a -> Component -> a) -> a -> Components -> a
componentFoldl = Set.foldl'

componentFoldM :: Monad m => (a -> Component -> m a) -> a -> Components -> m a
componentFoldM f initial components = foldM f initial $ Set.toList components

componentsToList :: Components -> [Component]
componentsToList = Set.toAscList

emptyComponents :: Components
emptyComponents = Set.empty

hasComponent :: Entity -> Component -> Bool
hasComponent (Entity _ _ components) component = Set.member component components
