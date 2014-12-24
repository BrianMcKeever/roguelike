module EntityComponentSystem (
    Component(..),
    componentToMask,
    createMask,
    Entity,
    hasMask,
    initialMasks,
    maskHas,
    Mask,
    Masks,
    maxEntities,
    trace,
    unnessary
)
where
import Data.Bits
import Data.List
import Data.Word
import qualified Data.Vector as Vector
import qualified Debug.Trace as Debug

data Component = EmptyComponent 
    | PhysicsComponent 
    | RenderableComponent 
    | SimpleMovementComponent
    deriving (Enum, Eq, Show)

componentToMask :: Component -> Mask
componentToMask component = if enum == 0 then 0 else shift 1 enum
    where
    enum = fromEnum component

createMask :: [Component] -> Mask
createMask = foldl' (\m c -> componentToMask c .|. m) 0

type Entity = Word16

--If the first mask contains the second mask, true. Otherwise, false
hasMask :: Mask -> Mask -> Bool
hasMask a b = a .&. b == b

initialMasks :: Masks
initialMasks = Vector.replicate maxEntities $ componentToMask EmptyComponent

type Mask = Word32

--this is hasMask flipped.
maskHas :: Mask -> Mask -> Bool
maskHas = flip hasMask

type Masks = Vector.Vector Mask

maxEntities :: Int
maxEntities = 3000

trace :: (Show a) => String -> a -> a
trace s a = Debug.trace (s ++ show a) a

unnessary :: a
unnessary = error "an argument that was thought unecessary was evaluated"
