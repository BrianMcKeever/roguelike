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
    onlyMap
)
where
import Data.Bits
import Data.List
import Data.Word
import qualified Data.Vector as Vector

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

--If the first argument contains the second argument, true. Otherwise, false
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

--This function maps over the zipped vector. If the fst part of the tuple passes
--the predicate, the function is applied to the snd part. Otherwise, the snd
--part is unchanged.
onlyMap :: (a -> Bool) -> 
    (b -> b) -> 
    Vector.Vector (a, b) -> 
    Vector.Vector b
onlyMap p f xs = Vector.map pf xs
    where
    pf (a, b) = if p a then f b else b 
