module EntityComponentSystem (
    ComponentMask,
    createMask,
    Entity,
    initialMasks,
    maskContains,
    Mask,
    Masks,
    maxEntities
)
where
import Data.Bits
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Vector.Unboxed as Vector

data ComponentMask = EmptyMask | PhysicsMask | RenderableMask | SimpleMovementMask 
    deriving (Eq)

instance Enum ComponentMask where
    fromEnum = fromJust . flip lookup componentMaskTable
    toEnum = fromJust . flip lookup (map swap componentMaskTable)

componentMaskTable :: [(ComponentMask, Mask)]
componentMaskTable = [
    (EmptyMask, 0), 
    (PhysicsMask, shift 1 1), 
    (RenderableMask, shift 1 2), 
    (SimpleMovementMask, shift 1 3)
    ]

createMask :: [ComponentMask] -> Mask
createMask = foldl' (\m c -> fromEnum c .|. m) 0

type Entity = Int

initialMasks :: Masks
initialMasks = Vector.replicate maxEntities $ fromEnum EmptyMask

type Mask = Int

--If the first argument contains the second argument, true. Otherwise, false
maskContains :: Mask -> Mask -> Bool
maskContains a b = a .&. b == b

type Masks = Vector.Vector Mask

maxEntities :: Int
maxEntities = 3000
