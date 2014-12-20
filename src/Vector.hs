module Vector (
    concatVector,
    removeDuplicates
)
where
import Data.Vector as Vector
import qualified Data.Set as Set
import Prelude hiding (concat)

--TODO: both of these could be made faster

concatVector :: Vector (Vector a) -> Vector a
concatVector v = concat $ toList v

removeDuplicates :: (Ord a) => Vector a -> Vector a
removeDuplicates v = fromList $ Set.toList $ Set.fromList $ toList v
