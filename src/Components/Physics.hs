{-# LANGUAGE MultiWayIf #-}
module Components.Physics (
    Collision,
    createCollision,
    EntityPhysics(..),
    Physics(..),
    physicsMask,
    physicsUpdate,
    initialPhysics,
    Shape(..)
)
where
import Control.Monad
import Control.Monad.ST
import EntityComponentSystem
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set
import Data.Vector as Vector
import qualified Data.Vector.Algorithms.AmericanFlag as AF
import Linear.Affine
import Linear.Metric hiding (project)
import Linear.V2
import Linear.Vector
import Prelude as Prelude hiding ((++), concat, filter, foldl, last, length, map, maximum, minimum, replicate, span, sum, unzip, zip) 
import Debug.Trace

newtype BucketIndexEntity = BucketIndexEntity (Int, Entity)
    deriving (Eq, Show)

instance AF.Lexicographic BucketIndexEntity where
    terminate (BucketIndexEntity (a, _)) i = AF.terminate a i
    size (BucketIndexEntity (a, _)) = AF.size a
    index i (BucketIndexEntity (a, _)) = AF.index i a

instance Ord BucketIndexEntity where
    compare (BucketIndexEntity (a1, _)) (BucketIndexEntity (a2, _)) = 
        compare a1 a2

--This function transforms a vector of (bucketIndex, Entity) into a jagged
--Vector (Vector Entity) where the index of the enclosing vector corresponds to
--the bucketIndex
bucketize :: Int -> 
    Vector (Int, Entity) -> 
    Space
bucketize gridSize xs = unfoldrN gridSize generate' (0, unwrapped)
    where
    wrapped = map BucketIndexEntity xs
    sorted = runST $ do 
        thawed <- thaw wrapped
        AF.sort thawed
        freeze thawed
    unwrapped = map (\ (BucketIndexEntity (a, b)) -> (a, b)) sorted
    generate' (a, b) = Just (next, (a + 1, remaining))
        where
        (matching, remaining) = span ((==) a . fst) b
        next = map snd matching

type Collision = (Entity, Entity) 
-- A collision between a and b. a < b

type Collisions = Set.Set Collision

createCollision :: Entity -> Entity -> Collision
createCollision a b 
    | a < b     = (a, b)
    | otherwise = (b, a)

createSpace :: Int -> 
    Int ->
    Int ->
    Int ->
    Vector (Mask, EntityPhysics) -> 
    Space
createSpace bucketWidth bucketHeight mapHeight mapWidth input = result
    where
    bucketEntity = imap f input
    f entity (mask, physics) | hasMask mask physicsMask = 
        map (\bucket -> (bucket, fromIntegral entity)) $ toBuckets bucketWidth bucketHeight mapWidth $ shape physics
                        | otherwise = empty
    result = bucketize (mapHeight * mapWidth) $ foldl' (++) empty bucketEntity

detailedCollisionCheck :: (Ord a, Floating a) => Shape a -> Shape a -> Bool
detailedCollisionCheck (Circle (P pos1) radius1) (Circle (P pos2) radius2) = 
    (radius1 + radius2)**2 > qdA pos1 pos2
detailedCollisionCheck (Circle (P (V2 x1 y1)) radius) (AABB (P (V2 x2 y2)) halfWidth halfHeight) = result
    where
    xDifference = abs(x1 - x2)
    yDifference = abs(y1 - y2)
    cornerDistanceSquared = 
        (xDifference - halfWidth)**2 + (yDifference - halfHeight) ** 2
    result = 
        if | xDifference > (halfWidth + radius) -> False
           | yDifference > (halfHeight + radius) -> False
           | xDifference <= (halfWidth) -> True
           | yDifference <= (halfHeight) -> True
           | otherwise -> cornerDistanceSquared <= radius ** 2
detailedCollisionCheck a@(AABB _ _ _) c@(Circle _ _) = detailedCollisionCheck c a
detailedCollisionCheck (AABB (P (V2 x1 y1)) halfWidth1 halfHeight1) (AABB (P (V2 x2 y2)) halfWidth2 halfHeight2) =
        (abs(x1 - x2) < (halfWidth1 + halfWidth2)) && (abs(y1 - y2) < (halfHeight1 + halfHeight2))
detailedCollisionCheck polygon@(Polygon _ _ _) circle@(Circle _ _) = result
    where
    {-
    maybeResult = do
        let circleAxis = getCircleSeparatingAxis circle polygon
        let shadow1 = project circle circleAxis
        let shadow2 = project polygon circleAxis
        overlap shadow1 shadow2
        -}
        
    --getAxis circle
    --project shapes onto axis
    --if overlap == false, we're done
    --getAxis polygon
    result = undefined
detailedCollisionCheck c@(Circle _ _) p@(Polygon _ _ _) = detailedCollisionCheck p c
detailedCollisionCheck (Polygon _ _ _) (AABB _ _ _ ) = undefined
detailedCollisionCheck a@(AABB _ _ _ ) p@(Polygon _ _ _) = detailedCollisionCheck p a
detailedCollisionCheck (Polygon _ _ _ ) p@(Polygon _ _ _) = undefined

data Edge = Edge PointIndex PointIndex 
--Edges are really non-parallel edges.
--We can't reference the point directly because this is Haskell. Instead, we are
--saving the vector indexes of the points that form the edges of our object.

edgeToAxis :: Num a => Vector (Point V2 a) -> Edge -> V2 a
edgeToAxis points (Edge i j) = perp $ (pointToV2 $ points ! i) - (pointToV2 $ points ! j) 

edgeToV2Pair :: Num a => Vector (Point V2 a) -> Edge -> (V2 a, V2 a)
edgeToV2Pair points (Edge i j) = (pointToV2 $ points ! i, pointToV2 $ points ! j)

data EntityPhysics = EntityPhysics {
    force :: V2 Double,
    isStatic :: Bool, 
    isTrigger :: Bool, 
    invertedMass :: Double,
    oldPosition :: V2 Double,
    position :: V2 Double,
    shape :: Shape Double
    }

getAABBSeparatingAxis :: Num a => Vector (V2 a)
getAABBSeparatingAxis = fromList [V2 0 1, V2 1 0]

getCenter :: Fractional a => Shape a -> Point V2 a
getCenter (Circle center _) = center
getCenter (AABB center _ _) = center
getCenter (Polygon points _ _) = sum points ^* (1 / (fromIntegral $ length points))

-- | This returns the center of the bucket the point is in.
-- >>> getCenterOfBucket 1 1 $ P (V2 5.5 0)
-- P (V2 5.5 0.5)
-- >>> getCenterOfBucket 1 1 $ P (V2 5.349 1.123)
-- P (V2 5.5 1.5)
-- >>> getCenterOfBucket 10 10 $ P (V2 5.5 0)
-- P (V2 5.0 5.0)
getCenterOfBucket :: (RealFrac a) => Int -> Int -> Point V2 a -> Point V2 a
getCenterOfBucket bucketWidth bucketHeight (P (V2 x y)) = P (V2 (startX + halfWidth) $ startY + halfHeight)
    where
    startX = fromIntegral $ floor x - (mod (floor x) bucketWidth)
    startY = fromIntegral $ floor y - (mod (floor y) bucketHeight)
    halfWidth = fromIntegral bucketWidth / 2
    halfHeight = fromIntegral bucketHeight / 2

getCircleSeparatingAxis :: (Ord a, Num a) => Shape a -> Shape a -> V2 a
getCircleSeparatingAxis (Circle center _) (Polygon points _ _) = circleAxis
    where
    (P v2) = fst $ minimumBy (\ (_, a) (_, b) -> compare a b) $ zip points $ map (qdA center) points
    circleAxis = perp v2
getCircleSeparatingAxis _ _ = error "getCircleSeparatingAxis should only be called with a circle and a polygon (in that order)"

getPolygonSeparatingAxis :: Num a => Shape a -> Vector (V2 a)
getPolygonSeparatingAxis (Polygon points edges _) = map (edgeToAxis points) edges
getPolygonSeparatingAxis _ = error "getPolygonSeparatingAxis should only be called with polygons"

initialPhysics :: Physics
initialPhysics = Physics (replicate maxEntities d) Set.empty Set.empty Set.empty empty
    where
    d = EntityPhysics (V2 0 0) False False 0 (V2 0 0) (V2 0 0) (Circle (P (V2 0 0)) 0)

-- A LineConstraint is like a stick between 2 points. The physics engine will
-- try to keep the points the same distance from each other.
-- Because Haskell can't reference the points directly, we are storing ghetto
-- references - the indexes of the points.
data LineConstraint a = LineConstraint PointIndex PointIndex a -- distance 

getOverlap :: (Num a, Ord a) => (a, a) -> (a, a) -> Maybe a
getOverlap (min1, max1) (min2, max2) = do
    --I tried to find a way to do this with less comparisons, but I failed
    when (max1 <= min2) Nothing
    when (max2 <= min1) Nothing
    if | min1 < min2 && max1 < max2 -> return $ max1 - min2
       -- 1 overlaps from left 
       | min2 < min1 && max2 < max1 -> return $ max2 - min1
       --2 overlaps from left 
       | min1 <= min2 && max2 <= max1 -> return $ min2 + max2 + (min (min2 - min1) $ max1 - max2)
       --2 is contained by 1 
       | otherwise -> return $ min1 + max1 + (min (min1 - min2) $ max1 - max1)
       --1 is contained by 2

overlaps :: (Num a, Ord a) => (a, a) -> (a, a) -> Bool
overlaps a b = isJust $ getOverlap a b

data Physics = Physics {
    entityPhysics :: (Vector EntityPhysics),
    newCollisions :: Collisions,
    noLongerCollisions :: Collisions,
    remainingCollisions :: Collisions,
    space :: Space
    }

physicsMask :: Mask
physicsMask = componentToMask PhysicsComponent

physicsUpdate :: 
    Int -> -- bucketWidth
    Int -> -- bucketHeight
    Int -> -- mapWidth
    Int -> -- mapHeight
    Int -> -- numberRelaxations
    Double -> 
    Vector Mask -> 
    Physics -> 
    Physics 
physicsUpdate bucketWidth bucketHeight mapWidth mapHeight numberRelaxations tick masks physics = physics'
    where
    integratedPhysics = verletIntegration tick $ zip masks $ entityPhysics physics
    space' = createSpace bucketWidth bucketHeight mapHeight mapWidth $ zip masks integratedPhysics
    (collisions, onceRelaxedPhysics) = resolveCollisions space' $ zip masks integratedPhysics
    oldCollisions = remainingCollisions physics
    newCollisions' = Set.difference collisions oldCollisions

    maybeRelax x = Just $ (result, result)
        where
        result = relax space' masks x

    (remainingCollisions', entityPhysics') = last $ unfoldrN numberRelaxations maybeRelax (Set.empty, onceRelaxedPhysics)
    noLongerCollisions' = Set.difference oldCollisions remainingCollisions'

    physics' = Physics (entityPhysics') newCollisions' noLongerCollisions' remainingCollisions' space'

type PointIndex = Int

pointToV2 ::  Point V2 a -> V2 a
pointToV2 (P a) = a

--TODO this function needs to be throughly tested, as it's certainly wrong in
--some cases
project :: (Ord a, Num a) => V2 a -> Shape a -> (a, a)
project axis (Circle _ radius) = ((-radius), radius)
--TODO: this is almost surely wrong
--http://board.flashkit.com/board/showthread.php?787281-Separating-Axis-Theorem
project axis (AABB center@(P cV2@(V2 x y)) halfWidth halfHeight) = ((b - r), b + r)
--http://en.wikipedia.org/wiki/Bounding_volume#Basic_intersection_checks
  where
  r = halfWidth * abs x + halfHeight * abs y
  --TODO I'm not sure whether x and y should be absolute valued or get the
  --magnitude of center
  b = dot cV2 axis
project axis (Polygon points edges _) = ((minimum projected), maximum projected)
--http://www.codezealot.org/archives/55
    where
    v2Pairs = map (edgeToV2Pair points) edges
    projected = map (\(a, b) -> dot a b) v2Pairs

relax :: Space -> Vector Mask -> (Collisions, Vector EntityPhysics) -> (Collisions, Vector EntityPhysics)
relax space' masks (_, entityP) = (collisions, constrainedPhysics)
    where 
    (collisions, entityPhysics') = resolveCollisions space' $ zip masks entityP
    constrainedPhysics = satisfyConstraints $ zip masks entityPhysics'

resolveCollisions :: Space -> Vector (Mask, EntityPhysics) -> (Collisions, Vector EntityPhysics)
resolveCollisions space' maskEntityPhysics = undefined

data Shape a = 
    Circle (Point V2 a) a -- center, radius
    | AABB (Point V2 a) a a -- center, halfWidth halfHeight
    | Polygon (Vector (Point V2 a)) 
              (Vector Edge) 
              (Vector (LineConstraint a)) 
    --  Complex Point (Vector Shape) (Vector ComplexLineConstraint)

type Space = Vector (Vector Entity)

satisfyConstraints :: Vector (Mask, EntityPhysics) -> Vector EntityPhysics
satisfyConstraints input = map solve input
    where
    solve (mask, physics) =
        if hasMask mask physicsMask && not (isStatic physics) && not (isTrigger physics)
        then satisfyLineConstraints physics
        else physics

satisfyLineConstraint :: (Num a , Fractional a) => Shape a -> LineConstraint a -> Shape a
satisfyLineConstraint (Polygon points edges constraints) (LineConstraint i j restLength) = 
    Polygon points' edges constraints
    where
    a = points ! i
    b = points ! j
    delta = (a ^-^ b) 
    restLengthSquared = restLength * restLength
    delta' = delta ^* 
        (restLengthSquared/(dot delta delta + restLengthSquared) - 0.5)
    a' = a + delta'
    b' = b - delta'
    points' = points // [(i, a'), (j, b')]
satisfyLineConstraint (AABB _ _ _) _ = 
    error "satisfyLineConstraint should never be called for AABBs"
satisfyLineConstraint (Circle _ _) _ = 
    error "satisfyLineConstraint should never be called for circles"

--This method assumes the physics data belongs to an entity with physics, and it
--assumes it isn't static or a trigger
satisfyLineConstraints :: EntityPhysics -> EntityPhysics
satisfyLineConstraints physics = case shape physics of
    (Circle _ _) -> physics
    (AABB _ _ _) -> physics
    (Polygon _ _ constraints) -> let
        newShape = foldl satisfyLineConstraint (shape physics) constraints
        in
        physics {shape = newShape}

sin45 :: (Floating a, Fractional a) => a
sin45 = sqrt 2 / 2 

-- | This method will convert the position to the correct bucketIndex. It assumes
-- the position it is given is valid and on the map.
-- >>> toBucket 1 1 100 $ V2 5 0
-- 5
-- >>> toBucket 1 1 100 $ V2 5.4524 1.23423
-- 105
-- >>> toBucket 10 10 100 $ V2 5.4524 1.23423
-- 0
-- >>> toBucket 10 10 100 $ V2 50 9.99999
-- 5
-- >>> toBucket 10 10 100 $ V2 50 10
-- 15
-- >>> toBucket 10 10 100 $ V2 50 11
-- 15
-- >>> toBucket 10 10 100 $ V2 50 10
-- 15
toBucket :: (RealFrac a, Floating a) => Int -> Int -> Int -> V2 a -> Int
toBucket bucketWidth bucketHeight mapWidth (V2 x y) = floor $ left + right
    where
    left = x / fromIntegral bucketWidth
    adjustedWidth = fromIntegral mapWidth / fromIntegral bucketWidth
    right = fromIntegral ((floor (y / fromIntegral bucketHeight)) :: Int) * adjustedWidth

-- | This method will return a vector of all the buckets a shape overlaps.
toBuckets :: (Floating a, RealFrac a) => Int -> Int -> Int -> Shape a -> Vector Int
toBuckets bucketWidth bucketHeight mapWidth (Circle (P center) radius) = result
    --TODO: all these lists and sets could probably be optimized away
    where
    north = center + V2 0 radius
    south = center - V2 0 radius
    east = center + V2 radius 0
    west = center - V2 radius 0
    orthagonal = radius * sin45
    --sin 45 degrees = opposite / hypotenus (radius)
    northWest = center + V2 (-orthagonal)   orthagonal
    northEast = center + V2 ( orthagonal)   orthagonal
    southWest = center + V2 (-orthagonal) (-orthagonal)
    southEast = center + V2 ( orthagonal) (-orthagonal)
    result = fromList $ Set.toList $ Set.fromList $ 
        fmap (toBucket bucketWidth bucketHeight mapWidth) [north, south, east, west, northWest, northEast, southWest, southEast]
toBuckets bucketWidth bucketHeight mapWidth (AABB (P center) halfWidth halfHeight) = result
    where
    northWest = center + V2 (-halfWidth)   halfHeight
    northEast = center + V2 ( halfWidth)   halfHeight
    southWest = center + V2 (-halfWidth) (-halfHeight)
    southEast = center + V2 ( halfWidth) (-halfHeight)
    result = fromList $ Set.toList $ Set.fromList $ 
        fmap (toBucket bucketWidth bucketHeight mapWidth) [northWest, northEast, southWest, southEast]
toBuckets bucketWidth bucketHeight mapWidth polygon@(Polygon _ _ _) = buckets
    --We have to collision test the surrounding 8 buckets to decide which
    --buckets to return
    where
    center = getCenter polygon
    centerOfBucket = getCenterOfBucket bucketWidth bucketHeight center
    halfHeight = fromIntegral bucketHeight / 2
    halfWidth  = fromIntegral bucketWidth  / 2
    bucketWidth'  = fromIntegral bucketWidth
    bucketHeight' = fromIntegral bucketHeight
    northPoint = centerOfBucket + P (V2 0   bucketHeight')
    southPoint = centerOfBucket + P (V2 0 (-bucketHeight'))
    eastPoint =  centerOfBucket + P (V2     bucketHeight'  0)
    westPoint =  centerOfBucket + P (V2   (-bucketHeight') 0)
    northWestPoint = centerOfBucket + P (V2 (-bucketWidth')   bucketHeight')
    northEastPoint = centerOfBucket + P (V2   bucketWidth'    bucketHeight')
    southWestPoint = centerOfBucket + P (V2 (-bucketWidth') (-bucketHeight'))
    southEastPoint = centerOfBucket + P (V2   bucketWidth'  (-bucketHeight'))
    northAABB = AABB northPoint halfWidth halfHeight
    southAABB = AABB southPoint halfWidth halfHeight
    eastAABB =  AABB eastPoint  halfWidth halfHeight
    westAABB =  AABB westPoint  halfWidth halfHeight
    northWestAABB = AABB northWestPoint halfWidth halfHeight
    northEastAABB = AABB northEastPoint halfWidth halfHeight
    southWestAABB = AABB southWestPoint halfWidth halfHeight
    southEastAABB = AABB southEastPoint halfWidth halfHeight
    pairs = fromList $ (center, undefined) : List.filter (\(_,b) -> detailedCollisionCheck polygon b) 
        [(northPoint, northAABB),
         (southPoint, southAABB),
         (eastPoint,  eastAABB),
         (westPoint,  westAABB),
         (northWestPoint, northWestAABB),
         (northEastPoint, northEastAABB),
         (southWestPoint, southWestAABB),
         (southEastPoint, southEastAABB)]
    buckets = map (toBucket bucketWidth bucketHeight mapWidth . pointToV2 . fst) pairs

verletIntegration :: Double -> Vector (Mask, EntityPhysics) -> Vector EntityPhysics
verletIntegration tick input = map integrate input
    where
    integrate (mask, physics@(EntityPhysics force' isStatic' 
        _ invertedMass' oldPosition' position' _)) = 

        if hasMask mask physicsMask && not isStatic'
        then  physics{ 
            oldPosition = position',
            position = 2 *^ position' - oldPosition' + 
                (force' ^* invertedMass') ^* tick ** 2
            }
        else physics
