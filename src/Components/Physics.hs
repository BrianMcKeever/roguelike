{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Components.Physics (
    BucketIndexEntity(..),
    Collision(..),
    createCollision,
    createSpace,
    detailedCollision,
    detailedCollisionCheck,
    Edge(..),
    edgeToAxis,
    getCenter,
    getEntitiesInBox,
    getDisplacement,
    EntityPhysics(..),
    gatherCollisions,
    Physics(..),
    physicsMask,
    physicsUpdate,
    project,
    projectShapes,
    initialPhysics,
    Shape(..),
    Space,
    toBucket,
    toBuckets
)
where
import Control.Applicative hiding (empty)
import Control.DeepSeq as DeepSeq
import Control.Monad hiding (sequence)
import Control.Monad.ST
import EntityComponentSystem
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set
import Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Linear.Affine
import Linear.Epsilon
import Linear.Metric hiding (project)
import Linear.V2
import Linear.Vector
import Prelude as Prelude 
    hiding ((++), concat, filter, foldl, init, last, length, map, maximum, minimum, null, replicate, sequence, span, sum, unzip, zip) 
import Vector

newtype BucketIndexEntity = BucketIndexEntity (Int, Entity)
    deriving (Eq, Show)

instance Ord BucketIndexEntity where
    compare (BucketIndexEntity (a1, _)) (BucketIndexEntity (a2, _)) = 
        compare a1 a2

aabbToPoints :: (Floating a) => Shape a -> Vector (Point V2 a)
aabbToPoints (AABB _ center halfWidth halfHeight) = fromList [northWest, northEast, southWest, southEast]
    where
    northWest = center + P (V2 (-halfWidth)   halfHeight )
    northEast = center + P (V2 ( halfWidth)   halfHeight )
    southWest = center + P (V2 (-halfWidth) (-halfHeight))
    southEast = center + P (V2 ( halfWidth) (-halfHeight))
aabbToPoints _ = error "aabbToPoints called on something other than aabb"

--This function transforms a vector of (bucketIndex, Entity) into a jagged
--Vector (Vector Entity) where the index of the enclosing vector corresponds to
--the bucketIndex
bucketize :: Int -> 
    Vector (Int, Entity) -> 
    Space
bucketize gridSize xs = map fromList lists
    where
    lists = create $ do 
        space' <- MVector.replicate gridSize []
        let prepend (i, entity) = do
                bucket <- MVector.read space' i
                MVector.write space' i (entity : bucket)
        Vector.mapM_ prepend xs
        return space'

calculateCollisionResult :: (Epsilon a, Ord a, Floating a) => Entity -> Maybe (a, V2 a) -> Entity -> Maybe (a, V2 a) -> Maybe (Collision a)
calculateCollisionResult entity1 left entity2 right = calculateCollisionResult2 entity1 entity2 maybeOverlapPair
    where
    maybeOverlapPair = min left right

calculateCollisionResult2 :: (Epsilon a, Ord a, Floating a) => Entity -> Entity -> Maybe (a, V2 a) -> Maybe (Collision a)
calculateCollisionResult2 entity1 entity2 maybeOverlapPair =
    case maybeOverlapPair of
        Just (displacement, axis) -> Just $ createCollision entity1 entity2 displacement axis
        Nothing -> Nothing
    {-
    if isJust maybeOverlapPair
    then let (displacement, axis) = fromJust maybeOverlapPair in
        Just $ createCollision entity1 entity2 displacement axis
    else Nothing
    -}

-- | Entity a, entity b, minimum displacement, minimum displacement axis. A < B
data Collision a = Collision Entity Entity a (V2 a)
    deriving (Show, Eq)

instance Eq a => Ord (Collision a) where
    compare (Collision a1 b1 _ _) (Collision a2 b2 _ _) = let
        firstCompare = compare a1 a2 in 
        case firstCompare of
            EQ -> compare b1 b2
            LT -> firstCompare
            GT -> firstCompare

instance NFData (Collision a) where
    rnf (Collision e1 e2 displacement axis) = rnf (seq e1 (), seq e2 (), seq displacement (), seq axis ())

type Collisions a = Set.Set (Collision a)

createCollision :: (Ord a, Floating a) => Entity -> Entity -> a -> V2 a -> Collision a
createCollision a b displacement axis
    | a < b = Collision a b displacement axis
    | a > b = Collision b a displacement axis
    | otherwise = error $ "collided with self"

-- | Spaces are vectors of vectors that contain entities that are all within the
-- same bucket.
createSpace :: 
    forall a . (Epsilon a, Floating a, Num a, RealFrac a) =>
    Int -> 
    Int ->
    Int ->
    Int ->
    Vector (Mask, EntityPhysics a) -> 
    Space
createSpace bucketWidth bucketHeight mapHeight mapWidth input = result
    where
    bucketEntity = imap f input
    f entity (mask, physics) | hasMask mask physicsMask = 
        map (\bucket -> (bucket, fromIntegral entity)) $ toBuckets bucketWidth bucketHeight mapWidth $ shape physics
                        | otherwise = empty
    numberXBuckets = ceiling $ ((fromIntegral mapWidth :: a) / fromIntegral bucketWidth)
    numberYBuckets = ceiling $ (fromIntegral mapHeight :: a) / fromIntegral bucketHeight
    result = bucketize (numberXBuckets * numberYBuckets) $ foldl' (++) empty bucketEntity

detailedCollision :: (Epsilon a, Floating a, Ord a) => Entity -> Shape a -> Entity -> Shape a -> Maybe (Collision a)
detailedCollision entity1 circle1@(Circle _ center1 _) entity2 circle2@(Circle _ center2 _) = result
    where
    axis = pointsToAxis center1 center2
    maybeOverlap = projectShapes circle1 circle2 axis
    result = calculateCollisionResult2 entity1 entity2 maybeOverlap

detailedCollision entity1 circle@(Circle _ _ _) entity2 aabb@(AABB _ _ _ _) = result
    where
    circleAxis = getCircleSeparatingAxis circle aabb
    maybeCircleOverlapPair = projectShapes circle aabb circleAxis
    maybeAABBOverlapPair = minimum $ map (projectShapes circle aabb) getAABBSeparatingAxis
    result = calculateCollisionResult entity1 maybeAABBOverlapPair entity2 maybeCircleOverlapPair

detailedCollision entity1 aabb1@(AABB _ _ _ _) entity2 aabb2@(AABB _ _ _ _) = result
    where
    maybeOverlap = minimum $ map (projectShapes aabb1 aabb2) getAABBSeparatingAxis
    result = calculateCollisionResult2 entity1 entity2 maybeOverlap

detailedCollision entity1 polygon@(Polygon _ _ _ _) entity2 aabb@(AABB _ _ _ _) = result
    where
    maybeAABBOverlapPair = minimum $ map (projectShapes polygon aabb) getAABBSeparatingAxis
    maybeEdgeOverlapPair = minimum $ map (projectShapes polygon aabb) $ getPolygonSeparatingAxis polygon
    result = calculateCollisionResult entity1 maybeAABBOverlapPair entity2 maybeEdgeOverlapPair

detailedCollision entity1 polygon@(Polygon _ _ _ _) entity2 circle@(Circle _ _ _) = result
    where
    circleAxis = getCircleSeparatingAxis circle polygon
    maybeCircleOverlapPair = projectShapes polygon circle circleAxis
    maybeEdgeOverlapPair = minimum $ map (projectShapes polygon circle) $ getPolygonSeparatingAxis polygon
    result = calculateCollisionResult entity1 maybeCircleOverlapPair entity2 maybeEdgeOverlapPair

detailedCollision entity1 polygon1@(Polygon _ _ _ _) entity2 polygon2@(Polygon _ _ _ _) = result
    where
    maybeEdgeOverlapPair1 = minimum $ map (projectShapes polygon1 polygon2) $ getPolygonSeparatingAxis polygon1
    maybeEdgeOverlapPair2 = minimum $ map (projectShapes polygon1 polygon2) $ getPolygonSeparatingAxis polygon2
    result = calculateCollisionResult entity1 maybeEdgeOverlapPair1 entity2 maybeEdgeOverlapPair2

detailedCollision entity1 aabb@(AABB _ _ _ _) entity2 circle@(Circle _ _ _) = 
    detailedCollision entity2 circle entity1 aabb

detailedCollision entity1 circle@(Circle _ _ _) entity2 polygon@(Polygon _ _ _ _) =
    detailedCollision entity2 polygon entity1 circle

detailedCollision entity1 aabb@(AABB _ _ _ _) entity2 polygon@(Polygon _ _ _ _) =
    detailedCollision entity2 polygon entity1 aabb

detailedCollisionCheck :: (Epsilon a, Ord a, Floating a) => Shape a -> Shape a -> Bool
detailedCollisionCheck (Circle _ (P pos1) radius1) (Circle _ (P pos2) radius2) = 
    (radius1 + radius2)**2 > qdA pos1 pos2
detailedCollisionCheck (Circle _ (P (V2 x1 y1)) radius) (AABB _ (P (V2 x2 y2)) halfWidth halfHeight) = result
    where
    xDifference = abs(x1 - x2)
    yDifference = abs(y1 - y2)
    cornerDistanceSquared = (xDifference - halfWidth)**2 + (yDifference - halfHeight) ** 2
    result = 
        if | xDifference > (halfWidth + radius) -> False
           | yDifference > (halfHeight + radius) -> False
           | xDifference <= (halfWidth) -> True
           | yDifference <= (halfHeight) -> True
           | otherwise -> cornerDistanceSquared <= radius ** 2
detailedCollisionCheck a@(AABB _ _ _ _) c@(Circle _ _ _) = detailedCollisionCheck c a
detailedCollisionCheck (AABB _ (P (V2 x1 y1)) halfWidth1 halfHeight1) (AABB _ (P (V2 x2 y2)) halfWidth2 halfHeight2) =
        (abs(x1 - x2) < (halfWidth1 + halfWidth2)) && (abs(y1 - y2) < (halfHeight1 + halfHeight2))
detailedCollisionCheck polygon@(Polygon _ _ _ _) circle@(Circle _ _ _) = 
    isJust $ detailedCollision unnessary polygon unnessary circle
detailedCollisionCheck c@(Circle _ _ _) p@(Polygon _ _ _ _) = detailedCollisionCheck p c
detailedCollisionCheck p@(Polygon _ _ _ _) a@(AABB _ _ _ _ ) = 
    isJust $ detailedCollision unnessary p unnessary a
detailedCollisionCheck a@(AABB _ _ _ _ ) p@(Polygon _ _ _ _) = detailedCollisionCheck p a
detailedCollisionCheck p1@(Polygon _ _ _ _) p2@(Polygon _ _ _ _) = 
    isJust $ detailedCollision unnessary p1 unnessary p2

data Edge = Edge PointIndex PointIndex 
    deriving (Show)
--Edges are really non-parallel edges.
--We can't reference the point directly because this is Haskell. Instead, we are
--saving the vector indexes of the points that form the edges of our object.

edgeToAxis :: (Epsilon a, Floating a, Num a) => Vector (Point V2 a) -> Edge -> V2 a
edgeToAxis points (Edge i j) = normalize $ perp $ (pointToV2 $ points ! i) - (pointToV2 $ points ! j)

--edgeToV2Pair :: Num a => Vector (Point V2 a) -> Edge -> (V2 a, V2 a)
--edgeToV2Pair points (Edge i j) = (pointToV2 $ points ! i, pointToV2 $ points ! j)

data EntityPhysics a = EntityPhysics {
    force :: (V2 a),
    isStatic :: Bool, 
    isTrigger :: Bool, 
    invertedMass :: a,
    shape :: (Shape a)
    }
    deriving (Show)

instance NFData (EntityPhysics a) where
    rnf (EntityPhysics f iS iT iM s) = rnf (seq f (), rnf iS, rnf iT, seq iM (), rnf s)

-- | This method takes the EntityPhysics data and a bucket of entities, 
-- collides all of the entities against each othes, and then returns a set of
-- Collisions.
gatherCollisions :: (Show a, Epsilon a, Eq a, Floating a, Ord a) => Vector (EntityPhysics a) -> Vector Entity -> Collisions a
gatherCollisions physics' entities' = result
    where
    len = length entities'

    --doCollisions :: Collisions a -> Int -> Entity -> Collisions a
    doCollisions collisions i entity = collisions'
        where
        front = i + 1
        back = len - front
        collisions' = foldl' (collide entity) collisions $ slice front back entities'

    --collide :: Entity -> Collisions a -> Entity -> Collisions a
    collide entity1 collisions' entity2 = 
        let shape1 = shape $ physics' ! fromIntegral entity1
            shape2 = shape $ physics' ! fromIntegral entity2
            maybeCollision = detailedCollision entity1 shape1 entity2 shape2 in
        case maybeCollision of
            Nothing -> collisions'
            Just collision -> Set.insert (collision) collisions'

    result = ifoldl' doCollisions Set.empty $ init entities'

getAABBSeparatingAxis :: Num a => Vector (V2 a)
getAABBSeparatingAxis = fromList [V2 0 1, V2 1 0]

getCenter :: Fractional a => Shape a -> Point V2 a
getCenter (Circle _ center _) = center
getCenter (AABB _ center _ _) = center
getCenter (Polygon _ points _ _) = sum points ^* (1 / (fromIntegral $ length points))

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

getCircleSeparatingAxis :: (Epsilon a, Floating a, Ord a, Num a) => Shape a -> Shape a -> V2 a
getCircleSeparatingAxis (Circle _ center _) (Polygon _ points _ _) = pointsToAxis closestPoint center
    where
    closestPoint = getClosestPointToCenter center points
getCircleSeparatingAxis (Circle _ center _) aabb@(AABB _ _ _ _) = pointsToAxis closestPoint center
    where
    points = aabbToPoints aabb
    closestPoint = getClosestPointToCenter center points
getCircleSeparatingAxis _ _ = 
    error "getCircleSeparatingAxis should only be called with a circle and a polygon (in that order) or circle and aabb (in that order)"

getClosestPointToCenter :: (Num a, Ord a) => Point V2 a -> Vector (Point V2 a) -> Point V2 a
getClosestPointToCenter center points = 
    fst $ minimumBy (\ (_, a) (_, b) -> compare a b) $ zip points $ map (qdA center) points

-- | This returns the minimum distance needed to separate the pairs on the axis.
getDisplacement :: (Num a, Ord a) => (a, a) -> (a, a) -> Maybe a
getDisplacement (min1, max1) (min2, max2) = do
    --I tried to find a way to do this with less comparisons, but I failed
    when (max1 <= min2) Nothing
    when (max2 <= min1) Nothing
    if | min1 < min2 && max1 < max2 -> return $ max1 - min2
       -- 1 overlaps from left 
       | min2 < min1 && max2 < max1 -> return $ max2 - min1
       --2 overlaps from left 
       | min1 <= min2 && max2 <= max1 -> return $ max2 - min2 + (min (min2 - min1) $ max1 - max2)
       --2 is contained by 1 
       | otherwise -> return $ max1 - min1 + (min (min1 - min2) $ max2 - max1)
       --1 is contained by 2

-- | This function will return all of the entites in the box of width displayWidth
-- displayHeight centered on center.
getEntitiesInBox :: forall a . (Show a, Epsilon a, Floating a, Fractional a, Ord a, RealFrac a) 
    => Int -> Int -> Int -> Physics a -> Int -> Int -> Point V2 a -> Vector Entity
getEntitiesInBox bucketWidth bucketHeight mapWidth physics displayWidth displayHeight center@(P (V2 centerX centerY)) = result
    -- Get entities from the possible buckets
    -- Make AABB
    -- Test AABB against entities
    where
    numberXBuckets =  ceiling $ (fromIntegral displayWidth / fromIntegral bucketWidth :: a) :: Int
    numberYBuckets = ceiling $ (fromIntegral displayHeight / fromIntegral bucketHeight :: a) :: Int

    xs = filter (>0) $ Vector.enumFromStepN (centerX - 0.5 * fromIntegral displayWidth) (fromIntegral bucketWidth) $ fromIntegral numberXBuckets
    ys = filter (>0) $ Vector.enumFromStepN (centerY - 0.5 * fromIntegral displayHeight) (fromIntegral (bucketHeight)) $ fromIntegral numberYBuckets
    coordinatesInBuckets = flip (V2) <$> ys <*> xs
    space' = space physics
    entities = concatVector $ map (\ coordinate -> space' ! (toBucket bucketWidth bucketHeight mapWidth coordinate)) coordinatesInBuckets
    uniqueEntities = removeDuplicates entities
    aabb = AABB (P $ V2 0 0) center (fromIntegral displayWidth * 0.5) (fromIntegral displayHeight * 0.5)
    entityPhysics' = entityPhysics physics
    result = filter (\entity -> detailedCollisionCheck (shape $ entityPhysics' ! fromIntegral entity) aabb) uniqueEntities
    --TODO we only need to check the entities in the buckets around the
    --edges

getPolygonSeparatingAxis :: (Epsilon a, Floating a, Num a) => Shape a -> Vector (V2 a)
getPolygonSeparatingAxis (Polygon _ points edges _) = map (edgeToAxis points) edges
getPolygonSeparatingAxis _ = error "getPolygonSeparatingAxis should only be called with polygons"

initialPhysics :: Physics a
initialPhysics = Physics (replicate maxEntities ep) Set.empty Set.empty Set.empty empty
    where
    ep = EntityPhysics unnessary unnessary unnessary unnessary unnessary 

-- A LineConstraint is like a stick between 2 points. The physics engine will
-- try to keep the points the same distance from each other.
-- Because Haskell can't reference the points directly, we are storing ghetto
-- references - the indexes of the points.
data LineConstraint a = LineConstraint PointIndex PointIndex a -- distance 
    deriving (Show)

-- | This integrates the point using verletIntegration and returns newPoint.
integratePoint :: (Floating a, Num a) => a -> V2 a -> a -> Point V2 a -> Point V2 a -> Point V2 a
integratePoint tick force' invertedMass' oldPoint point = 2 *^ point - oldPoint + (P force' ^* invertedMass') ^* tick ** 2

integrateShape :: Floating a => a -> Shape a -> V2 a -> a -> Shape a
integrateShape tick (Circle oldCenter center radius') force' invertedMass' = Circle center newPoint radius'
    where
    newPoint = integratePoint tick force' invertedMass' oldCenter center
integrateShape tick (AABB oldCenter center halfWidth halfHeight) force' invertedMass' = AABB center newPoint halfWidth halfHeight
    where
    newPoint = integratePoint tick force' invertedMass' oldCenter center
integrateShape tick (Polygon oldPoints points edges constraints) force' invertedMass' = Polygon points points' edges constraints
    where
    integrate (oldPoint, point) = integratePoint tick force' invertedMass' oldPoint point
    points' = map integrate $ zip oldPoints points

moveShape :: (Num a) => Shape a -> V2 a -> Shape a
moveShape (Circle oldCenter center radius) movementVector = Circle oldCenter newCenter radius
    where 
    newCenter = center + P movementVector
moveShape (AABB oldCenter center halfWidth halfHeight) movementVector = AABB oldCenter newCenter halfWidth halfHeight
    where 
    newCenter = center + P movementVector
moveShape (Polygon oldPoints points edges constraints) movementVector = Polygon oldPoints newPoints edges constraints
    where
    newPoints = map (+ P movementVector) points

-- | Returns true if the pairs overlap. Otherwise, false.
-- overlaps (1,4) (10,22)
-- False
-- overlaps (1,14) (10,22)
-- True
--overlaps :: (Num a, Ord a) => (a, a) -> (a, a) -> Bool
--overlaps a b = isJust $ getDisplacement a b

data Physics a = Physics {
    entityPhysics :: (Vector (EntityPhysics a)),
    newCollisions :: (Collisions a),
    noLongerCollisions :: (Collisions a),
    remainingCollisions :: (Collisions a),
    space :: Space
    }
    deriving (Show)

instance NFData (Physics a) where
    rnf (Physics ep nc nlc rc s) = rnf (rnf ep, rnf nc, rnf nlc, rnf rc, rnf s)

physicsMask :: Mask
physicsMask = componentToMask PhysicsComponent

physicsUpdate :: 
    (Show a, Epsilon a, Eq a, Floating a, RealFrac a) =>
    Int -> -- bucketWidth
    Int -> -- bucketHeight
    Int -> -- mapWidth
    Int -> -- mapHeight
    Int -> -- numberRelaxations
    a ->  -- Tick
    Vector Mask -> 
    Physics a -> 
    Physics a
physicsUpdate bucketWidth bucketHeight mapWidth mapHeight numberRelaxations tick masks physics = physics'
    where
    integratedPhysics = verletIntegration tick $ zip masks $ entityPhysics physics
    space' = createSpace bucketWidth bucketHeight mapHeight mapWidth $ zip masks integratedPhysics
    spaceWithCollisions = filter (\bucket -> length bucket > 1) space'
    (collisions, onceRelaxedPhysics) = resolveCollisions spaceWithCollisions integratedPhysics
    oldCollisions = remainingCollisions physics
    newCollisions' = Set.difference collisions oldCollisions

    maybeRelax x = Just $ (result, result)
        where
        result = relax spaceWithCollisions masks x

    (remainingCollisions', entityPhysics') = last $ unfoldrN numberRelaxations maybeRelax (Set.empty, onceRelaxedPhysics)
    noLongerCollisions' = Set.difference oldCollisions remainingCollisions'

    physics' = Physics (entityPhysics') newCollisions' noLongerCollisions' remainingCollisions' (DeepSeq.force space')

type PointIndex = Int

pointsToAxis :: (Epsilon a, Eq a, Floating a, Num a) => Point V2 a -> Point V2 a -> V2 a
pointsToAxis point1 point2 = result
    where
    difference = (pointToV2 point1) - (pointToV2 point2)
    result = if difference == V2 0 0 then V2 0 1 else normalize difference

pointToV2 ::  Point V2 a -> V2 a
pointToV2 (P a) = a

processCollisions :: (Num a, Fractional a) => Collisions a -> Vector (EntityPhysics a) -> Vector (EntityPhysics a)
processCollisions collisions physics = runST $ do
    thawed <- thaw physics
    sequence $ map (processCollision thawed) $ fromList $ Set.toList collisions
    freeze thawed
    where
    processCollision physics' (Collision entity1 entity2 displacement axis) = result
        where
        entityPhysics1 = physics ! fromIntegral entity1
        entityPhysics2 = physics ! fromIntegral entity2
        result = 
            if (not $ isStatic entityPhysics1) && (not $ isStatic entityPhysics2)
            then let
                halfDisplacement = displacement * 0.5
                movementVector1 = axis ^*   halfDisplacement
                movementVector2 = axis ^* (-halfDisplacement)
                --TODO we're going to get bit by a pushing them the wrong direction
                --half the time bug here.
                shape1 = moveShape (shape entityPhysics1) movementVector1
                shape2 = moveShape (shape entityPhysics2) movementVector2
                newEntityPhysics1 = entityPhysics1 {shape = shape1}
                newEntityPhysics2 = entityPhysics2 {shape = shape2}
                in
                MVector.write physics' (fromIntegral entity1) newEntityPhysics1 >> 
                    MVector.write physics' (fromIntegral entity2) newEntityPhysics2
            else if isStatic entityPhysics1
                then let 
                    movementVector = axis ^* displacement
                    --TODO we're going to get bit by a pushing them the wrong direction
                    --half the time bug here.
                    shape' = moveShape (shape entityPhysics2) movementVector
                    newEntityPhysics' = entityPhysics2 {shape = shape'}
                    in
                    MVector.write physics' (fromIntegral entity2) newEntityPhysics'
                else let
                    movementVector = axis ^* displacement
                    --TODO we're going to get bit by a pushing them the wrong direction
                    --half the time bug here.
                    shape' = moveShape (shape entityPhysics1) movementVector
                    newEntityPhysics' = entityPhysics1 {shape = shape'}
                    in
                    MVector.write physics' (fromIntegral entity1) newEntityPhysics'

-- | This method projects a shape onto an axis and returns the start and end on
-- that axis. The axis argument should be normalized.
project :: (Ord a, Num a, Floating a, Epsilon a) => V2 a -> Shape a -> (a, a)
project axis (Circle _ center radius) = (c - radius, c + radius)
--http://board.flashkit.com/board/showthread.php?787281-Separating-Axis-Theorem
    where
    c = dot (pointToV2 center) axis
project axis@(V2 x y) (AABB _ (P center) halfWidth halfHeight) = ((b - r), b + r)
--http://en.wikipedia.org/wiki/Bounding_volume#Basic_intersection_checks
    where
    r = halfWidth * abs x + halfHeight * abs y
    --TODO I'm not sure whether x and y should be absolute valued or get the
    --magnitude of center
    b = dot center axis
project axis (Polygon _ points _ _) = (minimum projected, maximum projected)
--http://www.codezealot.org/archives/55
--"Projecting a shape onto an axis"
    where
    v2s = map pointToV2 points
    projected = map (\ a -> dot a axis) v2s

-- | This method will project both shapes onto an axis. 
projectShapes :: (Epsilon a, Ord a, Floating a) => Shape a -> Shape a -> V2 a -> Maybe (a, V2 a)
projectShapes shape1 shape2 axis = fmap (flip (,) axis) $ getDisplacement overlap1 overlap2
    where
    overlap1 = project axis shape1
    overlap2 = project axis shape2

relax :: (Show a, Epsilon a, Eq a, Floating a, Fractional a, Ord a) => 
    Space -> Vector Mask -> (Collisions a, Vector (EntityPhysics a)) -> (Collisions a, Vector (EntityPhysics a))
relax space' masks (_, entityP) = (collisions, constrainedPhysics)
    where 
    (collisions, entityPhysics') = resolveCollisions space' entityP
    constrainedPhysics = satisfyConstraints $ zip masks entityPhysics'

resolveCollisions :: (Epsilon a, Eq a, Floating a, Ord a, Show a) => Space -> Vector (EntityPhysics a) -> (Collisions a, Vector (EntityPhysics a))
resolveCollisions space' physics = (combinedCollisions, newPhysics)
    where
    --collisions = map (gatherCollisions physics) space'
    --combinedCollisions = foldl' (\c cs -> Set.union c cs) Set.empty collisions
    -- It might be faster to do it this commented out way on multiple
    -- processors.
    combinedCollisions = foldl' (\s bucket -> Set.union s $ gatherCollisions physics bucket) Set.empty space'
    newPhysics = processCollisions combinedCollisions physics

data Shape a = 
    Circle (Point V2 a) (Point V2 a) a -- oldCenter, center, radius
    | AABB (Point V2 a) (Point V2 a) a a -- oldCenter, center, halfWidth halfHeight
    | Polygon (Vector (Point V2 a)) --oldPoints
              (Vector (Point V2 a)) --points
              (Vector Edge) 
              (Vector (LineConstraint a)) 
    --  Complex Point (Vector Shape) (Vector ComplexLineConstraint)
    deriving (Show)

instance NFData (Shape a) where
    rnf a = seq a ()

type Space = Vector (Vector Entity)

satisfyConstraints :: (Epsilon a, Floating a, Fractional a) => Vector (Mask, EntityPhysics a) -> Vector (EntityPhysics a)
satisfyConstraints input = map solve input
    where
    solve (mask, physics) =
        if hasMask mask physicsMask && not (isStatic physics) && not (isTrigger physics)
        then satisfyLineConstraints physics
        else physics

-- | satisfyLineConstraint will make sure one line constraint is enforced on a
-- shape.
-- >>> satisfyLineConstraint (Polygon empty (fromList [P (V2 1.0 0.0), P (V2 2.0 0.0)]) empty empty) (LineConstraint 0 1 1.0)
-- Polygon (fromList []) (fromList [P (V2 1.0 0.0),P (V2 2.0 0.0)]) (fromList []) (fromList [])
-- >>> satisfyLineConstraint (Polygon empty (fromList [P (V2 1.0 0.0), P (V2 2.0 0.0)]) empty empty) (LineConstraint 0 1 2.0)
-- Polygon (fromList []) (fromList [P (V2 0.5 0.0),P (V2 2.5 0.0)]) (fromList []) (fromList [])
satisfyLineConstraint :: (Epsilon a, Num a, Floating a, Fractional a) => Shape a -> LineConstraint a -> Shape a
satisfyLineConstraint (Polygon oldPoints' points edges constraints) (LineConstraint i j restLength) = 
    Polygon oldPoints' points' edges constraints
    where
    --This is explained in Advanced Character Physics by Thomas Jakobsen
    a = points ! i
    b = points ! j
    delta = (b ^-^ a) 
    deltaLength = sqrt (dot delta delta)
    difference = (deltaLength - restLength) / deltaLength
    adjustment = delta ^* (0.5 * difference)
    a' = a + adjustment
    b' = b - adjustment
    points' = if nearZero delta 
        then points
        else points // [(i, a'), (j, b')]
    --This is the sqrt approximation version explained in Advanced Character
    --Physics by Thomas Jakobsen, but it should be profiled before replacing the
    --other since it requires more iterations to get the right answer.
    --delta = (a ^-^ b) 
    --restLengthSquared = restLength * restLength
    --delta' = delta ^* (restLengthSquared/(dot delta delta + restLengthSquared) - 0.5)
    --a' = a + delta'
    --b' = b - delta'
    --points' = points // [(i, a'), (j, b')]
satisfyLineConstraint (AABB _ _ _ _) _ = 
    error "satisfyLineConstraint should never be called for AABBs"
satisfyLineConstraint (Circle _ _ _) _ = 
    error "satisfyLineConstraint should never be called for circles"

--This method assumes the physics data belongs to an entity with physics, and it
--assumes it isn't static or a trigger
satisfyLineConstraints :: (Epsilon a, Floating a, Fractional a) => EntityPhysics a -> EntityPhysics a
satisfyLineConstraints physics = case shape physics of
    (Circle _ _ _) -> physics
    (AABB _ _ _ _) -> physics
    (Polygon _ _ _ constraints) -> let
        newShape = foldl satisfyLineConstraint (shape physics) constraints
        in
        physics {shape = newShape}

sin45 :: (Floating a, Fractional a) => a
sin45 = sqrt 2 / 2 

-- | This method will convert the position to the correct bucketIndex. It assumes
-- the position it is given is valid and on the map.
toBucket :: (RealFrac a, Floating a) => Int -> Int -> Int -> V2 a -> Int
toBucket bucketWidth bucketHeight mapWidth (V2 x y) = floor $ left + right
    where
    left = x / fromIntegral bucketWidth
    adjustedWidth = fromIntegral mapWidth / fromIntegral bucketWidth
    right = fromIntegral ((floor (y / fromIntegral bucketHeight)) :: Int) * adjustedWidth

-- | This method will return a vector of all the buckets a shape overlaps.
toBuckets :: (Epsilon a, Floating a, RealFrac a) => Int -> Int -> Int -> Shape a -> Vector Int
toBuckets bucketWidth bucketHeight mapWidth (Circle _ (P center) radius) = result
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
    result = filter (\x -> x >= 0 && x < 40000) $ fromList $ Set.toList $ Set.fromList $ 
        fmap (toBucket bucketWidth bucketHeight mapWidth) [north, south, east, west, northWest, northEast, southWest, southEast]
toBuckets bucketWidth bucketHeight mapWidth (AABB _ (P center) halfWidth halfHeight) = result
    where
    northWest = center + V2 (-halfWidth)   halfHeight
    northEast = center + V2 ( halfWidth)   halfHeight
    southWest = center + V2 (-halfWidth) (-halfHeight)
    southEast = center + V2 ( halfWidth) (-halfHeight)
    result = filter (>= 0) $ fromList $ Set.toList $ Set.fromList $ 
        fmap (toBucket bucketWidth bucketHeight mapWidth) [northWest, northEast, southWest, southEast]
toBuckets bucketWidth bucketHeight mapWidth polygon@(Polygon _ _ _ _) = buckets
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
    northAABB = AABB unnessary northPoint halfWidth halfHeight
    southAABB = AABB unnessary southPoint halfWidth halfHeight
    eastAABB =  AABB unnessary eastPoint  halfWidth halfHeight
    westAABB =  AABB unnessary westPoint  halfWidth halfHeight
    northWestAABB = AABB unnessary northWestPoint halfWidth halfHeight
    northEastAABB = AABB unnessary northEastPoint halfWidth halfHeight
    southWestAABB = AABB unnessary southWestPoint halfWidth halfHeight
    southEastAABB = AABB unnessary southEastPoint halfWidth halfHeight
    pairs = fromList $ (center, unnessary) : List.filter (\(_,b) -> detailedCollisionCheck polygon b) 
        [(northPoint, northAABB),
         (southPoint, southAABB),
         (eastPoint,  eastAABB),
         (westPoint,  westAABB),
         (northWestPoint, northWestAABB),
         (northEastPoint, northEastAABB),
         (southWestPoint, southWestAABB),
         (southEastPoint, southEastAABB)]
    buckets = filter (>= 0) $ map (toBucket bucketWidth bucketHeight mapWidth . pointToV2 . fst) pairs

verletIntegration :: (Floating a) => a -> Vector (Mask, EntityPhysics a) -> Vector (EntityPhysics a)
verletIntegration tick input = map integrate input
    where
    integrate (mask, physics@(EntityPhysics force' isStatic' _ invertedMass' shape')) = 
        if hasMask mask physicsMask && not isStatic'
        then  physics{ 
            shape = integrateShape tick shape' force' invertedMass'
            }
        else physics
