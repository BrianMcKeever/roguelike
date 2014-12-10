{-# LANGUAGE MultiWayIf #-}
module Components.Physics (
    Collision,
    createCollision,
    PhysicsData(..),
    PhysicsDatas,
    physicsMask,
    physicsUpdate,
    initialPhysics,
    Shape(..)
)
where
import Control.Monad.ST
import EntityComponentSystem
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.AmericanFlag as AF
import Linear.Affine
import Linear.Metric
import Linear.V2
import Linear.Vector
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
    Vector.Vector (Int, Entity) -> 
    Space
bucketize gridSize xs = Vector.unfoldrN gridSize generate (0, unwrapped)
    where
    wrapped = Vector.map BucketIndexEntity xs
    sorted = runST $ do 
        thawed <- Vector.thaw wrapped
        AF.sort thawed
        Vector.freeze thawed
    unwrapped = Vector.map (\ (BucketIndexEntity (a, b)) -> (a, b)) sorted
    generate (a, b) = Just (next, (a + 1, remaining))
        where
        (matching, remaining) = Vector.span ((==) a . fst) b
        next = Vector.map snd matching

type Collision = (Entity, Entity) 
-- A collision between a and b. a < b

type Collisions = Vector.Vector Collision

createCollision :: Entity -> Entity -> Collision
createCollision a b 
    | a < b     = (a, b)
    | otherwise = (b, a)

createSpace :: Int -> 
    Int ->
    Vector.Vector (Mask, PhysicsData) -> 
    Space
createSpace mapHeight mapWidth input = result
    where
    positionEntity = Vector.imap f input
    f i (mask, physics) | hasMask mask physicsMask = 
        (toBucket mapWidth $ position physics, fromIntegral i)
                        | otherwise = (-1, fromIntegral i)
    bucketIdEntity = Vector.filter (\ (x, _) -> x /= -1) positionEntity
    result = bucketize (mapHeight * mapWidth) bucketIdEntity

detailedCollisionCheck :: (Ord a, Floating a) => 
    Shape a -> 
    Shape a -> 
    Bool

detailedCollisionCheck (Circle (P pos1) radius1) 
    (Circle (P pos2) radius2) =
    (radius1 + radius2)**2 > qdA pos1 pos2

detailedCollisionCheck (Circle (P (V2 x1 y1)) radius) 
                       (AABB (P (V2 x2 y2)) halfWidth halfHeight) = result
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

detailedCollisionCheck a@(AABB _ _ _) c@(Circle _ _) =
    detailedCollisionCheck c a

detailedCollisionCheck 
    (AABB (P (V2 x1 y1)) halfWidth1 halfHeight1) 
    (AABB (P (V2 x2 y2)) halfWidth2 halfHeight2) =
        (abs(x1 - x2) < (halfWidth1 + halfWidth2)) && 
        (abs(y1 - y2) < (halfHeight1 + halfHeight2))

data Edge = Edge PointIndex PointIndex 
--We can't reference the point directly because this is Haskell. Instead, we are
--saving the vector indexes of the points that form the edges of our object.

edgeToAxis :: Num a => Vector.Vector (Point V2 a) -> Edge -> V2 a
edgeToAxis points (Edge i j) = perp $ (pointToV2 $ points Vector.! i) - (pointToV2 $ points Vector.! j) 

edgeToV2Pair :: Num a => Vector.Vector (Point V2 a) -> Edge -> (V2 a, V2 a)
edgeToV2Pair points (Edge i j) = (pointToV2 $ points Vector.! i, pointToV2 $ points Vector.! j)

getAABBSeparatingAxis :: Vector.Vector (V2 a)
getAABBSeparatingAxis = Vector.fromList [V2 0 1, V2 1 0]

getPolygonSeparatingAxis :: Num a => Shape a -> Vector.Vector (V2 a)
getPolygonSeparatingAxis (Polygon _ points edges _) = Vector.map (edgeToAxis points) edges
getPolygonSeparatingAxis _ = error "getPolygonSeparatingAxis should only be called with polygons"

getSeparatingAxis :: (Ord a, Num a) => Shape a -> Shape a -> Vector.Vector (V2 a)
getSeparatingAxis polygon1@(Polygon _ _ _ _) polygon2@(Polygon _ points2 edges2 _) = 
    getPolygonSeparatingAxis polygon1 Vector.++ getPolygonSeparatingAxis polygon2
    --I'm not happy about concatanating the edges, but since the vectors should be very
    --small, it's not guaronteed that building a vector with unfoldr would be faster.
getSeparatingAxis polygon@(Polygon _ points edges _) (Circle center _) = getPolygonSeparatingAxis polygon Vector.++ Vector.singleton circleAxis
    where
    --http://www.codezealot.org/archives/55
    --I'm finding the closest point the stupid way because I'm tired of
    --googling.
    (P v2) = fst $ Vector.minimumBy (\ (_, a) (_, b) -> compare a b) $ Vector.zip points $ Vector.map (qdA center) points
    circleAxis = perp v2

getSeparatingAxis c@(Circle _ _) p@(Polygon _ _ _ _) = getSeparatingAxis p c
getSeparatingAxis (Polygon _ points1 edges1 _) (AABB _ _ _) = getPolygonSeparatingAxis polygon Vector.++ getAABBSeparatingAxis
getSeparatingAxis a@(AABB _ _ _) p@(Polygon _ _ _ _) = getSeparatingAxis p a
getSeparatingAxis (AABB _ _ _) (Circle  _ _) = error "getSeparatingAxis should not be called for AABB and circle"
getSeparatingAxis (Circle _ _) (AABB _ _ _) = error "getSeparatingAxis should not be called for AABB and circle"
getSeparatingAxis (Circle _ _) (Circle _ _) = error "getSeparatingAxis should not be called for circles"
getSeparatingAxis (AABB _ _ _) (AABB _ _ _) = error "getSeparatingAxis should not be called for two AABB"

initialPhysics :: PhysicsDatas
initialPhysics = Vector.replicate maxEntities d
    where
    d = PhysicsData (V2 0 0) False False 0 (V2 0 0) (V2 0 0) (Circle (P (V2 0 0)) 0)

data LineConstraint a = LineConstraint PointIndex PointIndex a -- distance 
--We can't reference the point directly because this is Haskell. Instead, we are
--saving the vector indexes of the points that should be (a) distance apart.

data PhysicsData = PhysicsData {
    force :: V2 Double,
    isStatic :: Bool, 
    isTrigger :: Bool, 
    invertedMass :: Double,
    oldPosition :: V2 Double,
    position :: V2 Double,
    shape :: Shape Double
    }

type PhysicsDatas = Vector.Vector PhysicsData

physicsMask :: Mask
physicsMask = componentToMask PhysicsComponent

physicsUpdate :: Double -> 
    Vector.Vector (Mask, PhysicsData) -> 
    (Collisions, Collisions, PhysicsDatas) --newCollisions, oldCollisions
physicsUpdate tick input = (Vector.empty, Vector.empty, physics)
    where
    newPhysics = verletIntegration tick input
    newPhysics' = satisfyConstraints newPhysics
    newPhysics2 = updateCenters newPhysics'
    physics = snd $ Vector.unzip newPhysics2

type PointIndex = Int

--pointToV2 ::  Point (V2 a) -> V2 a
pointToV2 (P a) = a

project :: (Ord a, Num a) => V2 a -> Shape a -> V2 a
project axis (Circle _ radius) = V2 (-radius) radius
--http://board.flashkit.com/board/showthread.php?787281-Separating-Axis-Theorem
project axis (AABB center@(P cV2@(V2 x y)) halfWidth halfHeight) = V2 (b - r) $ b + r
--http://en.wikipedia.org/wiki/Bounding_volume#Basic_intersection_checks
  where
  r = halfWidth * abs x + halfHeight * abs y
  --TODO I'm not sure whether x and y should be absolute valued or get the
  --magnitude of center
  b = dot cV2 axis
project axis (Polygon _ points edges _) = V2 (Vector.minimum projected) $ Vector.maximum projected
--http://www.codezealot.org/archives/55
    where
    v2Pairs = Vector.map (edgeToV2Pair points) edges
    projected = Vector.map (\(a, b) -> dot a b) v2Pairs
    

data Shape a = 
    Circle (Point V2 a) a -- center, radius
    | AABB (Point V2 a) a a -- center, halfWidth halfHeight
    | Polygon (Point V2 a) (Vector.Vector (Point V2 a)) 
                    (Vector.Vector Edge) 
                    (Vector.Vector (LineConstraint a)) 
           -- center
    -- | Complex Point (Vector Shape) (Vector ComplexLineConstraint)
           -- center

type Space = Vector.Vector (Vector.Vector Entity)

satisfyConstraints :: Vector.Vector (Mask, PhysicsData) -> Vector.Vector (Mask, PhysicsData)
satisfyConstraints input = 
    Vector.map solve input
    where
    solve (mask, physics@(PhysicsData _ _ _ invertedMass' _ position' shape')) =
        if hasMask mask physicsMask && not (isStatic physics) && not (isTrigger physics)
        then (mask, satisfyLineConstraints physics)
        else (mask, physics)

satisfyLineConstraint :: (Num a , Fractional a) => Shape a -> LineConstraint a -> Shape a
satisfyLineConstraint 
    (Polygon center points edges constraints) 
    (LineConstraint i j restLength) = Polygon center points' edges constraints
    where
    a = points Vector.! i
    b = points Vector.! j
    delta = (a ^-^ b) 
    restLengthSquared = restLength * restLength
    delta' = delta ^* 
        (restLengthSquared/(dot delta delta + restLengthSquared) - 0.5)
    a' = a + delta'
    b' = b - delta'
    points' = points Vector.// [(i, a'), (j, b')]
satisfyLineConstraint (AABB _ _ _) _ = 
    error "satisfyLineConstraint should never be called for AABBs"
satisfyLineConstraint (Circle _ _) _ = 
    error "satisfyLineConstraint should never be called for circles"

--This method assumes the physics data belongs to an entity with physics, and it
--assumes it isn't static or a trigger
satisfyLineConstraints :: PhysicsData -> PhysicsData
satisfyLineConstraints physics = case shape physics of
    (Circle _ _) -> physics
    (AABB _ _ _) -> physics
    (Polygon _ _ _ constraints) -> let
        newShape = Vector.foldl satisfyLineConstraint (shape physics) constraints
        in
        physics {shape = newShape}

--This method will convert the position to the correct bucketIndex. It assumes
--the position it is given is valid and on the map.
toBucket :: Int -> V2 Double -> Int
toBucket mapWidth (V2 x y) = floor $ x + y * fromIntegral mapWidth

--The center of polygons aren't getting updated with collisions for
--performance's sake, so we need to go back and update all the centers when
--we're done
updateCenters :: Vector.Vector (Mask, PhysicsData) -> Vector.Vector (Mask, PhysicsData)
updateCenters = undefined

verletIntegration :: Double -> Vector.Vector (Mask, PhysicsData) -> Vector.Vector (Mask, PhysicsData)
verletIntegration tick input = Vector.map integrate input
    where
    integrate (mask, physics@(PhysicsData force' isStatic' 
        _ invertedMass' oldPosition' position' _)) = 

        if hasMask mask physicsMask && not isStatic'
        then  (mask, physics{ 
            oldPosition = position',
            position = 2 *^ position' - oldPosition' + 
                (force' ^* invertedMass') ^* tick ** 2
            })
        else (mask, physics)
