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

data Edge = Edge PointIndex PointIndex 
--We can't reference the point directly because this is Haskell. Instead, we are
--saving the vector indexes of the points that form the edges of our object.

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
    physics = snd $ Vector.unzip newPhysics

type PointIndex = Int

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

--This method will convert the position to the correct bucketIndex. It assumes
--the position it is given is valid and on the map.
toBucket :: Int -> V2 Double -> Int
toBucket mapWidth (V2 x y) = floor $ x + y * fromIntegral mapWidth

satisfyConstraints :: Vector.Vector (Mask, PhysicsData) -> Vector.Vector (Mask, PhysicsData)
satisfyConstraints input = 
    Vector.map solve input
    where
    solve (mask, physics@(PhysicsData _ _ _ invertedMass' _ position' shape')) =
        if hasMask mask physicsMask && not (isStatic physics) && not (isTrigger physics)
        then undefined
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
