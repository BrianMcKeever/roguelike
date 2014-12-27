module Components.PhysicsSpec (main, spec) 
where

import Components.Physics
import EntityComponentSystem
import Test.Hspec
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Vector as Vector
import Linear.Affine
import Linear.Metric hiding (project)
import Linear.V2
import Prelude as Prelude hiding ((++), concat, filter, foldl, init, last, length, map, maximum, minimum, replicate, sequence, span, sum, unzip, zip) 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "getDisplacement" $ do
        it "should return nothing because they don't overlap" $ do
            shouldBe (getDisplacement (0, 3) (3, 5)) (Nothing :: Maybe Double)
            shouldBe (getDisplacement (4, 5) (0, 4)) (Nothing :: Maybe Double) 

        it "should return something because they overlap" $ do
            shouldBe (getDisplacement (1, 3) (2, 4)) (Just 1  :: Maybe Double) 
            shouldBe (getDisplacement (2, 4) (1, 3)) (Just 1  :: Maybe Double) 
            shouldBe (getDisplacement (1, 5) (2, 3)) (Just 2  :: Maybe Double) 
            shouldBe (getDisplacement (2, 3) (1, 5)) (Just 2  :: Maybe Double) 

    describe "createSpace" $ do
        it "Should empty space" $ do
            let entityP = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0 :: V2 Double)) (P (V2 1 1)) 5))
            let input = zip (replicate 10 $ componentToMask EmptyComponent) (replicate 10 entityP)
            shouldBe (createSpace 1 1 1 2 input) (fromList [empty, empty])

        it "Should be space" $ do
            let entityP = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 1 1)) 5)) :: EntityPhysics Double
            let input = zip (replicate 1 physicsMask) (replicate 1 entityP)
            shouldBe (createSpace 10 10 10 50 input) (fromList $ List.map fromList [[0],[],[],[],[]])

    describe "detailedCollisionCheck" $ do
        it "should not collide circle1" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 0 10)) 5
            shouldBe (detailedCollisionCheck circle1 circle2) False

        it "should not collide circle2" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 10 0)) 5
            shouldBe (detailedCollisionCheck circle1 circle2) False

        it "should collide circle1" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 0 9)) 5
            shouldBe (detailedCollisionCheck circle1 circle2) True

        it "should collide circle2" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 0 9)) 5
            shouldBe (detailedCollisionCheck circle1 circle2) True

    describe "detailedCollision" $ do
        it "should not collide circle1" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 0 11)) 5
            shouldBe (detailedCollision 0 circle1 1 circle2) Nothing

        it "should not collide circle2" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 10 0)) 5
            shouldBe (detailedCollision 0 circle1 1 circle2) Nothing

        it "should collide circle1" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 0 9)) 5
            shouldBe (detailedCollision 0 circle1 1 circle2) (Just $ Collision 0 1 1 (V2 0 (-1.0)))

        it "should collide circle2" $ do
            let circle1 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            let circle2 = Circle (P (V2 0 0)) (P (V2 0 9)) 5
            shouldBe (detailedCollision 0 circle1 1 circle2) (Just $ Collision 0 1 1 (V2 0 (-1.0))) 

    describe "gatherCollisions" $ do
        it "no chances for collision - 0 entities" $ do
            let ep = fromList [] :: Vector (EntityPhysics Double)
            let entities = fromList []
            shouldBe (gatherCollisions ep entities) Set.empty

        it "no chances for collision - 1 entity" $ do
            let entityP1 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 1 1)) 5)) :: EntityPhysics Double
            let ep = fromList [entityP1]
            let entities = fromList [0]
            shouldBe (gatherCollisions ep entities) Set.empty

        it "no chances for collision" $ do
            let entityP1 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 1)) 5)) :: EntityPhysics Double
            let entityP2 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 12)) 5)) :: EntityPhysics Double
            let ep = fromList [entityP1, entityP2]
            let entities = fromList [0,1]
            shouldBe (gatherCollisions ep entities) Set.empty

        it "1 collision" $ do
            let entityP1 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 1)) 5)) :: EntityPhysics Double
            let entityP2 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 5)) 5)) :: EntityPhysics Double
            let ep = fromList [entityP1, entityP2]
            let entities = fromList [0,1]
            shouldBe (gatherCollisions ep entities) $ Set.fromList [Collision 0 1 6 (V2 0 (-1))]

        it "3 collisions" $ do
            let entityP1 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 1)) 5)) :: EntityPhysics Double
            let entityP2 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 5)) 5)) :: EntityPhysics Double
            let entityP3 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 5)) 5)) :: EntityPhysics Double
            let ep = fromList [entityP1, entityP2, entityP3]
            let entities = fromList [0,1,2]
            shouldBe (gatherCollisions ep entities) $ 
                Set.fromList [Collision 0 1 6 (V2 0 (-1)), Collision 0 2 6 (V2 0 (-1)), Collision 1 2 10 (V2 0 1)]

        it "3 entities no collisions" $ do
            let entityP1 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 1)) 1)) :: EntityPhysics Double
            let entityP2 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 5)) 1)) :: EntityPhysics Double
            let entityP3 = (EntityPhysics (V2 1 2) False False 10 (Circle (P (V2 0 0)) (P (V2 0 10)) 1)) :: EntityPhysics Double
            let ep = fromList [entityP1, entityP2, entityP3]
            let entities = fromList [0,1,2]
            shouldBe (gatherCollisions ep entities) $ Set.empty

    describe "project" $ do
        it "polygon" $ do
            let result = project (V2 0 1) $ Polygon empty (fromList [P (V2 1 1), P (V2 2 1), P (V2 1.5 2)]) empty  empty :: (Double, Double)
            shouldBe result (1.0,2.0)

        it "polygon2" $ do
            let result = project (V2 1 0) $ Polygon empty (fromList [P (V2 1 1), P (V2 2 1), P (V2 1.5 2)]) empty  empty :: (Double, Double)
            shouldBe result (1.0,2.0)

        it "polygon3" $ do
            let result = project (normalize $ V2 1 1) $ Polygon empty (fromList [P (V2 1 1), P (V2 2 1), P (V2 1.5 2)]) empty  empty :: (Double, Double)
            shouldBe result (1.414213562373095,2.474873734152916)

        it "circle" $ do
            let result = project (V2 0 1) (Circle (P (V2 0 0)) (P (V2 1 5)) 5) :: (Double, Double)
            shouldBe result (0.0,10.0)

        it "circle2" $ do
            let result = project (V2 1 0) (Circle (P (V2 0 0)) (P (V2 1 1)) 1) :: (Double, Double)
            shouldBe result (0.0,2.0)

        it "circle3" $ do
            let result = project (normalize $ V2 1 1) (Circle (P (V2 0 0)) (P (V2 1 1)) 1) :: (Double, Double)
            shouldBe result (0.4142135623730949,2.414213562373095)

        it "circle4" $ do
            let circle = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            shouldBe (project (V2 0 1) circle) ((-5), 5)

        it "circle5" $ do
            let circle = Circle (P (V2 0 0)) (P (V2 0 10)) 5 :: Shape Double
            shouldBe (project (V2 0 1) circle) (5, 15)

        it "aabb" $ do
            let result = project (V2 0 1) (AABB (P (V2 0 0)) (P (V2 2 2)) 1 1) :: (Double, Double)
            shouldBe result (1.0,3.0)

        it "aabb2" $ do
            let result = project (V2 1 0) (AABB (P (V2 0 0)) (P (V2 2 2)) 1 1) :: (Double, Double)
            shouldBe result (1.0,3.0)

        it "aabb3" $ do
            let result = project (normalize $ V2 1 1) (AABB (P (V2 0 0)) (P (V2 2 2)) 1 1) :: (Double, Double)
            shouldBe result (1.414213562373095,4.242640687119285)

    describe "edgeToAxis" $ do
        it "basic" $ do
            let axis = edgeToAxis (fromList [P (V2 0 1), P (V2 0 10)]) $ Edge 0 1 :: V2 Double
            shouldBe axis (V2 1 0)

        it "basic2" $ do
            let axis = edgeToAxis (fromList [P (V2 1 0), P (V2 10 0)]) $ Edge 0 1 :: V2 Double
            shouldBe axis (V2 (-0) (-1))

    describe "projectShapes" $ do
        it "circle1" $ do
            let axis = (V2 0 1)
            let shape1 = Circle (P (V2 0 0)) (P (V2 0 10)) 5 :: Shape Double
            let shape2 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            shouldBe (projectShapes shape1 shape2 axis) Nothing 

        it "circle1" $ do
            let axis = (V2 1 0)
            let shape1 = Circle (P (V2 0 0)) (P (V2 0 10)) 5 :: Shape Double
            let shape2 = Circle (P (V2 0 0)) (P (V2 0 0)) 5 :: Shape Double
            shouldBe (projectShapes shape1 shape2 axis) $ Just (10.0, V2 1.0 0.0)

    describe "toBuckets" $ do
        it "circles centered" $ do
            let shape' = Circle (P (V2 5 5 )) (P (V2 25 25)) 4.9 :: Shape Double
            shouldBe (toBuckets 10 10 100 shape') $ fromList [22]
        it "circle right leaning" $ do
            let shape' = Circle (P (V2 5 5 )) (P (V2 25.5 25)) 4.9 :: Shape Double
            shouldBe (toBuckets 10 10 100 shape') $ fromList [22, 23]
        it "circles left leaning" $ do
            let shape' = Circle (P (V2 5 5 )) (P (V2 24.5 25)) 4.9 :: Shape Double
            shouldBe (toBuckets 10 10 100 shape') $ fromList [21, 22]
        it "circles south leaning" $ do
            let shape' = Circle (P (V2 5 5 )) (P (V2 25 25.5)) 4.9 :: Shape Double
            shouldBe (toBuckets 10 10 100 shape') $ fromList [22, 32]
        it "circles hangs off board" $ do
            let shape' = Circle (P (V2 5 5 )) (P (V2 0 0)) 4.9 :: Shape Double
            shouldBe (toBuckets 10 10 100 shape') $ fromList [0]

