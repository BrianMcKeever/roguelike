module Components.PhysicsSpec (main, spec) 
where

import Components.Physics
import EntityComponentSystem
import Test.Hspec
import qualified Data.List as List
import Data.Vector as Vector
import Linear.Affine
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
            let entityP = (EntityPhysics unnessary unnessary unnessary unnessary (Circle unnessary (P (V2 1 1)) 5)) :: EntityPhysics Double
            let input = zip (replicate 1 physicsMask) (replicate 1 entityP)
            shouldBe (createSpace 10 10 10 50 input) (fromList $ List.map fromList [[0],[],[],[],[]])

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

