module Tiles(
    loadTiles
)
where
import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss.Game
import Graphics.Gloss.Juicy
import Data.Maybe as Maybe
import qualified Data.Map as Map

crop :: Int -> Int -> Int -> Int -> DynamicImage -> DynamicImage
crop leftOffset topOffset width height = dynamicPixelMap squareImage
    where
    squareImage :: Pixel a => Image a -> Image a
    squareImage img = generateImage (\x y -> pixelAt img (leftOffset + x) (topOffset + y)) width height

dynamicToPicture :: DynamicImage -> Picture
dynamicToPicture = Maybe.fromJust . fromDynamicImage

loadImage :: FilePath -> IO DynamicImage
loadImage path = readImage path >>= either error return

loadSheet :: String -> FilePath -> Map.Map String (IO DynamicImage) -> Map.Map String (IO DynamicImage)
loadSheet name path = Map.insert name (loadImage path) 

loadSheets :: Map.Map String (IO DynamicImage)
loadSheets = Map.foldWithKey loadSheet Map.empty tileSheetPaths

loadTile :: Map.Map String (IO DynamicImage) -> String -> TileSheetPosition -> IO (Map.Map String Picture) -> IO (Map.Map String Picture)
loadTile sheets name (TileSheetPosition sheetName leftOffset topOffset width height) tiles = do
    tiles' <- tiles
    sheet <- Maybe.fromJust $ Map.lookup sheetName sheets
    return $ Map.insert name (dynamicToPicture $ crop leftOffset topOffset width height sheet) tiles'

loadTiles :: IO (Map.Map String Picture)
loadTiles = Map.foldWithKey (loadTile loadSheets) (return Map.empty) tileSheetPositions

data TileSheetPosition = TileSheetPosition String    Int          Int      Int    Int
--                                        sheetName leftOffset  topOffset width height

tileSheetPositions :: Map.Map String TileSheetPosition
tileSheetPositions = Map.fromList [
    ("player", TileSheetPosition "player0" 96 64 16 16),
    ("grass", TileSheetPosition "floor" 128 112 16 16),
    ("tree", TileSheetPosition "tree0" 48 0 16 16)
    ] 

tileSheetPaths :: Map.Map String String
tileSheetPaths = Map.fromList [
    ("floor", "Assets/DawnLike_3/Objects/Floor.png"),
    ("player0", "Assets/DawnLike_3/Characters/Player0.png"),
    ("tree0", "Assets/DawnLike_3/Objects/Tree0.png"),
    ("tree1", "Assets/DawnLike_3/Objects/Tree1.png") 
    ] 
