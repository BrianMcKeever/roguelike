module Tiles(
    loadTiles
)
where
import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Graphics.Gloss.Game
import Graphics.Gloss.Juicy
import Data.Maybe as Maybe
import qualified Data.Tiled as Tiled
import qualified Data.Vector as Vector

crop :: Int -> Int -> Int -> Int -> DynamicImage -> DynamicImage
crop leftOffset topOffset width height = dynamicPixelMap squareImage
    where
    squareImage :: Pixel a => Image a -> Image a
    squareImage img = generateImage (\x y -> pixelAt img (leftOffset + x) (topOffset + y)) width height

dynamicToPicture :: DynamicImage -> Picture
dynamicToPicture = Maybe.fromJust . fromDynamicImage

loadPicture :: FilePath -> IO DynamicImage
loadPicture path = readImage ("assets/" ++ path) >>= either error return

loadTile :: DynamicImage -> Int -> Int -> (Int, Int) -> Picture
loadTile sheet width height (leftOffset, topOffset) = 
    dynamicToPicture $ crop leftOffset topOffset width height sheet

loadTiles :: Tiled.TiledMap -> IO (Vector.Vector Picture)
loadTiles tilemap = do
    tiles <- mapM loadTileset $ Tiled.mapTilesets tilemap
    return $ Vector.fromList $ concat tiles

loadTileset :: Tiled.Tileset -> IO [Picture]
loadTileset tileset = do
    let image = head $ Tiled.tsImages tileset --multiple images arent supported in Data.Tiled yet
        tileWidth = Tiled.tsTileWidth tileset
        tileHeight = Tiled.tsTileHeight tileset
    pic <- loadPicture $ Tiled.iSource image
    let xs = [0, tileWidth .. Tiled.iWidth image - tileWidth] 
    let ys = [0, tileHeight .. Tiled.iHeight image - tileHeight] 
    let tileCoordinates = flip (,) <$> ys <*> xs
    return $ map (loadTile pic tileWidth tileHeight) tileCoordinates
