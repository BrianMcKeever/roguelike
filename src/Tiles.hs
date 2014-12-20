module Tiles(
    loadTiles,
    numberHorizontalTiles,
    numberVerticalTiles,
    pixelScale,
    renderArea,
    renderTile
)
where
import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Graphics.Gloss.Data.Picture
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

-- | This loads the images of all of the tiles mentioned in the Tiled TiledMap
-- into a vector of pictures.
loadTiles :: Tiled.TiledMap -> IO (Vector.Vector Picture)
loadTiles tiledMap = do
    let layer = head $ Tiled.mapLayers tiledMap
    unless (Tiled.layerName layer == "floor") $ error "floor is not first layer"
    tiles <- mapM loadTileset $ Tiled.mapTilesets tiledMap
    return $ Vector.map (scale scaleFactor scaleFactor) $ Vector.concat (Vector.singleton Blank : tiles)
    --I'm adding a blank picture on the front because tileGids aren't 0 indexed

-- | This is an intermediate step of loadTiles that loads one tile set.
loadTileset :: Tiled.Tileset -> IO (Vector.Vector Picture)
loadTileset tileset = do
    let image = head $ Tiled.tsImages tileset --multiple images arent supported in Data.Tiled yet
        tileWidth = Tiled.tsTileWidth tileset
        tileHeight = Tiled.tsTileHeight tileset
    pic <- loadPicture $ Tiled.iSource image
    let xs = Vector.enumFromStepN 0 tileWidth $ quot (Tiled.iWidth image) tileWidth
        ys = Vector.enumFromStepN 0 tileHeight $ quot (Tiled.iHeight image) tileHeight
        tileCoordinates = flip (,) <$> ys <*> xs
    return $ Vector.map (loadTile pic tileWidth tileHeight) tileCoordinates

numberHorizontalTiles :: Int
numberHorizontalTiles = 2 * sightRangeY + 2
-- + 2 because 1 for the center tile and another for a nonInteger number of
-- tiles on the screen

numberVerticalTiles :: Int
numberVerticalTiles = 2 * sightRangeY + 2
-- + 2 because 1 for the center tile and another for a nonInteger number of
-- tiles on the screen

pixelScale :: Float
pixelScale = scaleFactor * tileDimension

-- | Returns a picture of the tiles on the screen centered on x,y
renderArea :: Vector.Vector Picture -> Tiled.TiledMap -> (Float, Float) -> Picture
renderArea tiles tiledMap (x, y) = translate xOffset yOffset $ pictures tilePictures
    where
    layer = head $ Tiled.mapLayers tiledMap -- we are ensuring the first layer is the floor layer in loadTiles
    xs = Vector.enumFromStepN (floor x - sightRangeX) 1 numberHorizontalTiles
    ys = Vector.enumFromStepN (floor y - sightRangeY) 1 numberVerticalTiles
    tileCoordinates = flip (,) <$> ys <*> xs
    layerData = Tiled.layerData layer
    tilePictures = Vector.toList $ Vector.map (renderTile tiles layerData) $ Vector.zip tilePositions tileCoordinates
    xOffset = (-(snd $ (properFraction x :: (Int, Float))) * pixelScale)
    yOffset = (snd $ (properFraction y :: (Int, Float))) * pixelScale

-- | Returns a picture of the tile at map coordinate coordinates at xOffset,
-- yOffset
renderTile :: Vector.Vector Picture -> Map.Map (Int, Int) Tiled.Tile -> ((Float, Float), (Int, Int)) -> Picture
renderTile tilePictures layerData ((xOffset, yOffset), coordinates) = translate xOffset yOffset tilePicture
    where
    tileMaybe = Map.lookup coordinates layerData 
    tilePicture = case tileMaybe of
        Nothing -> Blank
        (Just tile) -> tilePictures Vector.! (fromIntegral $ Tiled.tileGid tile)

scaleFactor :: Float
scaleFactor = 4

sightRangeX :: Int
sightRangeX = 5

sightRangeY :: Int
sightRangeY = 5

--This is the size of one side of our square tiles
tileDimension :: Float
tileDimension = 16

-- | This a vector of the positions of all of the tiles that should be on the
-- screen.
tilePositions :: Vector.Vector (Float, Float)
tilePositions = flip (,) <$> ys <*> xs
    where
    xs = (Vector.enumFromStepN (- fromIntegral sightRangeX * pixelScale) (pixelScale) numberHorizontalTiles)
    ys = (Vector.enumFromStepN (fromIntegral sightRangeY * pixelScale) (-pixelScale) numberVerticalTiles)
