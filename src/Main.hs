import Components.Position
import Components.Renderable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import EntityComponentSystem
import GameState
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game
import Tiles
import World

hasComponent :: Entity -> Component -> Bool
hasComponent (Entity _ components) component = Set.member component components

draw :: GameState -> Picture
draw gameState = pictures $ toBeRendered gameState

{-[ 
    translate  100  100 (tiles Map.! "grass"),
    translate  100  100 (tiles Map.! "tree")
    ]
    -}
    
handleInput :: Event -> GameState -> GameState
handleInput _event gameState = gameState

main :: IO ()
main = do
    tiles' <- loadTiles
    let gameState = loadMap initialGameState {tiles = tiles'}
    play (InWindow windowTitle (600, 400) (50, 50)) white 30 gameState draw handleInput update

rectangleToCornerPoints :: Rect -> (Point, Point)
rectangleToCornerPoints ((x, y), (width, height)) = (((x - halfWidth), (y - halfHeight)), ((x + halfWidth), (y + halfHeight)))
    where
    halfWidth = width/2
    halfHeight = height/2

update :: Float -> GameState -> GameState
update tick = updateGraphics tick . updateGame tick 

updateEntityGraphic :: Float -> GameState -> Entity -> GameState
updateEntityGraphic tick gameState entity@(Entity serial _) = ((renderFunctions gameState) Map.! serial) tick gameState entity

updateGame :: Float -> GameState -> GameState
updateGame tick gameState = List.foldl' updateEntity gameState $ Map.keys $ entities gameState 
    where
    ifEntity gameState' serial f = maybe gameState' f $ Map.lookup serial $ entities gameState'

    updateEntity :: GameState -> Serial -> GameState
    updateEntity gameState' serial = ifEntity gameState' serial f
        where
        f (Entity _ components) = componentFoldl (updateComponent serial) gameState' components
    -- I'm passing serial numbers instead of entity instances because
    -- passing old copies of possibly updated entities would cause
    -- problems.    

    updateComponent :: Serial -> GameState -> Component -> GameState
    updateComponent serial gameState' component = ifEntity gameState' serial f
        where
        f entity@(Entity _ components) = if Set.notMember component components
            then gameState' 
            else (updateFunctions Map.! component) tick gameState' entity
    --Passing old copies of components doesn't matter because they store their
    --state in game state.

updateGraphics :: Float -> GameState -> GameState
-- Get entities that are renderable.
-- Keep the entities that are within the screen area plus half the width of our
-- biggest sprite.
-- Tell these entities to prepare their pictures.
updateGraphics tick gameState = Map.foldl' (updateEntityGraphic tick) gameState entities'
    where
    box = ((0, 0), (100, 100))
    willBeShown entity = hasComponent entity renderableComponent && withinBox gameState box entity
    entities' = Map.filter willBeShown $ entities gameState

windowTitle :: String
windowTitle = "Rogue Bard"

withinBox :: GameState -> Rect -> Entity -> Bool
withinBox gameState box entity = pointInBox entityPosition leftCorner rightCorner
    where
    entityPosition = getPosition (positionState gameState) entity
    (leftCorner, rightCorner) = rectangleToCornerPoints box
