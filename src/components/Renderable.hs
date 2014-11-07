module Components.Renderable (
    addRenderable,
    basicRender,
    normalScale,
    renderableComponent,
    RenderData(..),
    ZIndex(..)
)
where
import Components.Transform
import Components.RenderableBase
import Control.Applicative
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.List.Ordered
import EntityComponentSystem
import GameState
import Graphics.Gloss.Game

addRenderable :: Entity -> GameState Entity
addRenderable = addComponent renderableComponent

basicRender :: ZIndex -> String -> Float -> Entity -> GameState ()
basicRender zindex tileName _ entity = do
    gameData <- get
    let tile = tiles gameData Map.! tileName
    (x, y) <- positionToPoint <$> getPosition entity
    let (scaleX, scaleY) = getScale gameData entity
    let tile' = translate x y $ scale scaleX scaleY tile
    let renderD = RenderData zindex tile'
    let renderData' = insertBag renderD $ toBeRendered gameData
    put gameData{toBeRendered = renderData'}

normalScale :: Float
normalScale = 4
