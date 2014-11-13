module Entities.Player (
    createPlayer,
    renderPlayer
)
where
import Components.Renderable
import Components.SimpleMovement
import Components.TransformAndPhysics
import Control.Monad.State.Lazy
import GameState
import GHC.Float
import EntityComponentSystem

createPlayer :: Position -> GameState Entity
createPlayer position = do
    entity <- createEntity
    let square = createSquare (12 * float2Double normalScale) (16 * float2Double normalScale)
    addPhysics collisionTypeNormal 1 1 square entity False
    addTransform position 4 4 entity
    addSimpleMovement entity
    gameData <- get
    put gameData{player = entity}
    addRenderable entity renderPlayer
    return entity

renderPlayer :: Float -> Entity -> GameState ()
renderPlayer = basicRender Body "player"
