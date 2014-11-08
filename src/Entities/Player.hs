module Entities.Player (
    createPlayer,
    getPlayer,
    player,
    renderPlayer
)
where
import Components.Physics
import Components.Renderable
import Components.SimpleMovement
import Components.Transform
import Control.Monad.State.Lazy
import GameState
import GHC.Float
import EntityComponentSystem
import StringTable.Atom

createPlayer :: Position -> GameState Entity
createPlayer position = do
    entity <- createEntity player
    let square = createSquare (12 * float2Double normalScale) (16 * float2Double normalScale)
    entity2 <- addPhysics 1 1 square entity
    entity3 <- addTransform position 4 4 entity2
    entity4 <- addSimpleMovement entity3
    gameData <- get
    put gameData{playerSerial = getSerial entity4}
    entity5 <- addRenderable entity4 renderPlayer
    return entity5

getPlayer :: GameData -> Entity
getPlayer gameData = getEntity (playerSerial gameData) gameData

renderPlayer :: Float -> Entity -> GameState ()
renderPlayer = basicRender Body "player"

player :: Kind
player = toAtom "player"
