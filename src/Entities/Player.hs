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
import GameState
import GHC.Float
import EntityComponentSystem
import StringTable.Atom

createPlayer :: Position -> GameState -> IO GameState
createPlayer position gameState = do
    let (entity, gameState') = createEntity gameState player
    let square = createSquare (12 * float2Double normalScale) (16 * float2Double normalScale)
    (entity', gameState'') <- addPhysics 1 1 square gameState' entity
    (entity'', gameState3) <- addTransform position 4 4 gameState'' entity'
    let (entity3, gameState4) = addSimpleMovement gameState3 entity''
    let gameState5 = gameState4{playerSerial = getSerial entity3}
    return $ snd $ addRenderable gameState5 entity3

getPlayer :: GameState -> Entity
getPlayer gameState = getEntity (playerSerial gameState) gameState

renderPlayer :: Float -> GameState -> Entity -> IO GameState
renderPlayer = basicRender Body "player"

player :: Kind
player = toAtom "player"
