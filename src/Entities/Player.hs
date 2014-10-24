module Entities.Player (
    createPlayer,
    player,
    renderPlayer
)
where
import Components.Physics
import Components.Renderable
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
    return $ snd $ addRenderable gameState3 entity''

renderPlayer :: Float -> GameState -> Entity -> IO GameState
renderPlayer = basicRender Body "player"

player :: Kind
player = toAtom "player"
