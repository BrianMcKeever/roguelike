module Entities.Player (
    createPlayer,
    player,
    renderPlayer
)
where
import Components.Transform
import Components.Renderable
import GameState
import EntityComponentSystem
import StringTable.Atom

createPlayer :: Position -> GameState -> IO GameState
createPlayer position gameState = do
    let (entity, gameState') = createEntity gameState player
    (entity', gameState'') <- addTransform position gameState' entity
    return $ snd $ addRenderable gameState'' entity'

renderPlayer :: Float -> GameState -> Entity -> IO GameState
renderPlayer = basicRender Body "player"

player :: Kind
player = toAtom "player"
