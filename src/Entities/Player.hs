module Entities.Player (
    createPlayer,
    player,
    renderPlayer
)
where
import Components.Position
import Components.Renderable
import GameState
import EntityComponentSystem
import StringTable.Atom

createPlayer :: Point -> GameState -> GameState
createPlayer point gameState = gameState3
    where
    (entity, gameState') = createEntity gameState player
    (entity', gameState'') = addPosition point gameState' entity
    gameState3 = snd $ addRenderable gameState'' entity'

renderPlayer :: Float -> GameState -> Entity -> GameState
renderPlayer = basicRender Body "player"

player :: Kind
player = toAtom "player"
