module Entities.Plants (
    basicRender,
    createTree,
    tree,
    renderTree
)
where
import Components.Position
import Components.Renderable
import GameState
import EntityComponentSystem
import StringTable.Atom

createTree :: Point -> GameState -> GameState
createTree point gameState = gameState3
    where
    (entity, gameState') = createEntity gameState tree
    (entity', gameState'') = addPosition point gameState' entity
    gameState3 = snd $ addRenderable gameState'' entity'

renderTree :: Float -> GameState -> Entity -> GameState
renderTree = basicRender Body "tree"

tree :: Kind
tree = toAtom "tree"
