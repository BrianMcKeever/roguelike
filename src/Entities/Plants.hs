module Entities.Plants (
    createTree,
    renderTree,
    tree
)
where
import Components.Transform
import Components.Renderable
import GameState
import EntityComponentSystem
import StringTable.Atom

createTree :: Position -> GameState -> IO GameState
createTree position gameState = do
    let (entity, gameState') = createEntity gameState tree
    (entity', gameState'') <- addTransform position gameState' entity
    return $ snd $ addRenderable gameState'' entity'

renderTree :: Float -> GameState -> Entity -> IO GameState
renderTree = basicRender Body "tree"

tree :: Kind
tree = toAtom "tree"
