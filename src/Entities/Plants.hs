module Entities.Plants (
    createTree,
    renderTree,
    tree
)
where
import Components.Physics
import Components.Renderable
import Components.Transform
import GameState
import GHC.Float
import EntityComponentSystem
import StringTable.Atom

createTree :: Position -> GameState -> IO GameState
createTree position gameState = do
    let (entity, gameState') = createEntity gameState tree
    let square = createSquare (15 * float2Double normalScale) (15 * float2Double normalScale)
    (entity', gameState'') <- addPhysics 1 1 square gameState' entity
    (entity'', gameState3) <- addTransform position 4 4 gameState'' entity'
    return $ snd $ addRenderable gameState3 entity''

renderTree :: Float -> GameState -> Entity -> IO GameState
renderTree = basicRender Body "tree"

tree :: Kind
tree = toAtom "tree"
