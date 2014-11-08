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

createTree :: Position -> GameState Entity
createTree position = do
    entity <- createEntity tree
    let square = createSquare (15 * float2Double normalScale) (15 * float2Double normalScale)
    entity2 <- addPhysics 1 1 square entity
    entity3 <- addTransform position 4 4 entity2
    entity4 <- addRenderable entity3 renderTree
    return entity4

renderTree :: Float -> Entity -> GameState ()
renderTree = basicRender Body "tree"

tree :: Kind
tree = toAtom "tree"
