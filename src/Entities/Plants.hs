module Entities.Plants (
    createTree,
    renderTree
)
where
import Components.Renderable
import Components.TransformAndPhysics
import EntityComponentSystem
import GameState
import GHC.Float

createTree :: Position -> GameState Entity
createTree position = do
    entity <- createEntity
    let square = createSquare (15 * float2Double normalScale) (15 * float2Double normalScale)

    addPhysics collisionTypeNormal 1 1 square entity True
    addTransform position 4 4 entity
    addRenderable entity renderTree
    return entity

renderTree :: Float -> Entity -> GameState ()
renderTree = basicRender Body "tree"
