module Entities.Plants (
    createTree,
    renderTree
)
where
import Components.Renderable
import Components.TransformAndPhysics
import GameState
import GHC.Float
import EntityComponentSystem

createTree :: Position -> GameState Entity
createTree position = do
    entity <- createEntity
    let square = createSquare (15 * float2Double normalScale) (15 * float2Double normalScale)
    addPhysics 1 1 square entity
    addTransform position 4 4 entity
    addRenderable entity renderTree
    return entity

renderTree :: Float -> Entity -> GameState ()
renderTree = basicRender Body "tree"
