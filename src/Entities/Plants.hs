module Entities.Plants (
    createTree,
    renderTree
)
where
import Components.Renderable
import Components.TransformAndPhysics
import Control.Monad.State.Lazy
import EntityComponentSystem
import GameState
import GHC.Float
import qualified Physics.Hipmunk as H

createTree :: Position -> GameState Entity
createTree position = do
    entity <- createEntity
    let square = createSquare (15 * float2Double normalScale) (15 * float2Double normalScale)

    addPhysics 1 1 square entity True
    addTransform position 4 4 entity
    gameData <- get
    liftIO $ H.rehashStatic $ space gameData
    addRenderable entity renderTree
    return entity

renderTree :: Float -> Entity -> GameState ()
renderTree = basicRender Body "tree"
