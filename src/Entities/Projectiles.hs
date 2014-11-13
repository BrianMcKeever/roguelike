module Entities.Projectiles (
    createLightningBall,
    renderLightningBall
)
where
import Components.Renderable
import Components.TransformAndPhysics
import EntityComponentSystem
import GameState
import GHC.Float

createLightningBall :: Position -> GameState Entity
createLightningBall position = do
    entity <- createEntity
    let square = createSquare (5 * float2Double normalScale) (5 * float2Double normalScale)

    addPhysics collisionTypeDOA 1 1 square entity False
    addTransform position 4 4 entity
    addRenderable entity renderLightningBall
    return entity

renderLightningBall :: Float -> Entity -> GameState ()
renderLightningBall = basicRender Body "lightningBall"
