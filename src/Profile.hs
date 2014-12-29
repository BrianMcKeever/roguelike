import Components.Physics
import Game
import GameState
import Miscellaneous
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Vector as Vector

main :: IO ()
main = do
    gameData <- bouncingCirclesInitialData --initialGameData
    let gameData' = last $ take 300 $ iterate gameLoop gameData
    print $ Vector.last $ space $ physics gameData'
    return $ DeepSeq.deepseq gameData' ()

gameLoop :: GameData -> GameData
gameLoop gameData = DeepSeq.force $ seq picture inputedGameData
    where
    physicedGameData = update (1/60) gameData
    inputedGameData = handleInput undefined physicedGameData
    picture = draw inputedGameData
