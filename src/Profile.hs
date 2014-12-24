import Game
import GameState
import Miscellaneous
import qualified Control.DeepSeq as DeepSeq

main :: IO ()
main = do
    gameData <- bouncingCirclesInitialData --initialGameData
    let gameData' = last $ take 3000000000 $ iterate gameLoop gameData
    return $ DeepSeq.deepseq gameData' ()

gameLoop :: GameData -> GameData
gameLoop gameData = DeepSeq.force $ seq picture inputedGameData
    where
    physicedGameData = update (1/60) gameData
    inputedGameData = handleInput undefined physicedGameData
    picture = draw inputedGameData
