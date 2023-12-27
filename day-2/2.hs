import Data.Char (toUpper)
import Data.List
import Data.Array (indices)
import GHC.Num (naturalAdd)

isRoundPossible :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> Bool
isRoundPossible (red, green, blue) (confRed, confGreen, confBlue)
    | red > confRed || green > confGreen || blue > confBlue = False
    | otherwise                                             = True  

isGamePossible :: Integer -> [(Integer, Integer, Integer)] -> (Integer, Integer, Integer) -> Integer
isGamePossible gameId (c:gameCubes) gameConf
    | not $ isRoundPossible c gameConf = 0
    | null gameCubes                   = gameId 
    | isRoundPossible c gameConf       = isGamePossible gameId gameCubes gameConf

evaluteAllGames :: [(Integer, [(Integer, Integer, Integer)])] -> (Integer, Integer, Integer) -> Integer
evaluteAllGames ((gameId, game):allGames) gameConf
    | null allGames = acc
    | otherwise     = acc + evaluteAllGames allGames gameConf
    where
        acc = isGamePossible gameId game gameConf

main :: IO ()
main = do
    putStrLn "Results of game evaluation"
    print $ isGamePossible 123 [(1,1,1), (1,1,2), (1,3,2)] (1,3,2)
    print $ evaluteAllGames [(10, [(1,1,1), (1,1,2), (3,3,3)]), (20, [(1,1,1), (1,1,2), (1,3,2)]), (30, [(5,1,1), (1,1,2), (3,3,3)]), (40, [(1,1,1), (1,1,2), (3,3,3)])] (4,4,4)
