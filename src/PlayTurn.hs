module PlayTurn( playTurn ) where
import Game

-- [1,2,3] -> [2,3,1]
rotatePlayers :: [Player] -> [Player]
rotatePlayers (x:xs) = reverse (x:(reverse xs))

advanceTurn :: CardsDeal -> CardsDeal
advanceTurn (MkCardsDeal players ac) = MkCardsDeal (rotatePlayers players) ac

-- adds the given turn to de game
playTurn :: ClueGame -> Turn -> ClueGame
playTurn (MkClueGame turns deal) t = MkClueGame (t:turns) (advanceTurn deal) 