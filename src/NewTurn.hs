module NewTurn( newTurn ) where
import Clue ( Card )
import Game
import NewAccusation

players :: ClueGame -> [Player]
players (MkClueGame _ (MkCardsDeal players _)) = players

firstValue :: [Maybe a] -> Maybe a
firstValue ((Just x):_) = Just x
firstValue (Nothing:xs) = firstValue xs
firstValue [] = Nothing

matchCard :: [Card] -> Player -> Maybe Player
matchCard (x:xs) p = if elem x p then Just p else matchCard xs p
matchCard [] _ = Nothing

playerWithMatch :: Accusation -> [Player] -> Maybe Player
playerWithMatch (MkAccusation place suspect weapon) ps = firstValue (map (matchCard [(makePlaceCard place), (makeSuspectCard suspect), (makeWeaponCard weapon)]) ps)

currentPlayer :: ClueGame -> Player
currentPlayer game = head (players game)

newTurn :: ClueGame -> Turn
newTurn game = let accusation = newAccusation
               in MkSuggestion (currentPlayer game) accusation (playerWithMatch accusation (players game))