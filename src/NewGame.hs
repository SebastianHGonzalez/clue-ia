module NewGame ( newGame ) where
import Clue
import Game
import Data.List

dealCardsAmmount :: [Card] -> Int -> [Player]
dealCardsAmmount [] amount = []
dealCardsAmmount cs amount = let player = take amount cs
                                in (player:(dealCardsAmmount (cs \\ player) amount))

dealPlayerCards :: [Card] -> Int -> [Player]
dealPlayerCards cards playerAmount = dealCardsAmmount cards (div (length cards) playerAmount)

dealCards :: [Card] -> Int -> CardsDeal
dealCards cs playerAmount = let placeCard = takePlaceCard cs
                                in let suspectCard = takeSuspectCard cs
                                in let weaponCard = takeWeaponCard cs
                                in let place = figureOf placeCard
                                in let suspect = figureOf suspectCard
                                in let weapon = figureOf weaponCard
                                in let playerCards = cs \\ [placeCard, suspectCard, weaponCard]
                                in MkCardsDeal (dealPlayerCards playerCards playerAmount) (MkAccusation place suspect weapon)

-- creates a game with the given number of players
-- number of players mut be 6 or under
newGame :: Int -> ClueGame
newGame playerAmount = MkClueGame [] (dealCards shuffledCards playerAmount)