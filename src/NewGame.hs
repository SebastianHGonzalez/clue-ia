module NewGame ( newGame ) where
import Clue
import Game
import Data.List

-- TODO: SHUFFLE
shuffle :: [a] -> [a]
shuffle a = a

shuffledCards :: [Card]
shuffledCards = shuffle cards

-- unsafe
-- explodes if card with cardtype not in [card]
takeCard :: CardType -> [Card] -> Card
takeCard PlaceType (c@(MkCard f PlaceType):cs) = c
takeCard SuspectType (c@(MkCard f SuspectType):cs) = c
takeCard WeaponType (c@(MkCard f WeaponType):cs) = c
takeCard t (c:cs) = takeCard t cs

takePlaceCard :: [Card] -> Card
takePlaceCard = takeCard PlaceType

takeSuspectCard :: [Card] -> Card
takeSuspectCard = takeCard SuspectType

takeWeaponCard :: [Card] -> Card
takeWeaponCard = takeCard WeaponType

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