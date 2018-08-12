module Game where
import Clue( Suggestion, Accusation, Player, CardType )

type Turn = Suggestion

type ActualResult = Accusation

-- cards deal consist of wath the players have and what the actual murder was
data CardsDeal = MkCardsDeal [Player] ActualResult

data ClueGame = MkClueGame [Turn] CardsDeal

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

takeMaybeCard :: CardType -> [Card] -> Card
takeMaybeCard PlaceType (c@(MkCard f PlaceType):cs) = Just c
takeMaybeCard SuspectType (c@(MkCard f SuspectType):cs) = Just c
takeMaybeCard WeaponType (c@(MkCard f WeaponType):cs) = Just c
takeMaybeCard t (c:cs) = takeMaybeCard t cs
takeMaybeCard _ [] = Nothing

takePlaceCard :: [Card] -> Card
takePlaceCard = takeCard PlaceType

takeSuspectCard :: [Card] -> Card
takeSuspectCard = takeCard SuspectType

takeWeaponCard :: [Card] -> Card
takeWeaponCard = takeCard WeaponType
