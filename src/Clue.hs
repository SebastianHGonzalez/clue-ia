module Clue where

data Figure = White
            | Blue
            | Green
            | Orange
            | Yellow
            | Red
            | Study
            | Kitchen
            | Library
            | DiningRoom
            | LivingRoom
            | Basement
            | GamesRoom
            | BilliardRoom
            | StartRoom
            | Pistol
            | Knife
            | Chandelier
            | Pipe
            | Rope
            | Wrench

data CardType = SuspectType | PlaceType | WeaponType

data Card = MkCard Figure CardType

type Player = [Card]

type Place = Figure

type Suspect = Figure

type Weapon = Figure

data Accusation = MkAccusation Place Suspect Weapon

data Suggestion = MkSuggestion Player Accusation (Maybe Player)

figureOf :: Card -> Figure
figureOf (MkCard f _) = f

makeCard :: CardType -> Figure -> Card
makeCard t f = MkCard f t

makeSuspectCard = makeCard SuspectType
makePlaceCard = makeCard PlaceType
makeWeaponCard = makeCard WeaponType

cards = [
            makeSuspectCard White,
            makeSuspectCard Blue,
            makeSuspectCard Green,
            makeSuspectCard Orange,
            makeSuspectCard Yellow,
            makeSuspectCard Red,
            makePlaceCard Study,
            makePlaceCard Kitchen,
            makePlaceCard Library,
            makePlaceCard DiningRoom,
            makePlaceCard LivingRoom,
            makePlaceCard Basement,
            makePlaceCard GamesRoom,
            makePlaceCard BilliardRoom,
            makePlaceCard StartRoom,
            makeWeaponCard Pistol,
            makeWeaponCard Knife,
            makeWeaponCard Chandelier,
            makeWeaponCard Pipe,
            makeWeaponCard Rope,
            makeWeaponCard Wrench
        ]

instance Eq Figure where
    (==) White White = True
    (==) Blue Blue = True
    (==) Green Green = True
    (==) Orange Orange = True
    (==) Yellow Yellow = True
    (==) Red Red = True
    (==) Study Study = True
    (==) Kitchen Kitchen = True
    (==) Library Library = True
    (==) DiningRoom DiningRoom = True
    (==) LivingRoom LivingRoom = True
    (==) Basement Basement = True
    (==) GamesRoom GamesRoom = True
    (==) BilliardRoom BilliardRoom = True
    (==) StartRoom StartRoom = True
    (==) Pistol Pistol = True
    (==) Knife Knife = True
    (==) Chandelier Chandelier = True
    (==) Pipe Pipe = True
    (==) Rope Rope = True
    (==) Wrench Wrench = True
    (==) _ _ = False

instance Eq CardType where
    (==) SuspectType SuspectType = True
    (==) PlaceType PlaceType = True
    (==) WeaponType WeaponType = True
    (==) _ _ = False
    

instance Eq Card where
    (==) (MkCard f ct) (MkCard f2 ct2) = (f Prelude.== f2) && (ct Prelude.== ct2)