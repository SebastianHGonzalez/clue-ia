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

cards = [
            MkCard White SuspectType,
            MkCard Blue SuspectType,
            MkCard Green SuspectType,
            MkCard Orange SuspectType,
            MkCard Yellow SuspectType,
            MkCard Red SuspectType,
            MkCard Study PlaceType,
            MkCard Kitchen PlaceType,
            MkCard Library PlaceType,
            MkCard DiningRoom PlaceType,
            MkCard LivingRoom PlaceType,
            MkCard Basement PlaceType,
            MkCard GamesRoom PlaceType,
            MkCard BilliardRoom PlaceType,
            MkCard StartRoom PlaceType,
            MkCard Pistol WeaponType,
            MkCard Knife WeaponType,
            MkCard Chandelier WeaponType,
            MkCard Pipe WeaponType,
            MkCard Rope WeaponType,
            MkCard Wrench WeaponType
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