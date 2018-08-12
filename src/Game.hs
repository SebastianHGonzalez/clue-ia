module Game where
import Clue( Suggestion, Accusation, Player)

type Turn = Suggestion

type ActualResult = Accusation

-- cards deal consist of wath the players have and what the actual murder was
data CardsDeal = MkCardsDeal [Player] ActualResult

data ClueGame = MkClueGame [Turn] CardsDeal
