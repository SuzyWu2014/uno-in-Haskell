module GameSim where

import UnoDataModels
import Utils
import Config
import System.Random
import Control.Monad.State
import Control.Monad

-------------------------------------------------
-- Game initialization
-------------------------------------------------

shuffle :: StdGen -> Deck -> Deck
shuffle _ []        = []
shuffle gen (x:xs)  = take ind rec ++ [x] ++ drop ind rec
    where (n, gen') = random gen :: (Int, StdGen)
          ind       = mod n $ length xs + 1
          rec       = shuffle gen' xs

initDeck :: Deck
initDeck = shuffle (mkStdGen 1) cardPile

initPlayer :: Int -> String -> PlayerState
initPlayer _id _name = PlayerState _id _name 0 []

initRobotPlayers :: Int -> [PlayerState]
initRobotPlayers 0 = []
initRobotPlayers _num = initPlayer _id (getRobotPlayerName _id) : initRobotPlayers (_num-1) 
    where _id = _num - 1 --index: 0 - num-1

getRobotPlayerName :: Int -> String
getRobotPlayerName _id = playerNames !! _id

playerNames::[String]
playerNames = ["Alice", "Joe", "Mike", "Emily"]

pickStarter :: Int -> Int
pickStarter _num = div _num 2

initGameState :: Int -> String -> GameState
initGameState _num _name = GameState{
    dir = Clockwise,
    currClr = Green,
    realPlayer = _num,
    currCard = head initDeck, -- for test purpose
    whoseTurn  = pickStarter _num,
    players = initPlayer _num _name :initRobotPlayers _num,
    deck = initDeck
}  

dealCards :: GameState -> GameState
dealCards game@GameState{players=_players} = dealing (length _players -1) game

-- @Int num of players 
dealing :: Int -> GameState -> GameState
dealing _num game@GameState{players=_players, deck=_deck} 
                                | _num >= 0 =  dealing (_num-1) (drawCards 5 game _num) 
                                | otherwise = game                         

setStartingCard :: Game ()
setStartingCard = do 
    game@GameState{deck=_deck} <- get 
    put game{currCard=head _deck, deck=tail _deck}

-------------------------------------------------
-- Game State print
-------------------------------------------------
showState :: Game ()
showState = do 
    game <- get 
    lift $ putStr $ "Current Direction - " ++ show (dir game) ++"\n"
    lift $ putStr $ "Current Card - " ++ show (currCard game) ++"\n"
    let _currCard = currCard game
    Control.Monad.when
      (cardType _currCard == Wild || cardType _currCard == WildDrawFour)
      $ lift $ putStr $ "Current Color - " ++ show (currClr game) ++ "\n"

    
showCurrentPlayerInfo :: GameState -> String
showCurrentPlayerInfo = undefined

showCurrentCard :: GameState -> String
showCurrentCard = undefined

showCardsInHand :: [(Int, Card)] -> String
showCardsInHand = undefined

-------------------------------------------------
-- Each playing turn
-------------------------------------------------
isOver :: GameState -> Bool
isOver game =  null (deck game) || isEscaped (players game)
    

isEscaped :: [PlayerState] -> Bool
isEscaped [] = False
isEscaped (PlayerState _ _ _ _cards:_players) = null _cards || isEscaped _players

-- getPlayableCardList :: GameState -> [Card]
-- getPlayableCardList game{whoseTurn=_whoseTurn, players=_players} = playableCardListSubStep 

isMatch :: Card -> Card -> Bool
isMatch _card _currCard = num _card == num _currCard || clr _card == clr _currCard

-- @Int Player ID
getCurrPlayer :: Int -> GameState -> PlayerState
getCurrPlayer _id game@GameState{players=(p:_players)} = if _id == _pId then p else getCurrPlayer _id game{players=_players}
     where _pId = UnoDataModels.id p
-- IO pick an color and update GameState

checkUno :: Int -> Bool
checkUno = undefined

declareUno :: PlayerState -> String
declareUno = undefined

unoPenalty :: PlayerState -> PlayerState
unoPenalty = undefined

checkWinner :: PlayerState -> Bool
checkWinner = undefined

updateScore :: PlayerState -> PlayerState
updateScore = undefined


-------------------------------------------------
-- AI playing
-------------------------------------------------



