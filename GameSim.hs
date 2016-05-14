module GameSim where

import UnoDataModels
import Utils
import Config
import System.Random
import Control.Monad.State

-------------------------------------------------
-- Game initialization
-------------------------------------------------

shuffle :: StdGen -> Deck -> Deck
shuffle gen []      = []
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

initGame :: Int -> String -> Game ()
initGame _num _name = do
    put (initGameState _num _name)
    game <- get
    dealCards 5 game

dealCards :: Int -> GameState -> Game ()
dealCards _num game@GameState{players=_players, deck=_deck} = 
    if _num >= 0 then do
        let game'  = drawCards 5 game _num
        dealCards (_num-1) game' 
    else
        put game
-------------------------------------------------
-- Game State print
-------------------------------------------------
showState :: GameState -> String
showState = undefined

showCurrentPlayerInfo :: GameState -> String
showCurrentPlayerInfo = undefined

showCurrentCard :: GameState -> String
showCurrentCard = undefined

showCardsInHand :: [(Int, Card)] -> String
showCardsInHand = undefined

-------------------------------------------------
-- Each playing turn
-------------------------------------------------


playableCard :: [(Int, Card)] -> [(Int, Card)]
playableCard = undefined

matchColor :: Card -> Bool
matchColor = undefined

matchNum :: Card -> Bool
matchNum = undefined

--IO choose card to play and perform the effect
dropCard :: GameState -> GameState
dropCard = undefined

-- IO pick an color and update GameState
pickColor :: GameState -> GameState
pickColor = undefined

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

isOver :: GameState -> Bool
isOver = undefined

-------------------------------------------------
-- AI playing
-------------------------------------------------



