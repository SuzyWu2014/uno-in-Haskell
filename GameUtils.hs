module GameUtils where

import UnoDataModels
import Config
-------------------------------------------------
-- Game initialization
-------------------------------------------------

initDeck :: Deck
initDeck = cardPile

initPlayer :: Int -> String -> PlayerState
initPlayer _id _name = PlayerState _id _name 0 []

initRobotPlayers :: Int -> [PlayerState]
initRobotPlayers 0 = []
initRobotPlayers _num = initPlayer _id (getRobotPlayerName _id) : initRobotPlayers (_num-1) 
    where _id = _num - 1 --index: 0 - num-1

getRobotPlayerName :: Int -> String
getRobotPlayerName _id = playerNames !! _id

playerNames::[String]
playerNames =["Alice", "Joe", "Mike", "Emily"]

pickStarter :: Int -> Int
pickStarter _num = div _num 2

initGame :: Int -> String -> GameState
initGame _num _name = GameState{
    dir = Clockwise,
    currClr = Green,
    realPlayer = _num,
    whoseTurn  = pickStarter _num,
    players = initRobotPlayers _num ++ [initPlayer _num _name],
    deck = initDeck
}  

dealCards :: Int -> GameState -> IO GameState
dealCards _num game@GameState{players=_players, deck=_deck} = 
    if _num >= 0 then do
        game'  <- drawCards 5 game _num
        dealCards (_num-1) game' 
    else
        return game
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

-- change whoseTurn?
--draw one card from deck
-- Type: GameState -> Player ID -> IO GameState 
drawCard :: GameState -> Int -> IO GameState
drawCard game@GameState{deck=(card:ds), players=_players} usr = return game{deck=ds, players=p}
  where
    u@(PlayerState _ _ _ _cardsInHand) = players game !! usr
    p = take usr _players ++ [u{cardsInHand=card:_cardsInHand}] ++
        drop (usr+1) _players

-- First Int: nums of cards to drae
-- Second Int: playerID
drawCards :: Int -> GameState -> Int -> IO GameState
drawCards 0 game _ = return game
drawCards n game usr = do
    game' <- drawCard game usr
    drawCards (n-1) game' usr

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



