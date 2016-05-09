module GameUtils where

import UnoDataModels
-------------------------------------------------
-- Game initialization
-------------------------------------------------
getNumOfPlayers:: IO()
getNumOfPlayers = undefined

initDeck :: Deck
initDeck =undefined

shuffle :: Deck -> Deck
shuffle = undefined

initPlayer :: (Deck , [PlayerState])
initPlayer = undefined

initGame :: GameState
initGame = undefined

pickStartPlayer :: Int -> Int
pickStartPlayer = undefined

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

--draw one card from deck
drawCard :: GameState -> GameState
drawCard = undefined

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

-------------------------------------------------
-- AI playing
-------------------------------------------------



