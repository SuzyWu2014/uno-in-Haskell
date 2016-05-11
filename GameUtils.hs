module GameUtils where

import UnoDataModels
import CardEffects
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

cardPile :: [Card]
cardPile =            [Card{num=Just 0, clr=Just Red,     effect=regular, cardType=Regular, desc="red 0"}]
        ++            [Card{num=Just 0, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 0"}]
        ++            [Card{num=Just 0, clr=Just Yellow,  effect=regular, cardType=Regular, desc="blue 0"}]
        ++            [Card{num=Just 0, clr=Just Yellow,  effect=regular, cardType=Regular, desc="green 0"}]
        ++ replicate 2 Card{num=Just 1, clr=Just Red,     effect=regular, cardType=Regular, desc="red 1"}
        ++ replicate 2 Card{num=Just 2, clr=Just Red,     effect=regular, cardType=Regular, desc="red 2"}
        ++ replicate 2 Card{num=Just 3, clr=Just Red,     effect=regular, cardType=Regular, desc="red 3"}
        ++ replicate 2 Card{num=Just 4, clr=Just Red,     effect=regular, cardType=Regular, desc="red 4"}
        ++ replicate 2 Card{num=Just 5, clr=Just Red,     effect=regular, cardType=Regular, desc="red 5"}
        ++ replicate 2 Card{num=Just 6, clr=Just Red,     effect=regular, cardType=Regular, desc="red 6"}
        ++ replicate 2 Card{num=Just 7, clr=Just Red,     effect=regular, cardType=Regular, desc="red 7"}
        ++ replicate 2 Card{num=Just 8, clr=Just Red,     effect=regular, cardType=Regular, desc="red 8"}
        ++ replicate 2 Card{num=Just 9, clr=Just Red,     effect=regular, cardType=Regular, desc="red 9"}
        ++ replicate 2 Card{num=Just 1, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 1"}
        ++ replicate 2 Card{num=Just 2, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 2"}
        ++ replicate 2 Card{num=Just 3, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 3"}
        ++ replicate 2 Card{num=Just 4, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 4"}
        ++ replicate 2 Card{num=Just 5, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 5"}
        ++ replicate 2 Card{num=Just 6, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 6"}
        ++ replicate 2 Card{num=Just 7, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 7"}
        ++ replicate 2 Card{num=Just 8, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 8"}
        ++ replicate 2 Card{num=Just 9, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 9"}
        ++ replicate 2 Card{num=Just 1, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 1"}
        ++ replicate 2 Card{num=Just 2, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 2"}
        ++ replicate 2 Card{num=Just 3, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 3"}
        ++ replicate 2 Card{num=Just 4, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 4"}
        ++ replicate 2 Card{num=Just 5, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 5"}
        ++ replicate 2 Card{num=Just 6, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 6"}
        ++ replicate 2 Card{num=Just 7, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 7"}
        ++ replicate 2 Card{num=Just 8, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 8"}
        ++ replicate 2 Card{num=Just 9, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 9"}
        ++ replicate 2 Card{num=Just 1, clr=Just Green,   effect=regular, cardType=Regular, desc="green 1"}
        ++ replicate 2 Card{num=Just 2, clr=Just Green,   effect=regular, cardType=Regular, desc="green 2"}
        ++ replicate 2 Card{num=Just 3, clr=Just Green,   effect=regular, cardType=Regular, desc="green 3"}
        ++ replicate 2 Card{num=Just 4, clr=Just Green,   effect=regular, cardType=Regular, desc="green 4"}
        ++ replicate 2 Card{num=Just 5, clr=Just Green,   effect=regular, cardType=Regular, desc="green 5"}
        ++ replicate 2 Card{num=Just 6, clr=Just Green,   effect=regular, cardType=Regular, desc="green 6"}
        ++ replicate 2 Card{num=Just 7, clr=Just Green,   effect=regular, cardType=Regular, desc="green 7"}
        ++ replicate 2 Card{num=Just 8, clr=Just Green,   effect=regular, cardType=Regular, desc="green 8"}
        ++ replicate 2 Card{num=Just 9, clr=Just Green,   effect=regular, cardType=Regular, desc="green 9"}
        ++ replicate 2 Card{num=Nothing, clr=Just Red,    effect=skip,         cardType=Skip, desc="red skip"}
        ++ replicate 2 Card{num=Nothing, clr=Just Yellow, effect=skip,         cardType=Skip, desc="yellow skip"}
        ++ replicate 2 Card{num=Nothing, clr=Just Blue,   effect=skip,         cardType=Skip, desc="blue skip"}
        ++ replicate 2 Card{num=Nothing, clr=Just Green,  effect=skip,         cardType=Skip, desc="green skip"}
        ++ replicate 2 Card{num=Nothing, clr=Just Red,    effect=drawTwo,      cardType=DrawTwo, desc="red draw two cards for next player"}
        ++ replicate 2 Card{num=Nothing, clr=Just Yellow, effect=drawTwo,      cardType=DrawTwo, desc="yellow draw two cards for next player"}
        ++ replicate 2 Card{num=Nothing, clr=Just Blue,   effect=drawTwo,      cardType=DrawTwo, desc="blue draw two cards for next player"}
        ++ replicate 2 Card{num=Nothing, clr=Just Green,  effect=drawTwo,      cardType=DrawTwo, desc="green draw two cards for next player"}
        ++ replicate 2 Card{num=Nothing, clr=Just Red,    effect=reverseD,     cardType=Reverse, desc="red reverse the direction"}
        ++ replicate 2 Card{num=Nothing, clr=Just Yellow, effect=reverseD,     cardType=Reverse, desc="yellow reverse the direction"}
        ++ replicate 2 Card{num=Nothing, clr=Just Blue,   effect=reverseD,     cardType=Reverse, desc="blue reverse the direction"}
        ++ replicate 2 Card{num=Nothing, clr=Just Green,  effect=reverseD,     cardType=Reverse, desc="green reverse the direction"}
        ++ replicate 4 Card{num=Nothing, clr=Nothing,     effect=wild,         cardType=Wild,    desc="declare next color to be matched"}
        ++ replicate 4 Card{num=Nothing, clr=Nothing,     effect=wildDrawFour, cardType=WildDrawFour, desc="draw four cards for next player"}

-------------------------------------------------
-- AI playing
-------------------------------------------------



