module GameSim where

import UnoDataModels
import Utils
import Config
import System.Random
import Control.Monad.State
-- import Control.Monad

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
initDeck = shuffle (mkStdGen $ randomInt 108) cardPile

initPlayer :: Int -> String -> PlayerState
initPlayer _id _name = PlayerState{
    pId          = _id,
    name       = _name,
    cardsInHand = [],
    score       = 0
} 
initRobotPlayers :: Int -> [PlayerState]
initRobotPlayers 0 = []
initRobotPlayers _num = initRobotPlayers (_num-1) ++ [initPlayer _id (initRobotPlayerName _id) ]
    where _id = _num - 1 --index: 0 - num-1

initRobotPlayerName :: Int -> String
initRobotPlayerName _id = playerNames !! _id

pickStarter :: Int -> Int
pickStarter _num = div _num 2
-- pickStarter = randomInt

initGameState :: Int -> String -> GameState
initGameState _num _name = GameState{
    dir        = Clockwise, 
    realPlayer = _num,
    currCard   = head initDeck, -- for test purpose
    whoseTurn  = pickStarter _num,
    players    = initRobotPlayers _num ++ [initPlayer _num _name],
    deck       = initDeck,
    isOver     = False,
    ithTurn    = 0
}  

dealCards :: GameState -> GameState
dealCards game@GameState{players=_players} = dealing (length _players -1) game

-- @Int num of players 
dealing :: Int -> GameState -> GameState
dealing _num game@GameState{players=_players, deck=_deck} 
                                | _num >= 0 =  dealing (_num-1) (drawCards 5 _num game ) 
                                | otherwise = game                         

setStartingCard :: Game ()
setStartingCard = do 
    game@GameState{deck=_deck} <- get 
    put game{currCard=head _deck, deck=tail _deck}

-------------------------------------------------
-- Each playing turn
-------------------------------------------------

-- @Int player Id
isWin :: Int -> GameState -> Bool
isWin _playerId game =  null $ cardsInHand $ players game !! _playerId

getPlayableCards :: Int -> GameState -> [Card]
getPlayableCards _playerId game = doGetPlayableCards (currCard game) (getPlayerCards _playerId game)

doGetPlayableCards :: Card -> [Card] -> [Card]
doGetPlayableCards _currCard = filter (isMatch _currCard)

isMatch :: Card -> Card -> Bool
isMatch _currCard _card = case  cardType _card of
            Wild         -> True
            WildDrawFour -> True
            Regular      -> num _card == num _currCard || clr _card == clr _currCard
            _            -> clr _card == clr _currCard

isUno :: Int -> GameState -> Bool
isUno _playerId _game = 1 == countPlayerHandCards _playerId _game

declareUno :: Int -> Game ()
declareUno _playerId = do 
    _game <- get 
    let _name = getPlayerName _playerId _game
    if _playerId /= realPlayer _game then
        lift $ putStrLn $ _name ++ " declares Uno!"
    else 
        lift $ putStrLn "You declare uno!"


calScores :: [PlayerState] -> Game ()
calScores _players = do 
        _game <- get
        let _players_new = updateScores _players
        put _game{players=_players_new}

