module Utils where

import UnoDataModels
import Control.Monad.State
-- import Control.Monad
-------------------------------------------------
-- Field Checking
-------------------------------------------------
isRobotPlayer :: GameState -> Bool
isRobotPlayer  game = whoseTurn game /= realPlayer game

-------------------------------------------------
-- Info Retrieving
-------------------------------------------------
getCurrPlayerName :: GameState -> String
getCurrPlayerName game = getPlayerName (whoseTurn game) game

-- @Int Player ID
getPlayerName :: Int -> GameState -> String
getPlayerName i game = doGetPlayerName i (players game ) 

doGetPlayerName :: Int -> [PlayerState] -> String
doGetPlayerName  _   []                               = "Error"
doGetPlayerName _playerId (p:ps)  = if pId p == _playerId then name p
                                                      else doGetPlayerName  _playerId ps

getPlayerState :: Int -> GameState -> PlayerState
getPlayerState _playerId game = players game !! _playerId

-- doGetPlayerState :: Int -> [PlayerState] -> PlayerState 
-- doGetPlayerState _playerId (p:_players) 
--                                 | _playerId == UnoDataModels.id p = p 
--                                 | otherwise = doGetPlayerState _playerId _players

getPlayerCards :: Int -> GameState -> [Card]
getPlayerCards _playerId game = cardsInHand (getPlayerState _playerId game)

-------------------------------------------------
-- Display
-------------------------------------------------
showState :: Game ()
showState = do 
    game <- get 
    lift $ putStr $ "Current Direction - " ++ show (dir game) ++"\n"
    lift $ putStr $ "Current Card - " ++ show (currCard game) ++"\n"
    
showNextTurn :: Game ()
showNextTurn = do 
      game <- get 
      if isRobotPlayer game then 
        lift $ putStrLn $ "Next turn goes to " ++ getCurrPlayerName game
      else 
        lift $ putStrLn $ "It's your turn, " ++ getCurrPlayerName game ++ "!"
      lift $ putStrLn "----------------------------------------------------"

showDropCard :: Card -> Game()
showDropCard _card = do 
      game <- get 
      if isRobotPlayer game then
         lift $ putStrLn $ getCurrPlayerName game ++ " dropped " ++ show _card 
      else 
         lift $ putStrLn $ "You dropped " ++ show _card
-------------------------------------------------
-- Action
-------------------------------------------------
--TO DO: cases:
-- 1. no card to draw
-- 2. not enough card to draw
-- 3. after drawing, deck become empty 
-- @Int Player
drawCard :: Int -> GameState -> GameState
drawCard _ game@GameState{deck=[], players=_players}         = game
drawCard usr game@GameState{deck=(card:ds), players=_players}  = game{deck=ds, players=p}
  where
    u@PlayerState{cardsInHand=_cardsInHand} = players game !! usr
    p = take usr _players ++ [u{cardsInHand=card:_cardsInHand}] ++
        drop (usr+1) _players

-- First Int: nums of cards to draw
-- Second Int: playerID
drawCards :: Int -> Int -> GameState -> GameState
drawCards 0 _ game = game
drawCards n usr game = drawCards (n-1) usr (drawCard usr game) 

-- @Int next ith turn
-- @Int playerID of next turn 
nextTurn :: Int -> GameState -> Int
nextTurn 0 game= whoseTurn game
nextTurn i game@GameState{whoseTurn=_whoseTurn,players=_players,dir=_dir} = nextTurn (i-1) game{whoseTurn = doGetNextTurn _whoseTurn _players _dir}

-- @int Current turn
-- @[PlayerState]
-- @Direction
-- @Int NextTurn
doGetNextTurn :: Int -> [PlayerState]-> Direction -> Int
doGetNextTurn _whoseTurn _players _dir = varifyTurnNum (_whoseTurn + dirt _dir) (length _players)

-- varify the index of current player, making sure it goes in a manner of cycle
-- @Int CurrentTurn
-- @Int number of players
-- @Int Final decision of current turn
varifyTurnNum :: Int -> Int-> Int
varifyTurnNum _whoseTurn num_p 
                       | _whoseTurn == num_p = 0
                       | _whoseTurn < 0      = num_p
                       | otherwise           = _whoseTurn 

-- turn direction into operation                            
dirt :: Direction -> Int
dirt Clockwise        = 1
dirt CounterClockwise = -1

reverseDir :: Direction -> Direction
reverseDir Clockwise        = CounterClockwise
reverseDir CounterClockwise = Clockwise

-------------------------------------------------
-- Helper
-------------------------------------------------
-- @Int max number
-- @Int return number in [0,max]
genRanInt :: Int -> Int
genRanInt _ = 0


