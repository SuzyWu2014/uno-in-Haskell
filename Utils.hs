module Utils where

import UnoDataModels
import Control.Monad.State 
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Control.Arrow

-------------------------------------------------
-- Field Checking
-------------------------------------------------
isRobotPlayer :: GameState -> Bool
isRobotPlayer  _game = whoseTurn _game /= realPlayer _game

-------------------------------------------------
-- Info Retrieving
-----------------------------------------------
getCurrPlayerName :: GameState -> String
getCurrPlayerName _game = getPlayerName (whoseTurn _game) _game

-- @Int playerId
-- @Int #of cards in hand
countPlayerHandCards :: Int -> GameState -> Int
countPlayerHandCards _playerId _game = length $ cardsInHand (getPlayerState _playerId _game)

-- @Int Player ID
getPlayerName :: Int -> GameState -> String
getPlayerName i _game = doGetPlayerName i (players _game ) 

doGetPlayerName :: Int -> [PlayerState] -> String
doGetPlayerName  _   []                               = "Error"
doGetPlayerName _playerId (p:ps)  = if pId p == _playerId then name p
                                                      else doGetPlayerName  _playerId ps

getPlayerState :: Int -> GameState -> PlayerState
getPlayerState _playerId _game = players _game !! _playerId

getPlayerCards :: Int -> GameState -> [Card]
getPlayerCards _playerId _game = cardsInHand (getPlayerState _playerId _game)

-------------------------------------------------
-- Display
-------------------------------------------------
showState :: Game ()
showState = do 
    countTurn
    _game <- get 
    lift $ putStrLn $ "\n" ++"======================== Game Status ============================" ++"\n" 
    lift $ putStrLn $ "No." ++ show (ithTurn _game)  
    lift $ putStr $ "Current Direction: " ++ showDirection _game ++"\n"
    lift $ putStr $ "Current Card:      " ++ show (currCard _game) ++"\n"
    
showNextTurn :: Game ()
showNextTurn = do 
      _game <- get 
      if isRobotPlayer _game then 
        lift $ putStrLn $ "           => Next turn goes to " ++ getCurrPlayerName _game
      else 
        lift $ putStrLn $ "           => It's your turn, " ++ getCurrPlayerName _game ++ "!" 

showDropCard :: Card -> Game()
showDropCard _card = do 
      _game <- get 
      if isRobotPlayer _game then
         lift $ putStrLn $ getCurrPlayerName _game ++ " dropped " ++ show _card 
      else 
         lift $ putStrLn $ "You dropped " ++ show _card

showAllCardInHand :: Game ()
showAllCardInHand = do 
  _game <- get
  lift $ putStrLn $ "\n"++ "Your cards:         " ++ show (cardsInHand ( players _game !! whoseTurn _game)) ++"\n"


showDirection :: GameState -> String
showDirection _game 
              | dir _game == Clockwise = doShowDir (players _game) ++ name (head (players _game))
              | otherwise = doShowDir (reverse (players _game)) ++ name (last (players _game))

doShowDir :: [PlayerState] -> String
doShowDir [] = ""
doShowDir (_player:_players) = name _player ++ " -> " ++ doShowDir _players 

-- @Int Winner ID
showWinner :: Int -> Game()
showWinner _winnerId = do 
    _game <- get   
    if realPlayer _game == _winnerId then
        lift $ putStrLn "Congrats! You win the game!"
    else 
        lift $ putStrLn $ name (players _game !! _winnerId) ++ " win !!"

-------------------------------------------------
-- Action
-------------------------------------------------

-- @Int Winner ID
getWinnerId :: [PlayerState] -> Int
getWinnerId _players = pId $ minimum _players

showScores :: [(String, Int)] -> String
showScores = foldr ((++).(\s ->  fst s ++ ": " ++ show (snd s) ++ " | " )) "" 

getScores :: [PlayerState] -> [(String, Int)]
getScores = map (name Control.Arrow.&&& score) 
-- getScores = map (\p -> (name p, score p))

updateScores :: [PlayerState] -> [PlayerState]
updateScores = map(\p -> p{score=calScore (cardsInHand p)})

calScore :: [Card] -> Int 
calScore = foldr ((+).num) 0 

countTurn :: Game()
countTurn = do 
   _game <- get 
   let count = ithTurn _game +1
   put _game{ithTurn=count}

--TO DO: cases:
-- 1. no card to draw
-- 2. not enough card to draw
-- 3. after drawing, deck become empty 
-- @Int Player
drawCard :: Int -> GameState -> GameState
drawCard _ _game@GameState{deck=[], players=_players}         = _game
drawCard usr _game@GameState{deck=(card:ds), players=_players}  = _game{deck=ds, players=p}
  where
    u@PlayerState{cardsInHand=_cardsInHand} = players _game !! usr
    p = take usr _players ++ [u{cardsInHand=card:_cardsInHand}] ++
        drop (usr+1) _players

-- First Int: nums of cards to draw
-- Second Int: playerID
drawCards :: Int -> Int -> GameState -> GameState
drawCards 0 _ _game = _game
drawCards n usr _game = drawCards (n-1) usr (drawCard usr _game) 

-- @Int next ith turn
-- @Int playerID of next turn 
nextTurn :: Int -> GameState -> Int
nextTurn 0 _game= whoseTurn _game
nextTurn i _game@GameState{whoseTurn=_whoseTurn,players=_players,dir=_dir} = nextTurn (i-1) _game{whoseTurn = doGetNextTurn _whoseTurn _players _dir}

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
                       | _whoseTurn < 0      = num_p - 1
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
randomInt :: Int -> Int
randomInt n = unsafePerformIO $ getStdRandom $ randomR (0,n-1)

helpinfo :: IO ()
helpinfo = putStr $ unlines 
          ["Helpful Commands:",
           "/info <Card>:\tDisplay what the card does",
           "/hand:\t\t\tDisplay cards in your hand",
           "/help:\t\t\tDisplay this help information dialog"
          ]






