module Uno where

import UnoDataModels
import Utils 
import GameSim
import CardEffects
import Control.Monad.Trans
import Control.Monad.Trans.State
-- import Control.Monad

-- TO-DO: guard() to make sure # of player is [1-4]
uno :: IO ()
uno = do
    putStrLn "Greetings!  What is your name?"
    _name  <- getLine
    putStrLn $ "Welcome to UNO, " ++ _name ++ "!"
    putStrLn "How many players would you like to play with? [1-4]"
    _numStr <- getLine
    putStrLn "Initializing game..."
    --run game
    let _num = read _numStr :: Int
    let init_state = initGameState _num _name 
       in do
        putStrLn "Starting Game..."  
        evalStateT simGame init_state
        return ()
    putStrLn "Game is over."


simGame :: Game ()
simGame = do
    lift $ putStrLn "Dealing cards: each player gets 5 cards..."
    modify dealCards 
    lift $ putStrLn "----------------------------------------------------"
    _game <- get
    lift $ putStrLn $ "Starting from " ++ getCurrPlayerName _game 
    lift $ putStrLn $ "Direction: " ++ doShowDir (players _game )
    setStartingCard 
    goPlay   
        
goPlay :: Game ()
goPlay = do
    game <- get
    let currTurn = whoseTurn game
    let _playableCards = getPlayableCards currTurn game
    if null _playableCards then 
        doDrawAndPlay currTurn
    else
        doPlayFromHand _playableCards currTurn       
    doCheckGameOver currTurn


-- @Int current player
doDrawAndPlay:: Int -> Game()
doDrawAndPlay _currTurn = do  
    modify $ drawCard _currTurn
    game' <- get
    let _newPlayableCards = getPlayableCards _currTurn game'
    if null _newPlayableCards then do
        promptNoCardtoDrop _currTurn
        setNextTurn 1
        showNextTurn 
    else if isRobotPlayer game' then 
        dropCard  (head _newPlayableCards) _currTurn
    else do 
        showState
        askToDrop (head _newPlayableCards) _currTurn

-- @Int current player
promptNoCardtoDrop :: Int -> Game()
promptNoCardtoDrop _currTurn = do 
    _game <- get 
    if isRobotPlayer _game then 
        lift $ putStrLn $ getPlayerName _currTurn _game ++ " has no card to drop! "
    else 
        lift $ putStrLn "Draw a card from deck, and you still have no card to drop!"

-- @Card newly drawed card
askToDrop :: Card -> Int -> Game ()
askToDrop _card _currTurn = do 
    lift $ putStrLn $ "No card matched! Draw a card from deck.You get a matched card: " ++ show _card
    lift $ putStrLn "Would you like to drop it? Enter yes/no"
    _decision <- lift getLine  
    if  _decision `elem` ["yes","y","YES","Yes"] then 
        dropCard _card _currTurn
    else do
        setNextTurn 1
        showNextTurn

-- @[Card] playable card list
doPlayFromHand:: [Card] -> Int -> Game()
doPlayFromHand _cards _currTurn= do 
    _game <- get
    if isRobotPlayer _game then do 
        let _card = _cards !! genRanInt (length _cards) 
        dropCard _card _currTurn
    else do 
        showState
        askToPick _cards _currTurn

askToPick :: [Card] -> Int -> Game ()
askToPick _cards _currTurn= do
    _game <- get
    lift $ putStrLn $ "\n"++ "Your cards:         " ++ show (cardsInHand ( players _game !! whoseTurn _game))
    lift $ putStrLn $ "Cards you can drop: " ++ show _cards ++ "\n"
    lift $ putStrLn $ "Please pick one card to drop: (enter 1 - " ++ show (length _cards ) ++ ")"
    _numStr <- lift getLine
    let _num = read _numStr ::Int
    dropCard (_cards !! (_num-1)) _currTurn


doCheckGameOver :: Int -> Game()
doCheckGameOver _currTurn = do
    game'' <- get 
    if isWin _currTurn game'' then
        showWinner _currTurn
    else if null $ deck game'' then do
        lift $ putStrLn "No card in Deck! Calculating scores..." 
        lift $ print $ showScores $ getScores $ players game''
        let _winnerId = getWinnerId $ players game''
        showWinner _winnerId
    else 
        goPlay 

-- @Int Winner ID
showWinner :: Int -> Game()
showWinner _winnerId = do 
    _game <- get   
    if realPlayer _game == _winnerId then
        lift $ putStrLn "Congrats! You win the game!"
    else 
        lift $ putStrLn $ name (players _game !! _winnerId) ++ " win !!"



-- @Int Winner ID
getWinnerId :: [PlayerState] -> Int
getWinnerId _players = pId $ minimum _players

showScores :: [(String, Int)] -> String
showScores = foldr ((++).(\s ->  fst s ++ ": " ++ show (snd s) ++ " | " )) "" 

getScores :: [PlayerState] -> [(String, Int)]
getScores = map (\p -> (name p, calScore (cardsInHand p)))

calScore :: [Card] -> Int 
calScore = foldr ((+).num) 0 
