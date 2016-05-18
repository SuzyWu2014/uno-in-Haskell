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
    lift $ putStrLn  $ "Starting from " ++ getCurrPlayerName _game
    setStartingCard 
    goPlay   
        
goPlay :: Game ()
goPlay = do
    showState
    game <- get
    let currTurn = whoseTurn game
    let _playableCards = getPlayableCards currTurn game    
    if null _playableCards then 
        doDrawAndPlay currTurn
    else
        doPlayFromHand _playableCards        
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
        dropCard $ head _newPlayableCards
    else 
        askToDrop $ head _newPlayableCards

-- @Int current player
promptNoCardtoDrop :: Int -> Game()
promptNoCardtoDrop _currTurn = do 
    _game <- get 
    if isRobotPlayer _game then 
        lift $ putStrLn $ getPlayerName _currTurn _game ++ "has no card to drop! "
    else 
        lift $ putStrLn "You have no card to drop!"

-- @Card newly drawed card
askToDrop :: Card -> Game ()
askToDrop _card = do 
    lift $ putStrLn $ "You get a matched card: " ++ show _card
    lift $ putStrLn "Would you like to drop it? Enter 1 for yes, 0 for no"
    _decision <- lift getLine  
    if  _decision `elem` ["yes","y","YES","Yes"] then 
        dropCard _card
    else do
        setNextTurn 1
        showNextTurn

-- @[Card] playable card list
doPlayFromHand:: [Card] -> Game()
doPlayFromHand _cards = do 
    let _card = _cards !! genRanInt (length _cards)
    dropCard _card

doCheckGameOver :: Int -> Game()
doCheckGameOver _currTurn = do
    game'' <- get 
    if isWin _currTurn game'' then
        lift $ putStrLn "You Win"
    else if null $ deck game'' then
        lift $ putStrLn "No card in Deck! Calculating scores..." 
    else 
        goPlay 