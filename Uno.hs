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


-- @Int current turn
doDrawAndPlay:: Int -> Game()
doDrawAndPlay _currTurn = do  
    modify $ drawCard _currTurn
    game' <- get
    let _newPlayableCards = getPlayableCards (whoseTurn game') game'
    if null _newPlayableCards then do
        lift $ putStrLn "No card to Drop!"
        setNextTurn 1
        showNextTurn
    else do
        let _card = head _newPlayableCards
        dropCard _card

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