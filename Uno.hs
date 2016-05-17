module Uno where

import UnoDataModels
import Utils 
import GameSim
import CardEffects
import Control.Monad.Trans
import Control.Monad.Trans.State
-- import Control.Monad
 

-- main game process goes here
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
goPlay  = do
    showState
    game <- get
    dropCard $ head $ deck game
    goPlay

playTurn :: Game ()
playTurn = do
    game <- get
    let _playableCards = getPlayableCards (whoseTurn game) game
    if null _playableCards then do
        modify $ drawCard $ whoseTurn game
        game' <- get
        let _newPlayableCards = getPlayableCards (whoseTurn game') game'
        if null _newPlayableCards then do
            lift $ putStrLn "No card to Drop!"
            setNextTurn 1
            showNextTurn
        else do
            let _card = head _newPlayableCards
            dropCard _card
    else do
        let _card = _playableCards !! genRanInt (length _playableCards)
        dropCard _card

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

