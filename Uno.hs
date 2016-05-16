module Uno where

import UnoDataModels 
import GameSim
import CardEffects
import Control.Monad.Trans
import Control.Monad.Trans.State

 

-- main game process goes here
simGame :: GameState -> Game ()
simGame _game@GameState{deck=_deck} = 
        if  not (null _deck) then do
            put _game
            dropCard $ _deck !! 1
            dropCard $ _deck !! 2
            dropCard $ _deck !! 3
            dropCard $ _deck !! 4
            dropCard $ _deck !! 5
            dropCard $ _deck !! 6
            dropCard $ _deck !! 7
            dropCard $ _deck !! 8
            dropCard $ _deck !! 4
            dropCard $ _deck !! 5
            dropCard $ _deck !! 6
            dropCard $ _deck !! 7
            dropCard $ _deck !! 8
            -- game <- get
            -- simGame game
        else 
           return ()
        
       -- game <- get 
       -- lift $ print game

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
    putStrLn "Starting Game..." 
    putStrLn "Dealing cards: each player gets 5 cards..."
    let gameState = dealCards init_state
        in do
            evalStateT (simGame gameState)  gameState
            return ()
    putStrLn "Game is over."

