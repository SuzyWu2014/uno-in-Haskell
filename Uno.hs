module Uno where

import UnoDataModels 
import GameSim
import CardEffects
import Control.Monad.Trans
import Control.Monad.Trans.State

 

-- main game process goes here
simGame :: GameState -> Game ()
simGame _game = do
       lift $ putStrLn "Starting Game..."
       lift $ print _game
       lift $ putStrLn "Dealing cards: each player gets 5 cards..."
       lift $ print $ dealCards _game
       put $ dealCards _game
       runEffect Wild
       game <- get 
       lift $ print game

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
            evalStateT (simGame init_state)  init_state
            return ()
    putStrLn "Game is over."

