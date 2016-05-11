module Uno where

import UnoDataModels 
import GameSim

--main game process goes here
simGame :: GameState -> IO GameState
simGame game@(GameState _dir _currClr _realPlayer _whoseTurn _currColor _players _deck ) = do
	return game

uno = do
       putStrLn "Greetings!  What is your name?"
       _name <- getLine
       putStrLn $ "Welcome to UNO, " ++ _name ++ "!"
       putStrLn "How many players would you like to play with? [1-4]"
       _numStr <- getLine
       putStrLn "Initializing game..."
       --run game
       let _num = read _numStr :: Int
       let game = initGame _num _name
       putStrLn "Dealing cards..."
       game' <- dealCards _num game
       putStrLn "Starting game..."
       end   <- simGame game'
       putStrLn "Game is over."


