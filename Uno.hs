module Uno where

import UnoDataModels 
import GameSim
import CardEffects
import Control.Monad.State

--main game process goes here
simGame :: GameState -> Game ()
simGame _game = do
       put _game
       runEffect Skip


uno :: Game ()
uno = do
       lift $ putStrLn "Greetings!  What is your name?"
       _name <- lift $ getLine
       lift $ putStrLn $ "Welcome to UNO, " ++ _name ++ "!"
       lift $ putStrLn "How many players would you like to play with? [1-4]"
       _numStr <- lift $ getLine
       lift $ putStrLn "Initializing game..."
       --run game
       let _num = read _numStr :: Int
       initGame _num _name
       game <- get
       lift $ putStrLn "Starting game..."
       end  <- simGame game
       lift $ putStrLn "Game is over."


