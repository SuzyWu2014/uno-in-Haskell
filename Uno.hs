module Uno where

import UnoDataModels
import GameUtils

--main game process goes here
simGame :: GameState -> IO GameState
simGame = undefined

main = do
       putStrLn "Greetings!  What is your name?"
       _name <- getLine
       putStrLn $ "Welcome to UNO, " ++ _name ++ "!"
       putStrLn "How many players would you like to play with? [1-4]"
       _num <- getLine
       putStrLn "Initializing game..."
       --run game
       let game = initGame (read _num :: Int)
       putStrLn "Starting game..."
       end <- simGame game
       putStrLn "Game is over."


