module Uno where

import UnoDataModels 
import GameSim
import CardEffects
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

 

-- main game process goes here
simGame :: GameState -> Game ()
simGame _game = do
       put _game
       runEffect Skip



-- uno :: IO ()
-- uno = do
--        putStrLn "Greetings!  What is your name?"
--        _name  <- getLine
--        putStrLn $ "Welcome to UNO, " ++ _name ++ "!"
--        putStrLn "How many players would you like to play with? [1-4]"
--        _numStr <- getLine
--        putStrLn "Initializing game..."
--        --run game
--        let _num = read _numStr :: Int
--        let game = initGame _num _name 
--        lift $ putStrLn "Starting game..."
--        end  <- simGame game
--        lift $ putStrLn "Game is over."

uno::Game ()
uno = do 
   liftIO $ putStr  "some"
   _name <- lift getLine
   liftIO $ putStr "some"

