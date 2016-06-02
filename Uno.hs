module Uno where

import UnoDataModels
import Utils 
import GameSim
import CardEffects
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad

-- TO-DO: guard() to make sure # of player is [1-4]
uno :: IO ()
uno = do
    putStr $ "Greetings!  What is your name? \n" ++ "> "
    _name  <- getLine
    putStrLn $ "Welcome to UNO, " ++ _name ++ "!"
    putStrLn "How many players would you like to play with? [1-4]" 
    _num <- getLineInt 5
    putStrLn "Initializing game..."
    --run game 
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
    lift $ putStrLn $ "Direction: " ++ showDirection _game 
    setStartingCard 
    goPlay   
        
goPlay :: Game ()
goPlay = do
    game <- get
    let _currTurn = whoseTurn game
    let _playableCards = getPlayableCards _currTurn game 
    if null _playableCards then  
        doDrawAndPlay _currTurn 
    else  
        doPlayFromHand _playableCards _currTurn  
    doCheckUno _currTurn 
    doCheckGameOver _currTurn


-- @Int current player
doDrawAndPlay:: Int -> Game()
doDrawAndPlay _currTurn = do  
    modify $ drawCard _currTurn
    game' <- get
    let _newPlayableCards = getPlayableCards _currTurn game'
    if null _newPlayableCards then do
        promptNoCardtoDrop _currTurn
        setNextTurn 1
        -- showNextTurn 
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
    else do 
        showAllCardInHand
        lift $ putStrLn "Draw a card from deck, and you still have no card to drop!"

-- @Card newly drawed card
askToDrop :: Card -> Int -> Game ()
askToDrop _card _currTurn = do 
    showAllCardInHand
    lift $ putStrLn $ "No card matched! Draw a card from deck.You get a matched card: " ++ show _card
    lift $ putStrLn "Would you like to drop it? Enter yes to drop, otherwise will keep it."
    _decision <- lift getLine  
    if  _decision `elem` ["yes","y","YES","Yes"] then 
        dropCard _card _currTurn
    else 
        setNextTurn 1
        -- showNextTurn

-- @[Card] playable card list
doPlayFromHand:: [Card] -> Int -> Game()
doPlayFromHand _cards _currTurn= do 
    _game <- get
    if isRobotPlayer _game then do 
        let _card = _cards !! randomInt (length _cards) 
        dropCard _card _currTurn
    else do 
        showState
        askToPick _cards _currTurn

askToPick :: [Card] -> Int -> Game ()
askToPick _cards _currTurn= do
    showAllCardInHand 
    lift $ putStrLn $ "Cards you can drop: \n" ++ showCards _cards
    lift $ putStrLn $ "Please pick one card to drop: (enter 1 - " ++ show (length _cards ) ++ ")"
    lift $ putStrLn $ "Or you can just input /help to get help information."
    _num <- lift $ getLineInt $ 1+ length _cards
    dropCard (_cards !! (_num-1)) _currTurn

-- @Int PlayerId 
doCheckUno :: Int -> Game ()
doCheckUno _currTurn = do 
     _game <- get 
     Control.Monad.when (isUno _currTurn _game) $ declareUno _currTurn


doCheckGameOver :: Int -> Game()
doCheckGameOver _currTurn = do
    _game <- get 
    if isWin _currTurn _game then
        showWinner _currTurn
    else if null $ deck _game  then do
        lift $ putStrLn "No card in Deck! Calculating scores..." 
        calScores $ players _game
        _game' <- get
        lift $ print $ showScores $ getScores $ players _game'
        let _winnerId = getWinnerId $ players _game'
        showWinner _winnerId
    else do 
        showNextTurn
        goPlay 

