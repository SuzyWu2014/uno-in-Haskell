module CardEffects where

import UnoDataModels
import Utils
import Control.Monad.Trans
import Control.Monad.Trans.State

-- Every time when player drops a card, set the GameState.currCard to the droped card, then apply the card effect
dropCard :: Card -> Game ()
dropCard  _card@Card{cardType=_cardType} = do  
  showDropCard _card
  modify $ updateCurrCard _card 
  runEffect _cardType

updateCurrCard :: Card -> GameState -> GameState
updateCurrCard _card game = game{currCard=_card}

runEffect :: CardType -> Game ()
runEffect _cardType = case _cardType of
                          Regular       -> regular
                          Skip          -> skip 
                          Reverse       -> reverseD
                          DrawTwo       -> drawTwo
                          Wild          -> wild
                          WildDrawFour  -> wildDrawFour

-- #######################################################
-- Skip: Next player in sequence misses a turn
-- #######################################################

skip :: Game ()
skip = do
  setNextTurn 2
  lift $ putStrLn "Next player in sequence miss a turn!" 
  showNextTurn

-- #######################################################
-- drawTwo effect - Next player in sequence draws two cards and misses a turn
-- #######################################################
-- need to update whoseTurn
drawTwo :: Game ()
drawTwo = do 
  setNextTurn 1
  game <- get
  modify $ drawCards 2 $ whoseTurn game
  setNextTurn 1
  lift $ putStrLn "Next player in sequence draws two cards and misses a turn!"
  showNextTurn 

-- #######################################################
-- reverse effect - 
--     Order of play switches directions (clockwise to counterclockwise, and vice versa)
-- #######################################################
reverseD :: Game ()
reverseD = do 
  game <- get
  put game{dir=reverseDir (dir game)} 
  setNextTurn 1
  lift $ putStrLn "Direction reversed!"
  showNextTurn

-- #######################################################
-- wild effect - 
--      Player declares next color to be matched 
-- #######################################################
wild :: Game ()
wild  = do
  pickColor
  setNextTurn 1
  showNextTurn

-- #######################################################
-- wildDrawFour effect:
--    Player declares next color to be matched; 
--    next player in sequence draws four cards and loses a turn.
--    May be legally played only if the player has no cards of the current color;
--    Wild cards and cards with the same number or symbol in a different color do not count.
-- #######################################################
wildDrawFour :: Game ()
wildDrawFour = do 
  lift $ putStrLn "Next player in sequence draws four cards and loses a turn"
  pickColor
  setNextTurn 1
  _game <- get
  modify $ drawCards 4 $ whoseTurn _game
  setNextTurn 1
  showNextTurn

-- #######################################################
-- regular card effect - Move to next player
-- #######################################################
regular :: Game ()
regular = setNextTurn 1 >> showNextTurn
    
-- #######################################################
-- Sub steps
-- #######################################################    
setNextTurn :: Int -> Game ()
setNextTurn i = do 
  game <- get
  put game{whoseTurn=nextTurn i game}

pickColor :: Game ()
pickColor = do
  game <- get
  if isRobotPlayer game  then do
    let _currClr = colors !! genRanInt 3
    setCurrColor _currClr
    lift $ putStrLn $ show _currClr ++ " was picked!" 
  else  
    askForColor  

askForColor ::Game ()
askForColor = do 
  lift $ putStrLn "Please pick a color to continue: 1-Yellow, 2-Red, 3-Blue, 4-Green"
  _numStr <- lift getLine
  let clrInt = read _numStr :: Int
  let _currClr = colors !! (clrInt-1)
  setCurrColor _currClr
  lift $ putStrLn $ "You picked " ++ show _currClr ++"!" 

setCurrColor :: Color -> Game()
setCurrColor _clr = do
  game <- get 
  let _currCard = currCard game
  let _card = _currCard{clr=_clr} 
  put game{currCard=_card}
