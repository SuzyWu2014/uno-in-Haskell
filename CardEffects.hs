module CardEffects where

import UnoDataModels
import Utils
import Control.Monad.Trans
import Control.Monad.Trans.State
-- Every time when player drops a card, set the GameState.currCard to the droped card, then apply the card effect


runEffect :: CardType -> Game ()
runEffect _cardType = case _cardType of
                          Regular       -> get >>= regular
                          Skip          -> get >>= skip 
                          Reverse       -> get >>= reverseD
                          DrawTwo       -> get >>= drawTwo
                          Wild          -> get >>= wild
                          WildDrawFour  -> get >>= wildDrawFour

-- #######################################################
-- Skip: Next player in sequence misses a turn
-- #######################################################

skip :: GameState -> Game ()
skip game = do
  liftIO $ putStrLn "Player played Skip"
  put game{whoseTurn=_nextTurn}
   where _nextTurn = nextTurn 2 game

-- @Int next ith turn
-- @Int playerID of next turn 
nextTurn :: Int -> GameState -> Int
nextTurn 0 game@GameState{whoseTurn=_whoseTurn}= _whoseTurn
nextTurn i game@GameState{whoseTurn=_whoseTurn,players=_players,dir=_dir} = nextTurn (i-1) game{whoseTurn = getNextTurn _whoseTurn _players _dir}

-- @int Current turn
-- @[PlayerState]
-- @Direction
-- @Int NextTurn
getNextTurn :: Int -> [PlayerState]-> Direction -> Int
getNextTurn _whoseTurn _players _dir = varifyTurnNum (_whoseTurn + dirt _dir) (length _players)

-- varify the index of current player, making sure it goes in a manner of cycle
-- @Int CurrentTurn
-- @Int number of players
-- @Int Final decision of current turn
varifyTurnNum :: Int -> Int-> Int
varifyTurnNum _whoseTurn num_p 
                       | _whoseTurn == num_p = 0
                       | _whoseTurn < 0    = num_p
                       | otherwise          = _whoseTurn 

-- turn direction into operation                            
dirt :: Direction -> Int
dirt Clockwise        = 1
dirt CounterClockwise = -1

-- #######################################################
-- drawTwo effect - Next player in sequence draws two cards and misses a turn
-- #######################################################
-- need to update whoseTurn
drawTwo ::GameState -> Game ()
drawTwo  game = do 
    lift $ putStrLn "Player played drawTwo"
    let game' = drawCards 2 game _nextTurn
    put game'{whoseTurn=nextTurn 2 game}
  where
    _nextTurn = nextTurn 1 game   

-- #######################################################
-- reverse effect - Order of play switches directions (clockwise to counterclockwise, and vice versa)
-- #######################################################
reverseD :: GameState -> Game()
reverseD game@GameState{whoseTurn=_whoseTurn,players=_players, dir=_dir} = do 
    lift $ putStrLn "Player played reverse"
    put game{whoseTurn=_nextTurn, dir=_newDir}
  where
    _newDir   = reverseDir _dir
    _nextTurn = getNextTurn _whoseTurn _players _newDir
                                                                    
reverseDir :: Direction -> Direction
reverseDir Clockwise        = CounterClockwise
reverseDir CounterClockwise = Clockwise

-- #######################################################
-- wild effect - Player declares next color to be matched (may be used on any turn even if the player has matching color)
-- #######################################################
wild :: GameState -> Game ()
wild game = if robotPlayer game 
     then 
       put game{whoseTurn=nextTurn 1 game,currClr =  colors !! genRanInt 3}
     else do
        askForColor game
        game' <- get
        put game'{whoseTurn=nextTurn 1 game}

robotPlayer :: GameState -> Bool
robotPlayer  game@GameState{whoseTurn=_whoseTurn, realPlayer=_realPlayer} = _whoseTurn /= _realPlayer

askForColor :: GameState -> Game ()
askForColor game = do 
        lift $ putStrLn "Please pick a color to continue: 1-Yellow, 2-Red, 3-Blue, 4-Green"
        _numStr <- lift getLine
        let clrInt = read _numStr :: Int
        put game{currClr = colors !! (clrInt-1)}

-- #######################################################
-- wildDrawFour effect:
--Player declares next color to be matched; next player in sequence draws four cards and loses a turn.
-- May be legally played only if the player has no cards of the current color; Wild cards and cards with the same number or symbol in a different color do not count.
-- #######################################################
wildDrawFour :: GameState -> Game ()
wildDrawFour game = if robotPlayer game 
        then  do
          let game'= drawCards 4 game (nextTurn 1 game)
          put game'{whoseTurn=nextTurn 2 game, currClr= colors !! genRanInt 3} 
        else do 
          put $ drawCards 4 game (nextTurn 1 game)
          askForColor game    
          game' <- get      
          put game'{whoseTurn=nextTurn 2 game}  

-- #######################################################
-- regular card effect - Move to next player
-- #######################################################
regular :: GameState -> Game ()
regular game = do 
    lift $putStrLn "Player played regular"
    put  game{whoseTurn=_nextTurn}
  where
    _nextTurn = nextTurn 1 game


