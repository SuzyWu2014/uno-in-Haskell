module CardEffects where

import UnoDataModels

-- Card effect - update GameState, including 
    -- current card
    -- each player state
    -- Deck state
    -- direction

--Next player in sequence misses a turn
skip :: GameState -> GameState
skip (GameState direction currPlayer c playerList d) = GameState direction (getNextTurn (currPlayer + dirt direction) (length playerList)) c playerList d

type CurrentTurn = Int
type CountPlayer = Int
type NextTurn    = Int
-- varify the index of current player, making sure it goes in a manner of cycle
getNextTurn :: CurrentTurn -> CountPlayer-> NextTurn
getNextTurn curr num_p 
                       | curr > num_p = 1
                       | curr == 0    = num_p
                       | otherwise    = curr 

-- turn direction into operation                            
dirt :: Direction -> Int
dirt Clockwise        = 1
dirt CounterClockwise = -1

drawTwo :: GameState -> GameState
drawTwo = undefined

reverse :: GameState -> GameState
reverse = undefined

wild :: GameState -> GameState
wild = undefined

wildDrawFour :: GameState -> GameState
wildDrawFour = undefined

regular :: GameState -> GameState
regular = undefined

