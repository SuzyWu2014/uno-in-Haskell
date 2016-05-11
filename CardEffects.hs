module CardEffects where

import UnoDataModels

-- Every time when player drops a card, set the GameState.currCard to the droped card, then apply the card effect

-- Card effect - update GameState, including 
    -- current card
    -- each player state
    -- Deck state
    -- direction
-- #######################################################
--Next player in sequence misses a turn
-- #######################################################
skip :: GameState -> GameState
skip (GameState _dir _whoseTurn _currCard _players _deck) = GameState _dir (getNextTurn (getNextTurn _whoseTurn _players _dir) _players _dir ) _currCard _players _deck

type CurrentTurn = Int
type CountPlayer = Int
type NextTurn    = Int
getNextTurn :: CurrentTurn -> [PlayerState]-> Direction -> NextTurn
getNextTurn _whoseTurn _players _dir = varifyTurnNum (_whoseTurn + dirt _dir) (length _players)
-- varify the index of current player, making sure it goes in a manner of cycle
varifyTurnNum :: CurrentTurn -> CountPlayer-> NextTurn
varifyTurnNum _whoseTurn num_p 
                       | _whoseTurn > num_p = 1
                       | _whoseTurn == 0    = num_p
                       | otherwise          = _whoseTurn 

-- turn direction into operation                            
dirt :: Direction -> Int
dirt Clockwise        = 1
dirt CounterClockwise = -1
-- #######################################################
-- drawTwo effect
-- #######################################################
drawTwo :: GameState -> GameState
drawTwo (GameState _dir _whoseTurn _currCard _players _deck) = GameState _dir _nextTurn _currCard (updatePlayers _players _nextTurn _deck) (updateDeck _deck 2) where _nextTurn = getNextTurn _whoseTurn _players _dir

type PlayerID = Int
updatePlayers :: [PlayerState] -> PlayerID -> Deck -> [PlayerState]
updatePlayers  []            _  _                = []
updatePlayers  _playerState  _  []               = _playerState
updatePlayers (p:ps) _playerId _deck 
                    | getPlayerId p /= _playerId = updatePlayers ps _playerId _deck
                    | getPlayerId p == _playerId = playerStateUpdateDrawCard p _deck 2:ps

playerStateUpdateDrawCard :: PlayerState -> Deck -> Int ->PlayerState
playerStateUpdateDrawCard  (PlayerState _id _name _score  _cards) _deck i = PlayerState _id _name _score (getCards _deck i ++_cards)

getCards :: Deck -> Int -> [Card]
getCards _ 0          = []
getCards [] _         = []
getCards (c:_cards) i = c : getCards _cards (i-1)

updateDeck :: Deck -> Int -> Deck
updateDeck [] _         = []
updateDeck d  0         = d
updateDeck (_:_cards) i = updateDeck _cards (i-1)

-- #######################################################
-- reverse effect
-- #######################################################
reverse :: GameState -> GameState
reverse = undefined
-- #######################################################
-- wild effect
-- #######################################################
wild :: GameState -> GameState
wild = undefined
-- #######################################################
-- wildDrawFour effect
-- #######################################################
wildDrawFour :: GameState -> GameState
wildDrawFour = undefined
-- #######################################################
-- regular card effect
-- #######################################################
regular :: GameState -> GameState
regular = undefined

