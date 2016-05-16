module Utils where

import UnoDataModels


--TO DO: cases:
-- 1. no card to draw
-- 2. not enough card to draw
-- 3. after drawing, deck become empty 
-- @Int Player
drawCard :: GameState -> Int -> GameState
drawCard game@GameState{deck=[], players=_players} _        = game
drawCard game@GameState{deck=(card:ds), players=_players} usr = game{deck=ds, players=p}
  where
    u@(PlayerState _ _ _ _cardsInHand) = players game !! usr
    p = take usr _players ++ [u{cardsInHand=card:_cardsInHand}] ++
        drop (usr+1) _players

-- First Int: nums of cards to draw
-- Second Int: playerID
drawCards :: Int -> GameState -> Int -> GameState
drawCards 0 game _ = game
drawCards n game usr = drawCards (n-1) (drawCard game usr) usr

-- drawCards :: Int -> GameState -> Int -> IO GameState
-- drawCards 0 game _ = return game
-- drawCards n game usr = do
--     game' <- drawCard game usr
--     drawCards (n-1) game' usr 

-- @Int max number
-- @Int return number in [0,max]
genRanInt :: Int -> Int
genRanInt _ = 1

getCurrPlayerName :: GameState -> String
getCurrPlayerName game = getPlayerName (players game) (whoseTurn game)

getPlayerName :: [PlayerState] -> Int -> String
getPlayerName [] _                                  = " "
getPlayerName (PlayerState _id _name _ _:ps) _whoseTurn = if _id == _whoseTurn then _name
                                                      else getPlayerName ps _whoseTurn

