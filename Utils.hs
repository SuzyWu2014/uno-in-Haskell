module Utils where

import UnoDataModels


-- @Int Player
drawCard :: GameState -> Int -> GameState
drawCard game@GameState{deck=(card:ds), players=_players} usr = game{deck=ds, players=p}
  where
    u@(PlayerState _ _ _ _cardsInHand) = players game !! usr
    p = take usr _players ++ [u{cardsInHand=card:_cardsInHand}] ++
        drop (usr+1) _players

-- First Int: nums of cards to drae
-- Second Int: playerID
drawCards :: Int -> GameState -> Int -> GameState
drawCards 0 game _ = game
drawCards n game usr = drawCards (n-1) (drawCard game usr) usr
   

-- drawCards :: Int -> GameState -> Int -> IO GameState
-- drawCards 0 game _ = return game
-- drawCards n game usr = do
--     game' <- drawCard game usr
--     drawCards (n-1) game' usr 