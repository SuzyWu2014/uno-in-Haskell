module Config where

import UnoDataModels

cardPile :: Deck
cardPile = let zeros    = [Card 0 c Regular "0" | c <- [Yellow .. Green]] in
           let ncards   = [Card n c Regular d | n <- [1..9], c <- [Yellow .. Green], d <- [show n]] in
           let funcards = [Card 20 c t d | c <- [Yellow .. Green], t <- [Skip .. Reverse], d <- [show t]] in
           let blacks   = [Card 50 PickAColor t d | t <- [Wild, WildDrawFour], d <- [show t]]
           in zeros ++ concatMap (replicate 2) (ncards ++ funcards) ++ concatMap (replicate 4) blacks

