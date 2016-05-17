module Config where

import UnoDataModels
import Data.List

-- data Card = Card { num :: Int
--                  , clr :: Color
--                  , cardType :: CardType
--                  , desc :: String
-- }

cardPile :: Deck
cardPile = let zeros    = [Card 0 c Regular "0" | c <- [Yellow .. Green]] in
           let ncards   = [Card n c Regular d | n <- [1..9], c <- [Yellow .. Green], d <- [show n]] in
           let funcards = [Card 20 c t d | c <- [Yellow .. Green], t <- [Skip .. Reverse], d <- [show t]] in
           let blacks   = [Card 50 PickAColor t d | t <- [Wild, WildDrawFour], d <- [show t]]
           in zeros ++ (concatMap (replicate 2) (ncards ++ funcards)) ++ concatMap (replicate 4) blacks


-- cardPile :: Deck
-- cardPile = [Card{num=0,clr=Yellow,cardType=Wild,desc="regular"}
--           , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
--           , Card{num=10,clr=Red,cardType=Skip,desc="regular"}
--           , Card{num=10,clr=Blue,cardType=WildDrawFour,desc="regular"}
--           , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=Wild,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Reverse,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=Wild,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Reverse,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=Wild,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Reverse,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=Wild,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Reverse,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Wild,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=WildDrawFour,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=WildDrawFour,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Skip,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Reverse,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=Wild,desc="regular"}
--           , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
--           , Card{num=2,clr=Yellow,cardType=Regular,desc="regular"} 
--           , Card{num=3,clr=Yellow,cardType=Reverse,desc="regular"}
--           , Card{num=4,clr=Yellow,cardType=WildDrawFour,desc="regular"}
--           , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
--           ] 
-- cardPile =            [Card{num=0, clr=Red,      cardType=Regular, desc="red 0"}]
--         ++            [Card{num=0, clr=Yellow,   cardType=Regular, desc="yellow 0"}]
--         ++            [Card{num=0, clr=Blue,     cardType=Regular, desc="blue 0"}]
--         ++            [Card{num=0, clr=Green,    cardType=Regular, desc="green 0"}]
--         ++ replicate 2 Card{num=1, clr=Red,      cardType=Regular, desc="red 1"}
--         ++ replicate 2 Card{num=2, clr=Red,      cardType=Regular, desc="red 2"}
--         ++ replicate 2 Card{num=3, clr=Red,      cardType=Regular, desc="red 3"}
--         ++ replicate 2 Card{num=4, clr=Red,      cardType=Regular, desc="red 4"}
--         ++ replicate 2 Card{num=5, clr=Red,      cardType=Regular, desc="red 5"}
--         ++ replicate 2 Card{num=6, clr=Red,      cardType=Regular, desc="red 6"}
--         ++ replicate 2 Card{num=7, clr=Red,      cardType=Regular, desc="red 7"}
--         ++ replicate 2 Card{num=8, clr=Red,      cardType=Regular, desc="red 8"}
--         ++ replicate 2 Card{num=9, clr=Red,      cardType=Regular, desc="red 9"}
--         ++ replicate 2 Card{num=1, clr=Yellow,   cardType=Regular, desc="yellow 1"}
--         ++ replicate 2 Card{num=2, clr=Yellow,   cardType=Regular, desc="yellow 2"}
--         ++ replicate 2 Card{num=3, clr=Yellow,   cardType=Regular, desc="yellow 3"}
--         ++ replicate 2 Card{num=4, clr=Yellow,   cardType=Regular, desc="yellow 4"}
--         ++ replicate 2 Card{num=5, clr=Yellow,   cardType=Regular, desc="yellow 5"}
--         ++ replicate 2 Card{num=6, clr=Yellow,   cardType=Regular, desc="yellow 6"}
--         ++ replicate 2 Card{num=7, clr=Yellow,   cardType=Regular, desc="yellow 7"}
--         ++ replicate 2 Card{num=8, clr=Yellow,   cardType=Regular, desc="yellow 8"}
--         ++ replicate 2 Card{num=9, clr=Yellow,   cardType=Regular, desc="yellow 9"}
--         ++ replicate 2 Card{num=1, clr=Blue,     cardType=Regular, desc="blue 1"}
--         ++ replicate 2 Card{num=2, clr=Blue,     cardType=Regular, desc="blue 2"}
--         ++ replicate 2 Card{num=3, clr=Blue,     cardType=Regular, desc="blue 3"}
--         ++ replicate 2 Card{num=4, clr=Blue,     cardType=Regular, desc="blue 4"}
--         ++ replicate 2 Card{num=5, clr=Blue,     cardType=Regular, desc="blue 5"}
--         ++ replicate 2 Card{num=6, clr=Blue,     cardType=Regular, desc="blue 6"}
--         ++ replicate 2 Card{num=7, clr=Blue,     cardType=Regular, desc="blue 7"}
--         ++ replicate 2 Card{num=8, clr=Blue,     cardType=Regular, desc="blue 8"}
--         ++ replicate 2 Card{num=9, clr=Blue,     cardType=Regular, desc="blue 9"}
--         ++ replicate 2 Card{num=1, clr=Green,    cardType=Regular, desc="green 1"}
--         ++ replicate 2 Card{num=2, clr=Green,    cardType=Regular, desc="green 2"}
--         ++ replicate 2 Card{num=3, clr=Green,    cardType=Regular, desc="green 3"}
--         ++ replicate 2 Card{num=4, clr=Green,    cardType=Regular, desc="green 4"}
--         ++ replicate 2 Card{num=5, clr=Green,    cardType=Regular, desc="green 5"}
--         ++ replicate 2 Card{num=6, clr=Green,    cardType=Regular, desc="green 6"}
--         ++ replicate 2 Card{num=7, clr=Green,    cardType=Regular, desc="green 7"}
--         ++ replicate 2 Card{num=8, clr=Green,    cardType=Regular, desc="green 8"}
--         ++ replicate 2 Card{num=9, clr=Green,    cardType=Regular, desc="green 9"}
--         ++ replicate 2 Card{num=20, clr=Red,        cardType=Skip, desc="red skip"}
--         ++ replicate 2 Card{num=20, clr=Yellow,     cardType=Skip, desc="yellow skip"}
--         ++ replicate 2 Card{num=20, clr=Blue,       cardType=Skip, desc="blue skip"}
--         ++ replicate 2 Card{num=20, clr=Green,      cardType=Skip, desc="green skip"}
--         ++ replicate 2 Card{num=20, clr=Red,        cardType=DrawTwo, desc="red draw two cards for next player"}
--         ++ replicate 2 Card{num=20, clr=Yellow,     cardType=DrawTwo, desc="yellow draw two cards for next player"}
--         ++ replicate 2 Card{num=20, clr=Blue,       cardType=DrawTwo, desc="blue draw two cards for next player"}
--         ++ replicate 2 Card{num=20, clr=Green,      cardType=DrawTwo, desc="green draw two cards for next player"}
--         ++ replicate 2 Card{num=20, clr=Red,        cardType=Reverse, desc="red reverse the direction"}
--         ++ replicate 2 Card{num=20, clr=Yellow,     cardType=Reverse, desc="yellow reverse the direction"}
--         ++ replicate 2 Card{num=20, clr=Blue,       cardType=Reverse, desc="blue reverse the direction"}
--         ++ replicate 2 Card{num=20, clr=Green,      cardType=Reverse, desc="green reverse the direction"}
--         ++ replicate 4 Card{num=50, clr=PickAColor, cardType=Wild,    desc="declare next color to be matched"}
--         ++ replicate 4 Card{num=50, clr=PickAColor, cardType=WildDrawFour, desc="draw four cards for next player"}