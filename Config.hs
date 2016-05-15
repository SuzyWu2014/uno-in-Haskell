module Config where

import UnoDataModels

-- data Card = Card { num :: Int
--                  , clr :: Color
--                  , cardType :: CardType
--                  , desc :: String
-- }

cardPile :: Deck
cardPile = [Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
          , Card{num=10,clr=Red,cardType=Skip,desc="regular"}
          , Card{num=10,clr=Blue,cardType=Skip,desc="regular"}
          , Card{num=10,clr=Yellow,cardType=Skip,desc="regular"}
          , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=2,clr=Yellow,cardType=Regular,desc="regular"} 
          , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=4,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=2,clr=Yellow,cardType=Regular,desc="regular"} 
          , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=4,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=2,clr=Yellow,cardType=Regular,desc="regular"} 
          , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=4,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=0,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=1,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=2,clr=Yellow,cardType=Regular,desc="regular"} 
          , Card{num=3,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=4,clr=Yellow,cardType=Regular,desc="regular"}
          , Card{num=5,clr=Yellow,cardType=Regular,desc="regular"}
          ] 
-- cardPile =            [Card{num=Just 0, clr=Just Red,     effect=regular, cardType=Regular, desc="red 0"}]
--         ++            [Card{num=Just 0, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 0"}]
--         ++            [Card{num=Just 0, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 0"}]
--         ++            [Card{num=Just 0, clr=Just Green,   effect=regular, cardType=Regular, desc="green 0"}]
--         ++ replicate 2 Card{num=Just 1, clr=Just Red,     effect=regular, cardType=Regular, desc="red 1"}
--         ++ replicate 2 Card{num=Just 2, clr=Just Red,     effect=regular, cardType=Regular, desc="red 2"}
--         ++ replicate 2 Card{num=Just 3, clr=Just Red,     effect=regular, cardType=Regular, desc="red 3"}
--         ++ replicate 2 Card{num=Just 4, clr=Just Red,     effect=regular, cardType=Regular, desc="red 4"}
--         ++ replicate 2 Card{num=Just 5, clr=Just Red,     effect=regular, cardType=Regular, desc="red 5"}
--         ++ replicate 2 Card{num=Just 6, clr=Just Red,     effect=regular, cardType=Regular, desc="red 6"}
--         ++ replicate 2 Card{num=Just 7, clr=Just Red,     effect=regular, cardType=Regular, desc="red 7"}
--         ++ replicate 2 Card{num=Just 8, clr=Just Red,     effect=regular, cardType=Regular, desc="red 8"}
--         ++ replicate 2 Card{num=Just 9, clr=Just Red,     effect=regular, cardType=Regular, desc="red 9"}
--         ++ replicate 2 Card{num=Just 1, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 1"}
--         ++ replicate 2 Card{num=Just 2, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 2"}
--         ++ replicate 2 Card{num=Just 3, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 3"}
--         ++ replicate 2 Card{num=Just 4, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 4"}
--         ++ replicate 2 Card{num=Just 5, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 5"}
--         ++ replicate 2 Card{num=Just 6, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 6"}
--         ++ replicate 2 Card{num=Just 7, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 7"}
--         ++ replicate 2 Card{num=Just 8, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 8"}
--         ++ replicate 2 Card{num=Just 9, clr=Just Yellow,  effect=regular, cardType=Regular, desc="yellow 9"}
--         ++ replicate 2 Card{num=Just 1, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 1"}
--         ++ replicate 2 Card{num=Just 2, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 2"}
--         ++ replicate 2 Card{num=Just 3, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 3"}
--         ++ replicate 2 Card{num=Just 4, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 4"}
--         ++ replicate 2 Card{num=Just 5, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 5"}
--         ++ replicate 2 Card{num=Just 6, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 6"}
--         ++ replicate 2 Card{num=Just 7, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 7"}
--         ++ replicate 2 Card{num=Just 8, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 8"}
--         ++ replicate 2 Card{num=Just 9, clr=Just Blue,    effect=regular, cardType=Regular, desc="blue 9"}
--         ++ replicate 2 Card{num=Just 1, clr=Just Green,   effect=regular, cardType=Regular, desc="green 1"}
--         ++ replicate 2 Card{num=Just 2, clr=Just Green,   effect=regular, cardType=Regular, desc="green 2"}
--         ++ replicate 2 Card{num=Just 3, clr=Just Green,   effect=regular, cardType=Regular, desc="green 3"}
--         ++ replicate 2 Card{num=Just 4, clr=Just Green,   effect=regular, cardType=Regular, desc="green 4"}
--         ++ replicate 2 Card{num=Just 5, clr=Just Green,   effect=regular, cardType=Regular, desc="green 5"}
--         ++ replicate 2 Card{num=Just 6, clr=Just Green,   effect=regular, cardType=Regular, desc="green 6"}
--         ++ replicate 2 Card{num=Just 7, clr=Just Green,   effect=regular, cardType=Regular, desc="green 7"}
--         ++ replicate 2 Card{num=Just 8, clr=Just Green,   effect=regular, cardType=Regular, desc="green 8"}
--         ++ replicate 2 Card{num=Just 9, clr=Just Green,   effect=regular, cardType=Regular, desc="green 9"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Red,    effect=skip,         cardType=Skip, desc="red skip"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Yellow, effect=skip,         cardType=Skip, desc="yellow skip"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Blue,   effect=skip,         cardType=Skip, desc="blue skip"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Green,  effect=skip,         cardType=Skip, desc="green skip"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Red,    effect=drawTwo,      cardType=DrawTwo, desc="red draw two cards for next player"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Yellow, effect=drawTwo,      cardType=DrawTwo, desc="yellow draw two cards for next player"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Blue,   effect=drawTwo,      cardType=DrawTwo, desc="blue draw two cards for next player"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Green,  effect=drawTwo,      cardType=DrawTwo, desc="green draw two cards for next player"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Red,    effect=reverseD,     cardType=Reverse, desc="red reverse the direction"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Yellow, effect=reverseD,     cardType=Reverse, desc="yellow reverse the direction"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Blue,   effect=reverseD,     cardType=Reverse, desc="blue reverse the direction"}
--         ++ replicate 2 Card{num=Nothing, clr=Just Green,  effect=reverseD,     cardType=Reverse, desc="green reverse the direction"}
--         ++ replicate 4 Card{num=Nothing, clr=Nothing,     effect=wild,         cardType=Wild,    desc="declare next color to be matched"}
--         ++ replicate 4 Card{num=Nothing, clr=Nothing,     effect=wildDrawFour, cardType=WildDrawFour, desc="draw four cards for next player"}