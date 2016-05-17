module UnoDataModels where

import Control.Monad.State

data CardType = Skip  
              | DrawTwo  
              | Reverse  
              | Wild
              | WildDrawFour
              | Regular  
              deriving(Show, Eq)
data Color = Yellow | Red | Blue | Green | PickAColor
            deriving(Show, Eq)
            
colors :: [Color]
colors = [Yellow, Red, Blue, Green]

-- num - The score of the card. If there is a regular card, it is also the number shown in the card.
-- clr - The color of the card; NoColor indicates Wild or WildDrawFour
-- cardType - indicate the type of the card
-- desc: the abilities of the card
data Card = Card { num :: Int
                 , clr :: Color
                 , cardType :: CardType
                 , desc :: String
}

instance Show Card where
  show (Card nm cr t d) 
                    | nm > 9    = show t ++ ": score - " ++ show nm ++ "; color: " ++ show cr ++", effect - " ++ d ++ "\n"
                    | otherwise = show t ++ "("++show nm++", "++show cr++")"++ "\n"

data Direction = Clockwise | CounterClockwise
                deriving(Show, Eq)    

data PlayerState =  PlayerState{
      id :: Int
    , name :: String
    , score :: Int
    , cardsInHand :: [Card]
} deriving(Show)

getPlayerId :: PlayerState -> Int
getPlayerId (PlayerState _id _ _ _) = _id

data GameState = GameState {
      dir :: Direction
    , realPlayer :: Int
    , whoseTurn :: Int
    , currCard :: Card
    , players :: [PlayerState] 
    , deck :: [Card]
} deriving(Show)

type Game = StateT GameState IO

type Deck = [Card]

playerNames::[String]
playerNames = ["Alice", "Joe", "Mike", "Emily"]

