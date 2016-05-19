module UnoDataModels where

import Control.Monad.State

data CardType = Skip  
              | DrawTwo  
              | Reverse  
              | Wild
              | WildDrawFour
              | Regular  
              deriving(Show, Eq, Enum)
              
data Color = Yellow | Red | Blue | Green | PickAColor
            deriving(Show, Eq, Enum)
            
colors :: [Color]
colors = [Yellow, Red, Blue, Green]

-- num - The score of the card. If there is a regular card, it is also the number shown in the card.
-- clr - The color of the card; PickAColor indicates Wild or WildDrawFour
-- cardType - indicate the type of the card
-- desc: the abilities of the card
data Card = Card { num :: Int
                 , clr :: Color
                 , cardType :: CardType
                 , desc :: String
}

instance Show Card where
  show (Card nm cr t d) 
                    | nm > 9    = "\n" ++ show t ++ ": score - " ++ show nm ++ "; color: " ++ show cr ++", effect - " ++ d ++ "\n"
                    | otherwise = "\n" ++ show t ++ "("++show nm++", "++show cr++")"  ++ "\n"
data Direction = Clockwise | CounterClockwise
                deriving(Show, Eq)    

data PlayerState =  PlayerState{
      pId :: Int
    , name :: String
    , score :: Int
    , cardsInHand :: [Card]
    , isUno :: Bool
} deriving(Show)

instance Eq PlayerState where
  _player1 == _player2 = score _player1 == score _player2

instance Ord PlayerState where
  _player1 <= _player2 =  score _player1 <= score _player2

data GameState = GameState {
      dir :: Direction
    , realPlayer :: Int
    , whoseTurn :: Int
    , currCard :: Card
    , players :: [PlayerState] 
    , deck :: [Card]
    , isOver :: Bool
} deriving(Show)

type Game = StateT GameState IO

type Deck = [Card]

playerNames::[String]
playerNames = ["Alice", "Joe", "Mike", "Emily"]

