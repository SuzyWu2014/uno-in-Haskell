module UnoDataModels where

import Control.Monad.State

data CardType = Skip  
              | DrawTwo  
              | Reverse  
              | Wild
              | WildDrawFour
              | Regular  
              deriving(Eq, Enum)

instance Show CardType where
  show Skip         = "Next player in sequence misses a turn"
  show DrawTwo      = "Next player in sequence draws two cards and misses a turn"
  show Reverse      = "Order of play switches directions"
  show Wild         = "Player declares next color(any color) to be matched"
  show WildDrawFour = "Player declares next color to be matched; next player in sequence draws four cards and loses a turn."
  show _            = "number card"
              
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
} deriving(Eq)

instance Show Card where
  show (Card nm cr t _) 
      | nm <= 9                    =  " "++show t ++ "("++show nm++", "++show cr++")"  
      | t == Skip || t == Reverse || t == DrawTwo  = " "++  show t ++ "(" ++ show cr ++")"
      | otherwise                  =  " "++show t


data Direction = Clockwise | CounterClockwise
                deriving(Show, Eq)    

data PlayerState =  PlayerState{
      pId :: Int
    , name :: String
    , score :: Int
    , cardsInHand :: [Card]
} deriving(Show)

instance Eq PlayerState where
  _player1 == _player2 = score _player1 == score _player2

instance Ord PlayerState where
  _player1 <= _player2 =  score _player1 <= score _player2
  _player1 > _player2  =  score _player1 > score _player2

data GameState = GameState {
      dir :: Direction
    , realPlayer :: Int
    , whoseTurn :: Int
    , currCard :: Card
    , players :: [PlayerState] 
    , deck :: [Card]
    , isOver :: Bool
    , ithTurn :: Int
} deriving(Show)

type Game = StateT GameState IO

type Deck = [Card]

playerNames::[String]
playerNames = ["1-Alice", "2-Joe", "3-Mike", "4-Emily"]

