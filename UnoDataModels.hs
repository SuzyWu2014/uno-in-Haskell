module UnoDataModels where

data Card = Skip Color
          | DrawTwo Color
          | Reverse Color
          | Wild
          | WildDrawFour
          | Regular Color Int 

data Direction = Clockwise | CounterClockwise

data Color = Yellow | Red | Blue | Green

data PlayerState = PlayerState{
      id :: Int
    , name :: String
    , numOfCards :: Int
    , score :: Int
    , cardsInHand :: [(Int,Card)]
}

type Deck = [Card]

data GameState = GameState {
      dir :: Direction
    , clr :: Color
    , num :: Int
    , whoseTurn :: Int
    , numOfPlayers :: Int
    , players :: [PlayerState]
    , currCard :: Card
    , countDeck :: Int
    , deck :: Deck
}

