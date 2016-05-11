module UnoDataModels where

-- data Card = Skip Color
--           | DrawTwo Color
--           | Reverse Color
--           | Wild
--           | WildDrawFour
--           | Regular Color Int 

data CardType = Skip  
              | DrawTwo  
              | Reverse  
              | Wild
              | WildDrawFour
              | Regular  
              deriving(Show)
data Color = Yellow | Red | Blue | Green
            deriving(Show)
-- ?? Card effect, what if doesn't need to interacter with user
-- num - The number in card if there is one.
-- clr - The color of the card if it's not Wild or WildDrawFour
-- effect - a function that takes the game state and produce an IO game, which maybe interact with the user to apply the effect of the card
-- cardType - indicate the type of the card
-- desc: the description of the card
data Card = Card { num :: Maybe Int
                 , clr :: Maybe Color
                 , effect :: GameState -> IO GameState
                 , cardType :: CardType
                 , desc :: String
}
instance Show Card where
  show (Card nm cr _ t d) = case (nm, cr) of
                  (Just n, Just c)   -> show t ++ "("++show n++", "++show c++"): "++d
                  (Nothing, Just c)  -> show t ++ " - " ++ show c ++ ": "++d
                  (Nothing, Nothing) -> show t ++ ": "++ d
                  _                  -> show d -- ?

data Direction = Clockwise | CounterClockwise
                deriving(Show)    

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
    , currClr :: Color -- only check this when current card is Wild/WildDrawFour
    , realPlayer :: Int
    , whoseTurn :: Int
    , currCard :: Card
    , players :: [PlayerState] 
    , deck :: [Card]
} deriving(Show)

type Deck = [Card]
