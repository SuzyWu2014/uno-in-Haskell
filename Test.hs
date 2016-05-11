module Test where

import UnoDataModels
import GameSim
import CardEffects
import Control.Monad
import Uno


main :: IO ()
main = uno

showDeck::[Card]
showDeck = initDeck

showInitState :: GameState
showInitState = initGame 4 "Beaver"

dealing :: IO GameState
dealing = dealCards 4 showInitState

