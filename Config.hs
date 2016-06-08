module Config where

import UnoDataModels
import System.Console.ANSI

cardPile :: Deck
cardPile = let zeros    = [Card 0 c Regular "0" | c <- [Red .. Blue]] in
           let ncards   = [Card n c Regular d | n <- [1..9], c <- [Red .. Blue], d <- [show n]] in
           let funcards = [Card 20 c t d | c <- [Red .. Blue], t <- [Skip .. Reverse], d <- [show t]] in
           let blacks   = [Card 50 Black t d | t <- [Wild, WildDrawFour], d <- [show t]]
           in zeros ++ concatMap (replicate 2) (ncards ++ funcards) ++ concatMap (replicate 4) blacks

displayCs :: Deck -> IO ()
displayCs cs = do
    setSGR [SetSwapForegroundBackground True]
    displayCs' 1 cs
    setSGR [Reset]
    setSGR [SetColor Foreground Dull White]
    where displayCs' n [] = putStr "\n"
          displayCs' n (c:cs) = do
              displayC n c
              displayCs' (n + 1) cs

displayC :: Int -> Card -> IO ()
displayC n (Card value color ct d) = do
    putStr $ show n ++ ":"
    setSGR [SetColor Foreground Vivid color]
    case ct of
        Regular -> putStrLn $ "|"++show value ++"|"
        otherwise -> putStrLn $ "|"++show ct ++"|"
    setSGR [SetColor Foreground Dull White]

displayCard :: Card -> IO ()
displayCard (Card value color ct d) = do
    setSGR [SetColor Foreground Vivid color]
    case ct of
        Regular -> putStr $ "|"++show value ++"|"
        otherwise -> putStr $ "|"++show ct ++"|"
    setSGR [SetColor Foreground Dull White]

displayCards :: [Card] -> IO ()
displayCards (c:cs) = do
    setSGR [SetSwapForegroundBackground True]
    displayCard c
    setSGR [Reset]
    putStr " "
    displayCards cs
displayCards [] = do
    setSGR [Reset]
    setSGR [SetColor Foreground Dull White]
    return ()




