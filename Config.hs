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
    where displayCs' _ []     = putStr "\n"
          displayCs' n (c:xs) = do
                    displayC n c
                    displayCs' (n + 1) xs

displayC :: Int -> Card -> IO ()
displayC n (Card value color ct _) = do
    putStr $ show n ++ ":"
    setSGR [SetColor Foreground Vivid color]
    case ct of
        Regular -> putStrLn $ "|"++show value ++"|"
        _ -> putStrLn $ "|"++show ct ++"|"
    setSGR [SetColor Foreground Dull White]

displayCard :: Card -> IO ()
displayCard (Card value color ct _) = do
    setSGR [SetColor Foreground Vivid color]
    case ct of
        Regular   -> putStr $ "|"++show value ++"|"
        _ -> putStr $ "|"++show ct ++"|"
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




