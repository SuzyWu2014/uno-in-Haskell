# uno-in-Haskell
Implementing UNO in Haskell.

You can download this project [right here](https://github.com/SuzyWu2014/uno-in-Haskell)

# Milestone 1
1. Our goal is to implement one player versus a few bot in UNO card game. So far, we have implemented game initialization, including initializing the deck with shuffle, dealing cards to each players. Additionally, we have implemented drawing cards function, some cards effect.
2. In GHCi, users can run the Test.hs to run our project. When you run the game, you first input the name of the player, and then give a number of total players. After initializing game, you can see the cards each one hold(just for test case, actually player can't see others' cards in hand) and the current game state.
3. There are some problem we need to solve:
	* How to implement each card effect function connecting with the effect attribute in data Card, respectively?
	* How to design AI?