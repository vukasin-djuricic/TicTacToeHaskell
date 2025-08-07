# Tic-Tac-Toe: Game Tree Parser and Generator (Haskell)

This project implements a Haskell-based parser and game state manager for Tic-Tac-Toe (X/O), including simulation and full game tree generation.

## Key Features

✅ Parse board state and moves from plain text (e.g. `testInput.txt`)  
✅ Simulate moves using a custom State monad (`GameStateOp`)  
✅ Store history of moves using `GameStateOpHistory`  
✅ Generate a complete game tree from any current board state  
✅ Detect terminal states and winning conditions  
✅ Analyze tree metrics: number of leaves, depth-specific elements, and more  

## Sample Input Format (`testInput.txt`)
First - initial state (shown as X-O table)
Second -  plays structured as [Player] ([coordinate X], [coordinate Y])
```
|X|O| |
| |X| |
| | | |
O (2,2)
X (1,0)
O (1,2)
```

## Example Usage in GHCi

```haskell
parse iksoks "" testInput -- parsing testiInput
runGameState applyMoves iksOksInitialState -- applies multiple plays using monad (applyMoves) on empty initial state
-- find all combinations where X wins in 3 moves, X has next move:
filter zavrsnoStanje (elemsOnDepth 3 (fst( runGameState applyMovesTest iksOksInitialState)))
```

## Notes

- The parser is built using `Parsec`.
- Game logic is implemented through custom monads `GameStateOp` and `GameStateOpHistory`.
- The project models a general-purpose rose tree (`RoseTree`) to represent move trees.
- Includes utility functions for analyzing tree structure and outcome prediction.

