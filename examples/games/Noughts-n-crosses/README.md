
# MeTTa Tic-Tac-Toe Simulation

This project demonstrates a **Tic-Tac-Toe** game where two AI players ("X" and "O") play against each other using different inference engines.

It benchmarks and compares the execution time of:
- **MeTTaLog Compiler**: `0.04 seconds`
- **MeTTaLog Interpreter**: `6.21 seconds`
- **Hyperon Experimental**: `62.06 seconds`

## Usage

To run the simulation, use the following commands:

```bash
mettalog --compile=full TicTakToe.metta
mettalog TicTakToe.metta
metta TicTakToe.metta
```

- `--compile=full` fully compiles the `.metta` file using the MeTTaLog compiler.
- Running without `--compile` interprets the `.metta` file.
- `metta` runs it using the base interpreter (Hyperon Experimental).

## Game Overview

The Tic-Tac-Toe board is represented like this:

```
 1 | 2 | 3
-----------
 4 | 5 | 6
-----------
 7 | 8 | 9
```

Each number corresponds to a cell, and players take turns marking a cell (`X` or `O`).

### Sample Playthrough Summary

- **First Move**: X plays at cell 5 (random).
- **O's Move**: O plays at cell 9 (random).
- **X's Move**: X plays at cell 4.
- **O's Move**: O blocks X by playing at cell 6.
- **X's Move**: X blocks O by playing at cell 3.
- **O's Move**: O blocks X by playing at cell 7.
- **X's Move**: X blocks O by playing at cell 8.
- **O's Move**: O blocks X by playing at cell 2.
- **X's Move**: X plays at cell 1.

**Result**:  
The game ended in a **draw**.

## Performance Comparison

| Engine               | Time Taken |
|----------------------|------------|
| MeTTaLog Compiler     | 0.04s      |
| MeTTaLog Interpreter  | 6.21s      |
| Hyperon Experimental  | 62.06s     |

- **Compiler** is extremely fast.
- **Interpreter** is slower but playable.
- **Hyperon Experimental** is significantly slower.

## Notes

- The AI checks for winning moves and blocks the opponent if necessary.
- If no immediate threat or opportunity is found, it selects a random move.
- This simulation demonstrates basic **threat detection** and **blocking strategies**.

## Full Playthrough Log

```
; MeTTaLog-Compiler:  0.04 seconds.
; MeTTaLog-Interpeter:  6.21 seconds.
; Hyperon-Experimatal: 62.06 seconds.

       1 | 2 | 3
      -----------
       4 | 5 | 6
      -----------
       7 | 8 | 9

("Computer is moving as " X "...")
("? No win or threat — picking a random move.")
(X Move 5)

       1 | 2 | 3
      -----------
       4 | X | 6
      -----------
       7 | 8 | 9

("Computer is moving as " O "...")
("? No win or threat — picking a random move.")
(O Move 9)

       1 | 2 | 3
      -----------
       4 | X | 6
      -----------
       7 | 8 | O

("Computer is moving as " X "...")
("? No win or threat — picking a random move.")
(X Move 4)

       1 | 2 | 3
      -----------
       X | X | 6
      -----------
       7 | 8 | O

("Computer is moving as " O "...")
("?? Blocking " X " from winning!")
(O Move 6)

       1 | 2 | 3
      -----------
       X | X | O
      -----------
       7 | 8 | O

("Computer is moving as " X "...")
("?? Blocking " O " from winning!")
(X Move 3)

       1 | 2 | X
      -----------
       X | X | O
      -----------
       7 | 8 | O

("Computer is moving as " O "...")
("?? Blocking " X " from winning!")
(O Move 7)

       1 | 2 | X
      -----------
       X | X | O
      -----------
       O | 8 | O

("Computer is moving as " X "...")
("?? Blocking " O " from winning!")
(X Move 8)

       1 | 2 | X
      -----------
       X | X | O
      -----------
       O | X | O

("Computer is moving as " O "...")
("?? Blocking " X " from winning!")
(O Move 2)

       1 | O | X
      -----------
       X | X | O
      -----------
       O | X | O

("Computer is moving as " X "...")
("? No win or threat — picking a random move.")
(X Move 1)

       X | O | X
      -----------
       X | X | O
      -----------
       O | X | O

("It was a draw!")
```

