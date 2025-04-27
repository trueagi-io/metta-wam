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

