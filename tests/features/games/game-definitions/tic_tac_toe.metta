; Tic-Tac-Toe board represented as a list of 9 elements
(: board-state (-> Expression Atom)) ; The board-state is an expression type, stored as an atom.

; Constants for players and empty cells
(: X cv) (: O cv) (: . cv) ; Defining symbols for 'X', 'O', and empty cell ('.').

; Define players
(: X Player) (: O Player) ; 'X' and 'O' are both players.

; Determine the opponent of the current player
(: opponent (-> Player Player)) ; Returns the opponent of the current player.
(= (opponent X) O) ; X's opponent is O.
(= (opponent O) X) ; O's opponent is X.

; Initial empty board
!(add-atom &self (board-state (. . . . . . . . .))) ; Initialize the board with empty cells.

; Function to display the current board
(: display-board (-> board-state Atom)) ; This function takes the board-state and returns an Atom.
(= (display-board)
   (match &self (board-state $list)
      (println! (format-args "\n
       {} | {} | {} \n
      --------- \n
       {} | {} | {} \n
      --------- \n
       {} | {} | {} \n
      " $list)))) ; Formats the board as a 3x3 grid for display.

!(println! (format-args "\n
       {} | {} | {} \n
      --------- \n
       {} | {} | {} \n
      --------- \n
       {} | {} | {} \n
      " (1 2 3 4 5 6 7 8 9)))

; Helper function to get the nth element (1-indexed)
(: nth (-> Int Expression Atom)) ; Retrieves the nth element from the board.
(= (nth 1 $list) (car-atom $list)) ; Base case: when n is 1, return the first element (car-atom).
(= (nth $n $list) 
   (nth (- $n 1) (cdr-atom $list))) ; Recursion: move to the next element (cdr-atom) and decrease n.

; Replace nth element in a list (1-indexed)
(: replace-nth (-> Int Atom Expression Expression)) ; Replaces the nth element in a list with a new value.
(= (replace-nth 1 $val $list)
   (cons-atom $val (cdr-atom $list))) ; Base case: replace the first element and keep the rest unchanged.
(= (replace-nth $n $val $list)
   (cons-atom (car-atom $list) (replace-nth (- $n 1) $val (cdr-atom $list)))) ; Recursive step to replace the nth element.

; Check for win conditions
(: win-state (-> Expression Player Bool)) ; Checks if a player has won.
; The following patterns match different winning conditions (rows, columns, diagonals).
(= (win-state ($s $s $s $_ $_ $_ $_ $_ $_) $s) True) ; Row 1
(= (win-state ($_ $_ $_ $s $s $s $_ $_ $_) $s) True) ; Row 2
(= (win-state ($_ $_ $_ $_ $_ $_ $s $s $s) $s) True) ; Row 3
(= (win-state ($s $_ $_ $s $_ $_ $s $_ $_) $s) True) ; Column 1
(= (win-state ($_ $s $_ $_ $s $_ $_ $s $_) $s) True) ; Column 2
(= (win-state ($_ $_ $s $_ $_ $s $_ $_ $s) $s) True) ; Column 3
(= (win-state ($s $_ $_ $_ $s $_ $_ $_ $s) $s) True) ; Diagonal 1
(= (win-state ($_ $_ $s $_ $s $_ $s $_ $_) $s) True) ; Diagonal 2

; Check if a player has won
(: check-win (-> board-state Player Bool)) ; Determines if the specified player has a winning condition.
(= (check-win $board $player) 
   (win-state $board $player)) ; Calls the win-state function to check for winning conditions.

; Make a move on the board
(: make-move (-> Expression Player Int Expression)) ; Places a player's mark ('X' or 'O') on the board at a specific position.
(= (make-move $board $player $pos)
   (replace-nth $pos $player $board)) ; Replaces the nth position with the player's symbol.

; Game loop for human player
(: game-loop-human (-> Player Atom)) ; Human player’s turn loop.
(= (game-loop-human $human)
   (do
     ((display-board) ; Display the current board.
      (println! (format-args "Player {}'s turn." ($human))) ; Print whose turn it is.
      (match &self (board-state $board-before)) ; Retrieve the current board-state.
      (let $pos (get-player-move) ; Get a valid move from the player.
         (and (nth $pos $board-before .) ; Ensure the selected position is empty.
              (let $board-after (make-move $board-before $human $pos) ; Update the board with the player's move.
                (do ((remove-atom &self (board-state $board-before)) ; Remove the old board-state.
                      (add-atom &self (board-state $board-after)) ; Add the new board-state.
                      (if (check-win $board-after $human) ; Check if the player has won.
                          (println! (format-args "Player {} wins!" ($human))) ; Announce the winner.
                          (game-loop-computer (opponent $human))))))))))) ; If no win, continue to the computer's turn.

; Get validated player move (1-9)
(: get-player-move (-> Int)) ; Function to get a move from the player.
(= (get-player-move)
   (progn 
    ((println! "Enter position (1-9):")
     (flush-output!)
     (let $pos (- (get-single-char!) 48) ; Convert the input character to an integer (1-9).
      (if (and (>= $pos 1) (<= $pos 9)) ; Ensure the move is within valid range.
         $pos ; Return the valid position.
         (progn 
	   ((println! "Invalid move! Enter position (1-9):") ; Prompt for another input if invalid.
              (get-player-move))))))))

; Random move for computer
(: random-move (-> board-state Int)) ; Generates a random valid move for the computer.
(= (random-move $board)
   (let* (($positions (collapse ; Collapse the list of valid positions.
                        (let $i (between! 1 9) ; For each position from 1-9,
                          (if (nth $i $board .) $i))))) ; Check if the position is empty and available.
     (if (== $positions ()) -1 ; If no positions are available, return -1.
         (random-element! $positions)))) ; Otherwise, return a random valid position.

; Game loop for the computer player
(: game-loop-computer (-> Player Atom)) ; Computer player's turn loop.
(= (game-loop-computer $computer) ; The computer is $computer
   (do
    ((match &self (board-state $board)) ; Retrieve the current board-state.
     (let* (($win-move (find-winning-move $board $computer)) ; Check if the computer has a winning move.
	    ($opponent (opponent $computer)) ; The human player's symbol.
            ($block-move (find-winning-move $board $opponent)) ; Check if the human can win and block the move.
            ($random-move (random-move $board))) ; If no win/block, choose a random move.
       (let $pos (if (> $win-move 0) $win-move ; Prioritize winning.
                    (if (> $block-move 0) $block-move ; Then block the opponent's winning move.
                        $random-move))) ; Otherwise, make a random move.
         (if (>= $pos 1) ; If a valid move exists,
             (let $board-after (make-move $board $computer $pos) ; Make the move.
               (do ((remove-atom &self (board-state $board)) ; Update the board state.
                    (add-atom &self (board-state $board-after))
    	            (if (check-win $board-after $computer) ; Check if the computer won.
			   (println! "Computer wins!") ; Announce the win.
			   (game-loop-human $opponent)))))))))) ; Otherwise, continue to the human's turn.

; Start the game
; !(game-loop-human X) ; Start the game with the human player ('X').


