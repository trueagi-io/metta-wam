;; Declare 'X' and 'O' as valid Player atoms
(: X Player) 
(: O Player)

;; Define the opponent function: returns the other player
(: opponent (-> Player Player))
(= (opponent X) O)
(= (opponent O) X)

;; ----------------------------------------
;; Entry point: Starts the game
;; ----------------------------------------

(= (start-game)
  (let* 
    ;; Prompt user to choose player or press Enter for AI vs AI
    (($_1 (println! "If you want AI to play by itself press enter, otherwise choose X or O."))
     ;; Read user input and assign human player if applicable
     ($_2 (case (read-atom)
          ((X (add-atom &self (is-human X)))
            (x (add-atom &self (is-human X)))
            (o (add-atom &self (is-human O)))
            (O (add-atom &self (is-human O)))
            ($_))))
    ;; Begin game
    ($_3 (play-now))) True))

;; Start the game loop with an empty board and 'X' going first
(= (play-now) (play-game (1 2 3 4 5 6 7 8 9) X))

;; ----------------------------------------
;; Main game loop
;; ----------------------------------------

(= (play-game $Board $Player)
  (let $_1 (display-board $Board) ;; Show current board
      (let $Winner (collapse (winner $Board)) ;; Check if there's a winner
        (if (non-empty $Winner)
          (println! ($Winner "wins!")) ;; Declare winner
          (if (is-tie $Board)
            (println! ("It was a draw!")) ;; Declare draw
            ;; Otherwise, get move and continue the game
            (let $Move (choose-move $Board $Player)
              (let $_2 (println! ($Player Move $Move))
                (let $NewBoard (move $Board $Move $Player)
                  (play-game $NewBoard (opponent $Player))))))))))

;; ----------------------------------------
;; Display board in traditional format
;; ----------------------------------------

(= (display-board $Nine)
  (println! (format-args "\n
       {} | {} | {}
      -----------
       {} | {} | {}
      -----------
       {} | {} | {}" $Nine)))

;; ----------------------------------------
;; Determine who chooses the move: human or computer
;; ----------------------------------------

(= (choose-move $Board $Player)
  (if (== () (collapse (match &self (is-human $Player) True)))
    (computer-move $Board $Player) ;; AI move
    (human-move $Player)))         ;; Human move

(= (human-move $Player)
  (let $_ (println! ("Your move " $Player " (1–9):")) (read-atom)))

;; Parse user input into an Atom
(= (read-atom)
  (let $s ((py-atom input)) (parse $s)))

;; ----------------------------------------
;; AI move logic
;; ----------------------------------------

(= (computer-move $Board $Player)
  (let $Opponent (opponent $Player)
    (let $Moves (available-moves $Board)
      (let $_ (println! ("Computer is moving as " $Player "..."))
          (let $Move (collapse (winning-move $Board $Player))
            (if (non-empty $Move)
              (car-atom $Move) ;; Take win if possible
              (let $Move2 (collapse (winning-move $Board $Opponent))
                (if (non-empty $Move2)
                  (car-atom $Move2) ;; Block opponent if needed
                  (random-atom $Moves))))))))) ;; Else random move

;; Wrapper to produce each possible move position as choice
(= (move-choices $Board)
    (let $Moves (available-moves $Board)
	   (superpose $Moves)))

;; Determine if a move leads to a win
(= (winning-move $Board $Player)
    (let $Place (move-choices $Board)
      (let $Player (winner (move $Board $Place $Player))
		   $Place)))

;; Utility to check if a list/expression is non-empty
(: non-empty (-> Expression Bool))
(= (non-empty $list) (not (== () $list)))

;; ----------------------------------------
;; Move validation and winner detection
;; ----------------------------------------

;; Filters out positions that are already taken (i.e., not numbers)
(= (available-moves-mettalog-only $Board) (filter-atom $Board $x (eval (number-p $x))))

(: number-p (-> Atom Bool))
(= (number-p $v) (== Number (get-type $v)))

;; Recursive version to collect all numbered positions (i.e., available spots)
(= (available-moves $list)
  (if (== () $list)
	$list	
    (let* (($head (car-atom $list))
           ($rest (cdr-atom $list))
		   ($newrest (available-moves $rest)))
	   (if (== Number (get-type $head))
		  (cons-atom $head $newrest)
		  $newrest))))

;; Tie occurs when there are no more available moves
(= (is-tie $Board) (== () (available-moves $Board)))

;; Define all 8 win positions on a tic-tac-toe board
(win-pos 1 2 3) (win-pos 4 5 6) (win-pos 7 8 9)
(win-pos 1 4 7) (win-pos 2 5 8) (win-pos 3 6 9)
(win-pos 1 5 9) (win-pos 3 5 7)

;; Evaluate if a player has won by comparing values in win positions
(: winner (-> Expression Atom))
(= (winner $Board)
  (match &self (win-pos $A $B $C)
    (let $W (nth_x $Board $A)
	  (let $W (nth_x $Board $B)
		(let $W (nth_x $Board $C)
		   $W)))))

;; ----------------------------------------
;; List helpers for indexing and replacing elements
;; ----------------------------------------

;; Retrieve the Nth element in a list
(: nth_x (-> Expression Number Atom))
(= (nth_x $list $index)
   (if (== () $list) (empty)
  (if (== $index 1)
    (car-atom $list)
    (let* (($rest (cdr-atom $list))
           ($idx (- $index 1)))
         (nth_x $rest $idx)))))

;; Replace the Nth element in a list with a new value
(: move (-> Expression Number Atom Expression))
(= (move $list $index $replacement)
  (if (== $index 1)
    (let $rest (cdr-atom $list) (cons-atom $replacement $rest))
    (let* (($head (car-atom $list))
           ($rest (cdr-atom $list))
           ($idx (- $index 1))
           ($newrest (move $rest $idx $replacement)))
         (cons-atom $head $newrest))))

;; Pick a random element from a list
(: random-atom (-> Expression Atom))
(= (random-atom $list)
  (let $len (size-atom $list)
    (let $index (random-int &rng 1 (+ $len 1))
      (nth_x $list $index))))

;; ----------------------------------------
;; Sanity tests (not part of main game loop)
;; ----------------------------------------

(= (sanity-tests)
  (superpose (
	 (println! "what is your name?")
     (println! ("Hello " (read-atom)))
	 (display-board
		(X X X 4 5 6 O O O))
	 (println!
		(random-atom (1 2 3 4 5 6 7 8 9)))
	 (println!
		(random-atom (1 2 3 4 5 6 7 8 9)))
	 (println!
		(winner (O O 3 4 5 6 X X X)))
	 (println!
		(available-moves (O O 3 4 5 6 X X X)))
	 (println!
		(move-choices (O O 3 4 5 6 X X X)))
	 (println!
		(winning-move (O O 3 4 5 6 7 8 9) O))
  )))

;; Uncomment one of these to run it interactively:
;!(sanity-tests)
; !(start-game)
; (is-human O)

;; Run AI vs AI by default
!(play-now)

