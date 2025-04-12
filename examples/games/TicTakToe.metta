; ----------------------------------------
; Entry point
; ----------------------------------------

(Player X)
(Player O)

(= (start-game)
  (superpose (
      (println! "If you want AI to play by itself press enter, otherwise choose X or O.")
      (collapse (case (read-atom)
          ((X (add-atom &self (is-human X)))
            (x (add-atom &self (is-human X)))
            (o (add-atom &self (is-human O)))
            (O (add-atom &self (is-human O)))
            ($_))))
			         
      (play-now))))

(= (play-now) (play-game (1 2 3 4 5 6 7 8 9) X))

; (is-human X)
; ----------------------------------------
; Game loop
; ----------------------------------------

(= (play-game $Board $Player)
  (superpose ((display-board $Board)
      (let $Winner (collapse (winner $Board))
        (if (not (== () $Winner))
          (println! ($Winner "wins!"))
          (if (is-tie $Board)
            (println! ("It was a draw!"))
            (let $Move (choose-move $Board $Player)
              (let $_ (println! ($Player Move $Move))
                (let $NewBoard (move $Board $Move $Player)
                  (play-game $NewBoard (opponent $Player)))))))))))

; ----------------------------------------
; Display the board
; ----------------------------------------

(= (display-board $Nine)
  (println! (format-args "\n
       {} | {} | {}
      -----------
       {} | {} | {}
      -----------
       {} | {} | {}" $Nine)))

; ----------------------------------------
; Choose move
; ----------------------------------------

;; (: choose-move (-> Expression Atom Number))
(= (choose-move $Board $Player)
  (if (== () (collapse (match &self (is-human $Player) (is-human $Player))))
	(computer-move $Board $Player)
	(human-move $Player)))

;; (: human-move (-> Atom Number))
(= (human-move $Player)
  (let $_ (println! ("Your move " $Player " (1–9):")) (read-atom)))


;; (: read-atom (-> Atom))
(= (read-atom)
  (let $s ((py-atom input)) (parse $s)))

; ----------------------------------------
; Computer move logic
; ----------------------------------------

;; (: computer-move (-> Expression Atom Number))
(= (computer-move $Board $Player)
  (let $Opponent (opponent $Player)
    (let $Moves (available-moves $Board)
      (superpose
        ((println! ("Computer is moving as " $Player "..."))
          (let $Move (winning-move $Board $Player)
            (if (non-empty $Move)
              $Move
              (let $Move2 (winning-move $Board $Opponent)
                (if (non-empty $Move2)
                  $Move2
                  (random-atom $Moves))))))))))

(= (move-choices $Board)
    (let $Moves (available-moves $Board)
	  (let $Place (superpose $Moves) $Place)))

(= (winning-move $Board $Player)
    (let $Place (move-choices $Board)
      (let $Player (winner (move $Board $Player $Place))
		   $Place)))

; ----------------------------------------
; Game over / winner
; ----------------------------------------
;; (: available-moves (-> Expression Expression))
(= (available-moves $Board)
  (filter-atom $Board $x (number-p $x) ))

;; (: number-p (-> Atom Bool))
(= (number-p $v) (== Number (get-type $v)))

(= (is-tie $Board) (== () (available-moves $Board)))

;; (: winner (-> Expression Atom))

(= (winner $Board)
  (match &self (win-pos $A $B $C)
    (let $W (nth_x $Board $A)
	  (let $W (nth_x $Board $B)
		(let $W (nth_x $Board $C)
		   $W)))))

; ----------------------------------------
; Win positions
; ----------------------------------------

(win-pos 1 2 3) (win-pos 4 5 6) (win-pos 7 8 9)
(win-pos 1 4 7) (win-pos 2 5 8) (win-pos 3 6 9)
(win-pos 1 5 9) (win-pos 3 5 7)

; ----------------------------------------
; List indexing and replacement
; ----------------------------------------

;; (: nth_x (-> Expression Number Atom))
(= (nth_x $list $index)
  (if (== $index 1)
    (car-atom $list)
    (let* (($rest (cdr-atom $list))
           ($idx (- $index 1)))
         (nth_x $rest $idx))))

;; (: move (-> Expression Number Atom Expression))
(= (move $list $index $replacement)
  (if (== $index 1)
    (cons-atom $replacement (cdr-atom $list))
    (let* (($head (car-atom $list))
           ($rest (cdr-atom $list))
           ($idx (- $index 1))
           ($newrest (move $rest $idx $replacement)))
         (cons-atom $head $newrest))))

; ----------------------------------------
; Opponent function
; ----------------------------------------

;; (: opponent (-> Atom Atom))
(= (opponent X) O)
(= (opponent O) X)


;; (: random-atom (-> Expression Atom))
(= (random-atom $list)
  (let $len (size-atom $list)
    (let $index (random-int &rng 1 (+ $len 1))
      (nth_x $list $index))))


; !(start-game)

