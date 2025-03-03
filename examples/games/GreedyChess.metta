;		WORK IN PROGRESS!!, Will start but not play yet!

;   Mettalog Project 2024

;   Function:  Play chess (human vs MeTTa program) using a fairly simple "greedy" approach
;              with moves that do not project possible boards beyond the present board.
;   Input:     User's commands and moves from console.
;   Output:    Chess board displayed to console with computer's move.
;
;**********************************************************************
; * Redistribution and use in source and binary forms, with or without
; * modification, are permitted provided that the following conditions
; * are met:
; *
; * 1. Redistributions of source code must retain the above copyright
; *    notice, this list of conditions and the following disclaimer.
; *
; * 2. Redistributions in binary form must reproduce the above copyright
; *    notice, this list of conditions and the following disclaimer in the
; *    documentation and/or other materials provided with the
; *    distribution.
; *
; * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; * POSSIBILITY OF SUCH DAMAGE.
;**********************************************************************
; *

;*******************************************************
; Global type definitions
;*******************************************************
(: game-state (-> Expression Atom))             ; create an atom for game state (eg., "started" "checkmate")
(: board-state (-> Expression Atom))       ; for saving the board after each move 

;*******************************************************
; General utility functions
;*******************************************************

; nth function, eg:  (nth 2 (a b c)), answer = b
; if index too high, returns NIL
(= (nth $n $list) 
  (if (> $n (size-atom $list))
    NIL
    (if (== $n 1)
        (car-atom $list)
        (nth (- $n 1) (cdr-atom $list))))) ; Recursion: move to the next element (cdr-atom) and decrease n.

; Input a list and symbol, then return True if found, else False. Eg:  (contains_symbol (a b c) b) returns True.
(= (contains_symbol $list $sym) 
    (if (== $list ())
        False
        (if (== (car-atom $list) $sym)
            True
            (contains_symbol (cdr-atom $list) $sym))))

; convert a char to int, if invalid returns "no results"
(= (char_to_int $char)
   (if (== $char "0") 0
   (if (== $char "1") 1
   (if (== $char "2") 2
   (if (== $char "3") 3
   (if (== $char "4") 4
   (if (== $char "5") 5
   (if (== $char "6") 6
   (if (== $char "7") 7
   (if (== $char "8") 8
   (if (== $char "9") 9
   )))))))))))

; Input a list of lists. Output the concatenation of the input flattened (only flattens at top level).
; For example:  
;   Input list of lists    =  ((1 2) (3 4) () (5)) 
;   Output list            =  (1 2 3 4 5)
(= (concat_lists $ListofLists)
  (if (== $ListofLists () )
      ()
      (let $first_list (car-atom $ListofLists)
        (if (== $first_list ())
            (concat_lists (cdr-atom $ListofLists))
            (let*
              (
              ($a (car-atom $first_list))   ;$a = first element first list
              ($b (cdr-atom $first_list))   ;$b = rest of first list
              ($c (cdr-atom $ListofLists))  ;$c = all the remaining lists
              ) ;$d = remaining elements of first list and rest of lists
                ;$f = the concatenation of the rest of the lists
                ;we return the first element of the first list and concatenation of the remainder.
              (let $d (cons-atom $b $c) (let $f (concat_lists $d) (cons-atom $a $f )))  
            )))))
;
; Execute a block of unevaluated statements sequentially.
; Input: (do_quoted
;          (
;          (quote ( some function )) 
;          (quote ( some function )) 
;          .....))
; Result:  Evaluate each function in sequence.
;                    
(: (do_quoted) (-> Expression Atom))
(= (do_quoted $exp) 
      (if (== $exp ())
            empty
            (let () (unquote (car-atom $exp)) (do_quoted (cdr-atom $exp)))))
;

;
;#( = #(removelists () $A $A) True )
;; /* apparently swipl used: nth1(A, B, C):-integer(A), !, D is A-1, nth0_det(D, B, C). nth1(A, B, C):-var(A), ;!, nth_gen(B, C, 1, A). */
;(= (removelists #(Cons $A $B) #(Cons $A $C) $D)  
;  (removelists $B $C $D))
;(= (removelists $A #(Cons $B $C) #(Cons $B $D))  
;  (removelists $A $C $D))
;
;
;(= (len Nil 0)  
;  (set-det))
;(= (len #( :: ($A) ) 1) (atomic $A) (set-det))
;(= (len #(Cons $A $B) $C) (atomic $A) (len $B $D) (is $C (+ $D 1)))
;
;
;(= (returnrandominteger $A $B)  
;  (is $A 
;    (+ 
;      (random $B) 1)))
;

;*******************************************************
; Initialization 
;*******************************************************

(piece k) 
(piece q) 
(piece r) 
(piece b) 
(piece n) 
(piece p) 

(hpiece k) 
(hpiece q) 
(hpiece r) 
(hpiece b) 
(hpiece n) 
(hpiece p) 

(rpiece p) 
(rpiece n) 
(rpiece b) 
(rpiece r) 
(rpiece q) 
(rpiece k) 

(cord 1) 
(cord 2) 
(cord 3) 
(cord 4) 
(cord 5) 
(cord 6) 
(cord 7) 
(cord 8) 

!(add-atom &self (game-state initializing))

;*******************************************************
; Code invoked by the basic commands 
;*******************************************************

; (add-pieces)
; input the board as a list and create atoms of form '(square x y color piece)'
(= (add-pieces $board)
  (if (== $board ())
      True
      (let* (
            ($next-square (car-atom $board))
            ($next-square-atom (cons-atom square $next-square))
            ($_ (add-atom &self $next_square-atom))
            )
      (add-pieces (cdr-atom $board)))))

; (reset-pieces)
; input:    present board of form ( (piece-1) (piece-2) (empty-square-1)... etc )
; output:   individual atoms of the form (square x1 y1 color rank)  
(= (reset-pieces $board)
    (progn
      ; remove prior occupied square atoms from atomspace
      (remove-atom &self (square $x $y $s $p))  
      ; remove empty squares too
      (remove-atom &self (square $x $y))  
      ; now add the present board's squares to atomspace.
      (add-pieces $board)))

; (delete-pieces)
; output:   removes all pieces   
(= (delete-pieces)
    (progn
      ; remove prior occupied square atoms from atomspace
      (remove-atom &self (square $x $y $s $p))  
      ; remove empty squares too
      (remove-atom &self (square $x $y))  
      ))

(= (display_squares_atoms_debug_4)
    (match &self (square $x $y $s $p) 
        (progn 
            (println! "square")
            (println! $x)
            (println! $y)
            (println! $s)
            (println! $p))))
(= (display_squares_atoms_debug_2)
    (match &self (square $x $y) 
        (progn 
            (println! "square")
            (println! $x)
            (println! $y))))

; display game-state for debugging
(= (display-game-state)
    (match &self (game-state $msg) 
        (println! $msg)))

; reset state by deleting all prior 'game-state' atoms
(= (reset-state)
    (match &self (game-state $msg) 
        (remove-atom &self (game-state $msg))))

; Invoke with empty list, will return characters input from console until ENTER.
(: (get-player-command (-> list list)))
(= (get-player-command $input_list)  
   (let $cmd (get-single-char!)
      (progn 
          ; if initial execution flush output
          (if (== (size-atom $input_list) 0) (flush-output!) ())
          (if (== $cmd 13) ; if user hit <ENTER>
            ;return all input
            $input_list    
            ;else gather more input
            (let $new_list (cons-atom $cmd $input_list) (get-player-command $new_list))))))

; write welcome banner to console and call display_board to print the pieces
(= (welcome)  
 (progn   
    ((py-atom print) " ") ((py-atom print) " ") ((py-atom print) " ") ((py-atom print) " ")
    ((py-atom print) "M E T T A    G R E E D Y   C H E S S")
    ((py-atom print) "This program is a MeTTa exercise which takes the best immediate move without planning far ahead.") 
    (display_board (match &self (board-state $board) $board)) 
    ((py-atom print) "- Your pieces are marked with an asterisk.")
    ((py-atom print) "-------- C o m m a n d s -----------")
    ((py-atom print) "1 TO MOVE YOUR PIECE USE example   ->  M <ENTER> 1213 <ENTER>")
    ((py-atom print) "   Result:  YOUR pawn in 1,2 moved to location 1,3 based on standard cartesian x/y.")
    ((py-atom print) "2 Move MeTTa Greedy Chess          ->  G")
    ((py-atom print) "3 Reset                            ->  R")
    ((py-atom print) "4 Commands List                    ->  C")
    ((py-atom print) "5 Display Board                    ->  D")
    ((py-atom print) "6 Quit                             ->  Q")
    ))

; identify_piece inputs an expression of a piece, eg: "(2 1 s n)," and outputs a shorted two character string 
; describing the piece, eg: "*n" which means that it is the human player's knight (the astrisk denotes human player pieces).
(: identify_piece (-> list symbol))
(= (identify_piece $p) 
   (if (== (size-atom $p) 2)
      "  "
      (let*
        ( ; assign either * or " " 
          ($player (if (== (contains_symbol $p s) True) * " "))
          ; identify piece
          ($piece (nth 4 $p))
        )
        (format-args "{}{}" ($player $piece)))))

; Input the board, output a list of the board easier to read with an identifier for each piece, eg., human king is "*k."
(: display_filter (-> list list))
(= (display_filter $brd) 
    (if (== (size-atom $brd) 1) 
      ; if on last piece, return a one element list of this form eg.:  (*k). Extra parens are needed to create list.
      ( (identify_piece (car-atom $brd)) )
      ; otherwise convert all pieces to shorter description for display.
      (let $rest (display_filter (cdr-atom $brd))  (cons-atom (identify_piece (car-atom $brd)) $rest))))

(= (display_board $board) 
  (
   (let* (
          ($a (display_filter     $board ))
          )
          ((py-atom print) (format-args "\n
        --1-----2-----3-----4-----5-----6-----7-----8-- \n
      8| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |8\n
       |-----------------------------------------------| \n
      7| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |7\n
       |-----------------------------------------------| \n
      6| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |6\n
       |-----------------------------------------------| \n
      5| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |5\n
       |-----------------------------------------------| \n
      4| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |4\n
       |-----------------------------------------------| \n
      3| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |3\n
       |-----------------------------------------------| \n
      2| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |2\n
       |-----------------------------------------------| \n
      1| {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |1\n
        --1-----2-----3-----4-----5-----6-----7-----8-- \n
      " 
      
          $a)))))

;
;
;(= (kingnotincheck $A) (xy_box $B #( :: (g k) ) $A) (not (take_dest $B s $A)))
;
;
;(= (attemptcheckmate $A $B $C $D) (xy_box $E #( :: (s k) ) $B) (buildgold $B $F) (set-det) (rpiece $G) (cord ;$H) (cord $I) (member #( :: ($H $I g $G) ) $F) (= $C  
;  #( :: 
;    ($H $I g $G) )) (positiontotake $E $C $D $B) (move_piece $C $D $B $A) (nth1 1 $D $J) (nth1 2 $D $K) ;(return_entire_box #( :: ($J $K) ) $L $A) (threatOK1 $A s g #( :: ($L) )) (threatOK2 $A s g) (threatOK3 $A ;s g) (kingnotincheck $A)) 
;
;
;(= (playdefenseR $A $B $C $D) (returnrandominteger $E 3) (set-det) (or (== $E 1) (== $E 2)) (playdefense $A $B ;$C $D))
;; /* /* newer code start */ /* see if anybody can check the silver king first... if possible do next rule ;(long) */ attemptcheckmate(Newboard,Listofboxes,Goldbox,Destbox) :- /* find silver king */ xy_box(Kingbox,[s,;k],Listofboxes), /* return list of all gold pieces */ buildgold(Listofboxes,Currentgoldpieces), /* no gold can ;align to check king, sequential check. */ rpiece(Piece), cord(X), cord(Y), member([X,Y,g,Piece],;Currentgoldpieces), positiontotake(Kingbox,[X,Y,g,Piece],Destbox,Listofboxes), !, /* don't try the exhaustive ;search if it doesn't seem likely to work... */ deepattemptcheckmate(Newboard,Listofboxes,Goldbox,Destbox). /* ;move from -Goldbox to -Destbox for checkmate, return -Newboard */ deepattemptcheckmate(Newboard,Listofboxes,;Goldbox,Destbox) :- /* find silver king */ xy_box(Kingbox,[s,k],Listofboxes), /* return list of all gold ;pieces */ buildgold(Listofboxes,Currentgoldpieces), !, /* find *** -Goldbox AND -Destbox *** which can check ;Kingbox */ findgoldcheck(Currentgoldpieces,Listofboxes,Newboard,Goldbox,Destbox,Kingbox). findgoldcheck([],_,_,;_,_,_) :- !, fail. findgoldcheck([Goldbox|_],Listofboxes,Newboard,Goldbox,Destbox,Kingbox) :- /* see if ;Goldbox can be moved into position to take Kingbox */ positiontotake(Kingbox,Goldbox,Destbox,Listofboxes), ;move_piece(Goldbox,Destbox,Listofboxes,Newboard), nth1(1,Destbox,X), nth1(2,Destbox,Y), return_entire_box([X,;Y],EntireBox,Newboard), threatOK1(Newboard,s,g,[EntireBox]), /*can your piece be taken? */ threatOK2(Newboard,;s,g), /* can king move out of the way ?? */ threatOK3(Newboard,s,g). /* can a piece block threat? */ ;findgoldcheck([_|Currentgoldpieces],Listofboxes,Newboard,Goldbox,Destbox,Kingbox) :- findgoldcheck;(Currentgoldpieces,Listofboxes,Newboard,Goldbox,Destbox,Kingbox). /* newer code end */ */
;
;
;(= (playdefense $A $B $C $D) (buildgold $B $E) (checkgold $E $B Nil) (set-det) (fail))
;(= (playdefense $A $B $C $D) (buildgold $B $E) (checkgold $E $B $F) (piece $G) (member #( :: ($H $I g $G) ) ;$F) (= $J  
;  #( :: 
;    ($H $I g $G) )) (hpiece $K) (xy_box $L #( :: (s $K) ) $B) (return_entire_box $L $D $B) (clear_route $D $J ;$B) (findgoldhigh $E $B $C $L) (move_piece $C $D $B $A) (or (not (take_dest $L s $A)) (or (nth1 4 $C p) ;(guimessage check g s))) (kingnotincheck $A))
;(= (playdefense $A $B $C $D) (buildgold $B $E) (checkgold $E $B $F) (piece $G) (member #( :: ($H $I g $G) ) ;$F) (= $C  
;  #( :: 
;    ($H $I g $G) )) (hpiece $J) (xy_box $K #( :: (s $J) ) $B) (positiontotake $K $C $D $B) (move_piece $C $D ;$B $A) (not (take_dest $D s $A)) (kingnotincheck $A))
;(= (playdefense $A $B $C $D) (buildgold $B $E) (checkgold $E $B $F) (lookforempty $B $G) (set-det) (piece $H) ;(member #( :: ($I $J g $H) ) $F) (= $C  
;  #( :: 
;    ($I $J g $H) )) (member #( :: ($K $L) ) $G) (= $D  
;  #( :: 
;    ($K $L) )) (clear_route $C $D $B) (move_piece $C $D $B $A) (not (take_dest $D s $A)) (kingnotincheck $A))
;
;
;(= (movetoposition $A $B $C $D) (returnrandominteger $E 2) (set-det) (== $E 1) (buildgold $B $F) ;(checkeachgold $F $A $B $C $D))
;(= (movetoposition $Newboard $Listofboxes $Goldbox $Destbox) (buildgold $Listofboxes $Currentgoldpieces) ;(checkeachgold $Currentgoldpieces $Newboard $Listofboxes $Goldbox $Destbox))
;
;
;(= (checkeachgold Nil $A $B $C $D) (set-det) (fail))
;(= (checkeachgold #(Cons $A $B) $C $D $A $E) (piece $F) (xy_box $G #( :: (s $F) ) $D) (positiontotake $G $A $E ;$D) (move_piece $A $E $D $C) (not (take_dest $E s $C)) (kingnotincheck $C))
;(= (checkeachgold #(Cons $A $B) $C $D $E $F)  
;  (checkeachgold $B $C $D $E $F))

;  (random_move_empty_sq)
;  Make a random legal move to an empty square.
;
;  Outputs a list of ( (starting piece) (destination)) if successful or Empty
;
(= (random_move_empty_sq)
  (let*
    (
    ; Build a list of all gold pieces on the board.
    ;($gold_pieces (collapse (buildgold)))
    ; Identify gold pieces under threat.
    ($debug (println! "start"))
    ;($gold_under_threat (collapse (checkgold)))
    ;($debug2 (println! "end"))
    )
    ( (1 7 g p) (1 6))
  )
)


(= (decide_greedy_move) 
    ; make sure each move search call can either 
    ;     1) succeed and return the move, or 
    ;     2) fail and return Empty
    (let $random_move (random_move_empty_sq)
      (if (not (==  $random_move Empty)) 
        $random_move 

    ; next move searches   
    (println! "failed!") )))

    ;(let $move (random_move_empty_sq) $move))

;(= (random_move_empty_sq $A $B $C $D)  
;  (or 
;    (, 
;      (buildgold $B $E) 
;      (checkgold $E $B $F) 
;      (lookforempty $B $G) 
;      (buildrandomgold $B $H) 
;      (sort $H $I) 
;      (findgoldmove $I $G $B $C $D) 
;      (move_piece $C $D $B $A) 
;      (kingnotincheck $A) 
;      (not (take_dest $D s $A)) 
;      (buildgold $A $J) 
;      (checkgold $J $A $K) 
;      (length $F $L) 
;      (delete $K 
;        #( :: 
;          ($M $N g p) ) $O) 
;      (length $O $P) 
;      (=< $P $L)) 
;    (guimessage check g s)))
;
;
;(= (takehighestopen $A $B $C $D) (buildgold $B $E) (checkgold $E $B $F) (set-det) (piece $G) (xy_box $H #( :: ;(s $G) ) $B) (findgoldhigh $E $B $C $H) (return_entire_box $H $D $B) (move_piece $C $D $B $A) (not (take_dest ;$H s $A)) (kingnotincheck $A))
;
;
;(= (takehighestopenpawn $A $B $C $D) (returnrandominteger $E 3) (set-det) (or (== $E 1) (== $E 2)) (buildgold ;$B $F) (checkgold $F $B $G) (set-det) (piece $H) (\= $H p) (xy_box $I #( :: (s $H) ) $B) (findgoldhigh $F $B #;( :: ($J $K g p) ) $I) (= $C  
;  #( :: 
;    ($J $K g p) )) (return_entire_box $I $D $B) (move_piece $C $D $B $A) (kingnotincheck $A))
;
; (checkgold)
;
(= (checkgold)  
   (match &self (square $x $y g $rank)
        (if (== (take_dest ($x $y) s) True) ($x $y) (empty))))

;(= (checkgold #(Cons $A $B) $C #(Cons $A $D)) (take_dest $A s $C) (checkgold $B $C $D) (set-det))
;(= (checkgold #(Cons $A $B) $C $D) (not (take_dest $A s $C)) (checkgold $B $C $D) (set-det))
;
;
;#( = #(lookforempty () ()) True )
;(= (lookforempty #(Cons $A $B) #(Cons $A $C)) (len $A 2) (lookforempty $B $C))
;(= (lookforempty #(Cons $A $B) $C)  
;  (lookforempty $B $C))
;
;
;#( = #(buildrandomgold () ()) True )
;(= (buildrandomgold #(Cons $A $B) #(Cons $C $D)) (len $A 4) (nth1 3 $A g) (returnrandominteger $E 99) (is $F ;$E) (concat_lists #( :: (#( :: ($F) ) #( :: ($A) )) ) $C) (buildrandomgold $B $D))
;(= (buildrandomgold #(Cons $A $B) $C)  
;  (buildrandomgold $B $C))
;
;

; (buildgold)
;
; Input:  None
; Output: all gold (AI) pieces on board
(= (buildgold)
  (match &self (square $x $y g $p) 
      ($x $y g $p)))

;
;(= (findgoldmove #(Cons $A $B) $C $D $E $F) (= #( :: ($G $E) )  $A) (returnrandominteger $H 8) (is $I $H) ;(returnrandominteger $J 8) (is $K $J) (set-det) (findgolddest $E $D $C $F $I $K))
;
;
;(= (findgolddest $A $B $C $D $E $F) (= $D  
;  #( :: 
;    ($E $F) )) (member $D $C) (clear_route $A $D $B))
;
;
;#( = #(findgoldhigh () $A $B $C) (empty) )
;(= (findgoldhigh #(Cons $A $B) $C $A $D)  
;  (clear_route $A $D $C))
;(= (findgoldhigh #(Cons $A $B) $C $D $E)  
;  (findgoldhigh $B $C $D $E))
;

;(= (LoadPiecesAtomspace $CanAttack)
;   (if (> (size-atom $CanAttack) 0)
;       create atom
;       (LoadPiecesAtomspace (cdr-atom $CanAttack))
;)
;
(= (take_dest_recursive $Square $OpponentColor $Board) 
  (let* 
    (
    ;  Identify opponent pieces that can attack.
    ($CanAttack (takingboxes_recursive $OpponentColor $Board))
    ; Check if any route of pieces that can attack leads to the specified square.
    ($OpenRouteToSquare (list_clear_route $Board $Square $CanAttack))
    ;($OpenRouteToSquare () )
    )
    ; if the list is populated (size exceeds 0), that means the piece can be taken.
    (if (not (== $OpenRouteToSquare () )) True False)))
    

(= (take_dest $Square $OpponentColor)
    (let*
        (
        ;  Identify opponent pieces that can attack.
        ($CanAttack (collapse (takingboxes $OpponentColor)))
        ; Check if any route of pieces that can attack leads to the specified square.
        ($OpenRouteToSquare (list_clear_route $Square $CanAttack))
        )
        ; if the list is populated (size exceeds 0), that means the piece can be taken.
        (if (not (== $OpenRouteToSquare () )) True False)))
    
(= (return_entire_box $Coordinates)
      (let*  
         (($x (nth 1 $Coordinates) )
          ($y (nth 2 $Coordinates) )
          ($z (match &self (square $a $b) 
            (if 
              (and (== $a $x) (== $b $y)) 
              ($a $b))))
         )
         (if (== (size-atom $z) 2) $z)
      )) ;)
(= (return_entire_box $Coordinates)
      (let*  
         (($x (nth 1 $Coordinates) )
          ($y (nth 2 $Coordinates) )
          ($z (match &self (square $a $b $c $d) 
            (if 
              (and (== $a $x) (== $b $y)) 
              ($a $b $c $d))))
         )
         (if (== (size-atom $z) 4) $z)
      )) ;)
;
;   (return_entire_box_sequential)
;   Retrieve the full details of a square on the board based on its coordinates
;   using a sequential search.
;   
;   Input:
;   $Coordinates The coordinates of the desired square.
;   $Board The current board configuration.
;
;   Returns:
;   $next-square The full contents of square.
(= (return_entire_box_sequential $Coordinates $Board) 
  (if (== (size-atom $Coordinates) 4) 
   ; if you send this the full box just output the input, no need for further evaluation.
    $Coordinates
   ;else
    (let*
    ;examine the next square on the board
    (($next-square (car-atom $Board)  )
    ($X (nth 1 $next-square) )
    ($Y (nth 2 $next-square) )
    ;($G (concat_lists (($X) ($Y))) )) 
    )
    ; then execute
    (if (== ($X $Y) $Coordinates) 
      $next-square
      (return_entire_box_sequential $Coordinates (cdr-atom $Board))))))

;
; (xy_box)
;     1. Match the board for a square containing the specified piece.
;     2. Returns the x/y coordinates of the square(s) if a match is found.
;
(= (xy_box ($PieceColor $PieceRank))
       (match &self (square $x $y $PieceColor $PieceRank) ($x $y))) 

;
; (xy_box_recursive)
;     1. Searches the board for a square containing the specified piece.
;     2. Returns the x/y coordinates of the square if a match is found.
;
; NOTE: This version of xy_box is only designed to return 1 piece deterministically 
;       given 1 piece of specified type exists.
;       For multiple pieces and backtracking action devise another means.
(= (xy_box_recursive ($PieceColor $PieceRank) $Board)
  (let*
    (
    ; assign variables for the next piece
    ;($debug ((py-atom print) $PieceColor))
    ($next_square (car-atom $Board)  )
    ($next_x (nth 1 $next_square) )
    ($next_y (nth 2 $next_square) )
    )
    ; then examine next square
    (if (== (size-atom $next_square) 2) 
      ; empty square, skip
      (xy_box_recursive ($PieceColor $PieceRank) (cdr-atom $Board))
      ; else check this piece
      (let* (
            ($next_color (nth 3 $next_square) )
            ($next_rank  (nth 4 $next_square) )
            )
            (if
              (and (== $next_color $PieceColor)
                   (== $next_rank $PieceRank))
                ; return x and y of the found piece
                ($next_x $next_y)
                ; else keep checking the board
                (xy_box_recursive ($PieceColor $PieceRank) (cdr-atom $Board)))))))
  
;
;
;(= (samecolor $A $B) (nth1 3 $A $C) (nth1 3 $B $D) (set-det) (== $C $D))
;
;


(: (clear_route) (-> Expression Expression  Bool))
; Return True if there is a clear route for piece else False. The first parm is starting square, 2nd parm destination, returns BOOL.
; Note: prior to calling clear_route verify that the piece is moving to a different square and
; if the square is occupied it is occupied by the opponent's color. These tests are not duplicated by clear_route!

; TEST ONLY
;(= (clear_route ($X1 $Y1 $COLOR p) $destination) True)
;(= (clear_route ($X1 $Y1 $COLOR r) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR n) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR b) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR q) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR k) $destination) False)

; King: Moves one square in any direction.
(= (clear_route ($X1 $Y1 $Color k) $destination)
      (let*  (
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )          
             )
      (if (and 
         (xor      
          (== $X2 $X1)
          (xor 
          (== $X2 (- $X1 1))
          (== $X2 (+ $X1 1))
          )) 
         (xor
          (== $Y2 $Y1)
          (xor 
          (== $Y2 (- $Y1 1))
          (== $Y2 (+ $Y1 1))
          )) 
          )
        True
        False)))

; Queen:  Either rook or bishop move works.
(= (clear_route ($X1 $Y1 $Color q) $destination)
      (xor (clear_route ($X1 $Y1 $Color b) $destination)
           (clear_route ($X1 $Y1 $Color r) $destination))
           True
           False)

; Knight: Moves in an "L" shape (2 steps in one direction, 1 step perpendicular).
(= (clear_route ($X1 $Y1 $Color n) $destination)
      (let*  (
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )          
             )
      (if (xor 
          ; vertical 'L'
         (and
          (xor 
          (== $X2 (- $X1 1))
          (== $X2 (+ $X1 1))
          )
          (xor
          (== $Y2 (- $Y1 2))
          (== $Y2 (+ $Y1 2))
          )
         ) 
          ; sideways 'L'
         (and
          (xor
          (== $X2 (- $X1 2))
          (== $X2 (+ $X1 2))
          )
          (xor
          (== $Y2 (- $Y1 1))
          (== $Y2 (+ $Y1 1))
          )
          ))
        True
        False)))


; Move gold AI pawn 
(= (clear_route ($X1 $Y1 g p) $destination)
  (let*
    ( 
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )
      ($entire (if (== (size-atom $destination) 2) (return_entire_box ($X2 $Y2)) $destination))
      ($passing_1 ($X2 6) )
      ($entire_box_down_one  (return_entire_box $passing_1))
      )
    
    (if (xor 
        
          ; clear if moving to empty box 1 row down...
          (and  
          (== $X1 $X2)
          (and 
          (== $Y2 (- $Y1  1))
          ; check empty box to move into
          (== (size-atom $entire) 2)
          ))

          (xor
            ; clear if moving to empty box 2 rows down from staring position
            (and 
            ; moving down in same column  
            (== $X1 $X2)
            (and 
            ; moving from row 7
            (== $Y1 7)
            (and 
            ; definitely moving down 2 
            (== $Y2 5)
            (and 
            ; destination box down 2 is clear
            (== (size-atom $entire) 2)       
            ; first box down 1 is clear
            (== (size-atom $entire_box_down_one) 2))
            )))
    
            (xor
              ; clear if taking piece one box down and to the right
              (and  
              (== $X2 (+ $X1 1))
              (and 
              (== $Y2 (- $Y1 1))
              ; make sure there is a piece to take, must have 4 symbols in the target box
              (== (size-atom $destination) 4)
              )) 
      
              ; clear if taking piece one box down and to the left
              (and  
              (== $X2 (- $X1 1))
              (and 
              (== $Y2 (- $Y1 1))
              ; make sure there is a piece to take, must have 4 symbols in the target box
              (== (size-atom $destination) 4)
              ))
            )
          )
        )
        True
        False)))

; Move silver human pawn 
(= (clear_route ($X1 $Y1 s p) $destination)
  (let*
    ( 
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )      
      ($entire (if (== (size-atom $destination) 2) (return_entire_box ($X2 $Y2)) $destination))
      ($passing_1 ($X1 3) )
      ($entire_box_up_one  (return_entire_box $passing_1))
      )
    
    (if (xor 
         (xor      
          ; clear if taking piece one box up and to the right
          (and  
          (== $X2 (+ $X1 1))
          (and 
          (== $Y2 (+ $Y1 1))
          ; make sure there is a piece to take, must have 4 symbols in the target box
          (== (size-atom $destination) 4)
          )) 
  
          ; clear if taking piece one box up and to the left
          (and  
          (== $X2 (- $X1 1))
          (and 
          (== $Y2 (+ $Y1 1))
          ; make sure there is a piece to take, must have 4 symbols in the target box
          (== (size-atom $destination) 4)
          ))
         ) 
         (xor
          ; clear if moving to empty box 1 row up
          (and  
          (== $X1 $X2)
          (and 
          (== $Y2 (+ $Y1  1))
          ; check empty box to move in to
          (== (size-atom $entire) 2)
          ))

          ; clear if moving to empty box 2 rows up from staring position
          (and 
          ; moving up in same column  
          (== $X1 $X2)
          (and 
          ; moving from row 2
          (== $Y1 2)
          (and 
          ; definitely moving up 2 
          (== $Y2 4)
          (and 
          ; destination box up 2 is clear
          (== (size-atom $entire) 2)       
          ; first box up 1 is clear
          (== (size-atom $entire_box_up_one) 2))
          )))
         )
        )
        True
        False)))

; Rook: Moves horizontally or vertically.
(= (clear_route ($X1 $Y1 $Color r) $destination)
      (let*  (   
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )    
      ($RouteClearBool 
           ; if up
           (if (and (== $X2 $X1) (> $Y2  $Y1))
               (clearcheckup $X1 (+ 1 $Y1) $Y2) 
           ;elif down
           (if (and (== $X2 $X1) (< $Y2  $Y1))
               (clearcheckdown $X1 (- $Y1 1) $Y2)
           ;elif right
           (if (and (> $X2 $X1) (== $Y2  $Y1))
                (clearcheckright (+ $X1 1) $X2 $Y1)
           ;elif left
           (if (and (< $X2 $X1) (== $Y2  $Y1))
               (clearcheckleft (- $X1 1) $X2 $Y1) 
            ; otherwise rook bad move
               False)))))
      )
      (if (and 
          (== $RouteClearBool True)
          ; straight vertical 
          (xor 
            (and
              (== $X2 $X1)
              (xor
              (> $Y2 $Y1)
              (< $Y2 $Y1)))
          ; straight sideways 
            (and
              (== $Y2 $Y1)
              (xor
              (> $X2 $X1)
              (< $X2 $X1)))))
        True
        False
        )))

(: clearcheckup (-> Atom Atom Atom Bool))
(: clearcheckdown (-> Atom Atom Atom Bool))
(: clearcheckleft (-> Atom Atom Atom Bool))
(: clearcheckright (-> Atom Atom Atom Bool))


(= (clearcheckup $X $Y1 $Y2) 
   (let* (
        ($next_box (return_entire_box ($X $Y1)))
          )
        (if (== $Y1 $Y2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckup $X (+ $Y1 1) $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckdown $X $Y1 $Y2) 
   (let* (
        ($next_box (return_entire_box ($X $Y1)))
          )
        (if (== $Y1 $Y2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckdown $X (- $Y1 1) $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckright $X1 $X2 $Y) 
   (let* (
        ($next_box (return_entire_box ($X1 $Y)))
          )
        (if (== $X1 $X2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckright (+ $X1 1) $X2 $Y)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckleft $X1 $X2 $Y) 
   (let* (
        ($next_box (return_entire_box ($X1 $Y)))
          )
        (if (== $X1 $X2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckleft (- $X1 1) $X2 $Y)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

; Bishop: Moves diagonally
(= (clear_route ($X1 $Y1 $Color b) $destination)
      (let*  (
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )    
      ($RouteClearBool 
           ; if northeast
           (if (and (> $X2 $X1) (> $Y2  $Y1))
               (clearcheckNE (+ 1 $X1) (+ 1 $Y1) $X2 $Y2) 
           ;elif southeast
           (if (and (> $X2 $X1) (< $Y2  $Y1))
               (clearcheckSE (+ 1 $X1) (- $Y1 1) $X2 $Y2)
           ;elif northwest
           (if (and (< $X2 $X1) (> $Y2  $Y1))
               (clearcheckNW (- $X1 1) (+ $Y1 1) $X2 $Y2)
           ;elif southwest
           (if (and (< $X2 $X1) (< $Y2  $Y1))
               (clearcheckSW (- $X1 1) (- $Y1 1) $X2 $Y2)
            ; otherwise bishop bad move
               False))))
      ))
      (if  
          (== $RouteClearBool True)
        True
        False
        )))

(: clearcheckNE (-> Atom Atom Atom Atom Bool))
(: clearcheckSE (-> Atom Atom Atom Atom Bool))
(: clearcheckNW (-> Atom Atom Atom Atom Bool))
(: clearcheckSW (-> Atom Atom Atom Atom Bool))


(= (clearcheckNE $X1 $Y1 $X2 $Y2) 
   (let* (
        ($next_box (return_entire_box ($X1 $Y1)))
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckNE (+ 1 $X1) (+ 1 $Y1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckSE $X1 $Y1 $X2 $Y2) 
   (let* (
        ($next_box (return_entire_box ($X1 $Y1)))
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckSE (+ 1 $X1) (- $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckNW $X1 $Y1 $X2 $Y2) 
   (let* (
        ($next_box (return_entire_box ($X1 $Y1)))
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckNW (- $X1 1) (+ $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckSW $X1 $Y1 $X2 $Y2) 
   (let* (
        ($next_box (return_entire_box ($X1 $Y1)))
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckSW (- $X1 1) (- $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))


; THIS VERSION IS BEST AND ONLY MAKES ONE PASS BUT BUG IN INTERPRETER! craps out around 64th square!

; replace_square_recursive
;   Function:  Move one piece to new location  on board, return the new board. 
;   input: X1, Y1, color, rank, X2, Y2, board
;   output:  new board which overwrites X2 and Y2 with new piece and removes color  and rank from X1 and Y1
(: (replace_square_recursiveWORKS) (-> Atom Atom Atom Atom Atom Atom Expression Expression))
(= (replace_square_recursiveWORKS $X1 $Y1 $color $rank $X2 $Y2 $current_board) 
   (if (== (size-atom $current_board) 0)
        ()
        (let* (
          ($next-sq (car-atom $current_board))
          ($X (nth 1 $next-sq))
          ($Y (nth 2 $next-sq))
              )
        (if (and (== $X $X1) (== $Y $Y1))
          ; if we are on the square being moved FROM, then remove piece color and rank since it will disappear
          (let $from_square ($X1 $Y1)   ; <-- note no color and rank
               (let $rest (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 (cdr-atom $current_board)) 
                          (cons-atom $from_square $rest)))
        (if (and (== $X $X2) (== $Y $Y2))
          ; elif on square being moved INTO, use the moving piece's color and rank.
          (let $into_square ($X2 $Y2 $color $rank)  ; <-- color and rank in destination  
               (let $rest (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 (cdr-atom $current_board)) 
                          (cons-atom $into_square $rest)))
          ; otherwise just keep chugging through the board...
        (let $rest (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 (cdr-atom $current_board)) 
                          (cons-atom $next-sq $rest)))))))  ; <-- here we just copy the existing square

; replace_piece_recursive
;   Function:  Move a piece in new location on board, return the new board. 
;   input: X, Y, color, rank, board
;   output:  new board which overwrites X and Y with new piece
(= (replace_piece_recursive $X $Y $color $rank $current_board) 
   (if (== (size-atom $current_board) 0)
        ()
        (let* (
          ($next-sq (car-atom $current_board))
          ($X2 (nth 1 $next-sq))
          ($Y2 (nth 2 $next-sq))
              )
        (if (and (== $X $X2) (== $Y $Y2))
          ; if desired square, move in the color and rank, function ends returning rest of board too.
          (let $rest (replace_piece_recursive $X $Y $color $rank (cdr-atom $current_board)) 
            (cons-atom ($X $Y $color $rank) $rest))
          ; else keep looking for square
          (let $rest (replace_piece_recursive $X $Y $color $rank (cdr-atom $current_board)) 
            (cons-atom $next-sq $rest))
        ))))

; remove_piece_recursive
;   Function:  Remove piece from square, return new board
;   input: X, Y, board
;   output:  new board
(= (remove_piece_recursive $X $Y $current_board) 
   (if (== (size-atom $current_board) 0)
        ()
        (let* (
          ($next-sq (car-atom $current_board))
          ($X2 (nth 1 $next-sq))
          ($Y2 (nth 2 $next-sq))
              )
        (if (and (== $X $X2) (== $Y $Y2))
          ; if desired square, replace with no color/rank
          (let $rest (remove_piece_recursive $X $Y (cdr-atom $current_board)) 
            (cons-atom ($X2 $Y2) $rest))
          ; else keep looking for square
          (let $rest (remove_piece_recursive $X $Y (cdr-atom $current_board)) 
            (cons-atom $next-sq $rest))
        ))))

;
; (move_piece_on_board)
;   Input   starting square, target square, current board
;   Output: new board
(= (move_piece_on_board $starting_square $target_square $current_board) 
  (let* (
    ; move to new square
    ($X2  (nth 1 $target_square))
    ($Y2  (nth 2 $target_square))
    ($color (nth 3 $starting_square))
    ($rank  (nth 4 $starting_square))
    ($new_board1 (replace_piece_recursive $X2 $Y2 $color $rank $current_board))
    ; delete piece from old square
    ($X1  (nth 1 $starting_square))
    ($Y1  (nth 2 $starting_square))
    ($new_board2 (remove_piece_recursive $X1 $Y1 $new_board1))
    ($debug0 (println! $new_board2))
    ;($debug1 (println! $new_board1))
    ;($debug2 (println! $new_board2))
        ) 
    $new_board2))

   
; (reset_square)
; Input:  X and Y
; Output: Removes atom for this square occupied or not
(= (reset_square $X $Y)
    (let $remove_box (return_entire_box ($X $Y)) 
       (if (== (size-atom $remove_box) 4)
          (remove-atom &self (square $X $Y $anycolor $anyrank))  ; piece captured!
          (remove-atom &self (square $X $Y)))))
    
;
; move_piece
;   Input   starting square, target square, current board
;   Output: new board
(= (move_piece $starting_square $target_square) 
  (let* (
    ; delete piece from starting square
    ($X1  (nth 1 $starting_square))
    ($Y1  (nth 2 $starting_square))
    ($color (nth 3 $starting_square))
    ($rank  (nth 4 $starting_square))
    ($starting_square_complete_atom (square $X1 $Y1 $color $rank))
    ($_1 (remove-atom &self $starting_square_complete_atom))
    ; create atom for empty starting square
    ($old_square_complete (square $X1 $Y1))
    ($_2 (add-atom &self $old_square_complete))
    ; delete atom for whatever occupies the new square, if anything
    ($X2  (nth 1 $target_square))
    ($Y2  (nth 2 $target_square))
    ($_3 (reset_square $X2 $Y2))
    ; add piece at new square
    ($new_square_complete (square $X2 $Y2 $color $rank))
    ($_4 (add-atom &self $new_square_complete))    
        ) 
    True))
;
;
;(= (printmove $A $B $C) (nth1 1 $A $D) (nth1 2 $A $E) (nth1 3 $A $F) (nth1 4 $A $G) (nth1 1 $B $H) (nth1 2 $B ;$I) (or (, (len $B 4) (return_entire_box #( :: ($H $I) ) $J $C) (nth1 4 $J $K)) (= $K  nil)) (or (, (== $F g) ;(write 'DBD moves from:') (write $D) (write and2) (write $E) (write ' to: ') (write $H) (write and2) (write ;$I)) (, (== $F s) (write 'YOU move from:') (write $D) (write and2) (write $E) (write ' to: ') (write $H) ;(write and2) (write $I))) #(add-atom &self #(guimessage move $A $B $K)) (or (, (\= $K nil) (nl) (write 'Piece ;captured!! -> ') (write $K) (nl)) nl) (write 'Type c. for commands you can use.'))
;
;
;(= (examine_king $A $B $C) (cantakepiece $A $B k $C $D) (\= $D Nil) (threatOK1 $A $B $C $D) (threatOK2 $A $B ;$C) (threatOK3 $A $B $C) (write Checkmate!) (nl) #(add-atom &self #(guimessage checkmate $B $C)))
;(= (examine_king $A $B $C) (cantakepiece $A $B k $C $D) (\= $D Nil) (write Check!) (nl) #(add-atom &self #;(guimessage check $B $C)))
;#( = #(examine_king $A $B $C) True )
;
;
;(= (threatOK1 $A $B $C $D) (seekopponents $A $B $D $E) (== $E Nil) (set-det))
;(= (threatOK1 $A $B $C $D) (seekopponents $A $B $D $E) (checkthreat $E $A) (set-det))
;
;
;(= (checkthreat Nil $A)  
;  (set-det))
;(= (checkthreat #(Cons $A $B) $C) (checkeachthreat $A $C) (set-det) (checkthreat $B $C) (set-det))
;
;
;#( = #(checkeachthreat () $A) True )
;(= (checkeachthreat #(Cons $A #(Cons $B $C)) $D) (nth1 3 $A $E) (nth1 3 $B $F) (move_piece $A $B $D $G) ;(xy_box $H #( :: ($E k) ) $G) (set-det) (checkking $H $F $G) (checkeachthreat $C $D))
;
;
;(= (checkking $A $B $C)  
;  (take_dest $A $B $C))
;
;
;(= (threatOK2 $A $B $C) (lookforempty $A $D) (xy_box $E #( :: ($B k) ) $A) (return_entire_box $E $F $A) ;(set-det) (not (king_can_move $F $C $D $A)))
;
;
;(= (king_can_move $A $B #(Cons $C $D) $E) (clear_route $A $C $E) (move_piece $A $C $E $F) (not (take_dest $C ;$B $F)))
;(= (king_can_move $A $B #(Cons $C $D) $E) (not (clear_route $A $C $E)) (fail))
;(= (king_can_move $A $B #(Cons $C $D) $E) (clear_route $A $C $E) (move_piece $A $C $E $F) (take_dest $C $B $F) ;(fail))
;(= (king_can_move $A $B #(Cons $C $D) $E)  
;  (king_can_move $A $B $D $E))
;(= (king_can_move $A $B Nil $C) (set-det) (fail))
;
;
;(= (threatOK3 $A $B $C) (set-det) (not (opponentblock $A $B $C)))
;
;
;(= (opponentblock $A $B $C) (xy_box $D #( :: ($B k) ) $A) (rpiece $E) (\== $E k) (xy_box $F #( :: ($B $E) ) ;$A) (return_entire_box $F $G $A) (cord $H) (cord $I) (return_entire_box #( :: ($H $I) ) $J $A) (or (not ;(samecolor $G $J)) (len $J 2)) (clear_route $G $J $A) (move_piece $G $J $A $K) (not (take_dest $D $C $K)) ;(set-det))
;
;
;(= (seekopponents $A $B $C $D) (buildopponent $A $B $E) (set-det) (takingpieces $E $C $A $F) (set-det) (delete ;$F Nil $D) (set-det))
;
;
;#( = #(takingpieces () $A $B ()) True )
;(= (takingpieces #(Cons $A $B) $C $D #(Cons $E $F)) (checkopponent $A $C $D $E) (set-det) (takingpieces $B $C ;$D $F))
;(= (takingpieces #(Cons $A $B) $C $D $E)  
;  (takingpieces $B $C $D $E))
;
;
;#( = #(checkopponent $A () $B ()) True )
;(= (checkopponent $A #(Cons $B $C) $D #(Cons $A #(Cons $B $E))) (clear_route $A $B $D) (checkopponent $A $C $D ;$E))
;(= (checkopponent $A #(Cons $B $C) $D $E)  
;  (checkopponent $A $C $D $E))
;
;
;(= (takeyourpiece #(Cons Nil $A) $B)  
;  (takeyourpiece $A $C $B))
;(= (takeyourpiece #(Cons Nil $A) $B)  
;  (takeyourpiece $A $C $B))
;
;
;#( = #(buildopponent () $A ()) True )
;(= (buildopponent #(Cons $A $B) $C #(Cons $A $D)) (len $A 4) (nth1 3 $A $C) (buildopponent $B $C $D))
;(= (buildopponent #(Cons $A $B) $C $D)  
;  (buildopponent $B $C $D))
;
;
;#( = #(seekopponent () $A $B $C ()) True )
;(= (seekopponent #(Cons $A $B) $C $D $E $A) (len $A 4) (nth1 3 $A $C) (takeyourpiece $D $A $E))
;(= (seekopponent #(Cons $A $B) $C $D $E $F)  
;  (seekopponent $B $C $D $E $F))
;
;
;#( = #(takeyourpiece () $A $B) (empty) )
;(= (takeyourpiece #(Cons $A $B) $C $D)  
;  (clear_route $C $A $D))
;
;
;(= (cantakepiece $A $B $C $D $E) (takingboxes $D $A $F) (set-det) (xy_box $G #( :: ($B $C) ) $A) ;(list_clear_route $A $G $F $E))
;

; (takingboxes)
;
; Input:  pieces color to look for, board
; Output: a piece of a color
(= (takingboxes $OpponentColor)
  (match &self (square $x $y $OpponentColor $p) 
      ($x $y $OpponentColor $p)))

; (takingboxes_recursive)
;
; Input:  pieces color to look for, board
; Output: a list of all pieces belonging to a given color w/ square coordinates.
(= (takingboxes_recursive $OpponentColor $Board)
   (if (== (size-atom $Board) 0)
        ; end of the line, return empty list
        ()
        ; examine next square for opponent piece
        (let $next-sq (car-atom $Board)
             ; if size = 2, square is empty
             (if (== (size-atom $next-sq) 2) 
                  (let $rest (takingboxes_recursive $OpponentColor (cdr-atom $Board)) $rest)
                  ;else check color
                  (let $NextColor (nth 3 $next-sq)
                      (if (== $NextColor $OpponentColor)
                        ; if same, add this square to the list to return
                        (let $rest (takingboxes_recursive $OpponentColor (cdr-atom $Board)) (cons-atom $next-sq $rest))
                        (let $rest (takingboxes_recursive $OpponentColor (cdr-atom $Board)) $rest)))))))

;
; (list_clear_route)
;
; Input:  $Square that might be compromised, all pieces available to attack square in question, $CanAttack
; Output: list of pieces that can take the piece in the square in question
(= (list_clear_route $Square $CanAttack)
   (if (== (size-atom $CanAttack) 0)
       ; end of the line
       ()
       ; check if next piece in $CanAttack can capture $Square, if so add to return list.
       (let* (
          ($next-sq (car-atom $CanAttack))
          ($AttackBool (clear_route $next-sq $Square))
             )
          (if (== $AttackBool True) 
              (let $rest (list_clear_route $Square (cdr-atom $CanAttack)) (cons-atom $next-sq $rest))
              (let $rest (list_clear_route $Square (cdr-atom $CanAttack)) $rest)
          )
       )
   ) 
)

;
;(= (positiontotake #(Cons $A #(Cons $B $C)) $D $E $F) (cord $G) (cord $H) (return_entire_box #( :: ($G $H) ) ;$E $F) (or (not (samecolor $D $E)) (len $E 2)) (clear_route $D $E $F) (nth1 3 $D $I) (nth1 4 $D $J) (= $K  
;  #( :: 
;    ($G $H $I $J) )) (clear_route $K #( :: ($A $B) ) $F))
;
;
;#( = #(concat_lists () ()) True )

;*******************************************************
; Basic commands.  The game is executed using the following commands.
;*******************************************************

; This command must be invoked at startup with "!(chess)"
(= (chess) 
  (match &self (game-state $msg)
    ; if first invocation, just create board
    (if (==  initializing   $msg) ; then  
      (progn   
        ; remove the 'initializing' message
        (remove-atom &self (game-state $msg)) 
        ; create the board for the first time
        (add-atom &self 
          (board-state ((1 8 g r) (2 8 g n) (3 8 g b) (4 8 g q) (5 8 g k) (6 8 g b) (7 8 g n) (8 8 g r) 
                        (1 7 g p) (2 7 g p) (3 7 g p) (4 7 g p) (5 7 g p) (6 7 g p) (7 7 g p) (8 7 g p) 
                        (1 6) (2 6) (3 6) (4 6) (5 6) (6 6) (7 6) (8 6) 
                        (1 5) (2 5) (3 5) (4 5) (5 5) (6 5) (7 5) (8 5) 
                        (1 4) (2 4) (3 4) (4 4) (5 4) (6 4) (7 4) (8 4) 
                        (1 3) (2 3) (3 3) (4 3) (5 3) (6 3) (7 3) (8 3) 
                        (1 2 s p) (2 2 s p) (3 2 s p) (4 2 s p) (5 2 s p) (6 2 s p) (7 2 s p) (8 2 s p) 
                        (1 1 s r) (2 1 s n) (3 1 s b) (4 1 s q) (5 1 s k) (6 1 s b) (7 1 s n) (8 1 s r)))
        )
        ; indicate game has passed the initializing state
        (add-atom &self (game-state started))
        ; display welcome messages and board
        (welcome))
    ; elif there has already been one game played
    (if (==  restarted   $msg) ; then
      (progn 
        ; remove the old chess board
        (match &self (board-state $old_board) (remove-atom &self (board-state $old_board)))
        ; re-create a new board 
        (add-atom &self           
          (board-state ((1 8 g r) (2 8 g n) (3 8 g b) (4 8 g q) (5 8 g k) (6 8 g b) (7 8 g n) (8 8 g r) 
                        (1 7 g p) (2 7 g p) (3 7 g p) (4 7 g p) (5 7 g p) (6 7 g p) (7 7 g p) (8 7 g p) 
                        (1 6) (2 6) (3 6) (4 6) (5 6) (6 6) (7 6) (8 6) 
                        (1 5) (2 5) (3 5) (4 5) (5 5) (6 5) (7 5) (8 5) 
                        (1 4) (2 4) (3 4) (4 4) (5 4) (6 4) (7 4) (8 4) 
                        (1 3) (2 3) (3 3) (4 3) (5 3) (6 3) (7 3) (8 3) 
                        (1 2 s p) (2 2 s p) (3 2 s p) (4 2 s p) (5 2 s p) (6 2 s p) (7 2 s p) (8 2 s p) 
                        (1 1 s r) (2 1 s n) (3 1 s b) (4 1 s q) (5 1 s k) (6 1 s b) (7 1 s n) (8 1 s r)))
        )
        ;
        ; display welcome messages and board
        (welcome))
    (; else if
        empty))))) 

; The move command moves the human player's piece.

;(= (m $A $B $C $D) (guimessage checkmate $E $F) (write 'Game over.') (nl) (set-det))

;(: M (-> command))
(= (M)  
  (progn 
  ; prompt for x/y location
  ; THIS NEEDS TO BE ALTERED PRIOR TO PRODUCTION FOR EDITS! Convert to "file and rank"
  ; add edit to make sure move within boundaries of 1 -> 8!
  ((py-atom print) "Enter coordinates.")
  (let $coordinates ((py-atom input)) ())
  (let $x1 (char_to_int ((py-dot $coordinates __getitem__) 0)) ())
  (let $y1 (char_to_int ((py-dot $coordinates __getitem__) 1)) ())
  (let $x2 (char_to_int ((py-dot $coordinates __getitem__) 2)) ())
  (let $y2 (char_to_int ((py-dot $coordinates __getitem__) 3)) ())
  (let $f (concat_lists ( ($x1) ($y1))) ()) ; Create the source coordinate list.
  (let $g (concat_lists ( ($x2) ($y2))) ()) ; Create the destination coordinate list.
  ; get the current board
  (match &self (board-state $starting_board) $starting_board)
  ; reset, then add each piece to atomspace to work with individually with form eq '(square 1 1 s r)'
  (reset-pieces $starting_board)
  ; check if source location and destination are not the same
  (if (not (== $f $g))                            
   ; check if destination is empty or destination has opponent's piece  
    (progn 
    ; Retrieve the source box details.
    (let $h (return_entire_box $f) ())
          ;does not return here!
    ; Retrieve the destination box details.
    (let $i (return_entire_box $g) ()) 
    ; Check there is a piece to move.
    (if (and (== (size-atom $h) 4)                                    
             (xor  
             ; OK to move to open square size 2...
             (== (size-atom $i) 2)     
             ; OK to take another piece, check color is not the same...
             (and (== (size-atom $i) 4) (not (== (nth 3 $h) (nth 3 $i))))))
      ; Validate the path for the piece.
      (if (== (clear_route $h $i) True)               
        ; Try moving piece provisionally
        (let* ( 
            ; Try the move and update the board state.   
            ($MoveBool  (move_piece $h $i))   
            ; locate human's king
            ($king_square (xy_box (s k)))    
            ; 
            ($full_king_sq (return_entire_box $king_square))  
            ; Ensure the player's king is not in check from gold (g) if moved.  
            ($KingCompromised (take_dest $full_king_sq g))   ; <-- dies here 
                ) 
            ; If move valid, proceed to create new board atom with move.
            (if (== $KingCompromised True)
                ((py-atom print) "Can't move there, your king would be in check.")
                ;else
                (progn 
                ; re-create a new board 
                ((py-atom print) "Moving piece...")
                (let $provisional_board (move_piece_on_board $h $i $starting_board)
                     (add-atom &self (board-state $provisional_board)))
                ; remove the old chess board
                (remove-atom &self (board-state $starting_board))
                ; display
                (D) 
                ; Look for CHECK or CHECKMATE
                )
            )
        )
        ((py-atom print) "Can't move piece there."))
      ; else (moving to location with same piece or other problem)
      ((py-atom print) "Invalid move.")
    )
    )
    ;else (trying to move to same location)
    ((py-atom print) "Can't move to same location.")) 
  ; since move complete, delete any individual square atoms which only live for a single move.
  (delete-pieces)
  )
)

; AI move piece command
(: G (-> command))
(= (G)  
  (match &self (game-state $msg)
  ; check for game over...
  (if (==  checkmate   $msg) ; then  
      ((py-atom print) "Game over. Please reset to play again (enter R).")
      ;else keep playing...
      (progn 
          ; get the current board
          (match &self (board-state $starting_board) $starting_board)
          ; reset, then add each piece to atomspace to work with individually with form eq '(square 1 1 s r)'
          (reset-pieces $starting_board)


          (let $move (decide_greedy_move) () )
          (let $start (nth 1 $move) ())
          (let $destination (nth 2 $move) ())

          (println! "Greedy Chess moving...")

          ; create atom for the updated complete chess board...
          (let $provisional_board (move_piece_on_board $start $destination $starting_board)
                (add-atom &self (board-state $provisional_board)))
          ; remove the old chess board
          (remove-atom &self (board-state $starting_board))
          ; since move complete, delete any individual square atoms which only live for a single move.
          (delete-pieces)
          (println! (collapse (match &self (square $x $y $s $p) (square $x $y $s $p))))
          (println! (collapse (match &self (square $x $y) (square $x $y))))
          ; display
          (D)
          ; check to see if human player's king is in check
          ;     "
      ))))

;(= (g) (board $A) (attemptcheckmate $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board ;$B)) (b $B) (write 'Checkmate!  Deep Blue Dummy Wins!') (nl) #(add-atom &self #(guimessage checkmate s g)) ;(printmove $C $D $A) (set-det))
;(= (g) (board $A) (playdefenseR $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board $B)) (b ;$B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (takehighestopen $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board $B)) ;(b $B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (movetoposition $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board $B)) ;(b $B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (takehighestopenpawn $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board ;$B)) (b $B) (examine_king $B s g) (printmove $C $D $A) (set-det))


;

; The reset command will start the game over.
(: R (-> command))
(= (R)  
  (progn 
  (reset-state)
  (add-atom &self (game-state restarted))
  (chess)))

; The "commands" command just lists the available commands.
(: C (-> command))
(= (C) 
  (progn
    ((py-atom print) " ") ((py-atom print) " ") ((py-atom print) " ") ((py-atom print) " ")
    ((py-atom print) (format-args "-------- C o m m a n d s -----------" ()))
    ((py-atom print) "- Your pieces are marked with an asterisk.")
    ((py-atom print) "- Please take note of the following simple commands:")
    ((py-atom print) "-------- C o m m a n d s -----------")
    ((py-atom print) "1 TO MOVE YOUR PIECE USE example   ->  M <ENTER> 1213 <ENTER>")
    ((py-atom print) "   Result:  YOUR pawn in 1,2 moved to location 1,3 based on standard cartesian x/y.")
    ((py-atom print) "2 Move MeTTa Greedy Chess          ->  G")
    ((py-atom print) "3 Reset                            ->  R")
    ((py-atom print) "4 Commands List                    ->  C")
    ((py-atom print) "5 Display Board                    ->  D")
    ((py-atom print) "6 Quit                             ->  Q")))
;

; The display command shows the present board.
(: D (-> command))
(= (D) (display_board (match &self (board-state $board) $board)))

;*******************************************************
;   M A I N   C O M M A N D   L O O P
;*******************************************************
(= (command-loop)
  (progn 
    ((py-atom print) "Please enter your command.")
    (let $command ((py-atom input))   ; get user command
        (case $command
          (
          ("M"  (M))                         ; Move human piece 
          ("G"  (G))                         ; AI move ("G" = gold)
          ("R"  (R))                         ; Reset = "R"
          ("C"  (C))                         ; List valid commands = "C"
          ("D"  (D))                         ; Display board "D"
          ("Q"  ((py-atom print) "Quitting MeTTa Greedy Chess."))
          ($_   ((py-atom print) "Invalid command, please try again or enter C for a list of commands."))
          )))
    (if (== $command "Q") 
      (empty)
      (command-loop))
      ))

    ;(let $command ((py-atom input))   ; get user command
    ; case statement for commands
    ;(if (== $command "M") 
    ;    (progn 
    ;      (M)                         ; Move human piece 
    ;      (command-loop))             ; Get next command, stay in loop.
    ;(if (== $command "G")
    ;    (progn 
    ;      (G)                         ; AI move
    ;      (command-loop))             ; Get next command, stay in loop.
    ;(if (== $command "R")
    ;    (progn 
    ;      (R)                         ; Reset = "R"
    ;      (command-loop))             ; Get next command, stay in loop.
    ;(if (== $command "C")
    ;    (progn 
    ;      (C)                         ; List valid commands = "C"
    ;      (command-loop))             ; Get next command, stay in loop.
    ;(if (== $command "D")
    ;    (progn 
    ;      (D)                         ; Display board "D"
    ;      (command-loop))             ; Get next command, stay in loop.
    ;(if (== $command "Q")             ; EXIT HERE
    ;      ((py-atom print) "Quitting MeTTa Greedy Chess.")
    ; otherwise
    ;    (progn 
    ;        ((py-atom print) "Invalid command, please try again or enter C for a list of commands.") 
    ;        (command-loop)))))))))))

;-----------;
(= (main_loop) 
  (progn
    (chess)           ; Start the chess game
    (command-loop))) ; Enter the recursive command-processing loop
!(main_loop)
;-----------;
