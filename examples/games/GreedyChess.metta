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
(: console-messages (-> Expression Atom))  ; The state is an expression type, stored as an atom.
(: board-state (-> Expression Atom))       

;*******************************************************
; General utility functions
;*******************************************************

; nth function, eg:  (nth 2 (a b c)), answer = b
(: nth (-> Int Expression Atom)) ; Retrieves the nth element from the board.
(= (nth $n $list) 
    (if (== $n 1)
        (car-atom $list)
        (nth (- $n 1) (cdr-atom $list)))) ; Recursion: move to the next element (cdr-atom) and decrease n.

; Input a list and symbol, then return True if found, else False. Eg:  (contains_symbol (a b c) b) returns True.
(: contains_symbol (-> Expression Atom Bool))
(= (contains_symbol $list $sym) 
    (if (== $list ())
        False
        (if (== (car-atom $list) $sym)
            True
            (contains_symbol (cdr-atom $list) $sym))))

; helper routines if desired
(: car-car (-> Expression Atom))
(= (car-car $list) (let $first_list (car-atom $list) (car-atom $first_list)))
(: car-cdr (-> Expression Atom))
(= (car-cdr $list) (let $first_list (car-atom $list) (cdr-atom $first_list)))

; Input a list of lists. Output the concatenation of the input flattened (only flattens at top level).
; For example:  
;   Input list of lists    =  ((1 2) (3 4) () (5)) 
;   Output list            =  (1 2 3 4 5)
;
(: concat_lists (-> list list))
(= (concat_lists $ListofLists)
  ; if list of lists is empty, return empty list
  (if (==  (size-atom $ListofLists) 0) 
      ()
  ; if the first list in the list of lists is empty, ie., ( ()....), skip and return the rest of the list of lists.
  (if (== (size-atom (car-atom $ListofLists)) 0)
      (concat_lists  (cdr-atom $ListofLists))
  ; otherwise recursively transform eg ( (1 2) (3) ) into --> ( 1 (2) (3))...  into finally --> ( 1 2 3).
  ; (cons-atom (first-symbol (concat_lists (cdr-atom (car-atom ($ListofLists) cdr-atom ($ListofLists))))))
  (let*
    (  ;assign variables
      ($a (car-car $ListofLists))  ; the first symbol in first list
      ($b (car-cdr $ListofLists))  ; the rest of the first list 
      ($c (cdr-atom $ListofLists)) ; the lists following the first list
    )   ; $a = next symbol  $d = remaining list of lists
      ;(let $d (cons-atom $b $c) (cons-atom $a (concat_lists $d)))  ;<-- intended!!!
      (let $d (cons-atom $b $c) (let $f (concat_lists $d) (cons-atom $a $f )))  
  ))
))

;
!(concat_lists (() (1 2 3) () (4 5) (6) ) )
;
;#( = #(nth 0 ($A 
;(= (nth $A #(Cons $B $C) $B)  
;  (= $A  1))
;(= (nth $A #(Cons $B $C) $D) (is $E (- $A 1)) (nth $E $C $D))
;
;
;(= (nth1 $Index $_ $_) (< $Index 1) (fail) (set-det))
;; /* copied from dbd2 */
;(= (nth1 1 #(Cons $Element $Rest) $Element)  
;  (set-det))
;(= (nth1 $I #(Cons $First $List1) $Element) (is $Index (- $I 1)) (nth1 $Index $List1 $Element))
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

!(add-atom &self (console-messages (initializing)))

;*******************************************************
; Commands.  The game is executed using the following commands.
;*******************************************************

; This command must be invoked at startup with "!(chess)"
(= (chess) 
  (match &self (console-messages $msg)
    ; if first invocation, just create board
    (if (==  (initializing)   $msg) ; then  
      (progn (
        ; remove the 'initializing' message
        (remove-atom &self (console-messages $msg))
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
        (add-atom &self (console-messages (started)))
        ; display welcome messages and board
        (welcome)))
    ; elif there has already been one game played
    (if (==  (started)   $msg) ; then
      (progn (
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
        ; LATER ON, REMOVE THE OLD CONSOLE-MESSAGES!
        ;
        ; display welcome messages and board
        (welcome)))
    (; else if
        empty))))) 

; The move command moves the human player's piece.
(= (m $A $B $C $D) (guimessage checkmate $E $F) (write 'Game over.') (nl) (set-det))
;(= (m $A $B $C $D) (board $E) (concat_lists #( :: (#( :: ($A) ) #( :: ($B) )) ) $F) (concat_lists #( :: (#( :: ;($C) ) #( :: ($D) )) ) $G) (\= $F $G) (return_entire_box $F $H $E) (return_entire_box $G $I $E) (or (len $I 2) ;(not (samecolor $H $I))) (set-det) (clear_route $H $I $E) (move_piece $H $I $E $J) (xy_box $K #( :: (s k) ) ;$J) (not (take_dest $K g $J)) (move_piece $H $I $E $L) (= $M  $E) #(remove-atom &self #(board $E)) #(add-atom &;self #(board $L)) (b $L) (printmove $H $I $M) (set-det) (examine_king $L g s) (garbage_collect) (trim_stacks) ;(set-det))
;

;
;(= (g) (guimessage checkmate $A $B) (write 'Game over.') (nl) (set-det))
;(= (g) (board $A) (attemptcheckmate $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board ;$B)) (b $B) (write 'Checkmate!  Deep Blue Dummy Wins!') (nl) #(add-atom &self #(guimessage checkmate s g)) ;(printmove $C $D $A) (set-det))
;(= (g) (board $A) (playdefenseR $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board $B)) (b ;$B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (takehighestopen $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board $B)) ;(b $B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (movetoposition $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board $B)) ;(b $B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (takehighestopenpawn $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board ;$B)) (b $B) (examine_king $B s g) (printmove $C $D $A) (set-det))
;(= (g) (board $A) (random_move_empty_sq $B $A $C $D) #(remove-atom &self #(board $A)) #(add-atom &self #(board ;$B)) (b $B) (printmove $C $D $A) (set-det))
;

; The reset command will start the game over.
(: R (-> command))
(= (R)  
  (chess))

; The "commands" command just lists the available commands.
(: C (-> command))
(= (C) 
  (progn
    (println! " ") (println! " ") (println! " ") (println! " ")
    (println! (format-args '-------- C o m m a n d s -----------' ()))
    (println! (format-args '1) TO MOVE YOUR PIECE USE (example) ->  M 1 2 1 3' ()))
    (println! (format-args '   Result:  YOUR pawn in 1,2 moved to location 1,3 based on standard cartesian x/y.'   ()))
    (println! (format-args '2) Move MeTTa Greedy Chess          ->  G' ()))
    (println! (format-args '3) Reset                            ->  R' ()))
    (println! (format-args '4) Commands List                    ->  C' ()))
    (println! (format-args '5) Display Board                    ->  D' ()))
    (println! (format-args '6) Quit                             ->  Q' ()))
    (println! (format-args 'You may now enter your move M x1 y1 x2 y2 command.' ()))))
;

; The display command shows the present board.
(: D (-> command))
(= (D) (display_board (match &self (board-state $board) $board)))

;*******************************************************
; Code invoked by the basic commands (above) or elsewhere follow
;*******************************************************


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
   ((println! " ") (println! " ") (println! " ") (println! " ")
    (println! (format-args 'M E T T A    G R E E D Y   C H E S S' ()))
    (println! " ")
    (println! (format-args 'This program is a MeTTa exercise which takes the best immediate move without planning far ahead.'
              ()))
    (display_board (match &self (board-state $board) $board))
    (println! (format-args '******* I N S T R U C T I O N S ********' ()))
    (println! " ")
    (println! (format-args '- Your pieces are marked with an asterisk.' ()))
    (println! (format-args '- Please take note of the following simple commands:' ()))
    (println! (format-args '-------- C o m m a n d s -----------' ()))
    (println! (format-args '1) TO MOVE YOUR PIECE USE (example) ->  M 1 2 1 3' ()))
    (println! (format-args '   Result:  YOUR pawn in 1,2 moved to location 1,3 based on standard cartesian x/y.'   ()))
    (println! (format-args '2) Move MeTTa Greedy Chess          ->  G' ()))
    (println! (format-args '3) Reset                            ->  R' ()))
    (println! (format-args '4) Commands List                    ->  C' ()))
    (println! (format-args '5) Display Board                    ->  D' ()))
    (println! (format-args '6) Quit                             ->  Q' ()))
    (println! (format-args 'You may now enter your move M x1 y1 x2 y2 command.' ()))
    )))

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
   (let $a (display_filter     $board ) 
          (println! (format-args "\n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  |\n
      ------------------------------------------------- \n
      " 
      
          $a)))))

;*******************************************************
;   M A I N   C O M M A N D   L O O P
;*******************************************************
(= (command-loop)
  (do
    (println! "Please enter your command.")
    ; case statement for commands
    (let $command (get-player-command ())
      (if (== $command 77)
          (do 
            (println! "running command M") ; Run valid command
            (command-loop))              ; Get next command, stay in loop.
      (if (== $command 71)
          (do 
            (println! "running command G") ; Run valid command
            (command-loop))              ; Get next command, stay in loop.
      (if (== $command 82)
          (do 
            (R)                          ; Reset = "R"
            (command-loop))              ; Get next command, stay in loop.
      (if (== $command 67)
          (do 
            (C)                          ; List valid commands = "C"
            (command-loop))              ; Get next command, stay in loop.
      (if (== $command 68)
          (do 
            (D)                          ; Display board "D"
            (command-loop))              ; Get next command, stay in loop.
      (if (== $command 81)
            (println! "Quitting MeTTa Greedy Chess.")
      ; otherwise
      (do 
          (println! " ") (println! "Invalid command, please try again or enter C for a list of commands.") 
          (command-loop)))))))))))

;-----------;
(= (main_loop) 
  (do
    (chess)          ; Start the chess game
    (command-loop))) ; Enter the recursive command-processing loop
;!(main_loop)
;-----------;

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
;
;
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
;
;(= (checkgold Nil $A Nil)  
;  (set-det))
;; /* takehighestopenpawn(A, B, C, D) :- write('?????'). */
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
;(= (buildgold Nil Nil)  
;  (set-det))
;(= (buildgold #(Cons $A $B) #(Cons $A $C)) (len $A 4) (nth1 3 $A g) (buildgold $B $C) (set-det))
;(= (buildgold #(Cons $A $B) $C) (buildgold $B $C) (set-det))
;
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
;
;(= (take_dest $A $B $C) (takingboxes $B $C $D) (set-det) (list_clear_route $C $A $D $E) (set-det) (\== $E Nil))
;
;
;(= (return_entire_box $A $B #(Cons $C $D)) (nth1 1 $C $E) (nth1 2 $C $F) (concat_lists #( :: (#( :: ($E) ) #( ;:: ($F) )) ) $G) (== $G $A) (= $B  $C) (set-det))
;(= (return_entire_box $A $B #(Cons $C $D)) (return_entire_box $A $B $D) (set-det))
;
;
;(= (xy_box $A #( :: ($B $C) ) #(Cons $D $E)) (len $D 4) (nth1 3 $D $F) (nth1 4 $D $G) (== $B $F) (== $C $G) ;(nth1 1 $D $H) (nth1 2 $D $I) (concat_lists #( :: (#( :: ($H) ) #( :: ($I) )) ) $A))
;(= (xy_box $A $B #(Cons $C $D))  
;  (xy_box $A $B $D))
;
;
;(= (samecolor $A $B) (nth1 3 $A $C) (nth1 3 $B $D) (set-det) (== $C $D))
;
;
;(= (clear_route #( :: ($A $B $C k) ) #(Cons $D #(Cons $E $F)) $G) (or (= $D  $A) (or (is $D (+ $A 1)) (is $D ;(- $A 1)))) (or (= $E  $B) (or (is $E (+ $B 1)) (is $E (- $B 1)))))
;(= (clear_route #( :: ($A $B $C n) ) #(Cons $D #(Cons $E $F)) $G) (or (is $E (+ $B 2)) (is $E (- $B 2))) (or ;(is $D (+ $A 1)) (is $D (- $A 1))))
;(= (clear_route #( :: ($A $B $C n) ) #(Cons $D #(Cons $E $F)) $G) (or (is $E (+ $B 1)) (is $E (- $B 1))) (or ;(is $D (+ $A 2)) (is $D (- $A 2))))
;(= (clear_route #( :: ($A $B $C q) ) #(Cons $D #(Cons $E $F)) $G)  
;  (clear_route 
;    #( :: 
;      ($A $B $C r) ) 
;    #(Cons $D 
;      #(Cons $E $F)) $G))
;(= (clear_route #( :: ($A $B $C q) ) #(Cons $D #(Cons $E $F)) $G)  
;  (clear_route 
;    #( :: 
;      ($A $B $C b) ) 
;    #(Cons $D 
;      #(Cons $E $F)) $G))
;(= (clear_route #( :: ($A $B g p) ) #(Cons $C #(Cons $D $E)) $F) (= $A  $C) (= $B  2) (is $G (- $B 1)) ;(return_entire_box #( :: ($A $G) ) $H $F) (len $H 2) (is $D (- $B 2)) (return_entire_box #( :: ($C $D) ) $I ;$F) (len $I 2))
;(= (clear_route #( :: ($A $B g p) ) #(Cons $C #(Cons $D $E)) $F) (= $A  $C) (is $D (- $B 1)) ;(return_entire_box #( :: ($C $D) ) $G $F) (len $G 2))
;(= (clear_route #( :: ($A $B g p) ) #(Cons $C #(Cons $D $E)) $F) (return_entire_box #( :: ($C $D) ) $G $F) ;(len $G 4) (is $C (+ $A 1)) (is $D (- $B 1)))
;(= (clear_route #( :: ($A $B g p) ) #(Cons $C #(Cons $D $E)) $F) (return_entire_box #( :: ($C $D) ) $G $F) ;(len $G 4) (is $C (- $A 1)) (is $D (- $B 1)))
;(= (clear_route #( :: ($A $B s p) ) #(Cons $C #(Cons $D $E)) $F) (= $A  $C) (is $D (+ $B 1)) ;(return_entire_box #( :: ($C $D) ) $G $F) (len $G 2))
;(= (clear_route #( :: ($A $B s p) ) #(Cons $C #(Cons $D $E)) $F) (= $A  $C) (= $B  2) (is $G (+ $B 1)) ;(return_entire_box #( :: ($A $G) ) $H $F) (len $H 2) (is $D (+ $B 2)) (return_entire_box #( :: ($C $D) ) $I ;$F) (len $I 2))
;(= (clear_route #( :: ($A $B s p) ) #(Cons $C #(Cons $D $E)) $F) (return_entire_box #( :: ($C $D) ) $G $F) ;(len $G 4) (is $C (+ $A 1)) (is $D (+ $B 1)))
;(= (clear_route #( :: ($A $B s p) ) #(Cons $C #(Cons $D $E)) $F) (return_entire_box #( :: ($C $D) ) $G $F) ;(len $G 4) (is $C (- $A 1)) (is $D (+ $B 1)))
;(= (clear_route #( :: ($A $B $C r) ) #(Cons $D #(Cons $E $F)) $G) (= $A  $D) (> $E $B) (is $H (- $E 1)) (is $I ;(+ $B 1)) (checkclearup $A $I $H $G))
;(= (clear_route #( :: ($A $B $C r) ) #(Cons $D #(Cons $E $F)) $G) (= $A  $D) (< $E $B) (is $H (+ $E 1)) (is $I ;(- $B 1)) (checkcleardown $A $I $H $G))
;(= (clear_route #( :: ($A $B $C r) ) #(Cons $D #(Cons $E $F)) $G) (< $A $D) (= $E  $B) (is $H (- $D 1)) (is $I ;(+ $A 1)) (checkclearright $B $I $H $G))
;(= (clear_route #( :: ($A $B $C r) ) #(Cons $D #(Cons $E $F)) $G) (> $A $D) (= $E  $B) (is $H (+ $D 1)) (is $I ;(- $A 1)) (checkclearleft $B $I $H $G))
;(= (clear_route #( :: ($A $B $C b) ) #(Cons $D #(Cons $E $F)) $G)  
;  (or 
;    (, 
;      (is $D 
;        (+ $A 1)) 
;      (is $E 
;        (+ $B 1))) 
;    (, 
;      (> $D $A) 
;      (> $E $B) 
;      (is $H 
;        (+ $A 1)) 
;      (is $I 
;        (- $D 1)) 
;      (is $J 
;        (+ $B 1)) 
;      (is $K 
;        (- $E 1)) 
;      (checkclearupBUR $H $J $I $K $G))))
;(= (clear_route #( :: ($A $B $C b) ) #(Cons $D #(Cons $E $F)) $G)  
;  (or 
;    (, 
;      (is $D 
;        (+ $A 1)) 
;      (is $E 
;        (- $B 1))) 
;    (, 
;      (> $D $A) 
;      (< $E $B) 
;      (is $H 
;        (+ $A 1)) 
;      (is $I 
;        (- $D 1)) 
;      (is $J 
;        (- $B 1)) 
;      (is $K 
;        (+ $E 1)) 
;      (checkclearupBDR $H $J $I $K $G))))
;(= (clear_route #( :: ($A $B $C b) ) #(Cons $D #(Cons $E $F)) $G)  
;  (or 
;    (, 
;      (is $D 
;        (- $A 1)) 
;      (is $E 
;        (+ $B 1))) 
;    (, 
;      (< $D $A) 
;      (> $E $B) 
;      (is $H 
;        (- $A 1)) 
;      (is $I 
;        (+ $D 1)) 
;      (is $J 
;        (+ $B 1)) 
;      (is $K 
;        (- $E 1)) 
;      (checkclearupBUL $H $J $I $K $G))))
;(= (clear_route #( :: ($A $B $C b) ) #(Cons $D #(Cons $E $F)) $G)  
;  (or 
;    (, 
;      (is $D 
;        (- $A 1)) 
;      (is $E 
;        (- $B 1))) 
;    (, 
;      (< $D $A) 
;      (< $E $B) 
;      (is $H 
;        (- $A 1)) 
;      (is $I 
;        (+ $D 1)) 
;      (is $J 
;        (- $B 1)) 
;      (is $K 
;        (+ $E 1)) 
;      (checkclearupBDL $H $J $I $K $G))))
;
;
;(= (checkclearup $A $B $C $D)  
;  (> $B $C))
;(= (checkclearup $A $B $C $D) (return_entire_box #( :: ($A $B) ) $E $D) (len $E 2) (is $F (+ $B 1)) ;(checkclearup $A $F $C $D))
;
;(= (checkclearleft $A $B $C $D)  
;  (< $B $C))
;(= (checkclearleft $A $B $C $D) (return_entire_box #( :: ($B $A) ) $E $D) (len $E 2) (is $F (- $B 1)) ;(checkclearleft $A $F $C $D))
;
;(= (checkclearright $A $B $C $D)  
;  (> $B $C))
;(= (checkclearright $A $B $C $D) (return_entire_box #( :: ($B $A) ) $E $D) (len $E 2) (is $F (+ $B 1)) ;(checkclearright $A $F $C $D))
;
;(= (checkcleardown $A $B $C $D)  
;  (< $B $C))
;(= (checkcleardown $A $B $C $D) (return_entire_box #( :: ($A $B) ) $E $D) (len $E 2) (is $F (- $B 1)) ;(checkcleardown $A $F $C $D))
;
;
;(= (checkclearupBUR $A $B $C $D $E) (== $A $C) (== $B $D) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2))
;(= (checkclearupBUR $A $B $C $D $E) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2) (is $G (+ $A 1)) (is ;$H (+ $B 1)) (checkclearupBUR $G $H $C $D $E))
;
;(= (checkclearupBDR $A $B $C $D $E) (== $A $C) (== $B $D) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2))
;(= (checkclearupBDR $A $B $C $D $E) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2) (is $G (+ $A 1)) (is ;$H (- $B 1)) (checkclearupBDR $G $H $C $D $E))
;
;(= (checkclearupBUL $A $B $C $D $E) (== $A $C) (== $B $D) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2))
;(= (checkclearupBUL $A $B $C $D $E) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2) (is $G (- $A 1)) (is ;$H (+ $B 1)) (checkclearupBUL $G $H $C $D $E))
;
;(= (checkclearupBDL $A $B $C $D $E) (== $A $C) (== $B $D) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2))
;(= (checkclearupBDL $A $B $C $D $E) (return_entire_box #( :: ($A $B) ) $F $E) (len $F 2) (is $G (- $A 1)) (is ;$H (- $B 1)) (checkclearupBDL $G $H $C $D $E))
;
;
;(= (move_piece $A $B $C $D) (nth1 3 $A $E) (nth1 4 $A $F) (sort $C $G) (sort #( :: ($A $B) ) $H) (removelists ;$H $G $I) (nth1 1 $A $J) (nth1 2 $A $K) (= $L  
;  #( :: 
;    ($J $K) )) (nth1 1 $B $M) (nth1 2 $B $N) (= $O  
;  #( :: 
;    ($M $N $E $F) )) (sort #(Cons $L #(Cons $O $I)) $D) (set-det))
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
;
;#( = #(takingboxes $A () ()) True )
;(= (takingboxes $A #(Cons $B $C) #(Cons $B $D)) (nth1 3 $B $A) (takingboxes $A $C $D))
;(= (takingboxes $A #(Cons $B $C) $D)  
;  (takingboxes $A $C $D))
;
;
;#( = #(list_clear_route $A $B () ()) True )
;(= (list_clear_route $A $B #(Cons $C $D) #(Cons $C $E)) (clear_route $C $B $A) (list_clear_route $A $B $D $E))
;(= (list_clear_route $A $B #(Cons $C $D) $E)  
;  (list_clear_route $A $B $D $E))
;
;
;(= (positiontotake #(Cons $A #(Cons $B $C)) $D $E $F) (cord $G) (cord $H) (return_entire_box #( :: ($G $H) ) ;$E $F) (or (not (samecolor $D $E)) (len $E 2)) (clear_route $D $E $F) (nth1 3 $D $I) (nth1 4 $D $J) (= $K  
;  #( :: 
;    ($G $H $I $J) )) (clear_route $K #( :: ($A $B) ) $F))
;
;
;#( = #(concat_lists () ()) True )
