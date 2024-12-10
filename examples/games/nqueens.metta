; Define the 'select' function to split a list into head and tail.
(= (select $x) 
    ((car-atom $x) (cdr-atom $x))) ; Select decomposes $x into its head and tail.

; Extended 'select' function with nested let* for more complex deconstruction.
(= (select $x) 
    (let* (($y (car-atom $x))          ; Extract the head of $x into $y.
           ($z (cdr-atom $x))          ; Extract the tail of $x into $z.
           (($u $v) (select $z)))      ; Recursively apply select to $z.
      ($u (cons-atom $y $v))))         ; Combine $y with the processed tail.

; Define a function to create a range of numbers from $x to $y.
(= (range $x $y)
    (if (== $x $y)                    ; Base case: if start equals end, return $x.
        ($x)
        (let $z (range (+ $x 1) $y)   ; Recursive case: create range for $x+1 to $y.
            (cons-atom $x $z))))      ; Add $x to the front of the range list.

; Main function to solve the N-Queens problem for $n.
(= (nqueens $n)
    (let $r (range 1 $n)              ; Generate a list of positions 1 to $n.
        (nqueens_aux $r ())))         ; Start solving with all positions unplaced.

; Auxiliary function for solving N-Queens.
(= (nqueens_aux $unplaced $safe)
    (if (== $unplaced ())             ; Base case: all queens are placed.
        $safe
        (let ($q $r) (select $unplaced) ; Split unplaced into current $q and remaining $r.
            (if (not_attack $q 1 $safe) ; Check if $q can be placed safely.
                (let $safeext (cons-atom $q $safe) ; Extend $safe with the new placement.
                    (nqueens_aux $r $safeext)))))) ; Recur with remaining queens.

; Function to check if a queen placement attacks others.
(= (not_attack $q $d $s)
    (if (== $s ())                     ; Base case: no queens to check against.
        True
        (let* (($h (car-atom $s))      ; Get the head of the list ($h).
               ($t (cdr-atom $s)))     ; Get the tail of the list ($t).
            (if (or (== $q $h)         ; Check for same row.
                    (or (== $q (+ $d $h)) ; Check for diagonal attacks.
                        (== $h (+ $q $d)))) 
                False                  ; Attack detected.
                (not_attack $q (+ $d 1) $t))))) ; Recur with the next queen.

; Call the N-Queens function for a 12x12 chessboard.

!(range 4 10)
