; Move a disk from peg X to peg Y.
(= (move $x $y)
   (if (and (not-empty $x) (can-place $y $x))
       (seq (remove-top $x) (place-top $y $x)) nop))

; Check if peg is not empty
(= (not-empty $x) (neq (top $x) None))

; Check if a disk can be placed on a peg
(= (can-place $y $x)
   (or (eq (top $y) None) (< (top $x) (top $y))))

; Remove the top disk from a peg
(= (remove-top $x) (, (assign $x (rest $x)) True))

; Place the top disk from peg X onto peg Y
(= (place-top $y $x) (, (assign $y (cons (top $x) $y)) True))

; Get the top of a peg
(= (top $x) (head $x))

; Get the rest of a peg
(= (rest $x) (tail $x))


; Towers of Hanoi test
(= (hanoi $n $src $aux $dest)
   (if (eq $n 1)
       (move $src $dest)
       (seq (hanoi (- $n 1) $src $dest $aux)
            (move $src $dest)
            (hanoi (- $n 1) $aux $src $dest))))

(= (hanoi_test)
   (let ((src '(3 2 1)) (aux '()) (dest '()))
       (hanoi 3 src aux dest)
       (assertEqual dest '(3 2 1))))


!(hanoi_test)

