; Move a disk from peg X to peg Y.
(= (move-disk $disk $x $y)
   (if (and (can-remove $x $disk) (can-place $y $disk))
       (seq (remove-atom &$x (position bottom $disk))
            (add-atom &$y (position middle $disk))) nop))

; Check if a disk can be removed from a peg
(= (can-remove $x $disk) (atom-exists $x (position bottom $disk)))

; Check if a disk can be placed on a peg
(= (can-place $y $disk)
   (or (not (any-disk-exists $y)) (smaller-than-top $y $disk)))

; Check if any disk exists on a peg
(= (any-disk-exists $y) (or (atom-exists $y (position middle red-disk))
                            (atom-exists $y (position middle blue-disk))
                            (atom-exists $y (position middle green-disk))))

; Check if a disk is smaller than the top disk on a peg
; GPT4: Youd need to define the logic for this
; USER: Will use prolog''s "standard-order"
(= (smaller-than-top $y $disk) (and (@< $y $disk)))

; Towers of Hanoi test
(= (hanoi $n $src $aux $dest)
   (if (eq $n 1)
       (move-disk red-disk $src $dest)
       (seq (hanoi (- $n 1) $src $dest $aux)
            (move-disk red-disk $src $dest)
            (hanoi (- $n 1) $aux $src $dest))))

(= (hanoi_test)
   (let ((src (position bottom red-disk) (position middle blue-disk) (position top green-disk)) (aux ()) (dest ()))
       (hanoi 3 src aux dest)
       (assertEqual dest (position bottom red-disk) (position middle blue-disk) (position top green-disk))))


!(hanoi_test)

