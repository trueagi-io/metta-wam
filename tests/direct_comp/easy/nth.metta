
(= (nth_x $n $x) (if (== $n 0) (car-atom $x) (let* (($rest (cdr-atom $x)) ($nn (- $n 1))) (nth_x $nn $rest))))

!(nth_x 2 (1 2 3))

!(nth_x 1 (a b c))





