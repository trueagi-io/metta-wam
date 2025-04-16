(= (append $a $b)
    (if-decons-expr $a $h $t
        (let $tail (append $t $b) (cons-atom $h $tail))
        $b)
)

!(assertEqual (append () (1 2 3 4)) (1 2 3 4))
!(assertEqual (append (1 2) (3 4)) (1 2 3 4))
!(assertEqual (append (1 2 3 4) ()) (1 2 3 4))

(= (append-all $l)
    (if-decons-expr $l $h $t
        (let $tail (append-all $t)
            (append $h $tail))
        ()
    )
)

!(assertEqual (append-all (() ((1 2 1) (1 3 2)) ((2 0 2) (2 1 4)) ())) ((1 2 1) (1 3 2) (2 0 2) (2 1 4)))

(= (dup-list $n $v)
    (if (== $n 0) () (let $t (dup-list (- $n 1) $v) (cons-atom $v $t)))
)

!(assertEqual (dup-list 5 a) (a a a a a))

;; A cell is an ordered list of the values that that cell can still take.

(= (cell-pop-all $i $N)
    (if (== $i $N)
        ($N)
        (let $t (cell-pop-all (+ $i 1) $N) (cons-atom $i $t))))

!(assertEqual (cell-pop-all 1 9) (1 2 3 4 5 6 7 8 9))

;(= (cell_remove $c $v)
;    (if-decons-expr $c $h $t
;        (if (== $h $v)
;           (True $t)

(= (value-to-cell $N $v)
    (if (and (== (get-type $v) Number) (and (< 0 $v) (<= $v $N)))
        ($v)
        (cell-pop-all 1 $N)))

!(assertEqual (value-to-cell 9 0) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 10) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 x) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 -) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 7) (7))

(= (default-empty-cell) x)

(= (cell-to-value $c)
    (let ($t $u) (decons-atom $c)
        (if (== $u ()) $t (default-empty-cell))))

!(assertEqual (cell-to-value (7)) 7)
!(assertEqual (cell-to-value (2 7)) (default-empty-cell))

!(assertEqual (map-atom (0 0 1 2) $v (value-to-cell 4 $v)) ((1 2 3 4) (1 2 3 4) (1) (2)))

(= (collect-singleton1 $n $r)
    (if-decons-expr $r $h $t
        (let* (
            ($tail (collect-singleton1 (+ $n 1) $t))
            (($v $tt) (decons-atom $h)))
            (if (== $tt ()) (cons-atom ($n $v) $tail) $tail))
        ()
    )
)

(= (collect-singleton2 $n $r)
    (if-decons-expr $r $h $t
        (let* (
            ($tail (collect-singleton2 (+ $n 1) $t))
            ($m (map-atom $h $v (cons-atom $n $v))))
            (cons-atom $m $tail)
        )
        ()
    )
)

(= (extract-singletons $B)
    (append-all (collect-singleton2 0 (map-atom $B $r (collect-singleton1 0 $r))))
)

(= (sort-singleton ($s1 $s2 $s3) $a)
    (let ($h $t) (decons-atom $a)
        (if (== $s1 0)
            (let $h2 (cons-atom ($s2 $s3) $h) (cons-atom $h2 $t))
            (let $t2 (sort-singleton ((- $s1 1) $s2 $s3) $t) (cons-atom $h $t2))
        )
    )
)

!(assertEqual (sort-singleton (2 3 2) (() () () ())) (() () ((3 2)) ()))
!(assertEqual (foldl-atom () (() () () ()) $a $b (sort-singleton $a $b)) (() () () ()))
!(assertEqual (foldl-atom ((1 2 1)) (() () () ()) $a $b (sort-singleton $b $a)) (() ((2 1)) () ()))
!(assertEqual (foldl-atom ((1 2 1) (1 2 2) (3 2 1)) (() () () ()) $a $b (sort-singleton $b $a)) (() ((2 2) (2 1)) () ((2 1))))

(= (sudoku-iteration $N $singletons $bb)
    (let* (
        ($N2 (* $N $N))
        ($empty-sing (dup-list $N2 ()))
        ($sorted-sing (foldl-atom $singletons $empty-sing $a $b (sort-singleton $b $a)))
    )
    $sorted-sing)
)

(= (sudoku-n $N $B) (let* (
        ($N2 (* $N $N))
        ($bx (map-atom $B $r (map-atom $r $v (value-to-cell $N2 $v))))
        ($singletons (extract-singletons $bx))
        ($bxf (sudoku-iteration $N $singletons $bx))
        ($bx2 (map-atom $bx $rx (map-atom $rx $vx (cell-to-value $vx))))
    )
    ($singletons $bx $bx2 $bxf)
    )
)

!(sudoku-n 2 ( (x x x x) (x x 1 2) (2 4 x x) (x x x x) ))


