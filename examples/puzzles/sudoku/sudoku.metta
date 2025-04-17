;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell internals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (default-empty-cell) x)

;; A cell is an ordered list of the values that that cell can still take.

(= (cell-pop-all $i $N)
    (if (== $i $N)
        ($N)
        (let $t (cell-pop-all (+ $i 1) $N) (cons-atom $i $t))))

!(assertEqual (cell-pop-all 1 9) (1 2 3 4 5 6 7 8 9))

(= (cell-remove $c $v $changed)
    (if-decons-expr $c $h $t
        (if (== $h $v)
            (let $changed True $t)
            (if (> $h $v)
                (let $changed False $c)
                (let $rest (cell-remove $t $v $changed) (cons-atom $h $rest))
            )
        )
        (let $changed False ())
    )
)

(= (cell-length $c) (size-atom $c))

!(assertEqual (let $c (cell-remove (1 2 3 4) 8 $changed) ($changed $c)) (False (1 2 3 4)))
!(assertEqual (let $c (cell-remove (1 2 3 4) 0 $changed) ($changed $c)) (False (1 2 3 4)))
!(assertEqual (let $c (cell-remove (1 2 3 4) 3 $changed) ($changed $c)) (True (1 2 4)))
!(assertEqual (let $c (cell-remove (1 2 4) 3 $changed) ($changed $c)) (False (1 2 4)))

(= (value-to-cell $N $v)
    (if (== (get-type $v) Number)
        (if (and (< 0 $v) (<= $v $N)) ; have to do this separately, as HE won't short circuit, then will give type error
            ($v)
            (cell-pop-all 1 $N)
        )
        (cell-pop-all 1 $N)
    )
)

!(assertEqual (value-to-cell 9 0) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 10) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 x) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 -) (1 2 3 4 5 6 7 8 9))
!(assertEqual (value-to-cell 9 7) (7))

(= (cell-to-value $c)
    (let ($t $u) (decons-atom $c)
        (if (== $u ()) $t (default-empty-cell))
    )
)

!(assertEqual (cell-to-value (7)) 7)
!(assertEqual (cell-to-value (2 7)) (default-empty-cell))

!(assertEqual (map-atom (0 0 1 2) $v (value-to-cell 4 $v)) ((1 2 3 4) (1 2 3 4) (1) (2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell higher processing - dealing with singletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(= (remove-singletons $c $n $singletons $changed)
    (if-decons-expr $singletons $sh $st
        (let* (
            (($shi $shv) $sh)
            ($c1 (remove-singletons $c $n $st $changed1))
            ($c2
                (if (== $shi $n)
                    (let $changed2 False $c1)
                    (cell-remove $c1 $shv $changed2)
                )
            ))
            (let $changed (or $changed1 $changed2) $c2)
        )
        (let $changed False $c)
    )
)

;(= (retvalue $x $y) (let $yy $x (+ $y 1)))
;!(retvalue 1 $x)
;!(let $t (retvalue 1 $x) ($x $t))

!(assertEqual (let $c (remove-singletons (2) 0 () $changed) ($changed $c)) (False (2)))
!(assertEqual (let $c (remove-singletons (2) 0 ((1 4) (0 2)) $changed) ($changed $c)) (False (2)))
!(assertEqual (let $c (remove-singletons (4) 1 ((1 4) (0 2)) $changed) ($changed $c)) (False (4)))
!(assertEqual (let $c (remove-singletons (1 2 3 4) 2 ((1 4)) $changed) ($changed $c)) (True (1 2 3)))
!(assertEqual (let $c (remove-singletons (1 2 3 4) 2 ((0 2)) $changed) ($changed $c)) (True (1 3 4)))
!(assertEqual (let $c (remove-singletons (1 2 3 4) 2 ((1 4) (0 2)) $changed) ($changed $c)) (True (1 3)))
!(assertEqual (let $c (remove-singletons (1 2 3 4) 2 ((0 2) (1 4)) $changed) ($changed $c)) (True (1 3)))
!(assertEqual (let $c (remove-singletons (1 2 3) 2 ((1 4)) $changed) ($changed $c)) (False (1 2 3)))


(= (collect-histogram-aux1 $n $v $nv) (if (== $v None) ($nv $n) NoProcess))

!(assertEqual (collect-histogram-aux1 0 None 3) (3 0))
!(assertEqual (collect-histogram-aux1 0 (4 0) 3) NoProcess)
!(assertEqual (collect-histogram-aux1 0 NoProcess 3) NoProcess)

(= (collect-histogram-aux $histogram $n $nv $c)
    (let ($h $t) (decons-atom $histogram)
        (if (< $n $c)
            (let $hist2 (collect-histogram-aux $t (+ $n 1) $nv $c) (cons-atom $h $hist2))
            (let $h1 (collect-histogram-aux1 $n $h $nv) (cons-atom $h1 $t))
        )
    )
)

!(assertEqual (collect-histogram-aux (None None None None) 1 1 3) (None None (1 3) None))
!(assertEqual (collect-histogram-aux (None None (1 3) None) 1 2 4) (None None (1 3) (2 4)))
!(assertEqual (collect-histogram-aux (None None (1 3) (1 4)) 1 3 4) (None None (1 3) NoProcess))

(= (collect-histogram-singleton $histogram $n $c)
    (let ($h $t) (decons-atom $histogram)
        (if (< $n $c)
            (let $hist2 (collect-histogram-singleton $t (+ $n 1) $c) (cons-atom $h $hist2))
            (cons-atom NoProcess $t)
        )
    )
)
(= (collect-histogram $histogram $n $cell)
    (if-decons-expr $cell $c $ct
        (let $hist2 (collect-histogram $histogram $n $ct)
            (collect-histogram-aux $hist2 1 $n $c))
        $histogram
    )
)

!(assertEqual (collect-histogram (None None None None) 3 (1 3)) ((3 1) None (3 3) None))
!(assertEqual (collect-histogram ((3 1) None (3 3) None) 2 (2 3)) ((3 1) (2 2) NoProcess None))

(= (remove-singleton-row-collect-info-aux $row $n $N $singletons $changed $newsing $histogram)
    (if-decons-expr $row $rh $rt
        (let* (
            ($row1 (remove-singleton-row-collect-info-aux $rt (+ $n 1) $N $singletons $changed1 $newsing1 $histogram1))
            ($cell (remove-singletons $rh $n $singletons $changed2))
            ($changed (if $changed1 True $changed2))
            ($newrow (cons-atom $cell $row1))
            ($histogram
                (if-decons-expr $cell $ch $ct
                    (if (== $ct ())
                        (collect-histogram-singleton $histogram1 1 $ch)
                        (collect-histogram $histogram1 $n $cell)
                    )
                    (empty)
                )))
            (if (and $changed2 (== (size-atom $cell) 1))
                (let* (
                    (($f) $cell)
                    ($newsing (cons-atom ($n $f) $newsing1)))
                    $newrow
                )
                (let $newsing $newsing1 $newrow)
            )
        )
        (let* (
            ($changed False)
            ($newsing ())
            ($histogram (dup-list $N None)))
            ()
        )
    )
)

!(assertEqual (let $c (remove-singleton-row-collect-info-aux ((1 2 3 4) (1 2 3 4) (1) (2)) 0 4 () $changed $ns $h) ($changed $c $ns $h))
    (False ((1 2 3 4) (1 2 3 4) (1) (2)) () (NoProcess NoProcess NoProcess NoProcess)))
!(assertEqual (let $c (remove-singleton-row-collect-info-aux ((1 2 3 4) (1 2 3 4) (1) (2)) 0 4 ((3 2) (2 1)) $changed $ns $h) ($changed $c $ns $h))
    (True ((3 4) (3 4) (1) (2)) () (NoProcess NoProcess NoProcess NoProcess)))
!(assertEqual (let $c (remove-singleton-row-collect-info-aux ((3 4) (3 4) (1) (2)) 0 4 ((3 2) (2 1)) $changed $ns $h) ($changed $c $ns $h))
    (False ((3 4) (3 4) (1) (2)) () (NoProcess NoProcess NoProcess NoProcess)))
!(assertEqual (let $c (remove-singleton-row-collect-info-aux ((1 2 3) (1 2 3 4) (1) (2)) 0 4 ((3 2) (2 1)) $changed $ns $h) ($changed $c $ns $h))
    (True ((3) (3 4) (1) (2)) ((0 3)) (NoProcess NoProcess NoProcess (1 4))))
!(assertEqual (let $c (remove-singleton-row-collect-info-aux ((1 2 3) (1 2 4) (1 2) (1 2)) 0 4 () $changed $ns $h) ($changed $c $ns $h))
    (False ((1 2 3) (1 2 4) (1 2) (1 2)) () (NoProcess NoProcess (0 3) (1 4))))

(= (hist-cleanup $hist)
    (if-decons-expr $hist $h $t
        (let $tail (hist-cleanup $t)
            (if (== $h None) (Empty) (if (== $h NoProcess) $tail (cons-atom $h $tail)))
        )
        ()
    )
)

!(assertEqual (hist-cleanup (NoProcess (0 3) (1 4) NoProcess)) ((0 3) (1 4)))

(= (remove-singleton-row-collect-info $row $N $singletons)
    (let* (
        (($changed $newrow $newsing $hist) (remove-singleton-row-collect-info-aux $row 0 $N $singletons))
        ($newhist (hist-cleanup $hist)))
    )
)

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

