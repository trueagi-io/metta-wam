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

(= (divmod $x $y) (let $m (% $x $y) ((/ (- $x $m) $y) $m)))

(= (select $x) (if (== $x ()) (empty) (car-atom $x)))
(= (select $x) (if (== $x ()) (empty) (let $z (cdr-atom $x) (select $z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell internals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (default-empty-cell) x)

;; A cell is an ordered list of the values that that cell can still take.

(= (cell-pop-all $i $N2)
    (if (== $i $N2)
        ($N2)
        (let $t (cell-pop-all (+ $i 1) $N2) (cons-atom $i $t))))

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

(= (value-to-cell $N2 $v)
    (if (== (get-type $v) Number)
        (if (and (< 0 $v) (<= $v $N2)) ; have to do this separately, as HE won't short circuit, then will give type error
            ($v)
            (cell-pop-all 1 $N2)
        )
        (cell-pop-all 1 $N2)
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
            (($v $tt) (decons-atom $h))
            )
            (if (== $tt ()) (cons-atom ($n $v) $tail) $tail))
        ()
    )
)

(= (collect-singleton2 $n $r)
    (if-decons-expr $r $h $t
        (let* (
            ($tail (collect-singleton2 (+ $n 1) $t))
            ($m (map-atom $h $v (cons-atom $n $v)))
            )
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
            )
            )
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

(= (process-board-one-row-aux $row $n $N2 $singletons $changed $newsing $histogram)
    (if-decons-expr $row $rh $rt
        (let* (
            ($row1 (process-board-one-row-aux $rt (+ $n 1) $N2 $singletons $changed1 $newsing1 $histogram1))
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
                ))
            )
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
            ($histogram (dup-list $N2 None))
            )
            ()
        )
    )
)

!(assertEqual (let $c (process-board-one-row-aux ((1 2 3 4) (1 2 3 4) (1) (2)) 0 4 () $changed $ns $h) ($changed $c $ns $h))
    (False ((1 2 3 4) (1 2 3 4) (1) (2)) () (NoProcess NoProcess NoProcess NoProcess)))
!(assertEqual (let $c (process-board-one-row-aux ((1 2 3 4) (1 2 3 4) (1) (2)) 0 4 ((3 2) (2 1)) $changed $ns $h) ($changed $c $ns $h))
    (True ((3 4) (3 4) (1) (2)) () (NoProcess NoProcess NoProcess NoProcess)))
!(assertEqual (let $c (process-board-one-row-aux ((3 4) (3 4) (1) (2)) 0 4 ((3 2) (2 1)) $changed $ns $h) ($changed $c $ns $h))
    (False ((3 4) (3 4) (1) (2)) () (NoProcess NoProcess NoProcess NoProcess)))
!(assertEqual (let $c (process-board-one-row-aux ((1 2 3) (1 2 3 4) (1) (2)) 0 4 ((3 2) (2 1)) $changed $ns $h) ($changed $c $ns $h))
    (True ((3) (3 4) (1) (2)) ((0 3)) (NoProcess NoProcess NoProcess (1 4))))
!(assertEqual (let $c (process-board-one-row-aux ((1 2 3) (1 2 4) (1 2) (1 2)) 0 4 () $changed $ns $h) ($changed $c $ns $h))
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

(= (apply-singleton $n $row $sing)
    (let* (
        (($entry $value) $sing)
        (($h $t) (decons-atom $row))
        )
        (if (< $n $entry)
            (let $tail (apply-singleton (+ $n 1) $t $sing) (cons-atom $h $tail))
            (cons-atom ($value) $t)
        )
    )
)

!(assertEqual (apply-singleton 0 ((1 2 3) (1 2 4) (1 2) (1 2)) (1 4)) ((1 2 3) (4) (1 2) (1 2)))
!(assertEqual (foldl-atom ((1 4) (0 3)) ((1 2 3) (1 2 4) (1 2) (1 2)) $a $b (apply-singleton 0 $a $b)) ((3) (4) (1 2) (1 2)))

(= (process-board-one-row $row $N2 $singletons $changed $newsing)
    (let* (
        ($newrow0 (process-board-one-row-aux $row 0 $N2 $singletons $changed0 $newsing0 $hist))
        ($newhist (hist-cleanup $hist))
        ($changed (or $changed0 (not (== $newhist ()))))
        ($newsing (append $newsing0 $newhist))
        )
        (foldl-atom $newhist $newrow0 $a $b (apply-singleton 0 $a $b))
    )
)

!(assertEqual (let $c (process-board-one-row ((1 2 3 4) (1 2 3 4) (1) (2)) 4 () $changed $ns) ($changed $c $ns))
    (False ((1 2 3 4) (1 2 3 4) (1) (2)) ()))
!(assertEqual (let $c (process-board-one-row ((1 2 3 4) (1 2 3 4) (1) (2)) 4 ((3 2) (2 1)) $changed $ns) ($changed $c $ns))
    (True ((3 4) (3 4) (1) (2)) ()))
!(assertEqual (let $c (process-board-one-row ((3 4) (3 4) (1) (2)) 4 ((3 2) (2 1)) $changed $ns) ($changed $c $ns))
    (False ((3 4) (3 4) (1) (2)) ()))
!(assertEqual (let $c (process-board-one-row ((1 2 3) (1 2 3 4) (1) (2)) 4 ((3 2) (2 1)) $changed $ns) ($changed $c $ns))
    (True ((3) (4) (1) (2)) ((0 3) (1 4))))
!(assertEqual (let $c (process-board-one-row ((1 2 3) (1 2 4) (1 2) (1 2)) 4 () $changed $ns) ($changed $c $ns))
    (True ((3) (4) (1 2) (1 2)) ((0 3) (1 4))))

(= (process-board-one-dimension $board $n $N2 $singletons $changed $newsing)
    (if-decons-expr $board $boardh $boardt
       (let* (
            (($singh $singt) (decons-atom $singletons))
            ($newrow (process-board-one-row $boardh $N2 $singh $changedh $newsingh))
            ($newsinghp (map-atom $newsingh $v (cons-atom $n $v)))
            ($tail (process-board-one-dimension $boardt (+ $n 1) $N2 $singt $changedt $newsingt))
            ($changed (or $changedh $changedt))
            ($newsing (append $newsinghp $newsingt))
            )
            (cons-atom $newrow $tail)
        )
        (let* (($changed False) ($newsing ())) ())
    )
)

!(assertEqual (let $c (process-board-one-dimension (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1) (2)) ((2) (4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) 0 4 (() ((3 2) (2 1)) ((1 4) (0 2)) ()) $changed $newsing) ($changed $c $newsing))
    (True (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((3 4) (3 4) (1) (2)) ((2) (4) (1 3) (1 3)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) ()))
!(assertEqual (let $c (process-board-one-dimension (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((1 2 4) (1 2 3 4) (1) (2)) ((2) (4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) 0 4 (() ((3 2) (2 1)) ((1 4) (0 2)) ()) $changed $newsing) ($changed $c $newsing))
    (True (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((4) (3) (1) (2)) ((2) (4) (1 3) (1 3)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) ((1 0 4) (1 1 3))))

(= (transpose-board $b)
    (let* (
        ($h (map-atom $b $v0 (car-atom $v0)))
        ($t (map-atom $b $v0 (cdr-atom $v0)))
        ($tail
            (if (== (car-atom $t) ())
                ()
                (transpose-board $t)
            )
        )
        )
        (cons-atom $h $tail)
    )
)

!(assertEqual (transpose-board ((1 2 3) (4 5 6) (7 8 9))) ((1 4 7) (2 5 8) (3 6 9)))

(= (transpose-coordinate ($x $y $v)) ($y $x $v))

!(assertEqual (transpose-coordinate (1 2 3)) (2 1 3))

(= (transpose-cell-board (
    ($a1 $a2 $a3 $a4)
    ($b1 $b2 $b3 $b4)
    ($c1 $c2 $c3 $c4)
    ($d1 $d2 $d3 $d4)
    )) (
    ($a1 $a2 $b1 $b2)
    ($a3 $a4 $b3 $b4)
    ($c1 $c2 $d1 $d2)
    ($c3 $c4 $d3 $d4)
))

(= (transpose-cell-board (
    ($a1 $a2 $a3 $a4 $a5 $a6 $a7 $a8 $a9)
    ($b1 $b2 $b3 $b4 $b5 $b6 $b7 $b8 $b9)
    ($c1 $c2 $c3 $c4 $c5 $c6 $c7 $c8 $c9)
    ($d1 $d2 $d3 $d4 $d5 $d6 $d7 $d8 $d9)
    ($e1 $e2 $e3 $e4 $e5 $e6 $e7 $e8 $e9)
    ($f1 $f2 $f3 $f4 $f5 $f6 $f7 $f8 $f9)
    ($g1 $g2 $g3 $g4 $g5 $g6 $g7 $g8 $g9)
    ($h1 $h2 $h3 $h4 $h5 $h6 $h7 $h8 $h9)
    ($i1 $i2 $i3 $i4 $i5 $i6 $i7 $i8 $i9)
    )) (
    ($a1 $a2 $a3 $b1 $b2 $b3 $c1 $c2 $c3)
    ($a4 $a5 $a6 $b4 $b5 $b6 $c4 $c5 $c6)
    ($a7 $a8 $a9 $b7 $b8 $b9 $c7 $c8 $c9)
    ($d1 $d2 $d3 $e1 $e2 $e3 $f1 $f2 $f3)
    ($d4 $d5 $d6 $e4 $e5 $e6 $f4 $f5 $f6)
    ($d7 $d8 $d9 $e7 $e8 $e9 $f7 $f8 $f9)
    ($g1 $g2 $g3 $h1 $h2 $h3 $i1 $i2 $i3)
    ($g4 $g5 $g6 $h4 $h5 $h6 $i4 $i5 $i6)
    ($g7 $g8 $g9 $h7 $h8 $h9 $i7 $i8 $i9)
))

!(assertEqual (transpose-cell-board ((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) ((1 2 5 6) (3 4 7 8) (9 10 13 14) (11 12 15 16)))

(= (transpose-cell-coordinate $N ($y $x $value))
    (let* (
        (($xh $xl) (divmod $x $N))
        (($yh $yl) (divmod $y $N))
        )
        ((+ (* $N $yh) $xh) (+ (* $N $yl) $xl) $value)))

!(assertEqual (map-atom ((0 0 0) (0 1 1) (0 2 2) (0 3 3) (1 0 10) (1 1 11) (1 2 12) (1 3 13) (2 0 20) (2 1 11) (2 2 22) (2 3 23) (3 0 30) (3 1 31) (3 2 32) (3 3 33)) $x (transpose-cell-coordinate 2 $x))
    ((0 0 0) (0 1 1) (1 0 2) (1 1 3) (0 2 10) (0 3 11) (1 2 12) (1 3 13) (2 0 20) (2 1 11) (3 0 22) (3 1 23) (2 2 30) (2 3 31) (3 2 32) (3 3 33)))


(= (sort-singleton-list $N2 $singletons)
    (let $empty-sing (dup-list $N2 ()) (foldl-atom $singletons $empty-sing $a $b (sort-singleton $b $a))))

!(assertEqual (sort-singleton-list 4 ((1 3 2) (1 2 1) (2 1 4) (2 0 2))) (() ((2 1) (3 2)) ((0 2) (1 4)) ()))

(= (process-board-all-dimensions $board $N $singletons $changed $newsing)
    (let* (
        ($N2 (* $N $N))
        ; first the rows
        ($singsorted (sort-singleton-list $N2 $singletons))
        ($board0 (process-board-one-dimension $board 0 $N2 $singsorted $changed0 $newsing0))
        ; then the columns
        ($board0t (transpose-board $board0))
        ($singletonst (map-atom $singletons $v0 (transpose-coordinate $v0)))
        ($singletonstsorted (sort-singleton-list $N2 $singletonst))
        ($board1t (process-board-one-dimension $board0t 0 $N2 $singletonstsorted $changed1 $newsing1t))
        ($board1 (transpose-board $board1t))
        ($newsing1 (map-atom $newsing1t $v1 (transpose-coordinate $v1)))
        ; then the 3x3 cells
        ($board1u (transpose-cell-board $board1))
        ($singletonsu (map-atom $singletons $v2 (transpose-cell-coordinate $N $v2)))
        ($singletonsusorted (sort-singleton-list $N2 $singletonsu))
        ($board2u (process-board-one-dimension $board1u 0 $N2 $singletonsusorted $changed2 $newsing2u))
        ($board2 (transpose-cell-board $board2u))
        ($newsing2 (map-atom $newsing2u $v3 (transpose-cell-coordinate $N $v3)))
        ; then combine the change flags and new singletons
        ($changed (or $changed0 (or $changed1 $changed2)))
        ($newsing (append $newsing0 (append $newsing1 $newsing2)))
        )
        $board2
    )
)

; !(let $c (process-board-all-dimensions (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1) (2)) ((2) (4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) 2 ((1 3 2) (1 2 1) (2 1 4) (2 0 2)) $changed $newsing) ($changed $c $newsing))

(= (process-board-all-dimensions-fixed $iteration $left $leftout $board $N $singletons)
    (if (== $left (size-atom $singletons))
        (let $leftout 0 $board)
        (let* (
            ($newleft (- $left (size-atom $singletons)))
            (() (println! (format-args "Iteration:{}, cells left {}" ($iteration $newleft))))
            ($board2 (process-board-all-dimensions $board $N $singletons $changed $newsing))
            )
            (if $changed
                (process-board-all-dimensions-fixed (+ $iteration 1) $newleft $leftout $board2 $N $newsing)
                (let $leftout (- $newleft (size-atom $newsing)) $board2)
            )
        )
    )
)

(= (smallest-multicell-row $row $n $smallest)
    (if-decons-expr $row $h $t
        (let* (
            ($best (smallest-multicell-row $t (+ $n 1) $smallest0))
            ($len (size-atom $h))
            )
            (if (and (< 1 $len) (<= $len $smallest0))
                (let $smallest $len ($n $h))
                (let $smallest $smallest0 $best)
            )
        )
        (let $smallest 1000 ())
    )
)

!(assertEqual (smallest-multicell-row (() (1) (3 2) (1) (4 3) (5)) 0 $smallest) (2 (3 2)))

(= (smallest-multicell $board $n $smallest)
    (if-decons-expr $board $h $t
        (let* (
            ($best (smallest-multicell $t (+ $n 1) $smallest0))
            ($bestrow (smallest-multicell-row $h 0 $smallestrow))
            )
            (if (< $smallestrow $smallest0)
                (let $smallest $smallestrow (cons-atom $n $bestrow))
                (let $smallest $smallest0 $best)
            )
        )
        (let $smallest 1000 ())
    )
)

!(assertEqual (smallest-multicell (() ((4)) (() (1) (3 2) (1) (4 3) (5)) ((3))) 0 $smallest) (2 2 (3 2)))
!(assertEqual (smallest-multicell (((4)) (() (1) (3 2) (1) (4 3) (5))) 0 $smallest) (1 2 (3 2)))

(= (update-row $row $n $value)
    (let ($h $t) (decons-atom $row)
        (if (> $n 0)
            (let $tail (update-row $t (- $n 1) $value) (cons-atom $h $tail))
            (cons-atom $value $t)
        )
    )
)

!(assertEqual (update-row (() (1) (3 2) (1) (4 3) (5)) 2 xxx) (() (1) xxx (1) (4 3) (5)))

(= (update-board $board $r $c $value)
    (let ($h $t) (decons-atom $board)
        (if (> $r 0)
            (let $tail (update-board $t (- $r 1) $c $value) (cons-atom $h $tail))
            (let $nv (update-row $h $c $value) (cons-atom $nv $t))
        )
    )
)

!(assertEqual (update-board (() ((4)) (() (1) (3 2) (1) (4 3) (5)) ((3))) 2 2 xxx) (() ((4)) (() (1) xxx (1) (4 3) (5)) ((3))))

(= (process-board-backtrack $iteration $left $board $N $singletons)
    (let $bnext (process-board-all-dimensions-fixed $iteration $left $leftout $board $N $singletons)
        (if (== $leftout 0)
            $bnext
            ; find the smallest ambiguous cell
            (let* (
                (($r $c $values) (smallest-multicell $bnext 0 $_smallest))
                ($v (select $values))
                (() (println! "backtracking"))
                ($bupdate (update-board $bnext $r $c ($v)))
                ($bfinal (process-board-backtrack 0 $leftout $bupdate $N (($r $c $v))))
                ;(() (println! (exit $r $c $v)))
                )
                $bfinal
            )
        )
    )
)

; from: (
;   ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
;   ((1 2 3 4) (1 2 3 4) (1      ) (  2    ))
;   ((  2    ) (      4) (1 2 3 4) (1 2 3 4))
;   ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))
; ((1 3 2) (1 2 1) (2 1 4) (2 0 2))
; 1: (
;   ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
;   ((    3 4) (    3 4) (1      ) (  2    ))
;   ((  2    ) (      4) (1   3  ) (1   3  ))
;   ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))
; ()
; 2: (
;   ((1   3 4) (1 2 3  ) (  2 3 4) (1   3 4))
;   ((    3 4) (    3  ) (1      ) (  2    ))
;   ((  2    ) (      4) (    3  ) (1   3  ))
;   ((1   3 4) (1 2 3  ) (  2 3 4) (1   3 4)))
; ((1 1 3) (2 2 3))
; 3: (
;   ((1   3 4) (  2    ) (    3 4) (    3 4))
;   ((    3 4) (    3  ) (1      ) (  2    ))
;   ((  2    ) (      4) (    3  ) (1   3  ))
;   ((1   3  ) (1   3  ) (  2    ) (1   3 4)))
; ((0 1 2) (3 2 2))

(= (sudoku-n $N $B)
    (let* (
        ($N2 (* $N $N))
        ($bx (map-atom $B $r (map-atom $r $v (value-to-cell $N2 $v))))
        ($singletons (extract-singletons $bx))
        ($bxf (process-board-backtrack 1 (* $N2 $N2) $bx $N $singletons))
        ($bx2 (map-atom $bxf $rx (map-atom $rx $vx (cell-to-value $vx))))
        ($_ (map-atom $bx2 $x (println! $x)))
    )
    $bx2
    )
)

(= (sudoku $B) (sudoku-n 3 $B))

!(sudoku-n 2 ((x x x x) (x x 1 2) (2 4 x x) (x x x x)))

; !(sudoku-n 2 ((x x x x) (x x 1 2) (2 x x x) (x x x x)))

; !(sudoku ( (x x x x 2 x 3 5 x) (3 9 x 8 x x 7 6 x) (x x x x x x x x 9) (x x x 6 x 5 x 9 x) (5 8 x x x 4 x 2 6) (x x x 2 x 9 5 8 3) (4 x x 5 x 8 9 3 x) (x x 1 3 4 2 6 x 8) (6 x x 1 x 7 2 x 5) ))

;    (x x x x 2 x 3 5 x)
;    (3 9 x 8 x x 7 6 x)
;    (x x x x x x x x 9)
;    (x x x 6 x 5 x 9 x)
;    (5 8 x x x 4 x 2 6)
;    (x x x 2 x 9 5 8 3)
;    (4 x x 5 x 8 9 3 x)
;    (x x 1 3 4 2 6 x 8)
;    (6 x x 1 x 7 2 x 5)

; !(sudoku ( (3 x 8 5 x 4 9 x x) (9 x x x x x x 2 x) (x x x 1 x x x x x) (x 6 x x x x x x 7) (x x 5 9 x 8 2 x x) (x x x x 3 x x x x) (x x x x 1 x 4 x x) (x x 3 x 2 x x x x) (x 5 x 4 x 3 x 6 x) ))

;    (3 x 8 5 x 4 9 x x)
;    (9 x x x x x x 2 x)
;    (x x x 1 x x x x x)
;    (x 6 x x x x x x 7)
;    (x x 5 9 x 8 2 x x)
;    (x x x x 3 x x x x)
;    (x x x x 1 x 4 x x)
;    (x x 3 x 2 x x x x)
;    (x 5 x 4 x 3 x 6 x)

;    (3 2 8 5 6 4 9 7 1)
;    (9 x x 3 8 7 x 2 x)
;    (5 x x 1 9 2 x x x)
;    (x 6 x 2 5 1 x x 7)
;    (7 3 5 9 4 8 2 1 6)
;    (x x x 7 3 6 x x x)
;    (x x x x 1 x 4 x x)
;    (x x 3 x 2 x 7 x x)
;    (x 5 x 4 7 3 1 6 x))
