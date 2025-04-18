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
    ($c3 $c4 $d3 $d4)))

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
        ($singsorted (sort-singleton-list $N2 $singletons))
        ($board0 (process-board-one-dimension $board 0 $N2 $singsorted $changed0 $newsing0))
        ($board0t (transpose-board $board0))
        ($singletonst (map-atom $singletons $v0 (transpose-coordinate $v0)))
        ($singletonstsorted (sort-singleton-list $N2 $singletonst))
        ($board1t (process-board-one-dimension $board0t 0 $N2 $singletonstsorted $changed1 $newsing1t))
        ($board1 (transpose-board $board1t))
        ($newsing1 (map-atom $newsing1t $v1 (transpose-coordinate $v1)))
        ($board1u (transpose-cell-board $board1))
        ($singletonsu (map-atom $singletons $v2 (transpose-cell-coordinate $N $v2)))
        ($singletonsusorted (sort-singleton-list $N2 $singletonsu))
        ($board2u (process-board-one-dimension $board1u 0 $N2 $singletonsusorted $changed2 $newsing2u))
        ($board2 (transpose-cell-board $board2u))
        ($newsing2 (map-atom $newsing2u $v3 (transpose-cell-coordinate $N $v3)))
        ($changed (or $changed0 (or $changed1 $changed2)))
        ($newsing (append $newsing0 (append $newsing1 $newsing2)))
        )
        $board2
    )
)

!(let $c (process-board-all-dimensions (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1) (2)) ((2) (4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) 2 ((1 3 2) (1 2 1) (2 1 4) (2 0 2)) $changed $newsing) ($changed $c $newsing))

(= (process-board-all-dimensions-fixed $board $N $singletons)
    (let $board2 (process-board-all-dimensions $board $N $singletons $changed $newsing)
        (if $changed
            (process-board-all-dimensions-fixed $board2 $N $newsing)
            $board2
        )
    )
)

; !(process-board-all-dimensions-fixed (((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1) (2)) ((2) (4) (1 2 3 4) (1 2 3 4)) ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))) 2 ((1 3 2) (1 2 1) (2 1 4) (2 0 2)))

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
        ($bxf (process-board-all-dimensions-fixed $bx $N $singletons))
        ($bx2 (map-atom $bxf $rx (map-atom $rx $vx (cell-to-value $vx))))
    )
    $bx2
    )
)

!(time (sudoku-n 2 ( (x x x x) (x x 1 2) (2 4 x x) (x x x x) )))

