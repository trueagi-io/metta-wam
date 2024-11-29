(= (select $x) ((car-atom $x) (cdr-atom $x)))
(= (select $x) (let* (($y (car-atom $x)) ($z (cdr-atom $x)) (($u $v) (select $z))) ($u (cons-atom $y $v))))

(= (range $x $y)
    (if (== $x $y)
        ($x)
        (let $z (range (+ $x 1) $y)
            (cons-atom $x $z))))

(= (nqueens $n)
    (let $r (range 1 $n) (nqueens_aux $r ())))

(= (nqueens_aux $q $d)
  (let* (($ret (nqueens_aux_impl $q $d))
	; ($ (println! (format-args "!(assertEqualToResult (nqueens_aux {} {} {}) {})" ($q $d $ret))))
		  )
	  $ret))


(= (nqueens_aux_impl $unplaced $safe)
   (if (== $unplaced ())
        $safe
        (let ($q $r) (select $unplaced)
            (if (not_attack $q 1 $safe)
                (let $safeext (cons-atom $q $safe)
                (nqueens_aux_impl $r $safeext))))))
   

(= (not_attack $q $d $s)
  (let* (($ret (not_attack_impl $q $d $s))
	; ($ (println! (format-args "!(assertEqualToResult (not_attack {} {} {}) {})" ($q $d $s $ret))))
		  )
	  $ret))


(= (not_attack_impl $q $d $s)
    (if (== $s ())
        True
        (let* (($h (car-atom $s)) ($t (cdr-atom $s)))
            (if (or (== $q $h) (or (== $q (+ $d $h)) (== $h (+ $q $d)))) False
                (not_attack_impl $q (+ $d 1) $t)))))


;!(assertEqualToResult (nqueens_aux (6)) (4 2 5 3 1))

; Test select
!(assertEqualToResult (select (1 2 3 4)) ((1 (2 3 4)) (2 (1 3 4)) (3 (1 2 4)) (4 (1 2 3))))

!(set-debug! time True)

!(assertEqualToResult (not_attack 1 1 (5 3 6)) (False))
!(assertEqualToResult (not_attack 2 1 (5 3 6)) (True))
!(assertEqualToResult (not_attack 1 1 (2 5 3 6)) (False))
!(assertEqualToResult (not_attack 4 1 (2 5 3 6)) (True))

!(assertEqualToResult (not_attack 4 1 (3 1)) (False))
!(assertEqualToResult (not_attack 5 1 (3 1)) (True))


!(set-debug! time True)

!(assertEqualToResult (nqueens 4) ((3 1 4 2) (2 4 1 3)))

!(assertEqualToResult (nqueens 6) ((5 3 1 6 4 2)  (4 1 5 2 6 3)  (3 6 2 5 1 4)  (2 4 6 1 3 5)))

!(assertEqualToResult (nqueens 7) ((6 4 2 7 5 3 1)  (5 2 6 3 7 4 1)  (4 7 3 6 2 5 1)  (3 5 7 2 4 6 1)  (6 3 5 7 1 4 2)  (7 5 3 1 6 4 2)  (6 3 7 4 1 5 2)  (6 4 7 1 3 5 2)  (6 3 1 4 7 5 2)  (5 1 4 7 3 6 2)  (4 6 1 3 5 7 2)  (4 7 5 2 6 1 3)  (5 7 2 4 6 1 3)  (1 6 4 2 7 5 3)  (7 4 1 5 2 6 3)  (5 1 6 4 2 7 3)  (6 2 5 1 4 7 3)  (5 7 2 6 3 1 4)  (7 3 6 2 5 1 4)  (6 1 3 5 7 2 4)  (2 7 5 3 1 6 4)  (1 5 2 6 3 7 4)  (3 1 6 2 5 7 4)  (2 6 3 7 4 1 5)  (3 7 2 4 6 1 5)  (1 4 7 3 6 2 5)  (7 2 4 6 1 3 5)  (3 1 6 4 2 7 5)  (4 1 3 6 2 7 5)  (4 2 7 5 3 1 6)  (3 7 4 1 5 2 6)  (2 5 7 4 1 3 6)  (2 4 1 7 5 3 6)  (2 5 1 4 7 3 6)  (1 3 5 7 2 4 6)  (2 5 3 1 7 4 6)  (5 3 1 6 4 2 7)  (4 1 5 2 6 3 7)  (3 6 2 5 1 4 7)  (2 4 6 1 3 5 7)))

