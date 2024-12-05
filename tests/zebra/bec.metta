;; Basic event calculus axioms without constraints


; (= (tp-domain! $max-tp) (
;     (if (== $max-tp 0)
;         (add-atom &self (tp 0))
;         (let () (add-atom &self (tp $max-tp)) 
;             (tp-domain! (- $max-tp 1)))
;     )
; ))

((stoppedIn $t1 $f $t2) :- (
    (tp $t1) (tp $t2)
    (bool (< $t1 $t))
    (bool (< $t $t2))
    (terminates $e $f $t)
    (happens $e $t)
))

((stoppedIn $t1 $f $t2) :- (
    (tp $t1) (tp $t2)
    (bool (< $t1 $t))
    (bool (< $t $t2))
    (releases $e $f $t)
    (happens $e $t)
))

((holdsAt $f2 $t2) :- (
    (tp $t2) (tp $t1)
    (initiates $e $f1 $t1)
    (happens $e $t1)
    (naf (stoppedIn $t1 $f1 $t2))
))

((holdsAt $f $t) :- (
    (tp $t)
    (bool (< 0 $t))
    (initiallyP $f)
    (naf (stoppedIn 0 $f $t))
))

((-holdsAt $f $t) :- (
    (tp $t)
    (bool (< 0 $t))
    (initiallyN $f)
    (naf (startedIn 0 $f $t))
))

((holdsAt $f $t2) :- (
    (tp $t2) (tp $t1)
    (bool (< $t1 $t2))
    (initiates $e $f $t1)
    (happens $e $t1)
    (naf (stoppedIn $t1 $f $t2))
))

((-holdsAt $f $t2) :- (
    (tp $t1) (tp $t2)
    (bool (< $t1 $t2))
    (terminates $e $f $t1)
    (happens $e $t1)
    (naf (startedIn $t1 $f $t2))
))