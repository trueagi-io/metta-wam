(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)

(: is-expression (-> Atom Bool))
(= (is-expression $x) (== (get-metatype $x) Expression))

(: is-closed (-> Atom Bool))
(= (is-closed $x) (if (is-variable $x)
                      False
                      (if (is-expression $x)
                          (lazy-or (let $head (car-atom $x) (is-closed $head))
                                   (let $tail (cdr-atom $x) (is-closed $tail)))
                          True)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Backward chaining Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
(: bc (-> $a Nat $space $a))
(= (bc (: $prf $ccln) $_ $kb) 
    (let () (println! (bc-base (: $prf $ccln)))
     (match $kb (: $prf $ccln) (: $prf $ccln))))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k) $kb)
    (let () (println! (bc-rec (: ($prfabs $prfarg) $ccln)))
     (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k $kb))
            ((: $prfarg $prms) (bc (: $prfarg $prms) $k $kb)))
          (: ($prfabs $prfarg) $ccln))))
;; System proof for ⍃
;; if conclusion equals (⍃ $X $Y), then return (: CPU (⍃ $X $Y)) 
;; if $x and $Y are fully grounded and (< $X $Y)
(= (bc (: $prf (⍃ $X $Y)) $_ $kb)
    (let () (println! (bc-⍃ (: $prf (⍃ $X $Y))))
     (if (and (and (is-closed $X) (is-closed $Y)) (< $X $Y))
          (: CPU (⍃ $X $Y))
          (empty))))
;; System proof for isDiff
(= (bc (: $prf (isDiff $X $Y)) $_ $kb)
    (let () (println! (bc-isDiff (: $prf (isDiff $X $Y))))
     (if (and (and (is-closed $X) (is-closed $Y)) (== $X $Y))
          (empty)
          (: CPUDiff (isDiff $X $Y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Forward chaining Curried  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
;; The source is assumed to be true
(: fc (-> $a Nat $space $a))
(= (fc (: $proof $premise) $_ $kb) (: $proof $premise))
;; Recursive steps
(= (fc (: $prfarg $premise) (S $k) $kb)
   (let (: $prfabs (-> $premise $ccln)) (bc (: $prfabs (-> $premise $ccln)) $k $kb)
     (fc (: ($prfabs $prfarg) $ccln) $k $kb)))
(= (fc (: $prfabs (-> $prms $ccln)) (S $k) $kb)
    (let (: $prfarg $prms) (bc (: $prfarg $prms) $k $kb)
     (fc (: ($prfabs $prfarg) $ccln) $k $kb)))

;;;;;;;;;;;;
;; Helper ;;
;;;;;;;;;;;;

;; Define ad-atom-nodup, that adds an atom only if it is not already
;; in the atomspace
(: add-atom-nodup (-> $st Atom ()))
(= (add-atom-nodup $space $atom)
   (case (match $space $atom $atom)
     (($atom ())
      (%void% (add-atom $space $atom)))))

;; Add all atoms from an expression to a given atomspace
(: add-atoms-nodup (-> $st Expression ()))
(= (add-atoms-nodup $space $atoms)
   (if (== $atoms ())
       ()
       (let* (($head (car-atom $atoms))
              ($tail (cdr-atom $atoms))
              ($dummy (add-atom-nodup $space $head)))
         (add-atoms-nodup $space $tail))))

(: add-minsup (-> $st Expression ()))
(= (add-minsup $kb $cres)
    (case $cres
      (((: $f (minsup $p)) (add-atom $kb $cres))
       ($_ ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative Chainer Wrapped Around Forward DTL Curried (collapse) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the forward revertant chainer iteratively, adding the
;; conclusions to the knowledge space after each iterations.  To avoid
;; irreproducible behavior (due to the side effects of modifying the
;; atomspace), each call of the forward revertant chainer collapses
;; between iterations.
(: ifc (-> $a                            ; Premise
           Nat                           ; Depth
           Nat                           ; Number of iterations
           $space                        ; space to write results
           $a))                          ; Conclusion

;; Base case.  For now it terminates at exactly iteration Z to avoid
;; collecting too many redundant results.
(= (ifc $prms $depth Z $kb) $prms)

;; Iterative step
(= (ifc $prms $depth (S $k) $kb)
   (let* (($res (fc $prms $depth $kb))
          ($cres (collapse $res))
          ($dummy (add-minsup $kb $res)))
     (ifc (superpose $cres) $depth $k $kb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative backward chainer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; base case
(: ibc (-> $a Nat Nat $space $a))
(= (ibc $prms $depth Z $kb) $prms)

;; Iterative step
(= (ibc $prms $depth (S $k) $kb)
   (let* (($res (bc $prms $depth $kb))
          ($cres (collapse $res))
          ($dummy (add-minsup $kb $res)))
     (ibc (superpose $cres) $depth $k)))
