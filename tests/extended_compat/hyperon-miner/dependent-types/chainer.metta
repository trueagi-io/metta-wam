;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Backward chaining Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_) (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Forward chaining Curried  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
;; The source is assumed to be true
(: fc (-> $a Nat $a))
(= (fc (: $proof $premise) $_) (: $proof $premise))
;; Recursive steps
(= (fc (: $prfarg $premise) (S $k))
   (let (: $prfabs (-> $premise $ccln)) (bc (: $prfabs (-> $premise $ccln)) $k)
     (fc (: ($prfabs $prfarg) $ccln) $k)))
(= (fc (: $prfabs (-> $prms $ccln)) (S $k))
    (let (: $prfarg $prms) (bc (: $prfarg $prms) $k)
     (fc (: ($prfabs $prfarg) $ccln) $k)))

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
           $a))                          ; Conclusion

;; Base case.  For now it terminates at exactly iteration Z to avoid
;; collecting too many redundant results.
(= (ifc $prms $depth Z) $prms)

;; Iterative step
(= (ifc $prms $depth (S $k))
   (let* (($cres (collapse (fc $prms $depth)))
          ($dummy (add-atoms-nodup &kb $cres)))
     (ifc (superpose $cres) $depth $k)))
