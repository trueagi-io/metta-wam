;; Convert rule from curried format to uncurried format and back.  See
;; functions:
;; - curry-arrow, to convert from uncurried to curry format,
;; - uncurry-arrow, to convert from curried to uncurry format.

;; Define is-expression
(: is-expression (-> Atom Bool))
(= (is-expression $term) (== (get-metatype $term) Expression))

;; Define cons-cons-atom, that applies const-atom twice
(: cons-cons-atom (-> Atom Atom Atom Atom))
(= (cons-cons-atom $head1 $head2 $tail)
   (let $head2-tail (cons-atom $head2 $tail)
     (cons-atom $head1 $head2-tail)))

;; Given a tuple of arguments and a binary operator, unflattened in a
;; right-associative way the tuple of arguments by inserting the
;; operator.  The unflattening does not propagate recursively to
;; sub-expressions.
;;
;; For instance
;;
;; (right-associate-with (T1 T2 (T3 A B)) ->)
;;
;; outputs
;;
;; (-> T1 (-> T2 (T3 A B)))
(: right-associate-with (-> Expression Atom Atom))
(= (right-associate-with $expr $op)
   (if (== $expr ())
       ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr)))
         (if (== $tail ())
             $head
             (let $ra-tail (right-associate-with $tail $op)
               (cons-cons-atom $op $head ($ra-tail)))))))

;; Test right-associate-with
!(assertEqual
  (right-associate-with () ->)
  ())
!(assertEqual
  (right-associate-with (T) ->)
  T)
!(assertEqual
  (right-associate-with (T1 T2) ->)
  (-> T1 T2))
!(assertEqual
  (right-associate-with (T1 T2 T3) ->)
  (-> T1 (-> T2 T3)))
!(assertEqual
  (right-associate-with (T1 T2 (T3 A B)) ->)
  (-> T1 (-> T2 (T3 A B))))

;; Convert rule from uncurried format to curried format.
;;
;; For instance
;;
;; (curry-arrow (: Trans (-> (=== $x $y) (=== $y $z) (=== $x $z))))
;;
;; outputs
;;
;; (: Trans (-> (=== $x $y) (-> (=== $y $z) (=== $x $z))))
;;
;; Note that the currying only occurs at the most shallow arrow
;; operator, nowhere else.  For instance
;;
;; (curry-arrow (: MP3 (-> (-> $x $y $z) $x $y $z)))
;;
;; outputs
;;
;; (: MP3 (-> (-> $x $y $z) (-> $x (-> $y $z))))
;;
;; not
;;
;; (: MP3 (-> (-> $x (-> $y $z)) (-> $x (-> $y $z))))
;;
;; That is because we want to change the rule format without affecting
;; the rule content.
(: curry-arrow (-> Atom Atom))
(= (curry-arrow $term)
   (if (is-expression $term)
       (if (== $term ())
           ()
           (let* (($head (car-atom $term))
                  ($tail (cdr-atom $term)))
             (if (== $head ->)
                 (right-associate-with $tail $head)
                 (let* (($ca-head (curry-arrow $head))
                        ($ca-tail (curry-arrow $tail)))
                   (cons-atom $ca-head $ca-tail)))))
       $term))

;; Test curry-arrow
!(assertEqual
  (curry-arrow T)
  T)
!(assertEqual
  (curry-arrow ())
  ())
!(assertEqual
  (curry-arrow (T))
  (T))
!(assertEqual
  (curry-arrow (R A B))
  (R A B))
!(assertEqual
  (curry-arrow ((R A B)))
  ((R A B)))
!(assertEqual
  (curry-arrow (-> T1 T2))
  (-> T1 T2))
!(assertEqual
  (curry-arrow (-> T1 T2 T3))
  (-> T1 (-> T2 T3)))
!(assertEqual
  (curry-arrow ((-> T1 T2 T3)))
  ((-> T1 (-> T2 T3))))
!(assertEqual
  (curry-arrow (R (-> T1 T2 T3)))
  (R (-> T1 (-> T2 T3))))
!(assertEqual
  (curry-arrow (: R (-> T1 T2 T3)))
  (: R (-> T1 (-> T2 T3))))
!(assertEqual
  (curry-arrow (: MP3 (-> (-> $x $y $z) $x $y $z)))
  (: MP3 (-> (-> $x $y $z) (-> $x (-> $y $z)))))
!(assertEqual
  (curry-arrow (: Trans (-> (=== $x $y) (=== $y $z) (=== $x $z))))
  (: Trans (-> (=== $x $y) (-> (=== $y $z) (=== $x $z)))))

;; Given a pair of arguments and a binary operator, flattened the
;; right argument if it contains the binary operator.  The
;; unflattening does not propagate recursively to left, only to the
;; right and only to the operator right below, and so on.
;;
;; For instance
;;
;; (right-dissociate-with (-> T1 (-> T2 (T3 A B))) ->)
;;
;; outputs
;;
;; (-> T1 T2 (T3 A B))
(: right-dissociate-with (-> Atom Atom Atom))
(= (right-dissociate-with $term $op)
   (if (is-expression $term)
       (if (== $term ())
           ()
           (case $term
             ((($op $larg $rarg) (let $rd-rarg (right-dissociate-with $rarg $op)
                                   (if (is-expression $rd-rarg)
                                       (let* (($head (car-atom $rd-rarg))
                                              ($tail (cdr-atom $rd-rarg)))
                                         (if (== $head $op)
                                             (cons-cons-atom $op $larg $tail)
                                             (cons-cons-atom $op $larg ($rd-rarg))))
                                       (cons-cons-atom $op $larg ($rd-rarg)))))
              ($_ $term))))
       $term))

;; Test right-associate-with
!(assertEqual
  (right-dissociate-with T ->)
  T)
!(assertEqual
  (right-dissociate-with () ->)
  ())
!(assertEqual
  (right-dissociate-with (T) ->)
  (T))
!(assertEqual
  (right-dissociate-with (-> T1 T2) ->)
  (-> T1 T2))
!(assertEqual
  (right-dissociate-with (-> T1 (T2 A B)) ->)
  (-> T1 (T2 A B)))
!(assertEqual
  (right-dissociate-with (-> T1 (-> T2 T3)) ->)
  (-> T1 T2 T3))
!(assertEqual
  (right-dissociate-with (-> T1 (-> T2 (T3 A B))) ->)
  (-> T1 T2 (T3 A B)))

;; Convert rule from curried format to uncurried format.
;;
;; For instance
;;
;; (uncurry-arrow (: Trans (-> (=== $x $y) (-> (=== $y $z) (=== $x $z)))))
;;
;; outputs
;;
;; (: Trans (-> (=== $x $y) (=== $y $z) (=== $x $z)))
;;
;; Note that the uncurrying only occurs on the arrow operator, nowhere
;; else.
(: uncurry-arrow (-> Atom Atom))
(= (uncurry-arrow $term)
   (if (is-expression $term)
       (if (== $term ())
           ()
           (let* (($head (car-atom $term))
                  ($tail (cdr-atom $term)))
             (if (== $head ->)
                 (right-dissociate-with $term $head)
                 (let* (($ca-head (uncurry-arrow $head))
                        ($ca-tail (uncurry-arrow $tail)))
                   (cons-atom $ca-head $ca-tail)))))
       $term))

;; Test uncurry-arrow
!(assertEqual
  (uncurry-arrow T)
  T)
!(assertEqual
  (uncurry-arrow ())
  ())
!(assertEqual
  (uncurry-arrow (T))
  (T))
!(assertEqual
  (uncurry-arrow (R A B))
  (R A B))
!(assertEqual
  (uncurry-arrow ((R A B)))
  ((R A B)))
!(assertEqual
  (uncurry-arrow (-> T1 T2))
  (-> T1 T2))
!(assertEqual
  (uncurry-arrow (-> T1 (-> T2 T3)))
  (-> T1 T2 T3))
!(assertEqual
  (uncurry-arrow ((-> T1 (-> T2 T3))))
  ((-> T1 T2 T3)))
!(assertEqual
  (uncurry-arrow (R (-> T1 (-> T2 T3))))
  (R (-> T1 T2 T3)))
!(assertEqual
  (uncurry-arrow (: R (-> T1 (-> T2 T3))))
  (: R (-> T1 T2 T3)))
!(assertEqual
  (uncurry-arrow (: MP3 (-> (-> $x $y $z) (-> $x (-> $y $z)))))
  (: MP3 (-> (-> $x $y $z) $x $y $z)))
!(assertEqual
  (uncurry-arrow (: Trans (-> (=== $x $y) (-> (=== $y $z) (=== $x $z)))))
  (: Trans (-> (=== $x $y) (=== $y $z) (=== $x $z))))
