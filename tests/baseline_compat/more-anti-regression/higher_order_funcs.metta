; Type definition for `curry` function
(: curry (-> (-> $a $b $c) (-> $a (-> $b $c))))
(= (((curry $f) $x) $y) ($f $x $y))

!(get-type ((curry +) 2))
!(get-type (((curry +) 2) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Curry with first application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions
(: curry-a (-> (-> $a $b $c) $a (-> $b $c)))
(= ((curry-a $f $a) $b) ($f $a $b))

!(get-type (curry-a + 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The same trick works for defining lambda
; (basically, `lambda` is curried `let`)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions
(: lambda (-> Atom $t (-> $a $t)))

(= ((lambda $var $body) $arg)
    (let $var $arg $body))

!((lambda $x (+ $x 1)) 2)

(: Maybe (-> $t Type))
(: Nothing (-> (Maybe $t)))
(: Something (-> $t (Maybe $t)))

(: Either (-> $t Type))
(: Left (-> $t (Either $t)))
(: Right (-> $t (Either $t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We can implement a generic `fmap`, but it requires
; concrete patterns in the type constructors above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: fmap (-> (-> $a $b) ($F $a) ($F $b)))
;Mapping over empty functor, returns empty functor
(= (fmap $f ($C0)) ($C0))

;Inductive case for singleton functor: apply $f to the
; element and rewrap in $C.
(= (fmap $f ($C $x))
    ($C ($f $x)))

;Inductive case for non-empty functor
(= (fmap $f ($C $x $xs))
    ($C ($f $x) (fmap $f $xs)))

!(fmap (curry-a + 2) (Something 5))

(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

(: fmap-i (-> (-> $a $b) ($F $a) ($F $b)))

(= (fmap-i $f Nil) Nil)

(= (fmap-i $f (Cons $x $xs))
    (Cons ($f $x) (fmap-i $f $xs)))

!(fmap-i (curry-a + 1) (Cons 6 (Cons 5 (Cons 4 Nil))))

;; foldl and foldr implementation

(: foldl (-> (-> $a $b $b) $b ($F $a) $b))
(: foldr (-> (-> $a $b $b) $b ($F $a) $b))

(= (foldl $f $a Nil) $a)
(= (foldl $f $a (Cons $x $xs))
    (foldl $f ($f $x $a) $xs))

(= (foldr $f $a Nil) $a)
(= (foldr $f $a (Cons $x $xs))
    ($f $x (foldr $f $a $xs)))

!(foldl - 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))) ; expected result 2
!(foldr - 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))) ; expected result -2

!(foldl Cons Nil (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))) ; reverses the list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; As per issue
;
; https://github.com/trueagi-io/hyperon-experimental/issues/104
;
; Test if adding type declaration to List data structure does
; not interfere with executing functions operating on List.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Insert an element in a presumably sorted list
(= (insert $x Nil) (Cons $x Nil))
(= (insert $x (Cons $head $tail))
    (case (< $x $head)
        ((True (Cons $x (Cons $head $tail)))
         (False (Cons $head (insert $x $tail))))))


; Sort a list
(= (sort Nil) Nil)
(= (sort (Cons $head $tail)) (insert $head (sort $tail)))

!(sort (Cons 3 (Cons 1 (Cons 2 Nil))))

(Older (Father $y) $y)

!(match &self (Older (Father $x) John) $x)