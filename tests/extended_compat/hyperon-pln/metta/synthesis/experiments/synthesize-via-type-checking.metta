;; Import modules
!(import! &self ../../common/Num.metta)

;; ;; Type definitions checker
;; !(pragma! type-check auto)

;; ;; Equality type definition
;; (: = (-> $t $t Type))

;; Define an heterogeneous list.
(: List Type)
(: Nil List)
(: Cons (-> $a List List))

;; Define map, a map implementation operating on List
(: map (-> (-> $a $b) List List))
(= (map $fn Nil) Nil)
(= (map $fn (Cons $head $tail)) (Cons ($fn $head) (map $fn $tail)))

;; Define filter, a filter implementation operating on List
(: filter (-> (-> $a Bool) List List))
(= (filter $prd Nil) Nil)
(= (filter $prd (Cons $head $tail)) (if ($prd $head)
                                        (Cons $head (filter $prd $tail))
                                        (filter $prd $tail)))

;; Define lambda (essentially lambda is curried let)
(: lambda (-> $a $t (-> $a $t)))
(= ((lambda $var $body) $arg) (let $var $arg $body))

;; Define curry.  Given a binary function, and its first argument,
;; return a unary function corresponding to the partial evaluation of
;; the binary function over the first argument.
(: curry (-> (-> $a $b $c) $a (-> $b $c)))
(= ((curry $f $x) $y) ($f $x $y))

;; Interleave two lists
(: interleave (-> List List List))
(= (interleave Nil $ys) $ys)
(= (interleave (Cons $head $tail) $ys) (Cons $head (interleave $ys $tail)))

;; Cartesian product with a given operator on List
(: prodWith (-> (-> $a $b $c) List List List))
(= (prodWith $op Nil Nil) Nil)
(= (prodWith $op Nil (Cons $head $tail)) Nil)
(= (prodWith $op (Cons $head $tail) Nil) Nil)
(= (prodWith $op (Cons $xhd $xtl) (Cons $yhd $ytl))
   (Cons ($op $xhd $yhd) (interleave (map (curry $op $xhd) $ytl)
                                     (prodWith $op $xtl (Cons $yhd $ytl)))))

;; Define composition operator
(: . (-> (-> $b $c) (-> $a $d) (-> $a $c)))
(= ((. $g $f) $x) ($g ($f $x)))

;; Define well-typed.  Returns True iff its argument is well typed.
(: well-typed (-> Atom Bool))
(= (well-typed $x) (case $x
                     (((Error $v $m) False)
                      ($_ True))))

;; Program synthesizer.  It takes a list of functions, a depth, and
;; returns a list of programs composing these functions up to the
;; given depth.
(: synthesize (-> List Nat List))    ; TODO: add type signature as argument
(= (synthesize $fns Z) (trace! (synthesize= $fns Z Nil) Nil))
(= (synthesize $fns (S Z)) (trace! (synthesize= $fns (S Z) $fns) $fns))
(= (synthesize $fns (S (S $k))) (let* (($prgs (synthesize $fns (S $k)))
                                       ($cmbs (prodWith . $fns $prgs))
                                       ($vld (filter well-typed $cmbs)))
                                  ; TODO: should be concatenated with $prgs?
                                  (trace! (info (variables (prgs $prgs) (cmbs $cmbs))
                                                (synthesize= $fns (S (S $k)) $vld))
                                          $vld)))

;;;;;;;;;;;;;;;;
;; Unit tests ;;
;;;;;;;;;;;;;;;;
!("Unit Tests")

;; Test map
(: inc (-> Number Number))
(= (inc $n) (+ $n 1))
!(map inc (Cons 2 (Cons 3 (Cons 1 Nil))))  ; (Cons 3 (Cons 4 (Cons 2 Nil)))

;; Test filter
(: even (-> Number Bool))
(= (even $n) (== (* (ceil (/ $n 2)) 2) $n))
!(filter even (Cons 2 (Cons 3 (Cons 1 Nil))))  ; (Cons 2 Nil)

;; Test interleave
!(interleave Nil Nil)  ; Nil
!(interleave (Cons 1 (Cons 2 Nil)) (Cons "a" (Cons "b" Nil)))  ; (Cons 1 (Cons "a" (Cons 2 (Cons "b" Nil))))

;; Test prodWith
!(prodWith * Nil (Cons 2 Nil))  ; Nil
!(prodWith * (Cons 1 Nil) Nil)  ; Nil
!(prodWith * (Cons 1 Nil) (Cons 2 Nil))  ; (Cons 2 Nil)
!(prodWith * (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)))  ; (Cons 3 (Cons 4 (Cons 6 (Cons 8 Nil))))

;; Test synthesize

;; Define collection of functions to compose
(: f (-> Number String))
(= (f $x) (if (== $x 42) "42" "Not-42"))

(: g (-> String Bool))
(= (g $x) (== $x "42"))

(: h (-> Bool Atom))
(= (h $x) (if $x TrueAtom FalseAtom))

(: functions (-> List))
(= (functions) (Cons f (Cons g Nil)))
;; Uncomment to try `synthesize` with more functions
;; (= (functions) (Cons f (Cons g (Cons h Nil))))

!(prodWith . (Cons g Nil) (Cons f Nil))  ; (Cons (. g f) Nil)
!(prodWith . (Cons f Nil) (Cons g Nil))  ; (Cons (. f g) Nil)
!(prodWith . (Cons f (Cons g Nil)) (Cons f (Cons g Nil)))  ; (Cons (. f f) (Cons (. f g) (Cons (. g f) (Cons (. g g) Nil))))
!(synthesize (functions) (fromNumber 2))  ; (Cons (. f f) (Cons (. f g) (Cons (. g f) (Cons (. g g) Nil))))
