;; Implement functions to convert terms between MeTTa, lambda calculus
;; and combinatory logic.
;;
;; For the lambda calculus <-> combinatory logic part, the code is
;; inspired from the paper:
;;
;; A correct-by-construction conversion from lambda calculus to
;; combinatory logic, by Wouter Swierstra.
;;
;; As in the paper, the version of combinatory logic in consideration
;; is S, K, I.  In order not to have S, the combinator, be confused
;; with with S, the successor, we use the unicode characters 𝐒, 𝐊 and
;; 𝐈, as well as other similar unicode characters for more
;; combinators.  Also all functions are explicitly curried, thus 𝐊 x y
;; is represented in MeTTa as ((𝐊 x) y).
;;
;; On the lambda-calculus side, the MeTTa representation of a lambda
;; abstraction is (λ <VAR> <BODY>).  Applications are explicitly
;; curried as well, thus (x y z) is represented ((x y) z).
;;
;; An excellent source information about combinatory logic can be
;; found in this blog series
;;
;; https://farrugiamaths.quora.com/Combinatory-logic-Using-math-boldsymbol-mathsf-S-math-and-math-boldsymbol-mathsf-K-math-Part-1
;;
;; by Alexander Farrugia
;;
;; To test if our reduction rules and convertion algorithms between
;; lambda calculus and combinatory logic are correct we use arithmetic.
;;
;; For arithmetic in combinatory logic, see
;;
;; https://farrugiamaths.quora.com/Combinatory-logic-Natural-numbers-and-predicates-Part-6
;;
;; For arithmetic in lambda calculus, see
;;
;; https://en.wikipedia.org/wiki/Lambda_calculus#Arithmetic_in_lambda_calculus

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common functions and types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define Maybe type
(: Maybe (-> $a Type))
(: Nothing (Maybe $a))
(: Just (-> $a (Maybe $a)))

;; Define list type
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;; Define is-expression, a function that returns True iff the input is
;; a MeTTa expression, meaning it is not a symbol or a variable.
(: is-expression (-> Atom Bool))
(= (is-expression $x) (== (get-metatype $x) Expression))

;; Test is-expression
!(assertEqual (is-expression $x) False)
!(assertEqual (is-expression A) False)
!(assertEqual (is-expression (A B)) True)

;; Define is-variable, a function that returns True iff its input is a
;; MeTTa variable.
(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

;; Test is-variable
!(assertEqual (is-variable $x) True)
!(assertEqual (is-variable A) False)
!(assertEqual (is-variable (A $x)) False)

;; Define is-symbol, a function that returns True iff its input is a
;; MeTTa symbol.
(: is-symbol (-> Atom Bool))
(= (is-symbol $x) (== (get-metatype $x) Symbol))

;; Test is-symbol
!(assertEqual (is-symbol $x) False)
!(assertEqual (is-symbol A) True)
!(assertEqual (is-symbol (A $x)) False)

;; Till lazy or and and gets into the stdlib
(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)
(: lazy-and (-> Bool Atom Bool))
(= (lazy-and False $x) False)
(= (lazy-and True $x) $x)

;; Return True iff the first argument is a subterm of the second
;; argument.
;;
;; For instance
;;
;; (is-subterm-of A A) returns True
;; (is-subterm-of A (A B)) returns True
;; (is-subterm-of A B) returns False
;; (is-subterm-of A (B C)) returns False
;;
;; It should handle variables properly (that is assume that variables
;; with different names are different terms), for instance
;;
;; (is-subterm-of $x $x) returns True
;; (is-subterm-of $x ($x $y)) returns True
;; (is-subterm-of $x $y) returns False
;; (is-subterm-of $x ($y $z)) return False
;;
;; Terms can of course contain both symbols and variables, for
;; instance
;;
;; (is-subterm-of A ($x A)) returns True
;; (is-subterm-of $x ($x A)) returns True
;; (is-subterm-of A ($x B)) returns False
;; (is-subterm-of $x ($y A)) returns False
;;
;; It should of course detect a subterm when it is buried deeper
;; inside the superterm, for instance
;;
;; (is-subterm-of A (C (B A))) returns True
;; (is-subterm-of A (B (C D))) returns False
(: is-subterm-of (-> Atom Atom Bool))
(= (is-subterm-of $x $y)
   ;; Base cases
   (if (== $x $y)
       True
       (if (== $y ())
           False
           ;; Recursive step
           (if (is-expression $y)
               (lazy-or (let $head (car-atom $y) (is-subterm-of $x $head))
                        (let $tail (cdr-atom $y) (is-subterm-of $x $tail)))
               ;; Final base case
               False))))

;; Test is-subterm-of
!(assertEqual (is-subterm-of A A) True)
!(assertEqual (is-subterm-of A ()) False)
!(assertEqual (is-subterm-of A (A B)) True)
!(assertEqual (is-subterm-of A B) False)
!(assertEqual (is-subterm-of A (B C)) False)
!(assertEqual (is-subterm-of $x $x) True)
!(assertEqual (is-subterm-of $x ($x $y)) True)
!(assertEqual (is-subterm-of $x $y) False)
!(assertEqual (is-subterm-of $x ($y $z)) False)
!(assertEqual (is-subterm-of A ($x A)) True)
!(assertEqual (is-subterm-of $x ($x A)) True)
!(assertEqual (is-subterm-of A ($x B)) False)
!(assertEqual (is-subterm-of $x ($y A)) False)
!(assertEqual (is-subterm-of A (C (B A))) True)
!(assertEqual (is-subterm-of A (B (C D))) False)

;; Return True iff the expression is a tuple of variables
(: all-variables (-> Expression Bool))
(= (all-variables $x) (if (== $x ())
                             True
                             (lazy-and (let $head (car-atom $x) (is-variable $head))
                                  (let $tail (cdr-atom $x) (all-variables $tail)))))

;; Return True iff the expression is a tuple of unique variables.
;; Note that it only supports pairs and triples because that is all we
;; need here.
(: uniq-vars (-> Expression Bool))
(= (uniq-vars ($x $y)) (lazy-and (all-variables ($x $y)) (not (== $x $y))))
(= (uniq-vars ($x $y $z)) (lazy-and (all-variables ($x $y $z))
                               (lazy-and (not (== $x $y))
                                    (lazy-and (not (== $y $z))
                                         (not (== $x $z))))))

;; Define α-Binding type, that represents a binding from a variable to
;; another variable.
(: α-Binding Type)
(: ↔α (-> Variable Variable α-Binding))

;; Insert an α-binding in a list of α-bindings.  Returns maybe the new
;; list of α-bindings if the insertion succeeded, that is the
;; α-binding is consistent with the list, or entirely missing from it.
(: α-insert (-> α-Binding (List α-Binding) (Maybe (List α-Binding))))
;; Base case
(= (α-insert (↔α $x $y) Nil) (Just (Cons (↔α $x $y) Nil)))
;; Recursive step
(= (α-insert (↔α $x $y) (Cons (↔α $z $w) $tail))
   (if (== $x $z)
       (if (== $y $w)
           (Just (Cons (↔α $z $w) $tail))
           Nothing)
       (if (== $y $w)
           Nothing
           (case (α-insert (↔α $x $y) $tail)
             ((Nothing Nothing)
              ((Just $νtl) (Just (Cons (↔α $z $w) $νtl))))))))

;; Test α-insert
!(assertEqual
  (α-insert (↔α $x $y) Nil)
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-insert (↔α $x $y) (Cons (↔α $x $y) Nil))
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-insert (↔α $x $y) (Cons (↔α $x $z) Nil))
  Nothing)
!(assertEqual
  (α-insert (↔α $x $y) (Cons (↔α $z $w) Nil))
  (Just (Cons (↔α $z $w) (Cons (↔α $x $y) Nil))))

;; Join two α-bindings to produce an α-bindings consisting of the
;; union of these.  If the two α-bindings provided in input are
;; inconsistent, or one of them in Nothing, then return Nothing.
(: α-join (-> (Maybe (List α-Binding))
              (Maybe (List α-Binding))
              (Maybe (List α-Binding))))
;; Base cases
(= (α-join Nothing Nothing) Nothing)
(= (α-join Nothing (Just $_)) Nothing)
(= (α-join (Just $_) Nothing) Nothing)
(= (α-join (Just Nil) (Just $other)) (Just $other))
;; Recursive step
(= (α-join (Just (Cons $head $tail)) (Just $other))
   (α-join (Just $tail) (α-insert $head $other)))

;; Test α-join
!(assertEqual
  (α-join Nothing Nothing)
  Nothing)
!(assertEqual
  (α-join (Just Nil) (Just Nil))
  (Just Nil))
!(assertEqual
  (α-join (Just Nil) (Just (Cons (↔α $x $y) Nil)))
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-join (Just (Cons (↔α $x $y) Nil)) (Just Nil))
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-join (Just Nil) (Just (Cons (↔α $x $y) Nil)))
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-join (Just (Cons (↔α $x $y) Nil)) (Just (Cons (↔α $x $z) Nil)))
  Nothing)
!(assertEqual
  (α-join (Just (Cons (↔α $x $y) Nil)) (Just (Cons (↔α $x $y) Nil)))
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-join (Just (Cons (↔α $x $y) Nil)) (Just (Cons (↔α $z $w) Nil)))
  (Just (Cons (↔α $z $w) (Cons (↔α $x $y) Nil))))

;; Given two terms, $lhs and $rhs, return maybe a list of α-bindings
;; so that if $lhs is α-equivalent to $rhs, substituting all variables
;; in $rhs according to the α-bindings would result in a term that is
;; equal to $lhs.  If $lhs and $rhs are not α-equivalent then return
;; Nothing.
(: α-bindings (-> Atom Atom (Maybe (List α-Binding))))
(= (α-bindings $lhs $rhs)
   (case (get-metatype $lhs)
     ((Symbol (if (== $lhs $rhs)
                  (Just Nil)
                  Nothing))
      (Grounded (if (== $lhs $rhs)
                    (Just Nil)
                    Nothing))
      (Variable (if (is-variable $rhs)
                    (Just (Cons (↔α $lhs $rhs) Nil))
                    Nothing))
      (Expression (if (== $lhs ())
                      ;; $lhs is ()
                      (if (== $rhs ())
                          (Just Nil)
                          Nothing)
                      ;; $lhs is not ()
                      (if (== $rhs ())
                          Nothing
                          (case (get-metatype $rhs)
                            ((Symbol Nothing)
                             (Grounded Nothing)
                             (Variable Nothing)
                             (Expression
                              (let* (($hd-lhs (car-atom $lhs))
                                     ($tl-lhs (cdr-atom $lhs))
                                     ($hd-rhs (car-atom $rhs))
                                     ($tl-rhs (cdr-atom $rhs))
                                     ($hd-α-bs (α-bindings $hd-lhs $hd-rhs))
                                     ($tl-α-bs (α-bindings $tl-lhs $tl-rhs)))
                                (α-join $hd-α-bs $tl-α-bs)))))))))))

;; Test α-bindings
!(assertEqual
  (α-bindings A B)
  Nothing)
!(assertEqual
  (α-bindings $x B)
  Nothing)
!(assertEqual
  (α-bindings () ())
  (Just Nil))
!(assertEqual
  (α-bindings (R $x) $y)
  Nothing)
!(assertEqual
  (α-bindings $x $y)
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-bindings (R $x) (R $y))
  (Just (Cons (↔α $x $y) Nil)))
!(assertEqual
  (α-bindings ((R $x) A) ((R $y) $z))
  Nothing)
!(assertEqual
  (α-bindings ((R $x) $x) ((R $z) $w))
  Nothing)
!(assertEqual
  (α-bindings ((R $x) $y) ((R $z) $z))
  Nothing)
!(assertEqual
  (α-bindings (R $x (R $y)) (R $z $z))
  Nothing)
!(assertEqual
  (α-bindings ((R $x) $y) ((R $z) $w))
  (Just (Cons (↔α $y $w) (Cons (↔α $x $z) Nil))))

;; Return True iff two given terms are alpha-equivalent.  For now all
;; variables are assumed to be free, the notion of scope is not
;; implemented.
(: =α (-> $a $b Bool))
(= (=α $lhs $rhs) (case (α-bindings $lhs $rhs)
                    ((Nothing False)
                     ((Just $_) True))))

;; Test =α
!(assertEqual (=α $x $x) True)
!(assertEqual (=α $x $y) True)
!(assertEqual (=α A B) False)
!(assertEqual (=α A $y) False)
!(assertEqual (=α (R $x) (R $y)) True)
!(assertEqual (=α (R $x $x) (R $y $z)) False)
!(assertEqual (=α (R $x (R $y)) (R $z $z)) False)
!(assertEqual (=α (λ $x (λ $y $x)) (λ $z (λ $w $z))) True)

;;;;;;;;;;;;;;;;;;;;;
;; Reduction rules ;;
;;;;;;;;;;;;;;;;;;;;;

;; ;; Reduce lambda application
;; (= ((λ $x $f) $y) (let ($νx $νf) (sealed ($x) ($x $f)) (let $νx $y $νf)))

;; ;; Test lambda calculus reduction
;; !(((λ $f378 (λ $g379 (λ $x380 (($f378 $x380) ($g379 $x380))))) (((λ $f448 (λ $g449 (λ $x450 (($f448 $x450) ($g449 $x450))))) ((λ $x470 (λ $y471 $x470)) (λ $f482 (λ $g483 (λ $x484 (($f482 $x484) ($g483 $x484))))))) (((λ $f543 (λ $g544 (λ $x545 (($f543 $x545) ($g544 $x545))))) ((λ $x564 (λ $y565 $x564)) (λ $x570 (λ $y571 $x570)))) (λ $x579 $x579)))) ((λ $x604 (λ $y605 $x604)) (λ $x610 $x610)))
;; ;; Should reduce to (λ $f (λ $x ($f $x)))

;; Reduction rules for combinatory logic.  In the process, we
;; introduce combinators 𝐌, 𝐍, 𝐓, 𝐖, 𝐁, 𝐂 and 𝐔.
(= (𝐈 $x) $x)
(= ((𝐊 $x) $y) $x)
(= (((𝐒 $x) $y) $z) (($x $z) ($y $z)))
(= (𝐌 $x) ($x $x))
(= ((𝐍 $x) $y) $y)
(= ((𝐓 $x) $y) ($y $x))
(= ((𝐖 $x) $y) (($x $y) $y))
(= (((𝐁 $x) $y) $z) ($x ($y $z)))
(= (((𝐂 $x) $y) $z) (($x $z) $y))
(= ((𝐔 $x) $y) ($y (($x $x) $y)))
(= ((𝐒 𝐊) 𝐊) 𝐈)
(= ((𝐒 𝐈) 𝐈) 𝐌)
(= (𝐊 𝐈) 𝐍)
(= ((𝐒 (𝐊 (𝐒 𝐈))) 𝐊) 𝐓)
(= ((𝐒 𝐒) 𝐍) 𝐖)
(= ((𝐒 (𝐊 𝐒)) 𝐊) 𝐁)
(= ((𝐒 (𝐁 𝐁 𝐒)) (𝐊 𝐊)) 𝐂)
(= ((𝐁 (𝐒 𝐈)) (𝐒 𝐈 𝐈)) 𝐔)

;; Test combinatory logic reduction
!(assertEqual
  (𝐊 𝐈)                                 ; 0
  𝐍)
!(assertEqual
  ((𝐒 (𝐊 𝐒)) 𝐊)
  𝐁)
!(assertEqual
  ((𝐒 𝐒) (𝐊 𝐈))
  𝐖)
!(assertEqual
  ((𝐒 𝐒) 𝐍)
  𝐖)
!(assertEqual
  (𝐒 ((𝐒 (𝐊 𝐒)) 𝐊))                    ; Successor
  (𝐒 𝐁))
!(assertEqual
  ((𝐒 ((𝐒 (𝐊 𝐒)) 𝐊)) (𝐊 𝐈))            ; 1, as (Successor 0)
  ((𝐒 𝐁) 𝐍))
!(assertEqual
  ((𝐒 ((𝐒 (𝐊 𝐒)) 𝐊)) ((𝐒 ((𝐒 (𝐊 𝐒)) 𝐊)) (𝐊 𝐈))) ; 2, as (Successor (Successor 0))
  ((𝐒 𝐁) ((𝐒 𝐁) 𝐍)))
!(assertEqual
  (((𝐒 (𝐊 (𝐒 𝐈))) 𝐊) (𝐒 ((𝐒 (𝐊 𝐒)) 𝐊))) ; Plus
  (𝐓 (𝐒 𝐁)))
!(assertEqual
  (((((𝐒 (𝐊 (𝐒 𝐈))) 𝐊) (𝐒 ((𝐒 (𝐊 𝐒)) 𝐊))) (𝐊 𝐈)) (𝐊 𝐈)) ; 0, as (Plus 0 0)
  𝐍)
!(assertEqual
  (((((𝐒 (𝐊 (𝐒 𝐈))) 𝐊) (𝐒 ((𝐒 (𝐊 𝐒)) 𝐊))) ((𝐒 ((𝐒 (𝐊 𝐒)) 𝐊)) (𝐊 𝐈)))
   ((𝐒 ((𝐒 (𝐊 𝐒)) 𝐊)) ((𝐒 ((𝐒 (𝐊 𝐒)) 𝐊)) (𝐊 𝐈)))) ; 3, as (Plus 1 2)
  ((𝐒 𝐁) ((𝐒 𝐁) ((𝐒 𝐁) 𝐍))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combinatory logic to λ-calculus ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert combinatory logic to lambda-calculus
(: cl2lc (-> $a $a))
;; Base cases
(= (cl2lc $term)
   (case (get-metatype $term)
     ((Symbol (case $term
                ((𝐈 (λ $x $x))
                 (𝐊 (λ $x (λ $y $x)))
                 (𝐒 (λ $f (λ $g (λ $x (($f $x) ($g $x))))))
                 (𝐌 (λ $x ($x $x)))
                 (𝐍 (λ $x (λ $y $y)))
                 (𝐓 (λ $x (λ $y ($y $x))))
                 (𝐖 (λ $x (λ $y (($x $y) $y))))
                 (𝐁 (λ $x (λ $y (λ $z ($x ($y $z))))))
                 (𝐂 (λ $x (λ $y (λ $z (($x $z) $y)))))
                 (𝐔 (λ $x (λ $y ($y (($x $x) $y)))))
                 ($_ $term))))
      (Variable $term)
      (Grounded $term)
      (Expression (if (== $term ())
                      ()
                      (case $term
                        ((($f $g) ((cl2lc $f) (cl2lc $g)))
                         ($_ $term))))))))

;; ;; Test cl2lc
;; !(assertEqual
;;   (=α (cl2lc 𝐍) (λ $x (λ $y $y)))
;;   True)
;; !(cl2lc 𝐒)
;; !(cl2lc 𝐁)
;; !($f $x)

;; NEXT: get that fixed, see issue
;; https://github.com/trueagi-io/hyperon-experimental/issues/642
;;
;; !(($f $x) ($g $x))

;; !(λ $f (λ $g (λ $x (($f $x) ($g $x)))))
;; !(assertEqual
;; !(cl2lc (𝐒 𝐁))
;; !(=α (cl2lc (𝐒 𝐁)) (λ $n (λ $f (λ $x ($f (($n $f) $x))))))
;; ;; Convert (𝐊 𝐈) to (λ $f (λ $x $x)) (zero in lambda calculus)
;; ;;
;; ;; TODO: re-enable assertEqual when it will support alpha-equivalence.
;; ;; Maybe we should introduce something like assertAlphaEqual.
;; ;;
;; ;; !(assertEqual
;;   ;; !(cl2lc (𝐊 𝐈))
;;   ;; (λ $f (λ $x $x)))

;; ;; Convert ((𝐒 𝐈) 𝐈) to (λ $x ($x $x)) (M, the Mockingbird combinator)
;; ;; !(cl2lc ((𝐒 𝐈) 𝐈))

;; ;; Disabled because it does not half, as it should
;; ;; Convert (𝐌 𝐌) (reduction should not halt)
;; ;; !(cl2lc (𝐌 𝐌))

;; ;; Convert (𝐒 𝐈) to
;; ;; !(cl2lc (𝐒 𝐈))

;; ;; NEXT: find out if the following is correct

;; ;; Convert ((𝐒 (𝐊 𝐒)) 𝐊) to
;; ;; (λ $n (λ $f (λ $x ($f (($n $f) $x))))) (successor in lambda calculus)
;; ;; !(cl2lc ((𝐒 (𝐊 𝐒)) 𝐊))

;; ;; Convert 1 to lambda calculus
;; ;; !(cl2lc ((((𝐒 (𝐊 𝐒)) 𝐊) ((𝐒 (𝐊 𝐒)) 𝐊)) (𝐊 𝐈)))

;; ;; Convert 2 to lambda calculus
;; ;; !(cl2lc ((((𝐒 (𝐊 𝐒)) 𝐊) ((𝐒 (𝐊 𝐒)) 𝐊)) (𝐊 𝐈)))

;; ;; From (lc2cl (λ $f (λ $x ($f $x)))), corresponding to 1
;; ;; !(cl2lc ((𝐒 ((𝐒 (𝐊 𝐒)) ((𝐒 (𝐊 𝐊)) 𝐈))) (𝐊 𝐈)))
;; ;; (((λ $f378 (λ $g379 (λ $x380 (($f378 $x380) ($g379 $x380))))) (((λ $f448 (λ $g449 (λ $x450 (($f448 $x450) ($g449 $x450))))) ((λ $x470 (λ $y471 $x470)) (λ $f482 (λ $g483 (λ $x484 (($f482 $x484) ($g483 $x484))))))) (((λ $f543 (λ $g544 (λ $x545 (($f543 $x545) ($g544 $x545))))) ((λ $x564 (λ $y565 $x564)) (λ $x570 (λ $y571 $x570)))) (λ $x579 $x579)))) ((λ $x604 (λ $y605 $x604)) (λ $x610 $x610)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; λ-calculus to combinatory logic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert lambda-calculus to combinatory logic
(: lc2cl (-> Atom Number Atom Atom))
(= (lc2cl $term $cnt $dir)
   (if (is-expression $term)
       ;; Base cases
       (if (== $term ())
           (trace! (bas-∅ (term $term) (cnt $cnt) (dir $dir)) ())
           ;; NEXT: try to get rid of as many base cases as possible
           (if (=α $term (λ $x $x))
               (trace! (bas-I (term $term) (cnt $cnt) (dir $dir)) 𝐈)
               (if (=α $term (λ $x (λ $y $x)))
                   (trace! (bas-K (term $term) (cnt $cnt) (dir $dir)) 𝐊)
                   (if (=α $term (λ $f (λ $g (λ $x (($f $x) ($g $x))))))
                       (trace! (bas-S (term $term) (cnt $cnt) (dir $dir)) 𝐒)
                       (if (=α $term (λ $x ($x $x)))
                           (trace! (bas-M (term $term) (cnt $cnt) (dir $dir)) 𝐌)
                           ;; Recursive steps
                           (case $term
                             (((λ $x $f)
                               (if (is-variable $x)
                                   (let $clf (lc2cl $f (+ 1 $cnt) λC)
                                     (if (is-subterm-of $x $f)
                                         ; Build 𝐒
                                         (trace! (rec-S (term $term) (clf $clf) (cnt $cnt) (dir $dir))
                                         (if (is-expression $clf)
                                             (case $clf
                                               ((($g $h) ((𝐒 (lc2cl (λ $x $g) (+ 2 $cnt) λL))
                                                          (lc2cl (λ $x $h) (+ 2 $cnt) λR)))
                                                ($_ (empty))))
                                             $clf))
                                         ; Build 𝐊
                                         (trace! (rec-K (term $term) (clf $clf) (cnt $cnt) (dir $dir))
                                         (𝐊 $clf))))
                                   (empty)))
                              (($f $g) (trace! (rec-app (term $term) (cnt $cnt) (dir $dir))
                                       ((lc2cl $f (+ 1 $cnt) L) (lc2cl $g (+ 1 $cnt) R)))))))))))
       (trace! (bas-nch (term $term) (cnt $cnt) (dir $dir)) $term)))

;; ;; Test lc2cl
;; !(assertEqual
;;   (lc2cl $f)
;;   $f)
;; !(assertEqual
;;   (lc2cl ($f $x))
;;   ($f $x))
;; !(assertEqual
;;   (lc2cl ($f ($f $x)))
;;   ($f ($f $x)))
;; !(assertEqual
;;   (lc2cl (λ $x $x))
;;   𝐈)
;; !(assertEqual
;;   (lc2cl (λ $x (λ $y $x)))
;;   𝐊)
;; !(assertEqual
;;   (lc2cl (λ $f (λ $g (λ $x (($f $x) ($g $x))))))
;;   𝐒)
;; !(assertEqual
;;   (lc2cl (λ $x ($x $x)))
;;   𝐌)
;; !(assertEqual
;;   (lc2cl (λ $x $f))
;;   (𝐊 $f))
;; !(assertEqual
;;   (lc2cl (λ $x ($f $g)))
;;   (𝐊 ($f $g)))
;; !(assertEqual
;;   (lc2cl (λ $x ($f ($f $x))))
;;   ((𝐒 (𝐊 $f)) ((𝐒 (𝐊 $f)) 𝐈)))     ; NEXT: is it correct?
;; !(assertEqual
;;   (lc2cl (λ $f (λ $x $x)))    ; 0 in lambda calculus
;;   (𝐊 𝐈))                    ; 0 in combinatory logic


;; !(assertEqual
  ;; !(lc2cl (λ $f (λ $x ($f $x))) 0 C)    ; 1 in lambda calculus
;; ((𝐒 ((𝐒 (𝐊 𝐒)) ((𝐒 (𝐊 𝐊)) 𝐈))) (𝐊 𝐈))) ; NEXT: 1 in combinatory logic? (see calculi-converter.stdout
;; https://theory.stanford.edu/~blynn/lambda/cl.html finds
;; K(S(S(K S)(S(K K)(S K K)))(K(S K K)))
;; Note that https://crypto.stanford.edu/~blynn/lambda/kiselyov.html is newer


;; !(assertEqual
;;   (lc2cl (λ $f (λ $x ($f ($f $x)))))    ; Two in lambda calculus
;;   (...))                           ; NEXT: Two in combinatory logic

;; NEXT: find out why the following does not work

;; ;; Convert (λ $g (λ $x ($g $x))) to ?
;; !(lc2cl (λ $g (λ $x ($g $x))))

;; ;; Convert (λ $g (λ $x ($x ($g $x)))) to ?
;; !(lc2cl (λ $g (λ $x ($x ($g $x)))))

;; ;; Convert ((λ $f (λ $g (λ $x (($f $x) ($g $x))))) (λ $y $y)) to (𝐒 𝐈)
;; !(lc2cl ((λ $f (λ $g (λ $x (($f $x) ($g $x))))) (λ $y $y)))

;; ;; Convert S from lambda calculus to combinatory logic.  That is
;; ;; (λ $n (λ $f (λ $x ($f (($n $f) $x))))) |-> ((𝐒 (𝐊 𝐒)) 𝐊)
;; !(lc2cl (λ $n (λ $f (λ $x ($f (($n $f) $x))))))

;; (λ $n (λ $f (λ $x ($f (($n $f) $x)))))
;; -->

;;;;;;;;;;;;;;;;;;;;;;;;;
;; MeTTa to λ-calculus ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; NEXT:
;; 1. replace LET by let (if possible)
;; 2. support let*

;; Convert a MeTTa expression to lambda calculus
(: mt2lc (-> Atom Atom))
(= (mt2lc $expr)
   (if (is-variable $expr)
       $expr
       (case $expr
         (((LET $x $y $f) ((λ $x (mt2lc $f)) (mt2lc $y)))  ; Let
          (($f $x) ((mt2lc $f) (mt2lc $x)))                ; Application
          ($s $s)))))                                      ; Symbol

;; !(mt2lc
;;   (LET $spleeter-output
;;        (snet.sound-spleeter.spleeter ((snet.sound-spleeter.MkInput "") $input))
;;        (LET $english-vocals (snet.sound-spleeter.Output.vocals $spleeter-output)
;;             (LET $chinese-lyrics
;;                  (naint.machine-translation.Output.translation
;;                   (naint.machine-translation.translate
;;                    (((naint.machine-translation.MkInput "English") "Chinese")
;;                     (snet.speech-recognition.Text.text
;;                      (snet.speech-recognition.s2t
;;                       (snet.speech-recognition.MkAudio $english-vocals))))))
;;                  (LET $midi-notes (tomidi.MIDI.data (tomidi.MkAudio $english-vocals))
;;                       (LET $chinese-vocals
;;                            (naint.midi2voice-zh.Answer.output_audio
;;                             (naint.midi2voice-zh.singingZH
;;                              (((naint.midi2voice-zh.MkQuery $chinese-lyrics)
;;                                $midi-notes) 1.0)))
;;                            (mixer.Audio.data
;;                             (mixer.mix
;;                              ((mixer.MkMultiAudio
;;                                (snet.sound-spleeter.Output.accomp $spleeter-output))
;;                               $chinese-vocals)))))))))
