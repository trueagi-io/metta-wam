;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward and forward curried chainers generating fully annotated    ;;
;; proof tree.                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common types and functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Unary lambda
;; (: λ (-> Variable Atom Atom))
;; (= ((λ $x $f) $y) (let $x $y $f))

;; ;; Test unary λ
;; !(assertEqual
;;   ((λ $x (+ 1 $x)) 1)
;;   2)

;; ;; Binary lambda
;; (: λ (-> Variable Variable Atom Atom Atom))
;; (= ((λ $x $y $f) $z $w) (let* (($x $z) ($y $w)) $f))

;; ;; Test binary λ
;; !(assertEqual
;;   ((λ $x $y (+ $x $y)) 1 2)
;;   3)

;; Curry a binary function
(: curry2 (-> (-> $a $b $c) (-> $a (-> $b $c))))
(= (((curry2 $f) $x) $y) ($f $x $y))

;; Curry a trinary function
(: curry3 (-> (-> $a $b $c $d) (-> $a (-> $b (-> $c $d)))))
(= ((((curry3 $f) $x) $y) $z) ($f $x $y $z))

;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define <=
(: <= (-> $a $a Bool))
(= (<= $x $y) (or (< $x $y) (== $x $y)))

;; Define cast functions between Nat and Number
(: fromNumber (-> Number Nat))
(= (fromNumber $n) (if (<= $n 0) Z (S (fromNumber (- $n 1)))))
(: fromNat (-> Nat Number))
(= (fromNat Z) 0)
(= (fromNat (S $k)) (+ 1 (fromNat $k)))

;; Return the ceiling of a non negative number.  If the number is
;; negative it returns 1.
(: ceil (-> Number Number))
(= (ceil $n) (fromNat (fromNumber $n)))

;; Apply a given function to every element of a List
(: List.map (-> (-> $a $b) (List $a) (List $b)))
(= (List.map $f Nil) Nil)
(= (List.map $f (Cons $h $t)) (Cons ($f $h) (List.map $f $t)))

;; Apply a given function to every element of a tuple
(: map (-> (-> $a $b) $c $d))
(= (map $f $xs)
   (if (== $xs ())
       ()
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($fh ($f $h))
              ($ft (map $f $t)))
         (cons-atom $fh $ft))))

;; Test map
!(assertEqual
  (map repr (1 2 3))
  ("1" "2" "3"))

;; Return the location of the first element in a tuple meeting a
;; certain condition.  If no such element exists return -1.
(: findIf (-> (-> $a Bool) $b Number))
(= (findIf $p $t) (findIf-rec $p $t 0))

;; Like findIf but takes the location of the current tail tuple within
;; the initial tuple.
(: findIf-rec (-> (-> $a Bool) $b Number Number))
(= (findIf-rec $p $t $l)
   (if (== $t ())
       -1
       (let $hd (car-atom $t)
         (if ($p $hd)
             $l
             (findIf-rec $p $t (+ 1 $l))))))

;; Return the location of the first character in a string meeting a
;; certain condition.  If no such character exists return -1.
(: findIf-str (-> (-> Char Bool) String Number))
(= (findIf-str $p $s) (findIf $p (stringToChars $s)))

;; Return True iff the given character is a space character.
(: isSpace (-> Char Bool))
(= (isSpace $c)
   (case $c
     ((' ' True)
      ('\t' True)
      ($_ False))))

;; Return True off the given character is not a space character.
(: isNotSpace (-> Char Bool))
(= (isNotSpace $c) (not (isSpace $c)))

;; NEXT: test findIf-str

;; Zip two tuples of same arity with a given function.
;;
;; TODO: replace type variables by Expression or some other proper
;; types once Expression and quote has been decoupled.
(: zipWith (-> (-> $a $b $c) $d $e $f))
(= (zipWith $f $xs $ys)
   (if (== $xs ())                      ; We assume (arity $xs) == (arity $ys)
       ()
       (let* (($xs-hd (car-atom $xs))
              ($xs-tl (cdr-atom $xs))
              ($ys-hd (car-atom $ys))
              ($ys-tl (cdr-atom $ys))
              ($head ($f $xs-hd $ys-hd))
              ($tail (zipWith $f $xs-tl $ys-tl)))
         (cons-atom $head $tail))))

;; Test zipWith
!(assertEqual
  ()
  ())
!(assertEqual
  (zipWith + (1 2 3) (4 5 6))
  (5 7 9))

;; Fold a tuple from right to left
(: foldr (-> (-> $a $b $b) $b $c $d))
(= (foldr $f $i $xs)
   (if (== $xs ())
       $i
       (let* (($h (car-atom $xs))
              ($t (cdr-atom $xs))
              ($ft (foldr $f $i $t)))
         ($f $h $ft))))

;; Test foldr
!(assertEqual
  (foldr + 0 (1 2 3))
  6)

;; Generate a tuple by repeating an element n times
(: repeat (-> $a Number $b))
(= (repeat $x $n)
   (if (== $n 0)
       ()
       (let $tail (repeat $x (- $n 1))
         (cons-atom $x $tail))))

;; Test repeat
!(assertEqual
  (repeat () 0)
  ())
!(assertEqual
  (repeat () 3)
  (() () ()))

;; Return the last element of a tuple
(: last (-> $a $b))
(= (last $tuple)
   (if (== $tuple ())
       (empty)
       (let* (($head (car-atom $tuple))
              ($tail (cdr-atom $tuple)))
         (if (== $tail ())
             $head
             (last $tail)))))

;; Test last
!(assertEqual
  (last (A))
  A)
!(assertEqual
  (last (A B C))
  C)

;; Return True iff $term is an expression
(: is-expression (-> Atom Bool))
(= (is-expression $term) (== (get-metatype $term) Expression))

;; Concatenate two tuples
;;
;; for instance
;;
;; (concat-atom (a b) (c d)) returns (a b c d)
(: concat-atom (-> $a $a $a))
(= (concat-atom $lhs $rhs) (foldr cons-atom $rhs $lhs))

;; Test concat-atom
!(assertEqual
  (concat-atom (a b) ())
  (a b))
!(assertEqual
  (concat-atom (a b) (c d))
  (a b c d))

;; SplitAt tuple into two tuple from a given position.  For instance
;;
;; (splitAt (a b c) 0) returns (() (a b c))
;; (splitAt (a b c) 1) returns ((a) (b c))
;; (splitAt (a b c) 2) returns ((a b) (c))
;;
;; If the position exceeds the arity of the tuple, then it behaves as
;; if the position was the arity.  For instance
;;
;; (splitAt (a b c) 4) returns ((a b c) ())
;;
;; If $position is negative then it does not terminate.
(: splitAt (-> $a Number $a))
(= (splitAt $tuple $position)
   (if (== $position 0)
       (() $tuple)
       (if (== $tuple ())
           (() ())
           (let* (($head (car-atom $tuple))
                  ($tail (cdr-atom $tuple))
                  (($split-head $split-tail) (splitAt $tail (- $position 1))))
             ((cons-atom $head $split-head) $split-tail)))))

;; Test splitAt
!(assertEqual
  (splitAt (a b c) 0)
  (() (a b c)))
!(assertEqual
  (splitAt (a b c) 1)
  ((a) (b c)))
!(assertEqual
  (splitAt (a b c) 2)
  ((a b) (c)))
!(assertEqual
  (splitAt (a b c) 3)
  ((a b c) ()))
!(assertEqual
  (splitAt (a b c) 4)
  ((a b c) ()))

;; ;; Return the arity of an expression
;; (= (acc $x $n) (+ 1 $n))
;; (: arity (-> $a Number))
;; (= (arity $expr) (foldr acc 0 $expr))
;; ;; TODO: re-enable when the interpreter is fixed
;; ;; (= (arity $expr) (foldr (λ $x $n (+ 1 $n)) 0 $expr))

;; Test arity
!(assertEqual
  (arity ())
  0)
!(assertEqual
  (arity (a b c))
  3)

;; Return the maximum of two number
(: max (-> Number Number Number))
(= (max $x $y) (if (< $x $y) $y $x))

;; Given a tuple of numbers, return the maximum, or 0 if empty.
(: max-element (-> $a Number))
(= (max-element $expr) (foldr max 0 $expr))

;; Test max-element
!(assertEqual
  (max-element (1 3 2))
  3)

;; Test len-str
!(assertEqual
  (len-str "abc")
  3)

;; Test concat-str
!(assertEqual
  (concat-str "abc" "def")
  "abcdef")

;; Generate a String composed of a repeated String.
(: repeat-str (-> String Number String))
(= (repeat-str $s $n)
   (if (== $n 0)
       ""
       (let $r (repeat-str $s (- $n 1))
         (concat-str $s $r))))

;; Test repeat-str
!(assertEqual
  (repeat-str "abc" 3)
  "abcabcabc")

;; Test join-str
!(assertEqual
  (join-str ", " ("A" "B" "C"))
  "A, B, C")

;; Test split-str
!(assertEqual
  (split-str "A, B, C" ", ")
  ("A" "B" "C"))

;; Given a tuple of strings, return the length of the string with
;; maximum length.
(: max-len-strs (-> $a Number))
(= (max-len-strs $ss) (let $ls (map len-str $ss) (max-element $ls)))

;; Test max-len-strs
!(assertEqual (max-len-strs ()) 0)
!(assertEqual (max-len-strs ("abc" "a" "abcd")) 4)

;; Pad the tailing part of a string with a given character to reach a
;; given len.  If the string is longer than the target len, then the
;; string is left untouched.
;;
;; For instance
;;
;; (pad-trail ' ' 5 "abc")
;;
;; outputs
;;
;; "abc  "
(: pad-trail (-> String Number String String))
(= (pad-trail $c $n $s)
   (let $len (len-str $s)
     (if (< $len $n)
         (concat-str $s (repeat-str $c (- $n $len)))
         $s))))

;; Test pad-trail
!(assertEqual
  (pad-trail " " 5 "abc")
  "abc  ")

;; Like pad-trail but applied to a tuple of strings
(: pad-trails (-> String Number $a $b))
(= (pad-trails $c $n $ss) (map (((curry3 pad-trail) $c) $n) $ss))

;; Test pad-trails
!(assertEqual
  (pad-trails " " 2 ("A"))
  ("A "))
!(assertEqual
  (pad-trails " " 5 ("A" "AB" "ABC"))
  ("A    " "AB   " "ABC  "))

;;;;;;;;;;;;;;;;;;;;
;; Knowledge base ;;
;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

!(add-atom &kb (: a A))
!(add-atom &kb (: ab (→ A B)))
!(add-atom &kb (: bc (→ B C)))
!(add-atom &kb (: ModusPonens
                (-> (→ $p $q)
                    (-> $p
                        $q))))

;;;;;;;;;;;;;;;;;;;;;;
;; Backward chainer ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Curried Backward Chainer with fully annotated proofs.  The
;; arguments of the backward chainer are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).  Note that rules are explicitely
;;   curried, meaning that a rule with two premises is represented by
;;
;;   (: <NAME> (-> <PREMISE1> (-> <PREMISE2> <CONCLUSION>)))
;;
;; * Query: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables that may be
;;   filled by the backward chainer.
;;
;; * Maximum depth: maximum depth of the generated proof tree.
;;
;; A result is the query with its variables grounded, fully or
;; partially.  If multiple results are possible, they are returned as
;; a superposition.
(: bc (-> $a                            ; Knowledge base space
          $b                            ; Query
          Nat                           ; Maximum depth
          $b))                          ; Result
;; Base case
(= (bc $kb (: $prf $ccln) $_) (match $kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc $kb (: ($prfabs (: $prfarg $prms)) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc $kb (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc $kb (: $prfarg $prms) $k)))
     (: ($prfabs (: $prfarg $prms)) $ccln)))

;; Test curried backward chainer
!(assertEqual
  (bc &kb (: $prf A) (fromNumber 0))
  (: a A))
!(assertEqual
  (bc &kb (: $prf B) (fromNumber 2))
  (: ((ModusPonens (: ab (→ A B))) (: a A)) B))
!(assertEqual
  (bc &kb (: $prf C) (fromNumber 3))
  (: ((ModusPonens (: bc (→ B C))) (: ((ModusPonens (: ab (→ A B))) (: a A)) B)) C))

;;;;;;;;;;;;;;;;;;;;;
;; Forward chainer ;;
;;;;;;;;;;;;;;;;;;;;;

;; Curried Forward Chainer with fully annotated proofs.  The arguments
;; of the forward chainer are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).  Note that rules are explicitely
;;   curried, meaning that a rule with two premises is represented by
;;
;;   (: <NAME> (-> <PREMISE1> (-> <PREMISE2> <CONCLUSION>)))
;;
;; * Source: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables.  Beware that
;;   the source is assumed to be true.  That is especially important
;;   to consider when introducing free variables, because these free
;;   variables will be treated as if they are univerally quantified.
;;
;; * Maximum depth: maximum depth of the generated proof tree.
;;
;; A result is a conclusion that can be reached from the source.  If
;; multiple results are possible, they are returned as a
;; superposition.
(: fc (-> $a                            ; Knowledge base space
          $b                            ; Source
          Nat                           ; Maximum depth
          $b))                          ; Conclusion
;; Base case
(= (fc $kb (: $prf $prms) $_) (: $prf $prms))
;; Recursive step
(= (fc $kb (: $prfarg $prms) (S $k))
   (let (: $prfabs (-> $prms $ccln)) (bc $kb (: $prfabs (-> $prms $ccln)) $k)
        (fc $kb (: ($prfabs (: $prfarg $prms)) $ccln) $k)))
(= (fc $kb (: $prfabs (-> $prms $ccln)) (S $k))
   (let (: $prfarg $prms) (bc $kb (: $prfarg $prms) $k)
        (fc $kb (: ($prfabs (: $prfarg $prms)) $ccln) $k)))

;; Test curried forward chainer
!(assertEqualToResult
  (fc &kb (: ab (→ A B)) (fromNumber 1))
  ((: ab (→ A B))
   (: (ModusPonens (: ab (→ A B))) (-> A B))))
!(assertEqualToResult
  (fc &kb (: ab (→ A B)) (fromNumber 2))
  ((: ab (→ A B))
   (: (ModusPonens (: ab (→ A B))) (-> A B))
   (: ((ModusPonens (: ab (→ A B))) (: a A)) B)))
!(assertEqualToResult
  (fc &kb (: a A) (fromNumber 3))
  ((: a A)
   (: ((ModusPonens (: ab (→ A B))) (: a A)) B)
   (: ((ModusPonens (: bc (→ B C))) (: ((ModusPonens (: ab (→ A B))) (: a A)) B)) C)))

;;;;;;;;;;;;;;;
;; Flattener ;;
;;;;;;;;;;;;;;;

;; Like flatten but treat the given tuple as a list of expressions
;; instead of an expression
(: flatten-map (-> $a $a))
(= (flatten-map $tuple)
   (if (== $tuple ())
       ()
       (let* (($head (car-atom $tuple))
              ($tail (cdr-atom $tuple))
              ($flat-head (flatten $head))
              ($flat-tail (flatten-map $tail)))
         (cons-atom $flat-head $flat-tail))))

;; Flatten a curried expression.  For instance
;;
;; (flatten ((f a) b)) returns (f a b)
;;
;; Note that it only flattens left-associative application.  Therefore
;; it will not work on the arrow type as it is right-associative.  For
;; instance
;;
;; (flatten (-> T1 (-> T2 T3))) returns (-> T1 (-> T2 T3)), not (-> T1 T2 T3)
(: flatten (-> Atom Atom))
(= (flatten $term)
   (if (is-expression $term)
       (if (== $term ())
           ()
           (let* (($head (car-atom $term))
                  ($tail (cdr-atom $term))
                  ($flat-head (flatten $head))
                  ($flat-tail (flatten-map $tail)))
             (if (is-expression $flat-head)
                 (if (== $flat-head ())
                     (cons-atom $flat-head $flat-tail)
                     (let* (($hd-hd (car-atom $flat-head))
                            ($tl-hd (cdr-atom $flat-head))
                            ($tl-hd-tl (concat-atom $tl-hd $flat-tail)))
                       (cons-atom $hd-hd $tl-hd-tl)))
                 (cons-atom $flat-head $flat-tail))))
       $term))

;; Test flatten
!(assertEqual
  (flatten a)
  a)
!(assertEqual
  (flatten $x)
  $x)
!(assertEqual
  (flatten ())
  ())
!(assertEqual
  (flatten (()))
  (()))
!(assertEqual
  (flatten (f a))
  (f a))
!(assertEqual
  (flatten ((f a)))
  (f a))
!(assertEqual
  (flatten (() a))
  (() a))
!(assertEqual
  (flatten ((f a) b))
  (f a b))
!(assertEqual
  (flatten ((f a) ()))
  (f a ()))
!(assertEqual
  (flatten ((f a b) c))
  (f a b c))
!(assertEqual
  (flatten (((f a) b) c))
  (f a b c))
!(assertEqual
  (flatten ((f a) b c))
  (f a b c))
!(assertEqual
  (flatten ((f a) (g c)))
  (f a (g c)))
!(assertEqual
  (flatten (((f a) b) (g c)))
  (f a b (g c)))
!(assertEqual
  (flatten ((f a) (g c) (h d)))
  (f a (g c) (h d)))
!(assertEqual
  (flatten (((f a) b) ((g c) d)))
  (f a b (g c d)))
!(assertEqual
  (flatten (: ((plus (: 1 Number)) (: 2 Number)) Number))
  (: (plus (: 1 Number) (: 2 Number)) Number))
!(assertEqual
  (flatten (: ((ModusPonens (: bc (→ B C)))
               (: ((ModusPonens (: ab (→ A B))) (: a A)) B)) C))
  (: (ModusPonens (: bc (→ B C)) (: (ModusPonens (: ab (→ A B)) (: a A)) B)) C))

;;;;;;;;;;;;
;; Drawer ;;
;;;;;;;;;;;;

;; Given two tuples of lines, draw them side by side, as if they were
;; both standing on the ground, with a given margin separating them.
;;
;; For instance
;;
;; (draw-side-by-side lhs rhs 1)
;;
;; where lhs represents
;;
;; "  / /"
;; " /-/"
;; "/ /"
;;
;; and rhs represents
;;
;; "   /"
;; "  /"
;; " /"
;; "/"
;;
;; outputs
;;
;; "         /"
;; "  / /   /"
;; " /-/   /"
;; "/ /   /"
;;
;; TODO: replace type variables by Expression or some other proper
;; types once Expression and quote has been decoupled.
(: draw-side-by-side (-> $a $b Number $c))
(= (draw-side-by-side $lhs $rhs $margin)
   (let* (($lhs-nlines (arity $lhs))
          ($rhs-nlines (arity $rhs)))
     (if (< $lhs-nlines $rhs-nlines)
         (let* (($lhs-max-len (+ (max-len-strs $lhs) $margin))
                ($lhs-padded (pad-trails " " $lhs-max-len $lhs))
                ($lhs-blank-line (repeat-str " " $lhs-max-len))
                ($diff-nlines (- $rhs-nlines $lhs-nlines))
                ($lhs-blank-lines (repeat $lhs-blank-line $diff-nlines))
                ($lhs-fully-padded (concat-atom $lhs-blank-lines $lhs-padded)))
           (zipWith concat-str $lhs-fully-padded $rhs))
         (let* (($diff-nlines (- $lhs-nlines $rhs-nlines))
                (($lhs-top $lhs-bottom) (splitAt $lhs $diff-nlines))
                ($lhs-bottom-max-len (max-len-strs $lhs-bottom))
                ($lhs-bottom-padded (pad-trails " "
                                                (+ $lhs-bottom-max-len $margin)
                                                $lhs-bottom))
                ($bottom (zipWith concat-str $lhs-bottom-padded $rhs)))
           (concat-atom $lhs-top $bottom)))))

Test draw-side-by-side
!(assertEqual
  (draw-side-by-side () () 1)
  ())
!(assertEqual
  (draw-side-by-side ("1") () 1)
  ("1"))
!(assertEqual
  (draw-side-by-side () ("2") 1)
  (" 2"))
!(assertEqual
  (draw-side-by-side ("1") ("2") 1)
  ("1 2"))
!(assertEqual
  (draw-side-by-side ("1") (" 2" "3") 1)
  ("   2" "1 3"))
!(assertEqual
  (draw-side-by-side ("1" " 2") ("3") 1)
  ("1" " 2 3"))
!(assertEqual
  (draw-side-by-side ("------(2)" "Number") () 2)
  ("------(2)" "Number"))
!(assertEqual
  (draw-side-by-side ("------(1)" "Number") ("------(2)" "Number") 2)
  ("------(1)  ------(2)" "Number     Number"))

;; Given a tuple of expressions of the form (: PROOF THEOREM), return
;; an ascii drawing of these proofs in a Gentzen style side by side.
;; Like for draw, a multi-line string is represented by a tuple of
;; strings, one on each line.
;;
;; For instance
;;
;; (draw-tuple ((: bc (→ B C))
;;              (: (ModusPonens (: ab (→ A B)) (: a A)) B))
;;
;; outputs
;;
;; "             -------(ab)  -(a)"
;; "             (→ A B)      A"
;; "-------(bc)  --------------(ModusPonens)"
;; "(→ B C)             B"
;;
;; TODO: replace type variables by Expression or some other proper
;; types once Expression and quote has been decoupled.
(: draw-tuple (-> $a $b))
(= (draw-tuple $expr)
   (if (== $expr ())
       ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
              ($drawn-head (draw $head))
              ($drawn-tail (draw-tuple $tail)))
         (draw-side-by-side $drawn-head $drawn-tail 2))))

;; Given a fully annotated proof, return a string corresponding to an
;; ascii drawing of its proof in a Gentzen style.  For now, due to
;; MeTTa not supporting escaped characters, the ascii representation
;; is a tuple of lines, where each line is a String.  For instance the
;; string "abc\ndef" is represented by `("abs" "def")`.
;;
;; For instance
;;
;; (draw (: (ModusPonens (: bc (→ B C))
;;                       (: (ModusPonens (: ab (→ A B))
;;                                       (: a A)) B))
;;           C))
;;
;; Outputs
;;
;; "             -------(ab)  -(a)"
;; "             (→ A B)      A"
;; "-------(bc)  --------------(ModusPonens)"
;; "(→ B C)             B"
;; "---------------------(ModusPonens)"
;; "           C"
;;
;; TODO: replace type variables by Expression or some other proper
;; types once Expression and quote has been decoupled.
(: draw (-> $a $b))
(= (draw (: $prf $thrm))
   (if (is-expression $prf)
       ;; The proof is an expression
       (if (== $prf ())
           ()
           (let* (($prfabs (car-atom $prf))
                  ($prfabs-str (repr $prfabs))
                  ($prfabs-str-len (len-str $prfabs))
                  ($prfargs (cdr-atom $prf))
                  ($prfargs-strs (draw-tuple $prfargs))
                  ($prfargs-last-line (last $prfargs-strs))
                  ($prfargs-last-line-len (len $prfargs-last-line))
                  ;; NEXT: use $prfargs-last-line-len.  The last line
                  ;; needs to be split into its heading spaces and
                  ;; tail, then the size of the tail needs to be used
                  ;; as input for for (repeat-str "-" ...), and then
                  ;; the result needs to be stuck back to the same
                  ;; heading spaces.
                  ($prfargs-strs-lens (map len-str $prfargs-strs))
                  ($prfargs-strs-max-len (max-element $prfargs-strs-lens))
                  ($thrm-str (repr $thrm))
                  ($thrm-str-len (len-str $thrm-str))
                  ($line-str-len (max $prfargs-strs-max-len $thrm-str-len))
                  ($line-str (repeat-str "-" $line-str-len))
                  ($prfabs-par-str (concat-str (concat-str "(" $prfabs-str) ")"))
                  ($line-prfabs-str (concat-str $line-str $prfabs-par-str))
                  ($pad-len (ceil (/ (- $line-str-len $thrm-str-len) 2)))
                  ($space-pad (repeat-str " " $pad-len))
                  ($padded-thrm-str (concat-str $space-pad $thrm-str)))
             (concat-atom $prfargs-strs ($line-prfabs-str $padded-thrm-str))))
       ;; The proof is a symbol, or else
       (let* (($prf-str (repr $prf))
              ($prf-par-str (concat-str (concat-str "(" $prf-str) ")"))
              ($thrm-str (repr $thrm))
              ($thrm-str-len (len-str $thrm-str))
              ($line-str (repeat-str "-" $thrm-str-len))
              ($line-prf-str (concat-str $line-str $prf-par-str)))
         ($line-prf-str $thrm-str))))

;; Test draw-tuple
!(assertEqual
  (draw-tuple ())
  ())
!(assertEqual
  (draw-tuple ((: 2 Number)))
  ("------(2)" "Number"))
!(assertEqual
  (draw-tuple ((: 1 Number) (: 2 Number)))
  ("------(1)  ------(2)" "Number     Number"))

;; Test draw
!(assertEqual
  (draw (: a A))
  ("-(a)" "A"))
!(assertEqual
  (draw (: 2 Number))
  ("------(2)" "Number"))
!(assertEqual
  (draw (: Refl (=== $x $x)))
  ("-----------(Refl)" "(=== $x $x)"))
;; NEXT: create assertEqual tests from the following
;; !(draw (: (plus (: 1 Number) (: 2 Number)) Number))
;; !(draw (: ((ModusPonens (: ab (→ A B))) (: a A)) B))
;; !(draw (: ((ModusPonens (: bc (→ B C))) (: ((ModusPonens (: ab (→ A B))) (: a A)) B)) C))
