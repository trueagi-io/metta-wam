;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example of both backward chainer with lambda abstraction.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; Nat ;;
;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;
;; De Bruijn Index ;;
;;;;;;;;;;;;;;;;;;;;;

;; Define DeBruijn type
(: DeBruijn Type)

;; Define DeBruijn constructors
(: Z' DeBruijn)                        ; Zero
(: S' (-> DeBruijn DeBruijn))          ; Successor

;;;;;;;;;;
;; List ;;
;;;;;;;;;;

;; Declaration of List data type and constructors
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;;;;;;;;;;;;;;;;;;;;;
;; Match over list ;;
;;;;;;;;;;;;;;;;;;;;;

;; Similar to match but takes a list of terms instead of a space.
(: match' (-> (List Atom) $a $a $a))
(= (match' Nil $pattern $rewrite) (empty))
(= (match' (Cons $head $tail) $pattern $rewrite) (let $pattern $head $rewrite))
(= (match' (Cons $head $tail) $pattern $rewrite) (match' $tail $pattern $rewrite))

;; Test match' on empty list
!(assertEqualToResult
  (match' Nil ($x $y) ($y $x))
  ())

;; Test match' on singleton
!(assertEqual
  (match' (Cons (A B) Nil) ($x $y) ($y $x))
  (B A))

;; Test match' on pair
!(assertEqualToResult
  (match' (Cons (A B) (Cons (C D) Nil)) ($x $y) ($y $x))
  ((B A)
   (D C)))

;;;;;;;;;;;;;;;;;;;;;;
;; Backward chainer ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Curried Backward Chainer with lambda abstraction.  A lambda
;; abstraction is represented by
;;
;; (λ <INDEX> <BODY>)
;;
;; where <INDEX> is a De Bruijn index as define above, such as Z' or
;; (S' Z'), and <BODY> is a term possibly containing <INDEX>.  Note
;; that the use of De Bruijn index in lambda abstraction is somewhat
;; unconventional here.  It differs from what is described in
;; https://en.wikipedia.org/wiki/De_Bruijn_index in three ways:
;;
;; 1. The index is explicitely attached to a λ by being its first
;;    argument.  For instance the lambda term λx.x, which would
;;    traditionally be represented by λ1 using De Bruijn index, would
;;    be represented here by the MeTTa term (λ Z' Z').
;;
;; 2. As seen in the example above the index here starts at 0,
;;    represented by Z', instead of 1.
;;
;; 3. The index increases as the lambda abstraction gets deeper.  For
;;    instance λx.λy.x, which would traditionally be represented by
;;    λλ2 using De Bruijn index, is represented here by the MeTTa term
;;    (λ Z' (λ (S' Z') Z').
;;
;; This differences are due to the way the proof abstraction recursive
;; step is defined in the bc, as well as the need for having non
;; overlapping pattern matching between the two recursive steps.
;; Specifically, if λ had only one argument, then (λ <BODY>) would
;; overlap with (<ABS> <ARG>).  Having λ take 2 arguments instead of 1
;; has the disadvantage of making lambda abstraction not as compact.
;; On the flip side, the benefit is that the scope of an index is
;; easier to track.
;;
;; The arguments of the backward chainer are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).  Note that rules are explicitely
;;   curried, meaning that a rule with two premises is represented by
;;
;;   (: <NAME> (-> <PREMISE1> (-> <PREMISE2> <CONCLUSION>)))
;;
;; * Environment: a list of typing relationship between De Bruijn
;;   index and type, such as
;;
;;   (Cons (: Z' String) (Cons (: (S' Z') Number) Nil))
;;
;; * De Bruijn Index: De Bruijn Index to use if a lambda abstraction
;;   is introduced.
;;
;; * Maximum depth: maximum depth of the generated proof tree.
;;
;; * Query: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables that may be
;;   filled by the backward chainer.  Note that for arguments of
;;   applications must be type annotated (unknowns types are of course
;;   allowed).  For instance the following query
;;
;;   (: (ModusPonens ab) $thrm)
;;
;;   would not work, instead one needs to provide
;;
;;   (: (ModusPonens (: ab $lemma)) $thrm)
;;
;; A result is the query with its variables grounded, fully or
;; partially.  If multiple results are possible, they are returned as
;; a superposition.
;;
;; As explained, the proof arguments must be type annotated.  Without
;; such type annotation, lambda abstraction sometimes leads to
;; incorrect results.  It is suspected to come from the fact that
;; otherwise the proof application recursive step loses bindings about
;; the premise.  Having annotated proof is however a good thing, to
;; display a proof tree and to have an indepth view into the proof.
;; In order to remove (resp. add) type annotation one can use
;; remove-type (resp. add-type).
(: bc (-> $a                            ; Knowledge base space
          $b                            ; Environment
          DeBruijn                      ; De Bruijn Index
          Nat                           ; Maximum depth
          $c                            ; Query
          $c))                          ; Result

;; Base cases

;; Match the knowledge base
(= (bc $kb $env $idx $_ (: $prf $thrm))
   (match $kb (: $prf $thrm) (: $prf $thrm)))
;; Match the environment
(= (bc $kb $env $idx $_ (: $prf $thrm))
   (match' $env (: $prf $thrm) (: $prf $thrm)))

;; Recursive steps

;; Proof application
(= (bc $kb $env $idx (S $k) (: ($prfabs (: $prfarg $prms)) $thrm))
   (let* (((: $prfabs (-> $prms $thrm))
           (bc $kb $env $idx $k (: $prfabs (-> $prms $thrm))))
          ((: $prfarg $prms)
           (bc $kb $env $idx $k (: $prfarg $prms))))
     (: ($prfabs (: $prfarg $prms)) $thrm)))
;; Proof abstraction
(= (bc $kb $env $idx (S $k) (: (λ $idx $prfbdy) (-> $prms $thrm)))
   (let (: $prfbdy $thrm)
     (bc $kb (Cons (: $idx $prms) $env) (S' $idx) $k (: $prfbdy $thrm))
     (: (λ $idx $prfbdy) (-> $prms $thrm))))

;;;;;;;;;;;;;;;;;;;;
;; Knowledge base ;;
;;;;;;;;;;;;;;;;;;;;

;; Simple knowledge base for expository purpose.  Do not confuse `->`,
;; the MeTTa arrow type, and `→`, a regular MeTTa symbol denoting a
;; relationship.

!(bind! &kb (new-space))

!(add-atom &kb (: a A))
!(add-atom &kb (: ab (→ A B)))
!(add-atom &kb (: bc (→ B C)))
!(add-atom &kb (: ModusPonens
                (-> (→ $p $q)           ; Premise 1
                    (-> $p              ; Premise 2
                        $q))))          ; Conclusion

;;;;;;;;;;;;;;;;;;;;;
;; Reduction Rules ;;
;;;;;;;;;;;;;;;;;;;;;

;; ;; Application of identity
;; (= ((λ $x $x) $y) $y)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

;; Find proof of A
!(assertEqual
  (bc &kb Nil Z' (fromNumber 0) (: $prf A))
  (: a A))

;; Find proof of B
!(assertEqual
  (bc &kb Nil Z' (fromNumber 2) (: $prf B))
  (: ((ModusPonens (: ab (→ A B))) (: a A)) B))

;; Find proof of C
!(assertEqual
  (bc &kb Nil Z' (fromNumber 3) (: $prf C))
  (: ((ModusPonens (: bc (→ B C))) (: ((ModusPonens (: ab (→ A B))) (: a A)) B)) C))

;; Find proof of (-> A B)
!(assertEqual
  (bc &kb Nil Z' (fromNumber 1) (: $prf (-> A B)))
  (: (ModusPonens (: ab (→ A B))) (-> A B)))

;; Find proof of (-> A A)
!(assertEqualToResult
  (bc &kb Nil Z' (fromNumber 1) (: $prf (-> A A)))
  ((: (λ Z' a) (-> A A))
   (: (λ Z' Z') (-> A A))))

;; Infer type of (λ Z' ((ModusPonens (: ab (→ A B))) (: Z' A)))
!(assertEqual
  (bc &kb Nil Z' (fromNumber 3)
      (: (λ Z' ((ModusPonens (: ab (→ A B))) (: Z' A))) $thrm))
  (: (λ Z' ((ModusPonens (: ab (→ A B))) (: Z' A))) (-> A B)))

;; Find proof of (-> $h C), filling the $h hypothesis
!(assertEqualToResult
  (bc &kb Nil Z' (fromNumber 1) (: $prf (-> $h C)))
  ((: (λ Z' Z') (-> C C))
   (: (ModusPonens (: bc (→ B C))) (-> B C))))

;; Find proof of (-> A B) with depth 3 to include lambda abstractions
!(assertEqualToResult
  (bc &kb Nil Z' (fromNumber 3) (: (λ Z' $bdy) (-> A B)))
  ((: (λ Z' ((ModusPonens (: ab (→ A B))) (: Z' A))) (-> A B))
   (: (λ Z' ((ModusPonens (: ab (→ A B))) (: a A))) (-> A B))))

;; Check proof of B assuming the type of one De Bruijn index in the
;; environment
!(assertEqual
  (bc &kb (Cons (: Z' (→ A B)) Nil) Z' (fromNumber 2)
      (: ((ModusPonens (: Z' (→ A B))) (: a A)) B))
  (: ((ModusPonens (: Z' (→ A B))) (: a A)) B))

;; Check proof of B assuming the types of another De Bruijn indix in
;; the environment
!(assertEqual
  (bc &kb (Cons (: (S' Z') A) Nil) Z' (fromNumber 2)
      (: ((ModusPonens (: ab (→ A B))) (: (S' Z') A)) B))
  (: ((ModusPonens (: ab (→ A B))) (: (S' Z') A)) B))

;; Check proof of B assuming the types of two De Bruijn indices in the
;; environment
!(assertEqual
  (bc &kb (Cons (: Z' (→ A B)) (Cons (: (S' Z') A) Nil)) Z' (fromNumber 2)
      (: ((ModusPonens (: Z' (→ A B))) (: (S' Z') A)) B))
  (: ((ModusPonens (: Z' (→ A B))) (: (S' Z') A)) B))

;; Find proof of (-> A B) with depth 3 restricted to lambda abstractions
!(assertEqualToResult
  (bc &kb Nil Z' (fromNumber 3) (: (λ Z' $prfbdy) (-> A B)))
  ((: (λ Z' ((ModusPonens (: ab (→ A B))) (: a A))) (-> A B))
   (: (λ Z' ((ModusPonens (: ab (→ A B))) (: Z' A))) (-> A B))))

;; Check proof involving nested lambda abstractions
!(assertEqual
  (bc &kb Nil Z' (fromNumber 4)
      (: (λ Z' (λ (S' Z') ((ModusPonens (: Z' (→ A B))) (: (S' Z') A))))
         (-> (→ A B) (-> A B))))
  (: (λ Z' (λ (S' Z') ((ModusPonens (: Z' (→ A B))) (: (S' Z') A))))
     (-> (→ A B) (-> A B))))

;; Check proof
!(bc &kb Nil Z' (fromNumber 4) (: $prf (-> (-> $b $c) (-> (-> $a $b) (-> $a $c)))))
     ;; (: (: (λ Z' (λ (S' Z') Z')) (-> (-> $b $c) (-> (-> $b $b) (-> $b $c))))
     ;;    (-> (-> $b $c) (-> (-> $b $b) (-> $b $c)))))

!(bc &kb Nil Z' (fromNumber 3)
     (: (λ Z' (λ (S' Z') (S' Z'))) (-> (-> $c $c) (-> (-> $c $c) (-> $c $c)))))

;;;;;;;;;;;;;;;;;;;;;
;; Remove/add type ;;
;;;;;;;;;;;;;;;;;;;;;

;; Remove type annotations from a query.
;;
;; For instance
;;
;; (remove-type (: ((ModusPonens (: ab (→ A B))) (: a A)) B))
;;
;; outputs
;;
;; ((ModusPonens ab) a)
(: remove-type (-> $a $a))
(= (remove-type $term)
   (case (get-metatype $term)
     ((Symbol $term)
      (Grounded $term)
      (Variable $term)
      (Expression (case $term
                    ;; Only covers up to trinary expression because
                    ;; for now the bc is not supposed to generate
                    ;; n-ary expression where n is greater 3.
                    ((() ())
                     (($x) ((remove-type $x)))
                     (($x $y) ((remove-type $x) (remove-type $y)))
                     (($x $y $z) (if (== $x :)
                                     (remove-type $y)
                                     ((remove-type $x) (remove-type $y) (remove-type $z))))
                     ($_ $_)))))))

;; Test remove-type over closed term
!(assertEqual
  (remove-type (: ((ModusPonens (: ab (→ A B))) (: a A)) B))
  ((ModusPonens ab) a))

;; Test remove-type over a term with open types
!(assertEqual
  (remove-type (: ((ModusPonens (: ab $lemma)) (: a A)) $thrm))
  ((ModusPonens ab) a))

;; Test remove-type over a term with open proofs
!(assertEqual
  (remove-type (: ((ModusPonens (: ab (→ A B))) (: $x A)) B))
  ((ModusPonens ab) $x))

;; Test remove-type over a term with lambda abstraction
!(assertEqual
  (remove-type (: (λ Z' ((ModusPonens (: ab (→ A B))) (: Z' A))) (-> A B)))
  (λ Z' ((ModusPonens ab) Z')))

;; Add type missing annotation to be compatible with bc query format.
;; Whatever is missing is only added as variables, not inferred, for
;; that one may use the bc.  Note that the first annotation must be
;; provided manually
;;
;; For instance
;;
;; (add-type ((ModusPonens ab) a))
;;
;; only outputs
;;
;; ((ModusPonens (: ab $t#1)) (: a $t#2))
;;
;; not
;;
;; (: ((ModusPonens (: ab $t#1)) (: a $t#2)) $t#3)
;;
;; That is because add-type can only detect application, that is a
;; term of the form (<ABS> <ARG>) to add the type annotation to <ARG>.
;;
;; Last but not least, De Bruijn indices are treated atomically,
;; meaning that the whole index is type annotated, not its
;; sub-indices.  For instance
;;
;; (add-type (λ Z' (λ (S' Z') ((ModusPonens (S' Z')) Z'))))
;;
;; outputs
;;
;; (λ Z' (λ (S' Z') ((ModusPonens (: (S' Z') $t1)) (: Z' $t2))))
;;
;; not
;;
;; (λ Z' (λ (S' Z') ((ModusPonens (: (S' (: Z' $t3)) $t1)) (: Z' $t2))))
(: add-type (-> $a $a))
(= (add-type $term)
   (case (get-metatype $term)
     ((Symbol $term)
      (Grounded $term)
      (Variable $term)
      (Expression (case $term
                    ((() ())
                     (($x) ($x))
                     (($x $y) (if (== $x S')
                                  ;; Treat De Bruijn index atomically
                                  $term
                                  ;; Add type annotation to argument of application
                                  (let $νx (add-type $x) ($νx (: (add-type $y) $t)))))
                     (($x $y $z) (if (== $x :)
                                     ;; Already a type annotation, recurse only on $y
                                     (: (add-type $y) $z)
                                     (if (== $x λ)
                                         ;; Recurse only on body of lambda abstraction
                                         (λ $y (add-type $z))
                                         $term)))
                     ($_ $_)))))))

;; Test add-type to ((ModusPonens ab) a)
;; Cannot use assertEqual because the variables are new
!(add-type ((ModusPonens ab) a))

;; Test add-type to (: ((ModusPonens ab) a) $thrm)
;; Cannot use assertEqual because the variables are new
!(add-type (: ((ModusPonens ab) a) $thrm))

;; Test add-type to lambda abstraction
!(add-type (λ Z' ((ModusPonens ab) Z')))

;; Test add-type to a multi lambda abstraction term
!(add-type (λ Z' (λ (S' Z') ((ModusPonens (S' Z')) Z'))))
