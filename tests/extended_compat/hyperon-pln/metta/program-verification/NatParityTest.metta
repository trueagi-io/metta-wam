;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example of using MeTTa to prove properties, here evenness, over
;; programs, here a doubling function.
;;
;; That file contains a backward chainer running on a logic including
;; equality, natural numbers, structural induction, dependent sum, the
;; even property and the double function over natural numbers, and
;; proves that doubling any natural number results into an even
;; number.
;;
;; Some remarks:
;;
;; To not interfere with MeTTa built-in type checker, naturals are
;; redefined with other symbols in the knowledge base handed to the
;; backward chainer.  Specifically Nat becomes ℕ, Z becomes 𝐙 and S
;; becomes 𝐒.  For that and other symbols, your editor needs to
;; support unicodes.
;;
;; To not be confused with MeTTa variables (starting with $),
;; variables defined by lambda abstractions are De Bruijn indices with
;; the following syntax: the zero index is represented by z, the
;; successor index function is represented by s.
;;
;; In order for the backward chainer to support lambda abstraction,
;; the terms must be fully annotated, for instance
;;
;; (S Z)
;;
;; becomes
;;
;; (: (S (: Z ℕ)) ℕ)
;;
;; We do intend to develop a version of the backward chainer that does
;; not require full type annotation, in the meantime you can use the
;; functions add-type-annotation and remove-type-annotation to add and
;; remove type annotations from terms.
;;
;; To speed up the backward chainer, reduction rules can be used,
;; however due to a current limitation of MeTTa (see
;; https://github.com/trueagi-io/hyperon-experimental/issues/674) only
;; a tiny subset is used in that experiment.
;;
;; The dependent sum (aka sigma type) representation is borrowed from
;; https://idris2.readthedocs.io/en/latest/tutorial/typesfuns.html#dependent-pairs
;;
;; The structural induction rule is slightly tailored for that
;; specific problem by requiring that the property be expressed as the
;; composition of two functions.  We intend to remedy that in the
;; future.
;;
;; Running the whole file takes about 1h30 on an Intel i9-10940X.
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
(: z DeBruijn)                        ; Zero
(: s (-> DeBruijn DeBruijn))          ; Successor

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

;; Curried Backward Chainer with lambda abstraction and dependent
;; types.  A lambda abstraction is represented by
;;
;; (λ <INDEX> <BODY>)
;;
;; where <INDEX> is a De Bruijn index as define above, such as z or
;; (s z), and <BODY> is a term possibly containing <INDEX>.  Note
;; that the use of De Bruijn index in lambda abstraction is somewhat
;; unconventional here.  It differs from what is described in
;; https://en.wikipedia.org/wiki/De_Bruijn_index in three ways:
;;
;; 1. The index is explicitely attached to a λ by being its first
;;    argument.  For instance the lambda term λx.x, which would
;;    traditionally be represented by λ1 using De Bruijn index, would
;;    be represented here by the MeTTa term (λ z z).
;;
;; 2. As seen in the example above the index here starts at 0,
;;    represented by z, instead of 1.
;;
;; 3. The index increases as the lambda abstraction gets deeper.  For
;;    instance λx.λy.x, which would traditionally be represented by
;;    λλ2 using De Bruijn index, is represented here by the MeTTa term
;;    (λ z (λ (s z) z).
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
;;   In addition, each premise must be an inline typing relationship,
;;   to represent dependent types.  It should be noted that such
;;   typing relationship must be provided even if the witness is not
;;   present in the rest of the definition.  For instance <PREMISE1>
;;   would have the format
;;
;;   (: <TERM1> <TYPE1>)
;;
;; * Environment: a list of typing relationship between De Bruijn
;;   index and type, such as
;;
;;   (Cons (: z String) (Cons (: (s z) Number) Nil))
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
;; remove-type-annotation (resp. add-type).
(: bc (-> $a                            ; Knowledge base space
          (List $b)                     ; Environment
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
   (let* (((: $prfabs (-> (: $prfarg $prms) $thrm))
           (bc $kb $env $idx $k (: $prfabs (-> (: $prfarg $prms) $thrm))))
          ((: $prfarg $prms)
           (bc $kb $env $idx $k (: $prfarg $prms))))
     (: ($prfabs (: $prfarg $prms)) $thrm)))

;; TODO: proof abstraction is necessary for structural induction but
;; makes everything run much slower.  If your proof does not require
;; lambda abstraction, you can disable that definition, which should
;; result in a substantial speed-up.
;;
;; Proof abstraction
(= (bc $kb $env $idx (S $k) (: (λ $idx $prfbdy) (-> (: $idx $prms) $thrm)))
   (let (: $prfbdy $thrm)
     (bc $kb (Cons (: $idx $prms) $env) (s $idx) $k (: $prfbdy $thrm))
     (: (λ $idx $prfbdy) (-> (: $idx $prms) $thrm))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge/rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

;; The following code is translated into axioms and rules to handled
;; by the backward chainer.
;;
;; ;; Define Σ type (called DPair in Idris)
;; (: Σ (-> (: $a Type) (-> $a Type) Type))
;;
;; ;; Define DPair constructor
;; (: MkΣ (-> (: $p (-> $a Type)) (: $x $a) ($p $x) (Σ $a $p)))
;;
;; ;; Define the even property
;; (: Even (-> Nat Type))
;; (: MkEvenZ (Even Z))
;; (: MkEvenSS (-> (Even $k) (Even (S (S $k)))))
;;
;; ;; Define examples of even numbers (0 and 2)
;; (: (MkΣ Even Z MkEvenZ) (Σ Nat Even))
;; (: (MkΣ Even (S (S Z)) (MkEvenSS MkEvenZ)) (Σ Nat Even))
;; (: $prf (Σ Nat Even))
;;
;; ;; Define double function
;; (: double (-> Nat Nat))
;; (= (double Z) Z)
;; (= (double (S $k)) (S (S (double $k))))
;;
;; ;; Σ access functions
;; (: Σ.val (-> (Σ $a $p) $a))
;; (= (Σ.val (MkΣ $prop $val $prf)) $val)
;; (: Σ.prf (-> (Σ $a $p) $p))
;; (= (Σ.prf (MkΣ $prop $val $prf)) $prf)
;;
;; ;; Define double function, with the guaranty that the output is even
;; (: doubleΣ (-> Nat (Σ Nat Even)))
;; (= (doubleΣ Z) (MkΣ Even Z MkEvenZ))
;; (= (doubleΣ (S $k)) (MkΣ Even
;;                          (S (S (Σ.val (doubleΣ $k))))
;;                          (MkEvenSS (Σ.prf (doubleΣ $k)))))
;;
;; ;; For all x, (double x) is even
;; ;; ∀x (Even (double x))
;; (: double-even-prf (-> (: $x Nat) (Even (double $x))))
;;
;; ;; Independent product type (i.e. conjunction)
;; (: ⊗ (-> Type Type Type))
;;
;; ;; Equality
;; (: === (-> $a $a Type))
;;
;; ;; ;; TODO
;;
;; ;; ;; For all x, there exists k such that k = (double x) and k is even
;; ;; ;; ∀x ∃k k=(double x) ∧ (Even k)
;; ;; (: double-Σ-even-prf (-> ($x : Nat)
;; ;;                          (Σ Nat (λ $k (⊗ (=== $k (double $x)) (Even $k))))))
;;
;; ;; (: double-Σ-even-prf (-> ($x : Nat) (Σ Nat (EqualDoubleAndEven $x))))
;; ;; (: EqualDoubleAndEven (-> Nat (-> Nat Type)))
;; ;; (= ((EqualDoubleAndEven $x) $k) (⊗ (=== $k (double $x)) (Even $k)))
;; ;; ;; Or, alternatively
;; ;; ;; (= (EqualDoubleAndEven $x) (λ $k (× (=== $k (double $x)) (Even $k))))

;; Define Nat, called ℕ, with its constructors 𝐙 and 𝐒 to not have
;; MeTTa type checker interfere with the backward chainer
!(add-atom &kb (: ℕ Type))
!(add-atom &kb (: 𝐙 ℕ))
!(add-atom &kb (: 𝐒 (-> (: $x ℕ) ℕ)))

;; Define Even
!(add-atom &kb (: Even (-> (: $x ℕ) Type)))
!(add-atom &kb (: MkEvenZ (Even 𝐙)))
!(add-atom &kb (: MkEvenSS (-> (: $prf (Even $k))               ; Premise
                               (Even (𝐒 (: (𝐒 (: $k ℕ)) ℕ)))))) ; Conclusion

;; Define Σ
!(add-atom &kb
           (: MkΣ
              (-> (: $p (-> (: $_ $a) Type)) ; Premise 1.  We
                                             ;   need to use (: $_ $a)
                                             ;   as opposed to just $a
                                             ;   to be fully consistent with
                                             ;   The (: PROOF PREMISE)
                                             ;   notation, till it becomes
                                             ;   optional.
                  (-> (: $x $a)              ; Premise 2
                      (-> (: $prf ($p $x))   ; Premise 3
                          (Σ $a $p))))))     ; Conclusion

;; Structural induction on ℕ.  TODO: remove $q.  This has been added
;; to deal with (Even (double $x)), thus $p would be Even and $q
;; double.  Instead $q should be removed and the property should be
;; expressed with combinators, like (Even . double), or lambda
;; abstractions, like (λ z (Even (double z))).
!(add-atom &kb
           (: SIℕ
              (-> (: $prfZ ($p ($q (: 𝐙 ℕ))))                    ; Premise 1.  (p . q) is true for 𝐙.
                  (-> (: $prfI                                   ; Premise 2.
                         (-> (: $x ℕ)                            ;   Given any natural x,
                             (-> (: $hyp ($p ($q (: $x ℕ))))     ;   if (p . q) is true for x, then
                                 ($p ($q (: (𝐒 (: $x ℕ)) ℕ)))))) ;   it is true for (𝐒 x).
                      (-> (: $k ℕ)                               ; Conclusion.  Given any natural k
                          ($p ($q (: $k ℕ))))))))                ;   (p . q) is true for k.

;; Refl is disabled because it is not needed in that experiment
;;
;; ;; Equality is reflexive
;; !(add-atom &kb (: Refl (=== $x $x)))

;; Equality is transitive
!(add-atom &kb (: Trans (-> (: $prf1 (=== $x $y))     ; Premise 1
                            (-> (: $prf2 (=== $y $z)) ; Premise 2
                                (=== $x $z)))))       ; Conclusion

;; Equality is symmetric
!(add-atom &kb (: Sym (-> (: $prf (=== $x $y)) ; Premise
                          (=== $y $x))))       ; Conclusion

;; Equality respects function application
!(add-atom &kb (: Cong (-> (: $f (-> (: $_ $a) $b))                 ; Premise 1
                           (-> (: $x $a)                            ; Premise 2
                               (-> (: $x' $a)                       ; Premise 3
                                   (-> (: $prf (=== $x $x'))        ; Premise 4
                                       (=== ($f $x) ($f $x')))))))) ; Conclusion

;; Rule of replacement
!(add-atom &kb (: Replace (-> (: $prf1 (=== $x $x')) ; Premise 1
                              (-> (: $prf2 $x)       ; Premise 2
                                  $x'))))            ; Conclusion

;; Define double
!(add-atom &kb (: double (-> (: $k ℕ) ℕ)))
!(add-atom &kb (: double_base (=== (double (: 𝐙 ℕ)) 𝐙)))
!(add-atom &kb (: double_rec (-> (: $k ℕ)
                                 (=== (double (: (𝐒 (: $k ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: $k ℕ)) ℕ)) ℕ))))))

;;;;;;;;;;;;;;;
;; Reduction ;;
;;;;;;;;;;;;;;;

;; Reduction rules to simplify proofs and reduce redundancy

;; TODO: these rules should be proven first.  Then they could
;; automatically be inserted.

;; ;; Involution of symmetry
;; ;; (= (Sym (Sym $f)) $f)
;; (= (: (Sym (: (Sym (: $x $a)) $b)) $c) (: $x $a))

;; ;; Involution of Trans and Sym
;; ;; (= ((Trans $x) (Sym $x)) $x)
;; ;; (= ((Trans (Sym $x)) $x) $x)
;; (= (: ((Trans (: $x $a)) (: (Sym (: $x $a)) $b)) $c) (: $x $a))
;; (= (: ((Trans (: (Sym (: $x $a)) $b)) (: $x $a)) $c) (: $x $a))

;; Identity
(= ((λ $x $x) $y) $y)

;; Constant function
(= ((λ (s z) z) $x) z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove/add type annotation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove type annotation from a query.
;;
;; For instance
;;
;; (remove-type-annotation (: ((ModusPonens (: ab (→ A B))) (: a A)) B))
;;
;; outputs
;;
;; ((ModusPonens ab) a)
(: remove-type-annotation (-> $a $a))
(= (remove-type-annotation $term)
   (case (get-metatype $term)
     ((Symbol $term)
      (Grounded $term)
      (Variable $term)
      (Expression (case $term
                    ;; Only covers up to trinary expression because
                    ;; for now the bc is not supposed to generate
                    ;; n-ary expression where n is greater 3.
                    ((() ())
                     (($x) ((remove-type-annotation $x)))
                     (($x $y) ((remove-type-annotation $x) (remove-type-annotation $y)))
                     (($x $y $z) (if (== $x :)
                                     (remove-type-annotation $y)
                                     ((remove-type-annotation $x)
                                      (remove-type-annotation $y)
                                      (remove-type-annotation $z))))
                     ($_ $_)))))))

;; Add type missing annotation to be compatible with bc query format.
;; Whatever is missing is only added as variables, not inferred, for
;; that one may use the bc.  Note that the first annotation must be
;; provided manually
;;
;; For instance
;;
;; (add-type-annotation ((ModusPonens ab) a))
;;
;; only outputs
;;
;; ((ModusPonens (: ab $t#1)) (: a $t#2))
;;
;; not
;;
;; (: ((ModusPonens (: ab $t#1)) (: a $t#2)) $t#3)
;;
;; That is because add-type-annotation can only detect application, that is a
;; term of the form (<ABS> <ARG>) to add the type annotation to <ARG>.
;;
;; TODO: must keep track of the existing type to remain consistant.
(: add-type-annotation (-> $a $a))
(= (add-type-annotation $term)
   (case (get-metatype $term)
     ((Symbol $term)
      (Grounded $term)
      (Variable $term)
      (Expression (case $term
                    ((() ())
                     (($x) ($x))
                     (($x $y) (if (== $x s)
                                  ;; Treat De Bruijn index atomically
                                  $term
                                  ;; Add type annotation to argument of application
                                  (let $νx (add-type-annotation $x) ($νx (: (add-type-annotation $y) $t)))))
                     (($x $y $z) (if (== $x :)
                                     ;; Already a type annotation, recurse only on $y
                                     (: (add-type-annotation $y) $z)
                                     (if (== $x λ)
                                         ;; Recurse only on body of lambda abstraction
                                         (λ $y (add-type-annotation $z))
                                         $term)))
                     ($_ $_)))))))

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

;; Synthesize natural numbers
!(assertEqualToResult
  (bc &kb Nil z (fromNumber 1) (: $prf ℕ))
  ((: (𝐒 (: 𝐙 ℕ)) ℕ)
   (: (double (: 𝐙 ℕ)) ℕ)
   (: 𝐙 ℕ)))

;; Prove that 0 is even
!(assertEqual
  (bc &kb Nil z (fromNumber 0) (: $prf (Even 𝐙)))
  (: MkEvenZ (Even 𝐙)))

;; Prove that 2 is even
!(assertEqual
  (bc &kb Nil z (fromNumber 1) (: $prf (Even (𝐒 (: (𝐒 (: 𝐙 ℕ)) ℕ)))))
  (: (MkEvenSS (: MkEvenZ (Even 𝐙))) (Even (𝐒 (: (𝐒 (: 𝐙 ℕ)) ℕ)))))

;; Synthesize even numbers.  With a depth of 3 it can only synthesize
;; zero.  To synthesize 2 it needs a depth of 4 but it takes 15
;; minutes.
;;
;; TODO: needs alpha equivalence
;; !(assertEqual
  !(bc &kb Nil z (fromNumber 3) (: $prf (Σ ℕ Even)))
  ;; (: (((MkΣ (: Even (-> (: $_ ℕ) Type))) (: 𝐙 ℕ)) (: MkEvenZ (Even 𝐙))) (Σ ℕ Even)))

;; Synthesize unary functions over natural numbers, that is
;; prove (-> (: $k ℕ) ℕ).
!(assertEqualToResult
  (bc &kb Nil z (fromNumber 1) (: $prf (-> (: $k ℕ) ℕ)))
  ((: (λ z 𝐙) (-> (: z ℕ) ℕ))
   (: (λ z z) (-> (: z ℕ) ℕ))
   (: 𝐒 (-> (: $k ℕ) ℕ))
   (: double (-> (: $k ℕ) ℕ))))

;; Synthesize endofunctions, that is prove (-> (: $x $a) $a).
;;
;; TODO: the list is too big to be listed.  Re-enable tests when we
;; have assertContain or such.
!(bc &kb Nil z (fromNumber 1) (: $prf (-> (: $x $a) $a)))

;; ;; TODO: disabled because it takes too long
;; ;; Synthesize the composition operator
;; !(bc &kb Nil z (fromNumber 4)
;;      (: $prf (-> (: $g (-> (: $y $b) $c))
;;                  (-> (: $f (-> (: $x $a) $b))
;;                      (-> (: $x $a) $c)))))

;; ;; TODO: disabled because it takes too long
;; ;; Synthesize the flip operator, that is prove
;; ;;
;; ;; (-> (: $f (-> (: $x $a) (-> (: $y $b) $c)))
;; ;;     (-> (: $y $b) (-> (: $x $a) $c)))))
;; !(bc &kb Nil z (fromNumber 5)
;;      (: $prf (-> (: $f (-> (: $x $a) (-> (: $y $b) $c)))
;;                  (-> (: $y $b) (-> (: $x $a) $c)))))

;; Prove that Even is a type constructor that takes a natural
;;
;; A possible proof of that is represented by the following proof tree:
;;
;; -----------------(Even)
;; (-> (: $_ ℕ) Type)
;;
!(assertEqual
  (bc &kb Nil z (fromNumber 0) (: $prf (-> (: $_ ℕ) Type)))
  (: Even (-> (: $_ ℕ) Type)))

;; Verify that 𝐙 is a natural
;;
;; A possible proof of that is represented by the following proof tree:
;;
;; -(𝐙)
;; ℕ
;;
!(assertEqual
  (bc &kb Nil z (fromNumber 0) (: 𝐙 ℕ))
  (: 𝐙 ℕ))

;; Verify that (double 𝐙) is a natural.
;;
;; A possible proof of that is represented by the following proof tree:
;;
;;  -(𝐙)
;;  ℕ
;;  -(double)
;;  ℕ
;;
!(assertEqual
  (bc &kb Nil z (fromNumber 1) (: (double (: 𝐙 ℕ)) ℕ))
  (: (double (: 𝐙 ℕ)) ℕ))

;; Prove that 𝐙 = (double 𝐙).
;;
;; A possible proof of that is represented by the following proof tree:
;;
;; -----------------(double_base)
;; (=== (double 𝐙) 𝐙)
;; -----------------(Sym)
;; (=== 𝐙 (double 𝐙))
;;
;; Or in MeTTa format:
;;
;; (: (Sym double_base) (=== 𝐙 (double 𝐙)))
!(assertEqual
  (bc &kb Nil z (fromNumber 1) (: $prf (=== 𝐙 (double (: 𝐙 ℕ)))))
  (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙)))
     (=== 𝐙 (double (: 𝐙 ℕ)))))

;; ;; TODO: re-enabled once reduction works again
;; ;; Like above with depth of 3.  Used to test reduction rules.
;; ;; !(assertEqual
;;   !(bc &kb Nil z (fromNumber 3) (: $prf (=== 𝐙 (double (: 𝐙 ℕ)))))
;;   ;; (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙)))
;;   ;;    (=== 𝐙 (double (: 𝐙 ℕ)))))

;; Infer the theorem that (Cong Even) proves.
;;
;; A possible proof of that is represented by the following proof tree:
;;
;; -----------------(Even)
;; (-> (: $_ ℕ) Type)
;; ---------------------------------------------------------------------------------(Cong)
;; (-> (: $x ℕ) (-> (: $x' ℕ) (-> (: $prf (=== $x $x')) (=== (Even $x) (Even $x')))))
;;
;; TODO: use assertEqual once we support alpha-equivalence
;; !(assertEqual
!(bc &kb Nil z (fromNumber 1) (: (Cong (: Even $t)) $thrm))
  ;; (: (Cong (: Even (-> (: $_ ℕ) Type)))
  ;;    (-> (: $x ℕ)
  ;;        (-> (: $x' ℕ)
  ;;            (-> (: $prf (=== $x $x'))
  ;;                (=== (Even $x) (Even $x'))))))

;; Infer the theorem that (Cong Even 𝐙) proves.
;;
;; A possible proof of that is represented by the following proof tree:
;;
;; -----------------(Even) -(𝐙)
;; (-> (: $_ ℕ) Type)      ℕ
;; -------------------------------------------------------------------(Cong)
;; (-> (: $x' ℕ) (-> (: $prf (=== 𝐙 $x')) (=== (Even 𝐙) (Even $x'))))
;;
;; TODO: use assertEqual once we support alpha-equivalence
;; !(assertEqual
!(bc &kb Nil z (fromNumber 2) (: ((Cong (: Even $a)) (: 𝐙 $b)) $thrm))
  ;; (: ((Cong (: Even (-> (: $_ ℕ) Type))) (: 𝐙 ℕ))
  ;;    (-> (: $x' ℕ)
  ;;        (-> (: $prf (=== 𝐙 $x'))
  ;;            (=== (Even 𝐙) (Even $x')))))

;; Infer the theorem that (Cong Even 𝐙 (double 𝐙)) proves.
;;
;; A possible proof of that is represented by the following proof tree:
;;
;;                              -(𝐙)
;;                              ℕ
;; -----------------(Even) -(𝐙) -(double)
;; (-> (: $_ ℕ) Type)      ℕ    ℕ
;; -----------------------------------------------------------------(Cong)
;; (-> (: $prf (=== 𝐙 (double 𝐙))) (=== (Even 𝐙) (Even (double 𝐙))))
;;
;; TODO: use assertEqual once we support alpha-equivalence
;; !(assertEqual
!(bc &kb
     Nil
     z
     (fromNumber 3)
     (: (((Cong (: Even $a)) (: 𝐙 $b)) (: (double (: 𝐙 $c)) $d)) $thrm))
  ;; (: (((Cong (: Even (-> (: $_ ℕ) Type))) (: 𝐙 ℕ)) (: (double (: 𝐙 ℕ)) ℕ))
  ;;    (-> (: $prf (=== 𝐙 (double (: 𝐙 ℕ))))
  ;;        (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))

;; Find proof of (-> (: $hyp (=== 𝐙 (double 𝐙))) (=== (Even 𝐙) (Even (double 𝐙))))
;;
;; A possible proof of that is represented by the following proof tree:
;;
;;                              -(𝐙)
;;                              ℕ
;; -----------------(Even) -(𝐙) -(double)
;; (-> (: $_ ℕ) Type)      ℕ    ℕ
;; -----------------------------------------------------------------(Cong)
;; (-> (: $hyp (=== 𝐙 (double 𝐙))) (=== (Even 𝐙) (Even (double 𝐙))))
;;
;; TODO: use assertEqual once we support alpha-equivalence
;; !(assertEqual
  !(bc &kb
      Nil
      z
      (fromNumber 3)
      (: $prf (-> (: $eq (=== 𝐙 (double (: 𝐙 ℕ)))) (=== (Even 𝐙) (Even (double (: 𝐙 ℕ)))))))
  ;; (: (((Cong (: Even (-> (: $_ ℕ) Type))) (: 𝐙 ℕ)) (: (double (: 𝐙 ℕ)) ℕ))
  ;;    (-> (: $eq (=== 𝐙 (double (: 𝐙 ℕ))))
  ;;        (=== (Even 𝐙) (Even (double (: 𝐙 ℕ)))))))

;; Infer the theorem that (Cong Even 𝐙 (double 𝐙) (Sym double_base)) proves.
;;
;; A possible proof of that is represented by the following proof tree:
;;
;;                               -(𝐙)      -----------------(double_base)
;;                               ℕ         (=== (double 𝐙) 𝐙)
;; -----------------(Even) -(𝐙)  -(double) -----------------(Sym)
;; (-> (: $_ ℕ) Type)      ℕ     ℕ         (=== 𝐙 (double 𝐙))
;; ---------------------------------------------------------(Cong)
;;                (=== (Even 𝐙) (Even (double 𝐙)))
;;
;; TODO: use assertEqual once we support alpha-equivalence
;; !(assertEqual
!(bc &kb
     Nil
     z
     (fromNumber 4)
     (: ((((Cong
            (: Even $a))
           (: 𝐙 $b))
          (: (double (: 𝐙 $c)) $d))
         (: (Sym (: double_base $e)) $f))
        $thrm))
  ;; (: ((((Cong
  ;;        (: Even (-> (: $_ ℕ) Type)))
  ;;       (: 𝐙 ℕ))
  ;;      (: (double (: 𝐙 ℕ)) ℕ))
  ;;     (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
  ;;    (=== (Even 𝐙) (Even (double (: 𝐙 ℕ)))))

;; Find the proof of (=== (Even 𝐙) (Even (double (: 𝐙 ℕ)))) starting
;; by Cong.
;;
;; A possible proof of that is represented by the following proof tree:
;;
;;                               -(𝐙)      -----------------(double_base)
;;                               ℕ         (=== (double 𝐙) 𝐙)
;; -----------------(Even) -(𝐙)  -(double) -----------------(Sym)
;; (-> (: $_ ℕ) Type)      ℕ     ℕ         (=== 𝐙 (double 𝐙))
;; ----------------------------------------------------------(Cong)
;;                (=== (Even 𝐙) (Even (double 𝐙)))
;;
;; TODO: use assertEqual once we support alpha-equivalence
;; !(assertEqual
!(bc &kb
     Nil
     z
     (fromNumber 4)
     (: ((((Cong
            $prf1)
           $prf2)
          $prf3)
         $prf4)
        (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
  ;; (: ((((Cong
  ;;        (: Even (-> (: $_ ℕ) Type)))
  ;;       (: 𝐙 ℕ))
  ;;      (: (double (: 𝐙 ℕ)) ℕ))
  ;;     (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
  ;;    (=== (Even 𝐙) (Even (double (: 𝐙 ℕ)))))

;; ;; TODO: disabled because it takes too long.  To make it fast one
;; ;; needs to disabled lambda abstraction.
;; ;;
;; ;; Find the proof of (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))
;; ;;
;; ;; A possible proof of that is represented by the following proof tree:
;; ;;
;; ;;                               -(𝐙)      -----------------(double_base)
;; ;;                               ℕ         (=== (double 𝐙) 𝐙)
;; ;; -----------------(Even) -(𝐙)  -(double) -----------------(Sym)
;; ;; (-> (: $_ ℕ) Type)      ℕ     ℕ         (=== 𝐙 (double 𝐙))
;; ;; ---------------------------------------------------------(Cong)
;; ;;                (=== (Even 𝐙) (Even (double 𝐙)))
;; ;;
;; ;; TODO: use assertEqual once we support alpha-equivalence
;; ;; !(assertEqual
;; !(bc &kb
;;      Nil
;;      z
;;      (fromNumber 4)
;;      (: $prf (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
;;   ;; (: ((((Cong
;;   ;;        (: Even (-> (: $_ ℕ) Type)))
;;   ;;       (: 𝐙 ℕ))
;;   ;;      (: (double (: 𝐙 ℕ)) ℕ))
;;   ;;     (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
;;   ;;    (=== (Even 𝐙) (Even (double (: 𝐙 ℕ)))))

;; Verify that
;;
;; ((Replace ((((Cong Even) 𝐙) (double 𝐙)) (Sym double_base))))
;;
;; proves that (double 𝐙) is even, corresponding to the proof tree
;;
;;                         -(𝐙)       -----------------(double_base)
;;                         ℕ          (=== (double 𝐙) 𝐙)
;; ----------(Even)  -(𝐙)  -(double)  -----------------(Sym)
;; (-> ℕ Type)       ℕ     ℕ          (=== 𝐙 (double 𝐙))
;;  ---------------------------------------------------(Cong)  -------(MkEvenZ)
;;           (=== (Even 𝐙) (Even (double 𝐙)))                  (Even Z)
;; ---------------------------------------------------------------------(Replace)
;;                             (Even (double 𝐙))
;;
;; TODO: use alpha-equivalence
;; !(assertAlphaEqual
  !(bc &kb
      Nil
      z
      (fromNumber 6)
      (: ((Replace
           (: ((((Cong
                  (: Even $a))
                 (: 𝐙 $b))
                (: (double (: 𝐙 $c)) $d))
               (: (Sym (: double_base $e)) $f))
              $h))
          (: MkEvenZ $i))
         (Even (double (: 𝐙 ℕ)))))
  ;; (: ((Replace
  ;;      (: ((((Cong
  ;;             (: Even (-> (: $_ ℕ) Type)))
  ;;            (: 𝐙 ℕ))
  ;;           (: (double (: 𝐙 ℕ)) ℕ))
  ;;          (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
  ;;         (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
  ;;     (: MkEvenZ (Even 𝐙)))
  ;;    (Even (double (: 𝐙 ℕ)))))

;; ;; TODO: disabled because it takes too long.  To make it fast one
;; ;; needs to disabled lambda abstraction.
;; ;;
;; ;; Prove that (double 𝐙) is even.
;; ;;
;; ;; A possible proof of that is represented by the following proof tree:
;; ;;
;; ;;                               -(𝐙)      -----------------(double_base)
;; ;;                               ℕ         (=== (double 𝐙) 𝐙)
;; ;; -----------------(Even) -(𝐙)  -(double) -----------------(Sym)
;; ;; (-> (: $_ ℕ) Type)      ℕ     ℕ         (=== 𝐙 (double 𝐙))
;; ;; ----------------------------------------------------------(Cong)  -------(MkEvenZ)
;; ;;                (=== (Even 𝐙) (Even (double 𝐙)))                   (Even 𝐙)
;; ;;                ----------------------------------------------------------(Replace)
;; ;;                                       (Even (double 𝐙))
;; ;;
;; ;; Or in MeTTa format:
;; ;;
;; ;; (: ((Replace ((((Cong Even) 𝐙) (double 𝐙)) (Sym double_base))) MkEvenZ) (Even (double 𝐙)))
;; ;;
;; ;; TODO: re-enable with alpha-equivalence
;; ;; !(assertEqual
;;   !(bc &kb Nil z (fromNumber 6) (: $prf (Even (double (: 𝐙 ℕ)))))
;;   ;; (: ((Replace
;;   ;;      (: ((((Cong
;;   ;;             (: Even (-> (: $_ ℕ) Type)))
;;   ;;            (: 𝐙 ℕ))
;;   ;;           (: (double (: 𝐙 ℕ)) ℕ))
;;   ;;          (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙)))
;;   ;;             (=== 𝐙 (double (: 𝐙 ℕ)))))
;;   ;;         (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
;;   ;;     (: MkEvenZ (Even 𝐙)))
;;   ;;    (Even (double (: 𝐙 ℕ)))))

;; Prove
;;
;; (-> (: Even (-> (: $_ ℕ) Type))
;;     (-> (: (double (𝐒 $k)) ℕ)
;;         (-> (: (𝐒 (𝐒 (double $k))) ℕ)
;;             (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;;                 (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))))
;;
;; which should merely be Cong.
!(assertEqual
  (bc &kb Nil z (fromNumber 0)
      (: $prf (-> (: Even (-> (: $_ ℕ) Type))
                  (-> (: (double (𝐒 $k)) ℕ)
                      (-> (: (𝐒 (𝐒 (double $k))) ℕ)
                          (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
                              (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))))))
  (: Cong (-> (: Even (-> (: $_ ℕ) Type))
              (-> (: (double (𝐒 $k)) ℕ)
                  (-> (: (𝐒 (𝐒 (double $k))) ℕ)
                      (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
                          (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))))))

;; Prove
;;
;; (-> (: (double (𝐒 $k)) ℕ)
;;     (-> (: (𝐒 (𝐒 (double $k))) ℕ)
;;         (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;;             (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))))
;;
;; which should be (Cong Even).
;; TODO: re-enable after introducting assertAlphaEqual
;; !(assertEqual
  (bc &kb Nil z (fromNumber 1)
      (: $prf (-> (: (double (𝐒 $k)) ℕ)
                  (-> (: (𝐒 (𝐒 (double $k))) ℕ)
                      (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
                          (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))))))
  ;; (: (Cong (: Even (-> (: $_ ℕ) Type)))
  ;;    (-> (: (double (𝐒 $k)) ℕ)
  ;;        (-> (: (𝐒 (𝐒 (double $k))) ℕ)
  ;;            (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
  ;;                (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))))))

;; Type annotate (λ z (((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))))
;; TODO: re-enable once assertAlphaEqual is supported
;; !(assertEqual
  !(add-type-annotation (λ z (((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z))))))
  ;; (λ z (((Cong (: Even $a)) (: (double (: (𝐒 (: z $b)) $c)) $d)) (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h))))

;; Infer theorem of proof
;;
;; (λ z (((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))))
;;
;; which should be
;;
;; (-> (: $k ℕ)
;;     (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;;         (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))
;;
;; TODO: re-enable once assertAlphaEqual is supported
;; !(assertEqual
  !(bc &kb Nil z (fromNumber 5)
      (: (λ z (((Cong (: Even $a)) (: (double (: (𝐒 (: z $b)) $c)) $d)) (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h)))
         $thrm))
  ;; (: (λ z (((Cong (: Even (-> (: $_ ℕ) Type))) (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ)) (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ)))
  ;;    (-> (: z ℕ)
  ;;        (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
  ;;            (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))

;; Prove
;;
;; (-> (: $k ℕ)
;;     (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;;         (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))
;;
;; giving (λ z (((Cong ...) ...) ...)) as clue, which could be
;;
;; (λ z (((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))))
;; !(assertAlphaEqual
 !(bc &kb Nil z (fromNumber 5)
      (: (λ z (((Cong $prf1) $prf2) $prf3))
         (-> (: z ℕ)
             (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
                 (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))
  ;; (: (λ z (((Cong (: Even (-> (: $_ ℕ) Type)))
  ;;           (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
  ;;          (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ)))
  ;;    (-> (: z ℕ)
  ;;        (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
  ;;            (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))

;; Prove
;;
;; (-> (: $k ℕ)
;;     (-> (: Even (-> (: $_ ℕ) Type))
;;         (-> (: (double (𝐒 $k)) ℕ)
;;             (-> (: (𝐒 (𝐒 (double $k))) ℕ)
;;                 (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;;                     (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))))))
;;
;; A possible proof could be
;;
;; (λ $k Cong)
!(assertEqual
  (bc &kb Nil z (fromNumber 1)
      (: $prf
         (-> (: $k ℕ)
             (-> (: Even (-> (: $_ ℕ) Type))
                 (-> (: (double (: (𝐒 (: $k ℕ)) ℕ)) ℕ)
                     (-> (: (𝐒 (: (𝐒 (: (double (: $k ℕ)) ℕ)) ℕ)) ℕ)
                         (-> (: $eq (=== (double (: (𝐒 (: $k ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: $k ℕ)) ℕ)) ℕ))))
                             (=== (Even (double (: (𝐒 (: $k ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: $k ℕ)) ℕ)) ℕ)))))))))))
  (: (λ z Cong)
     (-> (: z ℕ)
         (-> (: Even (-> (: $_ ℕ) Type))
             (-> (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ)
                 (-> (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ)
                     (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
                         (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))))))

;; Add type annotation to (λ z ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))
;; TODO: re-enable once we have assertAlphaEqual
;; !(assertEqual
  !(add-type-annotation (λ z ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z))))
  ;; (λ z ((((Cong (: Even $a)) (: (double (: (𝐒 (: z $b)) $c)) $d)) (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h)) (: (double_rec (: z $i)) $j))))

;; ;; TODO: disabled because it takes too long.
;; ;;
;; ;; Prove
;; ;;
;; ;; (-> (: $k ℕ)
;; ;;     (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;; ;;         (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))
;; ;;
;; ;; giving (λ z ...) as clue, which could be
;; ;;
;; ;; (λ z (((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))))
;; ;; !(assertAlphaEqual
;;   !(bc &kb Nil z (fromNumber 5)
;;        (: (λ z $bdy)
;;           (-> (: z ℕ)
;;               (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
;;                   (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))
;;   ;; (: (λ z (((Cong (: Even (-> (: $_ ℕ) Type)))
;;   ;;           (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
;;   ;;          (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ)))
;;   ;;    (-> (: z ℕ)
;;   ;;        (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
;;   ;;            (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))

;; ;; TODO: disabled because it takes too long.
;; ;;
;; ;; Prove
;; ;;
;; ;; (-> (: $k ℕ)
;; ;;     (-> (: $eq (=== (double (𝐒 $k)) (𝐒 (𝐒 (double $k)))))
;; ;;         (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))))
;; ;;
;; ;; which could be
;; ;;
;; ;; (λ $k (((Cong Even) (double (𝐒 $k))) (𝐒 (𝐒 (double $k)))))
;; ;;
;; ;; TODO: re-enable once assertAlphaEqual is supported
;; ;; !(assertEqual
;;   !(bc &kb Nil z (fromNumber 5)
;;        (: $prf
;;           (-> (: z ℕ)
;;               (-> (: $eq (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
;;                   (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))

;; Prove
;;
;; (-> (: z ℕ)
;;     (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))
;;
;; which should merely be double_rec
!(assertEqual
  (bc &kb Nil z (fromNumber 0)
      (: $prf
         (-> (: z ℕ)
             (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
  (: double_rec (-> (: z ℕ) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))

;; Verify that
;;
;; (λ z ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))
;;
;; is a proof of
;;
;; (-> (: $k ℕ) (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))
!(assertEqual
  (bc &kb Nil z (fromNumber 6)
      (: (λ z ((((Cong (: Even (-> (: $_ ℕ) Type)))
                 (: (double (: (𝐒 (: z $b)) $c)) $d))
                (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h))
               (: (double_rec (: z $i)) $j)))
         (-> (: z ℕ)
             (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
  (: (λ z ((((Cong
              (: Even (-> (: $_ ℕ) Type)))
             (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
            (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
           (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
     (-> (: z ℕ) (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))

;; Prove
;;
;; (-> (: $k ℕ) (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))
;;
;; giving (λ z ((((Cong $prf1) $prf2) $prf3) $prf4)) as clue
;;
;; A possible proof could be
;;
;; (λ z ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))
;;
;; Beware that this may take half an hour.
;;
!(bc &kb Nil z (fromNumber 6)
     (: (λ z ((((Cong $prf1) $prf2) $prf3) $prf4))
        (-> (: z ℕ)
            (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))

;; ;; TODO: disabled because it takes too long.
;; ;;
;; ;; Prove
;; ;;
;; ;; (-> (: $k ℕ) (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k))))))
;; ;;
;; ;; giving (λ z $bdy) as clue
;; ;;
;; ;; A possible proof could be
;; ;;
;; ;; (λ z ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))
;; !(bc &kb Nil z (fromNumber 6)
;;      (: (λ z $bdy)
;;         (-> (: z ℕ)
;;             (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))

;; Verify that
;;
;; (λ z (Sym ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z))))
;;
;; is a proof of
;;
;; (-> (: $k ℕ) (=== (Even (𝐒 (𝐒 (double $k)))) (Even (double (𝐒 $k)))))
!(assertEqual
  (bc &kb Nil z (fromNumber 7)
      (: (λ z (Sym (: ((((Cong (: Even (-> (: $_ ℕ) Type)))
                         (: (double (: (𝐒 (: z $b)) $c)) $d))
                        (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h))
                       (: (double_rec (: z $i)) $j)) $k)))
         (-> (: z ℕ)
             (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
  (: (λ z (Sym (: ((((Cong (: Even (-> (: $_ ℕ) Type)))
                     (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
                    (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
                   (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
                  (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
     (-> (: z ℕ) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))

;; Verify that
;;
;; (λ z (Replace (Sym ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))))
;;
;; is a proof of
;;
;; (-> (: z ℕ) (-> (: $e (Even (𝐒 (𝐒 (double z))))) (Even (double (𝐒 z)))))
;;
;; corresponding to the following proof tree
;;
;;                                                   -(z)
;;                                                   ℕ
;;                                         -(z)      -(double)
;;                                         ℕ          ℕ
;;                                         -(𝐒)       -(𝐒)                      -(z)
;;                                         ℕ          ℕ                         ℕ
;;                -----------------(Even)  -(double)  -(𝐒)  --------------------------------------(double_rec)
;;                (-> (: $_ ℕ) Type)       ℕ          ℕ     (=== (double (𝐒 z)) (𝐒 (𝐒 (double z))))
;;                --------------------------------------------------------------------------------(Cong)
;;                (=== (Even (double (𝐒 z))) (Even (𝐒 (𝐒 (double z)))))
;;                ----------------------------------------------------(Sym)
;;                (=== (Even (𝐒 (𝐒 (double z)))) (Even (double (𝐒 z))))
;; -(z)         ----------------------------------------------------------(Replace)
;; ℕ            (-> (: $e (Even (𝐒 (𝐒 (double z))))) (Even (double (𝐒 z))))
;; -----------------------------------------------------------------------(λ)
;; (-> (: z ℕ) (-> (: $e (Even (𝐒 (𝐒 (double z))))) (Even (double (𝐒 z)))))
;;
;; Before checking, let us type annotate the proof (disabled till assertAlphaEqual is support)
;; !(assertAlphaEqual
;;   (add-type-annotation (λ z (Replace (Sym ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z))))))
;;   (λ z (Replace (: (Sym (: ((((Cong (: Even $a)) (: (double (: (𝐒 (: z $b)) $c)) $d)) (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h)) (: (double_rec (: z $i)) $j)) $k)) $l))))
!(assertEqual
  (bc &kb Nil z (fromNumber 8)
      (: (λ z
           (Replace
            (: (Sym
                (: ((((Cong (: Even (-> (: $_ ℕ) Type)))
                      (: (double (: (𝐒 (: z $b)) $c)) $d))
                     (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h))
                    (: (double_rec (: z $i)) $j))
                   $k))
               $l)))
         (-> (: z ℕ) (-> (: $prf (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
  (: (λ z
       (Replace
        (: (Sym
            (: ((((Cong (: Even (-> (: $_ ℕ) Type)))
                  (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
                 (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
                (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
               (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
           (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
     (-> (: z ℕ) (-> (: $prf (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))

;; Prove that if (double z) is even then (𝐒 (𝐒 (double z))) is even,
;; using (λ z (λ (s z) $bdy)) as initial proof clue.
!(assertEqual
  (bc &kb Nil z (fromNumber 3)
      (: (λ z (λ (s z) $bdy))
         (-> (: z ℕ)
             (-> (: (s z) (Even (double (: z ℕ))))
                 (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
  (: (λ z
       (λ (s z)
         (MkEvenSS (: (s z) (Even (double (: z ℕ)))))))
     (-> (: z ℕ)
         (-> (: (s z) (Even (double (: z ℕ))))
             (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))

;; Remove type annotation of next proof
!(assertEqual
  (remove-type-annotation
   (: ((Replace
        (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
       (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
      (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
  ((Replace (s (s z))) (MkEvenSS (s z))))

;; Verify that
;;
;; ((Replace (s (s z))) (MkEvenSS (s z)))
;;
;; is the proof that (double (𝐒 z)) is even, assuming that
;;
;; (: z ℕ)
;; (: (s z) (Even (double z)))
;; (: (s (s (s z))) (=== (Even (𝐒 (𝐒 (double $k)))) (Even (double (𝐒 $k)))))
;;
;; The proof may correspond to the proof tree below
;;
;;                                                                    ----------------(s z)
;;                                                                    (Even (double z))
;; ----------------------------------------------------(s (s z))  ------------------------(MkEvenSS)
;; (=== (Even (𝐒 (𝐒 (double z)))) (Even (double (𝐒 z))))          (Even (𝐒 (𝐒 (double z))))
;; -----------------------------------------------------------------------------------------(Replace)
;;                                  (Even (double (𝐒 z)))
!(assertEqual
  (bc &kb
      (Cons (: z ℕ)
            (Cons (: (s z) (Even (double (: z ℕ))))
                  (Cons (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
                        Nil)))
      z
      (fromNumber 2)
      (: ((Replace
           (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
          (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
         (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
  (: ((Replace
       (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
      (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
     (Even (double (: (𝐒 (: z ℕ)) ℕ)))))

;; Like above but the first hypothesis is wrapped in a lambda abstraction
!(assertEqual
  (bc &kb
      (Cons (: (s z) (Even (double (: z ℕ))))
            (Cons (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
                        Nil))
      z
      (fromNumber 3)
      (: (λ z
           ((Replace
             (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
            (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
         (-> (: z ℕ)
             (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
  (: (λ z
       ((Replace
         (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
        (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
     (-> (: z ℕ) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))

;; Like above but the first 2 hypotheses are wrapped in lambda abstractions
!(assertEqual
  (bc &kb
      (Cons (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
            Nil)
      z
      (fromNumber 4)
      (: (λ z
           (λ (s z)
             ((Replace
               (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
              (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
         (-> (: z ℕ)
             (-> (: (s z) (Even (double (: z ℕ))))
                 (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
  (: (λ z
       (λ (s z)
         ((Replace
           (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
          (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
     (-> (: z ℕ) (-> (: (s z) (Even (double (: z ℕ)))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))

;; Like above but only the last hypothesis is wrapped in a lambda abstraction
!(assertEqual
  (bc &kb
      (Cons (: z ℕ)
            (Cons (: (s z) (Even (double (: z ℕ))))
                  Nil))
      (s (s z))
      (fromNumber 3)
      (: (λ (s (s z))
           ((Replace
             (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
            (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
         (-> (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
             (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
  (: (λ (s (s z))
       ((Replace (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
        (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
     (-> (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
         (Even (double (: (𝐒 (: z ℕ)) ℕ))))))

;; Like above but the last 2 hypotheses are wrapped in lambda abstractions
!(assertEqual
  (bc &kb
      (Cons (: z ℕ) Nil)
      (s z)
      (fromNumber 4)
      (: (λ (s z)
           (λ (s (s z))
             ((Replace
               (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
              (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
         (-> (: (s z) (Even (double (: z ℕ))))
             (-> (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
                 (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
  (: (λ (s z)
       (λ (s (s z))
         ((Replace (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
          (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
     (-> (: (s z) (Even (double (: z ℕ))))
         (-> (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
             (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))

;; Like above but all hypotheses are wrapped in lambda abstractions.
;; In other words, type check that if (double z) is even, and
;;
;; (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))
;;
;; is equal to
;;
;; (Even (double (: (𝐒 (: z ℕ)) ℕ))))
;;
;; then (𝐒 (𝐒 (double z))) is even.
!(assertEqual
  (bc &kb
      Nil
      z
      (fromNumber 5)
      (: (λ z
           (λ (s z)
             (λ (s (s z))
               ((Replace
                 (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
                (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))
         (-> (: z ℕ)
             (-> (: (s z) (Even (double (: z ℕ))))
                 (-> (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
                     (Even (double (: (𝐒 (: z ℕ)) ℕ))))))))
  (: (λ z
       (λ (s z)
         (λ (s (s z))
           ((Replace
             (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
            (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))))
     (-> (: z ℕ)
         (-> (: (s z) (Even (double (: z ℕ))))
             (-> (: (s (s z)) (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))
                 (Even (double (: (𝐒 (: z ℕ)) ℕ))))))))

;; Verify that
;;
;; (λ z (λ (s z) ((Replace (Sym ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))) (MkEvenSS (s z)))))
;;
;; is the proof that if (double k) is even, then (double (𝐒 k)) is even, which corresponds to the tree below
;;
;;                                                                                 -(z)
;;                                                                                 ℕ
;;                                                                      -(z)      -(double)
;;                                                                      ℕ          ℕ
;;                                                                      -(𝐒)       -(𝐒)                      -(z)
;;                                                                      ℕ          ℕ                         ℕ
;;                                               -----------------(Even)  -(double)  -(𝐒)  --------------------------------------(double_rec)
;;                                               (-> (: $_ ℕ) Type)       ℕ          ℕ     (=== (double (𝐒 z)) (𝐒 (𝐒 (double z))))
;;                                               ----------------------------------------------------------------------------------(Cong)  ----------------(s z)
;;                                               (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))                                   (Even (double z))
;;                                               ------------------------------------------------------(Sym)                       ------------------------(MkEvenSS)
;;                                               (=== (Even (𝐒 (𝐒 (double $k)))) (Even (double (𝐒 $k))))                           (Even (𝐒 (𝐒 (double z))))
;;              ----------------(s z)            ----------------------------------------------------------------------------------------------------------(Replace)
;;              (Even (double z))                (Even (double (𝐒 z)))
;; -(z)         -----------------------------------------------------(λ)
;; ℕ            (-> (: (s z) (Even (double z))) (Even (double (𝐒 z))))
;; ------------------------------------------------------------------(λ)
;; (-> (: z ℕ) (-> (: (s z) (Even (double z))) (Even (double (𝐒 z)))))
;;
;; Before checking, let us type annotate the proof (disabled till assertAlphaEqual is supported)
;; !(assertAlphaEqual
;;   (add-type-annotation (λ z (λ (s z) ((Replace (Sym ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))) (MkEvenSS (s z))))))
;;   (λ z (λ (s z) ((Replace (: (Sym (: ((((Cong (: Even $a)) (: (double (: (𝐒 (: z $b)) $c)) $d)) (: (𝐒 (: (𝐒 (: (double (: z $e)) $f)) $g)) $h)) (: (double_rec (: z $i)) $j)) $k)) $l)) (: (MkEvenSS (: (s z) $m)) $n)))))
!(assertEqual
  (bc &kb
      Nil
      z
      (fromNumber 10)
      (: (λ z
           (λ (s z)
             ((Replace
               (: (Sym
                   (: ((((Cong (: Even (-> (: $_ ℕ) Type)))
                         (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
                        (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
                       (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
                      (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
                  (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
              (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
         (-> (: z ℕ)
             (-> (: (s z) (Even (double (: z ℕ))))
                 (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
  (: (λ z
       (λ (s z)
         ((Replace
           (: (Sym
               (: ((((Cong (: Even (-> (: $_ ℕ) Type)))
                     (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
                    (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
                   (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
                  (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
              (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
          (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
     (-> (: z ℕ)
         (-> (: (s z) (Even (double (: z ℕ))))
             (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))

;; Infer theorem of
;;
;; (SIℕ ((Replace ((((Cong Even) 𝐙) (double 𝐙)) (Sym double_base)))))
;; !(assertAlphaEqual
  !(bc &kb
      Nil
      z
      (fromNumber 7)
      (: (SIℕ
          (: ((Replace
               (: ((((Cong
                      (: Even $a))
                     (: 𝐙 $b))
                    (: (double (: 𝐙 $c)) $d))
                   (: (Sym (: double_base $e)) $f))
                  $g))
              (: MkEvenZ $h))
             (Even (double (: 𝐙 ℕ)))))
         $thrm))
  ;; (: (SIℕ
  ;;     (: ((Replace
  ;;          (: ((((Cong
  ;;                 (: Even (-> (: $_ ℕ) Type)))
  ;;                (: 𝐙 ℕ))
  ;;               (: (double (: 𝐙 ℕ)) ℕ))
  ;;              (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
  ;;             (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
  ;;         (: MkEvenZ (Even 𝐙)))
  ;;        (Even (double (: 𝐙 ℕ)))))
  ;;    (-> (: $prfI (-> (: $x ℕ) (-> (: $hyp (Even (double (: $x ℕ)))) (Even (double (: (𝐒 (: $x ℕ)) ℕ)))))) (-> (: $k ℕ) (Even (double (: $k ℕ)))))))

;; Verify that
;;
;; ((SIℕ ((Replace ((((Cong Even) 𝐙) (double 𝐙)) (Sym double_base)))))
;;  (λ z (λ (s z) ((Replace (Sym ((((Cong Even) (double (𝐒 z))) (𝐒 (𝐒 (double z)))) (double_rec z)))) (MkEvenSS (s z))))))
;;
;; proves that (double k) is even for any natural k, corresponding to the tree below
;;
;;                                                                                                                      -(z)
;;                                                                                                                      ℕ
;;                                                                                                           -(z)       -(double)
;;                                                                                                           ℕ          ℕ
;;                                                                                                           -(𝐒)       -(𝐒)                     -(z)
;;                                                                                                           ℕ          ℕ                        ℕ
;;                                                                                  -----------------(Even)  -(double)  -(𝐒)  --------------------------------------(double_rec)
;;                                                                                  (-> (: $_ ℕ) Type)       ℕ          ℕ     (=== (double (𝐒 z)) (𝐒 (𝐒 (double z))))
;;                                                                                  --------------------------------------------------------------------------------(Cong)     ----------------(s z)
;;                                                                                                            (=== (Even (double (𝐒 $k))) (Even (𝐒 (𝐒 (double $k)))))          (Even (double z))
;;                         -(𝐙)       -----------------(double_base)                                          ------------------------------------------------------(Sym)  ------------------------(MkEvenSS)
;;                         ℕ          (=== (double 𝐙) 𝐙)                                                      (=== (Even (𝐒 (𝐒 (double $k)))) (Even (double (𝐒 $k))))      (Even (𝐒 (𝐒 (double z))))
;; ----------(Even)  -(𝐙)  -(double)  -----------------(Sym)                           ----------------(s z)  -------------------------------------------------------------------------------------(Replace)
;; (-> ℕ Type)       ℕ     ℕ          (=== 𝐙 (double 𝐙))                               (Even (double z))                (Even (double (𝐒 z)))
;; ----------------------------------------------------(Cong)  -------(MkEvenZ)  -(z)  -----------------------------------------------------(λ)
;;           (=== (Even 𝐙) (Even (double 𝐙)))                  (Even Z)          ℕ     (-> (: (s z) (Even (double z))) (Even (double (𝐒 z))))
;;           ---------------------------------------------------------(Replace)  ------------------------------------------------------------------(λ)
;;                     (Even (double 𝐙))                                         (-> (: z ℕ) (-> (: (s z) (Even (double z))) (Even (double (𝐒 z)))))
;;                     ----------------------------------------------------------------------------------------------------------------------------(SIℕ)
;;                                                             (-> (: $k ℕ) (Even (double $k)))
!(assertEqual
  (bc &kb
      Nil
      z
      (fromNumber 11)
      (: ((SIℕ
           (: ((Replace
                (: ((((Cong
                       (: Even (-> (: $_1 ℕ) Type)))
                      (: 𝐙 ℕ))
                     (: (double (: 𝐙 ℕ)) ℕ))
                    (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
                   (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
               (: MkEvenZ (Even 𝐙)))
              (Even (double (: 𝐙 ℕ)))))
          (: (λ z
               (λ (s z)
                 ((Replace
                   (: (Sym
                       (: ((((Cong (: Even (-> (: $_2 ℕ) Type)))
                             (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
                            (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
                           (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
                          (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
                      (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
                  (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
             (-> (: z ℕ)
                 (-> (: (s z) (Even (double (: z ℕ))))
                     (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
         (-> (: $k ℕ) (Even (double (: $k ℕ))))))
  (: ((SIℕ
       (: ((Replace
            (: ((((Cong
                   (: Even (-> (: $_1 ℕ) Type)))
                  (: 𝐙 ℕ))
                 (: (double (: 𝐙 ℕ)) ℕ))
                (: (Sym (: double_base (=== (double (: 𝐙 ℕ)) 𝐙))) (=== 𝐙 (double (: 𝐙 ℕ)))))
               (=== (Even 𝐙) (Even (double (: 𝐙 ℕ))))))
           (: MkEvenZ (Even 𝐙)))
          (Even (double (: 𝐙 ℕ)))))
      (: (λ z
           (λ (s z)
             ((Replace
               (: (Sym
                   (: ((((Cong
                          (: Even (-> (: $_2 ℕ) Type)))
                         (: (double (: (𝐒 (: z ℕ)) ℕ)) ℕ))
                        (: (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)) ℕ))
                       (: (double_rec (: z ℕ)) (=== (double (: (𝐒 (: z ℕ)) ℕ)) (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))
                      (=== (Even (double (: (𝐒 (: z ℕ)) ℕ))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))))))
                  (=== (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ))) (Even (double (: (𝐒 (: z ℕ)) ℕ))))))
              (: (MkEvenSS (: (s z) (Even (double (: z ℕ))))) (Even (𝐒 (: (𝐒 (: (double (: z ℕ)) ℕ)) ℕ)))))))
         (-> (: z ℕ) (-> (: (s z) (Even (double (: z ℕ)))) (Even (double (: (𝐒 (: z ℕ)) ℕ)))))))
     (-> (: $k ℕ) (Even (double (: $k ℕ))))))
