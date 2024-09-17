;; Import synthesizer
!(import! ../synthesis/Synthesize.metta)

;; Define List data type and constructors
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;; Knowledge base
(: kb (-> Atom))
(= (kb) (superpose
         ;; Equality is reflexive.  We use === instead of == to make sure it
         ;; does not get reduced by the MeTTa interpreter.
         (: EqRefl (=== $x $x))

         ;; Base case of append function definition
         (: Base_app (=== (app Nil $ys) $ys))

         ;; Inductive case of append function definition
         (: Ind_app (=== (app (Cons $x $xs) $ys) (Cons $x (app $xs $ys))))

         ;; Base case of reverse function definition
         (: Base_rev (=== (rev Nil) Nil))

         ;; Inductive case of reverse function definition
         (: Ind_rev (=== (rev (Cons $x $xs)) (app (rev $xs) (Cons $x Nil))))))

;; Rule base
(: rb (-> Atom))
(= (rb) (superpose
         ;; Equality is transitive
         (: EqTrans (-> ;; Premises
                        (=== $x $y)
                        (=== $y $z)
                        ;; Conclusion
                        (=== $x $z)))

         ;; Equality is symmetric
         (: EqSym (-> ;; Premise
                      (=== $x $y)
                      ;; Conclusion
                      (=== $y $x)))

         ;; Induction on List
         (: ListInduction (-> ;; Premises
                              (: $p (-> (List $a) Type))       ; property
                              (: $x $a)                        ; list element
                              (: $xs (List $a))                ; list
                              ($p Nil)                         ; base
                              (-> ($p $xs) ($p (Cons $x $xs))) ; induction
                              ;; Conclusion
                              ($p (Cons $x $xs))))))

;; For any list, Nil is the right identity of app
(: NilRID_app (-> (List $a) Type))
(=== (NilRID_app $xs) (=== (app $xs Nil) $xs))

;; Prove that Nil is the right identity of app
!(synthesize (: $proof (=== (app $xs Nil) $xs)) kb rb (fromNumber 3))

;; Prove that (===

;; Prove that (=== (rev (rev $xs)) $xs)
