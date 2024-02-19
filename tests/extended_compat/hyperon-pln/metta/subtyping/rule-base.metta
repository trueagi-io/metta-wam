;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule base for the subtyping relationship ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Notations:
;;
;; 1. Variables in upper case, like `$T`, tend to correspond to types.
;; 2. Variables in lower case, like `$t`, tend to correspond to terms.
;; 3. In the rule names
;; 3.1. ST stands for SubTyping,
;; 3.2. Refl for Reflexive,
;; 3.3. Trans for transitive,
;; 3.4. FnTy for Function Type,
;; 3.5. ImplCoer for Implicit Coercion.

;; TODO: maybe we should add (: $T Type) as premises

;; Subtyping is relexive.  WARNING: that one is actually an axiom, not
;; a rule.
(: STRefl (<: $T $T))

;; Subtyping is transitive
(: STTrans (->
            ;; Premises
            (<: $S $T)
            (<: $T $U)
            ;; Conclusion
            (<: $S $U)))

;; Subtyping of function types is contravariant over inputs and
;; covariant over outputs.
(: STFnTy (->
           ;; Premises
           (<: $T1 $S1)
           (<: $S2 $T2)
           ;; Conclusion
           (<: (-> $S1 $S2) (-> $T1 $T2))))

;; Relationship between subtyping and type assuming implicit coercion.
;; That is if a term `t` is of type `S` a subtype of `T`, then `t` is
;; of type `T` as well.
(: STImplCoer (->
               ;; Premises
               (<: $S $T)
               (: $t $S)
               ;; Conclusion
               (: $t $T)))

;; Relationship between subtyping and type assume explicit coercion.
;; That is if a term `t` is of type `S` a subtype of `T`, then
;; `(coerce proof_S<:T t)` is of type `T`, where `proof_S<:T` is a
;; proof that `S` is a subtype of `T`.
(: coerce (->
           ;; Premises
           (<: $S $T)
           $S
           ;; Conclusion
           $T))
