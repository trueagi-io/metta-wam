;; Define (homogeneous) equality type, also called propositional equality
(: === (-> $a $a Type))

;; Define Refl, the reflexivity constructor of ===
;;
;; Note that to define Refl properly we'd need
;;
;; (: Refl (-> (: $x $a) (=== $x $x)))
;;
;; which is not possible as inlining (: $x $a) is not supported yet.
;; Instead we define it as
;;
;; (: Refl (-> $x (=== $x $x)))
;;
;; However that requires to use a workaround consisting of assigning
;; to each term a subterm.  Such as
;;
;; (: Term42 42)
;;
;; allowing us to use Refl as follows
;;
;; (Refl Term42)
;;
;; the proof by reflexivity of
;;
;; (=== 42 42)
(: Refl (-> $x (=== $x $x)))

;; What about
;;
;; (: Refl (=== $x $x))
;;
;; ?
