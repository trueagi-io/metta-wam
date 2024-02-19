;; Import ===
!(import! &self EqualityType.metta)

;; Define various subterms
(: TTrue True)
(: T42 42)

;; Define dummy functions to type check equality type
(: foo (-> (=== 42 42) Bool))
(= (foo $prf) True)
(: bar (-> (=== True True) Bool))
(= (bar $prf) True)
(: baz (-> (=== 42 True) Bool))
(= (baz $prf) False)

;; Test equality type
!(foo (Refl T42))                       ; Should succeed
!(bar (Refl T42))                       ; Should fail
!(bar (Refl TTrue))                     ; Should succeed
!(baz (Refl T42))                       ; Should fail
