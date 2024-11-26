;; define some-value returning Number and String
(= (some-value) 1)
(= (some-value) "a")
(= (some-value) 'c')
(= (some-value) ())
(= (some-value) (1 2 3))
(= (some-value) (+ 2 3))
(= (some-value) (println! "did it"))
(= (some-value) (format-args "x: {} y: {}" (1 2)))
(= (some-value) S)
(= (some-value) Z)
(= (some-value) (S Z))
(= (some-value) (S (S Z)))
(= (some-value) &self)

(: Filter-String (-> String $t))
(: Filter-Number (-> Number $t))

;; returns of appropriate type
!(assertEqualToResult (Filter-Number (some-value)) (1 6))
!(assertEqualToResult (Filter-String (some-value)) ("a" "x: 1 y: 2"))


;; Checks if the above would oif worked had there been a specific implemenation other than default
(: Impl-Filter-String (-> String $t))
(= (Impl-Filter-String $a) $a)

(: Impl-Filter-Number (-> Number $t))
(= (Impl-Filter-Number $a) $a)

;; returns of appropriate type
!(assertEqualToResult (Impl-Filter-Number (some-value)) (1 6))
!(assertEqualToResult (Impl-Filter-String (some-value)) ("a" "x: 1 y: 2"))



