;; define some-value returning Number and String
(= (some-value) 1)
(= (some-value) "a")
(= (some-value) 'a')
(= (some-value) ())
(= (some-value) (1 2 3))
(= (some-value) (println! "did it"))
(= (some-value) a-symbol)
(= (some-value) &self)

(: Filter-String (-> String $t))
(: Filter-Number (-> Number $t))

;; returns of appropriate type
!(assertEqualToResult (Filter-Number (some-value)) (1))
!(assertEqualToResult (Filter-String (some-value)) ("a"))


;; Checks if the above would oif worked had there been a specific implemenation other than default
(: Impl-Filter-String (-> String $t))
(= (Impl-Filter-String $a) $a)

(: Impl-Filter-Number (-> Number $t))
(= (Impl-Filter-Number $a) $a)

;; returns of appropriate type
!(assertEqualToResult (Impl-Filter-Number (some-value)) (1))
!(assertEqualToResult (Impl-Filter-String (some-value)) ("a"))



