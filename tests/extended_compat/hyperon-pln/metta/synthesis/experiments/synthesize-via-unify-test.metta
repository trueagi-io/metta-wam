;; Import modules
!(import! &self metta:synthesis:experiments:synthesize-via-unify)
!(import! &self metta:common:Record)

;; Knowledge base
(: kb (-> Atom))
(= (kb) (superpose ((: f (-> Number String))
                    (: g (-> String Bool))
                    (: h (-> Bool Number))
                    (: i (-> String Number Bool)))))

;; Rule base
(: rb (-> Atom))
(= (rb) (superpose ((: . (-> (-> $b $c) (-> $a $b) (-> $a $c))) ; Function composition
                    (: .: (-> (-> $c $d) (-> $a $b $c) (-> $a $b $d)))))) ; Blackbird

;; Test program synthesizer
!(assertEqualToResult (record synthesize ((: $term $type) Z))                  ( (: f (-> Number String))  (: g (-> String Bool))  (: h (-> Bool Number))))
!(assertEqualToResult (record synthesize ((: $term (-> Number String)) Z))     ( (: f (-> Number String))))
!(assertEqualToResult (record synthesize ((: $term (-> String Number Number)) kb rb (S Z)))     ( (: f (-> Number String))))
!(assertEqualToResult (record synthesize ((: $term (-> Number Bool)) (S Z)))   ( (: (. g f) (-> Number Bool))))
!(assertEqualToResult (record synthesize ((: (. g f) (-> Number Bool)) (S Z))) ( (: (. g f) (-> Number Bool))))
!(assertEqualToResult (record synthesize ((: (. g f) $type) (S Z)))            ( (: (. g f) (-> Number Bool))))
!(assertEqualToResult (record synthesize ((: $term $type) (S Z)))              ( (: f (-> Number String))  ...  (: (. f h) (-> Bool String))  ...))
!(assertEqualToResult (record synthesize ((: (. (. g f) h) (-> $in Bool)) (fromNumber 2))) ( (: (. (. g f) h) (-> Bool Bool))))
!(assertEqualToResult (record synthesize ((: (. $g h) (-> $in String)) (fromNumber 3))) ( (: (. f h) (-> Bool String))  (: (. (. (. f h) (. g f)) h) (-> Bool String))))
!(assertEqualToResult (record synthesize ((: (. $g h) (-> Number $out)) (fromNumber 3)))  ()) ; No answer
!(assertEqualToResult (record synthesize ((: (. g $f) (-> $in String)) (fromNumber 3)))  ()); No answer
!(assertEqualToResult (record synthesize ((: (. g $f) (-> $in Bool)) (fromNumber 3))) (: (. g f) (-> Number Bool))  (: (. g (. f h)) (-> Bool Bool))  (: (. g (. f (. h g))) (-> String Bool))  (: (. g (. (. f h) g)) (-> String Bool))  (. g (. (. f h) (. g f))) (-> Number Bool))
;; !(assertEqualToResult (record synthesize ((: $term $type) (fromNumber 4)))  ... )   ; Too long to list
;; !(record synthesize ((: $term (-> Number Number)) kb rb (fromNumber 6))) ; Too long to run

