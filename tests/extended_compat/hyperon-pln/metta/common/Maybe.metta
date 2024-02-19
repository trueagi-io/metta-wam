;; Define Maybe type
(: Maybe (-> $a Type))
(: Nothing (Maybe $a))
(: Just (-> $a (Maybe $a)))

;; Define greater than operator for Maybe (overloading is not
;; supported at this point).
(= (Maybe< (Just $x) (Just $y)) (< $x $y))
(= (Maybe< (Just $x) Nothing) False)
(= (Maybe< Nothing (Just $x)) True)
