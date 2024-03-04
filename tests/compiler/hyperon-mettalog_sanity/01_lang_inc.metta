
; a functional example
(= (maybe-inc $x)
    (case $x
     (((Just $v) (Just (+ 1 $v)))
	(Nothing Nothing))))

!(assertEqual (maybe-inc Nothing) Nothing)
!(assertEqual (maybe-inc (Just 2)) (Just 3))



