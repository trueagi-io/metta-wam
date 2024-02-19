
(: calculate-rotation-angle (-> AngleInDegreesForRotation AngleInDegrees))
(= (calculate-rotation-angle $angleInDegreesForRotation)
   (if (> $angleInDegreesForRotation 360)
       (- $angleInDegreesForRotation 360)
       $angleInDegreesForRotation))

(: print-and-return-fruit-count (-> SomethingWithAmount CountedAmount))
(= (print-and-return-fruit-count $apples)
   (print ("Amount of fruit =" $apples))
   (as-count $apples))

(: compare-apples-oranges (-> Apples Oranges String))
(= (compare-apples-oranges $appleCount $orangeCount)
   (if (> $appleCount $orangeCount)
       "More Apples"
       (if (< $appleCount $orangeCount) "More Oranges" "Equal Amount")))


;; Declares a function 'degrees-to-radians' that converts angles from degrees to radians
(: degrees-to-radians (-> AngleInDegrees AngleInRadians))
;; Defines the 'degrees-to-radians' function
(= (degrees-to-radians $angleInDegrees)
   ;; Multiplies the angle in degrees by (pi/180) to convert it to radians
   (* $angleInDegrees (/ pi 180)))

;; Declares the 'any-to-radians' function to accept an angle in degrees
(:  any-to-radians (-> AngleInDegrees AngleInRadians))
;; Converts angle from degrees to radians using the 'degrees-to-radians' function
(= (any-to-radians $angle) (degrees-to-radians $angle))

;; Declares the 'any-to-radians' function to accept an angle in radians
(:  any-to-radians (-> AngleInRadians AngleInRadians))
;; Returns the angle in radians directly without conversion
(= (any-to-radians $angle) $angle)

;; Declares a type 'radian-producer' that represents a function returning AngleInRadians
(: radian-producer (-> AngleInRadians))
;; Declares a type 'degree-producer' that represents a function returning AngleInDegrees
(: degree-producer (-> AngleInDegrees))

;; Defines 'degree-producer-1' as a constant function returning 1 degree
(= degree-producer-1 1.0)
;; Defines 'radian-producer-1' as a constant function returning 1 radian
(= radian-producer-1 1.0)

;; Invokes 'any-to-radians' with 'degree-producer-1', expecting conversion to radians
!(any-to-radians (degree-producer-1))
;; Invokes 'any-to-radians' with 'radian-producer-1', expecting no conversion
!(any-to-radians (radian-producer-1))

;; Asserts that the result of converting 1 degree to radians is not equal to 1 radian
!(assertNotEqual (any-to-radians (degree-producer-1)) (any-to-radians (radian-producer-1)))



