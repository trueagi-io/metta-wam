(= (pick-one-to-nine) 1)
(= (pick-one-to-nine) 2)
(= (pick-one-to-nine) 3)
(= (pick-one-to-nine) 4)
(= (pick-one-to-nine) 5)
(= (pick-one-to-nine) 6)
(= (pick-one-to-nine) 7)
(= (pick-one-to-nine) 8)
(= (pick-one-to-nine) 9)

(= (pick-zero-to-nine) 0)
(= (pick-zero-to-nine) (pick-one-to-nine))

; S and M are not 0 (no leading zeros)
(= (S (pick-one-to-nine)) True)
(= (M (pick-one-to-nine)) True)

; Other digits can be from 0 to 9, but unique
(= (E (pick-zero-to-nine)) True)
(= (N (pick-zero-to-nine)) True)
(= (D (pick-zero-to-nine)) True)
(= (O (pick-zero-to-nine)) True)
(= (R (pick-zero-to-nine)) True)
(= (Y (pick-zero-to-nine)) True)

; Define uniqueness constraints for all digits
(not (= S M)) (not (= S E)) (not (= S N)) (not (= S D)) (not (= S O))
(not (= M E)) (not (= M N)) (not (= M D)) (not (= M O)) (not (= M R))
(not (= E N)) (not (= E D)) (not (= E O)) (not (= E R)) (not (= E Y))
(not (= N D)) (not (= N O)) (not (= N R)) (not (= N Y))
(not (= D O)) (not (= D R)) (not (= D Y))
(not (= O R)) (not (= O Y))
(not (= R Y))

; Define arithmetic constraint: 1000*S + 100*E + 10*N + D + 1000*M + 100*O + 10*R + E = 10000*M + 1000*O + 100*N + 10*E + Y
(:=           (+ (+ (* 1000 S) (* 100 E) (* 10 N) D)
                 (+ (* 1000 M) (* 100 O) (* 10 R) E))
     (+ (* 10000 M) (* 1000 O) (* 100 N) (* 10 E) Y))



; Solve to find the values
!(solve)


(: member (-> $e (List $e) NDet))

