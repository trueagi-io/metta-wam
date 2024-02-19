
(= (count-change $amount)
    (cc $amount 5))

(= (cc $amount $kinds-of-coins)
    (if (== $amount 0)
        1
        (if (or (< $amount 0) (== $kinds-of-coins 0))
            0
            (+ (cc $amount (- $kinds-of-coins 1))
                (cc (- $amount (first-denomination $kinds-of-coins)) $kinds-of-coins)))))

(= (first-denomination $kinds-of-coins)
    (case $kinds-of-coins
        (
            (1 1)
            (2 5)
            (3 10)
            (4 25)
            (5 50)
        )))

!(assertEqual (count-change 11) 4)

; In the book's example 100 is used to call function count-change, but for the sake of performance I've put 20 here.
!(assertEqual
    (count-change 20)
    9)

!(assertEqual (count-change 51) 50)

!(assertEqual (count-change 60) 77)










