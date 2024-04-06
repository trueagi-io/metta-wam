((leaf1 leaf2) leaf3)
(((leaf0 leaf1) leaf2) leaf3)
(top ((leaf1 leaf2) leaf3))

(Sam is a frog)
(Tom is a cat)
(Sophia is a robot)
!(println! (match &self ($who is a $what) ($who the $what)))

(:= (Green Sam) True)
!(println! (match &self (:= (Green $who) True) ($who is really green)))

;variables in knowledge base entries
(implies (Frog $x) (Green $x))
(implies (Frog $x) (Eats-flies $x))

!(println! (match &self 
        (implies (Frog Sam) $what) $what))

!(println! (match &self
        (implies ($P $x) (Green Sam)) ($x might be $P)))

!(println! (match &self 
        (, (implies ($P $x) (Green $x))
        (implies ($P $x) (Eats-flies Sam)))
        ($x is definitely $P)))


(gene "A")
(chr (gene "A") "chr16")
(start (gene "A") 1000)
(end (gene "A") 1200)

(gene "B")
(chr (gene "B") "chr2")
(start (gene "B") 1000)
(end (gene "B") 1200)

(gene "C")
(chr (gene "C") "chr16")
(start (gene "C") 9000)
(end (gene "C") 9400)

(gene "D")
(chr (gene "D") "chr16")
(start (gene "D") 1600)
(end (gene "D") 1900)


(let* (($r1 (Add Z Z))
            ($r2 (Add $r1 (S Z)))
            ($r3 (Add $r2 (S Z))))
            (match &self (= (eq (S (S Z)) $r3) $tv) $tv))

!(match &self (, (chr $g "chr16")
                        (start $g $start)
                        (end $g $end))
                (if (and (> $start 500) (< $end 2000)) $g ()))