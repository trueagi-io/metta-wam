(initiates Load Loaded $t)
((initiates Shoot Dead $t) :- (
    (holdsAt Loaded $t)
))

((terminates Shoot Alive $t) :- (
    (holdsAt Loaded $t)
))

(initiallyP Alive)
(happens Load 1)
(happens Sneeze 3)
(happens Shoot 4)

(tp 0) (tp 1) (tp 2) (tp 3) (tp 4)
(tp 5)

!(import! &self bec)