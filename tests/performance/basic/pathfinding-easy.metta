; Pathfinding using a simple graph
; Graph is represented as facts: (= (edge A B) True) means there''s an one-way edge from A to B.

!(pragma! compile full)

(: edge (-> Symbol Symbol Bool))
(= (edge a b) True)
(= (edge b c) True)
(= (edge c d) True)
(= (edge e f) True)


;a -> b -> c -> d
;     \
;      e -> f

; Pathfinding rule
(: path (-> Symbol Symbol Bool))
; was base case, node to itself
(= (path $x $x) True)
(= (path $x $y) (and (edge $x $z) (path $z $y))) ; recursive case

; Not asserted
!(assertEqual (edge a c) False)

; Should be only one dirrection
;!(assertEqual (path c a) False)

; Not Connected
;!(assertEqual (path a f) False)

; Pathfinding tests
;!(assertEqual (path a b) True)
;!(assertEqual (path a c) True)



