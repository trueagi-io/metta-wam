; Pathfinding using a simple graph
; Graph is represented as one-way edge functions

!(pragma! compile full)

(: edge (-> Symbol Symbol))
(= (edge a) b)
(= (edge b) c)
(= (edge c) d)
(= (edge e) f)



; Epathfinding rule
(: path (-> Symbol Symbol))
; was base case, node to itself
(= (path $x) $x)
; recursive case
(= (path $x) (edge (epath $x)))


; Pathfinding rule
(: epath (-> Symbol Symbol))
; recursive case
(= (epath $x) (epath (edge $x)))
; was base case, node to itself
(= (epath $x) $x)


; Present
!(assertTrue (== (edge a) b))
; Not present
!(assertFalse (== (edge a) d))

; Pathfinding tests
;!(assertTrue (== (path e) e))
;!(assertTrue (== (path a) b))
;!(assertTrue (== (path a) d))
; Should be only one dirrection
;!(assertFalse (== (path c) a))
; Not Connected
;!(assertFalse (== (path a) f))


; Pathfinding tests
;!(assertTrue (== (epath e) e))
;!(assertTrue (== (epath a) b))
;!(assertTrue (== (epath a) d))
; Should be only one dirrection
;!(assertFalse (== (path c) a))
; Not Connected
;!(assertFalse (== (epath a) f))

