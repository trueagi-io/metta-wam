; Pathfinding using a simple graph
; Graph is represented as one-way edge-f functions

!(pragma! compile full)


(: edge-f (-> Symbol Symbol))
(= (edge-f a) b)
(= (edge-f b) c)
(= (edge-f c) d)
(= (edge-f e) f)



; Pathfinding rule
(: epath-f (-> Symbol Symbol))
; recursive case
(= (epath-f $x) (epath-f (edge-f $x)))
; was base case, node to itself
(= (epath-f $x) $x)


; Pathfinding rule
(: path-f (-> Symbol Symbol))
; was base case, node to itself
(= (path-f $x) $x)
; recursive case
(= (path-f $x) (edge-f (path-f $x)))


; Present
!(assertTrue (== (edge-f a) b))
; Not present
!(assertFalse (== (edge-f a) d))

; Pathfinding tests
;!(assertTrue (== (path-f e) e))
;!(assertTrue (== (path-f a) b))
;!(assertTrue (== (path-f a) d))
; Should be only one dirrection
;!(assertFalse (== (path-f c) a))
; Not Connected
;!(assertFalse (== (path-f a) f))


; Pathfinding tests
;!(assertTrue (== (epath-f e) e))
;!(assertTrue (== (epath-f a) b))
;!(assertTrue (== (epath-f a) d))
; Should be only one dirrection
;!(assertFalse (== (epath-f c) a))
; Not Connected
;!(assertFalse (== (epath-f a) f))

