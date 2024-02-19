; Metta Pathfinding Example

; these examples actually need the compiler!
!(pragma! compile full)

; Define the edge-f function, which represents one-way edges in the graph.
; This function is crucial for specifying connections between nodes.
(: edge-f (-> Symbol Symbol))

; Define the edges in the graph using the edge-f function.
(= (edge-f a) b)
(= (edge-f b) c)
(= (edge-f c) d)
(= (edge-f e) f)

; The following code is commented-out and relates to node connectivity checks.
;(= (at $x) (assertTrue $x))
;(= (at-a-b) (at (is-same (edge-f a) b)))
;!(at-a-b)
;(= (a-t-a-b) (assertTrue (is-same (edge-f a) b)))
;!(a-t-a-b)

(= (is-same $x $x) True)


; Testing the presence of nodes in the graph.
!(assertTrue (is-same (edge-f a) b)) ; Node 'a' is connected to 'b'
; Not present
!(assertFalse (is-same (edge-f a) d)) ; Node 'a' is not connected to 'd'

; Define the path-f function for pathfinding.
; This function utilizes recursive calls to the edge-f function.
(: path-f (-> Symbol Symbol))
; A node is always connected to itself.
(= (path-f $x) $x)
; The path-f function follows edges using the edge-f function.
(= (path-f $x) (edge-f (path-f $x)))

; The following unit tests for the path-f function are currently commented out.
;!(assertTrue (is-same (path-f e) e))
;!(assertTrue (is-same (path-f a) b))
;!(assertTrue (is-same (path-f a) d))
; There should be only one direction
;!(assertFalse (is-same (path-f c) a))
; Not connected
;!(assertFalse (is-same (path-f a) f))

; Define the epath-f function for pathfinding using edges.
; This function also employs recursive calls to the edge-f function.
(: epath-f (-> Symbol Symbol))

; The epath-f function follows edges using the edge-f function.
(= (epath-f $x) (epath-f (edge-f $x)))

; A node is still connected to itself.
(= (epath-f $x) $x)

; The following unit tests for the epath-f function are currently commented out.
; Uncomment these unit tests as needed.
;!(assertTrue (is-same (epath-f e) e))
;!(assertTrue (is-same (epath-f a) b))
;!(assertTrue (is-same (epath-f a) d))
; There should be only one direction
;!(assertFalse (is-same (epath-f c) a))
; Not connected
;!(assertFalse (is-same (epath-f a) f))

; Pathfinding with nondeterministic epath-f
!(assertEqualToResult (epath-f a) (collapse (superpose (a b c d))))

; Define the epath-f-p predicate for pathfinding using edges
(= (epath-f-p $x $y) (is-same (epath-f $x) $y))

; Running epath-f in reverse directly via 'is-same'
!(assertEqual (match &self (is-same (epath-f $x) c) $x) (superpose (a b c)))

; Running epath-f in reverse via the truth predicate
!(assertEqual (match &self (epath-f-p $x c) $x) (superpose (a b c)))

; Redefine the edge function using facts instead of functions.
; The graph is represented as facts: (= (edge A B) True) means there is a one-way edge from A to B.
(: edge (-> Symbol Symbol Bool))

; Define the directional edges in the graph using facts.
;a -> b -> c -> d
;     \
;      e -> f
(= (edge a b) True)
(= (edge b c) True)
(= (edge c d) True)
(= (edge b e) True)
(= (edge e f) True)
(= (edge g h) True)

; Define the path function using facts for pathfinding.
(: path (-> Symbol Symbol Bool))

; A node is always connected to itself.
(= (path $x $x) True)

; The path function uses facts to check edge connectivity.
(= (path $x $y) (and (edge $x $z) (path $z $y)))

; Unit tests for the path function.
!(assertEqual (path a b) True)
!(assertEqual (path a c) True)
!(assertEqual (path a d) True)
!(assertEqual (path a f) True)
!(assertEqual (path b e) True)
!(assertEqual (path e f) True)

; There should be only one direction
;!(assertEqual (path f a) False)
; Not asserted
;!(assertEqual (edge a g) False)
; Not connected
;!(assertEqual (path a g) False)



