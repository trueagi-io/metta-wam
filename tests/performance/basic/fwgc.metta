; The "Farmer, Wolf, Goat, and Cabbage" problem is a classic river crossing puzzle
; that can be easily modeled in MeTTa. The idea is to move all four across a river,
; adhering to the constraint that the wolf cant be left alone with the goat,
; and the goat cant be left alone with the cabbage. The boat can carry at most one
; item (wolf/goat/cabbage) along with the farmer.


(= (valid  (state $F $F $G  _) (state $F1 $F1 $G   _)) True)
(= (valid  (state $F $W $F  _) (state $F1 $W  $F1  _)) True)
(= (valid  (state $F $W $G $F) (state $F1 $W  $G $F1)) True)
(= (valid  (state $F $W $G $C) (state $F1 $W  $G $C )) True)

(= (move  (state  left $W $G $C) (state right $W $G $C)) True)
(= (move  (state right $W $G $C) (state  left $W $G $C)) True)
(= (move  (state    $F $F $G $C) (state  $F1 $F1 $G $C)) (opposite  $F $F1))
(= (move  (state    $F $W $F $C) (state  $F1 $W $F1 $C)) (opposite  $F $F1))
(= (move  (state    $F $W $G $F) (state  $F1 $W $G $F1)) (opposite  $F $F1))

(= (opposite  left right) True)
(= (opposite  right left) True)

(= (solve)
     (path  (:: (state left left left left)) $_))

(= (path  (:: (state right right right right) $T) $Path)
     (reverse  (:: (state right right right right) $T) $Path))

(= (path  (:: $Curr  $T) $Path)
     (sequential
	(move   $Curr $Next)
	(valid  $Curr $Next)
	(not  (member  $Next (:: $Curr $T)))
	(path  (:: $Next $Curr $T) $Path)))

