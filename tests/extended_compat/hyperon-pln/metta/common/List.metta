;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of a List data structure with various methods for it. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Declaration of List data type and constructors
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;; Insert an element to a presumably sorted list, remains sorted.
(: insert (-> $a (List $a) (List $a)))
(= (insert $x Nil) (Cons $x Nil))
(= (insert $x (Cons $head $tail))
   (if (< $x $head)
       (Cons $x (Cons $head $tail))
       (Cons $head (insert $x $tail))))

;; Sort a list
(: sort (-> (List $a) (List $a)))
(= (sort Nil) Nil)
(= (sort (Cons $head $tail)) (insert $head (sort $tail)))

;; Check if an element is in a list (sorted or not)
(: elem (-> $a (List $a) Bool))
(= (elem $x Nil) False)
(= (elem $x (Cons $head $tail)) (if (== $x $head) True (elem $x $tail)))

;; Remove duplicates from a list
(: uniq_ (-> (List $a) (List $a) (List $a)))
(= (uniq_ $acc Nil) Nil)
(= (uniq_ $acc (Cons $head $tail))
   (if (elem $head $acc)
       (uniq_ $acc $tail)
       (Cons $head (uniq_ (Cons $head $acc) $tail))))
(: uniq (-> (List $a) (List $a)))
(= (uniq $l) (uniq_ Nil $l))

;; Insert an element in a presumably sorted list without duplicate.
;; Only insert if the element is not already in the list as to produce
;; a sorted list without duplicate.
(: insert_uniq (-> $a (List $a) (List $a)))
(= (insert_uniq $x Nil) (Cons $x Nil))
(= (insert_uniq $x (Cons $head $tail))
   (if (== $x $head)
       (Cons $head $tail)
       (if (< $x $head)
           (Cons $x (Cons $head $tail))
           (Cons $head (insert_uniq $x $tail)))))
