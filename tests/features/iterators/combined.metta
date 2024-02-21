
!(switch_to_mettalog!) ;;; in case we where not started with --log

(: wait-for-key (-> Integer))
(= (wait-for-key)
 (, (println! "Press 'en-ee' key to continue...")
    (case (get-single-char-fn)     
    ((101 (halt! 6)) ; (e)xit
     (98 (, (repl!) (wait-for-key))) ; (b)reak
     ($x $x))))) ; return other ascii codes

!(pragma! sandbox False) ;; in case we are in SWISH
!(pragma! host-functions all) ;; throw a security exception in SWISH and the user is not logged in

!(bind! &iter1 (new-space))
!(add-atom &iter1  -1)
!(add-atom &iter1  1)

; Number, Strings, Expressions are values
(: zip (-> Value Value Expression))
(= (zip $i $j) ($i $j))

!(import! &self ../io/file-output.metta)

(: iterate (-> Atom Atom))
(= (iterate $iterable)
   (case ($iterable (get-mettatype $iterable))
    ((($_ GroundedSpace) (get-atoms $iterable))
      (((fifo $list) $_) (superpose $list))
      (($_ $_InputStream) (read-metta $iterable))
      ($_ $iterable))))


(: new-space-with (-> Atom Atom)) ; Space
(= (new-space-with $list)
  (let $space (new-space) 
      (, (compose 
        (add-atom $space (superpose $fill))) 
          $space)))

(: map (-> Atom Atom Atom))
(= (map $p2 $itr)
   (lazy-map! $p2 (iterate $itr)))

(: test-diamond (-> Atom Atom Atom))
(= (test-diamond $filename $iterable)
    (let $isPositive (map (< 0) $iterable)
    (let $doubledNumbers (map (* 2) $iterable)
    (let $combinedStream (zip $isPositive $doubledNumbers)
    (let $each (materialize! $combinedStream)
     (with-results-to-file $filename $each))))))


;; create our input file
;;; !(with-results-to-file "neg1_pos1.txt" (superpose (-1 1 )))
;;; !(with-output-to-file "neg1_pos1.txt" (and (println! -1)(println! 1)))


;; display and wait 
!(shell! "cat neg1_pos1.txt")


;!(test-diamond "space.txt" (new-space-with (-1 1)))
;!(shell! "cat space.txt")
;!(wait-for-key)

;!(test-diamond "fifo.txt" (fifo (-1 1)))
;!(shell! "cat fifo.txt")
;!(wait-for-key)

!(collapse (read-metta! (open! "neg1_pos1.txt" read)))


!(let $stream (open! "neg1_pos1.txt" read)
  (collapse (read-metta! $stream)))

!(wait-for-key)

!(test-diamond "from_file.txt" (open! "neg1_pos1.txt" read))
!(shell! "cat from_file.txt")
!(wait-for-key)







