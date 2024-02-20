
!(switch_to_mettalog!)

(= (wait-for-key)
 (, (println! "Press 'en-ee' key to continue...")
    (case (get-single-char-fn)
     
    (((101 (halt! 6)))
    (98 (repl!))
    ($_ ())))))

; !(pragma! sandbox False) ;; in case we are in SWISH
; !(pragma! host-functions all) ;; throw a security exception if use is not logged in

!(bind! &iter1 (new-space))

!(add-atom &iter1  -1)
!(add-atom &iter1  1)

(= (zip $i $j) ($i $j))
(= (double $x) (* $x 2))
(= (positive $x) (> $x 0))

; plain old metta
!(switch_to_mettarust!)

!(let $i (get-atoms &iter1)
  ((double $i) (positive $i)))

!(wait-for-key)
!(switch_to_mettalog!)

!(import! &self ../io/file-output.metta)

(= (iterate $obj)
   (case ($obj (get-mettatype $obj))
      (($_ GroundedSpace) (get-atoms $obj))
      (((fifo $list) $_) (superpose $list))
      (($_ $_InputStream) (read-metta $obj))
      ($_ $obj)))


(= (new-space-with $fill)
  (let $space (new-space) 
      (superpose 
        (do (add-atom $space (superpose $fill))) 
          $space)))

(= (map $p2 $itr)
   (lazy-map! $p2 (iterate $itr)))

(= (test-diamond $filename $obj)
   (let $isPositive (map (< 0) $obj)
      (let $doubledNumbers (map (* 2) $obj)
          (let $combinedStream (zip isPositive $doubledNumbers)
             (with-results-to-file $filename combinedStream)))))

!(switch_to_mettalog!)

;; create our input file
;;; !(with-results-to-file "neg1_pos1.txt" (superpose (-1 1 )))
!(with-output-to-file "neg1_pos1.txt" (and (println! -1)(println! 1)))


;; display and wait 
!(shell! "cat neg1_pos1.txt")
!(wait-for-key)

!(test-diamond "space.txt" (new-space-with (-1 1)))
!(shell! "cat space.txt")
!(wait-for-key)

!(test-diamond "fifo.txt" (fifo (-1 1)))
!(shell! "cat fifo.txt")
!(wait-for-key)

!(test-diamond "from_file.txt" (open! "neg1_pos1.txt" input))
!(shell! "cat from_file.txt")
!(wait-for-key)







