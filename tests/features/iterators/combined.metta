

!(pragma! sandbox False) ;; in case we are in SWISH
!(pragma! host-functions all) ;; throw a security exception if use is not logged in

!(import! ../io/file-output.metta)

(!bind &iter1 (new-space))

(add-atom &iter1  -1)
(add-atom &iter1  1)

(= (zip $i $j) ($i $j))
(= (double $x) (* $x 2))
(= (postive $x) (> $x 0))

; plain old metta
!(let $i (get-atoms &iter1)
  ((double $i) (positive $i)))

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


(= (test-diamond $fileout $constructor)
 (with-result-to-file $fileout
  (let $obj (eval! $constructor)
    (let $i (iterate $obj)
      (zip (double $i) (positive $i))))))


!(with-results-to-file "neg1_pos1.txt" (superpose -1 1 ))

!(test-diamond "from_file.txt" (open! "neg1_pos1.txt" input))
!(shell "cat from_file.txt")
!(break!)


!(test-diamond "space.txt" (new-space-with (-1 1)))
!(shell "cat space.txt")
!(break!)

!(test-diamond "fifo.txt" (fifo (-1 1)))
!(shell "cat fifo.txt")
!(break!)







