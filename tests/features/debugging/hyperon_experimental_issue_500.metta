
; https://github.com/trueagi-io/hyperon-experimental/issues/500


(: all (-> Atom Bool))
(= (all $A)
   (if (== () $A) True
       (if (car-atom $A)
          (let $cdr (cdr-atom $A) (all $cdr))
	  False)
   )
)

(= (loop) (loop))
!(all (True False (loop) True))
