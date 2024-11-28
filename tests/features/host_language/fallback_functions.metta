
(= ($myfun . $args)
      (registered-python-function $pymod $pyfn $myfun)
      (symbol-p $myfun) ;; ensures RPF doesnt just return the unbound $myfun 
      (py-call-fn (: $pymod ($myfun . $args))))

