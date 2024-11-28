
;; sometimes a something like this would be nice
;; !(bind! file-context &lsp-server)

!(add-atom &lsp-server
    (= (hook-hover-string $path $loc $term $arity)
	   (format-args "looking at {}" ($term))))


!(add-atom &lsp-server
	(= (hook-hover-print $path $loc $term $arity)
	   (println! (format-args "printing {}" ($term)))))

;; code lenses appear inline as to what is available even without user asking
!(add-atom &lsp-server
	(= (compute-each-code-lens $uri $lvl $ord $kind $what $vl $path $range)
	 (sequential
	   ( ;; (fail)
		(lsp-debug! todo (quote (compute-each-code-lens $uri $lvl $ord $kind $what $vl $path $range)))))))

;; code actions appear only when user asks what they can do
!(add-atom &lsp-server
	(= (compute-typed-code-action $objecttype $uri $range $object)
	 (sequential
	   ( ;; (fail)
		(lsp-debug! todo (quote (compute-typed-code-action $objecttype $uri $range $object)))))))

;; code lenses and actions have almost the same json format
(registered-every-file 
 ((range
    ((start (line 0) (character 0))
     (end (line 0) (character 1))))
  (command
    ((title "Run File At Toplevel")
     (command re-run-file)
     (arguments ($Uri))))))

;; example of an action 
!(add-atom &lsp-server
	(= (re-run-file $path)
	   (include $path)))



