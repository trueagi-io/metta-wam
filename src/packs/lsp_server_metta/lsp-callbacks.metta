
;; sometimes a something like this would be nice
;; !(bind! file-context &lsp-server)

!(add-atom &lsp-server
    (= (hook-hover-string $Path $Loc $Term $Arity)
	   (format-args "Looking at {}" ($Term))))


!(add-atom &lsp-server
	(= (hook-hover-print $Path $Loc $Term $Arity)
	   (println! (format-args "Printing {}" ($Term)))))


!(add-atom &lsp-server
	(= (compute-typed-code-action $ObjectType $Uri $Range $Object)
	 (sequential
	   ((fail)
		(lsp-debug! todo (quote (compute-typed-code-action $ObjectType $Uri $Range $Object)))))))


!(add-atom &lsp-server
	(= (compute-each-code-lens $Uri $Lvl $Ord $Kind $What $VL $Path $Range)
	 (sequential
	   ( ;; (fail)
		(lsp-debug! todo (quote (compute-each-code-lens $Uri $Lvl $Ord $Kind $What $VL $Path $Range)))))))




