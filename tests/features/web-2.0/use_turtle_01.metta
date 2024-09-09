!(use-module! (@ library semweb/turtle))
!(use-module! (@ library semweb/rdf_db))
!(rdf-load! data.ttl)
; (: q (-> Atom Atom)) (= (q $q) $q) 
(: show (-> Atom Atom))
(= (show $call) 
   (let  (quote $unquote) $call
	  (do (, $unquote (println! $call)))))

!(show (quote (rdf! $s $p $o)))
;(quote (rdf! http://example/class/Location http://www.w3.org/2000/01/rdf-schema#subClassOf http://example/class/NamedEntity))
;(quote (rdf! http://example/class/Location http://www.w3.org/2000/01/rdf-schema#label (literal  Location)))
;(quote (rdf! http://example/class/NamedEntity http://www.w3.org/2000/01/rdf-schema#label (literal  NamedEntity)))
;(quote (rdf! http://example/elem/loc/loc1 http://www.w3.org/2000/01/rdf-schema#label (literal  'label: Amsterdam')))
;(quote (rdf! http://example/elem/loc/loc1 http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://example/class/Location))
;(quote (rdf! http://example/elem/loc/loc1 http://example/prop/lookupName (literal  amsterdam)))
;(quote (rdf! http://example/elem/loc/loc2 http://www.w3.org/2000/01/rdf-schema#label (literal  'label: Den Haag')))
;(quote (rdf! http://example/elem/loc/loc2 http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://example/class/Location))
;(quote (rdf! http://example/elem/loc/loc2 http://example/prop/lookupName (literal  'den haag')))
;(quote (rdf! http://example/elem/loc/loc3 http://www.w3.org/2000/01/rdf-schema#label (literal  'label: IJmuiden')))
;(quote (rdf! http://example/elem/loc/loc3 http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://example/class/Location))
;(quote (rdf! http://example/elem/loc/loc3 http://example/prop/lookupName (literal  ijmuiden)))
;(quote (rdf! http://example/elem/loc/loc3 http://example/prop/disabled (literal  (type  http://www.w3.org/2001/XMLSchema#boolean true))))
;(quote (rdf! http://example/class/Person http://www.w3.org/2000/01/rdf-schema#subClassOf http://example/class/NamedEntity))
;(quote (rdf! http://example/class/Person http://www.w3.org/2000/01/rdf-schema#label (literal  Person)))
;(quote (rdf! http://example/elem/person/puk http://www.w3.org/2000/01/rdf-schema#label (literal  'label: Puk van de Petteflet')))
;(quote (rdf! http://example/elem/person/puk http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://example/class/Person))
;(quote (rdf! http://example/elem/person/puk http://example/prop/lookupName (literal  'puk van de petteflet')))


(: show2 (-> Atom Atom))
(= (show2 $call) 
   (let (quote $quoted) (quote (quote $call)) 
	 (let  (quote $unquote) $quoted
		(do (, $unquote (println! $quoted))))))

; !(show2 (rdf! $s $p $o))

