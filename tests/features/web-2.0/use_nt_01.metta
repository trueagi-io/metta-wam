!(use-module! (@ library semweb/rdf_ntriples))
!(use-module! (@ library semweb/rdf_db))
(: show (-> Atom Atom))
(= (show $call) 
   (let  (quote $unquote) $call
	  (do (, $unquote (println! $call)))))


!(rdf-load! rio/ntriples/test.nt)
!(show (quote (rdf! $s $p $o)))

;output:
;(quote (rdf! http://example.org/resource1 http://example.org/property http://example.org/resource2))
;(quote (rdf! _:file:///home/deb12user/metta-wam/tests/features/web-2.0/rio/ntriples/test.ntanon http://example.org/property http://example.org/resource2))
;(quote (rdf! http://example.org/resource2 http://example.org/property _:file:///home/deb12user/metta-wam/tests/features/web-2.0/rio/ntriples/test.ntanon))
;(quote (rdf! http://example.org/resource3 http://example.org/property http://example.org/resource2))
;(quote (rdf! http://example.org/resource4 http://example.org/property http://example.org/resource2))
;(quote (rdf! http://example.org/resource5 http://example.org/property http://example.org/resource2))
;(quote (rdf! http://example.org/resource6 http://example.org/property http://example.org/resource2))
;(quote (rdf! http://example.org/resource7 http://example.org/property (literal  'simple literal')))
;(quote (rdf! http://example.org/resource8 http://example.org/property (literal  backslash:\)))
;(quote (rdf! http://example.org/resource9 http://example.org/property (literal  'dquote:"')))
;(quote (rdf! http://example.org/resource10 http://example.org/property (literal  newline:
;)))
;)))ote (rdf! http://example.org/resource11 http://example.org/property (literal  return
;(quote (rdf! http://example.org/resource12 http://example.org/property (literal  tab:   )))
;(quote (rdf! http://example.org/resource13 http://example.org/property http://example.org/resource2))
;(quote (rdf! http://example.org/resource14 http://example.org/property (literal  x)))
;(quote (rdf! http://example.org/resource15 http://example.org/property _:file:///home/deb12user/metta-wam/tests/features/web-2.0/rio/ntriples/test.ntanon))
;(quote (rdf! http://example.org/resource16 http://example.org/property (literal  \u00E9)))
;(quote (rdf! http://example.org/resource17 http://example.org/property (literal  \u20AC)))
;(quote (rdf! http://example.org/resource21 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral ''))))
;(quote (rdf! http://example.org/resource22 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral ' '))))
;(quote (rdf! http://example.org/resource23 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral x))))
;(quote (rdf! http://example.org/resource23 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral "))))
;(quote (rdf! http://example.org/resource24 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral <a></a>))))
;(quote (rdf! http://example.org/resource25 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral 'a <b></b>'))))
;(quote (rdf! http://example.org/resource26 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral 'a <b></b> c'))))
;(quote (rdf! http://example.org/resource26 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral a
;<b></b>
;c))))
;(quote (rdf! http://example.org/resource27 http://example.org/property (literal  (type  http://www.w3.org/2000/01/rdf-schema#XMLLiteral chat))))
;(quote (rdf! http://example.org/resource30 http://example.org/property (literal  (lang  fr chat))))
;(quote (rdf! http://example.org/resource31 http://example.org/property (literal  (lang  en chat))))
;(quote (rdf! http://example.org/resource32 http://example.org/property (literal  (type  http://example.org/datatype1 abc))))

