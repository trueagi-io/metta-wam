;; This File allows MeTTaLog to:
;; ✔ Start an internal SPARQL server for local persistence and querying.
;; ✔ Use SPARQL Client for remote and internal RDF queries.
;; ✔ Query RDF data using `rdf/3` for in-memory RDF databases.
;; ✔ Store and retrieve Prolog queries as structured strings.
;; ✔ Optimize query execution with indexing.


;Time an expression and return the result
(: time (-> Atom $a))
(= (time $expr)
   (let $start ((py-atom time.time))
   (let $res $expr
   (let $stop ((py-atom time.time))
   (Time (- $stop $start) $res)))))

;; ===============================
;; Load Required Semantic Modules from SWI-Prolog Libraries
;; ===============================

!(use-module! [library semweb/rdf_db])        ;; RDF Store
!(use-module! [library semweb/rdf_persistency]) ;; RDF Persistence
!(use-module! [library semweb/sparql_client])  ;; SPARQL Client
!(use-module! [library semweb/rdf_sparql_server]) ;; Internal SPARQL Server
!(use-module! [library semweb/turtle])        ;; Turtle and TriG
!(use-module! [library semweb/rdf_ntriples]) ;; endpoint-info.nt
!(use-module! [library semweb/rdf_zlib_plugin])
!(use-module! [library semweb/rdf_http_plugin])
!(use-module! [library semweb/http_ssl_plugin])

;; Set SPARQL endpoint for external queries
(sparql_endpoint "http://your-sparql-endpoint.com/sparql")

;; ===============================
;; Start SWI-Prolog SPARQL Server (Internal)
;; ===============================

(= (start_sparql_server)
  ((rdf_attach_db! rdf_data ()) ; Persist RDF data
   (rdf_load! your_rdf_data.ttl (format[turtle])) ; Load RDF dataset
   (http_server! sparql_server (port[3050])) ; Start SPARQL server on port 3050
   (println! "SPARQL server started on http://localhost:3050/sparql~n")))

;; ===============================
;; SPARQL Queries (Remote & Internal)
;; ===============================

(test-query brca2  sparql "
    PREFIX ex: <http://example.org/schema/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?gene ?transcript ?protein ?interactor ?pathway
    WHERE {
        ?gene rdf:type ex:Gene ;
              ex:gene_name ?geneName .
        FILTER regex(?geneName, 'BRCA2', 'i')

        ?gene ex:transcribed_to ?transcript .
        ?transcript ex:translates_to ?protein .
        ?interactor ex:interacts_with ?protein .
        ?gene ex:genes_pathways ?pathway .
    }")

(test-query igf2 sparql "
    PREFIX ex: <http://example.org/schema/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?promoter ?gene ?enhancer ?pathway ?childPathway
    WHERE {
        ?gene rdf:type ex:Gene ;
              ex:gene_name ?geneName .
        FILTER regex(?geneName, 'IGF2', 'i')

        ?promoter ex:associated_with ?gene .
        ?enhancer ex:associated_with ?gene .
        ?gene ex:genes_pathways ?pathway .
        ?childPathway ex:child_pathway_of ?pathway .
    }")

;; Run the SPARQL client to get the result
(= (sparql-client $query)
 ;; call-fn '2'  means insert a new arg 2 and return it as the function return 
    (call-fn 2 (sparql! $query ([endpoint http://localhost:3050/sparql]))))

;; ===============================
;; Prolog RDF-Based Queries (Stored as Strings)
;; ===============================

(test-query brca2  prolog-rdf '
    rdf(Gene, ex:gene_name, literal(type(_, "BRCA2"))),
    rdf(Gene, ex:transcribed_to, Transcript),
    rdf(Transcript, ex:translates_to, Protein),
    rdf(Interactor, ex:interacts_with, Protein),
    rdf(Gene, ex:genes_pathways, Pathway) ')

(test-query igf2  prolog-rdf '
    rdf(Gene, ex:gene_name, literal(type(_, "IGF2"))),
    rdf(Promoter, ex:associated_with, Gene),
    rdf(Enhancer, ex:associated_with, Gene),
    rdf(Gene, ex:genes_pathways, Pathway),
    rdf(ChildPathway, ex:child_pathway_of, Pathway) ')

;; ===============================
;; MeTTa-Compatible Queries (Stored as Expressions)
;; ===============================

(test-query brca2  metta-rdf 
!(m 
    (rdf! $Gene ex:gene_name  [literal [type $_ "BRCA2"]])
    (rdf! $Gene ex:transcribed_to $Transcript)
    (rdf! $Transcript ex:translates_to $Protein)
    (rdf! $Interactor ex:interacts_with $Protein)
    (rdf! $Gene ex:genes_pathways $Pathway)
))

(test-query igf2  metta-rdf 
!(m 
    (rdf! $Gene ex:gene_name  [literal [type $_ "IGF2"]])
    (rdf! $Promoter ex:associated_with $Gene)
    (rdf! $Enhancer ex:associated_with $Gene)
    (rdf! $Gene ex:genes_pathways $Pathway)
    (rdf! $ChildPathway ex:child_pathway_of $Pathway)
))

;; ===============================
;; Performance Optimizations (Indexing)
;; ===============================
(= (ensure-indexed $pred)
   ;; this convert to ultra hash table
    (call-p rdf_set_predicate $pred [indexing true]))

!(ensure-indexed ex:gene_name)
!(ensure-indexed ex:transcribed_to)
!(ensure-indexed ex:translates_to)
!(ensure-indexed ex:interacts_with)
!(ensure-indexed ex:genes_pathways)
!(ensure-indexed ex:associated_with)

;; ===============================
;; 6️⃣ How to Use This Code
;; ===============================

; 1️⃣ Load This Code into MeTTaLog
;    !(import! metta_queries)

; 2️⃣ Start the Internal SPARQL Server:
; ✅ Server will run on: http://localhost:3050/sparql
!(start_sparql_server)

; 3️⃣ Retrieve a $query by Name & Type:
!(m (test-query brca2  sparql $query))
!(m (test-query igf2  prolog-rdf $query))
!(m (test-query brca2  metta-rdf $query))

; 4️⃣ Run a Local RDF $query:
!(m (test-query brca2  prolog-rdf $query) (call-prolog $query))

; 5️⃣ Execute a $query Against the Internal SPARQL Server:
!(m (test-query brca2  sparql $query) (call-fn sparql-internal ($query $Results)))

; 6️⃣ Send a MeTTa $query for Execution:
!(m (test-query brca2  metta-rdf $metta-code) (eval $metta-code))
!(m (test-query igf2  metta-rdf $metta-code) (eval $metta-code))


