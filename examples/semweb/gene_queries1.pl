:- module(metta_queries, [
    query_system_named/3,
    start_sparql_server/0,
    run_sparql_internal/2
]).

%% ===============================
%% Load Required SWI-Prolog Libraries
%% ===============================

:- use_module(library(semweb/rdf_db)).          % RDF store
:- use_module(library(semweb/rdf_persistency)). % RDF persistence
:- use_module(library(semweb/sparql_client)).   % SPARQL querying
:- use_module(library(semweb/rdf_sparql_server)). % Internal SPARQL server

% Set SPARQL endpoint for external queries
sparql_endpoint("http://your-sparql-endpoint.com/sparql").

%% ===============================
%% 1️⃣ Start SWI-Prolog SPARQL Server (Internal)
%% ===============================

start_sparql_server :-
    rdf_attach_db('rdf_data', []), % Persist RDF data
    rdf_load('your_rdf_data.ttl', [format(turtle)]), % Load RDF dataset
    http_server(sparql_server, [port(3050)]), % Start SPARQL server on port 3050
    format("SPARQL server started on http://localhost:3050/sparql~n").

%% ===============================
%% 2️⃣ SPARQL Queries (Remote & Internal)
%% ===============================

query_system_named("brca2", sparql_query, "
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
    }").

query_system_named("igf2", sparql_query, "
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
    }").

run_sparql_internal(Query, Results) :-
    sparql_query(Query, Results, [ endpoint('http://localhost:3050/sparql')]).

%% ===============================
%% 3️⃣ RDF-Based Queries (Local SWI-Prolog RDF Store)
%% ===============================

query_system_named("brca2", call, (
    rdf(Gene, ex:gene_name, literal(type(_, "BRCA2"))),
    rdf(Gene, ex:transcribed_to, Transcript),
    rdf(Transcript, ex:translates_to, Protein),
    rdf(Interactor, ex:interacts_with, Protein),
    rdf(Gene, ex:genes_pathways, Pathway)
)).

query_system_named("igf2", call, (
    rdf(Gene, ex:gene_name, literal(type(_, "IGF2"))),
    rdf(Promoter, ex:associated_with, Gene),
    rdf(Enhancer, ex:associated_with, Gene),
    rdf(Gene, ex:genes_pathways, Pathway),
    rdf(ChildPathway, ex:child_pathway_of, Pathway)
)).

%% ===============================
%% 4️⃣ MeTTa-Compatible Queries (Stored as Strings)
%% ===============================

query_system_named("brca2", metta_query, "
!(query
    (rdf! $Gene ex:gene_name  [literal [type $_ \"BRCA2\"]])
    (rdf! $Gene ex:transcribed_to $Transcript)
    (rdf! $Transcript ex:translates_to $Protein)
    (rdf! $Interactor ex:interacts_with $Protein)
    (rdf! $Gene ex:genes_pathways $Pathway)
)").

query_system_named("igf2", metta_query, "
!(query
    (rdf! $Gene ex:gene_name  [literal [type $_ \"IGF2\"]])
    (rdf! $Promoter ex:associated_with $Gene)
    (rdf! $Enhancer ex:associated_with $Gene)
    (rdf! $Gene ex:genes_pathways $Pathway)
    (rdf! $ChildPathway ex:child_pathway_of $Pathway)
)").

%% ===============================
%% 5️⃣ Performance Optimizations (Indexing)
%% ===============================

:- rdf_set_predicate(ex:gene_name, indexing(true)).
:- rdf_set_predicate(ex:transcribed_to, indexing(true)).
:- rdf_set_predicate(ex:translates_to, indexing(true)).
:- rdf_set_predicate(ex:interacts_with, indexing(true)).
:- rdf_set_predicate(ex:genes_pathways, indexing(true)).
:- rdf_set_predicate(ex:associated_with, indexing(true)).

%% ===============================
%% 6️⃣ How to Use This Module
%% ===============================
/*
1️⃣ Load the Prolog module:
   ?- [metta_queries].

2️⃣ Start the Internal SPARQL Server:
   ?- start_sparql_server.

   ✅ Server will run on: http://localhost:3050/sparql

3️⃣ Retrieve a Query by Name & Type:
   ?- query_system_named("brca2", sparql_query, Query).
   ?- query_system_named("igf2", call, Query).
   ?- query_system_named("brca2", metta_query, Query).

4️⃣ Run a Local RDF Query:
   ?- query_system_named("brca2", call, Query), call(Query).

5️⃣ Execute a Query Against the Internal SPARQL Server:
   ?- query_system_named("brca2", sparql_query, Query), run_sparql_internal(Query, Results).

6️⃣ Send a MeTTa Query for Execution:
   ?- query_system_named("brca2", metta_query, Query), eval_metta_log(Query).
   ?- query_system_named("igf2", metta_query, Query), eval_metta_log(Query).

This module allows Prolog to:
✔ Start an internal SPARQL server for local querying.
✔ Use SPARQL for remote and internal RDF queries.
✔ Query RDF data using `rdf/3` for in-memory Prolog databases.
✔ Store and retrieve MeTTa queries as structured strings.
✔ Optimize query execution with indexing.
*/

