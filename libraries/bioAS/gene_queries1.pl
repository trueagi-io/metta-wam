%% ===============================
%% 📚 Load Required Libraries
%% ===============================

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(filesex)).
:- use_module(library(apply)).

%:- use_module(library(semweb/rdf11)).  will switch back to this later
%:- use_module(library(semweb/sparql_http)). % this never existed .. use thje right one isntead

%% ===============================
%% ⚙️ Configuration
%% ===============================

sparql_endpoint("http://localhost:3050/sparql").
rdf_db_directory('datastore_neo4j_out_v3'). % Persistent DB directory

%% ===============================
%% 📂 Load RDF Data from Splits (only if not already loaded)
%% ===============================

rdf_already_loaded(Shared, File) :-
    rdf_graph_property(Shared, source(File)), !.

rdf_load_if_needed(Shared, File) :-
    ( rdf_already_loaded(Shared, File) ->
      format("✅ Already loaded: ~w~n", [File])
    ;
      ( format("📂 Loading RDF file: ~w~n", [File]),
        rdf_load_needed(Shared, File),
        format("📥 Loaded RDF file: ~w~n", [File])
      )).


rdf_load_needed(Shared, File) :-
    % findall(X-Y,rdf_current_prefix(X,Y),Prefixes),
    rdf_load(File, [graph(Shared),register_namespaces(true),concurrent(4), multifile(true)]).


rdf_load_splits(Stem) :-
    format(atom(WildCard), '~w_split_rdf/~w_part_*.rdf', [Stem, Stem]),
    expand_file_name(WildCard, Files),
    maplist(rdf_load_if_needed(Stem), Files).

%rdf_name_space(File, List):- rdf_load(File, [graph(Shared),register_namespaces(true),prefixes(List), multifile(true)]).

mount_rdf_db_directory :-
    rdf_db_directory(DBDir),
    format("🚀 Mounting persistence at ~w ~n", [DBDir]),
    rdf_attach_db(DBDir, [access(read_write), concurrency(4), cache_size(4096)]).

:- initialization(mount_rdf_db_directory).

load_rdf_db_directory:-
   /*rdf_load_needed(DBDir, 'neo4j_out_v3_split_rdf/neo4j_out_v3_part_000001.rdf'),
    rdf_load_needed(DBDir, 'neo4j_out_v3_split_rdf/neo4j_out_v3_part_000002.rdf'),
    rdf_load_needed(DBDir, 'neo4j_out_v3_split_rdf/neo4j_out_v3_part_000003.rdf'),
    rdf_load_needed(DBDir, 'neo4j_out_v3_split_rdf/neo4j_out_v3_part_000004.rdf'),
    rdf_load_needed(DBDir, 'neo4j_out_v3_split_rdf/neo4j_out_v3_part_000005.rdf'),*/
    % use this since it has all the prefix decls
    %rdf_load_if_needed(neo4j_out_v3, 'neo4j_out_v3_split_rdf/neo4j_out_v3_part_0001.rdf').
    % right now lets load the smallest file
    % rdf_load('neo4j_out_v3_split_rdf/neo4j_out_v3_part_26.rdf'),
    % Load splits, skipping already loaded
    rdf_load_splits('neo4j_out_v3'),
    !.

:- initialization(writeln(?- load_rdf_db_directory)).

%% ===============================
%% 🛰️ Internal SPARQL Server Setup
%% ===============================

skip_op(_).
sparql_server_port(3050).

% SPARQL endpoint HTTP handler
:- http_handler('/sparql', sparql_query, []).

start_sparql_server(Port) :-
    mount_rdf_db_directory,
    sparql_endpoint(Endpoint),
    skip_op(http_server(http_dispatch, [port(Port)])),
    skip_op(format("🚀 SPARQL server started on port ~w at ~w~n", [Port, Endpoint])),
    !.

start_sparql_server:- sparql_server_port(Port), start_sparql_server(Port).

%% ===============================
%% 🎯 SPARQL Client Query
%% ===============================

fetch_first_40_triples(Triples) :-
    sparql_endpoint(URL),
    sparql_query('SELECT ?s ?p ?o WHERE {?s ?p ?o} LIMIT 40', RowList,
                 [ endpoint(URL), result_format(prolog) ]),
    maplist(row_to_triple, RowList, Triples).

row_to_triple(row(S,P,O), rdf(S,P,O)).

%% ===============================
%% 🖥️ Display Results
%% ===============================

print_triples([]).
print_triples([rdf(S,P,O)|Rest]) :-
    format('~w ~w ~w.~n', [S,P,O]),
    print_triples(Rest).

%% ===============================
%% 🚩 Initialization and Test Run
%% ===============================

:- initialization(main, main).

main :-
    start_sparql_server,
    sleep(2), % ensure server has fully initialized
    fetch_40_triples.

fetch_40_triples:-
    format("🔎 Fetching first 40 triples via SPARQL client...~n"),
    fetch_first_40_triples(Triples),
    print_triples(Triples),
    !.


end_of_file.


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
