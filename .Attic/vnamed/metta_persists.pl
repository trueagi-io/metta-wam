% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

% Import necessary libraries for handling RDF, Turtle, N-Triples, and RDF persistence.
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).        % Turtle and TriG formats
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(semweb/rdf_persistency)).

%!  start_persistency is det.
%
%   Initializes the RDF database by attaching the persistent RDF store.
%   The directory for persistence is retrieved from the 'METTALOG_CACHE_DIR'
%   environment variable or defaults to '~/.local/mettalog/persistence'.
%   This predicate ensures that persistency is started only if the RDF database
%   is not already attached.
%
%   @example Start RDF database persistence:
%       ?- start_persistency.
%
start_persistency:-
    rdf_current_db(_), !.  % If the RDF database is already active, do nothing.
start_persistency:-
    % Retrieve the directory path from the environment variable or use the default.
    (getenv('METTALOG_CACHE_DIR', Dir); Dir = '~/.local/mettalog/persistence'), !,
    % Expand the directory path to an absolute path and attach the RDF database.
    expand_file_name(Dir, [AbsDir]),
    rdf_attach_db(AbsDir, []).

% need to check for ~/.local/mettalog/persistence/lock

%!  shutdown_persistency is det.
%
%   Detaches the RDF database, ensuring that any changes are saved.
%   This predicate safely shuts down the persistence system.
%
%   @example Shutdown RDF database persistence:
%       ?- shutdown_persistency.
%
shutdown_persistency:-
    rdf_current_db(_) -> rdf_detach_db ; true.

% Ensure that the persistency system is shut down on halt.
%:- initialization(start_persistency, after_load /**/).
:- at_halt(shutdown_persistency).

%!  query_example(-S, -P, -O, -G) is nondet.
%
%   Example query to retrieve triples from the RDF store.
%   Limits the query to 10 results.
%
%   @arg S Unifies with the subject of the triple.
%   @arg P Unifies with the predicate of the triple.
%   @arg O Unifies with the object of the triple.
%   @arg G Unifies with the graph of the triple.
%
%   @example Retrieve triples from the RDF store:
%       ?- query_example(S, P, O, G).
%
query_example(S, P, O, G) :-
    limit(10, rdf(S, P, O, G)).

%!  test_persistency is det.
%
%   Tests the RDF persistency system by loading an RDF file and querying it.
%   The test function loads an example RDF file in N-Triples format and performs
%   a query to retrieve triples. The results are printed, and then the database
%   is safely shut down.
%
%   @example Load and query an RDF file:
%       ?- test_persistency.
%
test_persistency :-
    rdf_load('TestTicket1893.nt'),
    %rdf_load('/home/deb12user/metta-wam/tests/features/web-2.0/monarch-kg.nt'),
    % Perform an example query to test the setup and print the results.
    query_example(S, P, O, G),
    format('Subject: ~w, Predicate: ~w, Object: ~w~n Graph: ~w', [S, P, O, G]), !,
    % Shut down the persistency system, ensuring all changes are written to disk.
    shutdown_persistency.



