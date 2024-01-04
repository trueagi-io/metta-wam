
:- use_module(library(socket)).
:- use_module(library(thread)).

call_wdet(Goal,WasDet):- call(Goal),deterministic(WasDet).
% Helper to parse Server and Port
parse_server_port(ServerPort,DefaultPort, Server, Port) :-
    (   ServerPort = Server:Port -> true
    ;   integer(ServerPort) -> Server = localhost, Port = ServerPort
    ;   Server = ServerPort, Port = DefaultPort  % Default port if none specified
    ).

start_vspace_server(Port) :-
    thread_create(run_vspace_server(Port),_,[detached(true),alias(vspace_server)]).

run_vspace_server(Port) :-
    tcp_socket(Socket),  tcp_bind(Socket, Port), tcp_listen(Socket, 5), tcp_open_socket(Socket, ListenFd),
    fbug(run_vspace_server(Port)),
    accept_vspace_connections(ListenFd).

accept_vspace_connections(ListenFd) :-
    tcp_accept(ListenFd, ClientFd, ClientAddr),
    format(atom(ThreadAlias), 'client_~w', [ClientAddr]),
    thread_create(setup_call_cleanup(
            tcp_open_socket(ClientFd, Stream),
            ignore(handle_vspace_client(Stream)),
            catch(close(Stream),_,true)), _, [detached(true), alias(ThreadAlias)] ),
    accept_vspace_connections(ListenFd).

handle_vspace_client(Stream) :-
    read_term(Stream, Goal,[]),
    (   Goal \= end_of_file
    ->  (   catch(call_wdet(Goal,WasDet), Error, true)
             *->  (   var(Error) ->  send_term(Stream, success(Goal,WasDet)) ;   send_term(Stream,error(Error)))
             ;send_term(Stream, 'failed'))),
        handle_vspace_client(Stream).

% Start the server automatically on a default port or a specified port
:- dynamic vspace_port/1.
start_vspace_server:- (   vspace_port(Port) -> start_vspace_server(Port); start_vspace_server(3023) ).

:- initialization(start_vspace_server).

% Connects to the server and sends the goal
remote_call(ServerPort, Goal) :-
    setup_call_cleanup(
        (connect_to_server(ServerPort, Stream),send_term(Stream, Goal)),
        read_response(Stream,Goal),
        close(Stream)).

% Helper to establish connection
connect_to_server(HostPort, Stream) :-
    parse_server_port(HostPort, 3023, Host, Port),
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, Stream).

% Helper to send goal and receive response
send_term(Stream, Goal) :-  write_canonical(Stream, Goal),writeln(Stream, '.'), flush_output(Stream).


% Read and process the server's response
read_response(Stream,Goal) :-
   flush_output(Stream),
    repeat, read_term(Stream,Response,[]),
    (Response == failed -> (!,fail) ;
       (Response = error(Throw) -> throw(Throw) ;
         ((Response = success(Goal,WasDet)),
            (WasDet==true-> (!, true) ; true)))).

% Example usage
% ?- remote_call('localhost', member(X, [1,2,3])).

:- dynamic predicate_server/2.  % Maps predicate to server

% Registers a predicate to a server
register_predicate_server(Predicate, Server) :- assertz(predicate_server(Predicate, Server)).

% Meta-interpreter with cut handling
execute_goal(Goal, IsCut) :-
    predicate_server(Goal, Server), !,
    remote_call(Server, Goal),  % If the goal is registered for a server, call remotely
    IsCut = _.
execute_goal(!, IsCut) :- !,  IsCut = cut.  % Handle cuts
execute_goal(Goal, IsCut) :- predicate_property(Goal,numberr_of_clauses(_)),!,
    clause(Goal, Body),  % Retrieve the clause body for the goal
    execute_goal(Body, IsCut),
    (nonvar(IsCut)->!;true).
execute_goal((SubGoal1, SubGoal2), IsCut) :- !, execute_goal(SubGoal1, IsCut), execute_goal(SubGoal2, IsCut).
execute_goal((SubGoal1; SubGoal2), IsCut) :- !, (execute_goal(SubGoal1, IsCut); execute_goal(SubGoal2, IsCut)).
execute_goal((SubGoal1 -> SubGoal2), IsCut) :- !, (execute_goal(SubGoal1, IsCut) ->  execute_goal(SubGoal2, IsCut)).
execute_goal(findall(X, Y, Z), _) :- !, findall(X, execute_goal(Y, _), Z).
execute_goal(forall(X, Y), _) :- !, forall(execute_goal(X, _), execute_goal(Y, _)).
execute_goal(SubGoal, _IsCut) :- call_wdet(SubGoal, _WasDet).

