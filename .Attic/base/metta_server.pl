
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

:- dynamic(was_vspace_server_in_use/1).

vspace_server_in_use(Port):-
   assert(was_vspace_server_in_use(Port)),
   Port100 is Port +100,run_vspace_server(Port100).

run_vspace_server(Port):-
    catch(
      run_vspace_server0(Port),
      error(socket_error(eaddrinuse,_),_),
      vspace_server_in_use(Port)).

run_vspace_server0(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5), tcp_open_socket(Socket, ListenFd),
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

:- dynamic remote_code/3.  % Maps predicate to server

% Registers a predicate to a server
register_remote_code(Predicate, NonDet, Server) :- assertz(remote_code(Predicate, NonDet, Server)).

% Meta-interpreter with cut handling
execute_goal(Goal, IsCut) :-
    remote_code(Goal, NonDet, Server),    % If the goal is registered for a server, call remotely
    (was_t(NonDet) -> true ; !),
    remote_call(Server, execute_goal(Goal,IsCut)).


execute_goal(!, IsCut) :- !,  IsCut = true.  % Handle cuts
execute_goal(fail, IsCut) :- !, (was_t(IsCut)->throw(cut_fail); fail).
execute_goal(Goal, _) :- predicate_property(Goal,number_of_clauses(_)),!,
    clause(Goal, Body),  % Retrieve the clause body for the goal
    catch(execute_goal(Body, IsCut),cut_fail,(!,fail)),
    (was_t(IsCut)-> !; true).
execute_goal(call(Cond), _ ) :- !, execute_goal(Cond, IsCut), (was_t(IsCut)->!;true).
execute_goal((Cond, Then), IsCut) :- !, execute_goal(Cond, IsCut), execute_goal(Then, IsCut).
execute_goal((Cond; Else), IsCut) :- !, (execute_goal(Cond, IsCut); execute_goal(Else, IsCut)).
execute_goal((Cond *-> Then; Else), IsCut) :- !, (execute_goal(Cond, IsCut) *->  execute_goal(Then, IsCut) ; execute_goal(Else, IsCut)).
execute_goal((Cond *->  Then), IsCut) :- !, (execute_goal(Cond, IsCut) *->  execute_goal(Then, IsCut)).
execute_goal((Cond -> Then; Else), IsCut) :- !, (execute_goal(Cond, IsCut) ->  execute_goal(Then, IsCut) ; execute_goal(Else, IsCut)).
execute_goal((Cond -> Then), IsCut) :- !, (execute_goal(Cond, IsCut) -> execute_goal(Then, IsCut)).
execute_goal(catch(X, E, Z), IsCut) :- !, catch(execute_goal(X, IsCut) , E,  execute_goal(Z, _)).
execute_goal(findall(X, Y, Z), _) :- !, findall(X, execute_goal(Y, _), Z).
execute_goal(forall(X, Y), _) :- !, forall(execute_goal(X, _), execute_goal(Y, _)).
execute_goal(SubGoal, _IsCut) :- call_wdet(SubGoal, WasDet), (was_t(WasDet)->!;true).

was_t(T):- T == true.


ccml_nth:attr_unify_hook(_Nth,_Var).

metta_hyperpose_v0(P2, InList, OutList) :-
    current_prolog_flag(cpu_count,Count),
    length(InList,Len), length(OutList,Len),
    max_min(Count,Len,_,Procs),
    findall(thread(Goal, OutputVar),
            (nth1(N, InList, InputVar), Goal = call(P2, InputVar, OutputVar), put_attr(OutputVar,ccml_nth,N)),
            GoalsWithOutputs),
    separate_goals_and_outputs(GoalsWithOutputs, Goals, OutList),
    concurrent(Procs, Goals, []).

separate_goals_and_outputs([], [], []).
separate_goals_and_outputs([thread(Goal, OutputVar)|GoalsWithOutputs], [Goal|Goals], [OutputVar|Outputs]) :-
    separate_goals_and_outputs(GoalsWithOutputs, Goals, Outputs).





%:- use_module(library(concurrent)).

% Meta predicate that combines concurrent processing and result gathering
metta_concurrent_maplist(P2, InList, OutList) :-  InList=[_,_|_],!,  % only use extra threads iof 2 or more
    setup_call_cleanup(
             concurrent_assert_result(P2, InList, Tag),
             gather_results_in_order(Tag, InList, OutList),
             cleanup_results(Tag)).
metta_concurrent_maplist(P2, InList, OutList):- maplist(P2, InList, OutList).

% Meta predicate that combines concurrent processing and result gathering
metta_hyperpose(Eq,RetType,Depth,Self,InList,Res) :- fail,  InList=[_,_|_],!,  % only use extra threads iof 2 or more
    setup_call_cleanup(
             concurrent_assert_result(eval_20(Eq,RetType,Depth,Self), InList, Tag),
             each_result_in_order(Tag, InList, Res),
             cleanup_results(Tag)).
metta_hyperpose(Eq,RetType,Depth,Self,ArgL,Res):- eval_20(Eq,RetType,Depth,Self,['superpose',ArgL],Res).


% Concurrently applies P2 to each element of InList, results are tagged with a unique identifier
concurrent_assert_result(P2, InList, Tag) :-
    current_prolog_flag(cpu_count,Count),
    length(InList,Len), max_min(Count,Len,_,Procs),
    gensym(counter, Tag),  % Generate a unique identifier
    concurrent_forall( nth1(Index, InList, InputVar),assert_result_after_computation(P2, Tag, Index, InputVar), [threads(Procs)]).
    %findall(assert_result_after_computation(P2, Tag, Index, InputVar), nth1(Index, InList, InputVar), Goals),
    %concurrent(Procs, Goals, []).

% Asserts the output of applying P2 to Input
assert_result_after_computation(P2, Tag, Index, Input) :-
    catch(
      (call(P2, Input, Output)*-> assert(result(Tag, Index, Input, Output)) ;  assert(result(Tag, Index, Input, failed(Tag)))),
                E, (assert(result(Tag, Index, Input, error(E))))).


% Gathers results in order, matching them with the corresponding inputs
gather_results_in_order(Tag, InList, OrderedResults) :-
    gather_results_in_order(Tag, InList, 0, OrderedResults).

use_result( IInput, RResult,  Input, Result):- var(RResult),!,IInput=Input,Result=RResult.
use_result( IInput, error(E),  Input,  _Result):- ignore(IInput=Input),!, throw(E).
use_result( IInput,  failed(_),  Input,  _Result):-  ignore(IInput=Input),!,fail.
use_result( IInput, RResult,  Input, Result):- IInput=Input,Result=RResult.

gather_results_in_order(_, [], _, []).
gather_results_in_order(Tag, [Input|RestInputs], Index, [Result|OrderedResults]) :-
     ( result(Tag, Index, IInput, RResult)
       *->  (use_result( IInput, RResult,  Input, Result),NextIndex is Index + 1,gather_results_in_order(Tag, RestInputs, NextIndex, OrderedResults))
      ;   % Wait for 75 milliseconds before retrying
        (  sleep(0.075),  gather_results_in_order(Tag, [Input|RestInputs], Index, [Result|OrderedResults]))).


each_result_in_order(Tag, InList, OrderedResults) :-
    each_result_in_order(Tag, InList, 0, OrderedResults).
each_result_in_order(_, [], _,_):-!,fail.
each_result_in_order(Tag, [Input|RestInputs], Index,Result) :-
     ( result(Tag, Index, IInput, RResult)
       *->  (use_result( IInput, RResult,  Input, Result);
                (NextIndex is Index + 1,each_result_in_order(Tag, RestInputs, NextIndex, Result)))
      ;   % Wait for 75 milliseconds before retrying
        (  sleep(0.075),  each_result_in_order(Tag, [Input|RestInputs], Index,Result))).


% Cleanup predicate to remove asserted results from the database
cleanup_results(Tag) :-
    retractall(result(Tag, _, _, _)).



:- initialization(start_vspace_server).

