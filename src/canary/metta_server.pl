/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

:- use_module(library(socket)).
:- use_module(library(thread)).

call_wdet(Goal,WasDet):- call(Goal),deterministic(WasDet).
% Helper to parse Server and Port
parse_service_port(Peer,DefaultPort, Server, Port) :-
    (   Peer = Server:Port -> true
    ;   integer(Peer) -> Server = localhost, Port = Peer
    ;   Server = Peer, Port = DefaultPort  % Default port if none specified
    ).


service_running(Alias):- thread_property(VSS,TS),VSS=Alias,TS=status(running),!.

% Start interpreter service with MSpace = &self
start_vspace_service(Port):-
    current_self(MSpace), start_vspace_service(MSpace,Port).
% see amples of using this https://github.com/logicmoo/hyperon-wam/blob/main/examples/features/distributed-processing/create-server.metta
start_vspace_service(MSpace,Port):-
      symbolic_list_concat([vspace_service,MSpace,Port],'_',Alias),
      start_vspace_service(Alias,MSpace,Port).

start_vspace_service(Alias,_Space,_Port):-  service_running(Alias),!.
start_vspace_service(Alias,MSpace,Port):-
      thread_create(run_vspace_service(MSpace,Port),_,[detached(true),alias(Alias)]).


:- dynamic(was_vspace_port_in_use/2).

handle_port_in_use(MSpace,Port):-
   assert(was_vspace_port_in_use(MSpace,Port)),
   Port100 is Port +100,run_vspace_service(MSpace,Port100).


run_vspace_service(MSpace,Port):-
    catch(
      run_vspace_service_unsafe(MSpace,Port),
      error(socket_error(eaddrinuse,_),_),
      handle_port_in_use(MSpace,Port)).

run_vspace_service_unsafe(MSpace,Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5), tcp_open_socket(Socket, ListenFd),
    not_compatio(fbugio(run_vspace_service(MSpace,Port))),
    retractall(vspace_port(_)),
    assert(vspace_port(Port)),
    accept_vspace_connections(MSpace,ListenFd).

accept_vspace_connections(MSpace,ListenFd) :-
    tcp_accept(ListenFd, RemoteFd, RemoteAddr),
    nb_setval(self_space,MSpace),
    format(atom(ThreadAlias0), 'peer_~w_~w_~w_', [RemoteAddr,RemoteFd,MSpace]),
    gensym(ThreadAlias0,ThreadAlias),
    thread_create(setup_call_cleanup(
            tcp_open_socket(RemoteFd, Stream),
            ignore(handle_vspace_peer(Stream)),
            catch(close(Stream),_,true)), _, [detached(true), alias(ThreadAlias)] ),
    accept_vspace_connections(MSpace,ListenFd).

handle_vspace_peer(Stream) :-
    recv_term(Stream, Goal),
    (   Goal \= end_of_file
    ->  (   catch(call_wdet(Goal,WasDet), Error, true)
             *->  (   var(Error) ->  send_term(Stream, success(Goal,WasDet)) ;   send_term(Stream,error(Error)))
             ;send_term(Stream, 'failed'))),
        handle_vspace_peer(Stream).

any_to_i(A,I):- integer(A),I=A.
any_to_i(A,I):- format(atom(Ay),'~w',[A]),atom_number(Ay,I).
% Start the service automatically on a default port or a specified port
:- dynamic vspace_port/1.
get_vspace_port(Port):- current_prolog_flag('argv',L),member(AA,L),atom_concat('--service=',P,AA),atom_number(P,Port),!,set_prolog_flag('port',Port).
get_vspace_port(Port):- current_prolog_flag('port',P),any_to_i(P,Port),!.
get_vspace_port(Port):- vspace_port(Port),!.
get_vspace_port(Port):- Port = 3023.
start_vspace_service:- is_compiling,!.
start_vspace_service:-  get_vspace_port(Port), start_vspace_service(Port),!.



% Helper to establish connection
connect_to_service(HostPort, Stream) :-
    parse_service_port(HostPort, 3023, Host, Port),
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port),
    tcp_open_socket(Socket, Stream).

% Helper to send goal and receive response
send_term(Stream, MeTTa) :-  write_canonical(Stream, MeTTa),writeln(Stream, '.'), flush_output(Stream).
recv_term(Stream, MeTTa) :-  read_term(Stream, MeTTa, []).


% Read and process the service's response
read_response(Stream,Goal) :-
   flush_output(Stream),
    repeat, recv_term(Stream,Response),
    (Response == failed -> (!,fail) ;
       (Response = error(Throw) -> throw(Throw) ;
         ((Response = success(Goal,WasDet)),
            (WasDet==true-> (!, true) ; true)))).

% Connects to the service and sends the goal
% ?- remote_call('localhost', member(X, [1,2,3])).
remote_call(Peer, Goal) :-
    setup_call_cleanup(
        (connect_to_service(Peer, Stream),send_term(Stream, Goal)),
        read_response(Stream,Goal),
        close(Stream)).

remote_eval(Peer, MeTTa, Result) :-
   remote_call(Peer, eval(MeTTa,Result)).

/*
;; Example usage (from MeTTa)

metta> !(remote-eval!  localhost (add-atom &self (A b b)))
metta> !(remote-eval!  localhost (add-atom &self (A b c)))
metta> !(remote-eval!  localhost (match &self $Code $Code))

*/


:- dynamic remote_code/4.  % Maps  MeTTa-Space and function to Service address

our_address(Host:Port):- gethostname(Host),vspace_port(Port).
we_exist(Addr):- our_address(Addr).

they_exist(Addr):-
   our_address(Ours),
   diff(Addr,Ours),
   execute_goal(we_exist(Addr)), \+ our_address(Addr).

% tell the services that took our place about us.
register_ready:-
  our_address(Ours),
  forall(was_vspace_port_in_use(MSpace,Port),
     remote_call(Port,register_remote_code(MSpace,we_exist(_),true,Ours))).

% before we terminate we should call this
:- at_halt(register_gone).
register_gone:- \+ service_running(_),!.
register_gone:-
 ignore((
   fail,
  our_address(Ours),
  forall(they_exist(Addr),
     remote_call(Addr,unregister_peer(Ours))))).

unregister_peer(Who):-
    forall(remote_code(MSpace,EntryPoint, _, Who),
       unregister_remote_code(MSpace,EntryPoint,Who)).

% Registers a predicate to a service
register_remote_code(MSpace,EntryPoint, NonDet, Server) :-
   unregister_remote_code(MSpace,EntryPoint, Server),
   assertz(remote_code(MSpace,EntryPoint, NonDet, Server)).
unregister_remote_code(MSpace,EntryPoint, Server) :-
   retractall(remote_code(MSpace,EntryPoint, _, Server)).



execute_goal(Goal):-
    current_self(MSpace),
    execute_goal(MSpace,Goal, IsDet),
    (was_t(IsDet) -> ! ; true).

execute_goal(_Self,true, _) :- !.
% Meta-interpreter with cut handling
execute_goal(MSpace,Goal, IsDet) :-
    remote_code(MSpace,Goal, NonDet, Peer),    % If the goal is registered for a service, call remotely
    (was_t(NonDet) -> true ; !),
    remote_call(Peer, execute_goal(MSpace,Goal,IsDet)).

execute_goal(_Self,!, IsDet) :- !,  IsDet = true.  % Handle cuts
execute_goal(_Self,fail, IsDet) :- !, (was_t(IsDet)->throw(cut_fail); fail).
execute_goal(MSpace,Goal, _) :- predicate_property(Goal,number_of_clauses(_)),!,
    clause(Goal, Body),  % Retrieve the clause body for the goal
    catch(execute_goal(MSpace,Body, IsDet),cut_fail,(!,fail)),
    (was_t(IsDet)-> !; true).
execute_goal(MSpace,call(Cond), _ ) :- !, execute_goal(MSpace,Cond, IsDet), (was_t(IsDet)->!;true).
execute_goal(MSpace,(Cond, Then), IsDet) :- !, execute_goal(MSpace,Cond, IsDet), execute_goal(MSpace,Then, IsDet).
execute_goal(MSpace,(Cond; Else), IsDet) :- !, (execute_goal(MSpace,Cond, IsDet); execute_goal(MSpace,Else, IsDet)).
execute_goal(MSpace,(Cond *-> Then; Else), IsDet) :- !, (execute_goal(MSpace,Cond, IsDet) *->  execute_goal(MSpace,Then, IsDet) ; execute_goal(MSpace,Else, IsDet)).
execute_goal(MSpace,(Cond *->  Then), IsDet) :- !, (execute_goal(MSpace,Cond, IsDet) *->  execute_goal(MSpace,Then, IsDet)).
execute_goal(MSpace,(Cond -> Then; Else), IsDet) :- !, (execute_goal(MSpace,Cond, IsDet) ->  execute_goal(MSpace,Then, IsDet) ; execute_goal(MSpace,Else, IsDet)).
execute_goal(MSpace,(Cond -> Then), IsDet) :- !, (execute_goal(MSpace,Cond, IsDet) -> execute_goal(MSpace,Then, IsDet)).
execute_goal(MSpace,catch(X, E, Z), IsDet) :- !, catch(execute_goal(MSpace,X, IsDet) , E,  execute_goal(MSpace,Z, _)).
execute_goal(MSpace,findall(X, Y, Z), _) :- !, findall(X, execute_goal(MSpace,Y, _), Z).
execute_goal(MSpace,forall(X, Y), _) :- !, forall(execute_goal(MSpace,X, _), execute_goal(MSpace,Y, _)).
execute_goal(_Self,SubGoal, _IsCut) :- call_wdet(SubGoal, WasDet), (was_t(WasDet)->!;true).

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
metta_hyperpose(Eq,RetType,Depth,MSpace,InList,Res) :- fail,  InList=[_,_|_],!,  % only use extra threads iof 2 or more
    setup_call_cleanup(
             concurrent_assert_result(eval_20(Eq,RetType,Depth,MSpace), InList, Tag),
             each_result_in_order(Tag, InList, Res),
             cleanup_results(Tag)).
metta_hyperpose(Eq,RetType,Depth,MSpace,ArgL,Res):- eval_20(Eq,RetType,Depth,MSpace,['superpose',ArgL],Res).


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


% :- initialization(start_vspace_service).

