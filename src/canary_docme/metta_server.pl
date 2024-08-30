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
% Load the socket and thread libraries for networking and concurrency
:- use_module(library(socket)).  % Provides predicates for socket operations
:- use_module(library(thread)).  % Provides predicates for multi-threading

% Predicate to execute a goal and determine if it was deterministic
%!  call_wdet(+Goal, -WasDet) is semidet.
%
%   Calls the given Goal and checks if it was deterministic.
%
%   @arg Goal is the goal to execute.
%   @arg WasDet is true if the Goal was deterministic, false otherwise.
call_wdet(Goal,WasDet):- 
    % Execute the provided Goal
	call(Goal),
    % Check if the goal was deterministic and unify the result with WasDet
	deterministic(WasDet).

% Helper to parse Server and Port from Peer, using a DefaultPort if needed
%!  parse_service_port(+Peer, +DefaultPort, -Server, -Port) is det.
%
%   Parses the service and port from Peer input. Defaults to localhost
%   and DefaultPort if not specified.
%
%   @arg Peer is the input that could be in the form of Server:Port or just a Port.
%   @arg DefaultPort is the port to use if Peer does not specify it.
%   @arg Server is the output server address.
%   @arg Port is the output port number.
parse_service_port(Peer,DefaultPort, Server, Port) :-
    % Check if Peer is in the form Server:Port
    (   Peer = Server:Port -> true
    ;   % If Peer is an integer, assume it's a port with localhost as the server
        integer(Peer) -> Server = localhost, Port = Peer
    ;   % Otherwise, use Peer as the server and DefaultPort as the port
        Server = Peer, Port = DefaultPort
    ).

% Predicate to check if a service is running under a specific alias
%!  service_running(+Alias) is semidet.
%
%   Checks if a thread with the given Alias is currently running.
%
%   @arg Alias is the alias of the thread to check.
service_running(Alias):-
    % Get the properties of the thread VSS and check if its status is running
   thread_property(VSS,TS),
    VSS = Alias, 
    TS = status(running), 
    !.

% Start the interpreter service using the current self (MSpace)
%!  start_vspace_service(+Port) is det.
%
%   Starts the VSpace service on the specified Port, using the current self as MSpace.
%
%   @arg Port is the port number on which the service will be started.
start_vspace_service(Port):-
%Getthecurrentself(MSpace)
    current_self(MSpace), 
    % Start the VSpace service with the current MSpace and specified Port
    start_vspace_service(MSpace,Port).

% Start the VSpace service with a specific alias, MSpace, and Port
%!  start_vspace_service(+Alias, +MSpace, +Port) is det.
%
%   Starts the VSpace service with a specified Alias, MSpace, and Port.
%
%   @arg Alias is the alias to assign to the service thread.
%   @arg MSpace is the memory space in which the service will operate.
%   @arg Port is the port number on which the service will be started.
start_vspace_service(MSpace,Port):-
    % Concatenate 'vspace_service', MSpace, and Port into an Alias string
      symbolic_list_concat([vspace_service,MSpace,Port],'_',Alias),
    % Start the VSpace service with the generated Alias, MSpace, and Port
      start_vspace_service(Alias,MSpace,Port).

% Skip starting the service if it is already running
%!  start_vspace_service(+Alias, +_Space, +_Port) is det.
%
%   Starts the VSpace service only if it is not already running under the given Alias.
%
%   @arg Alias is the alias to check for an existing service.
start_vspace_service(Alias,_Space,_Port):-  
    % If the service is already running under Alias, do nothing
 service_running(Alias),
 !.

% Create a new thread to run the VSpace service if not already running
start_vspace_service(Alias,MSpace,Port):-
    % Create a new thread to run the VSpace service with the given MSpace and Port
      thread_create(run_vspace_service(MSpace,Port),_,[detached(true),alias(Alias)]).

% Predicate to handle the situation when a port is already in use
%!  handle_port_in_use(+MSpace, +Port) is det.
%
%   Handles the error when the specified Port is already in use by trying another port.
%
%   @arg MSpace is the memory space in which the service operates.
%   @arg Port is the port number that is in use.
handle_port_in_use(MSpace,Port):-
    % Record that the port was in use for MSpace
   assert(was_vspace_port_in_use(MSpace,Port)),
    % Try starting the service on Port + 100
   Port100 is Port +100,
   run_vspace_service(MSpace,Port100).


% Run the VSpace service, handling the case where the port is already in use
%!  run_vspace_service(+MSpace, +Port) is det.
%
%   Runs the VSpace service on the specified Port, retrying on a different port if necessary.
%
%   @arg MSpace is the memory space in which the service operates.
%   @arg Port is the port number on which the service will be started.
run_vspace_service(MSpace,Port):-
    % Attempt to run the service, catching the error if the port is in use
    catch(
      run_vspace_service_unsafe(MSpace,Port),
      error(socket_error(eaddrinuse,_),_),
        % If the port is in use, handle the situation
        handle_port_in_use(MSpace, Port)
    ).

% Unsafe version of running the VSpace service that doesn't handle errors
%!  run_vspace_service_unsafe(+MSpace, +Port) is det.
%
%   Unsafe version of running the VSpace service on the specified Port.
%   This version does not handle errors related to the port being in use.
%
%   @arg MSpace is the memory space in which the service operates.
%   @arg Port is the port number on which the service will be started.
run_vspace_service_unsafe(MSpace,Port) :-
    % Create a TCP socket
    tcp_socket(Socket),
    % Bind the socket to the specified port
    tcp_bind(Socket, Port),
    % Listen on the socket with a backlog of 5 connections
    tcp_listen(Socket, 5), 
    % Open the socket for listening
    tcp_open_socket(Socket, ListenFd),
    % Perform any compatibility checks (not_compatio is assumed to be a custom predicate)
    not_compatio(fbugio(run_vspace_service(MSpace,Port))),
    % Remove any existing vspace_port facts
    retractall(vspace_port(_)),
    % Assert the current port as the vspace_port
    assert(vspace_port(Port)),
    % Start accepting connections on the listening socket
    accept_vspace_connections(MSpace,ListenFd).

% Accept connections to the VSpace service and create a thread for each connection
%!  accept_vspace_connections(+MSpace, +ListenFd) is det.
%
%   Accepts incoming connections to the VSpace service and creates a thread for each connection.
%
%   @arg MSpace is the memory space in which the service operates.
%   @arg ListenFd is the file descriptor for the listening socket.
accept_vspace_connections(MSpace,ListenFd) :-
    % Accept an incoming connection, returning a file descriptor and remote address
    tcp_accept(ListenFd, RemoteFd, RemoteAddr),
    % Set the current memory space for the thread
    nb_setval(self_space,MSpace),
    % Create a unique thread alias based on the remote address and file descriptor
    format(atom(ThreadAlias0), 'peer_~w_~w_~w_', [RemoteAddr,RemoteFd,MSpace]),
    % Generate a unique symbol for the thread alias
    gensym(ThreadAlias0,ThreadAlias),
    % Create a new thread to handle the connection
    thread_create(
       setup_call_cleanup(
            % Open the socket as a stream
            tcp_open_socket(RemoteFd, Stream),
            % Handle the connection by processing incoming goals
            ignore(handle_vspace_peer(Stream)),
            % Ensure the stream is closed when done
            catch(close(Stream),_,true)
            ), 
             _,
             [detached(true), alias(ThreadAlias)] 
             ),
    % Continue accepting more connections
    accept_vspace_connections(MSpace,ListenFd).

% Handle a peer connection by receiving and processing goals
%!  handle_vspace_peer(+Stream) is det.
%
%   Handles a peer connection by receiving and executing goals sent over the Stream.
%
%   @arg Stream is the input/output stream connected to the peer.
handle_vspace_peer(Stream) :-
    % Receive a Prolog term (goal) from the stream
    recv_term(Stream, Goal),
    % If the received term is not the end of file
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






% Helper to establish a connection to the VSpace service
%!  connect_to_service(+HostPort, -Stream) is det.
%
%   Connects to the VSpace service on the specified Host and Port and returns the Stream.
%
%   @arg HostPort is the Host:Port combination or just a port number.
%   @arg Stream is the output stream connected to the service.
connect_to_service(HostPort, Stream) :-
    % Parse the Host and Port from the input HostPort
    parse_service_port(HostPort, 3023, Host, Port),
    % Create a TCP socket
    tcp_socket(Socket),
    % Connect the socket to the specified Host and Port
    tcp_connect(Socket, Host:Port),
    % Open the socket as a stream for communication
    tcp_open_socket(Socket, Stream).

% Helper to send a Prolog term and receive a response
%!  send_term(+Stream, +MeTTa) is det.
%
%   Sends a Prolog term (MeTTa) over the Stream.
%
%   @arg Stream is the output stream to send the term through.
%   @arg MeTTa is the Prolog term to send.
send_term(Stream, MeTTa) :-
    % Write the term in canonical form to the stream
    write_canonical(Stream, MeTTa),
    % Write a period to indicate the end of the term
    writeln(Stream, '.'),
    % Flush the output to ensure the term is sent immediately
    flush_output(Stream).
    
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

% Declare remote_code/4 as a dynamic predicate to allow runtime modification
:- dynamic remote_code/4.  % Maps  MeTTa-Space and function to Service address

% Get the current address of the service (Host:Port)
%!  our_address(-HostPort) is det.
%
%   Retrieves the current Host and Port of this service instance.
%
%   @arg HostPort is the output in the form Host:Port.
our_address(Host:Port):- 
    % Get the hostname of the current machine
  gethostname(Host),
    % Retrieve the port number currently in use by this service
  vspace_port(Port).

% Check if this service instance exists at a given address
%!  we_exist(+Addr) is det.
%
%   Determines if the current service instance exists at the specified Addr.
%
%   @arg Addr is the address to check (Host:Port).
we_exist(Addr):-
    % Get the current address and unify it with Addr
  our_address(Addr).

% Check if another service exists at the specified address
%!  they_exist(+Addr) is det.
%
%   Determines if another service exists at the specified Addr.
%
%   @arg Addr is the address to check (Host:Port).
they_exist(Addr):-
    % Get the current service address
   our_address(Ours),
    % Ensure Addr is different from the current service address
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

% Execute a goal in the current memory space
%!  execute_goal(+Goal) is det.
%
%   Executes the specified goal in the current memory space.
%
%   @arg Goal is the goal to execute.
execute_goal(Goal):-
    % Get the current memory space (MSpace)
    current_self(MSpace),
    % Execute the goal in the current memory space and determine if it was deterministic
    execute_goal(MSpace,Goal, IsDet),
    % If the goal was deterministic, cut to prevent backtracking
    (was_t(IsDet) -> ! ; true).

% Always succeed if the goal is 'true'
execute_goal(_Self,true, _) :- !.
% Meta-interpreter with cut handling
%!  execute_goal(+MSpace, +Goal, -IsDet) is det.
%
%   Executes the specified goal within the given memory space, handling cuts and determinism.
%
%   @arg MSpace is the memory space in which the goal will be executed.
%   @arg Goal is the goal to execute.
%   @arg IsDet is true if the goal was deterministic.
execute_goal(MSpace,Goal, IsDet) :-
    remote_code(MSpace,Goal, NonDet, Peer),    
    % If the goal is registered for a service, call remotely
    (was_t(NonDet) -> true ; !),
    remote_call(Peer, execute_goal(MSpace,Goal,IsDet)).

execute_goal(_Self,!, IsDet) :- !,  IsDet = true.  % Handle cuts
execute_goal(_Self,fail, IsDet) :- !, 
  (was_t(IsDet)->throw(cut_fail); fail).
execute_goal(MSpace,Goal, _) :-
  predicate_property(Goal,number_of_clauses(_)),!,
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

