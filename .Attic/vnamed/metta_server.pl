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

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

%!  call_wdet(+Goal, -WasDet) is nondet.
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

%!  start_vspace_service(+Alias, +Space, +Port) is det.
%
%   Starts the VSpace service only if it is not already running under the given Alias.
%
%   @arg Alias is the alias to check for an existing service.
%   @arg Space is the memory space in which the service operates.
%   @arg Port is the port number on which the service will be started.
start_vspace_service(Alias,_Space,_Port):-
    % If the service is already running under Alias, do nothing
 service_running(Alias),
 !.

% Create a new thread to run the VSpace service if not already running
start_vspace_service(Alias,MSpace,Port):-
    % Create a new thread to run the VSpace service with the given MSpace and Port
      thread_create(run_vspace_service(MSpace,Port),_,[detached(true),alias(Alias)]).

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
    if_trace(main,not_compatio(fbugio(run_vspace_service(MSpace,Port)))),
    % Remove any existing vspace_port facts
    retractall(vspace_port(_)),
    % Assert the current port as the vspace_port
    assert(vspace_port(Port)),
    % Start accepting connections on the listening socket
    accept_vspace_connections(MSpace,ListenFd).

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
            % Generate a unique symbol for the thread alias
           (nb_setval(self_space,MSpace),
            % Handle the connection by processing incoming goals
            ignore(handle_vspace_peer(Stream))),
            % Ensure the stream is closed when done
            catch(close(Stream),_,true)
            ),
             _,
             [detached(true), alias(ThreadAlias)]
             ),
    % Continue accepting more connections
    accept_vspace_connections(MSpace,ListenFd).

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

%!  recv_term(+Stream, -MeTTa) is det.
%
%   Receives a term from the specified input Stream and unifies it with MeTTa.
%   This predicate reads a Prolog term from the Stream using read_term/3.
%
%   @arg Stream The input stream from which the term is read.
%   @arg MeTTa The variable to unify with the read term.
%
%   @example Receive a term from a stream:
%       ?- recv_term(Stream, Term).
%
recv_term(Stream, MeTTa) :-
    % Read a term from the stream and unify it with MeTTa.
    read_term(Stream, MeTTa, []).

%!  read_response(+Stream, +Goal) is det.
%
%   Reads and processes the service's response from the input Stream.
%   This predicate continuously reads terms from the Stream until it either fails or succeeds.
%
%   @arg Stream The input stream from which the response is read.
%   @arg Goal The goal that was sent to the service.
%
%   @example Read and process a service's response:
%       ?- read_response(Stream, my_goal).
%
read_response(Stream, Goal) :-
    % Ensure all output to the stream is flushed.
    flush_output(Stream),
    % Repeat the following steps until a termination condition is met.
    repeat,
        % Receive a term from the stream and unify it with Response.
        recv_term(Stream, Response),
        % Handle different types of responses.
        (
            % If the response indicates failure, cut and fail.
            Response == failed -> (!, fail)
            ;
            % If the response is an error, throw the associated exception.
            (Response = error(Throw) -> throw(Throw)
            ;
            % If the response indicates success, check if it was deterministic.
            (Response = success(Goal, WasDet) ->
                % If the goal was deterministic, cut and succeed; otherwise, continue.
                (WasDet == true -> (!, true) ; true)
            )
        )).

%!  remote_call(+Peer, +Goal) is det.
%
%   Connects to a remote service, sends a goal, and waits for the response.
%   This predicate sets up a connection to the specified Peer, sends the Goal,
%   and processes the response from the service.
%
%   @arg Peer The address of the remote service.
%   @arg Goal The goal to send to the remote service.
%
%   @example Send a goal to a remote service:
%       ?- remote_call('localhost', member(X, [1,2,3])).
%
remote_call(Peer, Goal) :-
    % Setup connection and cleanup steps.
    setup_call_cleanup(
        % Establish a connection to the service and send the goal.
        (connect_to_service(Peer, Stream), send_term(Stream, Goal)),
        % Read and process the response from the service.
        read_response(Stream, Goal),
        % Ensure the stream is closed after processing.
        close(Stream)
    ).

%!  remote_eval(+Peer, +MeTTa, -Result) is det.
%
%   Sends an evaluation request to a remote service and retrieves the result.
%   This predicate sends a request to evaluate a MeTTa expression on the specified Peer
%   and unifies the result with Result.
%
%   @arg Peer The address of the remote service.
%   @arg MeTTa The MeTTa expression to evaluate.
%   @arg Result The variable to unify with the evaluation result.
%
%   @example Evaluate a MeTTa expression remotely:
%       ?- remote_eval('localhost', [_], Result).
%
remote_eval(Peer, MeTTa, Result) :-
    % Call the remote service to evaluate the MeTTa expression.
    remote_call(Peer, eval(MeTTa, Result)).


/*
;; Example usage (from MeTTa)

metta> !(remote-eval!  localhost (add-atom &self (A b b)))
metta> !(remote-eval!  localhost (add-atom &self (A b c)))
metta> !(remote-eval!  localhost (match &self $Code $Code))

*/

% Declare remote_code/4 as a dynamic predicate to allow runtime modification
:- dynamic remote_code/4.  % Maps  MeTTa-Space and function to Service address


%!  our_address(-HostPort) is det.
%
%   Retrieves the current Host and Port of this service instance.
%
%   @arg HostPort is the output in the form Host:Port.
% Get the current address of the service (Host:Port)
our_address(Host:Port):-
    % Get the hostname of the current machine
  gethostname(Host),
    % Retrieve the port number currently in use by this service
  vspace_port(Port).

%!  we_exist(+Addr) is det.
%
%   Determines if the current service instance exists at the specified Addr.
%
%   @arg Addr is the address to check (Host:Port).
% Check if this service instance exists at a given address

:- dynamic(we_exist/1).
%we_exist(Addr):-
    % Get the current address and unify it with Addr
 % our_address(Addr).

% Check if another service exists at the specified address
%!  they_exist(+Addr) is det.
%
%   Determines if another service exists at the specified Addr.
%
%   @arg Addr is the address to check (Host:Port).
they_exist(Addr):-
    % Get the current service address
   % our_address(Ours),
    % Ensure Addr is different from the current service address
   % dif(Addr,Ours),
   execute_goal(we_exist(Addr)), \+ our_address(Addr).

% Inform services that have taken over about our presence.
%!  register_ready is det.
%
%   This predicate registers the current service with all remote services that are occupying
%   the ports we are supposed to use. It ensures that our presence is acknowledged by
%   those remote services.
%
%   @example Register the current service:
%       ?- register_ready.
%
register_ready :-
    % Retrieve our own service address.
    our_address(Ours),
    % For each virtual space port in use, notify the occupying service.
    forall(was_vspace_port_in_use(MSpace, Port),
           % Register with the remote service that is occupying the port.
           remote_call(Port, register_remote_code(MSpace, we_exist(_), true, Ours))).





% Ensure that we unregister before terminating the service.
% The directive ensures register_gone/0 is called at halt.
:- at_halt(register_gone).

%!  register_gone is det.
%
%   This predicate should be called before the service terminates to unregister
%   itself from all known remote services. It ensures a clean exit by informing
%   other services of our departure.
%
%   @example Unregister the current service:
%       ?- register_gone.
%
register_gone :-
    % If no service is currently running, succeed.
    \+ service_running(_), !.

register_gone :-
    % Ignore any potential failures during the unregister process.
    ignore((
    % Currently fails to skip the operation; modify as needed.
      %  fail,
        % Retrieve our own service address.
        our_address(Ours),
        % For each known remote service, inform them of our departure.
        forall(they_exist(Addr),
               % Unregister from the remote service.
               remote_call(Addr, unregister_peer(Ours))))).

%!  unregister_peer(+Who) is det.
%
%   Unregisters all remote codes associated with a particular peer from the system.
%
%   @arg Who The address or identifier of the peer to be unregistered.
%
%   @example Unregister a specific peer:
%       ?- unregister_peer('localhost:8080').
%
unregister_peer(Who) :-
    % For each remote code registered by the peer, unregister it.
    forall(remote_code(MSpace, EntryPoint, _, Who),
           % Unregister the remote code for the specified peer.
           unregister_remote_code(MSpace, EntryPoint, Who)).

%!  register_remote_code(+MSpace, +EntryPoint, +NonDet, +Server) is det.
%
%   Registers a predicate (EntryPoint) within a given module space (MSpace) to a remote service.
%
%   @arg MSpace The module space in which the predicate operates.
%   @arg EntryPoint The predicate to register.
%   @arg NonDet Boolean flag indicating if the predicate is non-deterministic.
%   @arg Server The remote server to register the predicate with.
%
%   @example Register a predicate to a remote service:
%       ?- register_remote_code(my_mspace, my_predicate, false, 'localhost:8080').
%
register_remote_code(MSpace, EntryPoint, NonDet, Server) :-
    % Unregister any existing remote code for the same entry point.
    unregister_remote_code(MSpace, EntryPoint, Server),
    % Assert the new remote code into the system.
    assertz(remote_code(MSpace, EntryPoint, NonDet, Server)).

%!  unregister_remote_code(+MSpace, +EntryPoint, +Server) is det.
%
%   Unregisters a predicate (EntryPoint) from a remote service within a given module space (MSpace).
%
%   @arg MSpace The module space from which the predicate operates.
%   @arg EntryPoint The predicate to unregister.
%   @arg Server The remote server from which to unregister the predicate.
%
%   @example Unregister a predicate from a remote service:
%       ?- unregister_remote_code(my_mspace, my_predicate, 'localhost:8080').
%
unregister_remote_code(MSpace, EntryPoint, Server) :-
    % Retract all instances of the remote code for the specified entry point and server.
    retractall(remote_code(MSpace, EntryPoint, _, Server)).

%!  execute_goal(+Goal) is det.
%
%   Executes a goal within the context of the current module space (MSpace).
%   The goal is executed with consideration for determinism.
%
%   @arg Goal The goal to execute.
%
%   @example Execute a goal:
%       ?- execute_goal(my_goal).
%
execute_goal(Goal) :-
    % Retrieve the current module space.
    current_self(MSpace),
    % Execute the goal within the module space.
    execute_goal(MSpace, Goal, IsDet),
    % If the goal is deterministic, cut to prevent backtracking.
    (was_t(IsDet) -> ! ; true).


%!  execute_goal(+Self, +Goal, +IsDet) is nondet.
%
%   Executes a given Goal in the context of a specified execution environment (Self).
%   This predicate handles various types of goals, including simple goals, remote calls, cuts, and failures.
%
%   @arg Self The execution environment or module space in which the Goal is executed.
%   @arg Goal The goal to execute.
%   @arg IsDet Boolean flag indicating if the goal should be deterministic.
%
%   @example Simple goal execution:
%       ?- execute_goal(mspace, true, IsDet).
%       IsDet = true.
%
%   @example Handling a cut:
%       ?- execute_goal(mspace, !, IsDet).
%       IsDet = true.
%

% The goal 'true' is always successful, so mark it as deterministic and cut.
execute_goal(_Self, true, _) :-
    % Cut to prevent backtracking.
    !.

% Handle remote execution of the goal if it is registered for remote service.
execute_goal(MSpace, Goal, IsDet) :-
    % Retrieve the remote execution details.
    remote_code(MSpace, Goal, NonDet, Peer),
    % If the goal is non-deterministic, we don't proceed further.
    % If NonDet is true, we proceed, otherwise we cut and stop.
    (was_t(NonDet) -> true ; !),
    % Perform the remote call.
    remote_call(Peer, execute_goal(MSpace, Goal, IsDet)).

% Handle cuts (!), marking the goal as deterministic.
execute_goal(_Self, !, IsDet) :-
    % Cut to ensure that this clause is not retried.
    !,
    % Set IsDet to true, indicating that the goal is deterministic.
    IsDet = true.

% Interpret the Goal `fail`.
execute_goal(_Self, fail, IsDet) :-
    % Cut to prevent further processing.
    !,
    % If IsDet is true, throw an exception; otherwise, fail.
    % IsDet was true (thus cutted), we send a cut_throw to bail fast; otherwise, fail.
    (was_t(IsDet) -> throw(cut_fail); fail).

% If the goal has associated clauses, process it by retrieving and executing the body.
execute_goal(MSpace, Goal, _) :-
    % Check if the goal has clauses.
    predicate_property(Goal, number_of_clauses(_)),
    % Cut to prevent backtracking.
    !,
    % Retrieve the body of the clause.
    clause(Goal, Body),
    % Execute the body and handle any exceptions.
    catch(execute_goal(MSpace, Body, IsDet), cut_fail, (!, fail)),
    % If IsDet is true, cut; otherwise, proceed.
    (was_t(IsDet) -> !; true).

% Handle the call/1 meta-predicate by executing the enclosed goal.
execute_goal(MSpace, call(Cond), _) :-
    % Cut to ensure this clause is chosen.
    !,
    % Execute the condition.
    execute_goal(MSpace, Cond, IsDet),
    % Cut if IsDet is true; otherwise, continue.
    (was_t(IsDet) -> !; true).

% Handle conjunction of goals (Cond, Then).
execute_goal(MSpace, (Cond, Then), IsDet) :-
    % Cut to prevent alternative clauses.
    !,
    % Execute the first part (Cond).
    execute_goal(MSpace, Cond, IsDet),
    % Execute the second part (Then).
    execute_goal(MSpace, Then, IsDet).

% Handle disjunction of goals (Cond; Else).
execute_goal(MSpace, (Cond; Else), IsDet) :-
    % Cut to choose between Cond and Else.
    !,
    % Try to execute Cond.
    (execute_goal(MSpace, Cond, IsDet);
    % If Cond fails, execute Else.
    execute_goal(MSpace, Else, IsDet)).

% Handle soft cut with else branch (Cond *-> Then; Else).
execute_goal(MSpace, (Cond *-> Then; Else), IsDet) :-
    % Cut after selecting the clause.
    !,
    % Try to execute Cond and then Then.
    (execute_goal(MSpace, Cond, IsDet) *->
    % If successful, execute Then.
    execute_goal(MSpace, Then, IsDet);
    % Otherwise, execute Else.
    execute_goal(MSpace, Else, IsDet)).

% Handle soft cut without else branch (Cond *-> Then).
execute_goal(MSpace, (Cond *-> Then), IsDet) :-
    % Cut to finalize this choice.
    !,
    % Execute Cond and Then if Cond is true.
    (execute_goal(MSpace, Cond, IsDet) *->
    execute_goal(MSpace, Then, IsDet)).

% Handle standard if-then-else construct (Cond -> Then; Else).
execute_goal(MSpace, (Cond -> Then; Else), IsDet) :-
    % Cut after clause selection.
    !,
    % Execute Cond and Then if Cond succeeds.
    (execute_goal(MSpace, Cond, IsDet) ->
    % If successful, execute Then.
    execute_goal(MSpace, Then, IsDet);
    % Otherwise, execute Else.
    execute_goal(MSpace, Else, IsDet)).

% Handle standard if-then construct without else (Cond -> Then).
execute_goal(MSpace, (Cond -> Then), IsDet) :-
    % Cut to prevent backtracking.
    !,
    % Execute Cond.
    (execute_goal(MSpace, Cond, IsDet) ->
    % If successful, execute Then.
    execute_goal(MSpace, Then, IsDet)).

% Handle exception catching construct catch(X, E, Z).
execute_goal(MSpace, catch(X, E, Z), IsDet) :-
    % Cut after processing this clause.
    !,
    % Execute X and handle exceptions.
    catch(execute_goal(MSpace, X, IsDet), E, execute_goal(MSpace, Z, _)).

% Handle findall/3 to collect all solutions of a goal.
execute_goal(MSpace, findall(X, Y, Z), _) :-
    % Cut to select this clause.
    !,
    % Find all solutions for Y and unify with Z.
    findall(X, execute_goal(MSpace, Y, _), Z).

% Handle forall/2 to ensure a goal holds for all solutions.
execute_goal(MSpace, forall(X, Y), _) :-
    % Cut to ensure clause selection.
    !,
    % Ensure Y is true for all solutions of X.
    forall(execute_goal(MSpace, X, _), execute_goal(MSpace, Y, _)).

% Default goal execution using call_wdet to check for determinism.
execute_goal(_Self, SubGoal, _IsCut) :-
    % Execute the goal and determine if it was deterministic.
    call_wdet(SubGoal, WasDet),
    % Cut if WasDet is true; otherwise, proceed.
    (was_t(WasDet) -> !; true).

% Helper predicate to check if a term is exactly 'true'.
was_t(T) :-
    % Succeed if T is true.
    T == true.

% Previously included directive to initialize the virtual space service.
:- initialization(start_vspace_service, after_load /**/).



:- nodebug(metta(telnet_server)).

:- use_module(library(socket)).
:- use_module(library(thread)).

% Start the Telnet server in a background thread on the given Port
start_dbg_telnet(Port) :-
    symbolic_list_concat([telnet_server,Port],'_', Alias),
    start_dbg_telnet(Port, Alias).

% The thread that runs the Telnet server, retries if port is in use
start_dbg_telnet(_Port, Alias):- thread_property(_, alias(Alias)), !,
    debug(metta(telnet_server),'Telnet server is already running: ~w.~n',[Alias]).
start_dbg_telnet(Port, Alias):-
    debug(metta(telnet_server),'Attempting to start Telnet server on port ~w.~n', [Port]),
    catch(
        (tcp_socket(Socket),
         tcp_bind(Socket, ip(0,0,0,0):Port),  % Bind to 0.0.0.0 to accept connections on all interfaces
         tcp_listen(Socket, 5),  % Maximum 5 concurrent connections
         debug(metta(telnet_server),'Telnet server started on port ~w, listening on 0.0.0.0.~n', [Port]),
         thread_create(accept_dbg_connections(Alias, Socket), _, [alias(Alias), detached(true)])
        ),
        error(_, _),  % Catch port in use error
        ( NewPort is Port + 10,
          debug(metta(telnet_server),'Port ~w in use, trying port ~w.~n', [Port, NewPort]),
          start_dbg_telnet(NewPort)  % Retry with the new port
        )
    ).

% Accept incoming connections
accept_dbg_connections(Alias, Socket) :-
    tcp_accept(Socket, ClientSocket, PeerAddress),
    format(atom(PeerAlias), "~w_~w_~w", [Alias, PeerAddress, ClientSocket]),
    thread_create(handle_dbg_client(ClientSocket), _, [detached(true),alias(PeerAlias)]),
    accept_dbg_connections(Alias, Socket).

% Handle an individual client connection
handle_dbg_client(ClientSocket) :-
    tcp_open_socket(ClientSocket, InStream, OutStream),
    handle_dbg_client_commands(InStream, OutStream),
    close(InStream),
    close(OutStream).

handle_dbg_client_commands(InStream, OutStream) :-
        OutStream = ErrStream,
        thread_self(Id),
        set_prolog_IO(InStream, OutStream, ErrStream),
        set_output(OutStream), set_input(InStream),
        set_stream(InStream, tty(true)),
        set_stream(OutStream, tty(true)),
        set_prolog_flag(tty_control, false),
        current_prolog_flag(encoding, Enc),
        set_stream(InStream, encoding(Enc)),
        set_stream(OutStream, encoding(Enc)),
        set_stream(ErrStream, encoding(Enc)),
        set_stream(InStream, newline(detect)),
        set_stream(OutStream, newline(dos)),
        set_stream(ErrStream, newline(dos)),

    set_stream(user_input, encoding(Enc)),
    set_stream(user_output, encoding(Enc)),
    set_stream(user_error, encoding(Enc)),
    set_stream(user_input, newline(detect)),
    set_stream(user_output, newline(dos)),
    set_stream(user_error, newline(dos)),

        format(ErrStream, 'Welcome to the MeTTaLog Telnet Server: ~w~n', [Id]),
        flush_output(ErrStream),
        % handle_dbg_client_commands_2(InStream, OutStream),
        call_cleanup(socket_repl,
                     ( close(InStream, [force(true)]),
                       close(OutStream, [force(true)]))).


socket_repl:- repl.

    % Process commands from the client
    handle_dbg_client_commands_2(InStream, OutStream) :-
       repeat,
        write(OutStream, '?- '), flush_output(OutStream),
        read_line_to_string(InStream, Command),
        ( Command == "quit" ->
           (format(OutStream, 'Goodbye!~n', []),
            flush_output(OutStream))
          ; Command == "ide." ->
              (prolog_ide(debug_monitor),
               prolog_ide(thread_monitor))
          ; Command == end_of_file ->
              true  % End connection on EOF
        ; (catch(
            ( term_string(Term, Command),
              (catch(call(Term), Error, format(OutStream, 'Error: ~q~n', [Error]))*->true;true),
              \+ \+ (numbervars(Term,0,_,[singletons(true),attvar(skip)]),format(OutStream, 'Result: ~q.~n', [Term])),
              fail
            ),
            Error,
            (format(OutStream, 'Invalid input: ~w~n', [Error]),make)
           ),
           flush_output(OutStream),
           fail)
        ).

% Start the server on port 44440
start_dbg_telnet :-
    start_dbg_telnet(44440).

% Automatically start the server at initialization, ensuring only one server is started
:- initialization(start_dbg_telnet, after_load /**/).


