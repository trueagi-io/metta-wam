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

%*********************************************************************************************
% PROGRAM FUNCTION: Provides utility predicates and data structures for handling and displaying various
% types of information, including grids, objects, and color-coded output.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Disable verbose autoload to prevent detailed loading messages during the autoloading process.
:- set_prolog_flag(verbose_autoload, false).

% Set the verbosity flag to silent to suppress informational messages during Prolog execution.
:- set_prolog_flag(verbose, silent).

% Silence messages related to file loading.
:- set_prolog_flag(verbose_load, silent).

% Ensure that the `logicmoo_utils` library is loaded, as it may contain utility predicates
% that are needed throughout the system.
:- ensure_loaded(library(logicmoo_utils)).

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).


% Prevent the dynamic predicate `user:'$exported_op'/3` from succeeding.
% Stop certain operations or exports in the user module by forcing them to fail.
:- assert((user:'$exported_op'(_,_,_):- fail)).

% Abolish the system definition of the `$exported_op/3` predicate, removing it from the
% system module if it exists.
:- abolish((system:'$exported_op'/3)).

% Similarly, prevent the dynamic predicate `system:'$exported_op'/3` from succeeding.
:- assert((system:'$exported_op'(_,_,_):- fail)).

% Conditionally check if the library `logicmoo_utils` exists, and if so, load it.
:- if(exists_source(library(logicmoo_utils))).
   % Ensure that the `logicmoo_utils` library is loaded, as it may contain utility predicates
   % needed for system functionality.
   :- ensure_loaded(library(logicmoo_utils)).
:- endif.

% Conditionally check if the library `dictoo` exists.
:- if(exists_source(library(dictoo))).
   % The following line is commented out:
   % If the `dictoo` library exists, it could be loaded here.
   % :- ensure_loaded(library(dictoo)).
:- endif.

% Declare `done_once/1` as a dynamic predicate to allow runtime modification.
:- dynamic(done_once/1).

%!  do_once(+Goal) is det.
%
%   Executes the given goal `Goal` only once.
%   If the goal has been executed before (determined by comparing with the stored goal using `=@=`),
%   the predicate succeeds without re-executing it.
%   Otherwise, it asserts the goal as "done", executes it using `once/1`, and cleans up if execution fails.
%
%   @arg Goal The goal to be executed.
do_once(G):-
    % Check if the goal has been marked as done (comparing using `=@=` to account for structural equality).
    ((done_once(GG), GG =@= G) -> true  % If it has, succeed without doing anything.
    ;  % Otherwise, assert it as done and try to execute it.
    (assert(done_once(G)),
     (once(@(G, user)) -> true  % If the execution succeeds, succeed.
     ; retract(done_once(G))))).  % If it fails, retract the "done" marker.

%!  cleanup_debug is det.
%
%   Cleans up redundant clauses in the `prolog_debug:debugging/3` predicate.
%   This predicate looks for duplicate clauses in the `debugging/3` predicate and removes
%   redundant instances, keeping only the first one found.
cleanup_debug :-
    % For all duplicate debugging clauses, erase the redundant ones.
    forall(
        ( clause(prolog_debug:debugging(A1, B, C), Body, Cl1),
          clause(prolog_debug:debugging(A2, B, C), Body, Cl2),
          A1 =@= A2, Cl1 \== Cl2),
        erase(Cl2)).

% Export the `plain_var/1` predicate to make it available outside this module.
:- export(plain_var/1).

%!  plain_var(+Var) is nondet.
%
%   True if `Var` is a plain Prolog variable, meaning it is a variable (`var/1`),
%   but not an attributed variable (`attvar/1`), and it has no `ci` attribute.
%
%   @arg Var The variable to check.
plain_var(V) :-
    notrace((var(V), \+ attvar(V), \+ get_attr(V, ci, _))).

%!  catch_nolog(+Goal) is det.
%
%   Executes the goal `Goal` without generating any debug logs.
%   If an error occurs during execution, it catches the error and ignores it (calls `nop/1` with the error).
%
%   @arg Goal The goal to execute.
catch_nolog(G) :-
    ignore(catch(notrace(G), E, once(true; nop(u_dmsg(E = G))))).

%!  catch_log(+Goal) is det.
%
%   Executes the goal `Goal` and logs any error that occurs.
%   If an error occurs, it catches it, logs the error (`u_dmsg/1`), and optionally triggers a trace (`ugtrace/2`).
%
%   @arg Goal The goal to execute.
catch_log(G) :-
    ignore(catch((G), E, ((u_dmsg(E = G), ugtrace(E, G))))).
% Alternative catch_log implementation (commented out):
% If an error occurs during the execution of `Goal`, it logs the error and then performs additional actions like debugging.
% catch_log(G) :- ignore(catch(notrace(G), E, (writeln(E = G), catch_nolog(ds)))).

%!  get_user_error(-Stream) is det.
%
%   Retrieves the stream associated with `user_error`.
%   The predicate checks for two properties of the stream: whether it has the file descriptor `2`
%   (commonly associated with standard error) or if it has the alias `user_error`.
%
%   @arg Stream The output stream for errors.
get_user_error(UE) :-
    % Check if the stream has file descriptor 2 (standard error).
    stream_property(UE, file_no(2)), !.
get_user_error(UE) :-
    % Check if the stream is aliased as `user_error`.
    stream_property(UE, alias(user_error)), !.

%!  ufmt(+Message) is det.
%
%   Attempts to format and print the given message `Message` without tracing.
%   It first checks if `fbug/1` succeeds (likely a debugging hook). If so, it prints the message using
%   `fbug/1`. Otherwise, it falls back to `ufmt0/1` to format or print the message.
%
%   @arg Message The message to format and print.
ufmt(G) :- notrace((fbug(G) -> true ; ufmt0(G))).

%!  ufmt0(+Message) is det.
%
%   Prints the message `Message` using `fmt/1`, or `writeln/1` if `fmt/1` fails.
%
%   @arg Message The message to print.
ufmt0(G) :- fmt(G) -> true ; writeln(G).

%!  u_dmsg(+Message) is det.
%
%   Sends a debug message to the `user_error` stream, optionally with ANSI formatting.
%   Depending on whether the message is a list and if certain predicates like `with_toplevel_pp/2` are available,
%   it chooses different strategies for printing the message. This predicate is useful for logging or debugging purposes.
%
%   @arg Message The message to print, which could be a list or a single item.
u_dmsg(G) :-
    % If the message is a list, apply `u_dmsg/1` to each element in the list.
    is_list(G), !, my_maplist(u_dmsg, G).
u_dmsg(M) :-
    % If `with_toplevel_pp/2` is not available, print the message directly to `user_error`.
    get_user_error(UE),\+ current_predicate(with_toplevel_pp/2),!,with_output_to(UE, ufmt(M)).
u_dmsg(M) :-
    % If `with_toplevel_pp/2` is available, print the message with ANSI formatting.
    get_user_error(UE), !, with_toplevel_pp(ansi, with_output_to(UE, ufmt(M))).
u_dmsg(M) :-
    % If the output stream is the same as the current output, use `fbug/1` for debugging output.
    % Otherwise, print to both `user_error` and the current output.
    get_user_error(UE),stream_property(UO, file_no(1)),current_output(CO), !,
    (UO == CO -> fbug(M) ; (with_toplevel_pp(ansi, with_output_to(UE, ufmt(M))), with_output_to(CO, pp(M)))).
% Fallback case: just format and print the message.
u_dmsg(G) :-
    ufmt(G), !.

% Declares that the predicate is_cgi/0 can have clauses defined across multiple files.
:- multifile(is_cgi/0).
% Declares is_cgi/0 as dynamic, allowing it to be modified during runtime.
:- dynamic(is_cgi/0).
% Declares that the predicate arc_html/0 can have clauses defined across multiple files.
:- multifile(arc_html/0).
% Declares arc_html/0 as dynamic, allowing it to be modified during runtime.
:- dynamic(arc_html/0).

%!  logicmoo_use_swish is det.
%
%   Initializes the SWISH web interface for the LogicMoo environment.
%   This predicate sets a Prolog flag `use_arc_swish` to true, loads the LogicMoo Web UI,
%   and starts both the SWISH and ClioPatria web services using `webui_start_swish_and_clio/0`.
%   It also sets up an HTTP handler to redirect requests from `/swish` to the appropriate URL.
logicmoo_use_swish :-
    % Set the Prolog flag for using arc swish.
    set_prolog_flag(use_arc_swish, true),
    % Load the LogicMoo Web UI module.
    ld_logicmoo_webui,
    % Start the SWISH and Clio web interfaces.
    call(call, webui_start_swish_and_clio),
    % Redirect `/swish` to the actual SWISH URL.
    http_handler('/swish', http_redirect(moved, '/swish/'), []).

%!  arc_user(-User) is det.
%
%   Determines the current user (arc user) based on the context in which the query is executed.
%   This predicate attempts to find the user from different sources, including the main thread,
%   pengine, CGI sessions, or the current thread.
%
%   @arg User The user identifier. This could be the thread ID, session username, or other user-related information.
arc_user(Nonvar) :-
    % If Nonvar is a non-variable, resolve it to a user variable.
    nonvar(Nonvar), !,
    arc_user(Var), !,
    Nonvar = Var.
arc_user(main) :-
    % If running on the main thread, return 'main' as the user.
    main_thread, !.
arc_user(ID) :-
    % Attempt to retrieve the user ID from a Pengine user session.
    catch((pengine:pengine_user(ID)), _, fail), !.
arc_user(ID) :-
    % Try to find the username from the HTTP session.
    catch((xlisting_web:is_cgi_stream,
           xlisting_web:find_http_session(User),
           http_session:session_data(User, username(ID))), _, fail), !.
arc_user(ID) :-
    % Check if CGI is running and retrieve the user ID from the session.
    catch((is_cgi, (xlisting_web:find_http_session(ID))), _, fail), !.
arc_user(ID) :-
    % If running in a CGI environment, return 'web_user' as the user.
    is_cgi, !, ID = web_user.
arc_user(ID) :-
    % Fallback to retrieving the current thread ID as the user ID.
    thread_self(ID).

% Declares arc_user_prop/0 as dynamic, allowing it to be modified during runtime.
:- dynamic(arc_user_prop/3).

%!  luser_setval(+N, +V) is det.
%
%   Sets a user-specific value for the current user or a specific user, associating
%   the value `V` with the key `N`. If `N` is an atom, it uses non-backtrackable storage
%   (`nb_setval/2`). This version of the predicate first retrieves the current user ID
%   using `arc_user/1`, and then calls the more specific `luser_setval/3` with the user ID.
%
%   @arg N The key (or name) for the value.
%   @arg V The value to be associated with the key.
%luser_setval(N,V):- nb_setval(N,V),!.
luser_setval(N, V) :-
    % Retrieve the current user ID using arc_user/1 and call the next clause with the ID.
    arc_user(ID),luser_setval(ID, N, V), !.

%!  luser_setval(+ID, +N, +V) is det.
%
%   Associates the value `V` with the key `N` for a specific user identified by `ID`.
%   This version checks if both `N` and `V` are sensical terms before proceeding. If not,
%   it generates a warning and skips the association.
%
%   @arg ID The user ID for whom the value is being set.
%   @arg N  The key (or name) for the value.
%   @arg V  The value to be associated with the key.
luser_setval(ID, N, V) :-
    % Check if both `N` and `V` are valid sensical terms using arc_sensical_term/1.
    % If they are not, generate a warning and skip the rest of the predicate.
    \+ (arc_sensical_term(N), arc_sensical_term(V)),warn_skip(not_arc_sensical_term(luser_setval(ID, N, V))).
luser_setval(ID, N, V) :-
    % If `N` is an atom, store the value in a non-backtrackable way using nb_setval/2.
    (atom(N) -> nb_setval(N, V); true),
    % Remove any existing property associated with the user and the key `N`.
    retractall(arc_user_prop(ID, N, _)),
    % Assert the new property with the user ID, key `N`, and value `V`.
    asserta(arc_user_prop(ID, N, V)).

%!  luser_unsetval(+N) is det.
%
%   Removes the value associated with the key `N` for the current user.
%   This predicate first attempts to delete the non-backtrackable variable `N` using `nb_delete/1`,
%   and then calls `luser_unsetval/2` to remove the association from the user properties.
%
%   @arg N The key (or name) whose value is to be removed.
luser_unsetval(N) :-
    % Ignore errors from `nb_delete/1` and delete the non-backtrackable variable associated with `N`.
    ignore(nb_delete(N)),
    % Retrieve the current user ID using `arc_user/1` and remove the key-value pair for the user.
    arc_user(ID),
    luser_unsetval(ID, N), !.

%!  luser_unsetval(+ID, +N) is det.
%
%   Removes the key-value pair for the specific user `ID` and key `N`.
%   It retracts any existing property that matches the user ID and key.
%
%   @arg ID The user ID whose property is being unset.
%   @arg N  The key (or name) whose value is to be removed.
luser_unsetval(ID, N) :-
    % Retract all properties for the given user ID and key `N`.
    retractall(arc_user_prop(ID, N, _)).

%!  set_luser_default(+N, +V) is det.
%
%   Sets a default value for the key `N` globally (across all users).
%   This calls `luser_setval/3` with `global` as the user ID to indicate a global setting.
%
%   @arg N The key for which the default value is set.
%   @arg V The default value to be associated with the key.
set_luser_default(N, V) :-
    luser_setval(global, N, V).

%!  luser_default(+N, -V) is nondet.
%
%   Retrieves the default value associated with the key `N`. If `V` is a variable,
%   it attempts to get the value using `luser_getval/2`. Otherwise, it sets the default
%   value using `set_luser_default/2`.
%
%   @arg N The key for the value.
%   @arg V The variable to unify with the value.
luser_default(N, V) :-
    % If `V` is unbound, attempt to retrieve the value.
    var(V), !,
    luser_getval(N, V).
luser_default(N, V) :-
    % If `V` is already bound, set the default value.
    set_luser_default(N, V).

%!  luser_linkval(+N, +V) is det.
%
%   Associates a value `V` with a key `N` for the current user using non-backtrackable storage.
%   If the value `V` is invalid (nonsensical), it logs a warning and skips the operation.
%
%   @arg N The key (or name) for the value.
%   @arg V The value to be associated with the key.
luser_linkval(N, V) :-
    % Retrieve the current user ID and delegate to `luser_linkval/3`.
    arc_user(ID),luser_linkval(ID, N, V), !.

%!  luser_linkval(+ID, +N, +V) is det.
%
%   Associates a value `V` with a key `N` for a specific user `ID`.
%   If the value is invalid (nonsensical), it generates a warning and skips the operation.
%
%   @arg ID The user ID for whom the value is being set.
%   @arg N  The key (or name) for the value.
%   @arg V  The value to be associated with the key.
luser_linkval(ID, N, V) :-
    % If `V` is not a variable and either `N` or `V` is nonsensical, skip the operation.
    \+ var(V), \+ (arc_sensical_term(N), arc_sensical_term(V)),
    trace,  % Enable tracing to observe execution.
    warn_skip(not_arc_sensical_term(luser_linkval(ID, N, V))).
luser_linkval(ID, N, V) :-
    % If `N` is an atom, link the value to the key non-backtrackably.
    (atom(N) -> nb_linkval(N, V); true),
    % Remove any existing property associated with the user and key.
    retractall(arc_user_prop(ID, N, _)),
    % Assert the new property for the user ID, key `N`, and value `V`.
    asserta(arc_user_prop(ID, N, V)).

%!  arc_sensical_term(+O) is nondet.
%
%   Determines whether the given term `O` is sensical (i.e., a valid term).
%   A term is considered sensical if it is nonvar, not an empty list, not an empty atom,
%   not a pair (_ - _), and not the end-of-file indicator.
%
%   @arg O The term to be checked for sensibility.
arc_sensical_term(O) :-
    nonvar(O),  % The term must be instantiated.
    O \== [],   % The term cannot be an empty list.
    O \== '',   % The term cannot be an empty atom.
    O \= (_ - _),  % The term cannot be a pair.
    O \== end_of_file.  % The term cannot be the end-of-file indicator.

%!  arc_sensical_term(+V, -O) is det.
%
%   Ensures that the term `V` is sensical. If it is, unify `O` with `V`.
%
%   @arg V The term to be checked.
%   @arg O The term unified with `V` if it is sensical.
arc_sensical_term(V, O) :-
    arc_sensical_term(V),  % Check if `V` is sensical.
    !,  % If true, cut to prevent further backtracking.
    O = V.  % Unify `O` with `V`.

%!  arc_option(+O) is nondet.
%
%   Checks whether a specific option `O` is set for the current user.
%   This is determined by retrieving the value associated with `O` and verifying
%   that it is set to `t` (true).
%
%   @arg O The option to check.
%arc_option(grid_size_only):- !,fail.
arc_option(O) :-
    % Retrieve the value for option `O` and check if it is set to 't'.
    luser_getval(O, t).

%!  if_arc_option(+O, :G) is det.
%
%   Conditionally executes the goal `G` if the option `O` is set to true for the user.
%   If the option is not set, the goal `G` is skipped.
%
%   @arg O The option to check.
%   @arg G The goal to be executed if the option `O` is true.
if_arc_option(O, G) :-
    % If `arc_option(O)` succeeds (i.e., option `O` is set), execute `G`. Otherwise, do nothing.
    (arc_option(O) -> must_det_ll(G); true).

%!  with_luser(+N, +V, :Goal) is det.
%
%   Temporarily sets a user-specific value for the key `N` to `V` while executing the goal `Goal`.
%   After the goal is executed (or if it fails), the original value of `N` is restored.
%   This is done using `setup_call_cleanup/3`, which ensures proper cleanup after the goal.
%
%   @arg N The key (or name) to temporarily set.
%   @arg V The value to set for the key `N`.
%   @arg Goal The goal to execute while the key `N` is set to `V`.
with_luser(N, V, Goal) :-
    % Retrieve the original value associated with key `N`, or use an empty list if no value exists.
    (luser_getval(N, OV); OV = []),

    % Temporarily set the key `N` to `V`, execute `Goal`, and then restore the original value.
    setup_call_cleanup(
        luser_setval(N, V),     % Setup: Set the key `N` to `V`.
        once(Goal),             % Call: Execute the goal once.
        luser_setval(N, OV)     % Cleanup: Restore the original value after the goal.
    ).

% luser_getval(N,V):- nb_current(N,VVV),arc_sensical_term(VVV,VV),!,V=VV.

%!  luser_getval(+N, -V) is nondet.
%
%   Retrieves the value associated with the key `N` for the current user. It first attempts to retrieve
%   the value using `luser_getval_0/2`. After retrieving the value, it ensures the value is sensical using
%   `arc_sensical_term/1`. If the value is valid, it unifies with `V`.
%
%   @arg N The key (or name) for the value.
%   @arg V The variable that will unify with the retrieved value.
%   This version caches the temporary value on this thread.
luser_getval(N, V) :-
    % Retrieve the value using `luser_getval_0/2`.
    luser_getval_0(N, VV),VV = V,
    % Ensure the value is sensical.
    arc_sensical_term(V),!.

%!  luser_getval_0(+N, -V) is nondet.
%
%   Dispatches the retrieval of user-specific values based on the key `N`.
%   If the key is `arc_user`, it retrieves the current user. Otherwise, it delegates
%   the retrieval to `luser_getval_1/2`.
%
%   @arg N The key (or name) for the value.
%   @arg V The variable that will unify with the retrieved value.
luser_getval_0(arc_user, V) :-
    % If the key is `arc_user`, retrieve the current user ID.
    arc_user(V).
luser_getval_0(N, V) :-
    % For other keys, delegate to `luser_getval_1/2`.
    luser_getval_1(N, V).

%!  luser_getval_1(+N, -V) is nondet.
%
%   Attempts to retrieve the value associated with the key `N` from various sources.
%   It first tries `luser_getval_2/2`, then checks `luser_getval_3/2`, and finally
%   falls back to a default value using `get_luser_default/2`. This predicate ensures
%   that it avoids conflicting values across these sources.
%
%   @arg N The key (or name) for the value.
%   @arg V The variable that will unify with the retrieved value.
luser_getval_1(N, V) :-
    % Attempt to retrieve the value using `luser_getval_2/2`.
    luser_getval_2(N, V).
luser_getval_1(N, V) :-
    % Try `luser_getval_3/2`, ensuring no conflicts with `luser_getval_2/2`.
    luser_getval_3(N, V),
    \+ (luser_getval_2(N, VV), nop(VV \= V)).
luser_getval_1(N, V) :-
    % As a last resort, use the default value from `get_luser_default/2`, ensuring consistency.
    get_luser_default(N, V),
    \+ (luser_getval_3(N, VV), nop(VV \= V)),
    \+ (luser_getval_2(N, VV), nop(VV \= V)).

%luser_getval_0(N,V):- luser_getval_2(N,V), \+ luser_getval_1(N,_).
%luser_getval_0(N,V):- luser_getval_3(N,V), \+ luser_getval_2(N,_), \+ luser_getval_1(N,_).
%luser_getval_3(N,V):- is_cgi, current_predicate(get_param_req/2),get_param_req(N,M),url_decode_term(M,V).

%!  luser_getval_2(+N, -V) is nondet.
%
%   Retrieves the value for the key `N` from the HTTP request parameters (if not on the main thread),
%   or from non-backtrackable storage. It checks that the value is sensical before unifying it with `V`.
%
%   @arg N The key (or name) for the value.
%   @arg V The variable that will unify with the retrieved value.
luser_getval_2(N, V) :-
    % If not on the main thread and `N` is an atom, attempt to retrieve the value from the HTTP request.
    \+ main_thread,atom(N),httpd_wrapper:http_current_request(Request),member(search(List), Request),
    member(N = VV, List),url_decode_term(VV, V),arc_sensical_term(V), !.
luser_getval_2(N, V) :-
    % If `N` is an atom, try to retrieve the value from non-backtrackable storage.
    atom(N),nb_current(N, ValV),arc_sensical_term(ValV, Val),Val = V.

%!  luser_getval_3(+N, -V) is nondet.
%
%   Retrieves the value for the key `N` from the current user properties or from session parameters.
%   This predicate tries to handle both CGI requests and session variables.
%
%   @arg N The key (or name) for the value.
%   @arg V The variable that will unify with the retrieved value.
luser_getval_3(N, V) :-
    % Retrieve the value from the current user properties.
    arc_user(ID),
    arc_user_prop(ID, N, V).
luser_getval_3(_, _) :-
    % If CGI mode is not active, fail.
    \+ is_cgi, !, fail.
luser_getval_3(N, V) :-
    % If not on the main thread and `N` is an atom, attempt to retrieve the value from session parameters.
    \+ main_thread,atom(N),current_predicate(get_param_sess/2),get_param_sess(N, M),url_decode_term(M, V),
    arc_sensical_term(V).
%luser_getval_3(N,V):- atom(N), nb_current(N,ValV),arc_sensical_term(ValV,Val),Val=V.

%!  get_luser_default(+N, -V) is nondet.
%
%   Retrieves a default value for the key `N` from either global user properties or Prolog flags.
%   The value is ensured to be sensical before unifying it with `V`.
%
%   @arg N The key (or name) for the default value.
%   @arg V The variable that will unify with the default value.
get_luser_default(N, V) :-
    % Retrieve the default value from global user properties.
    arc_user_prop(global, N, VV),VV = V,arc_sensical_term(V), !.
get_luser_default(N, V) :-
    % If `N` is an atom, retrieve the default value from Prolog flags.
    atom(N),current_prolog_flag(N, VV),VV = V,arc_sensical_term(V), !.

% luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.
% luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).

%!  ansi_main is det.
%
%   Succeeds if the current thread is the `main` thread and CGI mode is not active.
%   This predicate checks the current thread using `thread_self/1` and ensures that
%   CGI mode is not enabled by calling `nop(is_cgi)`. If both conditions are met,
%   it succeeds; otherwise, it fails.
ansi_main :-
    % Check if the current thread is the main thread.
    thread_self(main),
    % Check that CGI mode is not active. `nop/1` negates `is_cgi`, ensuring CGI mode is off.
    nop(is_cgi),!.

%!  main_thread is nondet.
%
%   Succeeds if the current thread is the `main` thread. This is used to identify
%   whether the system is running in the main thread.
main_thread :-
    % Check if the current thread is the main thread.
    thread_self(main),!.

%!  if_thread_main(:G) is det.
%
%   Executes the goal `G` if the current thread is the `main` thread. If the
%   system is running in any other thread, it does nothing.
%
%   This predicate checks if the current thread is the main thread using the `main_thread/0`
%   predicate, which is assumed to succeed only if the current thread is `main`. If the
%   thread is `main`, it calls the goal `G`. If not, it simply succeeds without executing `G`.
%
%   @arg G The goal to execute if the current thread is `main`.
if_thread_main(G) :-
    % Check if the current thread is the main thread.
    main_thread ->
    % If it is the main thread, execute the goal `G` using `call/1`.
    call(G);
    % If not the main thread, do nothing (i.e., succeed without executing `G`).
    true.

:- if(\+ current_predicate(fbug/1)).
%fbug(P):- format(user_error,'~N~p~n',[P]).
:- endif.

%!  substM(+T, +F, +R, -Result) is det.
%
%   Substitutes occurrences of the term `F` with `R` in the term `T`.
%   The result of the substitution is unified with `Result`.
%   This predicate handles atomic terms, lists, and compound terms.
%
%   @arg T       The input term in which substitution is to occur.
%   @arg F       The term to be substituted (the find term).
%   @arg R       The replacement term to substitute for `F`.
%   @arg Result  The term after the substitution has been applied.
substM(T, F, R, R) :-
    % If the term `T` is exactly equal to the term `F`, unify the result with `R`.
    T == F, !.
substM(T, _, _, R) :-
    % If `T` is not compound (i.e., atomic or a simple term), unify the result with `T` as no substitution is needed.
    \+ compound(T),!,R = T.
substM([H1|T1], F, R, [H2|T2]) :-
    % If `T` is a list, perform substitution on both the head `H1` and the tail `T1`.
    !,substM(H1, F, R, H2),  % Substitute in the head.
    substM(T1, F, R, T2).  % Substitute in the tail.
substM(C1, F, R, C2) :-
    % If `T` is a compound term, decompose it into its functor and arguments.
    C1 =.. [Fn|A1],
    % Perform substitution on the list of arguments.
    substM_l(A1, F, R, A2),
    % Recompose the term with the substituted arguments.
    !,C2 =.. [Fn|A2].

%!  substM_l(+A1, +F, +R, -A2) is det.
%
%   Substitutes occurrences of the term `F` with `R` in the list of arguments `A1`.
%   The result is unified with `A2`. This predicate is used to process lists of arguments
%   for compound terms.
%
%   @arg A1 The list of arguments in which substitution is to occur.
%   @arg F  The term to be substituted (the find term).
%   @arg R  The replacement term to substitute for `F`.
%   @arg A2 The list of arguments after substitution.
% Base case: an empty list remains empty after substitution.
substM_l([], _, _, []).
substM_l([H1|T1], F, R, [H2|T2]) :-
    % Perform substitution on the head of the list.
    substM(H1, F, R, H2),
    % Recursively process the tail of the list.
    substM_l(T1, F, R, T2).

%!  pp_m(+Cl) is det.
%
%   Pretty-prints or outputs the clause `Cl` using `write_src/1`.
%   This predicate uses a cut (`!`) to ensure no further backtracking.
%
%   @arg Cl The clause to be printed or outputted.
pp_m(Cl) :-
    % Call `write_src/1` to output the clause `Cl`.
    write_src(Cl),!.

%!  pp_m(+C, +Cl) is det.
%
%   Pretty-prints or outputs the clause `Cl` with some coloring or message handling.
%   This version uses `color_g_mesg/2` to print the clause `Cl` with a specific color `C`.
%
%   @arg C  The color or message type used for formatting.
%   @arg Cl The clause to be printed or outputted.
pp_m(C, Cl) :-
    % Call `color_g_mesg/2` to output the clause `Cl` with the color or formatting `C`.
    color_g_mesg(C, write_src(Cl)),!.

% The following line is commented out and appears to be an alternative approach for pretty-printing clauses.
% This code would format the clause `Cl`, number variables (with `numbervars/4`), and print it using
% `print_tree_with_final/2` while suppressing tracing.
% notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).

%!  pp_q(+Cl) is det.
%
%   Pretty-prints or outputs the clause `Cl` with variable numbering and tree formatting.
%   This predicate uses `numbervars/4` to number the variables in `Cl`, and then calls
%   `print_tree_with_final/2` to print the clause. Tracing is suppressed using `notrace/1`.
%
%   @arg Cl The clause to be printed or outputted.
pp_q(Cl):-
  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true),attvar(skip)]), print_tree_with_final(Cl,"."))))))).

%!  ncatch(:G, +E, :F) is det.
%
%   This predicate is a wrapper around the built-in `catch/3` predicate.
%   It attempts to execute the goal `G`. If an exception `E` is raised,
%   it handles the exception by executing the fallback goal `F`.
%
%   @arg G The goal to be executed.
%   @arg E The exception to catch.
%   @arg F The fallback goal to be executed if an exception `E` is caught.
ncatch(G, E, F) :-
    % Catch exceptions that occur while executing `G` and handle them with `F`.
    catch(G, E, F).

%!  mcatch(:G, +E, :F) is det.
%
%   This predicate is another wrapper around `catch/3` that functions
%   identically to `ncatch/3`. It attempts to execute the goal `G` and handles
%   any exception `E` by executing the fallback goal `F`.
%
%   @arg G The goal to be executed.
%   @arg E The exception to catch.
%   @arg F The fallback goal to be executed if an exception `E` is caught.
mcatch(G, E, F) :-
    % Catch exceptions that occur while executing `G` and handle them with `F`.
    catch(G, E, F).

%mcatch(G,E,F):- catch(G,E,(fbug(G=E),catch(bt,_,fail),fbug(G=E),ignore(call(F)),throw(E))).
%ncatch(G,E,F):- catch(G,E,(fbug(G=E),catch(bt,_,fail),fbug(G=E),call(G))).
%ncatch(G,E,(F)).

% Conditionally define the predicate `if_t/2` only if it is not already defined.
:- if(\+ current_predicate(if_t/2)).

% Declare `if_t/2` as a meta-predicate, indicating that both arguments are goals (0-arity).
:- meta_predicate(if_t(0, 0)).

%!  if_t(:IF, :THEN) is det.
%
%   Executes the goal `THEN` if the goal `IF` succeeds. It ensures that both
%   goals are called in a safe manner using `call/1` and `ignore/1`. The `ignore/1`
%   ensures that failure of the `THEN` goal does not propagate back to `IF`.
%
%   @arg IF   The condition (goal) to test.
%   @arg THEN The goal to execute if `IF` succeeds.
if_t(IF, THEN) :-
    % Safely call `IF`, and if it succeeds, execute `THEN`.
    call(call, ignore((((IF, THEN))))).

:- endif.

% Conditionally define the predicate `must_ll/1` only if it is not already defined.
:- if(\+ current_predicate(must_ll/1)).

% Declare `must_ll/1` as a meta-predicate, indicating that the argument is a goal (0-arity).
:- meta_predicate(must_ll(0)).

%!  must_ll(:G) is det.
%
%   Ensures that the goal `G` succeeds at least once. If `G` does not succeed,
%   an exception is thrown. The predicate uses `md/2` for execution (presumably
%   some defined mechanism for executing goals), and if `G` does not succeed,
%   it throws a `not_at_least_once/1` exception.
%
%   @arg G The goal that must succeed at least once.
must_ll(G) :-
    % Attempt to execute the goal `G`. If it succeeds, succeed; otherwise, throw an exception.
    md(call, G) *-> true ; throw(not_at_least_once(G)).

:- endif.


% Conditionally define the predicate `at_least_once/1` only if it is not already defined.
:- if(\+ current_predicate(at_least_once/1)).

% Declare `at_least_once/1` as a meta-predicate, indicating that the argument is a goal (0-arity).
:- meta_predicate(at_least_once(0)).

%!  at_least_once(:G) is det.
%
%   Ensures that the goal `G` succeeds at least once. If `G` does not succeed,
%   an exception is thrown. This provides a simple way to enforce that the goal
%   `G` must succeed at least once during its execution.
%
%   @arg G The goal that must succeed at least once.
at_least_once(G) :-
    % Attempt to execute the goal `G`. If it succeeds, succeed; otherwise, throw an exception.
    call(G) *-> true ; throw(not_at_least_once(G)).

:- endif.

%!  wraps_each(+Outer, +Inner) is det.
%
%   A placeholder or helper predicate that likely specifies that the predicate `Outer`
%   (e.g., `must_det_ll`) wraps around the predicate `Inner` (e.g., `once`). This indicates
%   a relationship between the two predicates, where `Outer` enforces certain behavior (e.g.,
%   determinism) on `Inner`.
%
%   @arg Outer The outer predicate (e.g., `must_det_ll`).
%   @arg Inner The inner predicate (e.g., `once`).
%wraps_each(must_ll,call).
wraps_each(must_det_ll, once).

%!  md_like(+MD) is nondet.
%
%   Checks if the predicate `MD` is defined as a wrapper by checking if `wraps_each/2`
%   contains an entry for `MD`. This acts as a filter or condition to see if `MD` behaves
%   like a "wrapper" predicate.
%
%   @arg MD The predicate to check if it wraps another predicate.
md_like(MD) :-
    wraps_each(MD, _).

%!  remove_must_det(+MD) is nondet.
%
%   A placeholder predicate intended to prevent the use of `MD` (possibly removing
%   or disabling behavior associated with `must_det_ll`). Currently, this version
%   unconditionally fails using the cut-fail combination (`!`, `fail`), preventing any action.
%
%   @arg MD The predicate for which `must_det_ll` should be removed.
remove_must_det(_) :- !,fail.
% remove_must_det(MD):- !.
% remove_must_det(MD):- nb_current(remove_must_det(MD),TF),!,TF==true.
% remove_must_det(MD):- \+ false.

%!  remove_mds(+MD, +G, -GO) is det.
%
%   This predicate removes instances of the predicate `MD` (e.g., `must_det_ll`) from the term `G`
%   and produces a modified term `GO`. It does so by recursively traversing the term `G`, looking for
%   sub-terms that match the structure `MD(GGG)`, and then replacing them with the goal `GGG`.
%
%   @arg MD The predicate to be removed (e.g., `must_det_ll`).
%   @arg G  The term from which instances of `MD` are removed.
%   @arg GO The resulting term after removal of `MD`.
%remove_mds(MD,G,GGG):- compound(G), G = must_det_ll(GG),!,expand_goal(GG,GGG),!.
%remove_mds(MD,G,GGG):- compound(G), G = must_det_l(GG),!,expand_goal(GG,GGG),!.
remove_mds(MD, GG, GO) :-
    % Traverse the term `GG` and check for sub-terms that match the pattern `MD(...)`.
    sub_term_safely(G, GG),
    % Check if the term is compound and matches the structure `MD(...)`.
    compound(G),
    compound_name_arg(G, MD, GGGG),
    % Substitute the matched term `G` with the inner goal `GGGG`.
    subst001(GG, G, GGGG, GGG),
    % Recursively apply the removal process.
    remove_mds(MD, GGG, GO).
% Base case: if no sub-terms match, return the term unchanged.
remove_mds(_, GG, GG).
%remove_mds(MD,G,GG):- compound(G), G = ..[MD,AA], compound(G),removed_term(G,GO),expand_goal(GO,GG).

%!  never_rrtrace is det.
%
%   Prevents tracing via `rrtrace` if certain conditions are met. This is used to
%   suppress debugging output in specific situations, such as when `cant_rrtrace` is set
%   to `t` or when the system is running in CGI mode.
%
%   The `notrace/0` predicate is used to disable tracing.
never_rrtrace:- \+ extreme_tracing,!.
never_rrtrace :-
    % If `cant_rrtrace` is currently set to `t`, disable tracing using `notrace`.
    nb_current(cant_rrtrace, t),!,notrace.
never_rrtrace :-
    % If the system is running in CGI mode, disable tracing using `notrace`.
    is_cgi,notrace.

% recolor(_,_):- ibreak.

%!  itrace is det.
%
%   Enables tracing conditionally for the main thread. This predicate checks if the
%   current thread is the main thread, and if so, it enables tracing using `trace/0`.
%itrace:- !.
%itrace:- \+ current_prolog_flag(debug,true),!.
itrace :-
    % If running in the main thread, enable tracing.
    if_thread_main(trace),!.

%!  ibreak is det.
%
%   Similar to `itrace/0`, but this predicate enables both tracing and sets a breakpoint
%   using `break/0` for the main thread. It is intended for debugging purposes where
%   both tracing and interaction at a breakpoint are required.
ibreak :-
    % If running in the main thread, enable tracing and invoke a breakpoint.
    if_thread_main(((trace, break))).

%recolor(_,_):- ibreak.

%!  tc_arg(+N, +C, -E) is det.
%
%   This predicate retrieves the N-th argument of the compound term C and unifies it with E.
%   If an error occurs during the `arg/3` call, it catches the error and attempts to handle it
%   with additional debugging steps, including backtracking (`bt`), logging the failure with
%   `fbug/1`, and tracing the execution if tracing is enabled.
%
%   @arg N  The position of the argument to retrieve from the compound term C.
%   @arg C  The compound term from which to retrieve the argument.
%   @arg E  Unifies with the N-th argument of C.
%%
%   @example
%   ?- tc_arg(1, f(a, b, c), Arg).
%   Arg = a.
%
%tc_arg(N,C,E):- compound(C),!,arg(N,C,E).
tc_arg(N,C,E):- catch(arg(N,C,E), Err,
      /* unrepress_output */ (
          (bt,fbug(tc_arg(N,C,E)=Err),((tracing -> true ; trace), break, arg(N,C,E))))).

%!  compound_name_arg(?G, +MD, ?Goal) is det.
%
%   This predicate handles the construction and deconstruction of compound terms
%   based on the name and arguments. It is used to either construct a compound
%   term from a functor and its argument or to decompose a compound term into
%   its functor and first argument.
%
%   @arg G    The compound term (either input or output). If G is unbound,
%             it will be constructed using `MD` and `Goal`.
%   @arg MD   The name of the functor for the compound term.
%   @arg Goal The argument of the compound term.
%
%   The predicate works in two modes:
%   1. If `G` is a variable, it constructs a compound term with functor `MD`
%      and argument `Goal` (using the `=../2` operator).
%   2. If `G` is a compound, it decomposes `G` into its functor `MD` and
%      its argument list `[Goal]`.
%
%   @example
%   % Constructing a compound term:
%   ?- compound_name_arg(G, foo, bar).
%   G = foo(bar).
%
%   % Decomposing a compound term:
%   ?- compound_name_arg(foo(bar), MD, Goal).
%   MD = foo,
%   Goal = bar.
%
compound_name_arg(G, MD, Goal) :-
    % If G is unbound (a variable), proceed with constructing the term.
    var(G), !,
    % Ensure that MD is an atom (functor name).
    atom(MD),
    % Use `=../2` to construct G as a compound with functor MD and argument Goal.
    G =.. [MD, Goal].
compound_name_arg(G, MD, Goal) :-
    % If G is a compound, decompose it into functor and arguments.
    compound(G), !,
    % Decompose G into functor MD and argument list [Goal].
    compound_name_arguments(G, MD, [Goal]).

% Allows the predicate `user:message_hook/3` to be defined in multiple files.
:- multifile(user:message_hook/3).

% Declares that `user:message_hook/3` can be modified during runtime, allowing new clauses to be added or retracted.
:- dynamic(user:message_hook/3).

%!  user:message_hook(+Term, +Kind, +Lines) is nondet.
%
%   This hook is used to intercept messages in the SWI-Prolog system. It is designed to handle
%   error messages by logging them using `fbug/1`. If the message kind is `error`, it will attempt
%   to log the message details, but ultimately fails, allowing other message handling logic to proceed.
%
%   @arg Term  The term representing the message content.
%   @arg Kind  The kind of message, such as `error`, `warning`, etc.
%   @arg Lines The message lines, typically used to format the output.
%
%   This predicate logs messages of kind `error` and then fails, meaning it does not handle the message
%   further. The `fail/0` at the beginning ensures that no other action is taken before logging.
%
%   @example
%   ?- user:message_hook(my_error_message, error, ["Some error occurred."]).
%   % This would log the error using fbug/1 and then fail.
%
%user:message_hook(Term, Kind, Lines):- error==Kind, itrace,fbug(user:message_hook(Term, Kind, Lines)),trace,fail.

user:message_hook(debug_no_topic(metta(_)), warning, _):-!.
user:message_hook(Term, Kind, Lines) :-
    % Always fail initially to ensure no action is taken before processing.
    fail,
    % Check if the message kind is an error.
    error == Kind,
    % Log the error message using fbug/1.
    fbug(message_hook(Term, Kind, Lines)),
    % Fail again to ensure further message handling occurs.
    fail.

% Define several predicates as meta_predicate.
% A meta-predicate in Prolog is a predicate that takes other predicates (or "goals") as arguments.
% This allows you to pass predicates to be executed or manipulated within other predicates.
% The purpose of a meta-predicate declaration is to tell Prolog how to treat those arguments.
:- meta_predicate(must_det_ll(0)).
:- meta_predicate(must_det_ll1(1,0)).
:- meta_predicate(md_failed(1,0)).
:- meta_predicate(must_not_error(0)).
%:- meta_predicate(must_det_l(0)).
%:- no_xdbg_flags.
:- meta_predicate(wno_must(0)).

%!  wno_must(:G) is det.
%
%   Temporarily sets the flags `no_must_det_ll` and `cant_rrtrace` to true
%   while executing the given goal `G`. This is used to ensure that the goal
%   runs without certain restrictions, such as deterministic checks or tracing.
%
%   @arg G The goal to be executed with modified environment settings.
%
wno_must(G) :-
    % Temporarily set the `no_must_det_ll` flag to true.
    locally(b_setval(no_must_det_ll, t),
        % Temporarily set the `cant_rrtrace` flag to true.
        locally(b_setval(cant_rrtrace, t),
            % Execute the goal G within the modified environment.
            call(G))).

:- thread_initialization(nb_setval(no_must_det_ll,[])).
:- thread_initialization(nb_setval(cant_rrtrace,[])).



%!  md_maplist(:MD, :P1, +List) is det.
%
%   Applies a predicate `P1` to each element in `List` using the modifier `MD`.
%   This version of `md_maplist/3` handles single-argument predicates.
%
%   @arg MD   Modifier goal to be applied before calling `P1`.
%   @arg P1   Predicate to be applied to each element of `List`.
%   @arg List Input list.
%
md_maplist(_MD, _, []) :- !.
md_maplist(MD, P1, [H|T]) :-
    % Call the modifier MD with the predicate P1 applied to H.
    call(MD, call(P1, H)),
    % Recursively apply MD and P1 to the tail of the list.
    md_maplist(MD, P1, T).

%!  md_maplist(:MD, :P2, +ListA, -ListB) is det.
%
%   Applies a two-argument predicate `P2` to corresponding elements of `ListA` and `ListB`
%   using the modifier `MD`. Each element from `ListA` is passed with the corresponding
%   element from `ListB` to `P2`.
%
%   @arg MD     Modifier goal to be applied before calling `P2`.
%   @arg P2     Two-argument predicate to be applied to each pair of elements.
%   @arg ListA  First input list.
%   @arg ListB  Second input list (output list).
%
md_maplist(_MD, _, [], []) :- !.
md_maplist(MD, P2, [HA|TA], [HB|TB]) :-
    % Call the modifier MD with the predicate P2 applied to HA and HB.
    call(MD, call(P2, HA, HB)),
    % Recursively apply MD and P2 to the tail of both lists.
    md_maplist(MD, P2, TA, TB).

%!  md_maplist(:MD, :P3, +ListA, -ListB, -ListC) is det.
%
%   Applies a three-argument predicate `P3` to corresponding elements of `ListA`, `ListB`,
%   and `ListC` using the modifier `MD`. Each element from `ListA`, `ListB`, and `ListC` is
%   passed to `P3`.
%
%   @arg MD     Modifier goal to be applied before calling `P3`.
%   @arg P3     Three-argument predicate to be applied to each triple of elements.
%   @arg ListA  First input list.
%   @arg ListB  Second input list (output list).
%   @arg ListC  Third input list (output list).
%
md_maplist(_MD, _, [], [], []) :- !.
md_maplist(MD, P3, [HA|TA], [HB|TB], [HC|TC]) :-
    % Call the modifier MD with the predicate P3 applied to HA, HB, and HC.
    call(MD, call(P3, HA, HB, HC)),
    % Recursively apply MD and P3 to the tails of the lists.
    md_maplist(MD, P3, TA, TB, TC).

%:- if( \+ current_predicate(must_det_ll/1)).

%!  must_det_ll(:Goal) is det.
%
%   Ensures that the given `Goal` is executed deterministically (i.e., it succeeds with exactly
%   one solution). If tracing is enabled, it executes the goal in a traced environment. Otherwise,
%   it delegates to `md/2` for non-traced deterministic execution.
%
%   @arg Goal The goal to execute deterministically.
%
%must_det_ll(G):- !, once((/*notrace*/ (G)*->true;md_failed(P1,G))).
must_det_ll(X) :-
    % If tracing is enabled, execute the goal `X` using `once/1` to ensure determinism.
    tracing, !,
    once(X).
must_det_ll(X) :-
    % If tracing is not enabled, delegate the execution to `md/2`, ensuring that `X` is executed once.
    md(once, X).

%:- endif.

%!  md(:P1, :G) is det.
%
%   Executes the goal `G` with the given modifier `P1`. This predicate applies various
%   context-sensitive behaviors, such as tracing, error handling, determinism, and goal wrapping,
%   depending on the conditions. It ensures proper execution of goals in different environments.
%
%   The modifier `P1` controls how the goal is called (e.g., ensuring determinism or handling errors).
%   The behavior of `md/2` changes depending on the structure of the goal `G`, such as conjunctions,
%   disjunctions, cuts, or list-based goals like `maplist/2`.
%
%   @arg P1  The modifier or context in which the goal `G` is executed.
%   @arg G   The goal to be executed, possibly wrapped with `P1` and evaluated under various conditions.
%
% If tracing is enabled, execute the goal G with modifier P1.
md(P1, G):- tracing, !, call(P1, G).
% Remove the deterministic checks (must_det) and apply the wrapped P1 to the goal G.
md(P1, G):- remove_must_det(MD), wraps_each(MD, P1), !, call(G).
% If rrtrace is never allowed, directly call the goal G with modifier P1.
md(P1, G):- never_rrtrace, !, call(P1, G).
% If in an HTML environment, call the goal G and ignore any errors.
md(P1, G):- (arc_html), !, ignore((call(P1, G))), !.
% If X is a trace call, trace the goal and execute it with P1.
md(P1, (X, Goal)):- is_trace_call(X), !, call((itrace, call(P1, Goal))).
%md(P1,X):- !,must_not_error(X).
% If X is a trace call, start tracing.
md(_, X):- is_trace_call(X), !, itrace.
% If no deterministic check is allowed, call X directly with P1.
md(P1, X):- nb_current(no_must_det_ll, t), !, call(P1, X).
% Throw an error if X is not callable.
md(P1, X):- \+ callable(X), !, throw(md_not_callable(P1, X)).
% Handle the if-then-else construct with deterministic branching.
md(P1, (A *-> X ; Y)):- !, (must_not_error(A) *-> md(P1, X); md(P1, Y)).
% Handle the standard if-then-else construct with error handling.
md(P1, (A -> X ; Y)):- !, (must_not_error(A) -> md(P1, X); md(P1, Y)).
% Handle cuts by executing the first goal and applying the cut.
md(P1,(X,Cut)):-(Cut==(!)),md(P1,X),!.
% Handle maplist/2 for applying P1 to each element in the list.
md(MD, maplist(P1, List)):- !, call(MD, md_maplist(MD, P1, List)).
% Handle maplist/3 for applying P2 to corresponding elements of ListA and ListB.
md(MD, maplist(P2, ListA, ListB)):- !, call(MD, md_maplist(MD, P2, ListA, ListB)).
% Handle maplist/4 for applying P3 to corresponding elements of ListA, ListB, and ListC.
md(MD, maplist(P3, ListA, ListB, ListC)):- !, call(MD, md_maplist(MD, P3, ListA, ListB, ListC)).
% Handle conjunction with cuts by executing X, then Y.
md(P1,(X,Cut,Y)):-(Cut==(!)),!,(md(P1,X),!,md(P1,Y)).
% Handle simple conjunction of two goals.
md(P1, (X, Y)):- !, (md(P1, X), md(P1, Y)).
% Ensure deterministic execution of the goal X using must_det_ll/1.
md(_, must_det_ll(X)):- !, must_det_ll(X).
% Handle grid_call/3, ensuring that the P2 goal operates on inputs and outputs deterministically.
md(_, grid_call(P2, I, O)):- !, must_grid_call(P2, I, O).
%md(P1,call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,(X,Y,Z)):- !, (md(P1,X)->md(P1,Y)->md(P1,Z)).
%md(P1,(X,Y)):- !, (md(P1,X)->md(P1,Y)).
%md(P1,if_t(X,Y)):- !, if_t(must_not_error(X),md(P1,Y)).
% Handle forall/2 by ensuring both the generator and test goals handle errors properly.
md(P1, forall(X, Y)):- !, md(P1, forall(must_not_error(X), must_not_error(Y))).
% Handle nested negations by converting them to a forall construct with error handling.
md(P1, \+ (X, \+ Y)):- !, md(P1, forall(must_not_error(X), must_not_error(Y))).
% Handle disjunction (or) of two goals, ensuring errors are caught and handled.
md(P1, (X; Y)):- !, ((must_not_error(X); must_not_error(Y)) -> true; md_failed(P1, X; Y)).
% Handle negation of a goal, ensuring proper error handling.
md(P1, \+ (X)):- !, (\+ must_not_error(X) -> true ; md_failed(P1, \+ X)).
%md(P1,(M:Y)):- nonvar(M), !, M:md(P1,Y).
% Attempt deterministic execution of the goal X, catching failures and exceptions.
md(P1, X):-
  ncatch(must_det_ll1(P1, X),                 % Try to execute X deterministically with P1.
  md_failed(P1, G, N),                        % Catch any failure or exception.
   % Bubble up the failure, retrying if possible, or aborting with a trace if retries are exhausted.
  ((M is N - 1, M > 0) -> throw(md_failed(P1, G, M)) ;
  (ugtrace(md_failed(P1, G, M), X), throw('$aborted')))), !.

%!  must_det_ll1(:P1, :X) is det.
%
%   Executes the goal `X` with modifier `P1`, ensuring that the goal is deterministic. This
%   predicate supports tracing, error handling, and conditional execution depending on the
%   structure of `X`. It also handles specific cases like `once/1` and adds tracing information
%   around the execution of the goal if tracing is enabled.
%
%   @arg P1  The modifier that wraps the execution of the goal `X`.
%   @arg X   The goal to execute deterministically, optionally with additional wrapping and tracing.
%
%   This predicate behaves differently in the following cases:
%   - If tracing is enabled, it executes the goal in a traced environment and ensures no errors occur.
%   - If the goal is wrapped in `once/1`, it ensures the goal is executed deterministically.
%   - Otherwise, it extracts the module and functor information of the goal and traces its execution.
%
% Execute the goal X with modifier P1 in a tracing environment, ensuring no errors occur.
must_det_ll1(P1, X):- tracing, !, must_not_error(call(P1, X)), !.
% Handle once/1 by executing the goal A deterministically with the modifier P1.
must_det_ll1(P1, once(A)):- !, once(md(P1, A)).
% Extract module and functor details from the goal X, execute it deterministically, and trace the call.
must_det_ll1(P1, X):-
  strip_module(X, M, P),                        % Extract the module M and functor P from goal X.
  functor(P, F, A),                             % Get the functor name F and arity A of the goal.
  setup_call_cleanup(
    nop(trace(M:F/A, +fail)),                   % Start tracing the goal M:F/A, with +fail indicating tracing activation.
    (must_not_error(call(P1, X)) *-> true;      % Try executing the goal with P1, handle errors, and check for failure.
     md_failed(P1, X)),                         % If the goal fails, log the failure using md_failed/2.
    nop(trace(M:F/A, -fail))                    % Stop tracing after execution, with -fail indicating tracing deactivation.
  ), !.

%!  must_not_error(:G) is det.
%
%   Executes the goal `G` while ensuring that no uncaught errors occur during execution.
%   Depending on the environment (e.g., tracing, CGI, or standard execution), it applies
%   different strategies for handling errors and tracing the execution. This predicate is
%   designed to ensure that errors are properly caught and logged, preventing the goal from
%   crashing or leaving exceptions unhandled.
%
%   @arg G  The goal to be executed with error handling and optional tracing.
%
%   This predicate adapts its behavior based on the context:
%   - If tracing is enabled or `never_rrtrace` is true, the goal is executed normally.
%   - In CGI environments, it catches errors and logs them using `u_dmsg/1`.
%   - If errors occur, they are caught and logged, and further handling is provided based on the context.
%
% If tracing or never_rrtrace is enabled, execute the goal G without additional error handling.
%must_not_error(G):- must(once(G)).
must_not_error(G):- (tracing; never_rrtrace), !, call(G).
% In a CGI environment, catch and log any errors that occur during execution.
must_not_error(G):- notrace(is_cgi), !, ncatch((G), E, ((u_dmsg(E = G)))).
%must_not_error(X):- is_guitracer,!, call(X).
%must_not_error(G):- !, call(G).
% Catch and log errors, using fbug/1 and ugtrace/2 to log the error and trace its execution.
must_not_error(X):- !, ncatch(X, E, (fbug(E = X), ugtrace(error(E), X))).
% In case of an error, rethrow or log the error, potentially enabling tracing and re-executing the goal.
must_not_error(X):- ncatch(X, E, (rethrow_abort(E); (writeq(E = X), pp(etrace = X), trace, rrtrace(visible_rtrace([-all, +exception]), X)))).

%!  always_rethrow(+E) is det.
%
%   Throws specific exceptions or errors based on the input `E`. If the error term `E` matches
%   certain predefined patterns, it is thrown immediately. In the case of certain special conditions
%   (like `never_rrtrace`), the error is thrown unconditionally.
%
%   @arg E  The exception or error term to be potentially rethrown.
%
%   This predicate defines specific error terms that will be rethrown immediately. For other terms,
%   it checks if the `never_rrtrace` flag is set and rethrows the error if necessary.
%
always_rethrow('$aborted').
always_rethrow(md_failed(_,_,_)).
always_rethrow(return(_)).
always_rethrow(metta_throw(_)).
always_rethrow(metta_return(_)).
always_rethrow(give_up(_)).
always_rethrow(time_limit_exceeded(_)).
always_rethrow(time_limit_exceeded).
always_rethrow(depth_limit_exceeded).
always_rethrow(restart_reading).
%always_rethrow(E):- never_rrtrace,!,throw(E).
%always_rethrow(_).

%!  catch_non_abort(:Goal) is det.
%
%   Executes the given `Goal` and catches any exceptions that occur, except for
%   those that should result in an abort. If an exception is caught, it is handled
%   using `rethrow_abort/1` to ensure that only non-abort errors are handled.
%
%   @arg Goal  The goal to be executed, which may raise an exception.
%
%   This predicate attempts to run the `Goal`, and if an exception is thrown, it catches it
%   and passes it to `rethrow_abort/1` to handle exceptions that require an abort.
%
% Catch any exception that occurs during the execution of Goal, but rethrow abort-related exceptions.
%catch_non_abort(Goal):- cant_rrtrace(Goal).
catch_non_abort(Goal):- catch(cant_rrtrace(Goal), E, rethrow_abort(E)), !.

%!  rethrow_abort(+E) is det.
%
%   Handles specific exceptions by either logging them or rethrowing them. The predicate
%   is designed to log abort-related exceptions like `$aborted` and provide detailed
%   error output for others. In certain cases, it can suppress or handle specific exceptions
%   based on conditions like debugging (`ds`).
%
%   @arg E  The exception or error term to be handled.
%
%   This predicate:
%   - Logs the exception `E` to the `user_error` stream for most cases.
%   - Rethrows the `$aborted` exception and performs additional actions like logging timeouts.
%
% Log the exception E by formatting and outputting to user_error, then fail to indicate error handling.
rethrow_abort(E):- format(user_error,'~N~q~n',[catch_non_abort_or_abort(E)]), fail.
%rethrow_abort(time_limit_exceeded):-!.
% Rethrow the `$aborted` exception and perform additional steps, including logging and handling timeouts.
rethrow_abort('$aborted'):- !, throw('$aborted'), !, forall(between(1,700,_), sleep(0.01)), writeln(timeout), !, fail.
% If debugging (ds) is enabled, log the exception and suppress further handling.
rethrow_abort(E):- ds, !, format(user_error,'~N~q~n',[catch_non_abort(E)]), !.

%!  cant_rrtrace(:Goal) is det.
%
%   Executes the given `Goal` while ensuring that "rrtrace" (reversible tracing) is not allowed.
%   If the `never_rrtrace` condition is true, it directly executes the goal without further setup.
%   Otherwise, it sets up a restricted tracing environment, runs the goal, and then cleans up the tracing setup.
%
%   @arg Goal  The goal to execute, ensuring that reversible tracing is not allowed.
%
%   This predicate works by:
%   - Directly calling `Goal` if `never_rrtrace` is enabled.
%   - Using `setup_call_cleanup/3` to disable and re-enable tracing around the execution of `Goal`.
%
% If never_rrtrace is enabled, execute the goal directly without any special setup.
cant_rrtrace(Goal):- never_rrtrace, !, call(Goal).
% Otherwise, set up a restricted tracing environment for the duration of the goal execution.
cant_rrtrace(Goal):- setup_call_cleanup(cant_rrtrace, Goal, can_rrtrace).

%!  main_debug is det.
%
%   Checks if the current thread is the main thread and whether the Prolog system
%   is running in debug mode. This predicate is used to ensure that the system is
%   in a debugging state within the main thread.
%
%   This predicate succeeds if both the conditions are met: the current thread is the
%   main thread and the Prolog flag `debug` is set to `true`.
%
%   @example
%   ?- main_debug.
%   % Succeeds if the current Prolog environment is running in debug mode.
%
% Succeeds if the current thread is the main thread and Prolog is in debug mode.
main_debug:- main_thread, current_prolog_flag(debug, true).

%!  cant_rrtrace is det.
%
%   Sets the non-backtrackable flag `cant_rrtrace` to true. This predicate is used
%   to signal that reversible tracing (`rrtrace`) should not be allowed during the
%   execution of subsequent goals.
%
%   @example
%   ?- cant_rrtrace.
%   % Sets the flag `cant_rrtrace` to true.
%
% Set the `cant_rrtrace` flag to true.
cant_rrtrace:- nb_setval(cant_rrtrace, t).

%!  can_rrtrace is det.
%
%   Sets the non-backtrackable flag `cant_rrtrace` to false. This predicate is used
%   to allow reversible tracing (`rrtrace`) during the execution of subsequent goals.
%
%   @example
%   ?- can_rrtrace.
%   % Sets the flag `cant_rrtrace` to false.
%
% Set the `cant_rrtrace` flag to false.
can_rrtrace:- nb_setval(cant_rrtrace, f).

%!  md_failed(:P1, :X) is det.
%
%   Handles the failure of a goal `X` wrapped by `P1`, logging or tracing the failure depending
%   on the environment (tracing, CGI, etc.). It can perform different actions such as logging,
%   invoking tracing, or rethrowing the failure as an exception. The behavior of this predicate
%   is highly context-sensitive, adapting to whether debugging, tracing, or testing environments are enabled.
%
%   @arg P1  The wrapper or context in which the goal `X` was executed.
%   @arg X   The goal that has failed.
%
%md_failed(P1,X):- predicate_property(X,number_of_clauses(1)),clause(X,(A,B,C,Body)), (B\==!),!,must_det_ll(A),must_det_ll((B,C,Body)).
% Write the failure to source output and fail, without any additional action.
md_failed(P1, X):- notrace((write_src_uo(failed(P1, X)))),
   on_mettalog_error(failed(P1, X)),
   notrace(fail).
% If tracing is enabled, trace the failure of the goal with visible trace options.
md_failed(P1, X):- tracing, visible_rtrace([-all, +fail, +call, +exception], call(P1, X)).
% If tracing is disabled, trace the failure with visible trace options but avoid full tracing.
md_failed(P1, X):- \+ tracing, !, visible_rtrace([-all, +fail, +exit, +call, +exception], call(P1, X)).
% If running in a CGI environment and not in main debug, log the failure as HTML and fail.
md_failed(P1, G):- is_cgi, \+ main_debug, !, u_dmsg(arc_html(md_failed(P1, G))), fail.
% If testing is enabled, log the failure and abort the goal with a specific failure message.
md_failed(_P1, G):- option_value(testing, true), !, T = 'FAILEDDDDDDDDDDDDDDDDDDDDDDDDDD!!!!!!!!!!!!!'(G),
    write_src_uo(T), give_up(T, G).
% If reversible tracing is disabled, log the failure and throw the failure exception.
md_failed(P1, G):- never_rrtrace, !, notrace, /*notrace*/(u_dmsg(md_failed(P1, G))), !, throw(md_failed(P1, G, 2)).
% md_failed(P1, G):- tracing, call(P1, G).
% If reversible tracing is disabled, fail silently.
md_failed(_, _):- never_rrtrace, !, fail.
% If in GUI tracer mode, log the failure, enable tracing, and call the goal again.
md_failed(P1, X):- notrace, is_guitracer, u_dmsg(failed(X))/*,arcST*/, nortrace, atrace, call(P1, X).
% If in main debug mode, log the failure and throw the failure exception.
md_failed(P1, G):- main_debug, /*notrace*/(write_src_uo(md_failed(P1, G))), !, throw(md_failed(P1, G, 2)).


% must_det_ll(X):- must_det_ll(X),!.

%!  write_src_uo(+G) is det.
%
%   Writes the output of the goal `G` to both the standard output (file descriptor 1) and
%   the standard error (file descriptor 2). The goal `G` is written to the output streams
%   with appropriate newline formatting. There is a commented-out reference to `stack_dump`
%   which could be used for debugging purposes if uncommented.
%
%   @arg G  The goal or content to be written to the output streams.
%
%   Example:
%   ?- write_src_uo(my_goal).
%

% Write the output of G to the stream with file descriptor 1 (standard output).
write_src_uo(G):-
/* current_output(S),
 with_output_to(S,
  (format('~N~n~n', []),
   write_src(G),
   format('~N~n~n'))), !,*/
% stack_dump,
% Write the output of G to the stream with file descriptor 2 (standard error).
 stream_property(S2, file_no(2)),
 with_output_to(S2,
  (format('~N~n', []),
   write_src(G),
   format('~N~n'))), !.

% Declare rrtrace/1 as a meta-predicate that takes a goal.
:- meta_predicate(rrtrace(0)).

%!  rrtrace(:X) is det.
%
%   Executes the given goal `X` with reversible tracing enabled. This predicate wraps the goal `X`
%   with a custom tracing mechanism (`etrace`). The meta-predicate declaration allows the goal `X`
%   to be passed and executed in a tracing context.
%
%   @arg X  The goal to execute with reversible tracing.
%
%   Example:
%   ?- rrtrace(my_goal).
%
% Execute the goal X with reversible tracing using etrace as the tracing context.
rrtrace(X):- rrtrace(etrace, X).

%!  stack_dump is det.
%
%   Attempts to perform a backtrace dump by invoking `bt`. If `bt` fails or is not available, it
%   silently ignores the failure. The commented-out lines suggest other possible debugging dumps
%   like `dumpST` or `bts` that are currently not active.
%
%   Example:
%   ?- stack_dump.
%

% Perform a backtrace dump (bt), ignore any failure.
stack_dump:- ignore(catch(dumpST,_,fail)),!. %ignore(catch(bts,_,true)).
stack_dump:- ignore(catch(bt, _, true)).


%!  ugtrace(+Why, :G) is det.
%
%   Executes the goal `G` with tracing enabled, and provides different behavior depending on
%   the value of `Why` and the environment (e.g., testing, tracing). If an error occurs or
%   tracing is required, this predicate handles logging and debugging actions appropriately.
%
%   @arg Why  The reason or condition for tracing.
%   @arg G    The goal to execute with tracing.
%
%   Example:
%   ?- ugtrace(error(my_reason), my_goal).
%

:- set_prolog_flag(mettalog_error,unset).
%:- set_prolog_flag(mettalog_error,break).
%:- set_prolog_flag(mettalog_error,keep_going).
on_mettalog_error(Why):- (current_prolog_flag(mettalog_error,break);extreme_tracing),!,bt,write_src_uo(on_mettalog_error(Why)),trace.
on_mettalog_error(Why):- write_src_uo(on_mettalog_error(Why)).

% super safety checks is optional code that can be ran .. normally this is done with assertion/1 but unfortionately assertion/1 is not guarenteed to keep bindings and can be said to be wrapped in `once/1`
super_safety_checks(G):- (call(G)*->true;on_mettalog_error(super_safety_checks(failed(G)))).

% If there is an error, log it, perform a stack dump
%ugtrace(Why, _):- notrace((write_src_uo(ugtrace(Why,G)),stack_dump, write_src_uo(ugtrace(Why,G)), fail)).

ugtrace(Why, _):- on_mettalog_error(Why), fail.
% If tracing is already enabled, log the reason and trace the goal G.
ugtrace(_Why, G):- tracing, !, notrace, rtrace(G), trace.

% If testing is enabled, handle the failure and abort.
ugtrace(Why, _):- is_testing, !, ignore(give_up(Why, 5)), throw('$aborted').
% Otherwise, log the reason, trace the goal G, and abort.
ugtrace(Why, G):- fbugio(Why), nortrace, notrace, trace, ggtrace(G), throw('$aborted').
% ugtrace(Why,G):- ggtrace(G).

%!  give_up(+Why, +N) is det.
%
%   Handles fatal errors or conditions by logging the reason `Why` and either throwing an exception
%   or halting the execution. When testing is enabled, the behavior is adjusted accordingly.
%
%   @arg Why  The reason for giving up or aborting.
%   @arg N    The exit code to use if halting the system.
%
%   Example:
%   ?- give_up(my_reason, 1).
%
% If testing is enabled, log the reason and throw an exception.
give_up(Why, _):- is_testing, !, write_src_uo(Why), !, throw(give_up(Why)).
% If testing is enabled, log the reason and halt the system with exit code N.
give_up(Why, N):- is_testing, !, write_src_uo(Why), !, halt(N).
% Otherwise, log the reason and abort the execution.
give_up(Why, _):- write_src_uo(Why), throw('$aborted').

%!  is_guitracer is nondet.
%
%   Succeeds if the Prolog environment is running in a GUI tracer environment. This predicate
%   checks whether the `DISPLAY` environment variable is set and whether the Prolog flag `gui_tracer`
%   is enabled. If both conditions are met, it indicates that a GUI tracer is available.
%
%   Example:
%   ?- is_guitracer.
%
% Succeeds if the 'DISPLAY' environment variable is set and the 'gui_tracer' flag is true.
is_guitracer:- getenv('DISPLAY', _), current_prolog_flag(gui_tracer, true).

:- meta_predicate(rrtrace(1,0)).

%!  rrtrace(:P1, :X) is det.
%
%   Executes the goal `X` with reversible tracing (`rrtrace`) enabled, depending on the environment.
%   This predicate adapts based on conditions such as whether tracing is disabled, whether a CGI environment
%   is active, or whether a GUI tracer is available. In each case, it adjusts the behavior accordingly.
%
%   @arg P1  The wrapper or context in which the goal `X` is executed.
%   @arg X   The goal to be executed with reversible tracing or other tracing options.
%
%   Example:
%   ?- rrtrace(my_wrapper, my_goal).
%
% If reversible tracing is disabled, log the message and fail.
rrtrace(P1, X):- never_rrtrace, !, ((u_dmsg(cant_rrtrace(P1, X)))), !, fail.
% If in a CGI environment, log the HTML output and call the goal normally.
rrtrace(P1, G):- is_cgi, !, u_dmsg(arc_html(rrtrace(P1, G))), call(P1, G).
% If not in a GUI tracer environment, disable tracing and call the goal, or enable interactive tracing (itrace).
rrtrace(P1, X):- notrace, \+ is_guitracer, !, nortrace, /*arcST, sleep(0.5), trace,*/
   (notrace(\+ current_prolog_flag(gui_tracer, true)) -> call(P1, X); (itrace, call(P1, X))).
% rrtrace(_,X):- is_guitracer,!,notrace,nortrace,ncatch(call(call,ugtrace),_,true),atrace,call(X).
% If interactive tracing (itrace) is enabled, call the goal with tracing.
rrtrace(P1, X):- itrace, !, call(P1, X).

:- meta_predicate(arc_wote(0)).

%!  arc_wote(+G) is det.
%
%   Executes the goal `G` using the `wote/1` predicate, with the output formatted using `ansi` styling.
%   This is typically used to write goal output with specific formatting (e.g., for tracing or debugging).
%
%   @arg G  The goal to be written with formatting.
%
%   Example:
%   ?- arc_wote(my_goal).
%
% Write the output of G using ansi formatting with wote/1.
arc_wote(G):- with_pp(ansi, wote(G)).

%!  arcST is det.
%
%   Performs a tracing operation using `itrace` before and after writing the backtrace (`bts`).
%   This predicate helps with tracing back the stack in a formatted manner.
%
%   Example:
%   ?- arcST.
%
% Perform interactive tracing (itrace) before and after writing the backtrace (bts).
arcST:- itrace, arc_wote(bts), itrace.

%!  atrace is det.
%
%   Writes the backtrace (`bts`) using `arc_wote/1`. The commented-out line suggests a fallback
%   method of dumping the stack trace using `dumpST` to the stream associated with file descriptor 2.
%
%   Example:
%   ?- atrace.
%

% Write the backtrace (bts) using arc_wote/1.
atrace:- arc_wote(bts).
% atrace:- ignore((stream_property(X, file_no(2)), with_output_to(X, dumpST))),!.

:- meta_predicate(odd_failure(0)).

%!  odd_failure(:G) is det.
%
%   Executes the goal `G`, ensuring that if reversible tracing (`rrtrace`) is disabled, the goal is executed directly.
%   Otherwise, it attempts to execute `G` using `wno_must/1`, and if `G` fails, it calls `fail_odd_failure/1` to handle the failure.
%
%   @arg G  The goal to be executed, with failure handling if it fails.
%
%   Example:
%   ?- odd_failure(my_goal).
%
% If reversible tracing is disabled, execute the goal directly.
odd_failure(G):- never_rrtrace, !, call(G).
% Attempt to execute the goal G using wno_must/1. If it fails, handle the failure with fail_odd_failure/1.
odd_failure(G):- wno_must(G) *-> true; fail_odd_failure(G).

:- meta_predicate(fail_odd_failure(0)).

%!  fail_odd_failure(:G) is det.
%
%   Handles the failure of the goal `G` by logging the failure using `u_dmsg/1`,
%   tracing the goal with `rtrace/1`, and then failing. This ensures that when the goal fails,
%   it is properly traced and logged for debugging purposes.
%
%   @arg G  The goal that failed and needs to be traced and logged.
%
%   Example:
%   ?- fail_odd_failure(my_goal).
%

% Log the failure of G, trace the goal using rtrace/1, and fail.
fail_odd_failure(G):- u_dmsg(odd_failure(G)), rtrace(G), fail.
% fail_odd_failure(G):- call(G)*->true;(u_dmsg(odd_failure(G)),fail,rrtrace(G)).

%!  bts is det.
%
%   Collects and prints the Prolog backtrace (stack trace) to the standard output (file descriptor 1).
%   It uses the `prolog_stack` library to retrieve the backtrace and print it. Additionally, if the output
%   stream is different from the standard output, it attempts to print the backtrace there as well.
%
%   This predicate is useful for debugging and inspecting the stack trace of Prolog goals.
%
%   Example:
%   ?- bts.
%
% Ensure the 'prolog_stack' library is loaded and import necessary predicates.
bts:-
 ensure_loaded(library(prolog_stack)),
 prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3),
 use_module(library(prolog_stack),[print_prolog_backtrace/2,get_prolog_backtrace_lc/3]),
 /*notrace*/ (prolog_stack:call(call,get_prolog_backtrace_lc,8000, Stack, [goal_depth(600)])),
 % Print the stack trace to the standard output (file descriptor 1).
 stream_property(S, file_no(1)), prolog_stack:print_prolog_backtrace(S, Stack),
 % If the current output is not standard output, print the stack trace there as well.
 ignore((fail, current_output(Out), \+ stream_property(Out, file_no(1)), print_prolog_backtrace(Out, Stack))), !.

%!  my_assertion(:G) is det.
%
%   Ensures that the goal `G` succeeds by attempting to call it. If `G` fails, the failure is logged
%   with additional debugging information, including a message and a break for inspection.
%
%   @arg G  The goal to be asserted or verified.
%
%   Example:
%   ?- my_assertion(my_goal).
%
% First clause: Call the goal G directly.
my_assertion(G):- my_assertion(call(G), G).
% If G succeeds, exit without further action.
my_assertion(_, G):- call(G), !.
% If G fails, log the failure, print debugging information, and enter a break for inspection.
my_assertion(Why, G):- u_dmsg(my_assertion(Why, G)), writeq(Why = goal(G)), nl, !, ibreak.

%!  must_be_free(+Free) is det.
%
%   Ensures that the term `Free` is a free variable (unbound). If `Free` is not a free variable,
%   this predicate either succeeds (if the term is a plain variable) or enters a debugging break
%   with relevant attributes printed.
%
%   @arg Free  The term that is expected to be a free variable.
%
%   Example:
%   ?- must_be_free(Var).
%
% Succeed if Free is a plain (unbound) variable.
must_be_free(Free):- plain_var(Free), !.
% Succeed if Free is not a non-variable or a constraint identifier (CI).
must_be_free(Free):- \+ nonvar_or_ci(Free), !.
% If Free is not free, log the failure, print attributes if available, and enter a break for inspection.
must_be_free(Nonfree):- arcST, u_dmsg(must_be_free(Nonfree)),
  ignore((attvar(Nonfree), get_attrs(Nonfree, ATTS), pp(ATTS))), ibreak, fail.

%!  must_be_nonvar(+Nonvar) is det.
%
%   Ensures that the term `Nonvar` is not a free variable (i.e., it is a non-variable). If `Nonvar` is
%   a variable, it logs the failure and enters a debugging break for inspection.
%
%   @arg Nonvar  The term that is expected to be non-variable.
%
%   Example:
%   ?- must_be_nonvar(my_term).
%
% Succeed if Nonvar is not a free variable or is a constraint identifier (CI).
must_be_nonvar(Nonvar):- nonvar_or_ci(Nonvar), !.
% If Nonvar is a free variable, log the failure and enter a debugging break for inspection.
must_be_nonvar(IsVar):- arcST, u_dmsg(must_be_nonvar(IsVar)), ibreak, fail.


% goal_expansion(must_det_l(G),I,must_det_ll(G),O):- nonvar(I),source_location(_,_), nonvar(G),I=O.

%goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), compound(G), remove_mds(MD,G,GG),I=O.

%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
/*
goal_expansion(Goal,Out):- compound(Goal), tc_arg(N1,Goal,E),
   compound(E), E = set(Obj,Member), setarg(N1,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/

%!  get_setarg_p1(+P3, +E, +Cmpd, -SA) is det.
%
%   Attempts to retrieve and set the argument `E` within the compound term `Cmpd` using `P3`.
%   If the compound term `Cmpd` contains `E`, it delegates the task to `get_setarg_p2/4` to handle
%   the actual operation.
%
%   @arg P3   A predicate or wrapper to apply to the argument index and the compound term.
%   @arg E    The argument to be found within the compound term `Cmpd`.
%   @arg Cmpd The compound term in which the argument `E` is located.
%   @arg SA   The result, a goal of the form `call(P3, N, Cmpd)` where `N` is the argument index.
%
%   Example:
%   ?- get_setarg_p1(my_predicate, arg_value, my_compound, SA).
%
get_setarg_p1(P3, E, Cmpd, SA):- compound(Cmpd), get_setarg_p2(P3, E, Cmpd, SA).

%!  get_setarg_p2(+P3, +E, +Cmpd, -SA) is det.
%
%   Attempts to find the argument `E` within the compound term `Cmpd`. If `E` is found,
%   it constructs the goal `SA` by applying `P3` to the argument index and the compound term.
%   If not found, it recursively searches deeper into nested compound terms.
%
%   @arg P3   A predicate or wrapper to apply to the argument index and the compound term.
%   @arg E    The argument to be found within the compound term `Cmpd`.
%   @arg Cmpd The compound term in which the argument `E` is located.
%   @arg SA   The result, a goal of the form `call(P3, N, Cmpd)` where `N` is the argument index.
%
%   Example:
%   ?- get_setarg_p2(my_predicate, arg_value, my_compound, SA).
%
get_setarg_p2(P3, E, Cmpd, SA):- arg(N1, Cmpd, E), SA = call(P3, N1, Cmpd).
get_setarg_p2(P3, E, Cmpd, SA):- arg(_, Cmpd, Arg), get_setarg_p1(P3, E, Arg, SA).

%!  my_b_set_dict(+Member, +Obj, +Var) is det.
%
%   Sets the dictionary member `Member` in the object `Obj` to the value `Var` by calling
%   `set_omemberh/4` with a base context of `b`. This predicate acts as a wrapper around
%   `set_omemberh/4` for setting a member in a dictionary-like structure.
%
%   @arg Member  The member key in the dictionary to be updated.
%   @arg Obj     The object or dictionary-like structure that contains the member.
%   @arg Var     The value to set for the member.
%
%   Example:
%   ?- my_b_set_dict(key, my_object, value).
%
% Set the member of the object using set_omemberh with the context 'b'.
my_b_set_dict(Member, Obj, Var):- set_omemberh(b, Member, Obj, Var).

%nb_set_dict(Member,Obj,Var),

%!  set_omemberh(+Context, +Member, +Obj, +Var) is det.
%
%   Sets the value of the `Member` in the object `Obj` to `Var`, disregarding the context. This predicate
%   directly updates the member value by calling `arc_setval/3`, effectively ignoring the first argument.
%
%   @arg Context  The context (ignored in this implementation).
%   @arg Member   The member key in the object to be updated.
%   @arg Obj      The object or dictionary-like structure that contains the member.
%   @arg Var      The value to set for the member.
%
%   Example:
%   ?- set_omemberh(_, key, my_object, value).
%

% Set the member of the object to Var, ignoring the context.
set_omemberh(_, Member, Obj, Var):- !, arc_setval(Obj, Member, Var).

%nb_link_dict(Member,Obj,Var),
%set_omemberh(nb,Member,Obj,Var):- !, nb_set_dict(Member,Obj,Var).
%set_omemberh(link,Member,Obj,Var):- !, nb_link_dict(Member,Obj,Var).
%set_omemberh(How,Member,Obj,Var):- call(call,How,Member,Obj,Var),!.

%!  set_omember(+Member, +Obj, +Var) is det.
%
%   Sets the `Member` of the object `Obj` to the value `Var` by calling `set_omember/4` with a default
%   context of `b`. This acts as a shorthand for calling `set_omember/4` with a predefined context.
%
%   @arg Member  The member key to be updated in the object.
%   @arg Obj     The object or dictionary-like structure containing the member.
%   @arg Var     The value to set for the member.
%
%   Example:
%   ?- set_omember(key, my_object, value).
%
set_omember(Member, Obj, Var):- set_omember(b, Member, Obj, Var).

%!  set_omember(+How, +Member, +Obj, +Var) is det.
%
%   Sets the `Member` of the object `Obj` to the value `Var`, with the `How` argument controlling
%   the context or method of setting the member. This ensures that `How`, `Member`, and `Obj` are
%   non-variables before proceeding to update the member using `set_omemberh/4`.
%
%   @arg How     The context or method of setting the member (must be non-variable).
%   @arg Member  The member key to be updated in the object.
%   @arg Obj     The object or dictionary-like structure containing the member.
%   @arg Var     The value to set for the member.
%
%   Example:
%   ?- set_omember(b, key, my_object, value).
%
set_omember(How, Member, Obj, Var):-
  must_be_nonvar(Member), must_be_nonvar(Obj), must_be_nonvar(How), !,
  set_omemberh(How, Member, Obj, Var), !.

%!  get_kov(+K, +O, -V) is nondet.
%
%   Retrieves the value `V` associated with key `K` in the object `O`. This predicate first checks
%   if the key-value pair can be obtained using `dictoo:is_dot_hook/4`, and if not, it tries alternative
%   methods to fetch nested values or properties within the object.
%
%   @arg K  The key to look up.
%   @arg O  The object or structure where the key is located.
%   @arg V  The value associated with the key.
%
%   Example:
%   ?- get_kov(key, object, Value).
%
% Try to retrieve the value using `is_dot_hook`, otherwise attempt other retrieval methods.
get_kov(K, O, V):- dictoo:is_dot_hook(user, O, K, V), !, o_m_v(O, K, V).
% Attempt nested lookups or alternative retrieval methods if the initial retrieval fails.
get_kov(K, O, V):- ((get_kov1(K, O, V) *-> true ; (get_kov1(props, O, VV), get_kov1(K, VV, V)))).

%!  get_kov1(+K, +O, -V) is nondet.
%
%   Tries to retrieve the value `V` associated with key `K` in the object `O`, first by checking if the
%   object `O` is a hooked object. If not, it delegates the task to `get_kov2/3`.
%
%   @arg K  The key to look up.
%   @arg O  The object or structure where the key is located.
%   @arg V  The value associated with the key.
%
%   Example:
%   ?- get_kov1(key, object, Value).
%
% Check if `O` is a hooked object and retrieve `K`, otherwise delegate to `get_kov2`.
get_kov1(K, O, V):- (is_hooked_obj(O), o_m_v(O, K, V)) *-> true ; get_kov2(K, O, V).

%!  get_kov2(+K, +O, -V) is nondet.
%
%   Attempts to retrieve the value `V` for key `K` in object `O` by checking if the object is a dictionary,
%   a red-black tree, or a nested structure with `oov` values.
%
%   @arg K  The key to look up.
%   @arg O  The object or structure where the key is located.
%   @arg V  The value associated with the key.
%
%   Example:
%   ?- get_kov2(key, object, Value).
%
% Retrieve the value from a dictionary, handling `oov` values.
get_kov2(K, O, V):- is_dict(O), !, get_dict(K, O, OOV), get_oov_value(OOV, V).
% Retrieve the value from a red-black tree using `rb_lookup`.
get_kov2(K, O, V):- nonvar(K), is_rbtree(O), !, rb_lookup(K, V, O).
% Iterate through a red-black tree and handle `oov` values.
get_kov2(K, O, V):- is_rbtree(O), !, rb_in(K, V, OOV), get_oov_value(OOV, V).
% get_kov(K,O,V):- is_rbtree(O),!,nb_rb_get_node(K,O,Node),nb_rb_node_value(Node,V).

%!  get_oov_value(+ValueOOV, -Value) is det.
%
%   Extracts the actual value from an `oov` (object-oriented value) wrapper if present, or returns the
%   value unchanged if no wrapper is found.
%
%   @arg ValueOOV  The wrapped or unwrapped value.
%   @arg Value     The actual value, unwrapped if necessary.
%
%   Example:
%   ?- get_oov_value(oov(value), UnwrappedValue).
%
% Unwrap the value from `oov` if present, otherwise return the value as is.
get_oov_value(ValueOOV, Value):- compound(ValueOOV), ValueOOV = oov(Value), !.
% Return the value as is if no `oov` wrapper is found.
get_oov_value(Value, Value).

%!  term_expansion_setter(+I, -O) is nondet.
%
%   Attempts to expand the term `I` using `must_det_ll/2`. If successful, returns the expanded term `O`.
%   This handles special cases where the input term has setter syntax or certain expansion rules apply.
%
%   @arg I  The input term to be expanded.
%   @arg O  The expanded output term.
%
%   Example:
%   ?- term_expansion_setter(my_input_term, ExpandedTerm).
%
% Attempt to expand the term using `must_det_ll` and return the expanded version.
term_expansion_setter(I, O):- maybe_expand_md(must_det_ll, I, O), I \=@= O, !.
% Try expanding the term again if the first expansion was incomplete.
term_expansion_setter(I, O):- maybe_expand_md(must_det_ll, I, M), I \=@= M, !, term_expansion_setter(M, O).
term_expansion_setter(Goal, get_kov(Func, Self, Value)):- compound(Goal),
  compound_name_arguments(Goal, '.', [Self, Func, Value]), var(Value).
term_expansion_setter((Head :- Body), Out) :-
   get_setarg_p1(setarg, I, Head, P1),is_setter_syntax(I, Obj, Member, Var, How),call(P1, Var),
   BodyCode = (Body, set_omember(How, Member, Obj, Var)),expand_term((Head :- BodyCode), Out), !.
% term_expansion_setter((Head:-Body), (Head:-GBody)) :- goal_expansion_setter(Body, GBody), !.

% Export the term_expansion_setter/2 predicate.
:- export(term_expansion_setter/2).

% Import the term_expansion_setter/2 predicate into the system module.
:- system:import(term_expansion_setter/2).

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).

%!  is_setter_syntax(+I, -Obj, -Member, -Var, -How) is nondet.
%
%   Checks whether the term `I` follows setter syntax for objects and members. This includes various
%   patterns like `set/2`, `gset/2`, and `hset/3`, as well as their shorthand dot notation forms.
%
%   @arg I       The input term to check for setter syntax.
%   @arg Obj     The object component extracted from the setter syntax.
%   @arg Member  The member component extracted from the setter syntax.
%   @arg Var     The variable to be assigned the value.
%   @arg How     The method or context for setting (e.g., `b`, `nb`, or custom).
%
%   Example:
%   ?- is_setter_syntax(set(my_object, my_member), Obj, Member, Var, How).
%
% Fail if I is not a compound term.
is_setter_syntax(I, _Obj, _Member, _Var, _) :- \+ compound(I), !, fail.
% Handle set/2 syntax with a base context (b).
is_setter_syntax(set(Obj, Member), Obj, Member, _Var, b).
% Handle gset/2 syntax with a non-backtrackable context (nb).
is_setter_syntax(gset(Obj, Member), Obj, Member, _Var, nb).
% Handle hset/3 syntax with a custom context (How).
is_setter_syntax(hset(How, Obj, Member), Obj, Member, _Var, How).
% Handle set/1 syntax using dot notation for object and member.
is_setter_syntax(set(ObjMember), Obj, Member, _Var, b):- obj_member_syntax(ObjMember, Obj, Member).
% Handle gset/1 syntax using dot notation for object and member.
is_setter_syntax(gset(ObjMember), Obj, Member, _Var, nb):- obj_member_syntax(ObjMember, Obj, Member).
% Handle hset/2 syntax using dot notation for object and member.
is_setter_syntax(hset(How, ObjMember), Obj, Member, _Var, How):- obj_member_syntax(ObjMember, Obj, Member).

%!  obj_member_syntax(+ObjMember, -Obj, -Member) is nondet.
%
%   Ensures the compound `ObjMember` follows the dot notation syntax for objects and members (i.e., `Obj.Member`).
%
%   @arg ObjMember  The compound term representing the object and member in dot notation.
%   @arg Obj        The object component.
%   @arg Member     The member component.
%
%   Example:
%   ?- obj_member_syntax(my_object.my_member, Obj, Member).
%
% Ensure the compound follows dot notation Obj.Member.
obj_member_syntax(ObjMember, Obj, Member):- compound(ObjMember), compound_name_arguments(ObjMember, '.', [Obj, Member]), !.

%!  maybe_expand_md(+MD, +I, -O) is nondet.
%
%   Expands the term `I` using the meta-predicate `MD` if it matches a specific pattern. If the input
%   term is a compound with the meta-predicate `MD` or contains a subterm that matches, this predicate
%   attempts to expand the term into `O`.
%
%   @arg MD  The meta-predicate used for expanding the term (e.g., `must_det_ll`).
%   @arg I   The input term to be checked for expansion.
%   @arg O   The expanded output term.
%
%   Example:
%   ?- maybe_expand_md(must_det_ll, must_det_ll(my_goal), ExpandedGoal).
%

% Fail if I is not a compound term.
maybe_expand_md(_MD, I, _) :- \+ compound(I), !, fail.
%maybe_expand_md(MD,I,_):- compound(I),!,fail. % THIS DISABLES
% THIS DISABLES
%maybe_expand_md(MD,must_det_ll(GoalL),GoalL):-!.
% Expand a compound term with MD as its functor.
maybe_expand_md(MD, MDGoal, GoalLO) :- compound_name_arg(MDGoal, MD, Goal), !, expand_md(MD, Goal, GoalLO).
% Expand a maplist using MD as the functor.
maybe_expand_md(MD, maplist(P1, GoalL), GoalLO) :- P1 == MD, !, expand_md(MD, GoalL, GoalLO).
% Handle further maplist expansion with MD.
maybe_expand_md(MD, maplist(P1, GoalL), GoalLO) :- P1 == MD, !, expand_md(MD, GoalL, GoalLO).
% Expand a subterm in I where the compound contains MD as the functor and has a conjunction.
maybe_expand_md(MD, I, O) :- sub_term_safely(C, I), compound(C), compound_name_arg(C, MD, Goal),
   compound(Goal), Goal = (_, _),
   once((expand_md(MD, Goal, GoalO), substM(I, C, GoalO, O))), I \=@= O.
%maybe_expand_md(MD,I,O):- sub_term_safely(S,I),compound(S),S=must_det_ll(G),
%  once(expand_md(MD,S,M)),M\=S,

%!  expand_md(+MD, +A, -AA) is det.
%
%   Expands the term `A` using the meta-predicate `MD`. This predicate handles expanding lists,
%   variables, and non-callable terms, as well as recursive expansion of compound terms.
%
%   @arg MD  The meta-predicate used for expanding.
%   @arg A   The input term to be expanded.
%   @arg AA  The expanded output term.
%
%   Example:
%   ?- expand_md(must_det_ll, [goal1, goal2], Expanded).
%
% Expand an empty list into true.
expand_md(_MD, Nil, true) :- Nil == [], !.
% If the input is not callable, return it as is.
expand_md(_MD, Var, Var) :- \+ callable(Var), !.
% Expand a list of goals into a conjunction of expanded goals.
expand_md(MD, [A|B], (AA, BB)) :- assertion(callable(A)), assertion(is_list(B)), !,
  expand_md1(MD, A, AA), expand_md(MD, B, BB).
% Expand any other term using expand_md1/3.
expand_md(MD, A, AA) :- !, expand_md1(MD, A, AA).

%!  prevents_expansion(+A) is nondet.
%
%   Succeeds if the term `A` is a trace-related call, indicating that expansion should be prevented.
%
%   @arg A  The term to be checked for trace calls.
%
%   Example:
%   ?- prevents_expansion(trace).
%
% Succeed if A is a trace-related call.
prevents_expansion(A):- is_trace_call(A).

%!  is_trace_call(+A) is nondet.
%
%   Checks if the term `A` is a trace call, such as `trace` or `itrace`.
%
%   @arg A  The term to be checked.
%
%   Example:
%   ?- is_trace_call(trace).
%
% Succeed if A is the `trace` term.
is_trace_call(A):- A == trace.
% Succeed if A is the `itrace` term.
is_trace_call(A):- A == itrace.

%!  skip_expansion(+A) is nondet.
%
%   Succeeds if the term `A` is a special case where expansion should be skipped. This includes
%   specific control structures and some compound terms.
%
%   @arg A  The term to be checked for expansion skipping.
%
%   Example:
%   ?- skip_expansion(true).
%
% Fail if A is a variable.
skip_expansion(A):- var(A), !, fail.
% Succeed if A is a cut.
skip_expansion(!).
% Succeed if A is `false`.
skip_expansion(false).
% Succeed if A is `true`.
skip_expansion(true).
% Succeed if A is a compound term whose functor should skip expansion.
skip_expansion(C):- compound(C), functor(C, F, A), skip_fa_expansion(F, A).

%!  skip_fa_expansion(+F, +A) is nondet.
%
%   Succeeds if the functor-arity pair `F/A` matches one of the special cases for skipping expansion.
%
%   @arg F  The functor of the term.
%   @arg A  The arity of the term.
%
%   Example:
%   ?- skip_fa_expansion(call, 2).
%
% Succeed if the functor is `once` and the arity is 1.
skip_fa_expansion(once, 1).
% Succeed if the functor is `call` with any arity.
skip_fa_expansion(call, _).
% Succeed if the functor is `if_t` and the arity is 2.
skip_fa_expansion(if_t, 2).

%!  expand_md1(+MD, +A, -AA) is det.
%
%   Expands the term `A` using the meta-predicate `MD`. This handles various cases such as skipping
%   certain terms, expanding conjunctions, conditionals, and applying expansions to specific functors
%   like `maplist/3`. It also handles predicates with meta-predicate properties.
%
%   @arg MD  The meta-predicate used for expanding.
%   @arg A   The input term to be expanded.
%   @arg AA  The expanded output term.
%
%   Example:
%   ?- expand_md1(must_det_ll, call(my_goal), Expanded).
%
% Do not expand if the term is not callable.
expand_md1(_MD, Var, Var):- \+ callable(Var), !.
% Skip expansion for specific terms like cuts, true/false, etc.
expand_md1(_MD, Cut, Cut):- skip_expansion(Cut), !.
% Expand compound terms that match the MD functor.
expand_md1(MD, MDAB, AABB):- compound(MDAB), compound_name_arg(MDAB, MD, AB), !, expand_md(MD, AB, AABB).
% Expand maplist/2 and maplist/3 using md_maplist.
expand_md1(MD, maplist(P1, A), md_maplist(MD, P1, A)):- !.
expand_md1(MD, maplist(P2, A, B), md_maplist(MD, P2, A, B)):- !.
expand_md1(MD, maplist(P3, A, B, C), md_maplist(MD, P3, A, B, C)):- !.
% Expand custom my_maplist/2 and my_maplist/3 using md_maplist.
expand_md1(MD, my_maplist(P1, A), md_maplist(MD, P1, A)):- !.
expand_md1(MD, my_maplist(P2, A, B), md_maplist(MD, P2, A, B)):- !.
expand_md1(MD, my_maplist(P3, A, B, C), md_maplist(MD, P3, A, B, C)):- !.
%expand_md1(MD,Goal,O):- \+ compound(Goal), !,O = must_det_ll(Goal).
%expand_md1(MD,(A,B),((A,B))):- remove_must_det(MD), prevents_expansion(A),!.
%expand_md1(MD,(A,B),must_det_ll((A,B))):- prevents_expansion(A),!.
% Expand conjunctions (A, B) by expanding both sides.
expand_md1(MD, (A, B), (AA, BB)):- !, expand_md(MD, A, AA), expand_md(MD, B, BB).
% Expand conditionals (*->; and ->;) by expanding conditions and branches.
expand_md1(MD, (C *-> A ; B), (CC *-> AA ; BB)):- !, expand_md(MD, A, AA), expand_md(MD, B, BB), expand_must_not_error(C, CC).
expand_md1(MD, (C -> A ; B), (CC -> AA ; BB)):- !, expand_md(MD, A, AA), expand_md(MD, B, BB), expand_must_not_error(C, CC).
% Expand disjunctions (C;B) by expanding both sides.
expand_md1(MD, (C ; B), (CC ; BB)):- !, expand_md(MD, B, BB), expand_must_not_error(C, CC).
% Expand locally/2 constructs by expanding the body.
expand_md1(MD, locally(C, A), locally(C, AA)):- !, expand_md(MD, A, AA).
% Expand call_cleanup/2 and setup_call_cleanup/3 constructs.
expand_md1(MD, call_cleanup(A, B), call_cleanup(AA, BB)):- !, expand_md(MD, A, AA), expand_md(MD, B, BB).
expand_md1(MD, setup_call_cleanup(C, A, B), setup_call_cleanup(CC, AA, BB)):- !,
  expand_md(MD, C, CC), expand_md(MD, A, AA), expand_md(MD, B, BB).
% Expand module-qualified calls.
expand_md1(MD, M:P, M:AABB):- !, expand_md(MD, P, AABB).
% Handle meta-predicates by expanding their arguments.
expand_md1(MD, P, AABB) :- predicate_property(P, (meta_predicate(MP))),
   strip_module(P, _, SP), strip_module(MP, _, SMP), kaggle_arc_1_pred(_, SP),
   \+ skippable_built_in(P),
   SP =.. [F | Args], SMP =.. [F | Margs], !,
   maplist(expand_meta_predicate_arg(MD), Margs, Args, EArgs),
   AABB =.. [F | EArgs].
% Expand non-must_det_ll goals.
expand_md1(MD, A, MDAA):- \+ remove_must_det(MD), !, expand_goal(A, AA), !, compound_name_arg(MDAA, MD, AA).
% Expand generic goals.
expand_md1(_MD, A, AA):- expand_goal(A, AA), !.

%!  expand_must_not_error(+C, -CC) is det.
%
%   Expands the goal `C` by wrapping it with `must_not_error/1`, unless `must_not_error`
%   is removed from consideration, or the goal already has a meta-predicate property.
%
%   @arg C   The input goal to expand.
%   @arg CC  The expanded goal wrapped with `must_not_error/1`, if applicable.
%
%   Example:
%   ?- expand_must_not_error(my_goal, ExpandedGoal).
%
% Do not expand if must_not_error is disabled.
expand_must_not_error(C, C):- remove_must_det(must_not_error), !.
% Wrap the goal in must_not_error unless it has a meta-predicate property.
expand_must_not_error(C, CC):- \+ predicate_property(C, meta_predicate(_)), !, CC = must_not_error(C), !.
% Recursively expand the goal using must_not_error.
expand_must_not_error(C, CC):- expand_md(must_not_error, C, CC).

%!  kaggle_arc_1_pred(-M, +P) is nondet.
%
%   Succeeds if the predicate `M:P` is defined in a file containing 'arc_' in its name,
%   excluding certain files like '_pfc' and '_afc'. This is used to identify specific
%   predicates related to 'arc_' modules.
%
%   @arg M  The module of the predicate.
%   @arg P  The predicate to check.
%
%   Example:
%   ?- kaggle_arc_1_pred(M, my_predicate).
%
% Succeed if the predicate M:P is defined in a file containing 'arc_' but not '_pfc', '_afc', or others.
kaggle_arc_1_pred(M, P):-
  predicate_property(M:P, file(F)),
  \+ predicate_property(M:P, imported_from(_)),
  \+ \+ atom_contains(F, 'arc_'),
  \+ atom_contains(F, '_pfc'),
  \+ atom_contains(F, '_afc'),
  % \+ atom_contains(F,'_ui_'),
  true.

%meta_builtin(P):- var(P),meta_builtin(P).
%meta_builtin(P):- predicate_property(P,interpreted),predicate_property(P,static).

%!  skippable_built_in(+MP) is nondet.
%
%   Succeeds if the predicate `MP` is a built-in system predicate that should be skipped during expansion.
%   It checks if the predicate is either ISO-compliant or marked as `notrace`.
%
%   @arg MP  The module-qualified predicate to check.
%
%   Example:
%   ?- skippable_built_in(system:my_builtin).
%
% Succeed if the predicate is built-in and either ISO-compliant or marked as `notrace`.
skippable_built_in(MP):-
  strip_module(MP, _, P),
  predicate_property(system:P, built_in),
  once(predicate_property(system:P, iso); predicate_property(system:P, notrace)).

%meta_builtin(P):- predicate_property(P,/*notrace*/), \+ predicate_property(P,nodebug).

%!  expand_meta_predicate_arg(+MD, +Spec, +A, -AA) is det.
%
%   Expands the argument `A` of a meta-predicate, based on the meta-predicate specification `Spec`.
%   Depending on the type of argument (`?`, `+`, `-`, `:`, or `0`), it may be passed through unchanged
%   or expanded using `expand_md1/3`.
%
%   @arg MD   The meta-predicate used for expansion.
%   @arg Spec The meta-predicate argument specification (e.g., `+`, `:`, `0`).
%   @arg A    The input argument to be expanded.
%   @arg AA   The expanded argument.
%
%   Example:
%   ?- expand_meta_predicate_arg(must_det_ll, ':', my_goal, ExpandedGoal).
%
% Do not expand arguments marked as '?'.
expand_meta_predicate_arg(_MD, '?', A, A):- !.
% Do not expand arguments marked as '+'.
expand_meta_predicate_arg(_MD, '+', A, A):- !.
% Do not expand arguments marked as '-'.
expand_meta_predicate_arg(_MD, '-', A, A):- !.
% Expand arguments marked as ':' using expand_md1.
expand_meta_predicate_arg(MD, ':', A, AA):- !, expand_md1(MD, A, AA).
% Expand arguments marked as '0' using expand_md1.
expand_meta_predicate_arg(MD, 0, A, AA):- !, expand_md1(MD, A, AA).
%expand_meta_predicate_arg(MD,*,A,AA):- !,expand_md1(MD,A,AA).
% Pass through any other arguments unchanged.
expand_meta_predicate_arg(_MD, _, A, A).

%!  goal_expansion_getter(+Goal, -O) is det.
%
%   Expands a goal by recursively applying meta-predicate expansion or checking for getter-like syntax
%   (e.g., dot notation). If the goal matches a getter pattern, it is transformed into a `get_kov/3` call.
%
%   @arg Goal The goal to be expanded.
%   @arg O    The expanded or transformed goal.
%
%   Example:
%   ?- goal_expansion_getter(my_object.my_member, ExpandedGoal).
%
% If the goal is not a compound, return it unchanged.
goal_expansion_getter(Goal, O):- \+ compound(Goal), !, O = Goal.
% Expand a goal using meta-predicate expansion.
goal_expansion_getter(I, O):- md_like(MD), maybe_expand_md(MD, I, O), I \=@= O, !.
% If the first expansion was partial, expand the goal again.
goal_expansion_getter(I, O):- md_like(MD), maybe_expand_md(MD, I, M), I \=@= M, !, goal_expansion_getter(M, O).
% Handle dot notation goals and convert them into get_kov/3.
goal_expansion_getter(Goal, get_kov(Func, Self, Value)):-
  compound(Goal),
  compound_name_arguments(Goal, '.', [Self, Func, Value]),
  var(Value).
% Recursively expand the arguments of compound goals.
goal_expansion_getter(Goal, Out):-
  compound_name_arguments(Goal, F, Args),
  maplist(goal_expansion_getter, Args, ArgsOut),
  compound_name_arguments(Out, F, ArgsOut).

% Export the goal_expansion_getter/2 predicate.
:- export(goal_expansion_getter/2).

% Import the goal_expansion_getter/2 predicate into the system module.
:- system:import(goal_expansion_getter/2).

%!  goal_expansion_setter(+Goal, -O) is nondet.
%
%   Expands a goal by handling setter-like syntax, applying meta-predicate expansions,
%   and transforming specific patterns into more suitable forms like `set_omember/4`.
%   It also handles compound terms and dot notation for objects.
%
%   @arg Goal  The goal to be expanded.
%   @arg O     The expanded goal.
%
%   Example:
%   ?- goal_expansion_setter(my_goal, ExpandedGoal).
%
% Fail if the goal is not a compound term.
goal_expansion_setter(Goal, _) :- \+ compound(Goal), !, fail.
% Attempt to expand the goal using meta-predicate expansion.
goal_expansion_setter(I, O) :- md_like(MD), maybe_expand_md(MD, I, O), I \=@= O, !.
% Remove must_det_ll from the goal and attempt further expansion.
goal_expansion_setter(G, GO) :- remove_must_det(MD), !, remove_mds(MD, G, GG), goal_expansion_setter(GG, GO).
%goal_expansion_setter(GG,GO):- remove_must_det(MD), sub_term_safely(G,GG),compound(G),G = must_det_ll(GGGG),subst001(GG,G,GGGG,GGG),!,goal_expansion_setter(GGG,GO).
%goal_expansion_setter((G1,G2),(O1,O2)):- !, expand_goal(G1,O1), expand_goal(G2,O2),!.
% Handle `set_omember/4` goals as pass-through.
goal_expansion_setter(set_omember(A, B, C, D), set_omember(A, B, C, D)) :- !.
% Handle `set_omember/3` goals by setting the default mode to 'b'.
goal_expansion_setter(set_omember(A, B, C), set_omember(b, A, B, C)) :- !.
% Handle dot notation goals and convert them into get_kov/3.
goal_expansion_setter(Goal, get_kov(Func, Self, Value)) :-
  compound(Goal),
  compound_name_arguments(Goal, '.', [Self, Func, Value]),
  var(Value).
% Attempt to expand the goal again after further meta-predicate expansion.
goal_expansion_setter(I, O) :- md_like(MD), maybe_expand_md(MD, I, M), I \=@= M, !, goal_expansion_setter(M, O).
% Expand goals with meta-predicate properties (early exit on fail for unsupported goals).
goal_expansion_setter(Goal, Out) :-
   predicate_property(Goal, meta_predicate(_)), !, fail,
   tc_arg(N1, Goal, P), goal_expansion_setter(P, MOut),
   setarg(N1, Goal, MOut), !, expand_goal(Goal, Out).
% Expand goals that follow setter syntax by updating members using set_omember/4.
goal_expansion_setter(Goal, Out) :-
   tc_arg(N1, Goal, P), is_setter_syntax(P, Obj, Member, Var, How),
   setarg(N1, Goal, Var), !, expand_goal((Goal, set_omember(How, Member, Obj, Var)), Out).
% Handle dot notation goals and convert them into get_kov/3 for further expansion.
goal_expansion_setter(Goal, Out) :-
   get_setarg_p1(setarg, I, Goal, P1), compound(I), compound_name_arguments(I, '.', [Self, Func, Value]),
   call(P1, get_kov(Func, Self, Value)), !,
   expand_goal(Goal, Out).
% Handle setter syntax for goals and expand them into set_omember/4.
goal_expansion_setter(Goal, Out) :-
   get_setarg_p1(setarg, I, Goal, P1), is_setter_syntax(I, Obj, Member, Var, How),
   call(P1, Var), !,
   expand_goal((Goal, set_omember(How, Member, Obj, Var)), Out).

% Export the goal_expansion_setter/2 predicate.
:- export(goal_expansion_setter/2).

% Import the goal_expansion_setter/2 predicate into the system module.
:- system:import(goal_expansion_setter/2).

/*
system:term_expansion((Head:-Goal),I,(Head:-Out),O):- nonvar(I),  compound(Goal),
 goal_expansion_setter(Goal,Out),Goal\=@=Out,I=O,!,
 nop((print(goal_expansion_getter(Goal-->Out)),nl)).
*/

%!  arc_term_expansion1(+Clause) is det.
%
%   Expands the given term or goal by applying specific transformations using term and goal expansion logic.
%   The first clause handles `term_expansion/4`, while the second clause handles `goal_expansion/4`.
%   Both expansions utilize setter-like syntax or goal transformations.
%
%   @arg Clause  The clause to be expanded, either a term or a goal.
%
%   Example:
%   ?- arc_term_expansion1((term_expansion((Head :- Body), I, Out, O))).
%
% Perform term expansion by applying the term_expansion_setter and printing the transformation if successful.
arc_term_expansion1((system:term_expansion((Head:-Body), I, Out, O) :-
   nonvar(I), compound(Head),
   term_expansion_setter((Head:-Body), Out), (Head:-Body) = In, In \== Out, I = O, !,
   nop((print(term_expansion_setter(In --> Out)), nl)))).
%system:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),Goal\==Out,I=O,!,
%  ((print(goal_expansion_getter(Goal-->Out)),nl)).
%user:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),Goal\==Out,I=O,!,
%  ((print(goal_expansion_getter(Goal-->Out)),nl)).
% Perform goal expansion by applying the goal_expansion_setter and printing the transformation if successful.
arc_term_expansion1((goal_expansion(Goal, I, Out, O) :-
   goal_expansion_setter(Goal, Out), Goal \== Out, I = O, !,
   nop((print(goal_expansion_setter(Goal --> Out)), nl)))).

% Export the arc_term_expansions/1 predicate.
:- export(arc_term_expansions/1).

% Define arc_term_expansions/1 for applying arc-term expansion rules.
arc_term_expansions(H:- (current_prolog_flag(arc_term_expansion, true), B)) :-
  arc_term_expansion1(H:-B).

% Export the enable_arc_expansion/0 predicate.
:- export(enable_arc_expansion/0).

%!  enable_arc_expansion is det.
%
%   Enables arc-term expansion by asserting all rules defined in `arc_term_expansions/1` and
%   setting the `arc_term_expansion` flag to true.
%
%   Example:
%   ?- enable_arc_expansion.
%
enable_arc_expansion :-
 forall(arc_term_expansions(Rule),
   (strip_module(Rule, M, Rule0),
     nop(u_dmsg(asserta_if_new(Rule, M, Rule0))),
     asserta_if_new(Rule))),
 set_prolog_flag(arc_term_expansion, true).

% Export the disable_arc_expansion/0 predicate.
:- export(disable_arc_expansion/0).

%!  disable_arc_expansion is det.
%
%   Disables arc-term expansion by retracting all rules defined in `arc_term_expansions/1`
%   and setting the `arc_term_expansion` flag to false.
%
%   Example:
%   ?- disable_arc_expansion.
%
disable_arc_expansion :-
 forall(arc_term_expansions(Rule), forall(retract(Rule), true)),
 set_prolog_flag(arc_term_expansion, false).

% Declare goal_expansion/4 as multifile and dynamic.
:- multifile(goal_expansion/4).
:- dynamic(goal_expansion/4).

%!  goal_expansion(+G, +I, -GG, -O) is nondet.
%
%   Expands the goal `G` into `GG` if the goal contains must_det_ll or other meta-predicates, and the
%   source location is known. The expansion may use either `remove_must_det/1` or `maybe_expand_md/3`.
%
%   @arg G   The original goal.
%   @arg I   The input indicator (nonvar).
%   @arg GG  The expanded goal.
%   @arg O   The output indicator (same as `I`).
%
% Expand goal if input is non-variable, the source is known, and G is a compound term.
goal_expansion(G, I, GG, O) :-
  nonvar(I), source_location(_, _),
  compound(G),
  % If must_det_ll should be removed, expand using remove_mds/3, else try meta-predicate expansion.
  (remove_must_det(MD) -> remove_mds(MD, G, GG) ; (md_like(MD), maybe_expand_md(MD, G, GG))),
  I = O.









/*
:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).

my_assertion(G):- call(G),!.
my_assertion(G):- fbug(my_assertion(G)),writeq(goal(G)),nl,!,break.
must_be_free(AllNew):- plain_var(AllNew),!.
must_be_free(AllNew):- arcST,fbug(must_be_free(AllNew)),break,fail.
must_be_nonvar(AllNew):- nonvar_or_ci(AllNew),!.
must_be_nonvar(AllNew):- arcST,fbug(must_be_nonvar(AllNew)),break,fail.

my_len(X,Y):- var(X),!,length(X,Y).
my_len(X,Y):- is_list(X),!,length(X,Y).
my_len(X,Y):- functor([_|_],F,A),functor(X,F,A),!,length(X,Y).
my_len(X,Y):- arcST,!,ibreak.
*/

%!  is_map(+G) is nondet.
%
%   Succeeds if `G` is a VM map. Uses `is_vm_map/1` for the check.
%
%   @arg G  The term to check.
%
%   Example:
%   ?- is_map(my_map).
%
% Check if G is a VM map.
is_map(G) :- is_vm_map(G), !.

%arc_webui:- false.

%!  sort_safe(+I, -O) is det.
%
%   Sorts the list `I` into `O`. If sorting fails due to an exception,
%   `O` is unified with `I` as a fallback.
%
%   @arg I  The input list to be sorted.
%   @arg O  The sorted output list or the original input on failure.
%
%   Example:
%   ?- sort_safe([3,1,2], Sorted).
%
% Safely sort a list, falling back to the original list if sorting fails.
sort_safe(I, O) :- catch(sort(I, O), _, I = O).

%!  my_append(+A, +B) is det.
%
%   Appends two lists `A` and `B`.
%
%   @arg A  The first list.
%   @arg B  The second list.
%
%   Example:
%   ?- my_append([1, 2], [3, 4], Result).
%
% Append two lists.
my_append(A, B) :- append(A, B).

%!  my_append(+A, +B, -C) is det.
%
%   Appends two lists `A` and `B` to produce a new list `C`.
%
%   @arg A  The first list.
%   @arg B  The second list.
%   @arg C  The resulting list after appending `A` and `B`.
%
%   Example:
%   ?- my_append([1, 2], [3, 4], Result).
%
% Append two lists to produce a third list.
my_append(A, B, C) :- append(A, B, C).

%!  with_tty_false(:Goal) is det.
%
%   Executes `Goal` with the `tty(false)` stream setting.
%
%   @arg Goal  The goal to be executed.
%
%   Example:
%   ?- with_tty_false(write('Hello, World!')).
%
% Execute a goal with the tty(false) stream setting.
with_tty_false(Goal) :- with_set_stream(current_output, tty(false), Goal).

%!  with_tty_true(:Goal) is det.
%
%   Executes `Goal` with the `tty(true)` stream setting.
%
%   @arg Goal  The goal to be executed.
%
%   Example:
%   ?- with_tty_true(write('Hello, World!')).
%
% Execute a goal with the tty(true) stream setting.
with_tty_true(Goal) :- with_set_stream(current_output, tty(true), Goal).

%!  count_of(+G, -N) is det.
%
%   Counts the occurrences of `G` and stores the result in `N`.
%
%   @arg G  The goal whose occurrences are to be counted.
%   @arg N  The number of occurrences found.
%
%   Example:
%   ?- count_of(member(X, [1, 2, 2, 3]), N).
%
% Count occurrences of G and store the result in N.
count_of(G, N) :- findall_vset(G, G, S), length(S, N).

%!  findall_vset(+T, :G, -S) is det.
%
%   Finds all solutions of `G` and removes duplicates to create a set `S`.
%
%   @arg T  The template for solutions.
%   @arg G  The goal to find solutions for.
%   @arg S  The resulting set of unique solutions.
%
%   Example:
%   ?- findall_vset(X, member(X, [1, 2, 2, 3]), Set).
%
% Find all solutions of G and remove duplicates to create a set.
findall_vset(T, G, S) :- findall(T, G, L), variant_list_to_set(L, S).

%!  flatten_objects(+Objs, -ObjsO) is det.
%
%   Flattens a list of objects `Objs` into a single list `ObjsO`.
%
%   @arg Objs   The nested list of objects to flatten.
%   @arg ObjsO  The resulting flattened list.
%
%   Example:
%   ?- flatten_objects([[1, 2], [3, [4]]], Flat).
%
% Flatten a list of objects.
flatten_objects(Objs, ObjsO) :- flatten([Objs], ObjsO), !.

%!  var_e(+E, +S) is nondet.
%
%   Succeeds if the term `E` is equivalent to `S` either by direct equality or structural equivalence.
%   Handles both non-variable terms and attributed variables.
%
%   @arg E  The first term to compare.
%   @arg S  The second term to compare.
%
%   Example:
%   ?- var_e(X, X).
%
% Succeed if E and S are identical.
var_e(E, S) :- E == S, !.
% Succeed if E is non-variable or an attributed variable and structurally equivalent to S.
var_e(E, S) :- (nonvar(E); attvar(E)), !, E =@= S.

%!  variant_list_to_set(+List, -Out) is det.
%
%   Converts a list into a set by removing structurally equivalent duplicates.
%
%   @arg List  The input list with possible duplicates.
%   @arg Out   The output list with duplicates removed.
%
%   Example:
%   ?- variant_list_to_set([a, b, a, c], Set).
%
% If E and S are structurally equivalent, skip S and continue with the rest of the list.
variant_list_to_set([E | List], Out) :-
  select(S, List, Rest), var_e(E, S), !,
  variant_list_to_set([E | Rest], Out).
% Include E in the output and continue processing the list.
variant_list_to_set([E | List], [E | Out]) :- !,
  variant_list_to_set(List, Out).
% If the input and output lists are identical, stop.
variant_list_to_set(H, H).

%!  nb_subst(:Obj, +New, +Old) is det.
%
%   Substitutes occurrences of `Old` with `New` in `Obj` non-backtrackably.
%   Uses `nb_setarg/3` to perform non-backtrackable assignment.
%
%   @arg Obj  The object in which substitutions are performed.
%   @arg New  The term to substitute in place of `Old`.
%   @arg Old  The term to be replaced.
%
%   Example:
%   ?- nb_subst(my_obj, new_value, old_value).
%
% Perform non-backtrackable substitution if Found is structurally equivalent to Old.
nb_subst(Obj, New, Old) :-
  get_setarg_p1(nb_setarg, Found, Obj, P1), Found =@= Old,
  p1_call(P1, New), !,
  nb_subst(Obj, New, Old).
% Stop if no further substitutions are needed.
nb_subst(_Obj, _New, _Old).

%!  system:any_arc_files(+Some) is nondet.
%
%   Checks if the input `Some` is a list of files containing 'arc' in their names.
%   If `Some` is a list, it recursively checks each element.
%
%   @arg Some  The atom or list of atoms to check.
%
%   Example:
%   ?- system:any_arc_files(['file1.arc', 'file2.txt']).
%

% Check if Some is a list and contains non-empty elements with 'arc' in their names.
system:any_arc_files(Some) :-
    is_list(Some), !,
    Some \== [],
    maplist(any_arc_files, Some).
% Check if a single atom contains the substring 'arc'.
system:any_arc_files(Some) :- atom_contains(Some, 'arc').

% Declare in_memo_cached/5 as thread-local to ensure thread-specific storage.
:- thread_local(in_memo_cached/5).

% Declare prolog:make_hook/2 as multifile and dynamic to allow redefinition.
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%!  prolog:make_hook(+Stage, +Some) is nondet.
%
%   A hook that is called before certain operations if the input `Some` contains
%   files related to 'arc'. It clears all caches if any such files are involved.
%
%   @arg Stage  The stage at which the hook is invoked (e.g., `before`).
%   @arg Some   The input to check for 'arc'-related files.
%
%   Example:
%   ?- prolog:make_hook(before, 'file.arc').
%
% Hook to clear all caches if the input contains 'arc'-related files.
prolog:make_hook(before, Some) :-
    any_arc_files(Some),
    forall(muarc:clear_all_caches, true).

% Declare muarc:clear_all_caches/0 as multifile and dynamic to allow redefinition.
:- multifile(muarc:clear_all_caches/0).
:- dynamic(muarc:clear_all_caches/0).

%!  muarc:clear_all_caches is det.
%
%   Clears all memoization caches unless extreme caching is enabled.
%   Uses `retractall/1` to remove all cached entries.
%
%   Example:
%   ?- muarc:clear_all_caches.
%
% Clear all caches unless extreme caching is enabled.
muarc:clear_all_caches :-
    \+ luser_getval(extreme_caching, true),
    retractall(in_memo_cached(_, _, _, _, _)),
    fail.

%!  arc_memoized(+G) is det.
%
%   Memoizes the result of calling the goal `G`. If `G` is already memoized, it retrieves the cached result.
%   Otherwise, it computes the result, caches it, and ensures that the cache is cleaned up in case of errors.
%
%   This predicate handles memoization by:
%   - Copying terms for consistent caching.
%   - Using `in_memo_cached/5` to store cached results.
%   - Preventing concurrent memoization for the same goal using `throw/1`.
%   - Cleaning up cached data on exceptions using `setup_call_cleanup/3`.
%
%   @arg G  The goal to memoize. It must be ground (fully instantiated) and a compound term.
%
%   Example:
%   ?- arc_memoized(my_goal(42)).
%
%arc_memoized(G):- !, call(G).
% If G is a ground compound term with an arity-1 functor, memoize it by reducing it to a simpler form.
arc_memoized(G) :-
    compound(G), ground(G), functor(G, F, 1),functor(C, F, 1), !,arc_memoized(C),G = C, !.
% Memoize a ground goal G by copying it and using in_memo_cached to store and retrieve results.
arc_memoized(G) :-
    copy_term(G, C, GT),(Key = (C + GT)),
    % Check if memoization is already in progress; if so, throw an error to avoid concurrent memoization.
    (in_memo_cached(Key, C, track, started, Info)
        -> throw(already_memoizing(in_memo_cached(Key, C, track, started, Info)))
        ; true),
    % Number variables in Key to make it unique and ready for caching.
    numbervars(Key, 0, _, [attvar(bind), singletons(true)]), !,
    % Use setup_call_cleanup to ensure cache cleanup if any error occurs.
    setup_call_cleanup(
        (asserta(in_memo_cached(Key, C, track, started, _), Started)),
        catch(
            % If the goal is cached, retrieve the result and apply any attached goals.
            (in_memo_cached(Key, C, GT, Found, AttGoals)
                *-> (G = Found, maplist(call, AttGoals))
                % Otherwise, compute the goal, cache it, or store it as failed if computation fails.
                ; ((call(G), copy_term(G, CG, GG))
                    *-> asserta(in_memo_cached(Key, C, GT, CG, GG))
                    ; asserta(in_memo_cached(Key, C, GT, failed, _)))),
            % On exception, remove the cache and rethrow the error.
            E, (retractall(in_memo_cached(Key, C, GT, _, _)), throw(E))
        ),
        % Cleanup the started assertion when done.
        erase(Started)
    ).

%!  set_nth1(+Index, +List, +Element, -ModifiedList) is det.
%
%   Replaces the element at the given 1-based index in a list with a new element.
%
%   @arg Index The 1-based position of the element to be replaced.
%   @arg List The input list in which the replacement will be made.
%   @arg Element The new element to place at the specified index.
%   @arg ModifiedList The list after the replacement has been made.
%
%   @example
%     ?- set_nth1(2, [a, b, c], x, Result).
%     Result = [a, x, c].
%
% Base case: When the index is 1, replace the head element.
set_nth1(1, [_|Row], E, [E|Row]) :- !.
% Recursive case: Decrement N and continue processing the tail.
set_nth1(N, [W|Row], E, [W|RowMod]) :- Nm1 is N - 1,set_nth1(Nm1, Row, E, RowMod).

%!  findall_count(+Template, +Goal, -Count) is det.
%
%   Counts the number of unique solutions for a given Goal.
%
%   @arg Template The template for the solutions.
%   @arg Goal The goal to find solutions for.
%   @arg Count The number of unique solutions.
%
%   @example
%     ?- findall_count(X, member(X, [1,2,2,3]), Count).
%     Count = 3.
%
% Collect unique solutions as a set and count the elements.
findall_count(T, G, N) :- findall_set(T, G, S),length(S, N).

%!  findall_set(+Template, +Goal, -Set) is det.
%
%   Collects all solutions to the Goal and removes duplicates to return a set.
%
%   @arg Template The template for the solutions.
%   @arg Goal The goal to find solutions for.
%   @arg Set A list of unique solutions.
%
%   @example
%     ?- findall_set(X, member(X, [1,2,2,3]), Set).
%     Set = [1, 2, 3].
%
% Find all solutions and convert them to a set to remove duplicates.
findall_set(T, G, S) :- findall(T, G, L),list_to_set(L, S).

%!  make_list_inited(+Length, +Element, -List) is det.
%
%   Creates a list with the specified length, initialized with a given element.
%
%   @arg Length The length of the resulting list.
%   @arg Element The element to initialize each position in the list with.
%   @arg List The resulting initialized list.
%
%   @example
%     ?- make_list_inited(3, 0, List).
%     List = [0, 0, 0].
%
% Base case: When length is 0, return an empty list.
make_list_inited(0, _, []) :- !.
% Base case: When length is 1, return a singleton list with the element.
make_list_inited(1, E, [E]) :- !.
% Recursive case: Decrement N and prepend the element.
make_list_inited(N, E, [E|List]) :- Nm1 is N - 1,make_list_inited(Nm1, E, List).

%!  nth_fact(+Predicate, +Index) is nondet.
%
%   Retrieves the clause reference of the nth clause for the given predicate.
%
%   @arg Predicate The predicate whose clause is being accessed.
%   @arg Index The 1-based index of the clause.
%
%   @example
%     % If foo/0 has three clauses, this query retrieves the second clause:
%     ?- nth_fact(foo, 2).
%
% Retrieve the clause reference and unify it with the index.
nth_fact(P, I) :- clause(P, true, Ref),nth_clause(P, I, Ref).

%!  nonvar_or_ci(+Term) is nondet.
%
%   Succeeds if the given term is either non-variable or an attributed variable.
%
%   @arg Term The term to check.
%
%   @example
%     ?- nonvar_or_ci(X).
%     false.
%
%     ?- nonvar_or_ci(1).
%     true.
%
% Succeed if the term is either non-variable or attributed variable.
nonvar_or_ci(C) :- (nonvar(C); attvar(C)), !.

%!  add_i(+Info) is det.
%
%   Adds the given information to two rule sets: `test_rules` and `pair_rules`.
%   This predicate uses the `nb_set_add/2` to efficiently update non-backtrackable sets.
%
%   @arg Info The information to be added.
%
%   @example
%     ?- add_i(foo).
%
% Tersify the info and add it to the rule sets.
add_i(Info) :-
    quietly((
        tersify(Info, InfoT),
        luser_getval(test_rules, TRules),
        luser_getval(pair_rules, PRules),
        nb_set_add(TRules, InfoT),
        nb_set_add(PRules, InfoT),
        nop(pp(cyan, +InfoT))
    )).

%!  add_i(+Functor, +Info) is det.
%
%   Adds a term constructed with the functor and info to the rule sets.
%
%   @arg Functor The functor to use in constructing the term.
%   @arg Info The information to be added.
%
%   @example
%     ?- add_i(foo, bar).
%
% Construct a new term and add it to the rule sets.
add_i(F, Info) :- append_term(i(F), Info, FInfo),add_i(FInfo).

%!  add_rule(+Info) is det.
%
%   Adds the given information as a rule to the rule set.
%
%   @arg Info The information to be added as a rule.
%
%   @example
%     ?- add_rule(foo).
%
% Add the information as a rule.
add_rule(Info) :- add_i(rule, Info).

%!  add_cond(+Info) is det.
%
%   Adds the given information as a condition to the rule set.
%
%   @arg Info The information to be added as a condition.
%
%   @example
%     ?- add_cond(bar).
%
% Add the information as a condition.
add_cond(Info) :- add_i(cond, Info).

%do_action(Info):- guess_pretty(Info),add_i(action,Info),call(Info).

%!  do_action(+Call) is det.
%
%   Executes the given call and records it as an action.
%
%   @arg Call The goal to be executed and recorded as an action.
%
%   @example
%     ?- do_action(write('Hello')).
%
% Copy the call, execute it, and record it as an action.
do_action(Call) :- !, copy_term(Call, Info), call(Call), add_i(action, Info).

%!  add_action(+Info) is det.
%
%   Adds the given information as an action to the rule set.
%
%   @arg Info The information to be added as an action.
%
%   @example
%     ?- add_action(foo).
%
% Add the information as an action.
add_action(Info) :- add_i(action, Info).

%!  add_note(+Info) is det.
%
%   Adds the given information as a note to the rule set.
%
%   @arg Info The information to be added as a note.
%
%   @example
%     ?- add_note('This is a note').
%
% Add the information as a note.
add_note(Info) :- add_i(note, Info).

%!  add_indiv(+W, +Info) is det.
%
%   Adds the given information as an individual entry.
%
%   @arg W The identifier for the individual entry.
%   @arg Info The information to associate with the individual entry.
%
%   @example
%     ?- add_indiv(john, 'is a teacher').
%
% Add the information as an individual entry.
add_indiv(W, Info) :- add_i(indiv(W), Info).

%!  add_comparitor(+Info) is det.
%
%   Adds the given information as a comparator to the rule set.
%
%   @arg Info The information to be added as a comparator.
%
%   @example
%     ?- add_comparitor('greater than').
%
% Add the information as a comparator.
add_comparitor(Info) :- add_i(comparitor, Info).

%!  show_rules is det.
%
%   Displays the current rules stored in `pair_rules` and `test_rules`.
%
%   The `pair_rules` are shown in cyan, and the `test_rules` are shown in blue.
%
%   @example
%     ?- show_rules.
%
% Display the current rules from both rule sets.
show_rules :-
    luser_getval(pair_rules, PRules), maplist(pp(cyan), PRules),
    luser_getval(test_rules, TRules), maplist(pp(blue), TRules), !.

%!  sub_atom_value(+TestID, -A) is nondet.
%
%   Extracts atomic or string sub-terms from the given `TestID`.
%
%   This predicate finds sub-terms of `TestID` and unifies them with `A` if
%   they are either atoms or strings.
%
%   @arg TestID The term to be searched for sub-terms.
%   @arg A      A sub-term of `TestID` that is either an atom or a string.
%
sub_atom_value(TestID, A) :- sub_term_safely(A, TestID),(atom(A) ; string(A)).

%!  my_list_to_set(+List, -Set) is det.
%
%   Converts a list to a set by removing duplicates using equality `(=)/2`.
%
%   @arg List The input list to be converted.
%   @arg Set  The resulting set with duplicates removed.
%
my_list_to_set(List, Set) :- my_list_to_set(List, (=), Set).

%!  my_list_to_set_variant(+List, -Set) is det.
%
%   Converts a list to a set using term equality `(=@=)/2`.
%
%   @arg List The input list to be converted.
%   @arg Set  The resulting set with duplicates removed.
%
my_list_to_set_variant(List, Set) :- my_list_to_set(List, (=@=), Set).

%!  my_list_to_set_cmp(+List, -Set) is det.
%
%   Converts a list to a set using custom term comparison `(=@=)/2`.
%
%   @arg List The input list to be converted.
%   @arg Set  The resulting set with duplicates removed.
%
my_list_to_set_cmp(List, Set) :- my_list_to_set(List, (=@=), Set).

%!  my_list_to_set(+List, +P2, -Set) is det.
%
%   Internal predicate to remove duplicates from a list using a predicate `P2`.
%
%   @arg List The input list.
%   @arg P2   The predicate used for comparison.
%   @arg Set  The output list without duplicates.
%
my_list_to_set([E|List], P2, Set) :- select(C, List, Rest),p2_call(P2, E, C),!,my_list_to_set([E|Rest], P2, Set).
my_list_to_set([E|List], P2, [E|Set]) :- !, my_list_to_set(List, P2, Set).
my_list_to_set([], _, []).

%!  my_list_to_set_cmp(+List, +C3, -Set) is det.
%
%   Internal predicate to remove duplicates using a comparison predicate `C3`.
%
%   @arg List The input list.
%   @arg C3   The comparison predicate.
%   @arg Set  The resulting list with duplicates removed.
%
my_list_to_set_cmp([E|List], C3, Set) :- select(C, List, Rest),call(C3, R, E, C),R == (=),
    my_list_to_set_cmp([C|Rest], C3, Set),!.
my_list_to_set_cmp([E|List], C3, [E|Set]) :- !, my_list_to_set_cmp(List, C3, Set).
my_list_to_set_cmp([], _, []).

%!  contains_nonvar(+N, +Info) is nondet.
%
%   Checks if the given `Info` term contains a sub-term that is non-variable
%   and matches `N`.
%
%   @arg N    The value to match with non-variable sub-terms of `Info`.
%   @arg Info The term to be searched for matching sub-terms.
%
contains_nonvar(N, Info) :- sub_term_safely(E, Info),nonvar_or_ci(E),E = N,!.

%!  max_min(+A, +B, -C, -D) is det.
%
%   Computes the maximum and minimum values between `A` and `B`.
%
%   @arg A The first input number.
%   @arg B The second input number.
%   @arg C The maximum value.
%   @arg D The minimum value.
%
max_min(A, B, C, D) :- must_be_free(C),must_be_free(D),max_min0(A, B, C, D).

%!  max_min0(+A, +B, -C, -D) is det.
%
%   Helper predicate to determine maximum and minimum values.
%
max_min0(A, B, B, B) :- plain_var(A).
max_min0(A, B, A, A) :- plain_var(B),!.
max_min0(A, B, C, D) :- number(A),number(B),!,((A > B) -> (C = A, D = B) ; (C = B, D = A)).
max_min0(_, A, A, A) :- number(A),!.
max_min0(A, _, A, A) :- number(A),!.
max_min0(_, _, _, _).

%!  as_debug(+L, +C, +G) is det.
%
%   Executes a goal `G` with debugging information, conditional on `C`.
%
%   @arg L Debug level, used to conditionally enable debugging.
%   @arg C Condition to check before executing the goal.
%   @arg G The goal to execute with debugging.
%
as_debug(L, G) :- as_debug(L, true, G).

%!  as_debug(+L, +C, +G) is det.
%
%   Executes a goal `G` with debugging information if `L` is not 9.
%
as_debug(9, _, _) :- !.
as_debug(_, C, G) :- ignore(catch((call(C) -> wots(S, G), format('~NDEBUG: ~w~N', [S]); true), _, true)).

%!  shall_count_as_same(+A, +B) is nondet.
%
%   Determines if two terms `A` and `B` should be considered the same.
%
%   This predicate compares two terms for equivalence based on multiple criteria,
%   including unification, variable checks, atomic checks, and structural equality.
%   It aims to identify whether the two terms can be treated as the same by
%   performing a series of conditional checks.
%
%   @arg A The first term to compare.
%   @arg B The second term to compare.
%
shall_count_as_same(A, B) :- same_term(A, B),!. % unify ok_ok match
shall_count_as_same(A, B) :- plain_var(A),!,A == B.
shall_count_as_same(A, B) :- atomic(A),!, A =@= B.
shall_count_as_same(A, B) :- var(B),!, A =@= B.
shall_count_as_same(A, B) :- A =@= B,!.
shall_count_as_same(A, B) :- copy_term(B, CB),copy_term(A, CA),\+ \+ (A = B, B =@= CB, A =@= CA),!.
%shall_count_as_same(A,B):- \+ A \= B, !.

%!  count_each(+List, +Group, -Counts) is det.
%
%   Counts occurrences of each element in `List` within the given `Group`.
%   The result is a list of pairs, where each pair contains the length
%   of occurrences and the corresponding element.
%
%   @arg List   The list of elements to be counted.
%   @arg Group  The group in which to count occurrences.
%   @arg Counts A list of pairs [Length-Element] with the counts.
%
count_each([C|L], GC, [Len-C|LL]) :- include(shall_count_as_same(C), GC, Lst),length(Lst, Len),
    !, count_each(L, GC, LL).
count_each([], _, []).

%!  count_each_inv(+List, +Group, -Counts) is det.
%
%   Inverse counting of occurrences, producing [Element-Length] pairs.
%
%   @arg List   The list of elements to be counted.
%   @arg Group  The group in which to count occurrences.
%   @arg Counts A list of pairs [Element-Length] with the counts.
%
count_each_inv([C|L], GC, [C-Len|LL]) :- include(shall_count_as_same(C), GC, Lst),length(Lst, Len),
    count_each_inv(L, GC, LL).
count_each_inv([], _, []).

%!  maplist_n(+N, :Predicate, +List) is det.
%
%   Applies a predicate to each element in `List`, passing an index that
%   starts from `N` and increments with each step.
%
%   @arg N        The starting index.
%   @arg Predicate The predicate to be applied to each element.
%   @arg List     The list of elements to process.
%
maplist_n(N, P, [H1|T1]) :- p2_call(P, N, H1),N1 is N + 1,maplist_n(N1, P, T1).
maplist_n(_N, _P, []).

%!  maplist_n(+N, :Predicate, +List1, -List2) is det.
%
%   Applies a predicate to corresponding elements of two lists,
%   passing an index that starts from `N` and increments with each step.
%
%   @arg N        The starting index.
%   @arg Predicate The predicate to be applied to each pair of elements.
%   @arg List1    The input list.
%   @arg List2    The output list with transformed elements.
%
maplist_n(N, P, [H1|T1], [H2|T2]) :- call(P, N, H1, H2),N1 is N + 1,maplist_n(N1, P, T1, T2).
maplist_n(_N, _P, [], []).

/*
print_points_grid(Points):-
 points_range(Points, LoH, LoV, HiH, HiV, H, V), writeqln(size_range(LoH, LoV, HiH, HiV, H, V)), points_to_grid(Points, Grid), print_grid(Grid).

print_points_grid(Grid):-
 points_range(Grid, LoH, LoV, HiH, HiV, _H, _V), print_grid(Grid, LoH, LoV, HiH, HiV, Grid).
*/


%print_trainer:- kaggle_arc_train(Name, Stuff), atom_json_term(Stuff, JSON, []), print_arc(Name, JSON).
%print_evaler:- kaggle_arc_eval(Name, Stuff), atom_json_term(Stuff, JSON, []), print_arc(Name, JSON).

 /*
% data looks like

kaggle_arc_train('007bbfb7', trn, [[0, 7, 7], [7, 7, 7], [0, 7, 7]], [[0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [7, 7, 7, 7, 7, 7, 7, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7]]).
kaggle_arc_train('007bbfb7', trn, [[4,0, 4], [0,0,0], [0, 4,0]], [[4,0, 4,0,0,0, 4,0, 4], [0,0,0,0,0,0,0,0,0], [0, 4,0,0,0,0,0, 4,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 4,0, 4,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 4,0,0,0,0]]).
kaggle_arc_train('007bbfb7', trn, [[0,0,0], [0,0, 2], [2,0, 2]], [[0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 2], [0,0,0,0,0,0, 2,0, 2], [0,0,0,0,0,0,0,0,0], [0,0, 2,0,0,0,0,0, 2], [2,0, 2,0,0,0, 2,0, 2]]).
kaggle_arc_train('007bbfb7', trn, [[6, 6,0], [6,0,0], [0, 6, 6]], [[6, 6,0, 6, 6,0,0,0,0], [6,0,0, 6,0,0,0,0,0], [0, 6, 6,0, 6, 6,0,0,0], [6, 6,0,0,0,0,0,0,0], [6,0,0,0,0,0,0,0,0], [0, 6, 6,0,0,0,0,0,0], [0,0,0, 6, 6,0, 6, 6,0], [0,0,0, 6,0,0, 6,0,0], [0,0,0,0, 6, 6,0, 6, 6]]).
kaggle_arc_train('007bbfb7', trn, [[2, 2, 2], [0,0,0], [0, 2, 2]], [[2, 2, 2, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0, 2, 2,0, 2, 2,0, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 2, 2,0, 2, 2]]).
kaggle_arc_train('007bbfb7', tst, [[7,0, 7], [7,0, 7], [7, 7,0]], [[7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7, 7,0, 7,0,0,0], [7,0, 7, 7,0, 7,0,0,0], [7, 7,0, 7, 7,0,0,0,0]]).

kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3,0, 3,0,0], [0,0, 3,0, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]], [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3, 4, 3,0,0], [0,0, 3, 4, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0,0,0,0,0], [0,0,0, 3,0, 3,0,0,0,0], [0,0, 3,0,0,0, 3,0,0,0], [0,0,0,0,0, 3,0, 3,0,0], [0,0,0, 3,0, 3, 3,0,0,0], [0,0, 3, 3, 3,0,0,0,0,0], [0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0,0,0,0,0], [0,0,0, 3,0, 3,0,0,0,0], [0,0, 3,0,0,0, 3,0,0,0], [0,0,0,0,0, 3, 4, 3,0,0], [0,0,0, 3,0, 3, 3,0,0,0], [0,0, 3, 3, 3,0,0,0,0,0], [0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0,0,0], [0, 3, 3,0, 3, 3,0, 3,0,0], [3,0,0, 3,0,0, 3,0, 3,0], [0,0,0, 3,0,0, 3, 3,0,0], [0,0,0, 3,0,0, 3,0,0,0], [0,0,0, 3,0,0, 3,0,0,0], [0,0,0,0, 3, 3,0, 3,0,0], [0,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0,0,0], [0, 3, 3,0, 3, 3,0, 3,0,0], [3,0,0, 3, 4, 4, 3, 4, 3,0], [0,0,0, 3, 4, 4, 3, 3,0,0], [0,0,0, 3, 4, 4, 3,0,0,0], [0,0,0, 3, 4, 4, 3,0,0,0], [0,0,0,0, 3, 3,0, 3,0,0], [0,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0], [0,0, 3, 3, 3, 3,0,0,0,0], [0,0, 3,0,0, 3,0,0,0,0], [0,0, 3,0,0, 3,0, 3,0,0], [0,0, 3, 3, 3, 3, 3, 3, 3,0], [0,0,0, 3,0,0,0,0, 3,0], [0,0,0, 3,0,0,0, 3, 3,0], [0,0,0, 3, 3,0,0, 3,0, 3], [0,0,0, 3,0, 3,0,0, 3,0], [0,0,0,0, 3,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0], [0,0, 3, 3, 3, 3,0,0,0,0], [0,0, 3, 4, 4, 3,0,0,0,0], [0,0, 3, 4, 4, 3,0, 3,0,0], [0,0, 3, 3, 3, 3, 3, 3, 3,0], [0,0,0, 3,0,0,0,0, 3,0], [0,0,0, 3,0,0,0, 3, 3,0], [0,0,0, 3, 3,0,0, 3, 4, 3], [0,0,0, 3, 4, 3,0,0, 3,0], [0,0,0,0, 3,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0, 3,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0, 3,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0, 3, 3,0,0, 3,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0, 3, 3,0,0, 3,0,0, 3,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 4, 3, 3,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 3,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0, 3,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0, 3,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0, 3, 3, 4, 4, 3,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 3, 3,0,0, 3,0,0, 3,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0, 3, 4, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', tst, [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0, 3,0, 3, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0, 3,0,0,0,0, 3,0,0, 3,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 3,0, 3, 3, 3,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3,0,0,0, 3,0,0], [0,0,0,0,0,0, 3, 3,0, 3,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0, 3,0,0,0,0,0, 3, 3,0,0,0,0,0,0,0,0,0,0], [0, 3,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0, 3, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0, 3, 4, 3, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 4, 4, 4, 4, 3, 4, 4, 3,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 3,0, 3, 3, 3,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0, 3, 3, 4, 3,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0, 3,0,0,0,0,0, 3, 3,0,0,0,0,0,0,0,0,0,0], [0, 3, 4, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0, 3, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
*/
%tell(s), ignore((nl, nl, task_pairs(Name, ExampleNum, In, Out), format('~N~q.~n', [test_pairs_cache(Name, ExampleNum, In, Out)]), fail)), told.
%!  map_pred(:Pred, +P, -X) is det.
%
%   Applies the given predicate `Pred` to `P` and produces `X`.
%   If `Pred` cannot be applied directly, it attempts a more complex transformation.
%
%   @arg Pred The predicate to be applied.
%   @arg P    The input term.
%   @arg X    The resulting transformed term.
%
map_pred(Pred, P, X) :- map_pred([], Pred, P, X).

%map_pred(NoCycles,_Pred, P, X) :- member(E,NoCycles), E==P,!, X = P.

%!  map_pred(+NoCycles, :Pred, +P, -X) is nondet.
%
%   Tries to apply `Pred` to `P` directly. If it fails, it falls back to `map_pred0`.
%
%   @arg NoCycles A list to detect cyclic references.
%   @arg Pred     The predicate to be applied.
%   @arg P        The input term.
%   @arg X        The resulting transformed term.
%
map_pred(NoCycles, Pred, P, X) :- p2_call(Pred, P, X) *-> true ; map_pred0(NoCycles, Pred, P, X).

%!  map_pred1(:Pred, +P, -P1) is det.
%
%   Wrapper that applies `Pred` to `P`, returning the transformed result in `P1`.
%
%   @arg Pred The predicate to apply.
%   @arg P    The input term.
%   @arg P1   The transformed term.
%
map_pred1(Pred, P, P1) :- map_pred1(P, Pred, P, P1).

%!  map_pred0(+NoCycles, :Pred, +Args, -ArgSO) is det.
%
%   Handles edge cases where `Args` is empty or applies `Pred` recursively on compound terms.
%
%   @arg NoCycles A list to detect cyclic references.
%   @arg Pred     The predicate to apply.
%   @arg Args     The input term (or list of terms).
%   @arg ArgSO    The output term after transformation.
%
map_pred0(_NoCycles, _Pred, Args, ArgSO) :- must_be_free(ArgSO), Args == [], !, ArgSO = [].
map_pred0(_NoCycles, Pred, P, P1) :- p2_call(Pred, P, P1), !. % *->true;fail.
map_pred0(NoCycles, Pred, P, X) :-
    fail, attvar(P), !,
    %duplicate_term(P,X),P=X,
    get_attrs(P, VS),map_pred([P | NoCycles], Pred, VS, VSX),P = X,put_attrs(X, VSX), !.
map_pred0(NoCycles, Pred, P, X) :- map_pred1(NoCycles, Pred, P, X).

% map_pred0(NoCycles,Pred, Args, ArgSO) :- is_list(Args), !, maplist(map_pred([Args|NoCycles],Pred), Args, ArgS), ArgS=ArgSO.

%!  map_pred1(+NoCycles, :Pred, +P, -P1) is det.
%
%   Applies `Pred` recursively to compound or list terms.
%
%   @arg NoCycles A list to detect cyclic references.
%   @arg Pred     The predicate to apply.
%   @arg P        The input term.
%   @arg P1       The output term after transformation.
%
map_pred1(_NoCycles, _Pred, P, P1) :- (\+ compound(P) ; is_ftVar(P)), !, must_det_ll(P1 = P), !.
map_pred1(NoCycles, Pred, IO, OO) :- is_list(IO),!,maplist(map_pred(NoCycles, Pred), IO, OO).
map_pred1(NoCycles, Pred, IO, [O | ArgS]) :- IO = [I | Args],!,
    map_pred([IO, ArgS | NoCycles], Pred, I, O),map_pred0([IO, I | NoCycles], Pred, Args, ArgS).
map_pred1(NoCycles, Pred, P, P1) :- compound_name_arguments(P, F, Args),
    maplist(map_pred([P | NoCycles], Pred), Args, ArgS),compound_name_arguments(P1, F, ArgS).
%map_pred(_Pred, P, P).

/*
:- meta_predicate map_pred(2, ?, ?, ?, ?).
map_pred(Pred, P, X, Sk, P1) :- must_be_free(X), p2_call(Pred, P, X), !, must(Sk=P1), !.
map_pred(_Pred, P, _, _, P1) :- is_ftVar(P), !, must(P1=P), !.
map_pred(Pred, [P|Args], X, Sk, [P1|ArgS]) :- !, map_pred(Pred, P, X, Sk, P1), !, must(map_pred(Pred, Args, X, Sk, ArgS)), !.
map_pred(Pred, P, X, Sk, P1) :- compound(P), !, compound_name_arguments(P, F, Args), map_pred(Pred, [F|Args], X, Sk, [Fs|ArgS]), !, compound_name_arguments(P1, Fs, ArgS), !.
map_pred(_Pred, P, _, _, P).
*/

%!  is_cons(+A) is nondet.
%
%   Checks if the given term `A` is a compound term representing a non-empty list.
%
%   This predicate succeeds if `A` is a compound term with the structure of a list.
%
%   @arg A The term to be checked. It can be any valid Prolog term.
%
%   @example
%     ?- is_cons([1, 2, 3]).
%     true.
%
is_cons(A) :- compound(A), A = [_|_].

%!  into_grid_or_var(+O, -G) is det.
%
%   Converts the given term `O` into a grid or keeps it as a variable, depending on the input type.
%
%   - If `G` is a non-empty list, it is returned as-is.
%   - If `G` is an uninstantiated variable, it remains unchanged.
%   - If `O` is neither a list nor a variable, it is cast to a grid using `cast_to_grid/3`.
%
%   @arg O The input term to normalize into a grid or keep as a variable.
%   @arg G The resulting grid or variable.
%
%   @example
%     ?- into_grid_or_var([1, 2, 3], G).
%     G = [1, 2, 3].
%
% If G is a non-empty list, return it unchanged.
into_grid_or_var(G, G) :- is_cons(G), !.
% If G is a variable, return it unchanged.
into_grid_or_var(G, G) :- var(G), !.
% Attempt to cast O to a grid if it is neither a list nor a variable.
into_grid_or_var(O, G) :- cast_to_grid(O, G, _Uncast), !.

%!  maybe_mapgrid(+P, +I, -O) is det.
%
%   Applies the predicate `P` to the grid `I` to produce the output `O`.
%   If `I` is recognized as a grid, it uses `mapgrid/3` to perform the operation.
%
%   @arg P The predicate to apply over the grid.
%   @arg I The input grid.
%   @arg O The output after applying the predicate.
%
%   @example
%     ?- maybe_mapgrid(pred, [[1, 2], [3, 4]], O).
%     O = [[Result1, Result2], [Result3, Result4]].
%
% If I is a grid, apply the predicate using mapgrid/3.
maybe_mapgrid(P2, I, O) :- is_grid(I), !, mapgrid(P2, I, O).

%!  maybe_mapgrid(+P, +I, -O, +M) is det.
%
%   Applies the predicate `P` over the grid `I` with an additional parameter `M`.
%   If `I` is recognized as a grid, it uses `mapgrid/4` to perform the operation.
%
%   @arg P The predicate to apply over the grid.
%   @arg I The input grid.
%   @arg O The output after applying the predicate.
%   @arg M An additional parameter for the predicate.
%
%   @example
%     ?- maybe_mapgrid(pred, [[1, 2], [3, 4]], O, ExtraParam).
%     O = [[Result1, Result2], [Result3, Result4]].
%
% If I is a grid, apply the predicate using mapgrid/4.
maybe_mapgrid(P3, I, O, M) :- is_grid(I), !, mapgrid(P3, I, O, M).

%!  maybe_mapgrid(+P, +I, -O, +M, +N) is det.
%
%   Applies the predicate `P` over the grid `I` with two additional parameters `M` and `N`.
%   If `I` is recognized as a grid, it uses `mapgrid/5` to perform the operation.
%
%   @arg P The predicate to apply over the grid.
%   @arg I The input grid.
%   @arg O The output after applying the predicate.
%   @arg M, N Additional parameters for the predicate.
%
%   @example
%     ?- maybe_mapgrid(pred, [[1, 2], [3, 4]], O, Extra1, Extra2).
%     O = [[Result1, Result2], [Result3, Result4]].
%
% If I is a grid, apply the predicate using mapgrid/5.
maybe_mapgrid(P4, I, O, M, N) :- is_grid(I), !, mapgrid(P4, I, O, M, N).

%!  mapgrid(+P4, +Grid, +GridM, +GridN, +GridO) is det.
%
%   Applies the predicate `P4` over four grids by first ensuring they are correctly formatted as grids or variables.
%
%   @arg P4 The predicate to apply over the grids.
%   @arg Grid, GridM, GridN, GridO The input and output grids.
%
%   @example
%     ?- mapgrid(pred, [[1]], [[2]], [[3]], [[4]]).
%     true.
%
% Ensure all inputs are normalized to grids and apply `P4` with `mapg_list/5`.
mapgrid(P4, Grid, GridM, GridN, GridO) :- into_grid_or_var(Grid, G1),into_grid_or_var(GridM, G2),
    into_grid_or_var(GridN, G3),into_grid_or_var(GridO, G4),mapg_list(P4, G1, G2, G3, G4).
% Apply P4 recursively if the input is a list of grids.
mapg_list(P4, Grid, GridM, GridN, GridO) :- is_list(Grid),!,maplist(mapg_list(P4), Grid, GridM, GridN, GridO).
% Call P4 on the grids when they are not lists.
mapg_list(P4, Grid, GridM, GridN, GridO) :- call(P4, Grid, GridM, GridN, GridO), !.

%!  mapgrid(+P3, +Grid, +GridN, +GridO) is det.
%
%   Applies the predicate `P3` over three grids after normalization.
%
%   @arg P3 The predicate to apply over the grids.
%   @arg Grid, GridN, GridO The input and output grids.
%
% Normalize inputs to grids and call `mapg_list/4`.
mapgrid(P3, Grid, GridN, GridO) :- into_grid_or_var(Grid, G1),into_grid_or_var(GridN, G2),into_grid_or_var(GridO, G3),
    mapg_list(P3, G1, G2, G3).
% Apply P3 recursively if the input is a list of grids.
mapg_list(P3, Grid, GridN, GridO) :- is_list(Grid),!,maplist(mapg_list(P3), Grid, GridN, GridO).
% Call P3 on the grids when they are not lists.
mapg_list(P3, Grid, GridN, GridO) :- call(P3, Grid, GridN, GridO), !.

%!  mapgrid(+P2, +Grid, +GridN) is det.
%
%   Applies the predicate `P2` over two grids after normalization.
%
%   @arg P2 The predicate to apply over the grids.
%   @arg Grid, GridN The input and output grids.
%
% Normalize inputs to grids and call `mapg_list/3`.
mapgrid(P2, Grid, GridN) :- into_grid_or_var(Grid, G1),into_grid_or_var(GridN, G2),!,mapg_list(P2, G1, G2).
% Apply P2 recursively if the input is a list of grids.
mapg_list(P2, Grid, GridN) :- is_list(Grid),!,maplist(mapg_list(P2), Grid, GridN).
% Call P2 on the grids when they are not lists.
mapg_list(P2, Grid, GridN) :- p2_call(P2, Grid, GridN), !.

%!  mapgrid(+P1, +Grid) is det.
%
%   Applies the predicate `P1` to a single grid.
%
%   @arg P1 The predicate to apply over the grid.
%   @arg Grid The input grid.
%
% Normalize the grid and call `mapg_list/2`.
mapgrid(P1, Grid) :- into_grid_or_var(Grid, G1),mapg_list(P1, G1).
% Apply P1 recursively if the input is a list of grids.
mapg_list(P1, Grid) :- is_list(Grid),!,maplist(mapg_list(P1), Grid).
% Call P1 on the grid when it is not a list.
mapg_list(P1, Grid) :- p1_call(P1, Grid), !.

%!  maplist_ignore(+P3, +H, +I, +J) is det.
%
%   Applies the predicate `P3` over three lists, handling cases where the lists may be empty.
%   Uses `ignore/1` to safely handle operations that could fail or raise exceptions.
%
%   @arg P3 The predicate to apply over the elements.
%   @arg H, I, J Input lists for processing.
%
% If any list is empty, ensure all are treated as empty.
maplist_ignore(_3, H, I, J) :- (H == [] ; I == [] , J == []), !, (ignore(H = []), ignore(I = []), ignore(J = [])).
% If H is not a list, call P3 and ignore failures.
maplist_ignore(P3, H, I, J) :- \+ is_list(H), !, ignore(p2_call(call(P3, H), I, J)).
% Process nested lists recursively.
maplist_ignore(P3, [H | Grid], [I | GridN], [J | GridO]) :- maplist_ignore(P3, H, I, J), !, maplist_ignore(P3, Grid, GridN, GridO).

%!  maplist_ignore(+P2, +H, +I) is det.
%
%   Applies the predicate `P2` over two lists, handling cases where the lists may be empty.
%   Uses `ignore/1` to ensure operations proceed safely without errors.
%
%   @arg P2 The predicate to apply over the elements.
%   @arg H, I Input lists for processing.
%
% If any list is empty, ensure both are treated as empty.
maplist_ignore(_2, H, I) :- (H == [] ; I == []), !, (ignore(H = []), ignore(I = [])).
% If H is not a list, call P2 and ignore failures.
maplist_ignore(P2, H, I) :- \+ is_list(H), !, ignore(p2_call(P2, H, I)).
% Process nested lists recursively.
maplist_ignore(P2, [H | Grid], [I | GridN]) :- maplist_ignore(P2, H, I), !, maplist_ignore(P2, Grid, GridN).

%p1_or(P1,Q1,E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).

%!  p1_call(+Goal, +E) is det.
%
%   Executes a callable predicate `Goal` with the argument `E`.
%   This predicate handles various logical operators like conjunction, disjunction, negation, and control constructs such as `once/1` and `ignore/1`.
%
%   @arg Goal The predicate or control structure to execute.
%   @arg E The argument passed to the predicate.
%
% Handle disjunction (P1;Q1).
p1_call((P1;Q1), E) :- must_be(callable, P1), !, (p1_call(P1, E); p1_call(Q1, E)).
% Handle conjunction (P1,Q1).
p1_call((P1, Q1), E) :- must_be(callable, P1), !, (p1_call(P1, E), p1_call(Q1, E)).
% Handle logical OR (or/2).
p1_call(or(P1, Q1), E) :- must_be(callable, P1), !, (p1_call(P1, E); p1_call(Q1, E)).
% Handle logical AND (and/2).
p1_call(and(P1, Q1), E) :- must_be(callable, P1), !, (p1_call(P1, E), p1_call(Q1, E)).
% Handle double negation (not(not/1)).
p1_call(not(not(P1)), E) :- !, \+ \+ p1_call(P1, E).
% Handle negation (not/1).
p1_call(not(P1), E) :- !, not(p1_call(P1, E)).
% Execute once/1 to prevent backtracking.
p1_call(once(P1), E) :- !, once(p1_call(P1, E)).
% Execute ignore/1 to suppress failures.
p1_call(ignore(P1), E) :- !, ignore(p1_call(P1, E)).
% Use double negation to check (chk/1).
p1_call(chk(P1), E) :- !, \+ \+ (p1_call(P1, E)).
% Handle negation (\+/1).
p1_call(\+ (P1), E) :- !, \+ p1_call(P1, E).
% Call the predicate with the given argument.
p1_call(P1, E) :- !, call(P1, E).

%!  chk(+X, +E) is nondet.
%
%   Executes `call(X, E)` with double negation to ensure deterministic behavior.
%
%   @arg X The callable term to execute.
%   @arg E The argument passed to the callable term.
% Double negation ensures the call is deterministic.
chk(X, E) :- \+ \+ call(X, E).

%!  p2_call_p2(+P2a, +P2b, +A, -B) is det.
%
%   Chains two predicates `P2a` and `P2b`, passing the result of the first to the second.
%
%   @arg P2a, P2b The predicates to execute sequentially.
%   @arg A Input to the first predicate.
%   @arg B Final output from the second predicate.
% Execute P2a on A, and use its result as input to P2b.
p2_call_p2(P2a, P2b, A, B) :- p2_call(P2a, A, M), p2_call(P2b, M, B).

%!  p2_call(+P2, +A, -B) is det.
%
%   Executes the predicate `P2` on input `A` to produce output `B`, handling lists, conjunctions, and disjunctions.
%
%   @arg P2 The predicate or structure to execute.
%   @arg A The input term.
%   @arg B The output term.
% If P2 is an empty list, unify A and B.
p2_call(P2, A, B) :- P2 == [], !, A = B.
% If P2 is a wrapped p1_call, call it on E and unify E with O.
p2_call(p1_call(P1), E, O) :- !, p1_call(P1, E), E = O.
% If P2 is a list with one element, call it on Grid and GridN.
p2_call([P2], Grid, GridN) :- !, p2_call(P2, Grid, GridN).
% If P2 is a list with multiple elements, apply each element sequentially.
p2_call([P2 | P2L], Grid, GridN) :- !, p2_call(P2, Grid, GridM), p2_call(P2L, GridM, GridN).
% Apply ignore logic to P2 to handle failure cases.
p2_call(ignore(P2), A, B) :- p2_call(P2, A, B) *-> true ; A = B.
% Convert A to the specified type and apply P2.
p2_call(type(Type, P2), A, B) :- into_type(Type, A, AA), p2_call(P2, AA, B).
% Handle logical OR between two predicates.
p2_call(or(P2, Q2), A, B) :- nop(must_be(callable, P2)), !, (p2_call(P2, A, B) ; p2_call(Q2, A, B)).
% Handle logical AND between two predicates.
p2_call(and(P2, Q2), A, B) :- nop(must_be(callable, P2)), !, (p2_call(P2, A, AB), p2_call(Q2, AB, B)).
% Call P2 if it is a valid callable term.
p2_call(P2, A, B) :- must_be(callable, P2), call(P2, A, B).

%!  p1_or(+P1A, +P1B, +X) is nondet.
%
%   Executes `P1A` or `P1B` on `X`. If `P1A` succeeds, it returns true; otherwise, it tries `P1B`.
%
%   @arg P1A, P1B The predicates to apply.
%   @arg X The input to the predicates.
% Try P1A on X; if it fails, try P1B.
p1_or(P1A, P1B, X) :- p1_call(P1A, X) -> true ; p1_call(P1B, X).

%!  p1_and(+P1A, +P1B, +X) is nondet.
%
%   Executes both `P1A` and `P1B` on `X`. Succeeds only if both succeed.
%
%   @arg P1A, P1B The predicates to apply.
%   @arg X The input to the predicates.
% Apply both P1A and P1B on X.
p1_and(P1A, P1B, X) :- p1_call(P1A, X), p1_call(P1B, X).

%!  p1_not(+P1, +E) is nondet.
%
%   Succeeds if the predicate `P1` fails on input `E`.
%
%   @arg P1 The predicate to negate.
%   @arg E The input to the predicate.
% Negate the result of P1 applied to E.
p1_not(P1, E) :- \+ p1_call(P1, E).

%!  p1_ignore(+P1, +E) is det.
%
%   Executes `P1` on `E` and ignores any failures.
%
%   @arg P1 The predicate to execute.
%   @arg E The input to the predicate.
% Apply P1 to E and ignore failures.
p1_ignore(P1, E) :- ignore(p1_call(P1, E)).

%!  p1_arg(+N, +P1, +E) is nondet.
%
%   Extracts the Nth argument from `E` and applies `P1` to it.
%
%   @arg N The position of the argument to extract.
%   @arg P1 The predicate to apply to the argument.
%   @arg E The input term.
% Extract the Nth argument and apply P1.
p1_arg(N, P1, E) :- tc_arg(N, E, Arg), p1_call(P1, Arg).

%!  p1_subterm(+P1, +E) is nondet.
%
%   Applies `P1` to each subterm of `E`.
%
%   @arg P1 The predicate to apply to subterms.
%   @arg E The term containing subterms.
% Apply P1 to each subterm of E.
p1_subterm(P1, E) :- sub_term_safely(Arg, E), p1_call(P1, Arg).

:- meta_predicate my_partition(-, ?, ?, ?).

%!  my_partition(:P1, +List, -Included, -Excluded) is det.
%
%   Partitions a list into two lists: one with elements that satisfy the predicate `P1` and one with the rest.
%
%   @arg P1 The predicate used to test each element.
%   @arg List The input list to partition.
%   @arg Included The list of elements satisfying `P1`.
%   @arg Excluded The list of elements not satisfying `P1`.
% Base case: an empty list results in two empty lists.
my_partition(_, [], [], []) :- !.
% If the head satisfies P1, add it to Included.
my_partition(P1, [H | L], [H | I], E) :- \+ \+ p1_call(P1, H), !, my_partition(P1, L, I, E).
% If the head does not satisfy P1, add it to Excluded.
my_partition(P1, [H | L], I, [H | E]) :- my_partition(P1, L, I, E), !.
% Handle special cases with arcST and ibreak.
my_partition(P1, H, I, HE) :- arcST, ibreak, my_partition(P1, [H], I, HE).

%!  mapgroup(:P2, +G1, -L2) is det.
%
%   Applies the predicate `P2` to each element of the group `G1` and collects the results in `L2`.
%
%   @arg P2 The predicate to apply.
%   @arg G1 The input group (can be a list or grid).
%   @arg L2 The output list after applying `P2` to each element.
% Convert G1 to a list and apply P2 to each element.
mapgroup(P2, G1, L2) :- into_list(G1, L1), !, with_my_group(L1, maplist(P2, L1, L2)).

%!  mapgroup(:P1, +G1) is det.
%
%   Applies the predicate `P1` to each element of the group `G1`.
%
%   @arg P1 The predicate to apply.
%   @arg G1 The input group (can be a list or grid).
% Convert G1 to a list and apply P1 to each element.
mapgroup(P1, G1) :- into_list(G1, L1), !, with_my_group(L1, maplist(P1, L1)).

%!  selected_group(-Grp) is det.
%
%   Retrieves the currently selected group from the non-backtrackable variable `$outer_group`.
%
%   @arg Grp The selected group, or an empty list if not found.
% Retrieve the current group or return an empty list.
selected_group(Grp) :- nb_current('$outer_group', Grp), !.
selected_group([]).

%!  sub_cmpd(+X, +Term) is nondet.
%
%   Checks if `X` is a sub-component of the compound term `Term`. It searches recursively through lists and compound terms.
%
%   @arg X The element to search for.
%   @arg Term The compound term to search within.
% If the term is not compound, fail.
sub_cmpd(_, LF) :- \+ compound(LF), !, fail.
% If X matches the term, succeed.
sub_cmpd(X, X).
% If Term is a list, search each member for X.
sub_cmpd(X, Term) :-
    (   is_list(Term)
    ->  member(E, Term), sub_cmpd(X, E)
    ;   % Otherwise, search each argument of the compound term.
        tc_arg(_, Term, Arg), sub_cmpd(X, Arg)
    ).

%!  with_my_group(+Group, :Goal) is det.
%
%   Executes the given `Goal` within the context of a group.
%   If the group is not handled specially, it simply calls the `Goal`.
%
%   @arg Group The group of objects (if applicable).
%   @arg Goal The goal to execute.
%
%with_my_group([O|Grp],Goal):- compound(O),O=obj(_),!, locally(nb_setval('$outer_group',[O|Grp]),Goal).
% Call the goal without special handling.
with_my_group(_, Goal) :- call(Goal).

%!  into_mlist(+L, -L) is det.
%
%   Ensures the input is treated as a list.
%   This is a direct pass-through function for list handling.
%
%   @arg L The input list.
into_mlist(L, L).

%!  my_maplist(:P4, +G1, -L2, -L3, -L4) is det.
%
%   Applies the predicate `P4` to elements of `G1` and collects results in `L2`, `L3`, and `L4`.
%
%   @arg P4 The predicate to apply.
%   @arg G1 The input group (can be a list or grid).
%   @arg L2, L3, L4 Output lists after applying the predicate.
% Convert G1 to a list and apply P4 to its elements.
my_maplist(P4, G1, L2, L3, L4) :- into_mlist(G1, L1), !, with_my_group(L1, maplist(P4, L1, L2, L3, L4)).

%!  my_maplist(:P3, +G1, -L2, -L3) is det.
%
%   Applies the predicate `P3` to elements of `G1` and collects results in `L2` and `L3`.
%
%   @arg P3 The predicate to apply.
%   @arg G1 The input group (can be a list or grid).
%   @arg L2, L3 Output lists after applying the predicate.
% Convert G1 to a list and apply P3 to its elements.
my_maplist(P3, G1, L2, L3) :- into_mlist(G1, L1), !, with_my_group(L1, maplist(P3, L1, L2, L3)).

%!  my_maplist(:P2, +G1, -L2) is det.
%
%   Applies the predicate `P2` to elements of `G1` and collects results in `L2`.
%
%   @arg P2 The predicate to apply.
%   @arg G1 The input group (can be a list or grid).
%   @arg L2 The output list after applying the predicate.
% Convert G1 to a list and apply P2 to its elements.
my_maplist(P2, G1, L2) :- into_mlist(G1, L1), !, with_my_group(L1, maplist(P2, L1, L2)).

%!  my_maplist(:P1, +G1) is det.
%
%   Applies the predicate `P1` to elements of `G1`.
%
%   @arg P1 The predicate to apply.
%   @arg G1 The input group (can be a list or grid).
% Convert G1 to a list and apply P1 to its elements.
my_maplist(P1, G1) :- into_mlist(G1, L1), !, with_my_group(L1, maplist(P1, L1)).

%!  my_include(:P1, +L, -I) is det.
%
%   Filters the elements of list `L` using the predicate `P1`, collecting the results in `I`.
%
%   @arg P1 The predicate to apply to each element.
%   @arg L The input list to filter.
%   @arg I The output list of elements that satisfy `P1`.
% Use include/3 to filter elements that satisfy p1_call(P1).
my_include(P1, L, I) :- include(p1_call(P1), L, I).
% my_include(P1, [H|L], O) :-
%     (p2_call(p1_call(P1), H, HH) *->
%         (my_include(P1, L, I), O = [HH | I])
%     ;
%         my_include(P1, L, O)
%     ).
% Base case: If the input list is empty, the result is also an empty list.
my_include(_, _, []).

%!  my_exclude(:P1, +I, -O) is det.
%
%   Filters the elements of the input list `I` using the predicate `P1`, collecting elements that do not satisfy `P1` into `O`.
%
%   @arg P1 The predicate used to test elements.
%   @arg I The input list to filter.
%   @arg O The output list of elements that do not satisfy `P1`.
% Use my_partition/4 to partition and collect elements that do not satisfy P1.
% my_exclude(P1, I, O) :- my_include(not(P1), I, O).
my_exclude(P1, I, O) :- my_partition(P1, I, _, O).

%!  subst_1L(+List, +Term, -NewTerm) is det.
%
%   Substitutes occurrences in `Term` based on the substitution pairs in `List`.
%
%   @arg List A list of substitution pairs X-Y.
%   @arg Term The original term to perform substitutions on.
%   @arg NewTerm The term after performing all substitutions.
% Base case: If the list is empty, return the original term.
subst_1L([], Term, Term) :- !.
% Apply substitution for the head pair and continue with the rest of the list.
subst_1L([X-Y | List], Term, NewTerm) :-
    subst0011(X, Y, Term, MTerm),
    subst_1L(List, MTerm, NewTerm).

%!  subst_2L(+From, +To, +Input, -Output) is det.
%
%   Substitutes elements in `Input` based on corresponding elements in `From` and `To`.
%
%   @arg From List of elements to replace.
%   @arg To List of replacement elements.
%   @arg Input The original input term.
%   @arg Output The resulting term after substitutions.
% Base case: If the replacement list is empty, the input remains unchanged.
subst_2L([], _, I, I).
% Apply substitution for the head elements and continue with the rest.
subst_2L([F | FF], [R | RR], I, O) :-
    subst0011(F, R, I, M),
    subst_2L(FF, RR, M, O).

%!  subst001(+I, +F, +R, -O) is det.
%
%   Substitutes occurrences of `F` with `R` in `I`.
%
%   @arg I The input term.
%   @arg F The term to replace.
%   @arg R The replacement term.
%   @arg O The output term after substitution.
% Call the core substitution logic.
subst001(I, F, R, O) :- subst0011(F, R, I, O), !.

%!  subst0011(+X, +Y, +Term, -NewTerm) is det.
%
%   Performs a deep substitution of `X` with `Y` within `Term`.
%
%   @arg X The term to replace.
%   @arg Y The replacement term.
%   @arg Term The original term.
%   @arg NewTerm The term after substitution.
% Copy and prepare terms for substitution, handling goals if present.
subst0011(X, Y, Term, NewTerm) :-
    copy_term((X, Y, Term), (CX, CY, Copy), Goals),
    (Goals == [] ->
        subst0011a(X, Y, Term, NewTerm)
    ;   (subst0011a(CX, CY, Goals, NewGoals),
         (NewGoals == Goals ->
             subst0011a(X, Y, Term, NewTerm)
         ;   (subst0011a(CX, CY, Copy, NewCopy),
              NewTerm = NewCopy, maplist(call, NewGoals))))).

%!  subst0011a(+X, +Y, +Term, -NewTerm) is det.
%
%   Helper predicate for `subst0011` to handle substitution recursively.
%
%   @arg X The term to replace.
%   @arg Y The replacement term.
%   @arg Term The original term.
%   @arg NewTerm The resulting term after substitution.
% Check if the current term matches and substitute or recurse.
subst0011a(X, Y, Term, NewTerm) :-
    ((X == Term) -> Y = NewTerm
    ; (is_list(Term) -> maplist(subst0011a(X, Y), Term, NewTerm)
    ; ((\+ compound(Term); Term = '$VAR'(_)) -> Term = NewTerm
    ; ((compound_name_arguments(Term, F, Args),
       maplist(subst0011a(X, Y), Args, ArgsNew),
       compound_name_arguments(NewTerm, F, ArgsNew)))))), !.

%!  subst001C(+I, +F, +R, -O) is det.
%
%   Wrapper for `subst0011` with a same-term comparison.
%
%   @arg I The input term.
%   @arg F The term to replace.
%   @arg R The replacement term.
%   @arg O The output term after substitution.
% Call the substitution logic with same-term comparison.
subst001C(I, F, R, O) :- subst001_p2(same_term, I, F, R, O), !.

%!  subst0011C(+F, +R, +I, -O) is det.
%
%   Calls `subst0011` with a same-term comparison.
%
%   @arg F The term to replace.
%   @arg R The replacement term.
%   @arg I The input term.
%   @arg O The output term after substitution.
% Call the core logic with same-term comparison.
subst0011C(F, R, I, O) :- subst0011_p2(same_term, F, R, I, O), !.

%!  subst_2LC(+From, +To, +I, -O) is det.
%
%   Calls `subst_2L_p2` with the same-term comparison.
%
%   @arg From List of elements to replace.
%   @arg To List of replacement elements.
%   @arg I The input term.
%   @arg O The output term after substitutions.
% Apply the substitution logic with same-term comparison.
subst_2LC(F, R, I, O) :- subst_2L_p2(same_term, F, R, I, O).

%!  subst_2L_p2(:P2, +From, +To, +I, -O) is det.
%
%   Substitutes elements using a predicate comparison `P2`.
%
%   @arg P2 The comparison predicate.
%   @arg From List of elements to replace.
%   @arg To List of replacement elements.
%   @arg I The input term.
%   @arg O The resulting term after substitutions.
% Base case: Stop when lists are exhausted.
subst_2L_p2(_P2, [], _, I, I) :- !.
subst_2L_p2(_P2, _, [], I, I) :- !.
% Apply the predicate-based substitution and continue.
subst_2L_p2(P2, [F | FF], [R | RR], I, O) :-
    subst0011_p2(P2, F, R, I, M),
    subst_2L_p2(P2, FF, RR, M, O).

%!  subst001_p2(:P2, +I, +F, +R, -O) is det.
%
%   Wrapper for `subst0011_p2` with a predicate comparison.
%
%   @arg P2 The comparison predicate.
%   @arg I The input term.
%   @arg F The term to replace.
%   @arg R The replacement term.
%   @arg O The output term after substitution.
% Call the substitution logic with the comparison predicate.
subst001_p2(P2, I, F, R, O) :- subst0011_p2(P2, F, R, I, O), !.

%!  subst_1L_p2(:P2, +List, +Term, -NewTerm) is det.
%
%   Substitutes occurrences using a predicate comparison `P2`.
%
%   @arg P2 The comparison predicate.
%   @arg List A list of substitution pairs X-Y.
%   @arg Term The original term to substitute.
%   @arg NewTerm The resulting term after substitutions.
% Base case: If the list is empty, return the original term.
subst_1L_p2(_, [], Term, Term) :- !.
% Apply predicate-based substitution for the head pair and continue.
subst_1L_p2(P2, [X-Y | List], Term, NewTerm) :-
    subst0011_p2(P2, X, Y, Term, MTerm),
    subst_1L_p2(P2, List, MTerm, NewTerm).

%!  subst0011_p2(:P2, +X, +Y, +Term, -NewTerm) is det.
%
%   Substitutes `X` with `Y` in `Term` using the predicate comparison `P2`.
%
%   @arg P2 The comparison predicate.
%   @arg X The term to replace.
%   @arg Y The replacement term.
%   @arg Term The original term.
%   @arg NewTerm The resulting term after substitution.
% Copy and prepare terms for predicate-based substitution.
subst0011_p2(P2, X, Y, Term, NewTerm) :-
    copy_term((X, Y, Term), (CX, CY, Copy), Goals),
    (Goals == [] ->
        subst0011a_p2(P2, X, Y, Term, NewTerm)
    ;   (subst0011a_p2(P2, CX, CY, Goals, NewGoals),
         (NewGoals == Goals ->
             subst0011a_p2(P2, X, Y, Term, NewTerm)
         ;   (subst0011a_p2(P2, CX, CY, Copy, NewCopy),
              NewTerm = NewCopy, maplist(call, NewGoals))))).

%!  subst0011a_p2(:P2, +X, +Y, +Term, -NewTerm) is det.
%
%   Recursive helper for `subst0011_p2` to perform substitution.
%
%   @arg P2 The comparison predicate.
%   @arg X The term to replace.
%   @arg Y The replacement term.
%   @arg Term The original term.
%   @arg NewTerm The resulting term after substitution.
% Apply substitution using the comparison predicate or recurse into terms.
subst0011a_p2(P2, X, Y, Term, NewTerm) :-
    (p2_call(P2, X, Term) -> Y = NewTerm
    ; (is_list(Term) -> maplist(subst0011a_p2(P2, X, Y), Term, NewTerm)
    ; ((\+ compound(Term); Term = '$VAR'(_)) -> Term = NewTerm
    ; ((compound_name_arguments(Term, F, Args),
       maplist(subst0011a_p2(P2, X, Y), Args, ArgsNew),
       compound_name_arguments(NewTerm, F, ArgsNew)))))), !.

%!  ppa(+FF) is det.
%
%   Processes and pretty-prints the given term `FF` with handling for attribute variables.
%
%   @arg FF The term to be processed and printed.
% Copy the term, handle attribute variables, and print the processed term.
ppa(FF) :-
    copy_term(FF, FA, GF),
    numbervars(FA + GF, 0, _, [attvar(bind), singletons(true)]),
    sort_safe(GF, GS), write(' '),
    locally(b_setval(arc_can_portray, nil), ppawt(FA)), format('~N'),
    ignore((GS \== [], format('\t'), ppawt(attvars = GS), nl)), nl, !.

%!  ppawt(+FA) is det.
%
%   Writes the term `FA` with custom printing options.
%
%   @arg FA The term to print.
% Print the term with extensive formatting options.
ppawt(FA) :-
    write_term(FA, [numbervars(false), quoted(true), character_escapes(true),
                    cycles(true), dotlists(false), no_lists(false),
                    blobs(portray), attributes(dots),
                    portray(true), partial(false), fullstop(true),
                    %portray(false), partial(true), fullstop(true),
                    ignore_ops(false), quoted(true), quote_non_ascii(true),
                    brace_terms(false)]).

%%  intersect_eq(+List1, +List2, -Intersection)
%
%   Determine the intersection of two lists without unifying values.

intersect_eq([], _, []).
intersect_eq([X|Xs], Ys, L) :-
    (   clpr:memberchk_eq(X, Ys)
    ->  L = [X|T],
        intersect_eq(Xs, Ys, T)
    ;   intersect_eq(Xs, Ys, L)
    ).

%!  intersection(+APoints, +BPoints, -Intersected, -LeftOverA, -LeftOverB) is det.
%
%   Computes the intersection of two sets of points, also returning leftover points from both sets.
%
%   @arg APoints The first set of points.
%   @arg BPoints The second set of points.
%   @arg Intersected The points present in both sets.
%   @arg LeftOverA Points present only in `APoints`.
%   @arg LeftOverB Points present only in `BPoints`.
% Calculate the intersection and leftover points using `intersection_univ/5`.
intersection(APoints, BPoints, Intersected, LeftOverA, LeftOverB) :-
    intersection_univ(APoints, BPoints, Intersected, LeftOverA, LeftOverB), !.

%!  same_univ(+A, +B) is nondet.
%
%   Checks if two terms are structurally equivalent or identical.
%
%   @arg A The first term to compare.
%   @arg B The second term to compare.
% Check if A and B are equivalent or identical.
same_univ(A, B) :-  (plain_var(A)->A==B;(B=@=A->true; (fail, \+ (A \=B )))).

%!  intersection_univ(+APoints, +BPoints, -Intersected) is det.
%
%   Computes the intersection of two sets of points.
%
%   @arg APoints The first set of points.
%   @arg BPoints The second set of points.
%   @arg Intersected The points present in both sets.
% Calculate the intersection without leftover points.
intersection_univ(APoints, BPoints, Intersected) :-
    intersection_univ(APoints, BPoints, Intersected, _, _), !.

%!  intersection_univ(+APoints, +BPoints, -Intersected, -LeftOverA, -LeftOverB) is det.
%
%   Computes the intersection of two sets of points, also returning leftover points from both sets.
%
%   @arg APoints The first set of points.
%   @arg BPoints The second set of points.
%   @arg Intersected The points present in both sets.
%   @arg LeftOverA Points present only in `APoints`.
%   @arg LeftOverB Points present only in `BPoints`.
% Use `pred_intersection/7` to compute the intersection and leftovers.
intersection_univ(APoints, BPoints, Intersected, LeftOverA, LeftOverB) :-
    pred_intersection(same_univ, APoints, BPoints, Intersected, _, LeftOverA, LeftOverB).

%!  intersection_eq(+APoints, +BPoints, -Intersected) is det.
%
%   Computes the intersection of two sets of points.
%
%   @arg APoints The first set of points.
%   @arg BPoints The second set of points.
%   @arg Intersected The points present in both sets.
% Calculate the intersection without leftover points using `intersection_eq/5`.
intersection_eq(APoints, BPoints, Intersected) :-
    intersection_eq(APoints, BPoints, Intersected, _, _), !.

%!  intersection_eq(+APoints, +BPoints, -Intersected, -LeftOverA, -LeftOverB) is det.
%
%   Computes the intersection of two sets of points, also returning leftover points from both sets.
%
%   @arg APoints The first set of points.
%   @arg BPoints The second set of points.
%   @arg Intersected The points present in both sets.
%   @arg LeftOverA Points present only in `APoints`.
%   @arg LeftOverB Points present only in `BPoints`.
% Use `pred_intersection/7` to calculate the intersection with leftover points.
intersection_eq(APoints, BPoints, Intersected, LeftOverA, LeftOverB) :-
    pred_intersection(same_univ, APoints, BPoints, Intersected, _, LeftOverA, LeftOverB).

/*
intersection_u([],LeftOverB,[],[],LeftOverB):-!.
intersection_u(LeftOverA,[],[],LeftOverA,[]):-!.
intersection_u([A|APoints],BPoints,[A|Intersected],LeftOverA,LeftOverB):-
  select(A,BPoints,BPointsMinusA),!,
  intersection_u(APoints,BPointsMinusA,Intersected,LeftOverA,LeftOverB).
intersection_u([A|APoints],BPoints,Intersected,[A|LeftOverA],LeftOverB):-
  intersection_u(APoints,BPoints,Intersected,LeftOverA,LeftOverB).
*/

:- meta_predicate(each_obj(?,?,0)).

%!  each_obj(+List, ?Obj, :Goal) is det.
%
%   Iterates over each element in the list `List`, setting `Obj` to the current element and executing `Goal`.
%
%   @arg List The list of objects to iterate over.
%   @arg Obj The current object in the iteration.
%   @arg Goal The goal to execute for each object.
% Base case: If the list is empty, do nothing.
each_obj([], _, _) :- !.
% Execute the goal for the current object and continue with the rest of the list.
each_obj([Obj | List], Obj, Goal) :-
    ignore(Goal),
    each_obj(List, Obj, Goal).

%!  pred_intersection(:P2, +LeftOverA, +LeftOverB, -IntersectedA, -IntersectedB, -LeftOverAOut, -LeftOverBOut) is det.
%
%   Computes the intersection of two lists using the predicate `P2` to determine equivalence, also collecting leftover elements.
%
%   @arg P2 The predicate to compare elements.
%   @arg LeftOverA The first input list.
%   @arg LeftOverB The second input list.
%   @arg IntersectedA The intersected elements from the first list.
%   @arg IntersectedB The intersected elements from the second list.
%   @arg LeftOverAOut Elements only in the first list.
%   @arg LeftOverBOut Elements only in the second list.
% Base case: If the second list is empty, return the first list as leftover.
pred_intersection(_P2, [], LeftOverB, [], [], [], LeftOverB) :- !.
% Base case: If the first list is empty, return the second list as leftover.
pred_intersection(_P2, LeftOverA, [], [], [], LeftOverA, []) :- !.
% If an element in the first list matches an element in the second list, include it in the intersection.
pred_intersection(P2, [A | APoints], BPoints, [A | IntersectedA], [B | IntersectedB], LeftOverA, LeftOverB) :-
    select(B, BPoints, BPointsMinusA),
    \+ \+ p2_call(P2, A, B), !,
    pred_intersection(P2, APoints, BPointsMinusA, IntersectedA, IntersectedB, LeftOverA, LeftOverB).
% If no match is found, add the element to the leftover list and continue.
pred_intersection(P2, [A | APoints], BPoints, IntersectedA, IntersectedB, [A | LeftOverA], LeftOverB) :-
    pred_intersection(P2, APoints, BPoints, IntersectedA, IntersectedB, LeftOverA, LeftOverB).







%!  pp(+PP) is det.
%
%   Prints the given term `PP` using the predicate `pp_m/1`.
%
%   @arg PP The term to print.
% Call `pp_m/1` to print the term.
pp(PP) :- pp_m(PP).

%!  pp(+Color, +PP) is det.
%
%   Prints the given term `PP` with the specified ANSI color formatting.
%
%   @arg Color The color to use for the formatted output.
%   @arg PP The term to print.
% Print the term with ANSI color formatting.
pp(Color, PP) :-
    ansi_format([fg(Color)], '~@', [pp(PP)]).

%!  warn_skip(+P) is det.
%
%   Prints a warning message for the skipped predicate `P`.
%
%   @arg P The predicate to be warned about.
% Print a warning message for the skipped predicate.
warn_skip(P) :- pp(warn_skip(P)).

%!  with_set_stream(+OldStream, +NewStream, :Goal) is det.
%
%   Executes the given goal `Goal` with the stream context temporarily changed.
%
%   @arg OldStream The original stream.
%   @arg NewStream The new stream to use during the execution of `Goal`.
%   @arg Goal The goal to execute within the stream context.
% Call the goal with the stream context.
with_set_stream(_, _, G) :- call(G).

%!  fake_impl(+Spec) is det.
%
%   Creates a fake implementation for the specified predicate or module predicate.
%   This is used to define placeholder predicates that always fail.
%
%   @arg Spec The specification of the predicate, either in the form `M:F/A` or `F/A`.
%
%   If given a module predicate (`M:F/A`), it asserts a failure clause for the predicate within the specified module.
%   If given a plain predicate (`F/A`), it asserts a failure clause for it in the current context.
fake_impl(M:F/A) :-
    functor(P, F, A),
    asserta((M:P :- !, fail)).
fake_impl(F/A) :-
    functor(P, F, A),
    asserta((P :- !, fail)).
% Fake implementations for various predicates.
:- fake_impl(arc_setval/3).
:- fake_impl(cast_to_grid/3).
:- fake_impl(dot_cfg:dictoo_decl/8).
:- fake_impl(get_param_sess/2).
:- fake_impl(into_list/2).
:- fake_impl(into_type/3).
:- fake_impl(is_grid/1).
:- fake_impl(is_hooked_obj/1).
:- fake_impl(is_vm_map/1).
:- fake_impl(ld_logicmoo_webui/0).
:- fake_impl(must_grid_call/3).
:- fake_impl(o_m_v/3).
:- fake_impl(quick_test/1).
:- fake_impl(url_decode_term/2).
:- fake_impl(xlisting_web:find_http_session/1).
:- fake_impl(xlisting_web:is_cgi_stream/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Higher arity maplists

% maplist/6 applies Pred to the elements of List1, List2, ..., List5 in parallel
maplist(Goal, List1, List2, List3, List4, List5) :-
    maplist_(List1, List2, List3, List4, List5, Goal).
maplist_([], [], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4, Elem5),
    maplist_(Tail1, Tail2, Tail3, Tail4, Tail5, Goal).

% maplist/7 applies Pred to the elements of List1, List2, ..., List6 in parallel
maplist(Goal, List1, List2, List3, List4, List5, List6) :-
    maplist_(List1, List2, List3, List4, List5, List6, Goal).
maplist_([], [], [], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], [Elem6|Tail6], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4, Elem5, Elem6),
    maplist_(Tail1, Tail2, Tail3, Tail4, Tail5, Tail6, Goal).

% maplist/8 applies Pred to the elements of List1, List2, ..., List8 in parallel
maplist(Goal, List1, List2, List3, List4, List5, List6, List7) :-
    maplist_(List1, List2, List3, List4, List5, List6, List7, Goal).
maplist_([], [], [], [], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], [Elem6|Tail6], [Elem7|Tail7], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4, Elem5, Elem6, Elem7),
    maplist_(Tail1, Tail2, Tail3, Tail4, Tail5, Tail6, Tail7, Goal).

% maplist/9 applies Pred to the elements of List1, List2, ..., List8 in parallel
maplist(Goal, List1, List2, List3, List4, List5, List6, List7, List8) :-
    maplist_(List1, List2, List3, List4, List5, List6, List7, List8, Goal).
maplist_([], [], [], [], [], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], [Elem6|Tail6], [Elem7|Tail7], [Elem8|Tail8], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4, Elem5, Elem6, Elem7, Elem8),
    maplist_(Tail1, Tail2, Tail3, Tail4, Tail5, Tail6, Tail7, Tail8, Goal).

map_fold1(_,[],[],A,A).
map_fold1(Pred,[X|Xt],[Y|Yt],A1,A3) :- call(Pred,X,Y,A1,A2),map_fold1(Pred,Xt,Yt,A2,A3).

end_of_file.














































































/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- meta_predicate(print_grid(+,+,+,+)).
:- meta_predicate(print_grid(+,+,+)).


%:- autoload(library(http/html_write),[html/3,print_html/1]).


is_debugging(M):- \+ \+ debugging(M),!.
is_debugging(_):- is_testing,!.
%is_debugging(_):- menu_or_upper('B').

debug_m(_,Tiny):- display_length(Tiny,Len),Len<30,!,pp(Tiny).
debug_m(M,_):- \+ is_debugging(M),!.
%debug_m(_,List):- is_list(List),!,print_ss(List).
debug_m(_,Term):- pp(Term).
debug_c(M,_):- \+ is_debugging(M),!.
debug_c(_,C):- call(C),!.
debug_c(M,C):- wots_hs(S,C),debug_m(M,S),!.

:- meta_predicate(wno(0)).
wno(G):-
 locally(b_setval(print_collapsed,10), G).

:- meta_predicate(print_collapsed(0)).
print_collapsed(Size,G):-
 locally(b_setval(print_collapsed,Size), print_collapsed0(Size,G)).

:- meta_predicate(print_collapsed0(0)).
print_collapsed0(Size,G):- Size<10, !, call(G).
% print_collapsed(Size,G):-  call(G).
print_collapsed0(Size,G):- Size>=10, !, wots_hs(_S,G).
print_collapsed0(_,G):- wots_vs(S,G),write(S).

tersify(I,O):- tracing,!,I=O.
%tersify(I,O):- term_variables(I,Vs), \+ ( member(V,Vs), attvar(V)),!,I=O.
tersify(I,O):- tersify23(I,O),!.
tersify(X,X):-!.

tersify23(I,O):- quietly((tersify2(I,M),tersify3(M,O))),!.

%srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
%srw_arc(I,O):- compound(I),!, wots_hs(O,(write(ppt(I)))).
/*
srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
*/
srw_arc(I,O):- is_vm_map(I),!, O='..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.
/*
srw_arc(List,O):- current_prolog_flag(dmsg_len,Three),
  is_list(List),length(List,L),L>Three,
   append([A,B,C],[F|_],List),F \='...'(_), !,
  simplify_goal_printed([A,B,C,'....'(L>Three)],O).
*/
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
srw_arc(I,O):- tersify(I,O),!,I\==O,!.

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

dumpst_hook:simple_rewrite(I,O):- fail, notrace(catch(arc_simple_rewrite(I,O),_,fail)).

arc_simple_rewrite(I,O):-
  \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(bfly_startup/0),
  current_predicate(is_group/1),
  b_setval(arc_can_portray,nil),
  locally(b_setval(arc_can_portray,nil),once((compound(I), lock_doing(srw_arc,I,srw_arc(I,O))))), I\==O, I\=@=O, !, \+ I=O,
  b_setval(arc_can_portray,t).


%:- set_prolog_flag(never_pp_hook, true).


portray_terse:- true,!.

:- discontiguous arc_portray/2.


arc_portray(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
arc_portray(_,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t), !, fail.
arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs).

arc_portray_t(G, _):- is_vm_map(G), !, write_map(G,'arc_portray_t').
arc_portray_t(G, _):- is_grid(G),  !, data_type(G,W),writeq(grid(W)).
arc_portray_t(G, _):- print(G),!.

arc_portray(G, _):- is_vm_map(G),  !, write_map(G,'arc_portray').
arc_portray(G, TF):- TF == true, portray_terse, arc_portray_t(G, TF),!.
arc_portray(G, TF):- catch(arc_portray_nt(G, TF),E,(writeln(E),never_let_arc_portray_again,fail)),!.
%arc_portray(G, _TF):- writeq(G),!.

% Portray In Debugger

arc_portray_nt(G, false):- is_grid(G), print_grid(G),!.
%arc_portray_nt([G|L],_False):- is_object(G), !, pp([G|L]).
%arc_portray_nt(G0, true):- is_group(G0), ppt(G0),!.
%arc_portray_nt(G0, false):- is_group(G0), ppt(G0),!.
arc_portray_nt(G0, Tracing):- is_group(G0), into_list(G0,G), length(G,L),% L>1, !,
   maplist(tersify,G0,GG), write(GG),
   if_t(Tracing==false,
    in_cmt((
     dash_chars,
     once(((why_grouped(_TestID,Why,WG),WG=@=G,fail);(Why = (size2D=L)))),!,
     print_grid(Why,G),nl_now,

     %underline_print(writeln(Why)),
     %print_info_l(G),
     dash_chars))).


arc_portray_nt(G,_False):- is_object(G), wots(S,writeg(G)),
  global_grid(G,GG),!,
  print_grid(GG),
  write(S),!. % show_indiv(S,G).
  %object_grid(G,OG),
  %neighbor_map(OG,NG), !,
  %print_grid(object_grid,NG),nl_now,
  %underline_print(print_info(G)),

arc_portray_nt(G,false):- via_print_grid(G),!, grid_size(G,H,V),!,H>0,V>0, print_grid(H,V,G).

% Portray In tracer
arc_portray_nt(G,true):- is_object(G),underline_print((ppt(G))).
arc_portray_nt(G,true):- via_print_grid(G),write_nbsp,underline_print((ppt(G))),write_nbsp.
arc_portray_nt(G,true):- tersify(G,O),write_nbsp,writeq(O),write_nbsp.
arc_portray_nt(G0, _):- \+ is_gridoid(G0),!,print(G0).


arc_portray_pairs(Type,TF,Pairs):-
  length(Pairs,N),
  writeln(arc_portray_pairs(Type,TF,len(N))),
  swap_kv(Pairs,VKPairs),
  keysort(VKPairs,SVKPairs),
  my_maplist(tc_arg(2),SVKPairs,SVKPairs2),
  arc_portray_type_pairs(TF,SVKPairs2).

arc_portray_type_pairs(TF,Pairs):- append(Left,[K1-V1,K2-V2|Right],Pairs),is_grid(V1),is_grid(V2),!,
  append(Left,[call-print_side_by_side(yellow,V1,K1,_,V2,K2)|Right],PairsM),
  arc_portray_type_pairs(TF,PairsM).
arc_portray_type_pairs(TF,Pairs):-
  forall(member(K-V,Pairs),arc_portray_pair(Pairs,K,V,TF)).

swap_kv([_-V|Pairs],VKPairs):- plain_var(V),!, swap_kv(Pairs,VKPairs).
swap_kv([K-V|Pairs],['-'(Type,K-V)|VKPairs]):-
  data_type(V,Type),
  swap_kv(Pairs,VKPairs).
swap_kv([],[]).


arc_portray_pair(Ps,K,Val,TF):-
 nl_if_needed,
 arc_portray_1_pair(Ps,K,Val,TF),
 nl_if_needed_ansi.

arc_portray_1_pair(_Ps,call,Val,_TF):- !, call(Val).
arc_portray_1_pair(Ps,K,Val,TF):-
 (via_print_grid(Val) -> print_grid(K,Val)
   ;  (print(K),write('= '),once(arc_portray(Val,TF);print(Val)))),
 ignore(arc_portray_pair_optional(Ps,K,Val,TF)),!.

arc_portray_pair_optional(Ps,K,Val,TF):-
 once(( Val\==[], is_list(Val),my_maplist(is_object,Val),
  print_info(Val),
  Val \= [_],
  compare_objects(Val,Diffs),
  color_print(cyan,call(arc_portray_pair(Ps,diffs(K),Diffs,TF))))).


% arc_portray(G):- \+ \+ catch((wots_hs(S,( tracing->arc_portray(G,true);arc_portray(G,false))),write(S),ttyflush),_,fail).
arc_portray(G):- \+ compound(G),fail.
arc_portray(G):- is_vm(G), !, write('..VM..').
arc_portray(G):- \+ nb_current(arc_portray,t),\+ nb_current(arc_portray,f),is_print_collapsed,!,
  locally(b_setval(arc_portray,t),arc_portray1(G)).
arc_portray(G):- \+ nb_current(arc_portray,f),!, locally(b_setval(arc_portray,t),arc_portray1(G)).
arc_portray(G):- locally(b_setval(arc_portray,f),arc_portray1(G)).


:- thread_initialization(nb_setval(arc_portray,[])).


arc_portray1(G):-
 flag(arc_portray_current_depth,X,X), X < 3,
 \+ \+
  setup_call_cleanup(flag(arc_portray_current_depth,X,X+1),catch(((tracing->arc_portray(G,true);
  arc_portray(G,false)),ttyflush),E,(fail,format(user_error,"~N~q~n",[E]),fail)),flag(arc_portray_current_depth,_,X)).


%via_print_grid(G):- tracing,!,fail.
via_print_grid(G):- is_points_list(G). %,!,fail,grid_size(G,H,V),number(H),number(V),H>1,V>1.
via_print_grid(G):- is_grid(G).
via_print_grid(G):- is_obj_props(G),!,fail.
via_print_grid(G):- is_object(G).
via_print_grid(G):- is_group(G).
via_print_grid(G):- is_gridoid(G).



terseA(_,[],[]):- !.
terseA(_,L,'... attrs ...'(N)):- is_list(L),length(L,N),N>10,!.
terseA(I,[A|L],[B|LL]):-terseA(I,A,B),terseA(I,L,LL),!.
terseA(I,dif(A,B),B):-A==I,!.
terseA(I,dif(B,A),B):-A==I,!.
terseA(_,put_attr(_,B,A),A):- B==ci,!.
terseA(_,put_attr(_,B,A),B=A):-!.
terseA(_,A,A):-!.


simple_enough(I):- plain_var(I).
simple_enough(I):- atomic(I).
simple_enough(I):- \+ compound(I),!.
simple_enough(_*_):-!.
simple_enough(_+_):-!.
simple_enough(A):- functor(A,_,1),tc_arg(1,A,E),!,simple_enough(E).
%simple_enough(I):- number(I).
%simple_enough(I):- atom(I).

tersify0(I,O):- simple_enough(I),!,I=O.
tersify0(I,av(C,Others)):- attvar(I),copy_term(I,C,Attrs),terseA(C,Attrs,Others),!.
tersify0(I,I):- var(I),!.


%tersifyC(D):- is_vm_map(D),!.
tersifyC(av(_,_)).
tersifyC(objFn(_,_)).
tersifyC(groupFn(_,_)).
tersifyC(objFn(_)).
tersifyC(groupFn(_)).

tersify1(I,O):- simple_enough(I),!,I=O.
tersify1(av(_,Blue), -(Blue)):-!.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.
tersify1(gridFn(I),gridFn(I)):-!. % tersifyG(I,O).
%tersify1(gridFn(I),gridFn(O)):-tersifyG(I,O).
tersify1(Nil,[]):- Nil == [],!.
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).
tersify1(I,gridFn(O)):- is_grid(I),tersifyG(I,O),!.
tersify1(I,groupFn(O,List)):- is_group(I), mapgroup(tersify1,I,List),mapgroup(obj_to_oid,I,OIDs),length(List,N), !,ignore((get_current_test(TestID),is_why_grouped(TestID,N,Why,OIDs),!,O=Why)).

tersify1(I,Q):- is_object(I),object_ref_desc(I,Q),!.
tersify1(I,O):- is_vm_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_vm_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').


tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.

%tersifyL(I,I):- is_ftVar(I),!.
%tersifyL(I,I):- \+ compound(I),!.
tersifyL(I,O):- \+ is_cons(I),!,O=I.
tersifyL([H|I],[HH|I]):- \+ is_list(I),!,tersify(H,HH).
tersifyL([H|I],O):- nonvar(H), \+ is_group(I), display_length(I,N) , N>170,
  length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O),!.
tersifyL(I,O):- tersify0(I,O),!.
tersifyL([H|TT],[HH|TT]):- tersify(H,HH),!,tersifyL(TT,TT),!.
tersifyL(I,O):- tersify1(I,O),!.
tersifyL(I,I).

tersify2(I,O):- compound(I),(I=(N=V)),tersify2(N,NN),tersify2(V,VV),!,O=(NN=VV).
tersify2(I,O):- simple_enough(I),!,I=O.
tersify2(I,O):- compound(I),tersify1(I,O),!.
tersify2(I,O):- tersify0(I,O),!.
tersify2(I,O):- is_list(I), !, my_maplist(tersify2,I,O).
tersify2(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify2(I,I).

tersify3(I,O):- compound(I),(I=(N=V)),tersify3(N,NN),tersify3(V,VV),!,O=(NN=VV).
tersify3(I,O):- simple_enough(I),!,I=O.
tersify3(I,O):- compound(I),tersify1(I,O),!.
tersify3(I,O):- tersify0(I,O),!.
tersify3([H|I],O):- is_list(I), ((display_length(I,N), N>170) ->
  (length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O)); I=O),!.
tersify3(I,O):- is_list(I), !, my_maplist(tersify3,I,O).
tersify3(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify3(I,I).

write_map(G,Where):- is_vm(G), !, write('...VM_'),write(Where),write('...').
write_map(G,Where):- is_vm_map(G), !, write('...Map_'),write(Where),write('...').
write_map(G,Where):- is_dict(G), !, write('...Dict_'),write(Where),write('...').
write_map(_G,Where):- write('...'),write(Where),write('...').



non_empty_wqs_c(V):- \+ empty_wqs_c(V).
empty_wqs_c(V):- var(V),!,fail.
empty_wqs_c(A):- atom(A),atom_string(A,S),!,empty_wqs_c(S).
empty_wqs_c([]).
empty_wqs_c("").
empty_wqs_c("&nbsp;").
empty_wqs_c(" ").
empty_wqs_c("\n").

is_writer_goal(H):- \+ callable(H),!,fail.
is_writer_goal(H):- is_list(H),!,fail.
is_writer_goal(A):- atom(A),!,is_writer_goal_f(A).
is_writer_goal(H):- \+ compound(H),!,fail.
%is_writer_goal((C1,C2)):- !, (is_writer_goal(C1);is_writer_goal(C2)).
is_writer_goal(C):- compound_name_arity(C,F,_),once(is_writer_goal_f(F);(tc_arg(_,C,E),is_writer_goal(E))).


is_writer_goal_f(wqs_c).
is_writer_goal_f(F):- is_writer_goal_l(F),!.
is_writer_goal_f(F):- \+ atom(F),!, term_to_atom(F,A),is_writer_goal_f(A).
is_writer_goal_f(F):- not_writer_goal_r(R),atom_concat(_,R,F),!,fail.
is_writer_goal_f(F):- is_writer_goal_l(L),atom_concat(L,_,F),!.
is_writer_goal_f(F):- is_writer_goal_l(R),atom_concat(_,R,F),!.
not_writer_goal_r(test). is_writer_goal_l(msg). is_writer_goal_l(call).
is_writer_goal_l(nl).  is_writer_goal_l(format). is_writer_goal_l(with_).
is_writer_goal_l(locally).

is_writer_goal_l(html).  is_writer_goal_l(ptcol).  is_writer_goal_l(wots).
is_writer_goal_l(print). is_writer_goal_l(flush_output).  is_writer_goal_l(wqs).
is_writer_goal_l(pp). is_writer_goal_l(write).  is_writer_goal_l(dash_).


maybe_color(SS,_):- term_contains_ansi(SS),!, write_nbsp, write(SS).
maybe_color(SS,P):- term_contains_ansi(P),!, write_nbsp, write(SS).
maybe_color(SS,P):- pp_msg_color(P,C), ansicall(C,is_maybe_bold(P,write(SS))),!.

write_atom(S):- \+ atom(S),!,wqs(S).
write_atom(S):- atom_contains(S,'~'),!,notrace(catch(format(S,[]),_,maybe_write_atom_link(S))).
write_atom(S):- maybe_write_atom_link(S),!.
write_atom(S):- into_title_str(S,TS),write(TS),!.

:- meta_predicate(into_title_str(+,-)).
into_title_str(Term,Str):- string(Term),!,Str=Term.
into_title_str(Term,Str):- plain_var(Term),sformat(Str,'~p',[Term]),!.
into_title_str(Term,Str):- var(Term),tersify0(Term,Terse), sformat(Str,'~p',[Terse]),!.
into_title_str(Term,Str):- term_is_ansi(Term), wots(Str,write_keeping_ansi_mb(Term)),!.
into_title_str(Term,Str):- (is_codelist(Term);is_charlist(Term)),catch(sformat(Str,'~s',[Term]),_,sformat(Str,'~p',[Term])),!.
into_title_str(Term,Str):- is_list(Term),my_maplist(into_title_str,Term,O3),atomics_to_string(O3," ",Str),!.
into_title_str([H|T],Str):- into_title_str(H,A),into_title_str(T,B),atomics_to_string([A,B]," ",Str),!.
into_title_str(Term,Str):- \+ callable(Term),sformat(Str,'~p',[Term]),!.
into_title_str(format(Fmt,Args),Str):- sformat(Str,Fmt,Args),!.
into_title_str(Term,""):- empty_wqs_c(Term),!.
into_title_str(out,"Output").
into_title_str(in,"Input").
into_title_str(i,"IN").
into_title_str(o,"OUT").
into_title_str(Term,Str):- atom(Term),is_valid_linkid(Term,Kind,_),Term\=@=Kind,into_title_str(Kind,KS),sformat(Str,'~w (~w)',[Term,KS]),!.
into_title_str(Term,Str):- atom(Term), atom_contains(Term,'_'), \+ atom_contains(Term,' '),  to_case_breaks(Term,T),
 include(\=(xti(_,punct)),T,O),my_maplist(tc_arg(1),O,O1),my_maplist(toProperCamelAtom,O1,O2),
  atomics_to_string(O2," ",Str),!.
into_title_str(Term,Str):- has_short_id(Term,Kind,ID),Term\=@=Kind,into_title_str(Kind,KS),sformat(Str,'~w (~w)',[ID,KS]),!.

into_title_str(T-U,Str):- into_title_str([some(T),"..to..",some(U)],Str).
into_title_str(T*U,Str):- into_title_str([some(T),"(",some(U),")"],Str).
into_title_str(T+U,Str):- into_title_str(T,S1), number(U), N is U+1, sformat(Str,'~w #~w',[S1,N]).
into_title_str(T+U,Str):- var(U), into_title_str(T,S1), sformat(Str,'~w(s)',[S1]).
into_title_str(title(Term),Str):- !, into_title_str(Term,Str),!.
into_title_str(some(Var),"Some"):- var(Var),!.
into_title_str(some(Var),Str):- !, into_title_str(Var,Str).
into_title_str(User:Term,Str):- User == user, !, into_title_str(Term,Str).
into_title_str(trn,"Training Pair").
into_title_str(tst,"EVALUATION TEST").
%into_title_str(Term,Str):- tersify23(Term,Terse),Term\=@=Terse,!,into_title_str(Terse,Str).
into_title_str(Term,Str):- callable_arity(Term,0),is_writer_goal(Term),catch(notrace(wots(Str,call_e_dmsg(Term))),_,fail),!.
into_title_str(Term,Str):- catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)),nonvar(Str),atom_length(Str,E50),E50<180,!.
into_title_str(Term,Str):- compound(Term), compound_name_arguments(Term,Name,Args),
   %include(not_p1(plain_var),Args,Nonvars),
   Args=Nonvars,
   my_maplist(tersify,Nonvars,ArgsT), into_title_str([Name,"(",ArgsT,")"],Str),!.
into_title_str(Term,Str):- catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)).

has_short_id(TestID,testid,UUID):- is_valid_testname(TestID),test_id_atom(TestID,UUID).
has_short_id(Obj,object,OID):- is_object(Obj),obj_to_oid(Obj,OID).
has_short_id(Grid,grid,GID):- is_grid(Grid),grid_to_gid(Grid,GID).


is_valid_linkid(ID,testid,TestID):- atom_id(ID,TestID),is_valid_testname(TestID),!.
is_valid_linkid(ID,object,Obj):- known_object(ID,Obj),!.
is_valid_linkid(ID,grid,Grid):- known_grid(ID,Grid),!.
% individuate_3(complete, two(v_1d398264_trn_0_in, v_1d398264_trn_0_out))
is_valid_linkid(ID,group,Grp):- get_current_test(TestID),is_why_grouped_g(TestID,_Count,ID,Grp).


wqs_c(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs_c(S):- (string(S);is_codelist(S);is_charlist(S)),catch(format('~s',[S]),_,writeq(S)).
wqs_c(S):- empty_wqs_c(S),!.
wqs_c(S):- var(S),!,write(var(S)).
wqs_c(S):- atom(S),into_title_str(S,TS),write(TS),!.
wqs_c(S):- atom(S),write_atom(S),!.
%wqs_c(S):- atom(S),write(S),!.
wqs_c(S):- \+compound(S),!,notrace(catch(format('~p',[S]),_,write(S))).
wqs_c(title(S)):- !, wqs_c(S).
wqs_c(H+T):- !, wqs_c(H),write_nbsp,wqs_c(T).
wqs_c(S):- is_grid(S), print_grid(S),!.
wqs_c(S):- is_vm(S), pp(S) ,!.
wqs_c(L):- is_list(L), include(non_empty_wqs_c,L,LL),!,wqs_c_l(LL).
wqs_c([H|T]):- pp([H|T]),!.
wqs_c(H):- callable_arity(H,0),is_writer_goal(H),catch(call_e_dmsg(H),_,fail),!.
%wqs_c(H):- callable_arity(H,0),call(H),!.
wqs_c(H):- locally(t_l:wqs_fb(pp_no_nl),wqs(H)),!.

wqs_c_l([]):-!.
wqs_c_l([H]):- wqs_c(H),!.
wqs_c_l([H|T]):- wqs_c(H),write_nbsp,wqs_c_l(T),!.





ppt(_):- is_print_collapsed,!.
ppt(G):- stack_check_or_call(4000,writeq(G)),!.
ppt(G):- is_vm_map(G), !, write_map(G,'ppt').
ppt(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
%ppt(P):- compound(P),wqs1(P),!.

ppt(P):- \+ ansi_main, wants_html,!,ptcol_html(P),write_br.
ppt(P):- \+ \+ ((tersify(P,Q),!,pp(Q))),!.
ppt(Color,P):- \+ ansi_main, wants_html,!,with_color_span(Color,ptcol_html(P)),write_br.
ppt(Color,P):- \+ \+ ((tersify(P,Q),!,pp(Color,Q))),!.


write_br:- ansi_main,!,nl.
write_br:- write('<br>').

ptc(Color,Call):- pp(Color,call(Call)).

:- meta_predicate(ppnl(+)).
ppnl(Term):- is_list(Term),!,g_out(wqs(Term)).
ppnl(Term):- nl_if_needed,format('~q',[Term]),nl_if_needed_ansi.

:- meta_predicate(pp(+)).
pp(Color,P):- \+ ansi_main, wants_html,!,with_color_span(Color,pp(P)),write_br.
pp(Color,P):- ignore((quietlyd((wots_hs(S,pp(P)),!,color_print(Color,S))))).

pp(_):- is_print_collapsed,!.
%pp(Term):- is_toplevel_printing(Term), !, nl_if_needed, pp_no_nl(Term),!,nl_if_needed_ansi.
pp(_Term):- nl_if_needed, fail.
pp(Term):- \+ ansi_main, wants_html,!, wots_vs(SS,ptcol_html_scrollable(Term)),write(SS),write_br.
pp(Term):- \+ nb_current(arc_can_portray,_),!,locally(nb_setval(arc_can_portray,t),print(Term)).
pp(Term):- az_ansi(pp_no_nl(Term)),!,nl_if_needed_ansi.

/*
ptcol(P):- wants_html,!,ptcol_html(P).
ptcol(call(P)):- callable(P),!,call(P).
ptcol(P):- pp(P).
*/

%ptcol_html(P):- ptcol_html_0(P).
ptcol_html(P):- ptcol_html_scrollable_0(P).
ptcol_html_scrollable(P):- with_tag_ats(div,scrollable,ptcol_html_scrollable_0(P)).


ptcol_html_0(P):- with_tag(pre,ptcol_html_wo_pre(P)).
ptcol_html_wo_pre(call(P)):- callable(P),!, in_pp_html(call(P)).
ptcol_html_wo_pre(P):- in_pp_html(print_tree_no_nl(P)).
ptcol_html_scrollable_0(P):- ptcol_html_wo_pre(P).


pp_wcg(G):- wants_html,!,ptcol_html_scrollable(G).
pp_wcg(G):- pp_safe(call((locally(nb_setval(arc_can_portray,t),print(G))))),!.

wqln(Term):- ppnl(Term).
wqnl(G):- pp_safe(call((locally(nb_setval(arc_can_portray,nil),print(G))))),!.

pp_safe(_):- nb_current(pp_hide,t),!.
pp_safe(call(W)):- !, nl_if_needed,nl_now,call(W),nl_now.
pp_safe(W):- nl_if_needed,nl_now,writeq(W),nl_now.
pp_safe(C,W):- color_print(C,call(pp_safe(W))).


%p_p_t_no_nl(Term):- is_toplevel_printing(Term), !, print_tree_no_nl(Term).

p_p_t_no_nl(P):- \+ ansi_main, wants_html,!,ptcol_html(P).
p_p_t_no_nl(Term):- az_ansi(print_tree_no_nl(Term)).

ppt_no_nl(P):- \+ ansi_main, wants_html,!,ptcol_html(P).
ppt_no_nl(P):- tersify(P,Q),!,pp_no_nl(Q).

is_toplevel_printing(_):- \+ is_string_output, line_position(current_output,N),  N<2, fail.

pp_no_nl(P):- var(P),!,pp(var_pt(P)),nop((dumpST,ibreak)).
pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- atom(P),atom_contains(P,'~'),!,format(P).
pp_no_nl(G):- is_vm_map(G), !, write_map(G,'pp').
%pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- \+ \+ (( pt_guess_pretty(P,GP),ptw(GP))).
%pp(P):-!,writeq(P).
%ptw(P):- quietlyd(p_p_t_nl(P)),!.
%ptw(_):- nl_if_needed,fail.
ptw(P):- var(P),!,ptw(var_ptw(P)),nop((dumpST,ibreak)).
ptw(G):- is_vm_map(G), !, write_map(G,'ptw').
ptw(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
ptw(P):- p_p_t_no_nl(P),!.

%ptw(P):- quietlyd(write_term(P,[blobs(portray),quoted(true),quote_non_ascii(false), portray_goal(print_ansi_tree),portray(true)])),!.
print_ansi_tree(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
print_ansi_tree(P,_):- catch(arc_portray(P),_,(never_let_arc_portray_again,fail)),!.
print_ansi_tree(P,_OL):- catch(p_p_t_no_nl(P),_,(never_let_arc_portray_again,fail)),!.

%p_p_t_nl(T):- az_ansi(print_tree_nl(T)).
%p_p_t(T):- az_ansi(print_tree(T)).

pt_guess_pretty(P,O):- \+ nb_current(in_pt_guess_pretty,t), locally(nb_setval(in_pt_guess_pretty,t),pt_guess_pretty_1(P,O)).
pt_guess_pretty(O,O).

upcase_atom_var_l(IntL,NameL):- upcase_atom_var(IntL,NameL).
upcase_atom_var_l(IntL,NameL):- is_list(IntL),!,my_maplist(upcase_atom_var_l,IntL,NameL).

pt_guess_pretty_1(P,O):- copy_term(P,O,_),
  ignore((sub_term_safely(Body,O), compound(Body), Body=was_once(InSet,InVars),upcase_atom_var_l(InSet,InVars))),
  ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
:- module_transparent(pretty_clauses:pp_hook/3).
pretty_clauses:pp_hook(FS,Tab,S):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),  notrace(catch(arc_pp_hook(FS,Tab,S),_,fail)).

arc_pp_hook(_,Tab,S):- term_is_ansi(S), !,prefix_spaces(Tab), write_keeping_ansi_mb(S).
%arc_pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
%arc_pp_hook(_,  _,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
arc_pp_hook(FS,_  ,G):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(is_group/1),
   locally(b_setval(pp_parent,FS),
     print_with_pad(pp_hook_g(G))),!.

pp_parent(PP):- nb_current(pp_parent,PP),!.
pp_parent([]):-!.

%:- meta_predicate(lock_doing(+,+,0)).
:- meta_predicate(lock_doing(+,+,:)).
lock_doing(Lock,G,Goal):-
 (nb_current(Lock,Was);(Was=[],nb_setval(Lock,Was)), !,
  \+ ((member(E,Was),E==G)),
  locally(b_setval(Lock,[G|Was]),Goal).

never_let_arc_portray_again:- set_prolog_flag(never_pp_hook, true),!.
arc_can_portray:- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t).

arcp:will_arc_portray:-
   \+ current_prolog_flag(never_pp_hook, true),
   \+ nb_current(arc_can_portray,f),
   %nb_current(arc_can_portray,t),
   current_prolog_flag(debug,false),
   \+ tracing,
   flag(arc_portray_current_depth,X,X),X<3,
   current_predicate(bfly_startup/0).

user:portray(Grid):-
  arcp:will_arc_portray, \+ \+ catch(quietly(arc_portray(Grid)),_,fail),!, flush_output.


pp_hook_g(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_hook_g(_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
pp_hook_g(S):- term_contains_ansi(S), !, write_nbsp, pp_hook_g0(S).
pp_hook_g(G):- \+ plain_var(G), lock_doing(in_pp_hook_g,G,pp_hook_g0(G)).

pp_hook_g0(S):- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g0(_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
pp_hook_g0(_):- in_pp(bfly),!,fail.
pp_hook_g0(G):- wots_hs(S,in_bfly(f,pp_hook_g10(G))),write(S).

mass_gt1(O1):- into_obj(O1,O2),mass(O2,M),!,M>1.

% Pretty printing
pp_hook_g10(G):- \+ plain_var(G), current_predicate(pp_hook_g1/1), lock_doing(in_pp_hook_g10,G,pp_hook_g1(G)).

%as_grid_string(O,SSS):- is_grid(O),wots_vs(S,print_grid(O)), sformat(SSS,'{  ~w}',[S]).
as_grid_string(O,SSS):- wots_vs(S,show_indiv(O)), sformat(SSS,'{  ~w}',[S]).
as_pre_string(O,SS):- wots_hs(S,show_indiv(O)), strip_vspace(S,SS).


pretty_grid(O):-
  catch(
  (wots_hs(S,print_grid(O)),strip_vspace(S,SS),
   ptc(orange,(format('"  ~w  "',[SS])))),
  _,fail),!.
/*
pretty_grid(O):-
  catch(
  (wots_hs(S,print_grid(O)),strip_vspace(S,SS),
   ptc(orange,(format('"  ~w  "',[SS])))),
  _,(never_let_arc_portray_again,fail)).
*/
pp_hook_g1(O):-  plain_var(O), !, fail.

pp_hook_g1(O):-  attvar(O), !, is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.
pp_hook_g1(S):-  term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
%pp_hook_g1(S):-  term_contains_ansi(S), !, fail, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g1(rhs(O)):- write_nbsp,nl,bold_print(print(r_h_s(O))),!.

pp_hook_g1(iz(O)):- compound(O), O = info(_),underline_print(print(izz(O))),!.
pp_hook_g1(O):-  is_grid(O), /* \+ (sub_term_safely(E,O),compound(E),E='$VAR'(_)), */ pretty_grid(O).


pp_hook_g1(O):- is_object(O), into_solid_grid(O,G), wots(SS,pretty_grid(G)),write(og(SS)),!.

pp_hook_g1(shape_rep(grav,O)):- is_points_list(O), as_grid_string(O,S), wotsq(O,Q), print(shape_rep(grav,S,Q)),!.
pp_hook_g1(vals(O)):- !, writeq(vals(O)),!.
%pp_hook_g1(l2r(O)):- into_solid_grid_strings(l2r(O),Str),Str\=@=l2r(O),print_term_no_nl(Str),!.
pp_hook_g1(localpoints(O)):- is_points_list(O), as_grid_string(O,S), wotsq(O,Q), print(localpoints(S,Q)),!.
pp_hook_g1(C):- compound(C), compound_name_arguments(C,F,[O]),is_points_list(O), length(O,N),N>2, as_grid_string(O,S), compound_name_arguments(CO,F,[S]), print(CO),!.

pp_hook_g1(O):-  is_points_list(O),as_grid_string(O,S),write(S),!.
pp_hook_g1(O):-  is_real_color(O), color_print(O,call(writeq(O))),!.
pp_hook_g1(O):-  is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.

pp_hook_g1(_):-  \+ in_pp(ansi),!, fail.


pp_hook_g1(Grp):- current_predicate(pp_ilp/1),is_rule_mapping(Grp),pp_ilp(Grp),!.

pp_hook_g1(O):- atom(O), atom_contains(O,'o_'), pp_parent([LF|_]), \+ (LF==lf;LF==objFn),
  resolve_reference(O,Var), O\==Var, \+ plain_var(Var),!,
  write_nbsp, writeq(O), write(' /* '), show_indiv(Var), write(' */ ').

pp_hook_g1(O):-  is_object(O),pp_no_nl(O), !.
pp_hook_g1(O):-  is_group(O),pp_no_nl(O), !.

%pp_hook_g1(change_obj(N,O1,O2,Sames,Diffs)):-  showdiff_objects5(N,O1,O2,Sames,Diffs),!.

pp_hook_g1(O):-  is_vm_map(O),data_type(O,DT), writeq('..map.'(DT)),!.
pp_hook_g1(O):-  is_gridoid(O),show_indiv(O), !.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  with_tagged('h5',w_section(object,[O1, O2],pp(O))).
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff), w_section(showdiff_objects(O1,O2)),!.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  w_section(object,[O1, O2],with_tagged('h5',pp(O))).
%pp_hook_g1(O):-  O = diff(A -> B), (is_gridoid(A);is_gridoid(B)),!, p_c_o('diff', [A, '-->', B]),!.
pp_hook_g1(O):-  O = showdiff( O1, O2), !, showdiff(O1, O2).
%pp_hook_g1(O):- compound(O),wqs1(O), !.
pp_hook_g1(O):- \+ compound(O),fail.
pp_hook_g1(G):- '@'(pp_hook_g1a(G),user).

pp_hook_g1a(G):- \+ current_prolog_flag(debug,true),
  current_predicate(pp_hook_g2/1), lock_doing(in_pp_hook_g3,any,pp_hook_g2(G)),!.
pp_hook_g1a(G):- fch(G),!.

%pp_hook_g2(O):- current_predicate(colorize_oterms/2),colorize_oterms(O,C), notrace(catch(fch(C),_,fail)),! .

fch(O):- wqs1(O).
%fch(O):- pp_no_nl(O).
%fch(O):- print(O).
%fch(O):- p_p_t_no_nl(O).

wotsq(O,Q):- wots_hs(Q,wqnl(O)).
has_goals(G):- term_attvars(G,AV),AV\==[].
has_goals(G):- term_variables(G,TV),term_singletons(G,SV),TV\==SV.

maybe_term_goals(Term,TermC,Goals):-
  term_attvars(Term,Attvars), Attvars\==[],!,
  term_variables(Term,Vars),
  include(not_in(Attvars),Vars,PlainVars),
  copy_term((Attvars+PlainVars+Term),(AttvarsC+PlainVarsC+TermC),Goals),
  numbervars(PlainVarsC,10,Ten1,[singletons(true),attvar(skip)]),
  numbervars(AttvarsC+Goals,Ten1,_Ten,[attvar(bind),singletons(false)]).

maybe_replace_vars([],SGoals,TermC,SGoals,TermC):-!.
maybe_replace_vars([V|VarsC],SGoals,TermC,RSGoals,RTermC):-
   my_partition(sub_var_safely(V),SGoals,Withvar,WithoutVar),
   Withvar=[OneGoal],
   freeze(OneGoal,(OneGoal\==null,OneGoal \== @(null))),
   findall(_,sub_var_safely(V,TermC),LL),LL=[_],!,
   subst([WithoutVar,TermC],V,{OneGoal},[SGoalsM,TermCM]),
   maybe_replace_vars(VarsC,SGoalsM,TermCM,RSGoals,RTermC).
maybe_replace_vars([_|VarsC],SGoals,TermC,RSGoals,RTermC):-
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC).


src_sameish(Orig,Find):- copy_term(Orig,COrig),Find=Orig,Orig=@=COrig.

number_vars_calc_goals(Term,SSRTermC,[1|SRSGoals]):-
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  notrace(catch(numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(skip)]),_,fail)),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst_1L_p2(src_sameish,[
    {dif('$VAR'('__'),RED)}=dif(RED),
    {cbg('$VAR'('__'))}=cbg],
     SRTermC,SSRTermC),!.

number_vars_calc_goals(Term,SRTermC,[2|RSGoals]):-
  term_attvars(Term,AVars),
  copy_term(Term+AVars,TermC+VarsC,GoalsI),
  term_attvars(GoalsI,GAttvars), copy_term(GoalsI+GAttvars,_+GAttvarsC,GoalsGoals),
  append(GoalsI,GoalsGoals,Goals),
  append([VarsC,GAttvarsC,AVars,GAttvars],SortVars),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,SortVars,SGoals),
  maybe_replace_vars(SortVars,SGoals,TermC,RSGoals,RTermC),
  subst_1L_p2(src_sameish,[
    {dif('$VAR'('___'),RED)}=dif(RED),
    {cbg('$VAR'('___'))}=cbg],
     RTermC,SRTermC),!.

number_vars_calc_goals(Term,SSRTermC,[3|SRSGoals]):-
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst(SRTermC,{cbg('_')},cbg,SSRTermC),!.

number_vars_calc_goals(Term,TermC,[4|SGoals]):-
  term_variables(Term,Vars),
  term_attvars(Term,Attvars),
  copy_term(Term+Vars+Attvars,TermC+VarsC+AttvarsC,Goals),
  notrace(catch(numbervars(TermC+Goals,0,_Ten1,[singletons(true)]),_,fail)),
  append([AttvarsC,VarsC,AttvarsC,Vars],Sorted),
  sort_goals(Goals,Sorted,SGoals),!.

number_vars_calc_goals(Term,TermC,[5|SGoals]):-
  term_variables(Term,Vars),
  term_attvars(Term,Attvars),
  copy_term(Term+Vars+Attvars,TermC+VarsC+AttvarsC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(skip)]),
  append([AttvarsC,VarsC,Attvars,Vars],Sorted),
  sort_goals(Goals,Sorted,SGoals),!.



writeg(Term):- ignore( \+ notrace(catch(once(writeg0(Term);ppa(Term)),E,(pp(E),ppa(Term))))),!.

writeg0(Term):- term_attvars(Term,Attvars),Attvars\==[],!,
  must_det_ll(((number_vars_calc_goals(Term,TermC,Goals),
  writeg5(TermC)),!,
  if_t(Goals\==[],(nl_if_needed,
    write(' goals='), call_w_pad_prev(3,az_ansi(print_tree_no_nl(Goals))))))),!.

writeg0(Term):- \+ ground(Term),
 \+ \+ must_det_ll((
  numbervars(Term,0,_Ten1,[singletons(true),attvar(skip)]), writeg5(Term))).
writeg0(Term):- writeg5(Term),!.

writeg5(X):- is_ftVar(X),!,write_nbsp,write_nbsp,print(X),write_nbsp.
writeg5(N=V):- is_simple_2x2(V),!,print_grid(N,V),writeln(' = '),call_w_pad_prev(2,writeg9(V)).
writeg5(N=V):- is_gridoid(V),!,print_grid(N,V),writeln(' = '),call_w_pad_prev(2,writeg9(V)).
writeg5(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad_prev(2,writeg5(V)).
writeg5(_):- write_nbsp, fail.
writeg5(V):- writeg9(V).

writeg8(X):- is_ftVar(X),!,print(X).
writeg8(X):- var(X),!,print(X).
writeg8(X):- writeq(X).

writeg9(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg9(V):- is_list(V),nl_if_needed,write('['),!,my_maplist(writeg5,V),write(']').
writeg9(_):- write_nbsp,write(' \t '),fail.
writeg9(X):- is_ftVar(X),!,write_nbsp,write_nbsp,print(X).
writeg9(V):- pp_no_nl(V).


/*
writeg5(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg5(V):- is_gridoid(V),!,call_w_pad_prev(2,writeg9(V)).
writeg5(V):- is_list(V),nl_if_needed,write('['),my_maplist(writeg5,V),write(']').
*/
arg1_near(Vars,Goal,Nth):- tc_arg(1,Goal,PreSort),nth1(Nth,Vars,E),E==PreSort,!.
arg1_near(_VarsC,Goal,PreSort):- tc_arg(1,Goal,PreSort),!.
arg1_near(_VarsC,Goal,Goal).

sort_goals(Goals,VarsC,SGoals):- predsort(sort_on(arg1_near(VarsC)),Goals,SGoals).

/*

writeg0(Obj):- is_object(Obj),pp(Obj),!.
writeg0(O):- writeg00(O).

writeg00(Term):-
  maybe_term_goals(Term,TermC,Goals),
  writeg00(TermC), call_w_pad(2,writeg00(Goals)),!.
writeg00(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad(2,writeg00(V)).
writeg00(O):- compound(O),compound_name_arguments(O,F,[A]),!,call_w_pad(2,((writeq(F),write('('),writeg3(A),write(')')))).
writeg00(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg00([H|T]):- compound(H),H=(_=_), my_maplist(writeg0,[H|T]).
writeg00([H|T]):- is_list(T),call_w_pad(2,((nl,write('['),writeg2(H),my_maplist(writeg0,T),write(']'),nl))).
%writeg0(Term):- \+ ground(Term),!, \+ \+ (numbervars(Term,99799,_,[singletons(true)]),
%   subst(Term,'$VAR'('_'),'$VAR'('_____'),TermO), writeg0(TermO)).
%writeg0(V):- \+ is_list(V),!,writeq(V),nl_now.
writeg00(V):- \+ is_list(V),!,pp(V).
writeg00(X):- call_w_pad(2,pp(X)).

writeg1(N=V):- is_gridoid(V),!,print_grid(N,V),call_w_pad(2,(my_maplist(writeg1,V))).
writeg1(X):- nl_if_needed,writeg2(X),!,write_nbsp,!.
writeg2(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- is_ftVar(X),!,print(X).
writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(true)]),!.
%writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(false)]),!.
%writeg1(X):- nl_if_needed,writeg(X).
writeg2(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- writeq(X),!.
writeg3(X):- is_list(X),X\==[],X=[_,_|_],!,writeg(X).
writeg3(X):- writeg2(X).
*/

% Nov 9th, 1989
/*
pp_hook_g1(T):-
 nb_current('$portraying',Was)
   ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T)))
   ; locally(b_setval('$portraying',[T]),ptv0(T)).
*/

%pp_hook_g(G):- compound(G),ppt(G),!.
%pp_hook_g(G):- ppt(G),!.


strip_vspace(S,Stripped):- string_concat(' ',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,' ',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\n',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\n',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\t',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\t',S),!,strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- replace_in_string([" \n"="\n","(   "="(  ","(\n"="( "],S,S2),S2\==S,!,strip_vspace(S2,Stripped).
%strip_vspace(S,Stripped):- split_string(S, "", "\t\r\n", [Stripped]).
strip_vspace(S,S).


print_nl(P):- nl_if_needed, wots_hs(SS,pp_no_nl(P)), maybe_color(SS,P),nl_if_needed.

color_write(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
color_write(P):- wots_hs(SS,write(P)), maybe_color(SS,P).

write_keeping_ansi_mb(P):- is_maybe_bold(P,write_keeping_ansi(P)).

is_maybe_bold(P):- sformat(S,'~w',[P]),atom_contains(S,'stOF').

is_maybe_bold(P,G):- is_maybe_bold(P),!, underline_print(bold_print(G)).
is_maybe_bold(_P,G):- call(G).

pp_msg_color(P,C):- compound(P),pc_msg_color(P,C),!.
pp_msg_color(P,C):- must_det_ll(mesg_color(P,C)).
pc_msg_color(iz(P),C):- pp_msg_color(P,C).
pc_msg_color(link(P,_,_),C):- pp_msg_color(P,C).
pc_msg_color(link(P,_),C):- pp_msg_color(P,C).
pc_msg_color((_->P),C):- pp_msg_color(P,C).
pc_msg_color([P|_],C):- pp_msg_color(P,C).
pc_msg_color(diff(P),C):- pp_msg_color(P,C).

%:- meta_predicate(wots_hs(0)).
%wots_hs(G):- wots_hs(S,G),write(S).

:- meta_predicate(wots_ansi(-,0)).
wots_ansi(S,Goal):- wots(S,woto_ansi(Goal)).
:- meta_predicate(wots_ansi(-,0)).
wots_html(S,Goal):- wots(S,woto_html(Goal)).

:- meta_predicate(woto_ansi(0)).
woto_ansi(Goal):- with_toplevel_pp(ansi,Goal).
:- meta_predicate(woto_html(0)).
woto_html(Goal):- with_toplevel_pp(http,Goal).

:- meta_predicate(wots_hs(-,0)).
%wots_hs(S,G):- \+ wants_html,!,wots(S,G).
%wots_hs(S,G):- wots(S,G),!.
wots_hs(S,G):- wots(SS,G),notrace(remove_huge_spaces(SS,S)).
:- meta_predicate(wots_vs(-,0)).
wots_vs(OOO,G):- wots(S,G),notrace(fix_vspace(S,OOO)).

fix_vspace(S,OOO):-
 strip_vspace(S,SS), (atom_contains(SS,'\n') ->
   wots_hs(SSS,(nl_now,write('   '),write(SS),nl_now));SSS=SS),
   remove_huge_spaces(SSS,OOO).


write_tall(L):- is_list(L),!,my_maplist(write_tall,L).
write_tall(E):- wots_vs(S,wqs_c(E)),writeln(S).
write_wide(L):- is_list(L),!,my_maplist(write_wide,L).
write_wide(E):- wots_vs(S,wqs_c(E)),write(S),write_nbsp.

p_to_br(S,SS):- fix_br_nls(S,S0),
  cr_to_br(S0,SSS),
  replace_in_string(['<p>'='<br>','<br/>'='<br>','</p>'=' ','<p/>'='<br>','<br><br>'='<br>'],SSS,SSSS),
  cr_to_br(SSSS,SS).

cr_to_br(S,SSS):- wants_html,!,cr_to_br_html(S,SSS).
cr_to_br(S,SSS):- cr_to_br_ansi(S,SSS).

cr_to_br_html(S,SSS):- replace_in_string(['\r\n'='<br>','\r'='<br>','\n'='<br>'],S,SSS).
cr_to_br_ansi(S,SSS):- replace_in_string(['<br>'='\n','&nbsp;'=' '],S,SSS).

fix_br_nls(S,O):- replace_in_string(
 ['<br/>\n'='<br/>','<br>\n'='<br>','</p>\n'='</p>','<p/>\n'='<p/>','<p>\n'='<p>',
  '\n<br>'='<br>','\n<br/>'='<br/>','\n</p>'='</p>','\n<p/>'='<p/>','\n<p>'='<p>'],S,O).

remove_huge_spaces(S,O):- notrace((fix_br_nls(S,SS),!,p_to_br(SS,O))),!.
/*
remove_huge_spaces(S,O):- fix_br_nls(S,S0),
  replace_in_string(['          '='     ',
    '                                                                          '='     ',
    '                                                                          '='     ',
    '                                                                                                                                                                                                                                                                                                                                                                                                               '='  ',
    '                                                                                                                                                                                                                                                                                   '='   ',
    '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        '='    ',
    '                                                                          '='     ',
    '\t'='  ',
    '                         '='     '],S0,SS),p_to_br(SS,O).
*/


wqs_l(H):- \+ is_list(H),!, wqs(H).
wqs_l(H):- wqs(H).

wqs(P):- wots_hs(SS,wqs0(P)), maybe_color(SS,P).
wqs(C,P):- ansicall(C,wqs0(P)),!.

wqs0(X):- plain_var(X), wqs(plain_var(X)),!.
wqs0(X):- plain_var(X), !, wqs(plain_var(X)), ibreak.
wqs0(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs0(C):- is_colorish(C),color_print(C,C),!.
wqs0(G):- is_vm_map(G), !, write_map(G,'wqs').
wqs0(X):- var(X), !, get_attrs(X,AVs),!,writeq(X),write('/*{'),print(AVs),write('}*/').
wqs0(X):- attvar(X), !, wqs(attvar(X)).
wqs0(nl_now):- !, nl_now. wqs0(X):- X=='', !. wqs0(X):- X==[], !.
wqs0(X):- is_grid(X), !, print_grid(X).
wqs0(G):- compound(G), G = call(C),callable(C),!,call(C).
wqs0([T]):- !, wqs(T).
wqs0([H|T]):- string(H), !, write(H), write_nbsp, wqs(T).
wqs0([H|T]):- compound(H),skip(_)=H, !,wqs(T).
wqs0([H|T]):- wqs(H), need_nl(H,T), wqs(T), !.
wqs0(X):- is_object(X), tersify1(X,Q), X\==Q,!, wqs(Q).
wqs0(X):- is_object(X), show_shape(X),!.
wqs0(X):- string(X), atom_contains(X,'~'), catch((sformat(S,X,[]),color_write(S)),_,fail),!.
wqs0(X):- string(X), !, color_write(X).
%wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
%wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
%wqs([H|T]):- compound(H),!, writeq(H), wqs(T).

wqs0(call(C)):- !, call(C).
wqs0(X):- \+ compound(X),!, write_nbsp, write(X).
wqs0(C):- compound(C),wqs1(C),!.
wqs0(C):- wqs2(C).
%wqs(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

wqs2(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
%wqs2(P):- wants_html,!,pp(P).

:- thread_local(t_l:wqs_fb/1).
wqs2(X):- t_l:wqs_fb(P1),call(P1,X),!.
%wqs2(X):- with_wqs_fb(writeq,X).
wqs2(X):- with_wqs_fb(writeq,print(X)),!.
%wqs2(X):- with_wqs_fb(writeq,((write_nbsp,write_term(X,[quoted(true)])))).

with_wqs_fb(FB,Goal):-
  locally(t_l:wqs_fb(FB),Goal).


as_arg_str(C,S):- wots_vs(S,print(C)).

arg_string(S):- string(S),!.
arg_string(S):- term_contains_ansi(S),!.

wqs1(C):- \+ compound(C),!,wqs0(C).
wqs1(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).

wqs1(format(C,N)):- catch((sformat(S,C,N),color_write(S)),_,fail),!.
wqs1(writef(C,N)):- !, writef(C,N).
wqs1(q(C)):-  \+ arg_string(C),wots_hs(S,writeq(C)),color_write(S),!.
wqs1(g(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs1(C))),print(g(S)),!.
wqs1(print_ss(C)):-  \+ arg_string(C), wots_vs(S,print_ss(C)),wqs1(print_ss(S)),!.
wqs1(b(C)):-  \+ arg_string(C), wots_vs(S,bold_print(wqs1(C))),color_write(S).
wqs1(T):- \+ is_list(T), term_contains_ansi(T),!,write_keeping_ansi_mb(T).
wqs1(norm(C)):- writeq(norm(C)),!.
wqs1(grid_rep(norm,C)):- writeq(grid_rep(norm,C)),!.
wqs1(grid(C)):- writeq(grid(C)),!.
wqs1(rhs(RHS)):- nl_now,wqnl(rhs(RHS)),nl_now.
%wqs1(grid_ops(norm,C)):- writeq(norm(C)),!.
%norm_grid

wqs1(pp(P)):-  wots_vs(S,pp_no_nl(P)),write((S)).
wqs1(ppt(P)):- wots_vs(S,ppt_no_nl(P)),write((S)).
wqs1(wqs(P)):- wots_vs(S,wqs(P)),write((S)).
wqs1(wqs(C,P)):- wots_vs(S,wqs(P)),color_print(C,S).

wqs1(vals(C)):- writeq(vals(C)),!.
%wqs1(colors_cc(C)):- \+ arg_string(C), as_arg_str(C,S),wqs(colorsz(S)).
wqs1(io(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs(C))),write(io(S)).

wqs1(uc(C,W)):- !, write_nbsp, color_print(C,call(underline_print(format("\t~@",[wqs(W)])))).
wqs1(cc(C,N)):- is_color(C),!,color_print(C,call(writeq(cc(C,N)))).
wqs1(write_nav_cmd(C,N)):- !, write_nav_cmd(C,N).

wqs1(-(C,N)):- is_color(C),!,color_print(C,call(writeq(C))), write('-'), wqs(N).
wqs1(cc(C,N)):- N\==0,attvar(C), get_attrs(C,PC), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- N\==0,var(C), sformat(PC,"~p",[C]), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- \+ arg_string(C), wots_hs(S,color_print(C,C)), wqs(cc(S,N)).
wqs1(color_print(C,X)):- is_color(C), !, write_nbsp, color_print(C,X).
wqs1(color_print(C,X)):- \+ plain_var(C), !, write_nbsp, color_print(C,X).
wqs1(X):- into_f_arg1(X,_,Arg),is_gridoid(Arg),area_or_len(Arg,Area),Area<5,writeq(X),!.
% wqs1(C):- callable(C), is_wqs(C),wots_vs(S,catch(C,_,fail)),write((S)).
wqs1(X):- is_gridoid_arg1(X), print_gridoid_arg1(X).

into_f_arg1(X,F,Arg):- compound(X), compound_name_arguments(X,F,[Arg]), compound(Arg).

is_gridoid_arg1(X):- into_f_arg1(X,_F,Arg),is_gridoid(Arg).
print_gridoid_arg1(X):- into_f_arg1(X,F,Arg),print_gridoid_arg1(F,Arg).

print_gridoid_arg1(F,Arg):- \+ wants_html,!, wots_vs(VS,wqs(Arg)), writeq(F),write('(`'),!, print_with_pad(write(VS)),write('`)').
print_gridoid_arg1(F,Arg):- wots_vs(VS,wqs(Arg)),
 with_tag_style(span,"display: inline; white-space: nowrap",(writeq(F),write('({'),!,write(VS),write('})'))).


nl_needed(N):- line_position(current_output,L1),L1>=N.

nl_now :- wants_html,!,nl_if_needed_ansi.
nl_now :- nl.

ansi_in_pre:- current_predicate(in_pre/0),in_pre.
nl_if_needed :- ansi_main,!, format('~N').
nl_if_needed :- ansi_in_pre,ignore((nl_needed(11),write('<br/>'))),!.
nl_if_needed :- wants_html,!,ignore((nl_needed(11),write('<br/>\n'))).
nl_if_needed :- format('~N').
nl_if_needed_ansi :- \+ ansi_main, wants_html,!.
nl_if_needed_ansi :- nl_if_needed.

write_nbsp:- ansi_main,!,write(' ').
write_nbsp:- wants_html,!,write('&nbsp;').
write_nbsp:- write(' ').

is_breaker(P):- compound(P),functor(P,_,A), A>=3.

last_f(H,F):- \+ compound(H),data_type(H,F).
last_f(H,F/A):- compound(H),!,functor(H,F,A).

need_nl(_,_):- line_position(current_output,L1),L1<40,!.
need_nl(_,_):- line_position(current_output,L1),L1>160,!,nl_if_needed.
need_nl(H0,[H1,H2|_]):- H1\=cc(_,_), last_f(H0,F0),last_f(H1,F1),last_f(H2,F2), F0\==F1, F1==F2,!,format('~N  ').
%need_nl(H0,[H1|_]):- last_f(H0,F0),last_f(H1,F1), F0==F1, !, write_nbsp.
need_nl(_,_).
/*
need_nl(_Last,[H|_]):- last_f(H,F),
 once(nb_current(last_h,cc(LF,C));(LF=F,C=0)),
   (LF==F-> (write_nbsp, plus(C,1,CC), nb_setval(last_h,cc(F,CC))) ; ((C>2 -> nl_now ; write_nbsp), nb_setval(last_h,cc(F,0)))).

need_nl(_,_):- wants_html,!,write_nbsp.
%need_nl(_,_):- !,write_nbsp.
need_nl(H,[P|_]):- \+ is_breaker(H),is_breaker(P),line_position(user_output,L1),L1>80,nl_now,bformatc1('\t\t').
need_nl(_,_):- line_position(user_output,L1),L1>160,nl_now,bformatc1('\t\t').
need_nl(_,_).
*/

dash_chars:- wants_html,!,section_break.
dash_chars:- dash_chars(40),!.

dash_chars(_):- wants_html,!,section_break.
dash_chars(H):- integer(H), dash_border(H).
dash_chars(S):- nl_if_needed,dash_chars(60,S),nl_if_needed_ansi.
dash_chars(_,_):- wants_html,!,section_break.
dash_chars(H,_):- H < 1,!.
dash_chars(H,C):- forall(between(0,H,_),bformatc1(C)).

%section_break:- wants_html,!,write('<p><hr></p>').
section_break.
%dash_uborder_no_nl_1:-  line_position(current_output,0),!, bformatc1('\u00AF\u00AF\u00AF ').
%dash_uborder_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- uborder(Short,Long),!, bformatc1(Short),bformatc1(Long),write_nbsp.
dash_uborder_no_nl(1):- !, dash_uborder_no_nl_1.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, uborder(Short,Long),write_nbsp, write(Short),dash_chars(WidthM1,Long),!.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write_nbsp, bformat('\u00AF'),dash_chars(WidthM1,'\u00AF\u00AF'),!.
dash_uborder_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' \u00AF'),dash_chars(WidthM1,'\u00AF\u00AF').

dash_uborder(Width):- nl_if_needed,dash_uborder_no_nl(Width),nl_now.

uborder('-','--'):- stream_property(current_output,encoding(utf8)),!.
uborder('\u00AF','\u00AF\u00AF'):- !. %stream_property(current_output,encoding(text)).
%uborder('-','--').

dash_border_no_nl_1:-  line_position(current_output,0),!, bformatc1(' ___ ').
dash_border_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('___ ').
dash_border_no_nl_1:- bformatc1(' ___ ').

%dash_border_no_nl(Width):- write(''),dash_chars(Width,'_'),write_nbsp,!.

dash_border_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' _'),dash_chars(WidthM1,'__').

dash_border(Width):- !, dash_border_no_nl(Width),nl_now,!.

functor_test_color(pass,green).
functor_test_color(fail,red).
functor_test_color(warn,yellow).

arcdbg(G):- is_vm_map(G), !, write_map(G,'arcdbg').
arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_test_color(F,C),
  wots_hs(S,print(G)),color_print(C,S),!,nl_if_needed_ansi.
arcdbg(G):- u_dmsg(G).


%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- quietlyd((is_object(Grid),print_grid(Grid))).
n_times(N,Goal):- forall(between(1,N,_),ignore(Goal)).
banner_lines(Color):- banner_lines(Color,1).
banner_lines(Color,N):- wants_html,!,format('\n<hr style="border: ~wpx solid ~w">\n',[N,Color]),!.
banner_lines(Color,N):-
 must_det_ll((nl_if_needed,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now,
  n_times(N,color_print(Color,'=================================================')),nl_now,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now,
  n_times(N,color_print(Color,'=================================================')),nl_now,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now)),!.

print_sso(A):- ( \+ compound(A) ; \+ (sub_term_safely(E,A), is_gridoid(E))),!, u_dmsg(print_sso(A)),!.
print_sso(A):- grid_footer(A,G,W),writeln(print_sso(W)), print_grid(W,G),!.
print_sso(A):- must_det_ll(( nl_if_needed, into_ss_string(A,SS),!,
  SS = ss(L,Lst),
  writeln(print_sso(l(L))),
  forall(member(S,Lst),writeln(S)),nl_if_needed)),!.

var_or_number(V):- var(V),!.
var_or_number(V):- integer(V),!.


find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

:- meta_predicate( print_with_pad(0)).
:- export( print_with_pad/1).
/*print_with_pad(Goal):-

  (line_position(current_output,O);O=0),!,
  O1 is O+1,
  call_w_pad(O1,Goal).
*/
print_with_pad(Goal):-(line_position(current_output,O);O=0),!,  O1 is O+1,wots(S,Goal),print_w_pad(O1,S).


into_s(Text,S):- notrace(catch(text_to_string(Text,S),_,fail)),!.
into_s(Obj,S):- wots_hs(S,pp(Obj)),!.

print_w_pad(Pad,Text):- into_s(Text,S), atomics_to_string(L,'\n',S)-> my_maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- nl_if_needed,dash_chars(Pad,' '), write(S).


:- meta_predicate(call_w_pad_prev(+,0)).
call_w_pad_prev(Pad,Goal):- wots_hs(S,Goal), print_w_pad(Pad,S).

%call_w_pad(N,Goal):- wants_html,!,format('<span style="margin-left:~w0%;">',[N]),call_cleanup(call(Goal),write('</span>')).
:- meta_predicate(call_w_pad(+,0)).
call_w_pad(_N,Goal):- wants_html,!,format('<span style="margin-left:10px;">',[]),call_cleanup(call(Goal),write('</span>')).
call_w_pad(N,Goal):- nl_if_needed,wots_hs(S,dash_chars(N,' ')),!,pre_pend_each_line(S,Goal).
maybe_print_pre_pended(Out,Pre,S):- atomics_to_string(L,'\n',S), maybe_print_pre_pended_L(Out,Pre,L).
maybe_print_pre_pended_L(Out,_,[L]):- write(Out,L),!,flush_output(Out).
maybe_print_pre_pended_L(Out,Pre,[H|L]):- write(Out,H),nl(Out),!,write(Out,Pre),maybe_print_pre_pended_L(Out,Pre,L).

%pre_pend_each_line(_,Goal):- !,ignore(Goal).
:- meta_predicate(pre_pend_each_line(+,0)).
pre_pend_each_line(Pre,Goal):- write(Pre),pre_pend_each_line0(Pre,Goal).
pre_pend_each_line0(Pre,Goal):-
  current_output(Out),
  current_predicate(predicate_streams:new_predicate_output_stream/2),!,
  call(call,predicate_streams:new_predicate_output_stream([Data]>>maybe_print_pre_pended(Out,Pre,Data),Stream)),
  arc_set_stream(Stream,tty(true)),
  %arc_set_stream(Stream,buffer(false)),
  %undo(ignore(catch(close(Stream),_,true))),!,
  setup_call_cleanup(true,
   (with_output_to_each(Stream,once(Goal)),flush_output(Stream)),
    ignore(catch(close(Stream),_,true))),!.
pre_pend_each_line0(Pre,Goal):-
  with_output_to_each(string(Str),Goal)*->once((maybe_print_pre_pended(current_output,Pre,Str),nl_if_needed)).



end_of_file.



run_source_code(ShareVars, SourceCode, Vs, QQ):-
  QQ = source_buffer(SourceCode,Vs),!,
  %print(term=Sourcecode -> vs=Vs),
  maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; maplist(mort,SourceCode)).

run_source_code(ShareVars, Vs, QQ):-
  QQ = source_buffer(SourceCode,Vs),!,
  %print(term=Sourcecode -> vs=Vs),
  maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; maplist(mort,SourceCode)).


%vars_to_dictation([_=Value|Gotten],TIn,TOut):- is_vm_map(Value),!, vars_to_dictation(Gotten,TIn,TOut).

vars_to_dictation([Name=Value|Gotten],TIn,TOut):- !,
  my_assertion(atom(Name)),
  vars_to_dictation(Gotten,TIn,TMid),
  to_prop_name(Name,UName),
  tio_tersify(Value,ValueT),!,
  put_dict(UName,TMid,ValueT,TOut).

vars_to_dictation([NameValue|Gotten],TIn,TOut):- !,
  vars_to_dictation(Gotten,TIn,TMid),
  to_prop_name(NameValue,UName),
  tio_tersify(NameValue,ValueT),!,
  put_dict(UName,TMid,ValueT,TOut).

vars_to_dictation([NameValue|Gotten],TIn,TOut):- compound(NameValue),compound_name_arguments(NameValue,Name,Value),!,
  vars_to_dictation([Name=Value|Gotten],TIn,TOut).

vars_to_dictation([],T,T).

tio_tersify(Value,ValueT):- is_grid(Value),!,ValueT=_.
tio_tersify(Value,Value).
:- export(copy_qq_//1).

copy_qq_([]) --> [].
copy_qq_([C|Cs]) --> [C], copy_qq_(Cs).

:- export(copy_qq//1).
muarc:copy_qq(A) --> copy_qq_(Cs), {atom_codes(A, Cs)}.

to_prop_name(Name=_,UName):- nonvar(Name),!,to_prop_name(Name,UName).
to_prop_name(Name,UName):- compound(Name),compound_name_arity(Name,F,_),!,to_prop_name(F,UName).
to_prop_name(Name,UName):- to_case_breaks(Name,Breaks),xtis_to_atomic(Breaks,UName).

xtis_to_atomic([xti(Str,upper),xti(StrL,lower)|Breaks],StrO):- string_upper(Str,Str),
   symbol_chars(Str,CharsList),append(Left,[U],CharsList),
   name(S1,Left),symbolic_list_concat([S1,'_',U,StrL],'',StrUL),!,
   xtis_to_atomic([xti(StrUL,lower)|Breaks],StrO).
xtis_to_atomic([],'').
xtis_to_atomic([xti(Str,_)],Lower):- downcase_atom(Str,Lower).
xtis_to_atomic([XTI|Breaks],Atomic):-
  xtis_to_atomic([XTI],S1),xtis_to_atomic(Breaks,S2),!,symbolic_list_concat([S1,S2],'_',Atomic).

share_vars(Vs,Name=Value):- member(VName=VValue,Vs),VName==Name,!,(Value=VValue->true;trace_or_throw(cant(share_vars(Vs,Name=Value)))).
share_vars(_,Name=_):- string_concat('_',_,Name),!. % Hide some vars
share_vars(V,Name=Value):- fbug(missing(share_vars(V,Name=Value))),!.



parse_expansions(_,Vs,Vs,Src,Src):- \+ compound(Src),!.
parse_expansions(_,Vs0,Vs,dont_include(Var),nop(dont_include(Var))):-
  dont_include_var(Vs0,Vs,Var),!.
parse_expansions(F, Vs0,Vs,[Src0|Sourcecode0],[Src|Sourcecode]):- !,
  parse_expansions(F, Vs0, Vs1, Src0, Src),
  parse_expansions(F, Vs1, Vs, Sourcecode0, Sourcecode).
parse_expansions(FF, Vs0, Vs, Cmpd0, Cmpd):-
  compound_name_arguments(Cmpd0,F,Args0),
  parse_expansions([F|FF], Vs0, Vs, Args0,Args),
  compound_name_arguments(Cmpd,F,Args).

dont_include_var(Vs0,Vs,Var):- select(_=VV,Vs0,Vs),VV==Var,!.
dont_include_var(Vs,Vs,_).

append_sets(Sets,Set):- append(Sets,List),list_to_set(List,Set).
append_sets(Set1,Set2,Set):- append(Set1,Set2,List),list_to_set(List,Set).
flatten_sets(Sets,Set):- flatten(Sets,List),list_to_set(List,Set).

print_prop_val(N=V):- to_prop_name(N,P),format('~N\t\t'),print(P=V),nl.


ignore_numvars(Name='$VAR'(Name)).


%!  example0(+Input) is det.
%
%   An example predicate that always fails, regardless of the input.
%
example0(_) :- fail.

%!  example1(+Input) is nondet.
%
%   Succeeds only for the input `a`. Fails for any other input.
%
example1(a).
example1(_) :- fail.

%!  example2(+Input) is nondet.
%
%   Succeeds for `a` and `b`. Fails for any other input.
%
example2(a).
example2(b).
example2(_) :- fail.

%!  example3(+Input) is nondet.
%
%   Succeeds for `a`, `b`, and `c`. Fails for any other input.
%
example3(a).
example3(b).
example3(c).
example3(_) :- fail.

