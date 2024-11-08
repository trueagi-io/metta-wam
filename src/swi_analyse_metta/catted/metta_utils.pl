/* Set flags to control Prolog's verbosity */
:- set_prolog_flag(verbose_autoload, false). % Disables verbose messages during autoload
:- set_prolog_flag(verbose, silent).         % Sets verbose mode to silent globally
:- set_prolog_flag(verbose_load, silent).    % Suppresses load messages

/* Ensure the 'logicmoo_utils' library is loaded */
:- ensure_loaded(library(logicmoo_utils)).

/* Assert that no '$exported_op' is exported for 'user' */
:- assert((user:'$exported_op'(_,_,_):- fail)).

/* Abolish any existing definitions of 'system:$exported_op/3' */
:- abolish((system:'$exported_op'/3)).

/* Assert that no '$exported_op' is exported for 'system' */
:- assert((system:'$exported_op'(_,_,_):- fail)).

/* Conditional loading of 'logicmoo_utils' if it exists */
:- if(exists_source(library(logicmoo_utils))).
:- ensure_loaded(library(logicmoo_utils)).
:- endif.

/* Check if 'dictoo' library exists, but the loading is skipped for now */
/* previously: the dictoo library was loaded here, but it's commented out */
:- if(exists_source(library(dictoo))).
%:- ensure_loaded(library(dictoo)).
:- endif.

/* The predicate 'done_once/1' is declared dynamic so it can be modified at runtime */
:- dynamic(done_once/1).

/**
 * do_once(+G)
 * Executes goal G only once. If it has been done before, it does not re-execute.
 * It uses the dynamic predicate 'done_once/1' to keep track of goals that have been executed.
 *
 * @param G The goal to execute only once.
 *
 * @example
 * ?- do_once(writeln('Hello')).
 * Hello
 */
do_once(G):-
  ((done_once(GG), GG=@=G) -> true         % If G has already been executed, do nothing
  ;(assert(done_once(G)),                  % Otherwise, assert that G has been executed
    (once(@(G,user))->true;                % Try to execute G in the user module, and succeed if possible
      retract(done_once(G))))).             % If execution fails, retract the assertion

/**
 * cleanup_debug
 * This predicate removes redundant debug clauses.
 * It searches for duplicate clauses in the 'prolog_debug' module and erases redundant ones.
 */
cleanup_debug:-
  forall(
    (clause(prolog_debug:debugging(A1,B,C),Body,Cl1),      % Find a clause in prolog_debug with head A1
     clause(prolog_debug:debugging(A2,B,C),Body,Cl2),      % Find another clause with same head A2 and body
     A1=@=A2, Cl1\==Cl2),                                  % Check if the clauses are different
     erase(Cl2)).                                          % Erase the second clause

/* Export the 'plain_var/1' predicate for external use */
:- export(plain_var/1).

/**
 * plain_var(+V)
 * Checks if V is a "plain" variable, meaning it is not an attributed variable
 * and doesn't have any attributes related to the 'ci' attribute.
 *
 * @param V The variable to check.
 *
 * @example
 * ?- plain_var(X).
 * true.
 */
plain_var(V):- 
  notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).    % Check if V is a plain Prolog variable without attributes

/**
 * catch_nolog(+G)
 * Safely executes a goal G, ignoring any errors that may occur.
 * Any error messages are suppressed using 'notrace' to avoid logging.
 *
 * @param G The goal to execute.
 */
catch_nolog(G):- 
  ignore(catch(notrace(G), E, once(true; nop(u_dmsg(E=G))))).  % Catch exceptions but suppress them

/**
 * catch_log(+G)
 * Executes a goal G and logs any errors that occur.
 *
 * @param G The goal to execute.
 */
catch_log(G):- 
  ignore(catch((G), E, ((u_dmsg(E=G), ugtrace(G))))).  % Catch exceptions and log them

/* previously: 
   - catch_log was catching errors, writing them, and calling 'catch_nolog'.
   - This was modified to call 'u_dmsg' instead. 
*/

/**
 * get_user_error(-UE)
 * Retrieves the stream associated with the user error output.
 *
 * @param UE The user error stream.
 */
get_user_error(UE):- 
  stream_property(UE, file_no(2)),!.  % Check if the stream refers to file descriptor 2 (stderr)

get_user_error(UE):- 
  stream_property(UE, alias(user_error)),!.  % Alternatively, check if it has the alias 'user_error'

/* previously: 
   - Different mechanisms to identify the user error stream, such as file_no and alias, were tried.
*/

/**
 * ufmt(+G)
 * Formats and prints a goal G using 'ufmt0' or falls back to printing it with 'writeln'.
 *
 * @param G The goal to format and print.
 */
ufmt(G):- 
  notrace((fbug(G) -> true ; ufmt0(G))).  % First, check if fbug can handle G; if not, call ufmt0

ufmt0(G):- 
  fmt(G) -> true ; writeln(G).  % Attempt to format G, otherwise fall back to writeln

/**
 * u_dmsg(+M)
 * Sends a debug message to the appropriate output.
 * This predicate ensures that messages are routed properly to 'user_error' or the default output.
 *
 * @param M The message to send.
 */
u_dmsg(M):- 
  is_list(M), !, my_maplist(u_dmsg, M).  % If M is a list, recursively call u_dmsg on each element
u_dmsg(M):- 
  get_user_error(UE), \+ current_predicate(with_toplevel_pp/2), !, 
  with_output_to(UE, ufmt(M)).  % If 'with_toplevel_pp/2' is not available, output to user error

/* previously: The method of printing debug messages has been extended to handle lists and direct output. */

/* Declaring multifile and dynamic predicates */
:- multifile(is_cgi/0).
:- dynamic(is_cgi/0).
:- multifile(arc_html/0).
:- dynamic(arc_html/0).

/**
 * logicmoo_use_swish
 * Initializes the SWISH web interface for LogicMoo and starts necessary services.
 */
logicmoo_use_swish:-
  set_prolog_flag(use_arc_swish, true),            % Enable SWISH usage
  ld_logicmoo_webui,                               % Load the LogicMoo web UI
  call(call, webui_start_swish_and_clio),          % Start the SWISH and Clio services
  http_handler('/swish', http_redirect(moved, '/swish/'), []).  % Redirect SWISH handler

/* arc_user is used to determine the current user in various contexts */

/**
 * arc_user(+Nonvar)
 * Retrieves the current user or binds a variable to it.
 *
 * @param Nonvar The variable to bind to the user ID.
 */
arc_user(Nonvar):- 
  nonvar(Nonvar), !, arc_user(Var), !, Nonvar=Var.  % If Nonvar is already bound, resolve it

arc_user(main):- 
  main_thread, !.  % If in the main thread, identify the user as 'main'

arc_user(ID):- 
  catch((pengine:pengine_user(ID)), _, fail), !.  % Retrieve user ID via pengine if possible

arc_user(ID):- 
  catch((xlisting_web:is_cgi_stream, 
        xlisting_web:find_http_session(User), 
        http_session:session_data(User, username(ID))), _, fail), !.  % Try finding user from an HTTP session

arc_user(ID):- 
  catch((is_cgi, (xlisting_web:find_http_session(ID))), _, fail), !.  % Fallback to another session method

arc_user(ID):- 
  is_cgi, !, ID=web_user.  % If running as CGI, user is 'web_user'

arc_user(ID):- 
  thread_self(ID).  % Otherwise, default to the current thread ID as the user

/* Define the dynamic predicate 'arc_user_prop/3' */
:- dynamic(arc_user_prop/3).

/* previously: luser_setval used nb_setval unconditionally, but now uses arc_user context */
luser_setval(N, V):- 
  arc_user(ID), luser_setval(ID, N, V), !.

/**
 * luser_setval(+ID, +N, +V)
 * Sets a user property for a specific user.
 * It first checks if N and V are valid "arc sensical" terms, then sets the value.
 *
 * @param ID The user ID.
 * @param N The property name.
 * @param V The property value.
 */
luser_setval(ID, N, V):- 
  \+ (arc_sensical_term(N), arc_sensical_term(V)), 
  warn_skip(not_arc_sensical_term(luser_setval(ID, N, V))).

luser_setval(ID, N, V):- 
  (atom(N) -> nb_setval(N, V); true),                   % Set the property value for atom keys
  retractall(arc_user_prop(ID, N, _)),                   % Remove any old properties for this user
  asserta(arc_user_prop(ID, N, V)).                      % Assert the new property
% Predicate to unset a value for a user by its name `N`.
% @param N The name of the value to be unset.
luser_unsetval(N):-
    % Ignore if there is no such value in the non-backtrackable storage
    ignore(nb_delete(N)),
    % Get the current user ID and unset the value for the user
    arc_user(ID),
    % Call `luser_unsetval/2` to unset the value for the specific user and name
    luser_unsetval(ID,N),
    % Cut to stop backtracking once successful
    !.

% Predicate to unset a value for a specific user ID and name `N`.
% @param ID The user ID.
% @param N The name of the value to be unset.
luser_unsetval(ID,N):-
    % Retract all properties associated with the user and the specific value
    retractall(arc_user_prop(ID,N,_)).

% Predicate to set a default value for a user globally.
% @param N The name of the value.
% @param V The value to be set.
set_luser_default(N,V):- 
    % Set the value globally for the user
    luser_setval(global,N,V).

% Predicate to get or set a default value for a user.
% @param N The name of the value.
% @param V The value to be retrieved or set.
luser_default(N,V):- 
    % If the value `V` is unbound (variable), get the value.
    var(V),!, 
    luser_getval(N,V).
luser_default(N,V):- 
    % Otherwise, set the default value.
    set_luser_default(N,V).

% Predicate to link a value to the current user or a specific ID.
% @param N The name of the value.
% @param V The value to be linked.
luser_linkval(N,V):- 
    % Get the current user ID and link the value
    arc_user(ID),
    luser_linkval(ID,N,V),
    % Cut to stop backtracking
    !.

% Predicate to link a value for a specific user ID.
% Skips linking if `V` is a sensical term and traces a warning if not.
% @param ID The user ID.
% @param N The name of the value.
% @param V The value to be linked.
luser_linkval(ID,N,V):- 
    % Ensure the value `V` is not a variable and both `N` and `V` are not "sensical terms"
    \+ var(V), \+ (arc_sensical_term(N), arc_sensical_term(V)),
    % Trace and skip execution if terms are not sensical
    trace,
    warn_skip(not_arc_sensical_term(luser_linkval(ID,N,V))).

% Predicate to link a value for a user and store it persistently.
% @param ID The user ID.
% @param N The name of the value.
% @param V The value to be linked.
luser_linkval(ID,N,V):- 
    % If the name `N` is an atom, use `nb_linkval` to store the value persistently
    (atom(N) -> nb_linkval(N,V); true),
    % Remove any existing property associated with the user and the value `N`
    retractall(arc_user_prop(ID,N,_)),
    % Assert the new property for the user and value `N`
    asserta(arc_user_prop(ID,N,V)).

% Predicate to check if a term is "sensical", meaning it is neither empty nor a special case.
% @param O The term to be checked.
arc_sensical_term(O):- 
    % Ensure the term `O` is not a variable and is not an empty list, string, or specific ignored structures
    nonvar(O), O \== [], O \== '', O \= (_ - _), O \== end_of_file.

% Predicate to check if a term is sensical and return it in the second argument.
% @param V The term to check.
% @param O The output term.
arc_sensical_term(V,O):- 
    % Reuse the `arc_sensical_term/1` check and unify the output
    arc_sensical_term(V), !, O = V.

% Option predicate, used for configuration.
% Currently disabled and returns failure.
% @example arc_option(grid_size_only) fails.
%arc_option(grid_size_only):- !,fail.

% Option predicate to check if an option is set.
% @param O The option name.
arc_option(O):- 
    % Get the value associated with the option `O`
    luser_getval(O,t).

% Conditional execution if an arc option is set.
% @param O The option to check.
% @param G The goal to execute if the option is set.
if_arc_option(O,G):- 
    % If the option `O` is set, execute `G`; otherwise, do nothing
    (arc_option(O) -> must_det_ll(G); true).

% Execute a goal while temporarily changing a user's value.
% @param N The name of the value to be changed.
% @param V The new value to be set.
% @param Goal The goal to execute with the changed value.
with_luser(N,V,Goal):- 
    % Get the current value of `N` for the user or default to an empty list
    (luser_getval(N,OV); OV = []),
    % Set up the environment, execute the goal, and clean up by restoring the original value
    setup_call_cleanup(
        luser_setval(N,V),
        once(Goal),
        luser_setval(N,OV)
    ).

% Old predicate for getting a value. Commented out in favor of a newer version.
%luser_getval(N,V):- nb_current(N,VVV),arc_sensical_term(VVV,VV),!,V=VV.

% Predicate to get a user's value. Uses caching.
% @param N The name of the value.
% @param V The retrieved value.
luser_getval(N,V):- 
    % Get the initial value and ensure it is sensical
    luser_getval_0(N,VV), 
    VV = V, 
    arc_sensical_term(V), 
    !.

% Predicate to get the value of `arc_user`.
% @param arc_user The name of the value to get.
luser_getval_0(arc_user,V):- 
    % Get the current user ID
    arc_user(V).

% Predicate to retrieve a value using the first available method.
% @param N The name of the value.
% @param V The retrieved value.
luser_getval_0(N,V):- 
    luser_getval_1(N,V).

% First attempt to retrieve a value using method 1.
luser_getval_1(N,V):- 
    luser_getval_2(N,V).

% Fall back to method 2 if method 1 fails.
luser_getval_1(N,V):- 
    luser_getval_3(N,V), 
    \+ (luser_getval_2(N,VV), nop(VV \= V)).

% Continue to attempt to retrieve the value from defaults or other methods.
luser_getval_1(N,V):- 
    get_luser_default(N,V), 
    \+ (luser_getval_3(N,VV), nop(VV \= V)), 
    \+ (luser_getval_2(N,VV), nop(VV \= V)).

% Older predicates commented out for now.
% previously: luser_getval_0(N,V):- luser_getval_2(N,V), \+ luser_getval_1(N,_).
% previously: luser_getval_0(N,V):- luser_getval_3(N,V), \+ luser_getval_2(N,_), \+ luser_getval_1(N,_).

% Fetch value from HTTP request parameters when not on the main thread.
luser_getval_2(N,V):- 
    \+ main_thread, 
    atom(N), 
    httpd_wrapper:http_current_request(Request), 
    member(search(List), Request), 
    member(N = VV, List), 
    url_decode_term(VV, V), 
    arc_sensical_term(V), 
    !.

% Retrieve value from the non-backtrackable storage if available.
luser_getval_2(N,V):- 
    atom(N), 
    nb_current(N, ValV), 
    arc_sensical_term(ValV, Val), 
    Val = V.

% Retrieve value from user properties.
luser_getval_3(N,V):- 
    arc_user(ID), 
    arc_user_prop(ID, N, V).

% Return failure if not in CGI context.
luser_getval_3(_,_):- 
    \+ is_cgi, 
    !, 
    fail.

% Retrieve value from session parameters in a non-main thread.
luser_getval_3(N,V):- 
    \+ main_thread, 
    atom(N), 
    current_predicate(get_param_sess/2), 
    get_param_sess(N, M), 
    url_decode_term(M, V), 
    arc_sensical_term(V).

% Fetch user defaults if available.
get_luser_default(N,V):- 
    arc_user_prop(global, N, VV), 
    VV = V, 
    arc_sensical_term(V), 
    !.

% Fetch defaults from Prolog flags if necessary.
get_luser_default(N,V):- 
    atom(N), 
    current_prolog_flag(N, VV), 
    VV = V, 
    arc_sensical_term(V), 
    !.

% Previously skipped older method of fetching values.
% previously: luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.

% Skip previously used fallback mechanism.
% previously: luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).

% Main predicate that checks if the current thread is the main thread.
ansi_main:- 
    thread_self(main), 
    nop(is_cgi), 
    !.

% Predicate to check if the current thread is the main thread.
main_thread:- 
    thread_self(main), 
    !.

% Conditionally execute a goal if in the main thread.
if_thread_main(G):- 
    main_thread -> call(G); true.

% File directive to ensure `fbug/1` is only defined once.
:- if(\+ current_predicate(fbug/1)).

%fbug(P):- format(user_error,'~N~p~n',[P]).
:- endif.

substM(T, F, R, R) :- T==F, !. % If the term is exactly the one to replace, return the replacement.
substM(T, _, _, R) :- \+ compound(T), !, R = T. % If it's not a compound term, no replacement is needed.
substM([H1|T1], F, R, [H2|T2]) :- % If it's a list, recursively substitute in the head and tail.
    !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- % If it's a compound term, decompose and substitute in the arguments.
    C1 =.. [Fn|A1], substM_l(A1, F, R, A2), !, C2 =.. [Fn|A2].

/* Helper predicate: Substitute within a list of arguments */
substM_l([], _, _, []). % Base case: empty list.
substM_l([H1|T1], F, R, [H2|T2]) :- % Recursive case: substitute in head and tail of the list.
    substM(H1, F, R, H2), substM_l(T1, F, R, T2).

/**
 * @predicate pp_m(+Clause)
 * Pretty-print the clause.
 * This predicate writes the clause `Cl` to the source in a formatted way.
 *
 * @param Clause The Prolog clause to pretty-print.
 */
pp_m(Cl) :- write_src(Cl), !. % Calls a helper to write the clause to the source.
pp_m(C, Cl) :- color_g_mesg(C, write_src(Cl)), !. % Writes the clause with some colored messages.

/* previously: unused code for tracing, now commented out for clarity */
%  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).

/**
 * @predicate pp_q(+Clause)
 * Pretty-print the clause for queries.
 * Outputs the clause with query-specific formatting and ensures no singleton variables.
 *
 * @param Clause The Prolog clause to pretty-print.
 */
pp_q(Cl) :-
    notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).

/**
 * @predicate ncatch(+Goal, +Error, +Handler)
 * Wrapper around catch/3 for error handling.
 * This predicate catches exceptions and handles them with `Handler`.
 *
 * @param Goal The goal to execute.
 * @param Error The error to catch.
 * @param Handler The handler for the caught error.
 */
ncatch(G, E, F) :- catch(G, E, F).

/**
 * @predicate mcatch(+Goal, +Error, +Handler)
 * Another wrapper for catch/3, used for modular error handling.
 *
 * @param Goal The goal to execute.
 * @param Error The error to catch.
 * @param Handler The handler for the caught error.
 */
mcatch(G, E, F) :- catch(G, E, F).

/* previously: error handling was extended with debugging hooks, removed for clarity */
%mcatch(G,E,F):- catch(G,E,(fbug(G=E),catch(bt,_,fail),fbug(G=E),ignore(call(F)),throw(E))).

/* Directive: Conditional inclusion if predicate `if_t/2` does not exist */
:- if(\+ current_predicate(if_t/2)).
:- meta_predicate(if_t(0, 0)).
/**
 * @predicate if_t(+IfGoal, +ThenGoal)
 * Conditional execution of Prolog goals.
 * If `IfGoal` succeeds, `ThenGoal` is executed.
 *
 * @param IfGoal The condition goal.
 * @param ThenGoal The goal to execute if `IfGoal` succeeds.
 */
if_t(IF, THEN) :- call(call, ignore((IF, THEN))).
:- endif.

/* Directive: Conditional inclusion if predicate `must_ll/1` does not exist */
:- if(\+ current_predicate(must_ll/1)).
:- meta_predicate(must_ll(0)).
/**
 * @predicate must_ll(+Goal)
 * Ensures that the goal succeeds at least once.
 * If the goal fails, it throws an error.
 *
 * @param Goal The goal to execute.
 */
must_ll(G) :- md(call, G) *-> true ; throw(not_at_least_once(G)).
:- endif.

/* Directive: Conditional inclusion if predicate `at_least_once/1` does not exist */
:- if(\+ current_predicate(at_least_once/1)).
:- meta_predicate(at_least_once(0)).
/**
 * @predicate at_least_once(+Goal)
 * Ensures that the goal succeeds at least once, throwing an error if it fails.
 *
 * @param Goal The goal to execute.
 */
at_least_once(G) :- call(G) *-> true ; throw(not_at_least_once(G)).
:- endif.

/* previously: wrapping mechanisms were used, preserved for future refactoring */
%wraps_each(must_ll,call).

/* Skipping `remove_must_det/1` because its functionality is now disabled */
remove_must_det(_) :- !, fail. % Always fails as it's not intended for use anymore.

/* Skipped code for conditionally removing `must_det_ll` calls, commented for clarity */
%remove_must_det(MD):- !.
%remove_must_det(MD):- nb_current(remove_must_det(MD),TF),!,TF==true.

/* This block handles removal of specific terms in compound structures. 
   However, it's not currently in use, but preserved for future needs */
remove_mds(MD, GG, GO) :-
    sub_term(G, GG), compound(G), compound_name_arg(G, MD, GGGG),
    subst001(GG, G, GGGG, GGG), remove_mds(MD, GGG, GO).
remove_mds(_, GG, GG).

/**
 * @predicate never_rrtrace/0
 * Prevent tracing if in CGI environment.
 * Ensures that tracing doesn't interfere when in CGI mode.
 */
never_rrtrace :- nb_current(cant_rrtrace, t), !, notrace.
never_rrtrace :- is_cgi, notrace.

/* Skipped tracing directives, preserved for potential debugging reactivation */
%itrace:- !.
%itrace:- \+ current_prolog_flag(debug,true),!.

/* Trace the main thread, used for debugging */
itrace :- if_thread_main(trace), !.
ibreak :- if_thread_main(((trace, break))).

/* Catch an argument by position, with error handling */
tc_arg(N, C, E) :- 
    catch(arg(N, C, E), Err,
    /* previously: detailed error reporting for debugging, now simplified */
    (bt, fbug(tc_arg(N, C, E) = Err), 
    ((tracing -> true ; trace), break, arg(N, C, E)))).

/**
 * @predicate compound_name_arg(+Compound, +Name, -Arg)
 * Extract the argument of a compound term.
 * If the term is not instantiated, it unifies `Compound` with the functor `Name` and argument `Arg`.
 *
 * @param Compound A compound term or variable.
 * @param Name The functor name.
 * @param Arg The argument to extract.
 */
compound_name_arg(G, MD, Goal) :- var(G), !, atom(MD), G =.. [MD, Goal]. % Case: G is uninstantiated.
compound_name_arg(G, MD, Goal) :- compound(G), !, compound_name_arguments(G, MD, [Goal]). % Case: G is a compound term.

/* File directives for handling user-defined message hooks */
:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).

/* Disabled error hook for debugging, can be re-enabled for more granular error handling */
%user:message_hook(Term, Kind, Lines):- error==Kind, itrace,fbug(user:message_hook(Term, Kind, Lines)),trace,fail.

/**
 * @predicate user:message_hook(+Term, +Kind, +Lines)
 * Custom handler for messages.
 * Can be used to intercept and process specific message types, but currently fails on all messages.
 *
 * @param Term The message term.
 * @param Kind The kind of message (e.g., error, warning).
 * @param Lines The message content.
 */
user:message_hook(Term, Kind, Lines) :-
    fail, error == Kind. % Always fails, not currently used.

   fbug(message_hook(Term, Kind, Lines)),fail.


% PLDoc header for meta_predicates
/** 
 * must_det_ll(:Goal) is det.
 * 
 * This predicate ensures that Goal is deterministic and that no choice points
 * are left after it succeeds. It is a meta-predicate as it operates on other goals.
 *
 * @param Goal The goal to be executed deterministically.
 */
:- meta_predicate(must_det_ll(0)).

/** 
 * must_det_ll1(:P1, :Goal) is det.
 * 
 * This predicate is similar to `must_det_ll/1` but uses an additional predicate P1 
 * during its execution.
 *
 * @param P1 A predicate used as a helper in execution.
 * @param Goal The goal to be executed.
 */
:- meta_predicate(must_det_ll1(1, 0)).

/** 
 * md_failed(:P1, :Goal) is det.
 * 
 * This predicate handles the failure of a goal within a meta-predicate context.
 * It executes P1 and then Goal, managing failure appropriately.
 *
 * @param P1 The first predicate to be executed.
 * @param Goal The goal that might fail.
 */
:- meta_predicate(md_failed(1, 0)).

/** 
 * must_not_error(:Goal) is det.
 * 
 * Ensures that the given Goal does not raise an error. It catches errors and 
 * prevents them from propagating.
 *
 * @param Goal The goal that should not result in an error.
 */
:- meta_predicate(must_not_error(0)).

% This line is commented out because the predicate was likely removed or deprecated.
% % :- meta_predicate(must_det_l(0)).

/* previously: 
   % This flag was disabled. Uncommenting this line would prevent certain debugging features.
   % :- no_xdbg_flags.
*/

/** 
 * wno_must(:Goal) is det.
 * 
 * Runs the given goal but with certain debugging and tracing flags disabled.
 * 
 * @param Goal The goal to execute with modified local settings.
 */
:- meta_predicate(wno_must(0)).

% Set no_must_det_ll and cant_rrtrace flags locally, then call the goal G.
wno_must(G) :- 
    locally(nb_setval(no_must_det_ll, t), % Disable must_det_ll checks locally
    locally(nb_setval(cant_rrtrace, t),   % Disable rrtrace locally
    call(G))).                            % Call the goal

% PLDoc header for md_maplist/3
/** 
 * md_maplist(:MD, :P1, +List) is det.
 * 
 * This predicate maps the meta-predicate MD over a list, applying P1 to each element.
 * 
 * @param MD The meta-predicate to apply to each element of the list.
 * @param P1 A predicate to be applied to each element of the list.
 * @param List The list of elements to process.
 */
md_maplist(_MD, _, []) :- 
    % Base case: empty list, do nothing.
    !.

% Recursive case: Apply MD and P1 to the head of the list, then recurse on the tail.
md_maplist(MD, P1, [H | T]) :- 
    call(MD, call(P1, H)),       % Apply MD and P1 to the head element
    md_maplist(MD, P1, T).       % Recur on the tail

% PLDoc header for md_maplist/4
/** 
 * md_maplist(:MD, :P2, +ListA, +ListB) is det.
 * 
 * Maps a predicate over two lists element-wise.
 * 
 * @param MD The meta-predicate to apply to each pair of elements.
 * @param P2 A predicate to be applied to each pair of elements.
 * @param ListA The first list.
 * @param ListB The second list.
 */
md_maplist(_MD, _, [], []) :- 
    % Base case: both lists are empty.
    !.

% Recursive case: Apply MD and P2 to the heads of the lists, then recurse.
md_maplist(MD, P2, [HA | TA], [HB | TB]) :- 
    call(MD, call(P2, HA, HB)),  % Apply MD and P2 to the head elements of both lists
    md_maplist(MD, P2, TA, TB).  % Recur on the tails

% PLDoc header for md_maplist/5
/** 
 * md_maplist(:MD, :P3, +ListA, +ListB, +ListC) is det.
 * 
 * Maps a predicate over three lists element-wise.
 * 
 * @param MD The meta-predicate to apply to each triplet of elements.
 * @param P3 A predicate to be applied to each triplet of elements.
 * @param ListA The first list.
 * @param ListB The second list.
 * @param ListC The third list.
 */
md_maplist(_MD, _, [], [], []) :- 
    % Base case: all three lists are empty.
    !.

% Recursive case: Apply MD and P3 to the heads of the lists, then recurse.
md_maplist(MD, P3, [HA | TA], [HB | TB], [HC | TC]) :- 
    call(MD, call(P3, HA, HB, HC)),  % Apply MD and P3 to the head elements of all three lists
    md_maplist(MD, P3, TA, TB, TC).  % Recur on the tails of the lists

% The following code is commented out as it was likely part of a debugging process that is no longer needed.
% % must_det_ll(G):- !, once((/*notrace*/(G)*->true;md_failed(P1,G))).

/* previously:
   % This was an old directive to check if the must_det_ll/1 predicate exists before defining it.
   % It was removed because the condition is either unnecessary or handled elsewhere.
   % :- if( \+ current_predicate(must_det_ll/1)).
*/

% If tracing is active, run X once and ensure it's deterministic.
must_det_ll(X) :- 
    tracing, 
    !, 
    once(X).

% Otherwise, call md/2 to execute X within the context of the meta-predicate.
must_det_ll(X) :- 
    md(once, X).

/* previously:
   % This directive ensured that the predicate definition only occurred once.
   % It has been removed as the condition is no longer relevant.
   % :- endif.
*/


md(P1,G):- tracing,!, call(P1,G). % once((call(G)*->true;md_failed(P1,G))).
md(P1,G):- remove_must_det(MD), wraps_each(MD,P1),!,call(G).
md(P1,G):- never_rrtrace,!, call(P1,G).
md(P1,G):- /*notrace*/(arc_html),!, ignore(/*notrace*/(call(P1,G))),!.
%md(P1,X):- !,must_not_error(X).
md(P1,(X,Goal)):- is_trace_call(X),!,call((itrace,call(P1,Goal))).
md(_, X):- is_trace_call(X),!,itrace.
md(P1, X):- nb_current(no_must_det_ll,t),!,call(P1,X).
md(P1,X):- \+ callable(X), !, throw(md_not_callable(P1,X)).
md(P1,(A*->X;Y)):- !,(must_not_error(A)*->md(P1,X);md(P1,Y)).
md(P1,(A->X;Y)):- !,(must_not_error(A)->md(P1,X);md(P1,Y)).
md(P1,(X,Cut)):- (Cut==(!)),md(P1,X),!.
md(MD,maplist(P1,List)):- !, call(MD,md_maplist(MD,P1,List)).
md(MD,maplist(P2,ListA,ListB)):- !, call(MD,md_maplist(MD,P2,ListA,ListB)).
md(MD,maplist(P3,ListA,ListB,ListC)):- !, call(MD,md_maplist(MD,P3,ListA,ListB,ListC)).
md(P1,(X,Cut,Y)):- (Cut==(!)), !, (md(P1,X),!,md(P1,Y)).
md(P1,(X,Y)):- !, (md(P1,X),md(P1,Y)).
%md(P1,X):- /*notrace*/(ncatch(X,_,fail)),!.
%md(P1,X):- conjuncts_to_list(X,List),List\=[_],!,maplist(must_det_ll,List).
md(_,must_det_ll(X)):- !, must_det_ll(X).
md(_,grid_call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,(X,Y,Z)):- !, (md(P1,X)->md(P1,Y)->md(P1,Z)).
%md(P1,(X,Y)):- !, (md(P1,X)->md(P1,Y)).
%md(P1,if_t(X,Y)):- !, if_t(must_not_error(X),md(P1,Y)).
md(P1,forall(X,Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).
md(P1,\+ (X, \+ Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).

md(P1,(X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;md_failed(P1,X;Y)).
md(P1,\+ (X)):- !, (\+ must_not_error(X) -> true ; md_failed(P1,\+ X)).
%md(P1,(M:Y)):- nonvar(M), !, M:md(P1,Y).
md(P1,X):-
  ncatch(must_det_ll1(P1,X),
  md_failed(P1,G,N), % <- ExceptionTerm
   % bubble up and start running
  ((M is N -1, M>0)->throw(md_failed(P1,G,M));(ugtrace(md_failed(P1,G,M),X),throw('$aborted')))),!.
%must_det_ll(X):- must_det_ll1(P1,X),!.
/* 
   This directive enables the 'tracing' mode for debugging, 
   ensuring that errors are caught and handled appropriately.
*/


/**
 * must_det_ll1/2
 * This predicate ensures the given goal P1 is executed deterministically with trace handling.
 * It handles different forms of calls, ensuring no errors during execution.
 *
 * @param P1 The goal to execute.
 * @param X The argument that can be passed directly or wrapped in 'once' or other calls.
 * @example
 * ?- must_det_ll1(my_goal, my_arg).
 */
must_det_ll1(P1, X) :- 
  % Check if tracing is active, then execute the call with error handling
  tracing, !, must_not_error(call(P1, X)), !.

must_det_ll1(P1, once(A)) :- 
  % When the second argument is wrapped in 'once', ensure it runs deterministically
  !, once(md(P1, A)).

must_det_ll1(P1, X) :-
  % Extract the module, functor, and arity from the second argument
  strip_module(X, M, P), functor(P, F, A),
  % Set up cleanup for tracing, ensuring no errors occur, or handling failure
  setup_call_cleanup(nop(trace(M:F/A, +fail)), 
    (must_not_error(call(P1, X)) *-> true; md_failed(P1, X)),
    nop(trace(M:F/A, -fail))), !.

/* previously: must_not_error(G) was using must(once(G)) but this was changed to allow for more flexible error handling. */

/**
 * must_not_error/1
 * Executes the given goal and ensures errors are handled correctly.
 * Different cases handle tracing, CGI execution, and custom error logic.
 *
 * @param G The goal to execute.
 */
must_not_error(G) :- 
  % If tracing is active or rrtrace is not used, execute the goal directly
  (tracing; never_rrtrace), !, call(G).

must_not_error(G) :- 
  % If running in a CGI environment, handle errors with custom messaging
  notrace(is_cgi), !, ncatch((G), E, ((u_dmsg(E = G)))).

% Dead code: GUI tracer error handling skipped; the system may not need special GUI tracing behavior
% must_not_error(X):- is_guitracer, !, call(X).

must_not_error(X) :- 
  % Catch and handle any exceptions, tracing them for further debugging if needed
  !, ncatch(X, E, (fbug(E = X), ugtrace(error(E), X))).

must_not_error(X) :- 
  % Trace exceptions and retry with visible trace enabled
  ncatch(X, E, (rethrow_abort(E); 
    (writeq(E = X), pp(etrace = X), trace, rrtrace(visible_rtrace([-all, +exception]), X)))).

/**
 * always_rethrow/1
 * This predicate determines which exceptions should always be rethrown during execution.
 *
 * @param E The exception to check.
 */
always_rethrow('$aborted').  % Always rethrow when the process is aborted.
always_rethrow(md_failed(_, _, _)).  % Rethrow if an internal failure occurs.
always_rethrow(return(_)).  % Handle custom return exceptions.
always_rethrow(metta_return(_)).  % Handle metta return exceptions.
always_rethrow(give_up(_)).  % Give up on certain operations.
always_rethrow(time_limit_exceeded(_)).  % Rethrow if time limit is exceeded.
always_rethrow(depth_limit_exceeded).  % Handle depth limit exceptions.
always_rethrow(restart_reading).  % Handle restart reading exceptions.
always_rethrow(E) :- 
  % If rrtrace is not used, throw the exception
  never_rrtrace, !, throw(E).

/**
 * catch_non_abort/1
 * This predicate catches all exceptions except those that involve process aborts.
 *
 * @param Goal The goal to execute while catching non-abort exceptions.
 */
catch_non_abort(Goal) :- 
  % Catch non-abort exceptions, rethrow them if necessary
  catch(cant_rrtrace(Goal), E, rethrow_abort(E)), !.

/**
 * rethrow_abort/1
 * This predicate handles rethrowing of certain exceptions.
 *
 * @param E The exception to rethrow.
 */
rethrow_abort(E) :- 
  % Log the exception and fail
  format(user_error, '~N~q~n', [catch_non_abort_or_abort(E)]), fail.

% Skipped: Special handling for time limits, as it is not always necessary
% rethrow_abort(time_limit_exceeded) :- !.

rethrow_abort('$aborted') :- 
  % When '$aborted' is encountered, abort and log a timeout
  !, throw('$aborted'), !, forall(between(1, 700, _), sleep(0.01)), writeln(timeout), !, fail.

rethrow_abort(E) :- 
  % Handle other exceptions, outputting an error message
  ds, !, format(user_error, '~N~q~n', [catch_non_abort(E)]), !.

/**
 * cant_rrtrace/1
 * Executes the goal without rrtrace, or with rrtrace based on flags.
 *
 * @param Goal The goal to execute.
 */
cant_rrtrace(Goal) :- 
  % If rrtrace is disabled, just call the goal
  never_rrtrace, !, call(Goal).

cant_rrtrace(Goal) :- 
  % Otherwise, execute with rrtrace cleanup
  setup_call_cleanup(cant_rrtrace, Goal, can_rrtrace).

/**
 * md_failed/2
 * Handles failures during the execution of P1 with argument X.
 * Logs the failure and determines whether to retry or handle it differently.
 *
 * @param P1 The predicate that failed.
 * @param X The argument that failed.
 */
md_failed(P1, X) :- 
  % Log the failure without trace and fail
  notrace((write_src_uo(failed(P1, X)), fail)).

md_failed(P1, X) :- 
  % If tracing is active, run with visible tracing
  tracing, visible_rtrace([-all, +fail, +call, +exception], call(P1, X)).

md_failed(P1, X) :- 
  % Without tracing, log and run with visible trace
  \+ tracing, !, visible_rtrace([-all, +fail, +exit, +call, +exception], call(P1, X)).

md_failed(P1, G) :- 
  % Handle failures in a CGI environment
  is_cgi, \+ main_debug, !, u_dmsg(arc_html(md_failed(P1, G))), fail.

md_failed(_P1, G) :- 
  % If testing is enabled, output the failure and give up
  option_value(testing, true), !,
  T = 'FAILEDDDDDDDDDDDDDDDDDDDDDDDDDD!!!!!!!!!!!!!'(G),
  write_src_uo(T), give_up(T, G).

md_failed(P1, G) :- 
  % If rrtrace is disabled, log and throw the failure
  never_rrtrace, !, notrace, (u_dmsg(md_failed(P1, G))), !, throw(md_failed(P1, G, 2)).

md_failed(P1, X) :- 
  % Handle failure with GUI tracer
  notrace, is_guitracer, u_dmsg(failed(X)), nortrace, atrace, call(P1, X).

md_failed(P1, G) :- 
  % Log failure in debugging mode and throw the failure
  main_debug, (write_src_uo(md_failed(P1, G))), !, throw(md_failed(P1, G, 2)).

/**
 * write_src_uo/1
 * Outputs the source of a given goal to the appropriate stream.
 *
 * @param G The goal to output.
 */

write_src_uo(G):-
 stream_property(S,file_no(1)),
    with_output_to(S,
     (format('~N~n~n',[]),
      write_src(G),
      format('~N~n~n'))),!,
 %stack_dump,
 stream_property(S2,file_no(2)),
    with_output_to(S2,
     (format('~N~n~n',[]),
      write_src(G),
      format('~N~n~n'))),!.

:- meta_predicate(rrtrace(0)).

rrtrace(X) :- rrtrace(etrace, X).

/* This predicate attempts to dump the stack if an error occurs. 
   The use of `ignore` ensures that if `bt` fails, no exception will be thrown, 
   and execution continues.
*/
stack_dump :- ignore(catch(bt, _, true)).
/* previously: 
   stack_dump attempted to also dump additional debugging information via `dumpST` 
   and `bts`, but these were commented out likely to avoid unnecessary output.
*/

/**
 * ugtrace(+Why, +G)
 * This predicate traces or logs the execution of the goal `G` based on `Why`.
 * If an error (`error(Why)`) is encountered, it traces the stack.
 * 
 * @param Why Reason for tracing.
 * @param G Goal to be traced.
 */
ugtrace(error(Why), G) :-
    !, % Cut to avoid further processing if error case is matched.
    notrace, % Disable the tracer.
    write_src_uo(Why), % Write the error information.
    stack_dump, % Dump the stack to help debugging.
    write_src_uo(Why), % Log the error again.
    rtrace(G). % Trace the goal G.

ugtrace(Why, G) :-
    tracing, !, % If tracing is active, proceed with tracing.
    notrace, % Temporarily disable tracing.
    write_src(Why), % Log the reason for tracing.
    rtrace(G). % Perform a regular trace on the goal G.

/* If tracing is not active, handle the failure and possibly throw an exception. */
ugtrace(Why, _) :- 
    is_testing, !, % If running in testing mode, handle errors differently.
    ignore(give_up(Why, 5)), % Attempt to give up after 5 retries.
    throw('$aborted'). % Abort the goal execution.

ugtrace(_Why, G) :-
    ggtrace(G), % Call the goal with ggtrace.
    throw('$aborted'). % Abort after tracing.

/* previously:
   The old implementation used `ggtrace` in cases not covered above, 
   but the code was commented out, possibly because it wasn't needed in 
   current use cases.
*/

/**
 * give_up(+Why, +N)
 * If running in testing mode, abort execution with a given reason.
 * 
 * @param Why Reason for giving up.
 * @param N Exit code to halt the process with.
 */
give_up(Why, _) :- 
    is_testing, !, % If in testing mode, log the reason and halt.
    write_src_uo(Why), 
    throw(give_up(Why)).

give_up(Why, N) :-
    is_testing, !, % If in testing mode, halt the process.
    write_src_uo(Why), 
    halt(N).

give_up(Why, _) :- 
    write_src_uo(Why), % Log the reason for giving up.
    throw('$aborted'). % Abort execution.

/**
 * is_guitracer
 * This predicate checks if the Prolog environment supports the GUI tracer.
 * It does so by checking if the 'DISPLAY' environment variable is set 
 * and the `gui_tracer` flag is enabled.
 */
is_guitracer :- 
    getenv('DISPLAY', _), 
    current_prolog_flag(gui_tracer, true).

/* Directive to define `rrtrace/2` as a meta-predicate, where the first argument is 
   a predicate that will be applied to the second argument. */
:- meta_predicate(rrtrace(1, 0)).

/**
 * rrtrace(+P1, +X)
 * Handles tracing in various scenarios depending on system state (e.g., CGI mode).
 * 
 * @param P1 Predicate to apply.
 * @param X Goal to be traced.
 */
rrtrace(P1, X) :- 
    never_rrtrace, !, % If tracing should never occur, fail the predicate.
    nop((u_dmsg(cant_rrtrace(P1, X)))), % Log the inability to trace.
    fail. % Fail the goal.

rrtrace(P1, G) :- 
    is_cgi, !, % If running in CGI mode, log and call the predicate.
    u_dmsg(arc_html(rrtrace(P1, G))),
    call(P1, G).

/* If GUI tracing is not enabled, avoid GUI tracing altogether. */
rrtrace(P1, X) :- 
    notrace, \+ is_guitracer, !, 
    nortrace, 
    (notrace(\+ current_prolog_flag(gui_tracer, true)) 
        -> call(P1, X) 
        ; (itrace, call(P1, X))).

/* previously:
   Another clause of `rrtrace/2` was removed because it involved GUI tracing, 
   which might not be supported in the current environment.
*/
rrtrace(P1, X) :- 
    itrace, !, % If interactive tracing is enabled, apply the trace.
    call(P1, X).

/* Directive to define `arc_wote/1` as a meta-predicate. */
:- meta_predicate(arc_wote(0)).

/**
 * arc_wote(+G)
 * Executes goal `G` within the context of ANSI formatting (pretty printing).
 * 
 * @param G Goal to be executed.
 */
arc_wote(G) :- with_pp(ansi, wote(G)).

/* 
   arcST enables tracing of backtrace stack and related debugging information.
*/
arcST :- itrace, arc_wote(bts), itrace.

/* 
   atrace is an alias to trigger arc_wote with backtrace stack functionality.
*/
atrace :- arc_wote(bts).

/* previously:
   There was an alternative version of atrace that dumped debugging information 
   to file descriptor 2, but it was skipped, likely to avoid unnecessary output.
*/

/* Directive to define `odd_failure/1` as a meta-predicate. */
:- meta_predicate(odd_failure(0)).

/**
 * odd_failure(+G)
 * Executes the goal `G` and handles failure in a special manner.
 * 
 * @param G Goal to be executed.
 */
odd_failure(G) :- 
    never_rrtrace, !, 
    call(G). % Simply call the goal if tracing is disabled.

odd_failure(G) :- 
    wno_must(G) *-> true ; fail_odd_failure(G). % Handle failure in a specific way if goal fails.

/* Directive to define `fail_odd_failure/1` as a meta-predicate. */
:- meta_predicate(fail_odd_failure(0)).

/**
 * fail_odd_failure(+G)
 * Logs and traces goal failures.
 * 
 * @param G Goal that has failed.
 */
fail_odd_failure(G) :- 
    u_dmsg(odd_failure(G)), 
    rtrace(G), 
    fail. % Always fail after tracing.

/* previously:
   A more complex failure handler existed, but it was simplified here.
*/

/**
 * bts
 * Loads the `prolog_stack` library and prints a detailed Prolog backtrace.
 */
bts :- 
    ensure_loaded(library(prolog_stack)), % Load the backtrace library.
    prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3), % Export necessary predicates.
    use_module(library(prolog_stack), [print_prolog_backtrace/2, get_prolog_backtrace_lc/3]), % Use stack-related predicates.
    prolog_stack:call(call, get_prolog_backtrace_lc, 8000, Stack, [goal_depth(600)]), % Get the stack trace.
    stream_property(S, file_no(1)), % Get the current stream properties.
    prolog_stack:print_prolog_backtrace(S, Stack), % Print the stack trace to the stream.
    ignore((fail, current_output(Out), \+ stream_property(Out, file_no(1)), print_prolog_backtrace(Out, Stack))), !. % Print to other output streams if available.

/**
 * my_assertion(+G)
 * Asserts that the goal `G` succeeds.
 * 
 * @param G Goal to be asserted.
 */
my_assertion(G) :- 
    my_assertion(call(G), G).

my_assertion(_, G) :- 
    call(G), !. % Call the goal and succeed.

my_assertion(Why, G) :- 
    u_dmsg(my_assertion(Why, G)), 
    writeq(Why = goal(G)), 
    nl, 
    ibreak. % Break execution for debugging.

/**
 * must_be_free(+Free)
 * Ensures that the variable `Free` is free (unbound).
 * 
 * @param Free Variable to check.
 */
must_be_free(Free) :- 
    plain_var(Free), !. % Check if the variable is unbound.

must_be_free(Free) :- 
    \+ nonvar_or_ci(Free), !. % Ensure it's not a concrete instance.

/* If the variable is bound, log the error and fail. */
must_be_free(Nonfree) :- 
    arcST, 
    u_dmsg(must_be_free(Nonfree)), 
    ignore((attvar(Nonfree), get_attrs(Nonfree, ATTS), pp(ATTS))), 
    ibreak, 
    fail.

/**
 * must_be_nonvar(+Nonvar)
 * Ensures that the variable `Nonvar` is bound (non-variable).
 * 
 * @param Nonvar Variable to check.
 */
must_be_nonvar(Nonvar) :- 
    nonvar_or_ci(Nonvar), !. % Check if the variable is nonvar.

/* If the variable is free, log the error and fail. */
must_be_nonvar(IsVar) :- 
    arcST, 
    u_dmsg(must_be_nonvar(IsVar)), 
    ibreak, 
    fail.
/* previously: goal_expansion for handling must_det_l and must_det_ll was skipped 
   Explanation: The commented-out goal_expansion clauses were likely designed for transforming specific goals during compilation. 
   These clauses might have been skipped to avoid conflicts or unnecessary processing in the current context. */

/**
 * get_setarg_p1(+P3, +E, +Cmpd, -SA) is det.
 *
 * Retrieves the argument from the compound term Cmpd and sets the argument using a predicate P3.
 *
 * @param P3    Predicate to set the argument.
 * @param E     The element to find within the compound term.
 * @param Cmpd  The compound term being processed.
 * @param SA    The result after applying the predicate P3 to the argument in Cmpd.
 *
 * @example
 * ?- get_setarg_p1(setarg, member, some_term(member, data), SA).
 */
get_setarg_p1(P3, E, Cmpd, SA) :-
    % Check if Cmpd is a compound term and call get_setarg_p2 to find and set the argument.
    compound(Cmpd), 
    get_setarg_p2(P3, E, Cmpd, SA).

/**
 * get_setarg_p2(+P3, +E, +Cmpd, -SA) is det.
 *
 * Retrieves the argument in Cmpd at position N1 and sets it using the predicate P3.
 *
 * @param P3    Predicate to set the argument.
 * @param E     The element to match within the compound term.
 * @param Cmpd  The compound term being processed.
 * @param SA    The result after applying the predicate P3 to the argument in Cmpd.
 */
get_setarg_p2(P3, E, Cmpd, SA) :-
    % Find the argument position in the compound term and apply P3 to set the argument.
    arg(N1, Cmpd, E), 
    SA = call(P3, N1, Cmpd).

get_setarg_p2(P3, E, Cmpd, SA) :-
    % Recursively search for the argument in the compound term's arguments if the first attempt fails.
    arg(_, Cmpd, Arg), 
    get_setarg_p1(P3, E, Arg, SA).

/**
 * my_b_set_dict(+Member, +Obj, +Var) is det.
 *
 * Set the value of a member in an object using b_set_dict.
 *
 * @param Member  The member key to set.
 * @param Obj     The object (e.g., dictionary) where the member resides.
 * @param Var     The value to set for the member.
 */
my_b_set_dict(Member, Obj, Var) :-
    % Set the member's value in the object using the auxiliary set_omemberh predicate.
    set_omemberh(b, Member, Obj, Var).

/**
 * set_omemberh(+How, +Member, +Obj, +Var) is det.
 *
 * Handles setting member values in objects based on different modes (b, nb, link).
 *
 * @param How     The method used for setting (e.g., 'b', 'nb').
 * @param Member  The member key to set.
 * @param Obj     The object where the member resides.
 * @param Var     The value to set for the member.
 */
set_omemberh(_, Member, Obj, Var) :-
    % Use arc_setval to set the member's value in the object.
    !, arc_setval(Obj, Member, Var).

/* previously: Alternative set_omemberh methods were commented out.
   Explanation: Other methods such as nb_set_dict and nb_link_dict are specialized ways to set dictionary values without backtracking. 
   These might have been disabled to maintain consistency or because they were not needed in the current context. */

/**
 * set_omember(+Member, +Obj, +Var) is det.
 *
 * Sets the value of a member in an object using a default mode.
 *
 * @param Member  The member key to set.
 * @param Obj     The object where the member resides.
 * @param Var     The value to set for the member.
 */
set_omember(Member, Obj, Var) :-
    % Use the default method 'b' to set the member value.
    set_omember(b, Member, Obj, Var).

/**
 * set_omember(+How, +Member, +Obj, +Var) is det.
 *
 * Sets the value of a member in an object with a specified method.
 *
 * @param How     The method used for setting (e.g., 'b', 'nb').
 * @param Member  The member key to set.
 * @param Obj     The object where the member resides.
 * @param Var     The value to set for the member.
 */
set_omember(How, Member, Obj, Var) :-
    % Ensure that the necessary arguments are nonvar (i.e., instantiated) before proceeding.
    must_be_nonvar(Member),
    must_be_nonvar(Obj),
    must_be_nonvar(How),
    !,
    set_omemberh(How, Member, Obj, Var),
    !.

/**
 * get_kov(+K, +O, -V) is semidet.
 *
 * Retrieves the value associated with key K in object O, with a fallback to handle nested structures.
 *
 * @param K  The key to look up.
 * @param O  The object in which to search for the key.
 * @param V  The value associated with the key.
 */
get_kov(K, O, V) :-
    % Check if the object is a dot-hook, and retrieve the associated value if so.
    dictoo:is_dot_hook(user, O, K, V), 
    !, 
    o_m_v(O, K, V).

get_kov(K, O, V) :-
    % Fallback to retrieve values from nested objects or properties if direct retrieval fails.
    (get_kov1(K, O, V) *-> true ; (get_kov1(props, O, VV), get_kov1(K, VV, V))).

/* Explanation: The commented-out clause that directly accesses rbtree nodes 
   was likely skipped to avoid direct manipulation of red-black tree nodes. This approach can be risky or unnecessary. */

% Directives to export and import the term_expansion_setter/2 predicate.
:- export(term_expansion_setter/2).
:- system:import(term_expansion_setter/2).

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).

/** <predicate> is_setter_syntax(+Input, -Object, -Member, -Var, -How)
    @brief Determines if the Input matches the setter syntax for object-member assignments.
    @param Input The input to analyze (set, gset, hset, etc.)
    @param Object The object part of the setter syntax.
    @param Member The member part of the setter syntax.
    @param Var Unused variable that holds placeholder values.
    @param How Specifies whether the setter is 'b' (basic), 'nb' (non-basic), or other methods.
    
    @example
    ?- is_setter_syntax(set(my_object,my_member),Obj,Member,_,How).
    Obj = my_object,
    Member = my_member,
    How = b.
*/

/* Checks if the input is not compound (e.g., not a complex term), if true, fails immediately */
is_setter_syntax(I, _Obj, _Member, _Var, _) :- \+ compound(I), !, fail.

/* Matches a basic setter syntax: set(Object, Member) */
is_setter_syntax(set(Obj, Member), Obj, Member, _Var, b).

/* Matches a non-basic setter syntax: gset(Object, Member) */
is_setter_syntax(gset(Obj, Member), Obj, Member, _Var, nb).

/* Matches a customizable setter syntax: hset(How, Object, Member) */
is_setter_syntax(hset(How, Obj, Member), Obj, Member, _Var, How).

/* Matches a dot syntax variant of set: set(Object.Member) */
is_setter_syntax(set(ObjMember), Obj, Member, _Var, b) :-
    obj_member_syntax(ObjMember, Obj, Member).

/* Matches a dot syntax variant of gset: gset(Object.Member) */
is_setter_syntax(gset(ObjMember), Obj, Member, _Var, nb) :-
    obj_member_syntax(ObjMember, Obj, Member).

/* Matches a dot syntax variant of hset: hset(How, Object.Member) */
is_setter_syntax(hset(How, ObjMember), Obj, Member, _Var, How) :-
    obj_member_syntax(ObjMember, Obj, Member).

/** <predicate> obj_member_syntax(+CompoundObjectMember, -Object, -Member)
    @brief Decomposes Object.Member syntax into Object and Member.
    @param CompoundObjectMember The term representing Object.Member.
    @param Object The object extracted from the compound.
    @param Member The member extracted from the compound.
*/
obj_member_syntax(ObjMember, Obj, Member) :-
    compound(ObjMember), 
    compound_name_arguments(ObjMember, '.', [Obj, Member]), 
    !.

/** <predicate> maybe_expand_md(+MD, +Input, -Output)
    @brief Attempts to expand a goal if it matches the MD pattern.
    @param MD The meta-predicate that defines the mapping.
    @param Input The input term to potentially expand.
    @param Output The expanded version of the input, if applicable.
*/
maybe_expand_md(_MD, I, _) :- \+ compound(I), !, fail.

/* The following lines were previously disabled to avoid certain expansions */
/* previously:
maybe_expand_md(MD, must_det_ll(GoalL), GoalL) :- !.
*/

/* Expands meta goals based on MD pattern */
maybe_expand_md(MD, MDGoal, GoalLO) :- 
    compound_name_arg(MDGoal, MD, Goal), !, 
    expand_md(MD, Goal, GoalLO).

/* Handles maplist expansions for MD */
maybe_expand_md(MD, maplist(P1, GoalL), GoalLO) :- 
    P1 == MD, !, 
    expand_md(MD, GoalL, GoalLO).

/* Skipping duplicate maplist handling case */
maybe_expand_md(MD, maplist(P1, GoalL), GoalLO) :- 
    P1 == MD, !, 
    expand_md(MD, GoalL, GoalLO).

/* Handles sub-term expansion within compound terms */
maybe_expand_md(MD, I, O) :- 
    sub_term(C, I), 
    compound(C), 
    compound_name_arg(C, MD, Goal), 
    compound(Goal), 
    Goal = (_, _),
    once((expand_md(MD, Goal, GoalO), substM(I, C, GoalO, O))), 
    I \=@= O.

/* previously: Skipped this expansion rule due to redundant handling */
/*maybe_expand_md(MD, I, O) :- sub_term(S, I), compound(S), S = must_det_ll(G),
  once(expand_md(MD, S, M)), M \= S,
*/

/** <predicate> expand_md(+MD, +Term, -Expanded)
    @brief Expands a term based on the meta-predicate MD.
    @param MD The meta-predicate identifier.
    @param Term The term to expand.
    @param Expanded The expanded version of the term.
*/
expand_md(_MD, Nil, true) :- Nil == [], !.

/* If the term is not callable, it cannot be expanded */
expand_md(_MD, Var, Var) :- \+ callable(Var), !.

/* Expands a list of terms, recursively expanding head and tail */
expand_md(MD, [A|B], (AA, BB)) :- 
    assertion(callable(A)), 
    assertion(is_list(B)), 
    !, 
    expand_md1(MD, A, AA), 
    expand_md(MD, B, BB).

/* Expands non-list terms */
expand_md(MD, A, AA) :- !, expand_md1(MD, A, AA).

/** <predicate> prevents_expansion(+Term)
    @brief Predicate to check if a term should not be expanded due to trace conditions.
*/
prevents_expansion(A) :- is_trace_call(A).

/** <predicate> is_trace_call(+Term)
    @brief Checks if a term is related to trace debugging, which prevents expansion.
*/
is_trace_call(A) :- A == trace.
is_trace_call(A) :- A == itrace.

/** <predicate> skip_expansion(+Term)
    @brief Skips certain expansions, such as cuts or control structures.
*/
skip_expansion(A) :- var(A), !, fail.
skip_expansion(!).
skip_expansion(false).
skip_expansion(true).

/* Skips expansion for specific compound terms based on functor/arity */
skip_expansion(C) :- compound(C), functor(C, F, A), skip_fa_expansion(F, A).

/** <predicate> skip_fa_expansion(+Functor, +Arity)
    @brief Predicate to skip expansion for certain functor/arity pairs.
*/
skip_fa_expansion(once, 1).
skip_fa_expansion(call, _).
skip_fa_expansion(if_t, 2).

/** <predicate> expand_md1(+MD, +Term, -ExpandedTerm)
    @brief Expands an individual term based on the meta-predicate MD.
*/
expand_md1(_MD, Var, Var) :- \+ callable(Var), !.

/* Skips expansion for cuts or other skipped constructs */
expand_md1(_MD, Cut, Cut) :- skip_expansion(Cut), !.

/* Expands a compound term where the functor matches MD */
expand_md1(MD, MDAB, AABB) :- 
    compound(MDAB), 
    compound_name_arg(MDAB, MD, AB), 
    !, 
    expand_md(MD, AB, AABB).

/* Expands maplist constructs */
expand_md1(MD, maplist(P1, A), md_maplist(MD, P1, A)) :- !.
expand_md1(MD, maplist(P2, A, B), md_maplist(MD, P2, A, B)) :- !.
expand_md1(MD, maplist(P3, A, B, C), md_maplist(MD, P3, A, B, C)) :- !.

/* Expands user-defined maplist constructs */
expand_md1(MD, my_maplist(P1, A), md_maplist(MD, P1, A)) :- !.
expand_md1(MD, my_maplist(P2, A, B), md_maplist(MD, P2, A, B)) :- !.
expand_md1(MD, my_maplist(P3, A, B, C), md_maplist(MD, P3, A, B, C)) :- !.

/* previously:
expand_md1(MD, Goal, O) :- \+ compound(Goal), !, O = must_det_ll(Goal).
expand_md1(MD, (A, B), ((A, B))) :- remove_must_det(MD), prevents_expansion(A), !.
expand_md1(MD, (A, B), must_det_ll((A, B))) :- prevents_expansion(A), !.
*/

/* Expands conjunctions (A, B) into (AA, BB) by recursively expanding both */
expand_md1(MD, (A, B), (AA, BB)) :- !, expand_md(MD, A, AA), expand_md(MD, B, BB).

/* Expands conditional statements (C*->A;B) */
expand_md1(MD, (C*->A;B), (CC*->AA;BB)) :- 
    !, 
    expand_md(MD, A, AA), 
    expand_md(MD, B, BB), 
    expand_must_not_error(C, CC).
/* 
  expand_md1/3: Expands meta-data (MD) for various Prolog constructs.
  This predicate dispatches based on different forms of Prolog terms (like `->`, `;`, `call_cleanup`, etc.).
  It applies expansions to both the "then" and "else" branches of conditional expressions, 
  as well as ensuring certain conditions are met for meta-data.
  
  @param MD The meta-data to expand.
  @param InTerm The input term to expand.
  @param OutTerm The resulting expanded term.
*/
expand_md1(MD, (C->A; B), (CC->AA; BB)) :- 
  !, 
  expand_md(MD, A, AA), % Expand the meta-data for the "then" part of the condition
  expand_md(MD, B, BB), % Expand the meta-data for the "else" part of the condition
  expand_must_not_error(C, CC). % Ensure that "C" is not skipped or handled incorrectly

/* 
  Handles disjunctions (C;B). Expands the metadata of both the first and second parts of the disjunction.
*/
expand_md1(MD, (C; B), (CC; BB)) :- 
  !, 
  expand_md(MD, B, BB), % Expand the second part
  expand_must_not_error(C, CC). % Ensure correctness for the first part

/* 
  Handles `locally(C, A)` constructs. Expands `A` inside the locally construct while leaving `C` untouched.
*/
expand_md1(MD, locally(C, A), locally(C, AA)) :- 
  !, 
  expand_md(MD, A, AA).

/* 
  Handles the call_cleanup/2 construct. Ensures both the main goal and the cleanup part are expanded.
*/
expand_md1(MD, call_cleanup(A, B), call_cleanup(AA, BB)) :- 
  !, 
  expand_md(MD, A, AA), 
  expand_md(MD, B, BB).

/* 
  Handles setup_call_cleanup/3. Expands each part: setup, main goal, and cleanup.
*/
expand_md1(MD, setup_call_cleanup(C, A, B), setup_call_cleanup(CC, AA, BB)) :- 
  !, 
  expand_md(MD, C, CC), 
  expand_md(MD, A, AA), 
  expand_md(MD, B, BB).

/* 
  Handles module-qualified predicates (M:P). Expands the predicate part.
*/
expand_md1(MD, M:P, M:AABB) :- 
  !, 
  expand_md(MD, P, AABB).

/* 
  Expands predicates with meta-predicate properties.
  This clause checks if the predicate is meta, and if so, applies the correct argument expansion.
*/
expand_md1(MD, P, AABB) :- 
  predicate_property(P, (meta_predicate(MP))), % Check if it's a meta-predicate
  strip_module(P, _, SP), 
  strip_module(MP, _, SMP), 
  kaggle_arc_1_pred(_, SP), % Check if it belongs to a specific context (e.g., kaggle)
  \+ skippable_built_in(P), % Skip built-in predicates if necessary
  SP =.. [F | Args], 
  SMP =.. [F | Margs], 
  !,
  maplist(expand_meta_predicate_arg(MD), Margs, Args, EArgs), % Expand arguments
  AABB =.. [F | EArgs].

/* 
  Expands goals without removing the must-det condition (which ensures determinism).
*/
expand_md1(MD, A, MDAA) :- 
  \+ remove_must_det(MD), 
  !, 
  expand_goal(A, AA), % Expand the goal
  compound_name_arg(MDAA, MD, AA). % Reconstruct the expanded term with meta-data

/* 
  Default case for goal expansion if other cases don't apply.
*/
expand_md1(_MD, A, AA) :- 
  expand_goal(A, AA).

/* 
  Ensures that certain goals are expanded as "must_not_error". 
  If the must-not-error flag is set, no expansion is done.
*/
expand_must_not_error(C, C) :- 
  remove_must_det(must_not_error), 
  !.

/* 
  Wraps goals with the must_not_error/1 construct if they are not meta-predicates.
*/
expand_must_not_error(C, CC) :- 
  \+ predicate_property(C, meta_predicate(_)), 
  !, 
  CC = must_not_error(C), 
  !.

/* 
  Otherwise, applies meta-data expansion to ensure the goal is correctly handled with must_not_error.
*/
expand_must_not_error(C, CC) :- 
  expand_md(must_not_error, C, CC).

/* 
  kaggle_arc_1_pred/2: Checks if a predicate belongs to a Kaggle-related file and is not imported from another module.
  This is a filtering mechanism to avoid processing predicates from non-relevant files.

  @param M The module.
  @param P The predicate.
*/
kaggle_arc_1_pred(M, P) :- 
  predicate_property(M:P, file(F)), % Check if the predicate has an associated file
  \+ predicate_property(M:P, imported_from(_)), % Ensure it's not imported
  \+ \+ atom_contains(F, 'arc_'), % File name should contain 'arc_'
  \+ atom_contains(F, '_pfc'), % Skip certain files
  \+ atom_contains(F, '_afc'), 
  true. % Always succeeds if the checks are passed

/* 
  skippable_built_in/1: Identifies system built-in predicates that can be skipped during processing.
  Built-in predicates that are ISO or marked notrace are skipped.

  @param MP The meta-predicate to check.
*/
skippable_built_in(MP) :- 
  strip_module(MP, _, P), 
  predicate_property(system:P, built_in), % Check if it's a system built-in
  once(predicate_property(system:P, iso); predicate_property(system:P, notrace)).

% Goal expansion logic follows below...

/* 
  expand_meta_predicate_arg/4: Expands meta-predicate arguments based on their mode ('?', '+', '-', ':', etc.).
  The mode dictates how the argument should be handled.

  @param MD The meta-data.
  @param Mode The argument mode.
  @param Arg The argument to expand.
  @param ExpandedArg The resulting expanded argument.
*/
expand_meta_predicate_arg(_MD, '?', A, A) :- !.
expand_meta_predicate_arg(_MD, '+', A, A) :- !.
expand_meta_predicate_arg(_MD, '-', A, A) :- !.
expand_meta_predicate_arg(MD, ':', A, AA) :- !, expand_md1(MD, A, AA).
expand_meta_predicate_arg(MD, 0, A, AA) :- !, expand_md1(MD, A, AA).
expand_meta_predicate_arg(_MD, _, A, A).

/* previously: ... 
   This dead code was likely related to older goal expansion mechanisms.
   It was skipped as it no longer fits the current architecture.
*/

/* 
  goal_expansion_getter/2: Attempts to retrieve an expanded version of a goal. 
  It handles simple cases directly, and more complex ones by recursively expanding arguments.

  @param Goal The original goal.
  @param Out The expanded goal.
*/
goal_expansion_getter(Goal, O) :- 
  \+ compound(Goal), % If it's not a compound term, return as is
  !, 
  O = Goal.

goal_expansion_getter(I, O) :- 
  md_like(MD), 
  maybe_expand_md(MD, I, O), 
  I \=@= O, % Ensure the expanded goal is different
  !.

goal_expansion_getter(Goal, get_kov(Func, Self, Value)) :- 
  compound(Goal), 
  compound_name_arguments(Goal, '.', [Self, Func, Value]), 
  var(Value), % Check for partially instantiated terms
  !.

goal_expansion_getter(Goal, Out) :- 
  compound_name_arguments(Goal, F, Args), 
  maplist(goal_expansion_getter, Args, ArgsOut), % Recursively expand arguments
  compound_name_arguments(Out, F, ArgsOut).

/* 
  Exports the goal_expansion_getter/2 predicate to be accessible externally.
*/
:- export(goal_expansion_getter/2).

/* 
  Imports the goal_expansion_getter/2 predicate to the system namespace for internal use.
*/
:- system:import(goal_expansion_getter/2).

/* 
  goal_expansion_setter/2: This predicate seems intended to handle "setter" goals, 
  but it fails on non-compound terms.

  @param Goal The goal to set.
  @param O The output goal after setting.
*/
goal_expansion_setter(Goal, _) :- 
  \+ compound(Goal), 
  !, 
  fail. % Non-compound goals cannot be processed

goal_expansion_setter(I,O):- md_like(MD),maybe_expand_md(MD,I,O),I\=@=O,!.

/**
 * goal_expansion_setter(+Goal, -ExpandedGoal)
 *
 * Expands the given Goal into its corresponding ExpandedGoal.
 * This predicate applies specific transformations based on the structure of the goal.
 * It handles setters (goal expansion for setting values).
 *
 * @param Goal The input goal.
 * @param ExpandedGoal The transformed goal after expansion.
 *
 * @example
 * ?- goal_expansion_setter(set_omember(a, b, c), ExpandedGoal).
 * ExpandedGoal = set_omember(b, a, b, c).
 */
goal_expansion_setter(G, GO) :-
    % Removes must_det condition from the goal G.
    remove_must_det(MD), 
    !,  % Cut to prevent backtracking.
    % Removes must_det set and rewrites the goal G to GG.
    remove_mds(MD, G, GG),
    % Recursively process the new goal GG.
    goal_expansion_setter(GG, GO).

/* previously: 
%goal_expansion_setter(GG, GO):- remove_must_det(MD), sub_term(G,GG),compound(G),
% G = must_det_ll(GGGG),subst001(GG,G,GGGG,GGG),!,goal_expansion_setter(GGG,GO).
The above code was skipped because it seems redundant with existing logic,
or has been replaced by better abstractions. */

% Expands a goal involving multiple terms (conjunction of two goals).
% Skipped because it's a simplified case of goal expansion.
%goal_expansion_setter((G1, G2), (O1, O2)):- !, expand_goal(G1,O1), expand_goal(G2,O2),!.

% Handles the case where the goal is set_omember with 4 arguments.
goal_expansion_setter(set_omember(A, B, C, D), set_omember(A, B, C, D)) :- !.

% Handles the case where the goal is set_omember with 3 arguments and adds a default 'b'.
goal_expansion_setter(set_omember(A, B, C), set_omember(b, A, B, C)) :- !.

% Transforms a '.' predicate into a get_kov structure if Value is uninstantiated.
goal_expansion_setter(Goal, get_kov(Func, Self, Value)) :-
    compound(Goal),  % Ensure Goal is a compound term.
    compound_name_arguments(Goal, '.', [Self, Func, Value]),  % Deconstruct '.' predicate.
    var(Value).  % Only proceed if Value is a variable.

% Attempt to expand MD-like patterns.
goal_expansion_setter(I, O) :-
    md_like(MD),  % Check if there's a must_det pattern.
    maybe_expand_md(MD, I, M),  % Attempt to expand it.
    I \=@= M,  % Ensure the new goal M is different from I.
    !,  % Cut to avoid backtracking.
    goal_expansion_setter(M, O).  % Recursively expand the new goal M.

% Expand meta-predicate goals (skipped due to a fail condition and might be handled elsewhere).
goal_expansion_setter(Goal, Out) :-
    predicate_property(Goal, meta_predicate(_)),  % Check if it's a meta-predicate.
    !, fail,  % Fail and skip this clause.
    tc_arg(N1, Goal, P),  % Get the argument at position N1.
    goal_expansion_setter(P, MOut),  % Recursively expand P.
    setarg(N1, Goal, MOut),  % Set the expanded argument.
    !,  % Cut to avoid backtracking.
    expand_goal(Goal, Out).  % Expand the goal.

% Expands goals that follow setter syntax.
goal_expansion_setter(Goal, Out) :-
    tc_arg(N1, Goal, P),  % Get the N1th argument of the goal.
    is_setter_syntax(P, Obj, Member, Var, How),  % Check if it's setter syntax.
    setarg(N1, Goal, Var),  % Modify the argument in the original goal.
    !,  % Cut to avoid backtracking.
    expand_goal((Goal, set_omember(How, Member, Obj, Var)), Out).  % Expand into a set_omember.

% Handles a specific setter case with get_kov.
goal_expansion_setter(Goal, Out) :-
    get_setarg_p1(setarg, I, Goal, P1),  % Extract setarg data.
    compound(I),  % Ensure I is a compound term.
    compound_name_arguments(I, '.', [Self, Func, Value]),  % Deconstruct '.' predicate.
    call(P1, get_kov(Func, Self, Value)),  % Call P1 with the get_kov predicate.
    !,  % Cut to avoid backtracking.
    expand_goal(Goal, Out).  % Expand the goal.

% Another case handling setter syntax expansion.
goal_expansion_setter(Goal, Out) :-
    get_setarg_p1(setarg, I, Goal, P1),  % Extract setarg data.
    is_setter_syntax(I, Obj, Member, Var, How),  % Check setter syntax.
    call(P1, Var),  % Call P1 with Var.
    !,  % Cut to avoid backtracking.
    expand_goal((Goal, set_omember(How, Member, Obj, Var)), Out).  % Expand the goal.

% Export the goal_expansion_setter predicate for use in other modules.
:- export(goal_expansion_setter/2).

% Import the goal_expansion_setter predicate into the system module.
:- system:import(goal_expansion_setter/2).

/* previously: 
system:term_expansion((Head:-Goal),I,(Head:-Out),O):- nonvar(I),  compound(Goal),
goal_expansion_setter(Goal,Out),Goal\=@=Out,I=O,!,nop((print(goal_expansion_getter(Goal-->Out)),nl)).
The above code was skipped because it's commented out, and its logic may have been refactored into more abstract forms elsewhere.
*/

% Handles term expansions for system goals involving setters.
arc_term_expansion1((system:term_expansion((Head:-Body), I, Out, O):-
    nonvar(I),  % Ensure I is not a variable.
    compound(Head),  % Ensure Head is a compound term.
    term_expansion_setter((Head:-Body), Out),  % Apply term expansion setter.
    (Head:-Body) = In, In \== Out,  % Ensure the expanded term is different.
    I = O,  % Set I to O.
    !,  % Cut to avoid backtracking.
    nop((print(term_expansion_setter(In-->Out)), nl)))).  % Optionally print debug information.

/* previously: 
%system:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),
% Goal\==Out,I=O,!,(print(goal_expansion_getter(Goal-->Out)),nl)).
This block was skipped because it deals with goal expansion getter logic, which is handled by the setter logic above.
*/

% Handles goal expansion for user-defined goals (similar to system goals).
arc_term_expansion1((goal_expansion(Goal, I, Out, O):-
    goal_expansion_setter(Goal, Out),  % Apply the setter expansion.
    Goal \== Out,  % Ensure the goal has changed.
    I = O,  % Set I to O.
    !,  % Cut to avoid backtracking.
    nop((print(goal_expansion_setter(Goal-->Out)), nl)))).  % Optionally print debug information.

% Exporting arc_term_expansions/1 predicate.
:- export(arc_term_expansions/1).

% Expands rules for arc_term_expansion when enabled.
arc_term_expansions(H:- (current_prolog_flag(arc_term_expansion, true), B)):-
    arc_term_expansion1(H:-B).  % Apply arc term expansion.

% Exporting enable_arc_expansion/0 predicate.
:- export(enable_arc_expansion/0).

% Enables arc term expansion by asserting new rules.
enable_arc_expansion :-
    forall(arc_term_expansions(Rule),  % For all arc term expansions,
        (strip_module(Rule, M, Rule0),  % Strip the module from the rule.
        nop(u_dmsg(asserta_if_new(Rule, M, Rule0))),  % Optionally print debug info.
        asserta_if_new(Rule))),  % Assert the rule if it's new.
    set_prolog_flag(arc_term_expansion, true).  % Enable the arc term expansion flag.

% Exporting disable_arc_expansion/0 predicate.
:- export(disable_arc_expansion/0).
% Predicate to disable arc expansion rules by retracting them and setting a flag
% This predicate disables all arc_term_expansion rules and ensures that they are no longer applied.
% @param None
% @example disable_arc_expansion. % Disables the arc expansion rules
disable_arc_expansion:-
    % For each arc_term_expansions rule, retract it from the database
    forall(arc_term_expansions(Rule), forall(retract(Rule), true)),
    % Set the Prolog flag to false, preventing further arc term expansion
    set_prolog_flag(arc_term_expansion, false).

% Directive to declare a multifile predicate
% The `goal_expansion/4` predicate can be defined in multiple files.
:- multifile(goal_expansion/4).

% Directive to declare a dynamic predicate
% The `goal_expansion/4` predicate can be modified during runtime.
:- dynamic(goal_expansion/4).

% Predicate for expanding goals during execution, specifically handling compound goals
% @param G - The goal to expand
% @param I - Input context for the expansion
% @param GG - Expanded goal output
% @param O - Output context (usually identical to input)
% @example goal_expansion(goal, input, expanded_goal, output).
goal_expansion(G, I, GG, O):-
    % Ensure that the input is non-variable and that the source location is known
    nonvar(I), source_location(_, _),
    % If the goal is a compound term, proceed with expansion
    compound(G),
    % If a must_det (deterministic) condition is removed, expand the goal accordingly
    % If not, check for md_like conditions and try to expand those
    (remove_must_det(MD) -> remove_mds(MD, G, GG)
    ; (md_like(MD), maybe_expand_md(MD, G, GG))),
    % Input and output contexts are set to be equal
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


% Predicate to check if a goal is a map (using some virtual machine map logic)
% @param G - The goal to check
is_map(G):- 
    % Check if G is a virtual machine map
    is_vm_map(G), !.

% Sort the input list I into output list O, safely handling errors
% @param I - Input list
% @param O - Output list (sorted)
sort_safe(I, O):- 
    % Attempt to sort the list; if sorting fails, return the input as output
    catch(sort(I, O), _, I = O).

% Predicate to append two lists
% @param A - First list
% @param B - Second list
my_append(A, B):- 
    % Append list B to list A
    append(A, B).

% Predicate to append two lists and output the result in a third list
% @param A - First list
% @param B - Second list
% @param C - Resulting list after appending A and B
my_append(A, B, C):- 
    % Append list B to list A, resulting in list C
    append(A, B, C).

% Execute a goal with tty output set to false (for silent execution)
% @param Goal - The goal to execute
with_tty_false(Goal):- 
    % Temporarily set the tty flag to false and execute the goal
    with_set_stream(current_output, tty(false), Goal).

% Execute a goal with tty output set to true (for normal execution)
% @param Goal - The goal to execute
with_tty_true(Goal):- 
    % Temporarily set the tty flag to true and execute the goal
    with_set_stream(current_output, tty(true), Goal).

% Count occurrences of a goal G and store the result in N
% @param G - The goal to count
% @param N - The number of occurrences of G
% @example count_of(member(X, [1,2,3]), N). % N = 3
count_of(G, N):- 
    % Find all variants of G, collect them in a set, and count the length of the set
    findall_vset(G, G, S), length(S, N).

% Find all variants of T where goal G is true, store in set S
% @param T - Term to find
% @param G - Goal to check
% @param S - Set of variants found
findall_vset(T, G, S):- 
    % Find all instances of T where G is true, convert the list to a set
    findall(T, G, L), variant_list_to_set(L, S).

% Flatten a list of objects into a single list
% @param Objs - The list of objects to flatten
% @param ObjsO - The flattened list
flatten_objects(Objs, ObjsO):- 
    % Flatten the list, ensuring that nested lists are merged into one
    flatten([Objs], ObjsO), !.

% Compare if two terms are equal using variable handling rules
% @param E - First term
% @param S - Second term
var_e(E, S):- 
    % If E and S are the same, succeed
    E == S, !.
var_e(E, S):- 
    % If E is non-variable or an attributed variable, compare using unification
    (nonvar(E); attvar(E)), !, E =@= S.

% Convert a list of variants into a set, removing duplicates
% @param List - List of variants
% @param Out - Resulting set of variants
variant_list_to_set([E|List], Out):- 
    % Select a variant from the list, compare it to E, and continue
    select(S, List, Rest), var_e(E, S), !, 
    variant_list_to_set([E|Rest], Out).
variant_list_to_set([E|List], [E|Out]):- 
    % If E is unique, add it to the set and continue
    !, variant_list_to_set(List, Out).
variant_list_to_set(H, H).

% Predicate to substitute terms in a structure using non-backtracking set argument
% @param Obj - The object to modify
% @param New - The new term to insert
% @param Old - The old term to replace
nb_subst(Obj, New, Old):- 
    % Get the position P1 where the substitution needs to happen and apply it
    get_setarg_p1(nb_setarg, Found, Obj, P1), Found =@= Old,
    p1_call(P1, New), !, nb_subst(Obj, New, Old).
nb_subst(_Obj, _New, _Old).

% Predicate to check if any arc-related files are present in a list or term
% @param Some - The list or atom to check
system:any_arc_files(Some):- 
    % If Some is a non-empty list, map over the list and check each element
    is_list(Some), !, Some \== [], maplist(any_arc_files, Some).
system:any_arc_files(Some):- 
    % Check if the atom contains the word 'arc'
    atom_contains(Some, 'arc').

% Directive to declare a thread-local predicate
% The `in_memo_cached/5` predicate is local to each thread.
:- thread_local(in_memo_cached/5).

% Directive to declare a multifile hook for the make process
% The `prolog:make_hook/2` predicate can be defined across multiple files.
:- multifile(prolog:make_hook/2).

% Directive to declare a dynamic hook for the make process
% The `prolog:make_hook/2` predicate can be modified at runtime.
:- dynamic(prolog:make_hook/2).

% Hook to handle actions before the make process, clearing caches if necessary
% @param Some - The files or terms involved in the make process
prolog:make_hook(before, Some):- 
    % If there are any arc-related files, clear all caches before proceeding
    any_arc_files(Some), forall(muarc:clear_all_caches, true).

% Multifile predicate to clear all caches (may be defined in other files)
:- multifile(muarc:clear_all_caches/0).

% Dynamic predicate to clear all caches (modifiable at runtime)
:- dynamic(muarc:clear_all_caches/0).

% Predicate to clear all caches unless extreme caching is enabled
% @param None
muarc:clear_all_caches:- 
    % If extreme caching is not enabled, retract all cached entries
    \+ luser_getval(extreme_caching, true), 
    retractall(in_memo_cached(_, _, _, _, _)), fail.

% Memoization mechanism for caching goals to avoid redundant computations
% @param G - The goal to cache
arc_memoized(G):- 
    % If G is a compound term and ground, attempt memoization
    compound(G), ground(G), functor(G, F, 1), functor(C, F, 1), !, 
    arc_memoized(C), G = C, !.
arc_memoized(G):-
  copy_term(G,C,GT),
  (Key = (C+GT)),
  (in_memo_cached(Key,C,track,started,Info)->throw(already_memoizing(in_memo_cached(Key,C,track,started,Info))) ; true),
  numbervars(Key,0,_,[attvar(bind),singletons(true)]),!,
  setup_call_cleanup((asserta(in_memo_cached(Key,C,track,started,_),Started)),
  catch(
  (in_memo_cached(Key,C,GT,Found,AttGoals)*->(G=Found,maplist(call,AttGoals))

    ; ((call(G),copy_term(G,CG,GG)) *->asserta(in_memo_cached(Key,C,GT,CG,GG))
                  ;asserta(in_memo_cached(Key,C,GT,failed,_)))),
  E, (retractall(in_memo_cached(Key,C,GT,_,_)),throw(E))),erase(Started)).




/**
 * Set the nth element in a list.
 *
 * Replaces the element at position `N` in the list with element `E`.
 * 
 * @param N Index of the element to replace.
 * @param Row List in which to replace the element.
 * @param E Element to insert.
 * @param RowMod Modified list.
 * @example 
 * ?- set_nth1(2, [a, b, c], x, L).
 * L = [a, x, c].
 */
set_nth1(1, [_|Row], E, [E|Row]) :- !.
set_nth1(N, [W|Row], E, [W|RowMod]) :-
    /* Decrement the index and recurse until N = 1 */
    Nm1 is N - 1,
    set_nth1(Nm1, Row, E, RowMod).

/**
 * Find all solutions and count them.
 *
 * Uses `findall_set/3` to find the set of all solutions and then counts the 
 * number of solutions found.
 * 
 * @param T Template of the solutions.
 * @param G Goal to be satisfied.
 * @param N Number of solutions.
 * @example 
 * ?- findall_count(X, member(X, [a, b, b, c]), N).
 * N = 3.
 */
findall_count(T, G, N) :- 
    findall_set(T, G, S),
    length(S, N).

/**
 * Find all solutions and return as a set.
 *
 * Uses `findall/3` to collect solutions, then converts the list into a set
 * to remove duplicates.
 * 
 * @param T Template of the solutions.
 * @param G Goal to be satisfied.
 * @param S Set of unique solutions.
 * @example 
 * ?- findall_set(X, member(X, [a, b, b, c]), S).
 * S = [a, b, c].
 */
findall_set(T, G, S) :- 
    findall(T, G, L),
    list_to_set(L, S).

/**
 * Create a list initialized with a specified element.
 *
 * Generates a list of length `N`, where every element is `E`.
 * 
 * @param N Number of elements in the list.
 * @param E Element to initialize the list with.
 * @param List Resulting list.
 * @example 
 * ?- make_list_inited(3, x, L).
 * L = [x, x, x].
 */
make_list_inited(0, _, []) :- !.
make_list_inited(1, E, [E]) :- !.
make_list_inited(N, E, [E|List]) :-
    /* Decrement the counter and recurse */
    Nm1 is N - 1,
    make_list_inited(Nm1, E, List).

/**
 * Retrieve the nth clause of a predicate.
 *
 * @param P Predicate to search for.
 * @param I Index of the clause to retrieve.
 * @example 
 * ?- nth_fact(my_predicate, 2).
 */
nth_fact(P, I) :- 
    clause(P, true, Ref),
    nth_clause(P, I, Ref).

/**
 * Check if a term is nonvar or a compound term.
 *
 * @param C The term to check.
 * @example 
 * ?- nonvar_or_ci(X).
 * false.
 */
nonvar_or_ci(C) :- 
    (nonvar(C); attvar(C)), 
    !.

/**
 * Add information to both test and pair rule sets.
 *
 * @param Info The information to add.
 * @example 
 * ?- add_i('info').
 */
add_i(Info) :-
    quietly((
        tersify(Info, InfoT),
        luser_getval(test_rules, TRules),
        luser_getval(pair_rules, PRules),
        /* Add the tersified Info to both sets */
        nb_set_add(TRules, InfoT),
        nb_set_add(PRules, InfoT),
        nop(pp(cyan, +InfoT))
    )).

/**
 * Append a term `i(F)` to the info and add it.
 *
 * @param F The term to append.
 * @param Info The information to append to.
 * @example 
 * ?- add_i(fact, 'info').
 */
add_i(F, Info) :-
    append_term(i(F), Info, FInfo),
    add_i(FInfo).

/* Skipped old do_action code with explanation */
/* previously: do_action(Info):- guess_pretty(Info),add_i(action,Info),call(Info).
   Explanation: This version was removed because it included a guess_pretty step,
   which is now handled in a different context elsewhere in the program. */

% Proceeding with the new, simpler implementation
do_action(Call) :- 
    !, 
    copy_term(Call, Info), 
    call(Call), 
    add_i(action, Info).

/* Remaining predicates follow similar patterns */

add_action(Info):- add_i(action,Info).
add_note(Info):- add_i(note,Info).
add_indiv(W,Info):- add_i(indiv(W),Info).
add_comparitor(Info):- add_i(comparitor,Info).
show_rules:-
 luser_getval(pair_rules,PRules), maplist(pp(cyan),PRules),
 luser_getval(test_rules,TRules), maplist(pp(blue),TRules),
 !.


sub_atom_value(TestID,A):- sub_term(A,TestID),(atom(A);string(A)).

my_list_to_set(List, Set):- my_list_to_set(List, (=) ,Set).
my_list_to_set_variant(List, Set):- my_list_to_set(List, (=@=) ,Set).
my_list_to_set_cmp(List, Set):- my_list_to_set(List, (=@=) ,Set).

my_list_to_set([E|List],P2, Set):- select(C,List,Rest), p2_call(P2, E,C), !, my_list_to_set([E|Rest],P2, Set).
my_list_to_set([E|List],P2, [E|Set]):-!, my_list_to_set(List,P2, Set).
my_list_to_set([],_,[]).

my_list_to_set_cmp([E|List],C3, Set):- select(C,List,Rest), call(C3,R,E,C),
   R== (=), my_list_to_set_cmp([C|Rest],C3, Set),!.
  my_list_to_set_cmp([E|List],C3, [E|Set]):-!, my_list_to_set_cmp(List,C3, Set).
my_list_to_set_cmp([],_,[]).


contains_nonvar(N,Info):- sub_term(E,Info),nonvar_or_ci(E),E=N,!.

max_min(A,B,C,D):- must_be_free(C),must_be_free(D),max_min0(A,B,C,D).
max_min0(A,B,B,B):- plain_var(A).
max_min0(A,B,A,A):- plain_var(B),!.
max_min0(A,B,C,D):- number(A),number(B), !, ((A > B) -> (C=A, D=B) ; (C=B, D=A)).
max_min0(_,A,A,A):- number(A),!.
max_min0(A,_,A,A):- number(A),!.
max_min0(_,_,_,_).

as_debug(L,G):- as_debug(L,true,G).
as_debug(9,_,_):- !.
as_debug(_,C,G):- ignore(catch((call(C)->wots(S,G),format('~NDEBUG: ~w~N',[S]);true),_,true)).

shall_count_as_same(A,B):- same_term(A,B),!. % unify ok_ok cmatch
shall_count_as_same(A,B):- plain_var(A),!,A==B.
shall_count_as_same(A,B):- atomic(A),!, A=@=B.
shall_count_as_same(A,B):- var(B),!,A=@=B.
shall_count_as_same(A,B):- A=@=B,!.



/** 
 * shall_count_as_same(+A, +B)
 *
 * Predicate to determine whether two terms A and B should be considered the same.
 * Uses copy_term to ensure variables are not shared and then checks if A and B are identical
 * both structurally and syntactically using @=.
 * The use of double negation (\+ \+) ensures that the comparison fails gracefully.
 * 
 * @param A First term.
 * @param B Second term.
 * @example 
 * ?- shall_count_as_same(foo(X), foo(_)).
 * true.
 */
shall_count_as_same(A, B) :- 
    copy_term(B, CB),       % Copy term B into CB to ensure no variable sharing.
    copy_term(A, CA),       % Copy term A into CA to ensure no variable sharing.
    \+ \+ (A = B,           % Perform double negation to check if A and B are equal.
           B =@= CB,        % Check if B and its copy CB are syntactically identical.
           A =@= CA),       % Check if A and its copy CA are syntactically identical.
    !.                      % Cut to prevent backtracking.

/* previously:
   The line below was commented out because the alternative approach is less flexible 
   and doesn't account for variable binding in A and B.
   shall_count_as_same(A, B) :- \+ A \= B, !.
*/

/**
 * count_each(+List, +Group, -Result)
 *
 * Predicate to count the occurrence of each distinct element in List within Group.
 * Uses shall_count_as_same/2 to compare elements and length/2 to calculate the count.
 *
 * @param List The list of elements to count.
 * @param Group The group in which elements are counted.
 * @param Result The result as a list of pairs [Count-Element].
 * @example 
 * ?- count_each([a, b, a], [a, a, b, c], Result).
 * Result = [2-a, 1-b].
 */
count_each([C|L], GC, [Len-C|LL]) :- 
    include(shall_count_as_same(C), GC, Lst),   % Include elements in GC that match C.
    length(Lst, Len),                           % Get the length of the matching elements.
    !,                                          % Cut to prevent backtracking.
    count_each(L, GC, LL).                      % Recur for the rest of the list.
count_each([], _, []).                          % Base case: empty list.

/**
 * count_each_inv(+List, +Group, -Result)
 *
 * Similar to count_each/3 but the result pairs are formatted as Element-Count.
 * 
 * @param List The list of elements to count.
 * @param Group The group in which elements are counted.
 * @param Result The result as a list of pairs [Element-Count].
 * @example 
 * ?- count_each_inv([a, b, a], [a, a, b, c], Result).
 * Result = [a-2, b-1].
 */
count_each_inv([C|L], GC, [C-Len|LL]) :- 
    include(shall_count_as_same(C), GC, Lst),   % Include elements in GC that match C.
    length(Lst, Len),                           % Get the length of the matching elements.
    count_each_inv(L, GC, LL).                  % Recur for the rest of the list.
count_each_inv([], _, []).                      % Base case: empty list.

/**
 * maplist_n(+N, :P, +List)
 *
 * Applies the predicate P with the current index N to each element in the list.
 * Increments N on each recursive call.
 *
 * @param N Starting index.
 * @param P Predicate to apply.
 * @param List Input list.
 * @example 
 * ?- maplist_n(1, writeln, [a, b, c]).
 * 1
 * a
 * 2
 * b
 * 3
 * c
 * true.
 */
maplist_n(N, P, [H1|T1]) :- 
    p2_call(P, N, H1),      % Call predicate P with current index N and head H1.
    N1 is N + 1,            % Increment N.
    maplist_n(N1, P, T1).   % Recur with incremented N and rest of the list.
maplist_n(_, _, []).        % Base case: empty list.

/**
 * maplist_n(+N, :P, +List1, -List2)
 *
 * Applies predicate P with the current index N to corresponding elements of List1 and List2.
 * Increments N on each recursive call.
 *
 * @param N Starting index.
 * @param P Predicate to apply.
 * @param List1 Input list.
 * @param List2 Output list with transformed elements.
 * @example 
 * ?- maplist_n(1, writeln, [a, b], [x, y]).
 * 1
 * a
 * x
 * 2
 * b
 * y
 * true.
 */
maplist_n(N, P, [H1|T1], [H2|T2]) :- 
    call(P, N, H1, H2),     % Call predicate P with current index N, head H1 and corresponding head H2.
    N1 is N + 1,            % Increment N.
    maplist_n(N1, P, T1, T2).  % Recur with incremented N, rest of List1 and List2.
maplist_n(_, _, [], []).    % Base case: both lists are empty.

/* previously:
   The following code to print points grid is preserved but not used, 
   possibly because the current scope doesn't require graphical output handling.
*/

/*
print_points_grid(Points):- 
    points_range(Points, LoH, LoV, HiH, HiV, H, V), 
    writeqln(size_range(LoH, LoV, HiH, HiV, H, V)), 
    points_to_grid(Points, Grid), 
    print_grid(Grid).

print_points_grid(Grid):- 
    points_range(Grid, LoH, LoV, HiH, HiV, _H, _V), 
    print_grid(Grid, LoH, LoV, HiH, HiV, Grid).
*/

/* previously:
   The following code related to kaggle_arc_train and kaggle_arc_eval is skipped 
   because it's tied to a specific data set (Kaggle ARC), and may require access to external data.
   However, the structure is preserved as it may be useful for future use.
*/

/*
%print_trainer:- 
%    kaggle_arc_train(Name, Stuff), 
%    atom_json_term(Stuff, JSON, []), 
%    print_arc(Name, JSON).

%print_evaler:- 
%    kaggle_arc_eval(Name, Stuff), 
%    atom_json_term(Stuff, JSON, []), 
%    print_arc(Name, JSON).
*/


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
map_pred(Pred, P, X) :- map_pred([],Pred, P, X).
%map_pred(NoCycles,_Pred, P, X) :- member(E,NoCycles), E==P,!, X = P.
map_pred(NoCycles,Pred, P, X) :- p2_call(Pred, P, X)*->true;map_pred0(NoCycles,Pred, P, X).

map_pred1(Pred, P, P1) :- map_pred1(P, Pred, P, P1).

map_pred0(_NoCycles,_Pred, Args, ArgSO) :- must_be_free(ArgSO), Args==[],!, ArgSO=[].
map_pred0(_NoCycles, Pred, P, P1) :-  p2_call(Pred, P, P1),!. % *->true;fail.
map_pred0(NoCycles,Pred, P, X) :- fail, attvar(P), !, %duplicate_term(P,X),P=X,
  get_attrs(P,VS), map_pred([P|NoCycles],Pred, VS, VSX), P=X, put_attrs(X,VSX),!.
map_pred0(NoCycles,Pred, P, X):- map_pred1(NoCycles,Pred, P, X).

map_pred1(_NoCycles,_Pred, P, P1) :- ( \+ compound(P) ; is_ftVar(P)), !, must_det_ll(P1=P), !.
% map_pred0(NoCycles,Pred, Args, ArgSO) :- is_list(Args), !, maplist(map_pred([Args|NoCycles],Pred), Args, ArgS), ArgS=ArgSO.
/** <module> Map predicates and grid processing utility

This module contains predicates that map over lists or grid structures
and apply predicates to them. It also includes utility predicates for
handling compound terms, lists, and variable grids.

The predicates make use of some advanced techniques such as `maplist/3`
and custom mapping predicates for handling complex data structures. The code 
also contains legacy, commented-out sections for reference, showing 
previous iterations and dead code explanations.

*/

% File directive: specifies the module containing the utility for mapping predicates
% :- module(map_predicates, [map_pred1/4, maybe_mapgrid/3, mapgrid/5]).

/** 
 * map_pred1(+NoCycles, +Pred, +Input, -Output)
 * 
 * This predicate maps the predicate `Pred` over the input `Input` to produce the output `Output`.
 * It supports both lists and compound terms and ensures no cycles are created by keeping track of 
 * visited elements in the `NoCycles` list.
 *
 * @param NoCycles A list to prevent cycles.
 * @param Pred The predicate to apply.
 * @param Input The input data (either list or compound term).
 * @param Output The transformed output.
 *
 * @example
 * ?- map_pred1([], some_predicate, [1, 2, 3], Output).
 * 
 * This applies `some_predicate` to each element of the list `[1, 2, 3]`.
 */

% If the input is a list, apply the predicate to each element using maplist
map_pred1(NoCycles,Pred, IO, OO) :- 
    is_list(IO),   % Check if the input is a list
    !,             % Cut to prevent backtracking
    maplist(map_pred(NoCycles,Pred), IO, OO).  % Apply map_pred for each element in the list

% If the input is a compound term, recursively map the predicate over each argument
map_pred1(NoCycles,Pred, IO, [O|ArgS]) :-  
    IO = [I|Args],  % Destructure the input into head (I) and tail (Args)
    !,              
    map_pred([IO,ArgS|NoCycles],Pred, I, O),  % Apply the predicate to the head
    map_pred0([IO,I|NoCycles],Pred, Args, ArgS).  % Recursively apply to the tail

% Handling compound terms, preserving the functor while applying the predicate to each argument
map_pred1(NoCycles,Pred, P, P1) :-
    compound_name_arguments(P, F, Args),  % Extract the functor and arguments from the compound term
    maplist(map_pred([P|NoCycles],Pred), Args, ArgS),  % Apply predicate to each argument
    compound_name_arguments(P1, F, ArgS).  % Reconstruct the term with the transformed arguments

/* previously: map_pred(_Pred, P, P).
 * This was a base case that was previously used, but now replaced
 * with a more generic implementation.
*/

/*
:- meta_predicate map_pred(2, ?, ?, ?, ?).
map_pred(Pred, P, X, Sk, P1) :- 
    must_be_free(X),  % Ensure X is a free variable
    p2_call(Pred, P, X),  % Call the predicate Pred with P and X
    !, 
    must(Sk = P1),  % Ensure Sk unifies with P1
    !.
map_pred(_Pred, P, _, _, P1) :- 
    is_ftVar(P),  % Check if P is a flexible variable
    !, 
    must(P1 = P),  % Ensure P1 is equal to P
    !.
map_pred(Pred, [P|Args], X, Sk, [P1|ArgS]) :- 
    !, 
    map_pred(Pred, P, X, Sk, P1),  % Apply predicate to head
    !, 
    must(map_pred(Pred, Args, X, Sk, ArgS)),  % Recursively apply to the tail
    !.
map_pred(Pred, P, X, Sk, P1) :- 
    compound(P),  % Check if P is a compound term
    !, 
    compound_name_arguments(P, F, Args),  % Extract functor and arguments
    map_pred(Pred, [F|Args], X, Sk, [Fs|ArgS]),  % Apply predicate to functor and arguments
    !, 
    compound_name_arguments(P1, Fs, ArgS),  % Reconstruct the compound term
    !.
map_pred(_Pred, P, _, _, P).  % Base case, no-op
*/

% Predicate to check if a term is a cons list
is_cons(A) :- 
    compound(A),  % Check if A is a compound term
    A = [_|_].    % Ensure it has the form of a cons (non-empty list)

into_grid_or_var(G,G):- is_cons(G),!.
into_grid_or_var(G,G):- var(G),!.
into_grid_or_var(O,G):- cast_to_grid(O,G,_Uncast),!.

maybe_mapgrid(P2,I,O):- is_grid(I),!,mapgrid(P2,I,O).
maybe_mapgrid(P3,I,O,M):- is_grid(I),!,mapgrid(P3,I,O,M).
maybe_mapgrid(P4,I,O,M,N):- is_grid(I),!,mapgrid(P4,I,O,M,N).

mapgrid(P4,Grid,GridM,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridM,G2),into_grid_or_var(GridN,G3),into_grid_or_var(GridO,G4),mapg_list(P4,G1,G2,G3,G4).
mapg_list(P4,Grid,GridM,GridN,GridO):- is_list(Grid),!,maplist(mapg_list(P4),Grid,GridM,GridN,GridO).
mapg_list(P4,Grid,GridM,GridN,GridO):- call(P4,Grid,GridM,GridN,GridO),!.

mapgrid(P3,Grid,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),into_grid_or_var(GridO,G3),mapg_list(P3,G1,G2,G3).
mapg_list(P3,Grid,GridN,GridO):- is_list(Grid),!,maplist(mapg_list(P3),Grid,GridN,GridO).
mapg_list(P3,Grid,GridN,GridO):- call(P3,Grid,GridN,GridO),!.

mapgrid(P2, Grid,GridN):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),!,mapg_list(P2, G1,G2).
mapg_list(P2, Grid,GridN):- is_list(Grid),!,maplist(mapg_list(P2),Grid,GridN).
mapg_list(P2, Grid,GridN):- p2_call(P2, Grid,GridN),!.

mapgrid(P1,Grid):- into_grid_or_var(Grid,G1),mapg_list(P1,G1).
mapg_list(P1,Grid):- is_list(Grid),!,maplist(mapg_list(P1),Grid).
mapg_list(P1,Grid):- p1_call(P1,Grid),!.


maplist_ignore(_3,H,I,J):- (H==[];I==[],J==[]),!,(ignore(H=[]),ignore(I=[]),ignore(J=[])).
maplist_ignore(P3,H,I,J):- \+ is_list(H),!, ignore(p2_call(call(P3,H),I,J)).
maplist_ignore(P3,[H|Grid],[I|GridN],[J|GridO]):- maplist_ignore(P3,H,I,J), !,maplist_ignore(P3,Grid,GridN,GridO).

maplist_ignore(_2,H,I):- (H==[];I==[]),!,(ignore(H=[]),ignore(I=[])).
maplist_ignore(P2, H,I):- \+ is_list(H),!, ignore(p2_call(P2, H,I)).
maplist_ignore(P2, [H|Grid],[I|GridN]):- maplist_ignore(P2, H,I), !,maplist_ignore(P2, Grid,GridN).

%p1_or(P1,Q1,E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).

/* 
  p1_call/2 recursively handles logical expressions like conjunctions (and), disjunctions (or), and negations (not).
  Each predicate ensures that the first argument is callable, and then proceeds to evaluate it.
*/

% Attempt to handle disjunctions in the form (P1;Q1).
% @param P1 First part of the disjunction.
% @param Q1 Second part of the disjunction.
% @param E Environment for the call.
p1_call((P1;Q1),E):- 
    must_be(callable,P1),  % Ensure P1 is callable.
    !,  % Use cut to avoid backtracking.
    (p1_call(P1,E);p1_call(Q1,E)).  % Call P1, and if it fails, call Q1.

% Attempt to handle conjunctions in the form (P1,Q1).
% @param P1 First part of the conjunction.
% @param Q1 Second part of the conjunction.
% @param E Environment for the call.
p1_call((P1,Q1),E):- 
    must_be(callable,P1),  % Ensure P1 is callable.
    !,  % Use cut to avoid backtracking.
    (p1_call(P1,E),p1_call(Q1,E)).  % Call both P1 and Q1.

% Handle disjunctions in the form or(P1,Q1).
% @param P1 First part of the disjunction.
% @param Q1 Second part of the disjunction.
% @param E Environment for the call.
p1_call(or(P1,Q1),E):- 
    must_be(callable,P1),  % Ensure P1 is callable.
    !, 
    (p1_call(P1,E);p1_call(Q1,E)).  % Same as above but using the or/2 functor.

% Handle conjunctions in the form and(P1,Q1).
% @param P1 First part of the conjunction.
% @param Q1 Second part of the conjunction.
% @param E Environment for the call.
p1_call(and(P1,Q1),E):- 
    must_be(callable(P1),  % Ensure P1 is callable.
    !, 
    (p1_call(P1,E),p1_call(Q1,E)).  % Same as conjunction above but using the and/2 functor.

% Handle double negation not(not(P1)).
% @param P1 Predicate to negate twice.
% @param E Environment for the call.
p1_call(not(not(P1)),E):- 
    !, 
    \+ \+ p1_call(P1,E).  % Double negation reduces to calling P1.

% Handle negation not(P1).
% @param P1 Predicate to negate.
% @param E Environment for the call.
p1_call(not(P1),E):- 
    !, 
    not(p1_call(P1,E)).  % Negate the result of p1_call.

% Call once(P1) to enforce the predicate only succeeds once.
% @param P1 Predicate to call once.
% @param E Environment for the call.
p1_call(once(P1),E):- 
    !, 
    once(p1_call(P1,E)).

% Ignore failures from P1, allowing success regardless.
% @param P1 Predicate to ignore failures from.
% @param E Environment for the call.
p1_call(ignore(P1),E):- 
    !, 
    ignore(p1_call(P1,E)).

% Check if P1 can succeed without leaving choice points.
% @param P1 Predicate to check.
% @param E Environment for the call.
p1_call(chk(P1),E):- 
    !, 
    \+ \+ (p1_call(P1,E)).  % Check if P1 succeeds without leaving choice points.

% Handle negation \+ (P1).
% @param P1 Predicate to negate.
% @param E Environment for the call.
p1_call( \+ (P1),E):- 
    !, 
    \+ p1_call(P1,E).

% Default case: call P1 in environment E.
% @param P1 Predicate to call.
% @param E Environment for the call.
p1_call(P1,E):- 
    !, 
    call(P1,E).

/* 
  chk/2 is a convenience predicate that wraps around a call to ensure it succeeds without leaving choice points. 
  It provides a way to check success without affecting the logic of the program.
*/
% @param X Predicate to check.
% @param E Environment for the call.
chk(X,E):- 
    \+ \+ call(X,E).  % Check if X succeeds without leaving choice points.

/* 
  p2_call_p2/4 manages sequential execution of two predicates, P2a and P2b.
  The result of the first call is passed as input to the second.
*/
% @param P2a First predicate to call.
% @param P2b Second predicate to call.
% @param A Input to the first call.
% @param B Output from the second call.
p2_call_p2(P2a,P2b,A,B):- 
    p2_call(P2a,A,M),  % Call P2a with input A, result M.
    p2_call(P2b,M,B).  % Call P2b with input M, result B.

/* 
  p2_call/3 processes complex terms like lists, conjunctions, and disjunctions.
  It can handle custom functors like type/2, and also directly call predicates.
*/

% Base case: when P2 is an empty list, A and B are the same.
% @param P2 The empty list.
% @param A Input term.
% @param B Output term.
p2_call(P2,A,B):- 
    P2==[],  % Check if P2 is an empty list.
    !, 
    A=B.  % A and B are equal in this case.

% Special case: call p1_call within p2_call.
% @param P1 Predicate to call via p1_call.
% @param E Input.
% @param O Output.
p2_call(p1_call(P1),E,O):- 
    !, 
    p1_call(P1,E),  % Call P1.
    E=O.  % Set output equal to input.

% Recursive case: handle single-element lists.
% @param P2 A single element.
% @param Grid Input.
% @param GridN Output.
p2_call([P2],Grid,GridN):- 
    !, 
    p2_call(P2, Grid,GridN).  % Process the single element.

% Recursive case: handle multi-element lists.
% @param P2 Head of the list.
% @param P2L Tail of the list.
% @param Grid Input.
% @param GridN Output.
p2_call([P2|P2L],Grid,GridN):- 
    !, 
    p2_call(P2, Grid,GridM),  % Process head.
    p2_call(P2L,GridM,GridN).  % Process tail.

/* The rest of the code follows similar patterns with more specialized cases. */


p2_call(ignore(P2),A,B):- p2_call(P2,A,B)*->true;A=B.
p2_call(type(Type,P2),A,B):- into_type(Type,A,AA),p2_call(P2,AA,B).
p2_call(or(P2,Q2),A,B):- nop(must_be(callable,P2)),!, (p2_call(P2,A,B);p2_call(Q2,A,B)).
p2_call(and(P2,Q2),A,B):- nop(must_be(callable,P2)),!, (p2_call(P2,A,AB),p2_call(Q2,AB,B)).
p2_call(P2,A,B):- must_be(callable,P2), call(P2,A,B).


p1_or(P1A,P1B,X):- p1_call(P1A,X)->true;p1_call(P1B,X).
p1_and(P1A,P1B,X):- p1_call(P1A,X),p1_call(P1B,X).
p1_not(P1,E):- \+ p1_call(P1,E).
p1_ignore(P1,E):- ignore(p1_call(P1,E)).
p1_arg(N,P1,E):- tc_arg(N,E,Arg),p1_call(P1,Arg).
p1_subterm(P1,E):- sub_term(Arg,E),p1_call(P1,Arg).

:- meta_predicate my_partition(-, ?, ?, ?).
my_partition(_,[],[],[]):-!.
my_partition(P1,[H|L],[H|I],E):- \+ \+ p1_call(P1,H),!,
  my_partition(P1,L,I,E).
my_partition(P1,[H|L],I,[H|E]):-
   my_partition(P1,L,I,E),!.
my_partition(P1,H,I,HE):- arcST,ibreak,
  my_partition(P1,[H],I,HE).


mapgroup(P2,G1,L2):- into_list(G1,L1),!, with_my_group(L1,maplist(P2,L1,L2)).
mapgroup(P1,G1):- into_list(G1,L1), !, with_my_group(L1,maplist(P1,L1)).

selected_group(Grp):- nb_current('$outer_group',Grp),!.
selected_group([]).

sub_cmpd(_, LF) :- \+ compound(LF), !, fail.
sub_cmpd(X, X).
sub_cmpd(X, Term) :-
    (   is_list(Term)
    ->  member(E, Term),
        sub_cmpd(X, E)
    ;   tc_arg(_, Term, Arg),
        sub_cmpd(X, Arg)
    ).



%with_my_group([O|Grp],Goal):- compound(O),O=obj(_),!, locally(nb_setval('$outer_group',[O|Grp]),Goal).
with_my_group(_,Goal):- call(Goal).

into_mlist(L,L).
my_maplist(P4,G1,L2,L3,L4):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P4,L1,L2,L3,L4)).
my_maplist(P3,G1,L2,L3):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P3,L1,L2,L3)).
my_maplist(P2,G1,L2):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P2,L1,L2)).
my_maplist(P1,G1):- into_mlist(G1,L1), !, with_my_group(L1,maplist(P1,L1)).


my_include(P1,L,I):- include(p1_call(P1),L,I).
%my_include(P1,[H|L],O):- (p2_call(p1_call(P1),H,HH)*->(my_include(P1,L,I),O=[HH|I]);my_include(P1,L,O)).


/** 
 * my_include/3
 * This predicate is a placeholder for including elements from a list based on a condition.
 * 
 * @param +P1: The condition predicate (currently unused in this version).
 * @param +I: Input list.
 * @param -O: Output list (currently always empty).
 * 
 * @example 
 * ?- my_include(_, _, X). 
 * X = [].
 */
my_include(_,_,[]). % Base case for inclusion, currently outputs an empty list.

/* previously: commented out code for my_exclude with different implementation */
%my_exclude(P1,I,O):- my_include(not(P1),I,O).

/**
 * my_exclude/3
 * This predicate excludes elements from a list based on a condition by partitioning them.
 * 
 * @param +P1: Predicate to decide which elements to exclude.
 * @param +I: Input list.
 * @param -O: Output list with excluded elements.
 * 
 * @example
 * ?- my_exclude(>(3), [1,2,3,4,5], X).
 * X = [1, 2, 3].
 */
my_exclude(P1,I,O):- my_partition(P1,I,_,O). % Exclude elements based on partitioning by P1.

/**
 * subst_1L/3
 * Substitute elements in a term based on a list of substitutions.
 * 
 * @param +List: List of pairs (X-Y) to be substituted.
 * @param +Term: The original term.
 * @param -NewTerm: Term after performing the substitutions.
 * 
 * @example
 * ?- subst_1L([a-b], a, X).
 * X = b.
 */
subst_1L([],Term,Term):-!. % If substitution list is empty, Term remains unchanged.
subst_1L([X-Y|List], Term, NewTerm ) :- % Iterate through substitution list.
  subst0011(X, Y, Term, MTerm ), % Perform substitution on the term.
  subst_1L(List, MTerm, NewTerm ). % Recursively substitute the rest.

/**
 * subst_2L/4
 * Substitute elements in a term based on two lists of substitutions.
 * 
 * @param +List1: First list of substitutions.
 * @param +List2: Second list of substitutions.
 * @param +I: Initial term.
 * @param -O: Final term after substitutions.
 * 
 * @example
 * ?- subst_2L([a, b], [x, y], a, X).
 * X = x.
 */
subst_2L([],_,I,I). % If first list is empty, term remains unchanged.
subst_2L([F|FF],[R|RR],I,O):- % Recursively substitute elements from both lists.
  subst0011(F,R,I,M), % Substitute F with R in the term I.
  subst_2L(FF,RR,M,O). % Recursively apply the rest of the substitutions.

/**
 * subst001/4
 * Wrapper for subst0011/4 to force cut after substitution.
 * 
 * @param +I: Input term.
 * @param +F: From term.
 * @param +R: Replacement term.
 * @param -O: Output term.
 * 
 * @example
 * ?- subst001(a, a, b, X).
 * X = b.
 */
subst001(I,F,R,O):- subst0011(F,R,I,O),!. % Perform substitution and cut.

/**
 * subst0011/4
 * Core substitution predicate, substitutes X with Y in Term.
 * 
 * @param +X: Term to be substituted.
 * @param +Y: Term to substitute with.
 * @param +Term: Input term.
 * @param -NewTerm: Output term after substitution.
 * 
 * @example
 * ?- subst0011(a, b, a, X).
 * X = b.
 */
subst0011(X, Y, Term, NewTerm ) :- 
  copy_term((X,Y,Term),(CX,CY,Copy),Goals), % Copy terms to avoid variable clashes.
  (Goals==[] % Check if there are no constraints.
   -> subst0011a( X, Y, Term, NewTerm ) % Perform direct substitution.
   ; (subst0011a(CX, CY, Goals, NewGoals), % Substitute in the goal constraints.
     (NewGoals==Goals -> % If the new goals are the same as old ones.
       subst0011a( X, Y, Term, NewTerm ) % Perform substitution.
       ; (subst0011a(CX, CY, Copy, NewCopy), % Perform substitution with the copy.
          NewTerm = NewCopy, % Set the new term to the copy.
          maplist(call,NewGoals))))). % Apply all goal constraints.

/**
 * subst0011a/4
 * Helper predicate for substitution. Handles different term structures like lists, compounds, etc.
 * 
 * @param +X: Term to be substituted.
 * @param +Y: Term to substitute with.
 * @param +Term: Input term.
 * @param -NewTerm: Output term after substitution.
 */
subst0011a(X, Y, Term, NewTerm ) :-
 ((X==Term) -> Y=NewTerm ; % If the term is exactly X, replace it with Y.
  (is_list(Term) -> maplist(subst0011a(X, Y), Term, NewTerm ); % If it is a list, substitute in all elements.
   ((\+ compound(Term); Term='$VAR'(_)) -> Term=NewTerm; % If it's not a compound term or a variable, return as is.
     (compound_name_arguments(Term, F, Args), % Decompose compound term into functor and arguments.
      maplist(subst0011a(X, Y), Args, ArgsNew), % Substitute in arguments.
      compound_name_arguments( NewTerm, F, ArgsNew )))))),!. % Rebuild the term with new arguments.

/* previously: code related to specialized substitutions for specific predicates */
/* These substitutions handle cases where specialized comparison (e.g., same_term) is needed. */

/* Directives and predicates continue for specific versions, all following the similar pattern of detailed commenting for handling complex term substitutions. */


subst001C(I,F,R,O):- subst001_p2(same_term,I,F,R,O),!.
subst0011C(F,R,I,O):- subst0011_p2(same_term,F,R,I,O),!.
subst_2LC(F,R,I,O):- subst_2L_p2(same_term,F,R,I,O).

subst_2L_p2(_P2, [],_,I,I):-!.
subst_2L_p2(_P2, _,[],I,I):-!.
subst_2L_p2(P2, [F|FF],[R|RR],I,O):- subst0011_p2(P2, F,R,I,M),subst_2L_p2(P2, FF,RR,M,O).

subst001_p2(P2, I,F,R,O):- subst0011_p2(P2, F,R,I,O),!.

subst_1L_p2(_,  [],Term,Term):-!.
subst_1L_p2(P2, [X-Y|List], Term, NewTerm ) :-
  subst0011_p2(P2, X, Y, Term, MTerm ),
  subst_1L_p2(P2, List, MTerm, NewTerm ).

subst0011_p2(P2, X, Y, Term, NewTerm ) :-
  copy_term((X,Y,Term),(CX,CY,Copy),Goals),
  (Goals==[]
  ->subst0011a_p2(P2, X, Y, Term, NewTerm )
  ;(subst0011a_p2(P2, CX, CY, Goals, NewGoals),
     (NewGoals==Goals ->
       subst0011a_p2(P2, X, Y, Term, NewTerm )
       ; (subst0011a_p2(P2, CX, CY, Copy, NewCopy),
          NewTerm = NewCopy, maplist(call,NewGoals))))).

subst0011a_p2(P2, X, Y, Term, NewTerm ) :-
 (p2_call(P2,X,Term)-> Y=NewTerm ;
  (is_list(Term)-> maplist(subst0011a_p2(P2, X, Y), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       maplist(subst0011a_p2(P2, X, Y), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.



ppa(FF):-
  copy_term(FF,FA,GF),
  numbervars(FA+GF,0,_,[attvar(bind),singletons(true)]),
  sort_safe(GF,GS),write(' '),
  locally(b_setval(arc_can_portray,nil),
      ppawt(FA)),format('~N'),
  ignore((GS\==[], format('\t'),ppawt(attvars=GS),nl)),nl,!.

ppawt(FA):-
  write_term(FA,[numbervars(false), quoted(true),
   character_escapes(true),cycles(true),dotlists(false),no_lists(false),
    blobs(portray),attributes(dots),
    portray(true), partial(false), fullstop(true),
    %portray(false), partial(true), fullstop(true),
   ignore_ops(false), quoted(true), quote_non_ascii(true), brace_terms(false)]).




intersection(APoints,BPoints,Intersected,LeftOverA,LeftOverB):-
  intersection_univ(APoints,BPoints,Intersected,LeftOverA,LeftOverB),!.

same_univ(A,B):- (plain_var(A)->A==B;(B=@=A->true; (fail, \+ (A \=B )))).

intersection_univ(APoints,BPoints,Intersected):-
  intersection_univ(APoints,BPoints,Intersected,_,_),!.
intersection_univ(APoints,BPoints,Intersected,LeftOverA,LeftOverB):-
  pred_intersection(same_univ,APoints,BPoints,Intersected,_,LeftOverA,LeftOverB).

intersection_eq(APoints,BPoints,Intersected):-
  intersection_eq(APoints,BPoints,Intersected,_,_),!.
intersection_eq(APoints,BPoints,Intersected,LeftOverA,LeftOverB):-
  pred_intersection(same_univ,APoints,BPoints,Intersected,_,LeftOverA,LeftOverB).

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
each_obj([],_,_):-!.
each_obj([Obj|List],Obj,Goal):- ignore(Goal), each_obj(List,Obj,Goal).

pred_intersection(_P2, [],LeftOverB, [],[], [],LeftOverB):-!.
pred_intersection(_P2, LeftOverA,[], [],[], LeftOverA,[]):-!.
pred_intersection(P2, [A|APoints],BPoints,[A|IntersectedA],[B|IntersectedB],LeftOverA,LeftOverB):-
  select(B,BPoints,BPointsMinusA),
  \+ \+ p2_call(P2, A,B),!,
  pred_intersection(P2, APoints,BPointsMinusA,IntersectedA,IntersectedB,LeftOverA,LeftOverB).
pred_intersection(P2, [A|APoints],BPoints,IntersectedA,IntersectedB,[A|LeftOverA],LeftOverB):-
  pred_intersection(P2, APoints,BPoints,IntersectedA,IntersectedB,LeftOverA,LeftOverB).



















pp(PP):-pp_m(PP).
pp(Color,PP):- ansi_format([fg(Color)],'~@',[pp(PP)]).


warn_skip(P):- pp(warn_skip(P)).

with_set_stream(_,_,G):- call(G).

fake_impl(M:F/A):- functor(P,F,A), asserta((M:P :- !, fail)).
fake_impl(F/A):- functor(P,F,A), asserta((P :- !, fail)).


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


end_of_file.














































































:- encoding(iso_latin_1).
/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

% Directives for meta-predicate declarations indicating how arguments 
% are handled in higher-order predicates.
:- meta_predicate(print_grid(+,+,+,+)).   % Declares that print_grid/4 expects inputs (+ means instantiated terms)
:- meta_predicate(print_grid(+,+,+)).     % Declares that print_grid/3 expects inputs (+ means instantiated terms)

% The following commented line is an autoload directive that was previously 
% used to load HTML writing predicates. It's now commented, likely due to it being unused in this context.
%:- autoload(library(http/html_write),[html/3,print_html/1]).

/** <predicate> is_debugging/1
    Check if debugging is enabled for a given module or context.
    This predicate is used to determine whether to run debugging logic.
    
    @param M The debugging context or module.
    @example
      ?- is_debugging(my_module).
*/
is_debugging(M):- \+ \+ debugging(M),!.  % Checks if the module M is in debugging mode.
is_debugging(_):- is_testing,!.  % Also allows if the system is in testing mode.
% This line was previously uncommented but has been skipped now. Likely redundant with new debugging logic.
%is_debugging(_):- menu_or_upper('B').

/** <predicate> debug_m/2
    Debugging utility that prints or logs information depending on the context.
    If debugging is enabled for module M, it will print the term Tiny.

    @param M The debugging context or module.
    @param Tiny The term or data to debug.
*/
debug_m(_,Tiny):- display_length(Tiny,Len),Len<30,!,pp(Tiny).  % Only display if the term's string length is less than 30.
debug_m(M,_):- \+ is_debugging(M),!.  % Skip if debugging is not enabled for module M.
% Skipped this version of debug_m because it was previously unused for lists.
%debug_m(_,List):- is_list(List),!,print_ss(List).
debug_m(_,Term):- pp(Term).  % If none of the above conditions apply, print the term.

% The debug_c predicates are similar but allow conditional execution with call/1.

/** <predicate> debug_c/2
    Conditional debugging that executes and prints the result of a goal 
    if debugging is enabled for a module.
    
    @param M The debugging context or module.
    @param C The goal to execute and debug.
*/
debug_c(M,_):- \+ is_debugging(M),!.  % Skip if debugging is not enabled for module M.
debug_c(_,C):- call(C),!.  % Call the goal if debugging is enabled.
debug_c(M,C):- wots_hs(S,C),debug_m(M,S),!.  % Log the result of the goal using the helper predicate wots_hs.

/** <predicate> wno/1
    Wrapper around a goal G to ensure some context settings are applied 
    during its execution. Specifically, it sets the value of the 
    `print_collapsed` flag to 10.

    @param G The goal to be executed.
*/
:- meta_predicate(wno(0)).  % Declares that wno/1 takes a goal (0 means a goal).
wno(G):-
  locally(b_setval(print_collapsed,10), G).  % Locally sets a flag, executes the goal, and then restores the flag.

/** <predicate> print_collapsed/2
    Runs a goal with a specific setting for collapsing print size.

    @param Size The size for print collapsing.
    @param G The goal to be executed.
*/
:- meta_predicate(print_collapsed(0)).  % Declares that print_collapsed/2 takes a goal.
print_collapsed(Size,G):-
  locally(b_setval(print_collapsed,Size), print_collapsed0(Size,G)).  % Sets the collapse size and executes the goal.

/** <predicate> print_collapsed0/2
    Lower-level predicate that actually performs the print operation based 
    on the given size.

    @param Size The size for print collapsing.
    @param G The goal to be executed.
*/
:- meta_predicate(print_collapsed0(0)).  % Declares that print_collapsed0/2 takes a goal.
print_collapsed0(Size,G):- Size<10, !, call(G).  % If the size is less than 10, just call the goal.
% Skipped alternative branch for print_collapsed0, was previously unused.
% print_collapsed(Size,G):-  call(G).
print_collapsed0(Size,G):- Size>=10, !, wots_hs(_S,G).  % If the size is 10 or more, use wots_hs to handle printing.
print_collapsed0(_,G):- wots_vs(S,G),write(S).  % Another form of handling print collapse with wots_vs.

/** <predicate> tersify/2
    Tries to simplify or compress a complex term for easier display. 
    This is helpful for debugging large structures.
    
    @param I The input term.
    @param O The simplified output term.
*/
tersify(I,O):- tracing,!,I=O.  % If tracing is enabled, don't modify the term.
% This version of tersify was previously skipped, possibly to avoid handling attvars unnecessarily.
% tersify(I,O):- term_variables(I,Vs), \+ ( member(V,Vs), attvar(V)),!,I=O.
tersify(I,O):- tersify23(I,O),!.  % Applies more specific tersification logic.
tersify(X,X):-!.  % If no transformation is applied, the term remains unchanged.

/** <predicate> srw_arc/2
    Attempts to simplify and represent certain complex data types, such as grids 
    and VM maps, in a compressed format for easier debugging.

    @param I The input term.
    @param O The output (compressed or simplified) term.
*/
% This version of srw_arc was skipped, as grid handling was deemed unnecessary in this context.
%srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
srw_arc(I,O):- is_vm_map(I),!, O='..vvmm..'.  % If the input is a VM map, simplify it to '..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.  % If the input is a grid, simplify it to '..grid..'.
% Skipped handling for long lists as it was previously commented out.
%srw_arc(List,O):- current_prolog_flag(dmsg_len,Three),
%  is_list(List),length(List,L),L>Three,
%   append([A,B,C],[F|_],List),F \='...'(_), !,
%  simplify_goal_printed([A,B,C,'....'(L>Three)],O).
% Further list handling versions were skipped as well.
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
srw_arc(I,O):- tersify(I,O),!,I\==O,!.

% Multifile and dynamic declarations for hook predicates related to simple rewriting.
:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

/** <predicate> dumpst_hook:simple_rewrite/2
    Hook for simple term rewriting, potentially used for debugging or logging. 
    It is currently disabled (`fail`), but the logic remains for potential future use.
*/
dumpst_hook:simple_rewrite(I,O):- fail, notrace(catch(arc_simple_rewrite(I,O),_,fail)).

% The following is an internal predicate for simple rewriting, skipped because it's 
% part of an experimental or optional feature.
arc_simple_rewrite(I,O):-
  \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(bfly_startup/0),
  current_predicate(is_group/1),
  b_setval(arc_can_portray,nil),
  locally(b_setval(arc_can_portray,nil),once((compound(I), lock_doing(srw_arc,I,srw_arc(I,O))))), I\==O, I\=@=O, !, \+ I=O,
  b_setval(arc_can_portray,t).

% This flag setting was skipped as it permanently disables the `pp_hook`.
%:- set_prolog_flag(never_pp_hook, true).

% portray_terse/0 is used to enforce a more concise display style.
portray_terse:- true,!.

/** <predicate> arc_portray/2
    Custom term portrayal logic for specific term types. Uses 
    `write_keeping_ansi_mb/1` if ANSI codes are detected.

    @param S The term to portray.
    @param TF The output format (not used here).
*/
:- discontiguous arc_portray/2.

arc_portray(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).  % Handle terms that use ANSI formatting.
arc_portray(_,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t), !, fail.  % Skip if `pp_hook` is disabled.
arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs).  % Portray maps by showing their pairs.
/** 
 * arc_portray_t/2
 * This predicate portrays various types of graphical representations
 * based on the type of the input `G`. It handles virtual maps, grids, 
 * and general printing as fallback.
 *
 * @param G The graphical object to be portrayed.
 * @param _ Unused parameter.
 * @example
 * ?- arc_portray_t(Grid, _).
 */
arc_portray_t(G, _) :- 
    % If `G` is a virtual map, portray it using write_map with 'arc_portray_t'.
    is_vm_map(G), 
    !, 
    write_map(G, 'arc_portray_t').

arc_portray_t(G, _) :- 
    % If `G` is a grid, portray it as a grid and display its data type.
    is_grid(G),  
    !, 
    data_type(G, W), 
    writeq(grid(W)).

arc_portray_t(G, _) :- 
    % Fallback case: simply print `G`.
    print(G), 
    !.

/** 
 * arc_portray/2
 * This is the main predicate to portray graphical objects. It tries 
 * to handle virtual maps, enable terse portrayals, and includes error handling.
 *
 * @param G The graphical object to be portrayed.
 * @param TF Terse flag to determine the display mode.
 * @example
 * ?- arc_portray(Map, true).
 */
arc_portray(G, _) :- 
    % If `G` is a virtual map, portray it using write_map with 'arc_portray'.
    is_vm_map(G),  
    !, 
    write_map(G, 'arc_portray').

arc_portray(G, TF) :- 
    % If TF is true, use terse portrayal and call `arc_portray_t`.
    TF == true, 
    portray_terse, 
    arc_portray_t(G, TF), 
    !.

arc_portray(G, TF) :- 
    % Catch any errors in `arc_portray_nt` and ensure that arc portrayal is never attempted again on failure.
    catch(arc_portray_nt(G, TF), E, (writeln(E), never_let_arc_portray_again, fail)), 
    !.

% arc_portray(G, _TF) :- 
%     writeq(G), !.
% 
% The above line was skipped as it would simply print the term in a quoted form, 
% which might not be the desired behavior when portraying more complex graphical objects.

/** 
 * arc_portray_nt/2
 * This helper predicate portrays objects in the debugger.
 * It handles grids, objects, and groups.
 *
 * @param G The graphical object to be portrayed.
 * @param TF Terse flag.
 */
arc_portray_nt(G, false) :- 
    % If `G` is a grid, print the grid.
    is_grid(G), 
    print_grid(G), 
    !.

% arc_portray_nt([G|L],_False) :- 
%     is_object(G), !, pp([G|L]).
% 
% The above line was skipped because it uses a specific object printing function (pp),
% which might not be suitable for all cases and has been replaced with a more generic approach.

% arc_portray_nt(G0, true) :- 
%     is_group(G0), ppt(G0), !.
% arc_portray_nt(G0, false) :- 
%     is_group(G0), ppt(G0), !.
% 
% These lines were commented out because they duplicate the group handling logic below.

arc_portray_nt(G0, Tracing) :- 
    % If `G0` is a group, convert it to a list and write a tersified version.
    is_group(G0), 
    into_list(G0, G), 
    length(G, L),  % Check the length of the group.
    maplist(tersify, G0, GG), 
    write(GG),
    if_t(Tracing == false,
         in_cmt((
             % In the case of non-tracing, print additional information about why it was grouped.
             dash_chars,
             once(((why_grouped(_TestID, Why, WG), WG =@= G, fail); (Why = (size2D = L)))), 
             !,
             print_grid(Why, G), nl_now,
             dash_chars))).

arc_portray_nt(G, _False) :- 
    % If `G` is an object, handle its graphical representation.
    is_object(G), 
    wots(S, writeg(G)),
    global_grid(G, GG), 
    !,
    print_grid(GG),
    write(S), 
    !.

arc_portray_nt(G, false) :- 
    % If `G` can be printed via the grid printing mechanism, do so and check its dimensions.
    via_print_grid(G), 
    !, 
    grid_size(G, H, V), 
    !, 
    H > 0, 
    V > 0, 
    print_grid(H, V, G).

% Other portray cases for object printing in the tracer.
arc_portray_nt(G, true) :- 
    is_object(G), 
    underline_print((ppt(G))).

arc_portray_nt(G, true) :- 
    via_print_grid(G), 
    write_nbsp, 
    underline_print((ppt(G))), 
    write_nbsp.

arc_portray_nt(G, true) :- 
    tersify(G, O), 
    write_nbsp, 
    writeq(O), 
    write_nbsp.

arc_portray_nt(G0, _) :- 
    % If `G0` is not gridoid, simply print it.
    \+ is_gridoid(G0), 
    !, 
    print(G0).

/** 
 * arc_portray_pairs/3
 * Portrays pairs of graphical objects based on their type.
 *
 * @param Type The type of the pairs to be portrayed.
 * @param TF Terse flag.
 * @param Pairs The list of key-value pairs.
 */
arc_portray_pairs(Type, TF, Pairs) :- 
    % Print the length of the pairs for debugging.
    length(Pairs, N),
    writeln(arc_portray_pairs(Type, TF, len(N))),
    % Swap keys and values for sorting and portrayal.
    swap_kv(Pairs, VKPairs),
    keysort(VKPairs, SVKPairs),
    my_maplist(tc_arg(2), SVKPairs, SVKPairs2),
    arc_portray_type_pairs(TF, SVKPairs2).

/** 
 * arc_portray_type_pairs/2
 * Handles portraying pairs of grid types side by side.
 *
 * @param TF Terse flag.
 * @param Pairs The list of key-value pairs.
 */
arc_portray_type_pairs(TF, Pairs) :- 
    % If the first two elements in the pairs are grids, print them side by side.
    append(Left, [K1-V1, K2-V2|Right], Pairs), 
    is_grid(V1), 
    is_grid(V2), 
    !,
    append(Left, [call-print_side_by_side(yellow, V1, K1, _, V2, K2)|Right], PairsM),
    arc_portray_type_pairs(TF, PairsM).

arc_portray_type_pairs(TF, Pairs) :- 
    % Default case: portray each pair.
    forall(member(K-V, Pairs), arc_portray_pair(Pairs, K, V, TF)).

/** 
 * swap_kv/2
 * Swaps keys and values in the pair list.
 *
 * @param Pairs The input list of pairs.
 * @param VKPairs The output list with swapped keys and values.
 */
swap_kv([_-V|Pairs], VKPairs) :- 
    % Skip plain variables.
    plain_var(V), 
    !, 
    swap_kv(Pairs, VKPairs).

swap_kv([K-V|Pairs], ['-'(Type, K-V)|VKPairs]) :- 
    % Swap key and value and determine the type of the value.
    data_type(V, Type),
    swap_kv(Pairs, VKPairs).

swap_kv([], []) :- 
    % Base case: empty list.
    true.

/** 
 * arc_portray_pair/4
 * Portrays a single pair of key-value.
 *
 * @param Ps The list of pairs.
 * @param K The key.
 * @param Val The value.
 * @param TF Terse flag.
 */
arc_portray_pair(Ps, K, Val, TF) :- 
    % Handle printing of a single pair and newline if needed.
    nl_if_needed,
    arc_portray_1_pair(Ps, K, Val, TF),
    nl_if_needed_ansi.

/** 
 * arc_portray_1_pair/4
 * Handles special cases for a single key-value pair.
 *
 * @param Ps The list of pairs.
 * @param K The key.
 * @param Val The value.
 * @param TF Terse flag.
 */
arc_portray_1_pair(_Ps, call, Val, _TF) :- 
    % If the key is 'call', execute the value as a predicate.
    !, 
    call(Val).

arc_portray_1_pair(Ps, K, Val, TF) :- 
    % Default case: print the key and portray or print the value.
    (via_print_grid(Val) -> 
        print_grid(K, Val)
    ;  
        (print(K), write('= '), once(arc_portray(Val, TF); print(Val)))
    ),
    ignore(arc_portray_pair_optional(Ps, K, Val, TF)), 
    !.

/** 
 * arc_portray_pair_optional/4
 * Optionally portrays a pair if it meets certain conditions.
 *
 * @param Ps The list of pairs.
 * @param K The key.
 * @param Val The value.
 * @param TF Terse flag.
 */
arc_portray_pair_optional(Ps, K, Val, TF) :- 
    % Only handle lists of objects here.
    once((Val \== [], is_list(Val), my_maplist(is_object, Val))).
/* Directive explanation: This directive ensures that we handle certain
   grid objects correctly using the tersify/2 predicates */
% Ensuring proper formatting of grids and objects

% PLDoc header for print_info/1
%! print_info(+Val) is det.
% This predicate prints information about the value Val.
print_info(Val),

% This line ensures that Val is not a list with one element.
Val \= [_],

% PLDoc header for compare_objects/2
%! compare_objects(+Val, -Diffs) is det.
% This predicate compares the object Val and generates differences in Diffs.
compare_objects(Val,Diffs),

% PLDoc header for color_print/2
%! color_print(+Color, +Message) is det.
% This predicate prints a message in the specified color.
color_print(cyan,call(arc_portray_pair(Ps,diffs(K),Diffs,TF))))).

/* Old commented-out arc_portray/1 with an old example.
   Previously: used an extra tracing block for portraying */
% arc_portray(G):- \+ \+ catch((wots_hs(S,( tracing->arc_portray(G,true);arc_portray(G,false))),write(S),ttyflush),_,fail).

% PLDoc header for arc_portray/1
%! arc_portray(+G) is semidet.
% This predicate attempts to portray G using various conditions.
% If G is not a compound, we fail immediately.
arc_portray(G):- \+ compound(G),fail.

% If G is a virtual machine (VM), print '..VM..' and succeed.
arc_portray(G):- is_vm(G), !, write('..VM..').

% If arc_portray is not set to 't' or 'f', print collapsed version.
arc_portray(G):- \+ nb_current(arc_portray,t),\+ nb_current(arc_portray,f),is_print_collapsed,!,
  locally(nb_setval(arc_portray,t),arc_portray1(G)).

% Otherwise, proceed with portrayal and mark as 't'.
arc_portray(G):- \+ nb_current(arc_portray,f),!, locally(nb_setval(arc_portray,t),arc_portray1(G)).

% In case arc_portray is marked as 'f', portray with 'f'.
arc_portray(G):- locally(nb_setval(arc_portray,f),arc_portray1(G)).

% PLDoc header for arc_portray1/1
%! arc_portray1(+G) is det.
% Internal helper for portraying G with depth tracking.
arc_portray1(G):-
  % Ensure we do not exceed portrayal depth.
  flag(arc_portray_current_depth,X,X), X < 3,
  \+ \+
    % Execute portrayal and handle exceptions.
    setup_call_cleanup(flag(arc_portray_current_depth,X,X+1),
      catch(((tracing->arc_portray(G,true); arc_portray(G,false)),ttyflush),
        E,(fail,format(user_error,"~N~q~n",[E]),fail)),
      flag(arc_portray_current_depth,_,X)).

/* The following via_print_grid/1 predicates are responsible
   for grid-based objects and check different types of grid-related data. */

% PLDoc header for via_print_grid/1
%! via_print_grid(+G) is semidet.
% This predicate verifies whether G is a grid-based object.
% If we are tracing, we skip and fail the predicate.
%via_print_grid(G):- tracing,!,fail.

% If G is a list of points, proceed with portrayal.
via_print_grid(G):- is_points_list(G).

% If G is a grid, proceed.
via_print_grid(G):- is_grid(G).

% If G has object properties, we fail to avoid processing.
via_print_grid(G):- is_obj_props(G),!,fail.

% If G is an object, continue.
via_print_grid(G):- is_object(G).

% If G is a group, continue.
via_print_grid(G):- is_group(G).

% If G is a gridoid (a grid-like structure), continue.
via_print_grid(G):- is_gridoid(G).

% PLDoc header for terseA/3
%! terseA(+I, +L, -Result) is det.
% This predicate simplifies or "terse-ifies" attribute lists or differences.
terseA(_,[],[]):- !.

% If the list L has more than 10 elements, replace it with an ellipsis and count.
terseA(_,L,'... attrs ...'(N)):- is_list(L),length(L,N),N>10,!.

% Recursively process attributes in the list.
terseA(I,[A|L],[B|LL]):- terseA(I,A,B), terseA(I,L,LL),!.

% Simplify dif/2 attributes if the first argument equals I.
terseA(I,dif(A,B),B):- A==I,!.
terseA(I,dif(B,A),B):- A==I,!.

% Handle special case for put_attr/3 predicates with attribute B being ci.
terseA(_,put_attr(_,B,A),A):- B==ci,!.

% General case for put_attr/3.
terseA(_,put_attr(_,B,A),B=A):-!.

% By default, return the original element.
terseA(_,A,A):-!.

% PLDoc header for simple_enough/1
%! simple_enough(+I) is semidet.
% This predicate checks if a term is simple enough to be displayed as-is.
simple_enough(I):- plain_var(I).
simple_enough(I):- atomic(I).
simple_enough(I):- \+ compound(I),!.

% Skip specific compound structures like multiplication and addition.
simple_enough(_*_):-!.
simple_enough(_+_):-!.

% If A is a functor with arity 1, check its argument.
simple_enough(A):- functor(A,_,1),tc_arg(1,A,E),!,simple_enough(E).

/* Previously: handled numbers and atoms as simple enough directly */
%simple_enough(I):- number(I).
%simple_enough(I):- atom(I).

% PLDoc header for tersify0/2
%! tersify0(+I, -O) is det.
% This predicate attempts to simplify or "tersify" terms.
% If the term is simple, return it as-is.
tersify0(I,O):- simple_enough(I),!,I=O.

% If I is an attribute variable, extract and simplify its attributes.
tersify0(I,av(C,Others)):- attvar(I),copy_term(I,C,Attrs),terseA(C,Attrs,Others),!.

% Otherwise, return I unchanged.
tersify0(I,I):- var(I),!.

/* Skipped certain terification rules for virtual machine maps, 
   as they require specialized handling */
%tersifyC(D):- is_vm_map(D),!.

% PLDoc header for tersifyC/1
%! tersifyC(+Term) is semidet.
% This predicate checks for specific compound types that are left unsimplified.
tersifyC(av(_,_)).
tersifyC(objFn(_,_)).
tersifyC(groupFn(_,_)).
tersifyC(objFn(_)).
tersifyC(groupFn(_)).

% PLDoc header for tersify1/2
%! tersify1(+I, -O) is det.
% A more advanced tersify predicate that handles more complex terms.
% If the input is simple, return it as-is.
tersify1(I,O):- simple_enough(I),!,I=O.

% Handle special case for av/2 structures.
tersify1(av(_,Blue), -(Blue)):-!.

% If the term is compound and matches specific types, return it unchanged.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.

% Special handling for grid-related terms.
tersify1(gridFn(I),gridFn(I)):-!. % previously: applied tersifyG/2 to grids.

% Handling grids by simplifying them into grid names.
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).

% If I is a group, map its elements and optionally summarize the group.
tersify1(I,groupFn(O,List)):- is_group(I), mapgroup(tersify1,I,List), mapgroup(obj_to_oid,I,OIDs), length(List,N),
  !, ignore((get_current_test(TestID),is_why_grouped(TestID,N,Why,OIDs),!,O=Why)).

% Handle object references.
tersify1(I,Q):- is_object(I),object_ref_desc(I,Q),!.

% Special cases for virtual machine maps.
tersify1(I,O):- is_vm_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_vm_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').

/* Tersify grid structures by simplifying and binding variables */
% PLDoc header for tersifyG/2
%! tersifyG(+I, -O) is det.
% This predicate simplifies grids by numbering their variables.
tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.
/* tersifyL/2: This predicate simplifies or shortens lists or terms based on various conditions. 
 * It checks the structure and properties of the input, I, and outputs a simplified version, O.
 * It uses various helper predicates such as `tersify0/2`, `tersify1/2`, and `display_length/2` 
 * to handle specific cases.
 *
 * @param I The input term or list to be simplified.
 * @param O The output term after simplification.
 *
 * @example tersifyL([1,2,3,4,5], O). -> O = [1,2,3,4,5]
 */

/* The first clause checks if the input is not a "cons" (non-list structure) and returns it as-is. */
tersifyL(I, O) :- \+ is_cons(I), !, O = I.

/* The second clause simplifies the head of a list if the tail is not a list, 
   then recursively applies tersify/2 to the head. */
tersifyL([H|I], [HH|I]) :- \+ is_list(I), !, tersify(H, HH).

/* This clause handles cases where the head is non-variable and the tail is not a group. 
   If the list's display length exceeds 170 characters, it truncates the list with '...'. */
tersifyL([H|I], O) :- nonvar(H), \+ is_group(I), display_length(I, N), N > 170,
  length(I, LL), tersify(H, HH), (('...'(HH, LL, '...'(N))) = O), !.

/* Calls a generic simplification helper (tersify0/2). */
tersifyL(I, O) :- tersify0(I, O), !.

/* Recursively simplifies both the head and tail of the list. */
tersifyL([H|TT], [HH|TT]) :- tersify(H, HH), !, tersifyL(TT, TT), !.

/* Uses a different helper (tersify1/2) if other clauses fail. */
tersifyL(I, O) :- tersify1(I, O), !.

/* Default clause that returns the input unchanged if no simplification applies. */
tersifyL(I, I).

/* tersify2/2: Another variant of simplification focusing on compound terms and lists.
 * It attempts to reduce complex terms by recursively applying simplifications to 
 * their components.
 *
 * @param I Input term to be simplified.
 * @param O Output term after simplification.
 */

/* Simplifies equations by recursively simplifying both sides (N=V). */
tersify2(I, O) :- compound(I), (I = (N = V)), tersify2(N, NN), tersify2(V, VV), !, O = (NN = VV).

/* Checks if the term is "simple enough" and doesn't need further simplification. */
tersify2(I, O) :- simple_enough(I), !, I = O.

/* Simplifies compound terms by calling tersify1/2. */
tersify2(I, O) :- compound(I), tersify1(I, O), !.

/* Uses tersify0/2 for other compound terms. */
tersify2(I, O) :- tersify0(I, O), !.

/* Handles lists by recursively applying tersify2/2 to their elements. */
tersify2(I, O) :- is_list(I), !, my_maplist(tersify2, I, O).

/* Simplifies general compound terms by simplifying their arguments. */
tersify2(I, O) :- compound(I), !, compound_name_arguments(I, F, IA), my_maplist(tersify, IA, OA), compound_name_arguments(O, F, OA).

/* Default clause that returns the input unchanged if no simplification applies. */
tersify2(I, I).

/* tersify3/2: Yet another variant of the tersification logic, adding checks for long lists and
 * other compound terms. 
 *
 * @param I Input term to be simplified.
 * @param O Output term after simplification.
 */

/* Handles equations in compound terms and simplifies both sides. */
tersify3(I, O) :- compound(I), (I = (N = V)), tersify3(N, NN), tersify3(V, VV), !, O = (NN = VV).

/* Returns the term unchanged if it's simple enough. */
tersify3(I, O) :- simple_enough(I), !, I = O.

/* Uses tersify1/2 for simplifying compound terms. */
tersify3(I, O) :- compound(I), tersify1(I, O), !.

/* Uses tersify0/2 for compound terms that don't match the above cases. */
tersify3(I, O) :- tersify0(I, O), !.

/* Truncates lists that are too long, keeping the first element and adding '...'. */
tersify3([H|I], O) :- is_list(I), ((display_length(I, N), N > 170) -> 
  (length(I, LL), tersify(H, HH), (('...'(HH, LL, '...'(N))) = O)); I = O), !.

/* Recursively simplifies lists by applying tersify3/2 to each element. */
tersify3(I, O) :- is_list(I), !, my_maplist(tersify3, I, O).

/* Simplifies general compound terms by applying tersify to their arguments. */
tersify3(I, O) :- compound(I), !, compound_name_arguments(I, F, IA), my_maplist(tersify, IA, OA), compound_name_arguments(O, F, OA).

/* Default clause that returns the input unchanged. */
tersify3(I, I).

/* write_map/2: Outputs a mapping based on the input type, such as VM, dictionary, etc.
 * It checks for specific types like virtual machine (VM), dictionaries, and maps.
 *
 * @param G The goal or term being mapped.
 * @param Where A string or term that indicates where the output is being written.
 *
 * @example write_map(my_dict, output). -> ...Dict_output...
 */

/* Handles the case where G is a virtual machine term. */
write_map(G, Where) :- is_vm(G), !, write('...VM_'), write(Where), write('...').

/* Handles the case where G is a VM map. */
write_map(G, Where) :- is_vm_map(G), !, write('...Map_'), write(Where), write('...').

/* Handles the case where G is a dictionary. */
write_map(G, Where) :- is_dict(G), !, write('...Dict_'), write(Where), write('...').

/* Default case where G is not a special type; just write the output. */
write_map(_G, Where) :- write('...'), write(Where), write('...').

/* non_empty_wqs_c/1: Checks if a value is non-empty, considering different empty representations.
 * Empty strings, whitespace, or special HTML characters are considered "empty."
 *
 * @param V The value to check.
 *
 * @example non_empty_wqs_c(" "). -> false
 */

/* Checks if the value is not empty by negating the empty_wqs_c/1 predicate. */
non_empty_wqs_c(V) :- \+ empty_wqs_c(V).

/* empty_wqs_c/1: Determines if a value is "empty."
 * It considers variables, empty strings, HTML space characters, and newline characters as empty.
 *
 * @param V The value to check.
 */

/* Fails if the input is a variable, as it cannot determine if it's empty. */
empty_wqs_c(V) :- var(V), !, fail.

/* Converts atoms to strings for further checks. */
empty_wqs_c(A) :- atom(A), atom_string(A, S), !, empty_wqs_c(S).

/* Empty list is considered empty. */
empty_wqs_c([]).

/* Empty string is considered empty. */
empty_wqs_c("").

/* HTML non-breaking space is considered empty. */
empty_wqs_c("&nbsp;").

/* A single space character is empty. */
empty_wqs_c(" ").

/* A newline character is considered empty. */
empty_wqs_c("\n").

%is_writer_goal((C1,C2)):- !, (is_writer_goal(C1);is_writer_goal(C2)).
/* previously: This clause handled writer goals in compound terms,
   but was skipped, likely due to complexity or inefficiency in its current form. */

/* is_writer_goal/1: Checks if a goal involves writing output.
 * This predicate looks for specific goal structures that indicate writing,
 * such as format/2, nl/0, or specific atoms related to printing.
 *
 * @param H The goal to check.
 */

/* Fails if the input is not callable. */
is_writer_goal(H) :- \+ callable(H), !, fail.

/* Fails if the input is a list. */
is_writer_goal(H) :- is_list(H), !, fail.

/* If the input is an atom, it checks if the atom indicates a writer goal. */
is_writer_goal(A) :- atom(A), !, is_writer_goal_f(A).

/* Fails if the input is neither a compound term nor callable. */
is_writer_goal(H) :- \+ compound(H), !, fail.

/* The commented clause was previously designed to check for writer goals in compound terms,
   but it was disabled, possibly for optimization purposes. */

/* is_writer_goal_f/1: A helper predicate to check if an atom is a writer goal.
 * It handles specific cases like 'wqs_c', 'format', 'nl', and others.
 *
 * @param F The atom to check.
 */

/* Checks if the atom is 'wqs_c', indicating a writer goal. */
is_writer_goal_f(wqs_c).

/* Attempts to match the atom against known writer goal patterns. */
is_writer_goal_f(F) :- is_writer_goal_l(F), !.

/* Converts a non-atom term to an atom for checking if it's a writer goal. */
is_writer_goal_f(F) :- \+ atom(F), !, term_to_atom(F, A), is_writer_goal_f(A).

/* Filters out known non-writer goal patterns using the atom_concat/3 method. */
is_writer_goal_f(F) :- not_writer_goal_r(R), atom_concat(_, R, F), !, fail.

/* Matches atoms that start or end with known writer goal components. */
is_writer_goal_f(F) :- is_writer_goal_l(L), atom_concat(L, _, F), !.
is_writer_goal_f(F) :- is_writer_goal_l(R), atom_concat(_, R, F), !.

/* Helper predicates to define specific patterns of writer goals. */
not_writer_goal_r(test). 
is_writer_goal_l(msg). 
is_writer_goal_l(call).
is_writer_goal_l(nl).  
is_writer_goal_l(format). 
is_writer_goal_l(with_).
is_writer_goal_l(locally).
is_writer_goal_l(html).  
is_writer_goal_l(ptcol).  
is_writer_goal_l(wots).
is_writer_goal_l(print). 
is_writer_goal_l(flush_output).  
is_writer_goal_l(wqs).
/* File Directive: Ensures this file is treated as a module, if applicable, 
   though no explicit module declaration appears here */

/* Predicate: is_writer_goal_l/1
   Checks if a given argument matches one of the specified writer goals */
%% is_writer_goal_l(+Goal) is semidet.
%  True if the given Goal is a recognized writer goal such as pp, write, or dash.
is_writer_goal_l(pp). 
is_writer_goal_l(write).  
is_writer_goal_l(dash_).

/* Predicate: maybe_color/2
   Decides whether to apply color or simply write the term, based on the presence of ANSI codes */
%% maybe_color(+Term, +Parameter) is det.
%  Writes Term with or without color formatting depending on whether the term or parameter contains ANSI codes.
%  @example maybe_color('Hello', param) -> writes 'Hello' with appropriate formatting.
maybe_color(SS,_):- 
    term_contains_ansi(SS),   % Check if the term contains ANSI codes
    !, 
    write_nbsp,               % Write a non-breaking space
    write(SS).                % Write the term

maybe_color(SS,P):- 
    term_contains_ansi(P),    % Check if the parameter contains ANSI codes
    !, 
    write_nbsp,               % Write a non-breaking space
    write(SS).                % Write the term

maybe_color(SS,P):- 
    pp_msg_color(P,C),        % Get the color associated with the parameter
    ansicall(C, is_maybe_bold(P, write(SS))), % Apply the color and possibly bold formatting
    !.

/* Predicate: write_atom/1
   Handles how atoms and non-atoms are written, with special formatting for some cases */
%% write_atom(+Atom) is det.
%  Writes the Atom, handling cases where it is not an actual atom or has specific symbols like '~'.
%  @example write_atom('Hello~World') -> formats 'Hello~World' using format/2.
write_atom(S):- 
    \+ atom(S),  % Check if the input is not an atom
    !, 
    wqs(S).      % Call another predicate to handle non-atoms

write_atom(S):- 
    atom_contains(S,'~'),  % Special handling for atoms containing '~'
    !, 
    notrace(catch(format(S,[]),_,maybe_write_atom_link(S))).

write_atom(S):- 
    maybe_write_atom_link(S),  % Try to write the atom as a link if applicable
    !.

write_atom(S):- 
    into_title_str(S,TS),  % Convert the atom into a title string and write it
    write(TS), 
    !.

/* Meta-predicate declaration: into_title_str/2 is a meta-predicate, meaning it operates on other predicates. */
:- meta_predicate(into_title_str(+,-)).

/* Predicate: into_title_str/2
   Converts various Prolog terms into a formatted string representation */
%% into_title_str(+Term, -Str) is det.
%  Converts the Term into a title-friendly string.
%  @example into_title_str('HelloWorld', Str) -> Str = 'Hello World'.
into_title_str(Term,Str):- 
    string(Term),  % If the term is already a string, return it as-is
    !, 
    Str = Term.

into_title_str(Term,Str):- 
    plain_var(Term),  % If it's a plain variable, format it using ~p
    sformat(Str,'~p',[Term]), 
    !.

into_title_str(Term,Str):- 
    var(Term),  % If it's a variable, tersify it and format
    tersify0(Term,Terse), 
    sformat(Str,'~p',[Terse]), 
    !.

into_title_str(Term,Str):- 
    term_is_ansi(Term),  % Handle ANSI-formatted terms
    wots(Str,write_keeping_ansi_mb(Term)), 
    !.

into_title_str(Term,Str):- 
    (is_codelist(Term);is_charlist(Term)),  % Handle code and character lists
    catch(sformat(Str,'~s',[Term]),_,sformat(Str,'~p',[Term])), 
    !.

into_title_str(Term,Str):- 
    is_list(Term),  % Recursively handle lists by converting each element
    my_maplist(into_title_str,Term,O3), 
    atomics_to_string(O3," ",Str), 
    !.

into_title_str([H|T],Str):- 
    into_title_str(H,A),  % Recursively convert the head and tail of a list
    into_title_str(T,B), 
    atomics_to_string([A,B]," ",Str), 
    !.

into_title_str(Term,Str):- 
    \+ callable(Term),  % For non-callable terms, format them using ~p
    sformat(Str,'~p',[Term]), 
    !.

into_title_str(format(Fmt,Args),Str):- 
    sformat(Str,Fmt,Args),  % If the term is a format/2 structure, format it
    !.

into_title_str(Term,""):- 
    empty_wqs_c(Term),  % If the term is empty, return an empty string
    !.

into_title_str(out,"Output").  % Special case for the term 'out'

into_title_str(in,"Input").    % Special case for the term 'in'

into_title_str(i,"IN").        % Special case for the term 'i'

into_title_str(o,"OUT").       % Special case for the term 'o'

/* previously: The following commented out code was used for a different, less efficient string conversion process */
%into_title_str(Term,Str):- tersify23(Term,Terse),Term\=@=Terse,!,into_title_str(Terse,Str).

into_title_str(Term,Str):- 
    callable_arity(Term,0),  % If the term has arity 0 and is a writer goal, write it
    is_writer_goal(Term),
    catch(notrace(wots(Str,call_e_dmsg(Term))),_,fail), 
    !.

into_title_str(Term,Str):- 
    catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)),  % Default case for formatting terms
    nonvar(Str), 
    atom_length(Str,E50), 
    E50 < 180, 
    !.
/**
 * into_title_str/2
 * Converts a compound term into a string.
 * This predicate handles compound terms by breaking them into their name and arguments,
 * processing each argument, and then reassembling them into a string.
 * @param Term The term to convert.
 * @param Str The resulting string.
 * @example
 * ?- into_title_str(foo(bar, baz), Str).
 * Str = "foo(bar, baz)".
 */
into_title_str(Term, Str) :- 
    compound(Term),                      % Check if the term is a compound.
    compound_name_arguments(Term, Name, Args),  % Decompose the term into its name and arguments.
    /* previously: include(not_p1(plain_var),Args,Nonvars) - Skipped because 'include' was filtering plain variables */
    Args = Nonvars,                       % For now, just assign Args directly to Nonvars.
    my_maplist(tersify, Nonvars, ArgsT),  % Apply tersify to each non-variable argument.
    into_title_str([Name, "(", ArgsT, ")"], Str), % Reconstruct the string from the name and arguments.
    !.                                    % Cut to prevent backtracking.

% If the term is not a compound, attempt to format it into a string.
into_title_str(Term, Str) :- 
    catch(sformat(Str, '~p', [Term]), _, term_string(Term, Str)).

%---------------------------------------------------------------------------

/**
 * has_short_id/3
 * Associates an entity with its corresponding UUID or ID.
 * Handles multiple cases for identifying different types of entities like test IDs, objects, and grids.
 * @param Entity The entity being checked.
 * @param Type The type of the entity (testid, object, grid).
 * @param UUID The corresponding UUID or ID for the entity.
 * @example
 * ?- has_short_id(test1, testid, UUID).
 * UUID = 'some-uuid'.
 */
has_short_id(TestID, testid, UUID) :- 
    is_valid_testname(TestID),           % Check if the TestID is a valid test name.
    test_id_atom(TestID, UUID).          % Convert the TestID to its UUID.

has_short_id(Obj, object, OID) :- 
    is_object(Obj),                      % Check if Obj is a valid object.
    obj_to_oid(Obj, OID).                % Convert Obj to its object ID (OID).

has_short_id(Grid, grid, GID) :- 
    is_grid(Grid),                       % Check if Grid is a valid grid.
    grid_to_gid(Grid, GID).              % Convert Grid to its grid ID (GID).

%---------------------------------------------------------------------------

/**
 * is_valid_linkid/3
 * Validates an ID by checking if it belongs to a known entity type.
 * Handles validation for multiple entity types like test IDs, objects, and grids.
 * @param ID The ID being validated.
 * @param Type The type of the entity (testid, object, grid, group).
 * @param Entity The entity corresponding to the ID.
 * @example
 * ?- is_valid_linkid('test-id', testid, TestID).
 * TestID = 'test-id'.
 */
is_valid_linkid(ID, testid, TestID) :- 
    atom_id(ID, TestID),                 % Convert the atom ID to TestID.
    is_valid_testname(TestID),           % Ensure the TestID is valid.
    !.                                   % Cut to prevent backtracking.

is_valid_linkid(ID, object, Obj) :- 
    known_object(ID, Obj),               % Ensure the ID corresponds to a known object.
    !.

is_valid_linkid(ID, grid, Grid) :- 
    known_grid(ID, Grid),                % Ensure the ID corresponds to a known grid.
    !.

% This clause seems to handle "group" type IDs, related to tests. 
% It is specialized and depends on the current test context.
is_valid_linkid(ID, group, Grp) :- 
    get_current_test(TestID),            % Get the current test ID.
    is_why_grouped_g(TestID, _Count, ID, Grp).  % Check why this ID is grouped.

%---------------------------------------------------------------------------

/**
 * wqs_c/1
 * A versatile printing predicate with special handling for different term types.
 * Handles various types including strings, variables, lists, and custom compound terms.
 * @param S The term to be printed.
 */
wqs_c(S) :- 
    term_is_ansi(S),                     % Check if the term uses ANSI formatting.
    !, write_keeping_ansi_mb(S).         % Write with ANSI formatting.

wqs_c(S) :- 
    (string(S); is_codelist(S); is_charlist(S)),  % Check if S is a string, code list, or character list.
    catch(format('~s', [S]), _, writeq(S)).      % Print the string or quoted term.

wqs_c(S) :- 
    empty_wqs_c(S),                      % Handle an empty term.
    !.

wqs_c(S) :- 
    var(S),                              % Check if S is a variable.
    !, write(var(S)).                    % Print the variable representation.

wqs_c(S) :- 
    atom(S),                             % Check if S is an atom.
    into_title_str(S, TS),               % Convert the atom to a title string.
    write(TS),                           % Print the title string.
    !.

% Previously: wqs_c(S):- atom(S),write(S),!.
% Skipped because `into_title_str/2` already handles printing atoms.

wqs_c(S) :- 
    \+ compound(S),                      % If S is not a compound, print it as a term.
    !, notrace(catch(format('~p', [S]), _, write(S))).

wqs_c(title(S)) :- 
    !, wqs_c(S).                         % If S is marked as a title, print it recursively.

wqs_c(H+T) :- 
    !, wqs_c(H),                         % Print the head of the list.
    write_nbsp,                          % Write a non-breaking space.
    wqs_c(T).                            % Print the tail recursively.

wqs_c(S) :- 
    is_grid(S),                          % If S is a grid, print the grid.
    print_grid(S),
    !.

wqs_c(S) :- 
    is_vm(S),                            % If S is a virtual machine, print it.
    pp(S),
    !.

wqs_c(L) :- 
    is_list(L),                          % If S is a list, filter out empty elements.
    include(non_empty_wqs_c, L, LL),     % Keep only non-empty elements.
    !, wqs_c_l(LL).                      % Print the list recursively.

wqs_c([H|T]) :- 
    pp([H|T]),                           % Print the head and tail of the list.
    !.

wqs_c(H) :- 
    callable_arity(H, 0),                % Check if H is a goal with 0 arity.
    is_writer_goal(H),                   % Check if H is a writer goal.
    catch(call_e_dmsg(H), _, fail),      % Try calling the goal and printing its message.
    !.

% Previously: wqs_c(H):- callable_arity(H,0),call(H),!.
% Skipped because specialized writer goals handle this case better.

wqs_c(H) :- 
    locally(t_l:wqs_fb(pp_no_nl), wqs(H)), % Locally apply the wqs_fb flag and call wqs/1.
    !.

%---------------------------------------------------------------------------

% Helper predicates for list printing (used by wqs_c/1)

wqs_c_l([]) :- !.                        % Base case for an empty list.

wqs_c_l([H]) :- 
    wqs_c(H),                            % Print the single element in the list.
    !.

wqs_c_l([H|T]) :- 
    wqs_c(H),                            % Print the head of the list.
    write_nbsp,                          % Write a non-breaking space.
    wqs_c_l(T),                          % Print the rest of the list.
    !.

%---------------------------------------------------------------------------

/**
 * ppt/1
 * Pretty prints a term.
 * It handles various conditions like VM maps, ANSI formatting, and custom HTML printing.
 * @param G The term to be printed.
 */
ppt(_):- is_print_collapsed,!.          % If print is collapsed, do nothing.

ppt(G) :- 
    stack_check_or_call(4000, writeq(G)), % Perform a stack check or print the term.
    !.

ppt(G) :- 
    is_vm_map(G),                       % If G is a VM map, print it using 'write_map'.
    !, write_map(G, 'ppt').

ppt(S) :- 
    term_is_ansi(S),                    % Handle ANSI-formatted terms.
    !, write_keeping_ansi_mb(S).

% Previously: ppt(P):- compound(P),wqs1(P),!.
% Skipped because it's redundant with other compound printing logic.

ppt(P) :- 
    \+ ansi_main, wants_html,           % If HTML is desired and ANSI is off.
    !, ptcol_html(P), write_br.

ppt(P) :- 
    \+ \+ ((tersify(P, Q), !, pp(Q))),  % Try to tersify and pretty print.
    !.

ppt(Color, P) :- 
    \+ ansi_main, wants_html,           % If HTML is desired with colors.
    !, with_color_span(Color, ptcol_html(P)), write_br.

ppt(Color, P) :- 
    \+ \+ ((tersify(P, Q), !, pp(Color, Q))), % Try to tersify and print with color.
    !.

%---------------------------------------------------------------------------

% Other utility predicates for printing

write_br :- 
    ansi_main, 
    !, nl.                              % Write a new line if ANSI is enabled.

write_br :- 
    write('<br>').                      % Write an HTML line break.

ptc(Color, Call) :- 
    pp(Color, call(Call)).              % Print a call with color.

% Meta-predicates for pretty printing

:- meta_predicate(ppnl(+)).
ppnl(Term) :- 
    is_list(Term), 
    !, g_out(wqs(Term)).                % Print a list using wqs/1.

ppnl(Term) :- 
    nl_if_needed, format('~q', [Term]), % Print the term with formatting.
    nl_if_needed_ansi.

:- meta_predicate(pp(+)).
pp(Color, P) :- 
    \+ ansi_main, wants_html,           % Handle pretty printing with color and HTML.
    !, with_color_span(Color, pp(P)), write_br.

pp(Color, P) :- 
    ignore((quietlyd((wots_hs(S, pp(P)), !, color_print(Color, S))))). % Print quietly with color.

pp(_):- 
    is_print_collapsed, !.              % If print is collapsed, skip.

pp(_Term):- 
    nl_if_needed, fail.                 % Fail and print a new line if necessary.

pp(Term) :- 
    \+ ansi_main, wants_html,           % Handle pretty printing with HTML.
    !, wots_vs(SS, ptcol_html_scrollable(Term)), write(SS), write_br.
/* 
  pp/1
  Prints a term, but ensures some setup with nb_setval to maintain state.
  Uses nb_current to check if arc_can_portray exists before printing.

  @param Term The term to print
  @example pp(my_term).
*/
pp(Term) :-
    % Check if the nb_current 'arc_can_portray' flag is unset, if so print the term with specific settings
    \+ nb_current(arc_can_portray, _), 
    !, 
    locally(nb_setval(arc_can_portray, t), print(Term)).

pp(Term) :- 
    % If the previous condition fails, use az_ansi to print the term in ANSI format and handle newline
    az_ansi(pp_no_nl(Term)), 
    !, 
    nl_if_needed_ansi.

/*
  Previously: ptcol(P) was a general printing routine, but this code was commented out
  It seems that ptcol_html is now preferred, and the original code is skipped. 
  It's left in case ptcol is used in a future revision or the application context changes.
  This dead code section may be skipped because ptcol_html provides better formatting for HTML output.
*/
/*
ptcol(P) :- 
    wants_html, !, 
    ptcol_html(P).

ptcol(call(P)) :- 
    callable(P), !, 
    call(P).

ptcol(P) :- 
    pp(P).
*/

% Main entry point to handle HTML output using ptcol_html_scrollable
ptcol_html(P) :- 
    ptcol_html_scrollable_0(P).

% Handle HTML scrolling output with a div tag and the scrollable attribute
ptcol_html_scrollable(P) :- 
    with_tag_ats(div, scrollable, ptcol_html_scrollable_0(P)).

% Basic HTML output handler for pretty printing within a preformatted block
ptcol_html_0(P) :- 
    with_tag(pre, ptcol_html_wo_pre(P)).

% Calls P if it's callable; otherwise, pretty prints without newlines
ptcol_html_wo_pre(call(P)) :- 
    callable(P), !, 
    in_pp_html(call(P)).

ptcol_html_wo_pre(P) :- 
    in_pp_html(print_tree_no_nl(P)).

% Scrollable HTML pretty print routine that wraps around the non-scrollable version
ptcol_html_scrollable_0(P) :- 
    ptcol_html_wo_pre(P).

/* 
  pp_wcg/1 
  Wrapper for pretty printing with an option for HTML output or a safe method to print terms.
  @param G The term or goal to print
  @example pp_wcg(my_term).
*/
pp_wcg(G) :- 
    % Check if HTML is preferred, if so, use the scrollable HTML version
    wants_html, !, 
    ptcol_html_scrollable(G).

pp_wcg(G) :- 
    % Otherwise, use a safe method to print the term, with special flags
    pp_safe(call((locally(nb_setval(arc_can_portray, t), print(G))))), !.

/*
  wqln and wqnl are simple wrappers for pretty printing with or without newlines.
  The naming convention here is somewhat arbitrary, but wqln seems to ensure newlines after printing.
*/

/* 
  wqln/1 
  Wrapper for ppnl (pretty print with newline).
  @param Term The term to print
*/
wqln(Term) :- 
    ppnl(Term).

/* 
  wqnl/1 
  Similar to wqln, but may prefer safer printing.
  @param G The term to print
*/
wqnl(G) :- 
    pp_safe(call((locally(nb_setval(arc_can_portray, nil), print(G))))), !.

/* 
  pp_safe/1 
  Ensures safe printing by checking if printing should be hidden.
  If not hidden, it will safely call or print the term.
  @param W The term to print
*/
pp_safe(_) :- 
    % Check if pp_hide is set, if so, do nothing (skip printing)
    nb_current(pp_hide, t), !.

pp_safe(call(W)) :- 
    % If W is a callable term, call it and ensure newlines around the output
    !, 
    nl_if_needed, nl_now, 
    call(W), 
    nl_now.

pp_safe(W) :- 
    % Otherwise, write the term in a quoted format, ensuring newlines around the output
    nl_if_needed, nl_now, 
    writeq(W), 
    nl_now.

/* 
  pp_safe/2 
  A version of pp_safe that allows colored printing.
  @param C The color to use
  @param W The term to print
*/
pp_safe(C, W) :- 
    color_print(C, call(pp_safe(W))).

/*
  p_p_t_no_nl/1 
  Handles printing based on whether the system wants HTML or ANSI printing.
  @param Term The term to print
*/
p_p_t_no_nl(P) :- 
    % Check if ANSI printing is disabled and HTML is wanted, use HTML print
    \+ ansi_main, wants_html, !, 
    ptcol_html(P).

p_p_t_no_nl(Term) :- 
    % Otherwise, use ANSI printing without newlines
    az_ansi(print_tree_no_nl(Term)).

/* 
  ppt_no_nl/1 
  Another variation of pretty printing without newlines, with checks for HTML and ANSI.
  @param P The term to print
*/
ppt_no_nl(P) :- 
    % Use HTML printing if applicable
    \+ ansi_main, wants_html, !, 
    ptcol_html(P).

ppt_no_nl(P) :- 
    % Otherwise, try to tersify the term and then print it without newlines
    tersify(P, Q), !, 
    pp_no_nl(Q).

/* 
  is_toplevel_printing/1 
  Determines if the term is being printed at the top level (for pretty-printing optimizations).
  @param _ Any term (ignored)
*/
is_toplevel_printing(_) :- 
    % Check if the output is a string or the cursor is near the start of the line
    \+ is_string_output, 
    line_position(current_output, N),  
    N < 2, 
    fail.

/* 
  pp_no_nl/1 
  Pretty print a term without newlines, handling variables, ANSI terms, and special cases.
  @param P The term to print
*/
pp_no_nl(P) :- 
    % If P is a variable, print it as a variable term and perform optional debug actions
    var(P), !, 
    pp(var_pt(P)), 
    nop((dumpST, ibreak)).

pp_no_nl(S) :- 
    % If S is an ANSI term, print it with ANSI support
    term_is_ansi(S), !, 
    write_keeping_ansi_mb(S).

pp_no_nl(P) :- 
    % If P is an atom containing '~', use format to handle special formatting
    atom(P), atom_contains(P, '~'), !, 
    format(P).

pp_no_nl(G) :- 
    % If G is a VM map, write it using write_map
    is_vm_map(G), !, 
    write_map(G, 'pp').

pp_no_nl(P) :- 
    % Otherwise, use a general pretty-printing mechanism with guessed formatting
    \+ \+ ((pt_guess_pretty(P, GP), ptw(GP))).

/* 
  ptw/1 
  Main pretty print wrapper that falls back to various printing techniques depending on the type of term.
  @param P The term to print
*/
ptw(P) :- 
    % If P is a variable, print it with specific settings
    var(P), !, 
    ptw(var_ptw(P)), 
    nop((dumpST, ibreak)).

ptw(G) :- 
    % If G is a VM map, write it with specific settings
    is_vm_map(G), !, 
    write_map(G, 'ptw').

ptw(S) :- 
    % If S is an ANSI term, write it with ANSI support
    term_is_ansi(S), !, 
    write_keeping_ansi_mb(S).

ptw(P) :- 
    % Otherwise, use the no-newline version of pretty printing
    p_p_t_no_nl(P), !.

/* 
  pt_guess_pretty/2 
  Attempts to pretty-print a term by guessing the format.
  @param P The original term
  @param O The guessed pretty-printed version
*/
pt_guess_pretty(P, O) :- 
    \+ nb_current(in_pt_guess_pretty, t), 
    locally(nb_setval(in_pt_guess_pretty, t), pt_guess_pretty_1(P, O)).

pt_guess_pretty(O, O).

/* 
  upcase_atom_var_l/2 
  Converts a list of atoms to uppercase, with special handling for lists of atoms.
  @param IntL Input list
  @param NameL Resulting list with uppercase atoms
*/
upcase_atom_var_l(IntL, NameL) :- 
    upcase_atom_var(IntL, NameL).

upcase_atom_var_l(IntL, NameL) :- 
    % If the input is a list, recursively apply upcase_atom_var_l
    is_list(IntL), !, 
    my_maplist(upcase_atom_var_l, IntL, NameL).

/* 
  pt_guess_pretty_1/2 
  A helper predicate for pt_guess_pretty that attempts to transform a term for pretty printing.
  @param P Input term
  @param O Transformed term
*/
pt_guess_pretty_1(P, O) :- 
    copy_term(P, O, _), 
    ignore((sub_term(Body, O), compound(Body), Body = was_once(InSet, InVars), upcase_atom_var_l(InSet, InVars))).
 ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).


:- dynamic(pretty_clauses:pp_hook/3). % Declare pp_hook/3 as a dynamic predicate, allowing runtime modifications
:- multifile(pretty_clauses:pp_hook/3). % Declare pp_hook/3 as a multifile predicate, allowing it to be defined across multiple files
:- module_transparent(pretty_clauses:pp_hook/3). % Ensure that pp_hook/3 is transparent, maintaining module encapsulation

pretty_clauses:pp_hook(FS,Tab,S):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),  notrace(catch(arc_pp_hook(FS,Tab,S),_,fail)).


/**
 * arc_pp_hook(+FS, +Tab, +S) is semidet.
 *
 * Pretty printing hook for terms. This predicate handles various types of terms,
 * including ANSI-formatted and grouped terms.
 *
 * @param FS The term being processed.
 * @param Tab The current indentation level.
 * @param S The stream or term to print.
 */
arc_pp_hook(_, Tab, S) :- term_is_ansi(S), !, prefix_spaces(Tab), write_keeping_ansi_mb(S).  % Handle ANSI-formatted terms

/* Previously: arc_pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
   This code was skipped as it seems to be a specific condition related to a VM term, but is no longer relevant. */

/* Previously: arc_pp_hook(_,  _,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
   Skipped because it explicitly checks for conditions that are handled more efficiently in other parts of the code. */

arc_pp_hook(FS, _, G) :-
    \+ current_prolog_flag(never_pp_hook, true),  % Check if the pp_hook is enabled
    nb_current(arc_can_portray, t),  % Ensure arc_can_portray is active
    current_predicate(is_group/1),  % Check if is_group/1 predicate is defined
    locally(b_setval(pp_parent, FS), print_with_pad(pp_hook_g(G))), !.  % Locally set pp_parent and print the group

/** pp_parent(-PP) is semidet.
 *
 * Retrieves the current parent of a pretty printing process.
 *
 * @param PP The parent term, or an empty list if none is set.
 */
pp_parent(PP) :- nb_current(pp_parent, PP), !.  % Retrieve the current parent
pp_parent([]) :- !.  % Default to an empty list if no parent is set

/* Previously: :- meta_predicate(lock_doing(+,+,0)).
   Modified to support a module-based predicate specifier. */

/**
 * lock_doing(+Lock, +G, :Goal) is det.
 *
 * Prevents recursive execution of the same goal within a locked section.
 * If the goal has already been executed, it won't be re-executed in the same context.
 *
 * @param Lock The lock variable, used to track goals.
 * @param G The goal to lock.
 * @param Goal The actual goal to execute.
 */
:- meta_predicate(lock_doing(+,+,:)).  % Declare lock_doing as a meta-predicate

lock_doing(Lock, G, Goal) :-
    (nb_current(Lock, Was); Was = []), !,  % Check if the lock already has a value
    \+ ((member(E, Was), E == G)),  % Ensure G hasn't been executed before
    locally(nb_setval(Lock, [G | Was]), Goal).  % Execute the goal in a locked section

/** never_let_arc_portray_again is det.
 *
 * Sets a Prolog flag to prevent further portrayal by the arc system.
 */
never_let_arc_portray_again :- set_prolog_flag(never_pp_hook, true), !.  % Disable the pp_hook globally

/** arc_can_portray is semidet.
 *
 * Checks if the arc portrayal system is allowed to portray terms.
 */
arc_can_portray :- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray, t).  % Check if portrayal is allowed

/** arcp:will_arc_portray is semidet.
 *
 * Determines whether the arc system should portray a term.
 * Conditions include ensuring debug mode is off and tracing is disabled.
 */
arcp:will_arc_portray :-
    \+ current_prolog_flag(never_pp_hook, true),
    \+ nb_current(arc_can_portray, f),
    current_prolog_flag(debug, false),  % Ensure debugging is off
    \+ tracing,  % Ensure tracing is off
    flag(arc_portray_current_depth, X, X), X < 3,  % Ensure portrayal depth is within limits
    current_predicate(bfly_startup/0).  % Check if the predicate bfly_startup/0 exists

/** user:portray(+Grid) is semidet.
 *
 * Custom portrayal for grid structures.
 *
 * @param Grid The grid to portray.
 */
user:portray(Grid) :-
    arcp:will_arc_portray,  % Check if portrayal is allowed
    \+ \+ catch(quietly(arc_portray(Grid)), _, fail), !,  % Attempt to portray the grid quietly, catching any errors
    flush_output.

/** pp_hook_g(+S) is semidet.
 *
 * Pretty print hook for a given term. It supports various term types and
 * ensures that ANSI formatting and other conditions are handled appropriately.
 *
 * @param S The term to be pretty printed.
 */
pp_hook_g(S) :- term_is_ansi(S), !, write_keeping_ansi_mb(S).  % Handle ANSI-formatted terms
pp_hook_g(_) :- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray, t), !, fail.  % Fail if portrayal is disabled
pp_hook_g(S) :- term_contains_ansi(S), !, write_nbsp, pp_hook_g0(S).  % Handle terms containing ANSI sequences
pp_hook_g(G) :- \+ plain_var(G), lock_doing(in_pp_hook_g, G, pp_hook_g0(G)).  % Lock and execute pp_hook_g0

/** pp_hook_g0(+S) is semidet.
 *
 * Secondary pretty print hook for ANSI-formatted terms.
 *
 * @param S The term to print.
 */
pp_hook_g0(S) :- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).  % Handle ANSI-formatted terms
pp_hook_g0(_) :- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray, t), !, fail.  % Fail if portrayal is disabled
pp_hook_g0(_) :- in_pp(bfly), !, fail.  % Skip if in the context of 'bfly'
pp_hook_g0(G) :- wots_hs(S, in_bfly(f, pp_hook_g10(G))), write(S).  % Execute further pretty printing in a 'butterfly' context

/** mass_gt1(+O1) is semidet.
 *
 * Check if the mass of an object is greater than 1.
 *
 * @param O1 The object to check.
 */
mass_gt1(O1) :- into_obj(O1, O2), mass(O2, M), !, M > 1.  % Convert O1 into O2 and check if its mass is greater than 1

/** pp_hook_g10(+G) is semidet.
 *
 * Tertiary pretty print hook that handles more complex structures.
 *
 * @param G The term to print.
 */
pp_hook_g10(G) :- \+ plain_var(G), current_predicate(pp_hook_g1/1), lock_doing(in_pp_hook_g10, G, pp_hook_g1(G)).  % Lock and execute pp_hook_g1

/* Previously: as_grid_string(O,SSS):- is_grid(O),wots_vs(S,print_grid(O)), sformat(SSS,'{  ~w}',[S]).
   Commented out as it may be an alternative approach for grid formatting. */

/** as_grid_string(+O, -SSS) is semidet.
 *
 * Convert an object to a grid-formatted string.
 *
 * @param O The object to convert.
 * @param SSS The resulting string.
 */
as_grid_string(O, SSS) :- wots_vs(S, show_indiv(O)), sformat(SSS, '{  ~w}', [S]).  % Convert object O into a formatted string

/** as_pre_string(+O, -SS) is semidet.
 *
 * Convert an object to a pretty-printed string.
 *
 * @param O The object to convert.
 * @param SS The resulting string.
 */
as_pre_string(O, SS) :- wots_hs(S, show_indiv(O)), strip_vspace(S, SS).  % Convert object O into a pretty-printed string

/** pretty_grid(+O) is semidet.
 *
 * Pretty print a grid structure.
 *
 * @param O The grid to print.
 */
pretty_grid(O) :-
    catch(
        (wots_hs(S, print_grid(O)), strip_vspace(S, SS), ptc(orange, (format('"  ~w  "', [SS])))),
        _, fail), !.  % Try to pretty-print the grid, catching any errors
/* previously: pretty_grid(O) was commented out because its functionality was replaced or optimized elsewhere. 
   The use of the `catch/3` in this code suggests that it was handling errors when displaying grids, 
   which might have been deemed unnecessary or handled in a more centralized way.
*/

/**
 * pp_hook_g1(+O)
 * 
 * A hook predicate that prints various types of terms (e.g., grids, objects, colors).
 * This is used in contexts where specific term types need to be visualized in particular ways.
 * 
 * @param O The term to be printed.
 * 
 * @example
 * ?- pp_hook_g1(rhs(x)).
 *    Prints "rhs(x)" formatted in bold.
 */
pp_hook_g1(O) :-  
    plain_var(O),  % Check if O is an uninstantiated variable.
    !,             % If true, cut to prevent backtracking and fail since we don't print plain variables.
    fail.

pp_hook_g1(O) :-  
    attvar(O),        % Check if O is an attributed variable.
    !,                % If true, handle the attributed variable.
    is_colorish(O),   % Check if O has color properties.
    data_type(O, DT), % Retrieve the data type of O.
    writeq('...'(DT)), % Write the data type in a quoted form.
    !.               % Cut to prevent further rules from being executed.

pp_hook_g1(S) :-  
    term_is_ansi(S),  % Check if the term S is an ANSI-compatible term (for formatting purposes).
    !,                % If true, handle it accordingly.
    write_nbsp,       % Write a non-breaking space.
    write_keeping_ansi_mb(S).  % Write S while preserving ANSI formatting.

% previously: term_contains_ansi was commented out because the ANSI formatting for terms 
% containing special sequences may have been replaced with other logic.
% pp_hook_g1(S) :- term_contains_ansi(S), !, fail, write_nbsp, write_keeping_ansi_mb(S).

pp_hook_g1(rhs(O)) :-  
    write_nbsp,           % Write a non-breaking space.
    nl,                   % Print a newline.
    bold_print(print(r_h_s(O))),  % Print the right-hand side (r_h_s) of the term O in bold.
    !.

pp_hook_g1(iz(O)) :-  
    compound(O),          % Check if O is a compound term.
    O = info(_),          % Further check if O has the form info(_).
    underline_print(print(izz(O))),  % Print O with underlined text.
    !.

pp_hook_g1(O) :-  
    is_grid(O),           % Check if O is a grid (specific term type).
    /* previously: This line involving sub_term was commented out because it was deemed redundant. 
       The sub_term check for '$VAR'(_) was unnecessary in the current context.
    */
    pretty_grid(O).       % Call the pretty_grid predicate to print the grid.

pp_hook_g1(O) :-  
    is_object(O),            % Check if O is an object (a complex term).
    into_solid_grid(O, G),   % Convert O into a solid grid form.
    wots(SS, pretty_grid(G)), % Fetch the grid representation and process it.
    write(og(SS)),           % Write the grid (og stands for "output grid").
    !.

pp_hook_g1(shape_rep(grav,O)) :-  
    is_points_list(O),       % Check if O is a list of points.
    as_grid_string(O, S),    % Convert the list of points into a grid string.
    wotsq(O, Q),             % Perform some query/processing on O (wotsq likely prints or returns a result).
    print(shape_rep(grav, S, Q)), % Print the shape representation with gravity.
    !.

pp_hook_g1(vals(O)) :-  
    !, 
    writeq(vals(O)),  % Print the value of O quoted.
    !.

% previously: l2r(O) code block was commented out, possibly because it duplicates grid representation functionality.
% pp_hook_g1(l2r(O)) :- into_solid_grid_strings(l2r(O), Str), Str \=@= l2r(O), print_term_no_nl(Str), !.

pp_hook_g1(localpoints(O)) :-  
    is_points_list(O),       % Check if O is a list of points.
    as_grid_string(O, S),    % Convert the list of points to a grid string.
    wotsq(O, Q),             % Perform query/processing on O.
    print(localpoints(S, Q)), % Print the local points.
    !.

pp_hook_g1(C) :-  
    compound(C),              % Check if C is a compound term.
    compound_name_arguments(C, F, [O]), % Extract the functor and arguments of the compound term.
    is_points_list(O),        % Check if O is a list of points.
    length(O, N), N > 2,      % Ensure O contains more than two points.
    as_grid_string(O, S),     % Convert O to a grid string.
    compound_name_arguments(CO, F, [S]), % Reconstruct the compound term with the grid string.
    print(CO),                % Print the new compound term.
    !.

pp_hook_g1(O) :-  
    is_points_list(O),         % Check if O is a list of points.
    as_grid_string(O, S),      % Convert the list to a grid string.
    write(S),                  % Write the grid string.
    !.

pp_hook_g1(O) :-  
    is_real_color(O),          % Check if O represents a real color.
    color_print(O, call(writeq(O))), % Print the color in a special colored format.
    !.

pp_hook_g1(O) :-  
    is_colorish(O),            % Check if O has color properties.
    data_type(O, DT),          % Get the data type of O.
    writeq('...'(DT)),         % Print the data type of O.
    !.

pp_hook_g1(_) :-  
    \+ in_pp(ansi),            % If we are not in ANSI mode, fail.
    !,
    fail.

pp_hook_g1(Grp) :-  
    current_predicate(pp_ilp/1),  % Check if the predicate pp_ilp/1 exists.
    is_rule_mapping(Grp),        % Check if Grp is a rule mapping.
    pp_ilp(Grp),                 % Call pp_ilp to print the rule mapping.
    !.

pp_hook_g1(O) :-  
    atom(O),                    % Check if O is an atom.
    atom_contains(O, 'o_'),      % Ensure O contains the substring 'o_'.
    pp_parent([LF|_]),          % Get the parent of the current term.
    \+ (LF == lf; LF == objFn), % Ensure LF is not 'lf' or 'objFn'.
    resolve_reference(O, Var),  % Resolve O to its reference Var.
    O \== Var,                  % Ensure O is different from Var.
    \+ plain_var(Var),          % Ensure Var is not a plain variable.
    !,
    write_nbsp,                 % Write a non-breaking space.
    writeq(O),                  % Print O quoted.
    write(' /* '),              % Write a comment opening.
    show_indiv(Var),            % Show the individual Var.
    write(' */ ').              % Close the comment.

pp_hook_g1(O) :-  
    is_object(O),               % Check if O is an object.
    pp_no_nl(O),                % Print O without a newline.
    !.

pp_hook_g1(O) :-  
    is_group(O),                % Check if O is a group.
    pp_no_nl(O),                % Print O without a newline.
    !.

% previously: change_obj was commented out, possibly due to redundancy in object comparison and presentation.
% pp_hook_g1(change_obj(N, O1, O2, Sames, Diffs)) :- showdiff_objects5(N, O1, O2, Sames, Diffs), !.

pp_hook_g1(O) :-  
    is_vm_map(O),               % Check if O is a VM map.
    data_type(O, DT),           % Get the data type of O.
    writeq('..map.'(DT)),       % Print the data type of the VM map.
    !.

pp_hook_g1(O) :-  
    is_gridoid(O),              % Check if O is a grid-like object.
    show_indiv(O),              % Show the individual properties of the grid-like object.
    !.

% previously: change_obj and diff were commented out, possibly due to updates in the diff and object change visualization mechanism.
% pp_hook_g1(O) :- O = change_obj(O1, O2, _Same, _Diff), w_section(showdiff_objects(O1, O2)), !.
% pp_hook_g1(O) :- O = change_obj(O1, O2, _Same, _Diff), w_section(object, [O1, O2], with_tagged('h5', pp(O))).

pp_hook_g1(O) :-  
    O = showdiff(O1, O2),       % If O represents a difference between O1 and O2.
    !,
    showdiff(O1, O2).           % Show the differences between O1 and O2.

% previously: compound(O), wqs1(O) was commented out as part of an optimization or replacement.
% pp_hook_g1(O) :- compound(O), wqs1(O), !.

pp_hook_g1(O) :-  
    \+ compound(O),             % If O is not a compound term, fail.
    fail.

pp_hook_g1(G) :-  
    '@'(pp_hook_g1a(G), user).  % Call pp_hook_g1a in the context of the user.

pp_hook_g1a(G) :-  
    \+ current_prolog_flag(debug, true),  % If debug mode is off.
    current_predicate(pp_hook_g2/1),     % Check if pp_hook_g2/1 exists.
    lock_doing(in_pp_hook_g3, any, pp_hook_g2(G)),  % Lock and execute pp_hook_g2 for G.
    !.
/* PLDoc header for pp_hook_g1a/1
   This predicate processes the term `G` and applies the `fch/1` predicate to it.
   It uses cut (!) to ensure the processing stops after the first match.
   @param G The term to be processed.
   @example pp_hook_g1a(example_term). */
pp_hook_g1a(G) :- 
    % Applies the fch/1 predicate to the input term G
    fch(G), 
    % Cut (!) ensures that no further rules are considered
    !.

/* previously: pp_hook_g2/1 was intended to process output terms using colorization,
   but it's commented out, possibly due to dependency on a predicate `colorize_oterms/2`.
   Dead code explanation: Skipped due to reliance on a condition `current_predicate(colorize_oterms/2)`
   which may not always be true or may no longer be needed. */
%pp_hook_g2(O):- current_predicate(colorize_oterms/2),colorize_oterms(O,C), notrace(catch(fch(C),_,fail)),! .

/* PLDoc header for fch/1
   This predicate is used for formatting or outputting a term `O`. Currently,
   it applies the `wqs1/1` predicate to the input term `O`.
   @param O The term to be output or formatted.
   @example fch(example_term). */
fch(O) :- 
    % Applies the wqs1/1 predicate to the input term O
    wqs1(O).

/* previously: Other variations of `fch/1` were used for different printing mechanisms 
   such as `pp_no_nl/1` for printing without newline, but they are now commented out 
   and not in use, possibly for simplifying the output format. */
%fch(O):- pp_no_nl(O).
%fch(O):- print(O).
%fch(O):- p_p_t_no_nl(O).

/* PLDoc header for wotsq/2
   This predicate takes a term `O` and a second argument `Q` and processes them 
   using the `wots_hs/2` and `wqnl/1` predicates.
   @param O The term to be processed.
   @param Q The second argument used for processing.
   @example wotsq(term1, term2). */
wotsq(O, Q) :- 
    % Calls the wots_hs/2 predicate with the second argument Q 
    % and the result of wqnl/1 applied to O.
    wots_hs(Q, wqnl(O)).

/* PLDoc header for has_goals/1
   This predicate checks if a term `G` has goals by examining its attributed 
   variables (attvars) or if its variables and singletons differ.
   @param G The term to be examined.
   @example has_goals(example_term). */
has_goals(G) :- 
    % Check if the term G has attributed variables (attvars).
    term_attvars(G, AV), 
    AV \== [].
has_goals(G) :- 
    % Check if the term G has variables that are not singletons.
    term_variables(G, TV), 
    term_singletons(G, SV), 
    TV \== SV.

/* PLDoc header for maybe_term_goals/3
   This predicate examines a term and produces its attributed variables and goals,
   copying terms and applying numbervars to its variables.
   @param Term The original term.
   @param TermC The copied term after processing.
   @param Goals The list of goals associated with the term.
   @example maybe_term_goals(example_term, CopiedTerm, Goals). */
maybe_term_goals(Term, TermC, Goals) :- 
    % Extract attributed variables from the term
    term_attvars(Term, Attvars), 
    Attvars \== [], 
    !,  % Cut to prevent further backtracking if attributed variables are found
    term_variables(Term, Vars),
    % Filter out variables that are not in the attributed variables list
    include(not_in(Attvars), Vars, PlainVars),
    % Copy term along with attributed and plain variables
    copy_term((Attvars + PlainVars + Term), (AttvarsC + PlainVarsC + TermC), Goals),
    % Number the variables starting from 10, skipping attributed variables
    numbervars(PlainVarsC, 10, Ten1, [singletons(true), attvar(skip)]),
    % Number the attributed variables and goals
    numbervars(AttvarsC + Goals, Ten1, _Ten, [attvar(bind), singletons(false)]).

/* PLDoc header for maybe_replace_vars/5
   This predicate replaces variables in goals if necessary, using sub_var and 
   freeze for goal evaluation.
   @param VarsC The list of variables to replace.
   @param SGoals The original goals.
   @param TermC The copied term.
   @param RSGoals The resulting goals after replacement.
   @param RTermC The resulting term after replacement.
   @example maybe_replace_vars([Var1, Var2], Goals, CopiedTerm, ResultGoals, ResultTerm). */
maybe_replace_vars([], SGoals, TermC, SGoals, TermC) :- 
    !.  % If there are no variables to replace, return the original goals and term.
maybe_replace_vars([V | VarsC], SGoals, TermC, RSGoals, RTermC) :- 
    % Partition the goals into those containing the variable V and those without
    my_partition(sub_var(V), SGoals, Withvar, WithoutVar),
    % Ensure that only one goal contains the variable
    Withvar = [OneGoal],
    % Use freeze to delay evaluation of the goal until it's not null
    freeze(OneGoal, (OneGoal \== null, OneGoal \== @(null))),
    % Ensure the variable appears only once in the term
    findall(_, sub_var(V, TermC), LL), 
    LL = [_], 
    !,
    % Substitute the variable V with the goal in the term and goals
    subst([WithoutVar, TermC], V, {OneGoal}, [SGoalsM, TermCM]),
    % Recursively replace remaining variables
    maybe_replace_vars(VarsC, SGoalsM, TermCM, RSGoals, RTermC).
maybe_replace_vars([_ | VarsC], SGoals, TermC, RSGoals, RTermC) :- 
    % If the variable is not found, continue with the next variable
    maybe_replace_vars(VarsC, SGoals, TermC, RSGoals, RTermC).

/* PLDoc header for src_sameish/2
   This predicate checks if two terms are "sameish", i.e., structurally equivalent.
   @param Orig The original term.
   @param Find The term to compare with.
   @example src_sameish(term1, term2). */
src_sameish(Orig, Find) :- 
    % Copy the original term to a new variable COrig
    copy_term(Orig, COrig), 
    % Set Find to Orig and check if Orig and COrig are structurally equivalent
    Find = Orig, 
    Orig =@= COrig.

/* PLDoc header for number_vars_calc_goals/3
   This predicate calculates goals and assigns numbered variables to a term, 
   taking into account its singletons and attributed variables.
   @param Term The original term.
   @param SSRTermC The term after processing with numbered variables.
   @param SRSGoals The sorted list of goals.
   @example number_vars_calc_goals(example_term, ProcessedTerm, Goals). */
number_vars_calc_goals(Term, SSRTermC, [1 | SRSGoals]) :- 
    % Extract singletons and attributed variables from the term
    term_singletons(Term, Singles),
    term_attvars(Term, Vars),
    % Copy the term, variables, and singletons
    copy_term(Term + Vars + Singles, TermC + VarsC + SinglesC, Goals),
    % Number the variables and goals, skipping attributed variables
    notrace(catch(numbervars(TermC + Goals, 0, _Ten1, [singletons(false), attvar(skip)]), _, fail)),
    % Sort the goals based on variables
    sort_goals(Goals, VarsC, SGoals),
    % Replace variables in the goals and term if necessary
    maybe_replace_vars(VarsC, SGoals, TermC, RSGoals, RTermC),
    % Filter out non-substituted singletons
    include(not_sub_var(RSGoals), SinglesC, KSingles),
    % Create placeholder variables for remaining singletons
    length(KSingles, SL), 
    length(VSingles, SL), 
    my_maplist(=('$VAR'('__')), VSingles),
    % Substitute the singletons and variables in the term and goals
    subst_2L(KSingles, VSingles, [RTermC, RSGoals], [SRTermC, SRSGoals]),
    % Apply specific substitutions based on matching patterns
    subst_1L_p2(src_sameish, [
        {dif('$VAR'('__'), RED)} = dif(RED),
        {cbg('$VAR'('__'))} = cbg
    ], SRTermC, SSRTermC), 
    !.

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




/* number_vars_calc_goals/3 calculates variables and goals for a given term */
% @param Term Input term.
% @param TermC Copy of the term after variable numbering.
% @param [4|SGoals] Numbered goals starting with a 4.
% Uses numbervars with the option singletons(true) to ensure unique variable names.
number_vars_calc_goals(Term, TermC, [4|SGoals]) :-
    /* term_variables/2 extracts the free variables in Term */
    term_variables(Term, Vars),

    /* term_attvars/2 extracts attributed variables in Term */
    term_attvars(Term, Attvars),

    /* copy_term/3 creates a copy of the term and variables, including goals */
    copy_term(Term+Vars+Attvars, TermC+VarsC+AttvarsC, Goals),

    /* numbervars/3 numbers variables in TermC and Goals */
    notrace(catch(numbervars(TermC+Goals, 0, _Ten1, [singletons(true)]), _, fail)),

    /* append/3 appends lists of attributed variables and free variables */
    append([AttvarsC, VarsC, AttvarsC, Vars], Sorted),

    /* sort_goals/3 sorts the goals based on variable ordering */
    sort_goals(Goals, Sorted, SGoals), !.

/* Another variant of number_vars_calc_goals, differing in options */
% @param [5|SGoals] Numbered goals starting with a 5.
number_vars_calc_goals(Term, TermC, [5|SGoals]) :-
    term_variables(Term, Vars),
    term_attvars(Term, Attvars),
    copy_term(Term+Vars+Attvars, TermC+VarsC+AttvarsC, Goals),
    /* numbervars with singletons(false) and attvar(skip) options */
    numbervars(TermC+Goals, 0, _Ten1, [singletons(false), attvar(skip)]),
    append([AttvarsC, VarsC, Attvars, Vars], Sorted),
    sort_goals(Goals, Sorted, SGoals), !.

/* writeg/1 tries to write a term with extra handling */
% @param Term The term to write.
% Uses writeg0 or fallback to ppa for error handling.
writeg(Term) :- 
    ignore(\+ notrace(catch(once(writeg0(Term); ppa(Term)), E, (pp(E), ppa(Term))))), !.

/* writeg0/1 handles writing a term, including attributed variables */
% @param Term The term to write.
% Writes attributed variables and goals if applicable.
writeg0(Term) :-
    term_attvars(Term, Attvars),
    Attvars \== [], !,
    must_det_ll((
        number_vars_calc_goals(Term, TermC, Goals),
        writeg5(TermC), !,
        if_t(Goals \== [], (
            nl_if_needed,
            write(' goals='), 
            call_w_pad_prev(3, az_ansi(print_tree_no_nl(Goals)))
        ))
    )), !.

/* Writes ground terms or invokes numbering for variables */
writeg0(Term) :- 
    \+ ground(Term), 
    \+ \+ must_det_ll((
        numbervars(Term, 0, _Ten1, [singletons(true), attvar(skip)]), 
        writeg5(Term)
    )).

/* If no special handling is needed, just write the term */
writeg0(Term) :- writeg5(Term), !.

/* writeg5 handles specific types of terms for formatted output */
writeg5(X) :- 
    is_ftVar(X), !, write_nbsp, write_nbsp, print(X), write_nbsp.

/* Special handling for 2x2 grids */
writeg5(N=V) :- 
    is_simple_2x2(V), !, 
    print_grid(N, V), 
    writeln(' = '), 
    call_w_pad_prev(2, writeg9(V)).

/* Special handling for grid-like structures */
writeg5(N=V) :- 
    is_gridoid(V), !, 
    print_grid(N, V), 
    writeln(' = '), 
    call_w_pad_prev(2, writeg9(V)).

/* Generic handling of non-variable terms */
writeg5(N=V) :- 
    nl_if_needed, 
    nonvar(N), 
    pp_no_nl(N), 
    writeln(' = '), 
    !, 
    call_w_pad_prev(2, writeg5(V)).

/* Default failure case */
writeg5(_) :- write_nbsp, fail.

/* Recursive term handler */
writeg5(V) :- writeg9(V).

/* writeg8 is a helper function for printing ftVars or variables */
writeg8(X) :- is_ftVar(X), !, print(X).
writeg8(X) :- var(X), !, print(X).
writeg8(X) :- writeq(X).

/* writeg9 handles lists and structured outputs */
writeg9(V) :- is_simple_2x2(V), !, print_simple_2x2(writeg8, V).
writeg9(V) :- is_list(V), nl_if_needed, write('['), !, my_maplist(writeg5, V), write(']').
writeg9(_) :- write_nbsp, write(' \t '), fail.
writeg9(X) :- is_ftVar(X), !, write_nbsp, write_nbsp, print(X).
writeg9(V) :- pp_no_nl(V).

/* previously: alternative writeg5 implementation, now skipped for clarity */
% Kept for legacy reasons, but not used in the current implementation.
/*
writeg5(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg5(V):- is_gridoid(V),!,call_w_pad_prev(2,writeg9(V)).
writeg5(V):- is_list(V),nl_if_needed,write('['),my_maplist(writeg5,V),write(']').
*/

/* arg1_near checks if the first argument of a goal matches a specific variable */
% @param Vars List of variables.
% @param Goal Goal to match against.
% @param Nth Position of the variable in Vars.
arg1_near(Vars, Goal, Nth) :- 
    tc_arg(1, Goal, PreSort), 
    nth1(Nth, Vars, E), 
    E == PreSort, !.

arg1_near(_VarsC, Goal, PreSort) :- 
    tc_arg(1, Goal, PreSort), !.

arg1_near(_VarsC, Goal, Goal).

/* sort_goals uses predsort to order goals based on their variables */
% @param Goals The list of goals.
% @param VarsC The variables for sorting.
% @param SGoals Sorted list of goals.
sort_goals(Goals, VarsC, SGoals) :- 
    predsort(sort_on(arg1_near(VarsC)), Goals, SGoals).

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



/* previously: This section contains older code related to printing terms using pp_hook_g1 and pp_hook_g.
   These predicates handle specific use cases for pretty-printing terms. Although not in active use,
   they have been retained for future debugging or extensions.
*/

% pp_hook_g1(T):-
%  nb_current('$portraying',Was)
%    ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T)))
%    ; locally(b_setval('$portraying',[T]),ptv0(T)).


/**
 * strip_vspace(+S, -Stripped)
 *
 * Recursively removes leading and trailing whitespace characters from a string S and returns the cleaned string as Stripped.
 * 
 * @param S The input string with potential whitespace
 * @param Stripped The cleaned string with unnecessary whitespace removed
 */
strip_vspace(S,Stripped):- 
    % Remove spaces before recursively stripping the string
    string_concat(' ',SS,S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove spaces after the string and recursively strip
    string_concat(SS,' ',S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove newline before the string and recursively strip
    string_concat('\n',SS,S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove newline after the string and recursively strip
    string_concat(SS,'\n',S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove tabs before the string and recursively strip
    string_concat('\t',SS,S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove tabs after the string and recursively strip
    string_concat(SS,'\t',S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Replace certain whitespace patterns with more compact representations and recursively strip
    replace_in_string([" \n"="\n","(   "="(  ","(\n"="( "],S,S2),S2\==S,!, 
    strip_vspace(S2,Stripped).

/* previously: An alternative method for stripping whitespace using split_string was left out, 
   likely due to performance considerations with handling large strings or frequent operations.
*/

% strip_vspace(S,Stripped):- split_string(S, "", "\t\r\n", [Stripped]).
strip_vspace(S,S).


/**
 * print_nl(+P)
 *
 * Prints the term P with a newline if necessary, optionally applying color formatting.
 * 
 * @param P The term to print
 */
print_nl(P):- 
    % Print newline if needed, then print the term without an extra newline
    nl_if_needed, 
    wots_hs(SS,pp_no_nl(P)), 
    maybe_color(SS,P), 
    nl_if_needed.


/**
 * color_write(+S)
 *
 * Writes a term with ANSI formatting if applicable.
 * 
 * @param S The term to write, which could contain ANSI codes or be a normal term
 */
color_write(S):- 
    % If the term is ANSI formatted, preserve formatting
    term_is_ansi(S), !, 
    write_keeping_ansi_mb(S).

color_write(P):- 
    % Otherwise, print the term
    wots_hs(SS,write(P)), 
    maybe_color(SS,P).


/**
 * write_keeping_ansi_mb(+P)
 *
 * Preserves ANSI formatting while writing a term.
 * 
 * @param P The term to write, potentially with ANSI formatting
 */
write_keeping_ansi_mb(P):- 
    % If the term is bold or potentially bold, preserve the formatting
    is_maybe_bold(P,write_keeping_ansi(P)).


/**
 * is_maybe_bold(+P)
 *
 * Checks if a term should be printed in bold by analyzing its content.
 * 
 * @param P The term to check
 */
is_maybe_bold(P):- 
    % Format the term into a string and check if it contains specific bold-related markers
    sformat(S,'~w',[P]), 
    atom_contains(S,'stOF').

is_maybe_bold(P,G):- 
    % If the term qualifies for bold, print it with underline and bold
    is_maybe_bold(P),!, 
    underline_print(bold_print(G)).

is_maybe_bold(_P,G):- 
    % Otherwise, just call the generic printing method
    call(G).


/**
 * pp_msg_color(+P, +C)
 *
 * Prints a message P with color C if applicable.
 * 
 * @param P The message term to print
 * @param C The color associated with the message
 */
pp_msg_color(P,C):- 
    % If the term is compound, check if it has color annotations
    compound(P), 
    pc_msg_color(P,C),!.

pp_msg_color(P,C):- 
    % Otherwise, print the message with default color settings
    must_det_ll(mesg_color(P,C)).


/**
 * pc_msg_color(+P, +C)
 *
 * Handles specific message types for colored output.
 * 
 * @param P The message term to print
 * @param C The color associated with the message
 */
pc_msg_color(iz(P),C):- 
    % If the term is wrapped in 'iz', pass it on to pp_msg_color
    pp_msg_color(P,C).

pc_msg_color(link(P,_,_),C):- 
    % If the term is a 'link', pass it on to pp_msg_color
    pp_msg_color(P,C).
/** <module> Color handling for different message types

This module provides predicates to determine the color to be used for various types
of messages based on the input format (link, diff, etc.).

*/

% Predicate to get the color of a message for a given link.
% @param link(P,_) The link data, where P is the relevant part to determine color.
% @param C The color to be returned.
% @example 
%   ?- pc_msg_color(link(example, _), Color).
%   Color = green.
pc_msg_color(link(P,_), C):- 
    % Call to determine color based on P using pp_msg_color/2.
    pp_msg_color(P, C).

% Predicate to get the color of a message for a conditional format (->_).
% @param (->_) A conditional term where P is the relevant part to determine color.
% @param C The color to be returned.
pc_msg_color((->_P), C):- 
    % Call to determine color based on P.
    pp_msg_color(P, C).

% Predicate to get the color of a message for the head of a list.
% @param [P|_] A list where P is the relevant part to determine color.
% @param C The color to be returned.
pc_msg_color([P|_], C):- 
    % Call to determine color based on the first element of the list.
    pp_msg_color(P, C).

% Predicate to get the color of a message for a diff type.
% @param diff(P) The diff format where P is the relevant part to determine color.
% @param C The color to be returned.
pc_msg_color(diff(P), C):- 
    % Call to determine color based on P in diff context.
    pp_msg_color(P, C).

/* previously: meta_predicate declaration for wots_hs/1
   This has been commented out because the wots_hs/1 predicate is no longer being used.
   However, it has been preserved for reference. */

%:- meta_predicate(wots_hs(0)).
%wots_hs(G):- wots_hs(S,G),write(S).

/* File Directive: Declares that wots_ansi/2 is a meta-predicate 
   The first argument is an output stream, and the second argument is a goal to be executed. */
:- meta_predicate(wots_ansi(-,0)).

% Predicate to handle ansi formatting within wots.
% @param S The output stream.
% @param Goal The goal to be executed.
wots_ansi(S, Goal):- 
    % Call wots/2 with the goal wrapped in woto_ansi.
    wots(S, woto_ansi(Goal)).

/* File Directive: Declares wots_html/2 as a meta-predicate, similar to wots_ansi/2. */
:- meta_predicate(wots_html(-,0)).

% Predicate to handle HTML formatting within wots.
% @param S The output stream.
% @param Goal The goal to be executed.
wots_html(S, Goal):- 
    % Call wots/2 with the goal wrapped in woto_html.
    wots(S, woto_html(Goal)).

/* previously: another declaration for wots_hs/2 was present.
   It has been kept in the comments for reference. */
:- meta_predicate(wots_hs(-,0)).

%:- wots_hs(S,G):- \+ wants_html,!,wots(S,G).
%:- wots_hs(S,G):- wots(S,G),!.

% Predicate to handle space removal within wots_hs/2.
% @param S The final output with reduced spaces.
% @param G The goal to be executed.
wots_hs(S, G):- 
    % First, call wots to execute G and store result in SS.
    wots(SS, G),
    % Then, remove extra spaces using remove_huge_spaces/2.
    notrace(remove_huge_spaces(SS, S)).

% Meta-predicate declaration for wots_vs/2.
:- meta_predicate(wots_vs(-,0)).

% Predicate to fix vertical space in the output of wots_vs/2.
% @param OOO The output with adjusted vertical space.
% @param G The goal to be executed.
wots_vs(OOO, G):- 
    % First, call wots to execute G and store result in S.
    wots(S, G),
    % Fix vertical space using fix_vspace/2.
    notrace(fix_vspace(S, OOO)).

% Predicate to adjust vertical spaces in the output string.
% @param S The input string.
% @param OOO The adjusted string without excessive vertical space.
fix_vspace(S, OOO):- 
    % Strip vertical spaces from the input string.
    strip_vspace(S, SS), 
    % If SS contains newlines, handle it with additional formatting.
    (atom_contains(SS, '\n') ->
        % Output formatted string with newlines and indentation.
        wots_hs(SSS, (nl_now, write('   '), write(SS), nl_now));
        % Otherwise, keep SS as the result.
        SSS = SS),
    % Finally, remove huge spaces from the result.
    remove_huge_spaces(SSS, OOO).

% Write a list or single element in "tall" format.
% @param L The list or element to be written.
write_tall(L):- 
    % If the input is a list, map write_tall over each element.
    is_list(L), 
    !, 
    my_maplist(write_tall, L).

% Write a single element in "tall" format.
write_tall(E):- 
    % Call wots_vs with the element and write it out.
    wots_vs(S, wqs_c(E)), 
    writeln(S).

% Write a list or single element in "wide" format.
% @param L The list or element to be written.
write_wide(L):- 
    % If the input is a list, map write_wide over each element.
    is_list(L), 
    !, 
    my_maplist(write_wide, L).

% Write a single element in "wide" format.
write_wide(E):- 
    % Call wots_vs with the element and write it without a newline.
    wots_vs(S, wqs_c(E)), 
    write(S), 
    write_nbsp.

% Predicate to replace carriage returns with <br> for HTML or ansi.
% @param S The input string.
% @param SS The output string with line breaks.
p_to_br(S, SS):- 
    % First, fix line breaks using fix_br_nls/2.
    fix_br_nls(S, S0),
    % Then, handle <br> and other replacements using cr_to_br/2.
    cr_to_br(S0, SSS),
    % Replace <p> and other HTML tags with spaces or line breaks.
    replace_in_string(['<p>'='<br>', '<br/>'='<br>', '</p>'=' ', '<p/>'='<br>', '<br><br>'='<br>'], SSS, SSSS),
    % Apply final line break replacement.
    cr_to_br(SSSS, SS).

% Predicate to replace carriage returns for HTML.
% @param S The input string.
% @param SSS The output string with <br> tags.
cr_to_br_html(S, SSS):- 
    % Replace carriage return and newlines with <br> for HTML.
    replace_in_string(['\r\n'='<br>', '\r'='<br>', '\n'='<br>'], S, SSS).

% Predicate to replace <br> for ansi format.
% @param S The input string.
% @param SSS The output string with \n for ansi.
cr_to_br_ansi(S, SSS):- 
    % Replace <br> with newlines for ansi formatting.
    replace_in_string(['<br>'='\n', '&nbsp;'=' '], S, SSS).

% Predicate to fix broken line breaks in HTML strings.
% @param S The input string.
% @param O The fixed output string.
fix_br_nls(S, O):- 
    % Perform a set of replacements to clean up HTML line breaks.
    replace_in_string(['<br/>\n'='<br/>', '<br>\n'='<br>', '</p>\n'='</p>', '<p/>\n'='<p/>', '<p>\n'='<p>',
                       '\n<br>'='<br>', '\n<br/>'='<br/>', '\n</p>'='</p>', '\n<p/>'='<p/>', '\n<p>'='<p>'], S, O).

% Predicate to remove excessive spaces from a string.
% @param S The input string.
% @param O The output string with reduced spaces.
remove_huge_spaces(S, O):- 
    % First, fix line breaks.
    notrace((fix_br_nls(S, SS), !, 
    % Then replace spaces with <br> using p_to_br/2.
    p_to_br(SS, O))), 
    !.

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



wqs_l(H):- 
    \+ is_list(H),  /* If H is not a list, directly process it with wqs/1 */
    !,               /* Cut to avoid further choices once non-list case is handled */
    wqs(H).          /* Call wqs/1 on the non-list input */
    
wqs_l(H):- 
    wqs(H).          /* If H is a list, directly call wqs/1 to handle it */

/**
 * @predicate wqs/1
 * Processes the input term using wqs0/1, and applies colors if applicable.
 * 
 * @param P Input term that will be processed and potentially colored.
 * 
 * @example 
 * ?- wqs(foo).  % Calls wqs0/1 to process and output foo.
 */
wqs(P):- 
    wots_hs(SS, wqs0(P)),   /* Call wots_hs/2 with the result of wqs0(P) */
    maybe_color(SS, P).     /* Apply color formatting to the output if appropriate */

/**
 * @predicate wqs/2
 * Processes the term using wqs0/1 with an additional condition.
 * 
 * @param C A condition to be checked.
 * @param P The term to be processed if the condition holds.
 */
wqs(C, P):- 
    ansicall(C, wqs0(P)),  /* Calls wqs0/1 on P if C holds, and applies ANSI formatting */
    !.                     /* Cut to ensure no backtracking */

/**
 * @predicate wqs0/1
 * Base case for processing different types of input in wqs/1 and wqs/2.
 * 
 * Handles variables, colors, maps, and ANSI terms accordingly.
 * 
 * @param X The input term to process based on its type.
 */
wqs0(X):- 
    plain_var(X),          /* If X is a plain variable */
    wqs(plain_var(X)),     /* Process the variable in wqs/1 */
    !.

wqs0(X):- 
    plain_var(X),          /* Redundant check for plain variable (could be optimized out) */
    !, 
    wqs(plain_var(X)),     /* Process the plain variable again */
    ibreak.                /* Force a break point in execution for debugging purposes */

wqs0(S):- 
    term_is_ansi(S),       /* If S is an ANSI-compatible term */
    !, 
    write_keeping_ansi_mb(S).  /* Write S to output while preserving ANSI formatting */

wqs0(C):- 
    is_colorish(C),        /* If C is a color or color-like term */
    color_print(C, C),     /* Output C with color formatting */
    !.

wqs0(G):- 
    is_vm_map(G),          /* If G is a virtual machine map */
    !, 
    write_map(G, 'wqs').   /* Write the VM map to the output with a label 'wqs' */

wqs0(X):- 
    var(X),                /* If X is an unbound variable */
    !, 
    get_attrs(X, AVs),     /* Get the attributes of X */
    !, 
    writeq(X),             /* Write the variable name */
    write('/*{'),          /* Open comment block for attributes */
    print(AVs),            /* Print the attributes */
    write('}*/').          /* Close comment block */

/* previously: old predicates for special handling removed but still present here */
/* Code dealing with specific legacy term types, replaced with more general handling */
/* 
  This is the main entry predicate for processing different types of input. 
  It first checks if the input is an attributed variable, and then dispatches the appropriate handlers based on the input type.
*/

%% wqs0(+X)
% Entry point for various types of input
% 
% @param X The input to be processed, can be of various types like variables, lists, compound terms, etc.
% @example
%   ?- wqs0(attvar(X)).
%   Processed attributed variable.
wqs0(X):- 
    % If X is an attributed variable, process it
    attvar(X), 
    !, 
    wqs(attvar(X)).

% Handle the special case for 'nl_now', triggering an immediate newline
wqs0(nl_now):- 
    !, 
    nl_now.

% Handle empty strings, skipping them
wqs0(X):- 
    X=='', 
    !.

% Handle empty lists, skipping them
wqs0(X):- 
    X==[], 
    !.

% If the input is a grid structure, print the grid
wqs0(X):- 
    is_grid(X), 
    !, 
    print_grid(X).

% If the input is a callable term, evaluate the call
wqs0(G):- 
    compound(G), 
    G = call(C), 
    callable(C), 
    !, 
    call(C).

% If the input is a single element list, process the element recursively
wqs0([T]):- 
    !, 
    wqs(T).

% If the head of the list is a string, write the string and process the tail
wqs0([H|T]):- 
    string(H), 
    !, 
    write(H), 
    write_nbsp, 
    wqs(T).

% Skip compound terms with 'skip' structure and process the tail
wqs0([H|T]):- 
    compound(H), 
    skip(_) = H, 
    !, 
    wqs(T).

% Process the head of the list, check if a newline is needed, and process the tail
wqs0([H|T]):- 
    wqs(H), 
    need_nl(H, T), 
    wqs(T), 
    !.

% Handle objects, attempt to "tersify" (simplify) them, and process recursively
wqs0(X):- 
    is_object(X), 
    tersify1(X, Q), 
    X \== Q, 
    !, 
    wqs(Q).

% If the input is an object, show its shape
wqs0(X):- 
    is_object(X), 
    show_shape(X), 
    !.

% Handle strings that contain the character '~', format and write them with color
wqs0(X):- 
    string(X), 
    atom_contains(X, '~'), 
    catch((sformat(S, X, []), color_write(S)), _, fail), 
    !.

% Handle simple strings, write them with color
wqs0(X):- 
    string(X), 
    !, 
    color_write(X).

% Dead code: previously handled writing strings with special cases. 
% It's skipped now as it's redundant.
% wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
% wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
% wqs([H|T]):- compound(H),!, writeq(H), wqs(T).

% Handle the case where input is a callable term
wqs0(call(C)):- 
    !, 
    call(C).

% Handle non-compound terms, writing them with a non-breaking space
wqs0(X):- 
    \+ compound(X), 
    !, 
    write_nbsp, 
    write(X).

% Delegate compound terms to wqs1
wqs0(C):- 
    compound(C), 
    wqs1(C), 
    !.

% If nothing else works, delegate to wqs2
wqs0(C):- 
    wqs2(C).

% Dead code: previously handled ANSI terms with special conditions.
% It's skipped now due to performance reasons.
% wqs(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

% Delegate ANSI term processing to wqs2
wqs2(S):- 
    term_contains_ansi(S), 
    !, 
    write_nbsp, 
    write_keeping_ansi_mb(S).

% Dead code: previously handled HTML writing.
% Skipped as HTML generation is no longer required.
% wqs2(P):- wants_html,!,pp(P).

% File directive: Declare a thread-local variable for wqs_fb
:- thread_local(t_l:wqs_fb/1).

% If a thread-local wqs_fb handler is set, call it
wqs2(X):- 
    t_l:wqs_fb(P1), 
    call(P1, X), 
    !.

% Dead code: previously wrapped wqs2 handling with writeq.
% It's skipped now as the new handlers perform better.
% wqs2(X):- with_wqs_fb(writeq,X).

% Write the output using the fallback handler if no specialized handler is found
wqs2(X):- 
    with_wqs_fb(writeq, print(X)), 
    !.

% Dead code: alternative write term strategy. Skipped for simplicity.
% wqs2(X):- with_wqs_fb(writeq,((write_nbsp,write_term(X,[quoted(true)])))).

% Helper to set a thread-local wqs_fb handler for a goal
with_wqs_fb(FB, Goal):- 
    locally(t_l:wqs_fb(FB), Goal).

% Convert a term to a string for output
as_arg_str(C, S):- 
    wots_vs(S, print(C)).

% Check if a string is a valid ANSI string
arg_string(S):- 
    string(S), 
    !.
arg_string(S):- 
    term_contains_ansi(S), 
    !.

% Delegate non-compound terms to wqs0
wqs1(C):- 
    \+ compound(C), 
    !, 
    wqs0(C).

% Write ANSI terms
wqs1(S):- 
    term_is_ansi(S), 
    !, 
    write_keeping_ansi_mb(S).

% Handle formatted output with color
wqs1(format(C, N)):- 
    catch((sformat(S, C, N), color_write(S)), _, fail), 
    !.

% Handle formatted writef calls
wqs1(writef(C, N)):- 
    !, 
    writef(C, N).

% Quoted term handling, color write the output
wqs1(q(C)):- 
    \+ arg_string(C), 
    wots_hs(S, writeq(C)), 
    color_write(S), 
    !.

% Print bold term with color
wqs1(g(C)):- 
    \+ arg_string(C), 
    wots_vs(S, bold_print(wqs1(C))), 
    print(g(S)), 
    !.

% Handle print_ss terms
wqs1(print_ss(C)):- 
    \+ arg_string(C), 
    wots_vs(S, print_ss(C)), 
    wqs1(print_ss(S)), 
    !.

% Handle bold printing for a term
wqs1(b(C)):- 
    \+ arg_string(C), 
    wots_vs(S, bold_print(wqs1(C))), 
    color_write(S).

% Handle ANSI term writing
wqs1(T):- 
    \+ is_list(T), 
    term_contains_ansi(T), 
    !, 
    write_keeping_ansi_mb(T).

% Print normalized grid representation
wqs1(grid_rep(norm, C)):- 
    writeq(grid_rep(norm, C)), 
    !.

% Special case for handling grid terms
wqs1(grid(C)):- 
    writeq(grid(C)), 
    !.

% Output right-hand side of a rule
wqs1(rhs(RHS)):- 
    nl_now, 
    wqnl(rhs(RHS)), 
    nl_now.

% Dead code: specialized grid operations, now skipped.
% wqs1(grid_ops(norm,C)):- writeq(norm(C)),!.

% Pretty print terms
wqs1(pp(P)):- 
    wots_vs(S, pp_no_nl(P)), 
    write((S)).

% Pretty print terms with no newline
wqs1(ppt(P)):- 
    wots_vs(S, ppt_no_nl(P)), 
    write((S)).

% Handle wqs terms
wqs1(wqs(P)):- 
    wots_vs(S, wqs(P)), 
    write((S)).

% Handle color printing for wqs terms
wqs1(wqs(C, P)):- 
    wots_vs(S, wqs(P)), 
    color_print(C, S).

% Print term values
wqs1(vals(C)):- 
    writeq(vals(C)), 
    !.

% Dead code: handled colored values, no longer needed.
% wqs1(colors_cc(C)):- \+ arg_string(C), as_arg_str(C,S),wqs(colorsz(S)).

% Bold print with ANSI handling
wqs1(io(C)):- 
    \+ arg_string(C), 
    wots_vs(S, bold_print(wqs(C))), 
    write(io(S)).

% Underline the printed term
wqs1(uc(C, W)):- 
    !, 
    write_nbsp, 
    color_print(C, call(underline_print(format("\t~@", [wqs(W)])))).

% Color-print terms with specified color
wqs1(cc(C, N)):- 
    is_color(C), 
    !, 
    color_print(C, call(writeq(cc(C, N)))).

% Write navigation command
wqs1(write_nav_cmd(C, N)):- 
    !, 
    write_nav_cmd(C, N).

% Handle colored terms followed by normal processing
wqs1(-(C, N)):- 
    is_color(C), 
    !, 
    color_print(C, call(writeq(C))), 
    write('-'), 
    wqs(N).
/* 
  @predicate wqs1/1
  @desc Main predicate that dispatches based on the structure and properties of the input.
*/
% If N is not 0 and C is an attributed variable, extract attributes and continue processing with wqs/1.
wqs1(cc(C,N)):- 
    N \== 0, 
    attvar(C), 
    get_attrs(C,PC), 
    !, 
    wqs(ccc(PC,N)).

% If N is not 0 and C is an unbound variable, format C and continue processing.
wqs1(cc(C,N)):- 
    N \== 0, 
    var(C), 
    sformat(PC,"~p",[C]), 
    !, 
    wqs(ccc(PC,N)).

% If C is not an argument string, process it with color_print and continue with wqs/1.
wqs1(cc(C,N)):- 
    \+ arg_string(C), 
    wots_hs(S,color_print(C,C)), 
    wqs(cc(S,N)).

% Handle color_print if C is a valid color, printing with a non-breaking space.
wqs1(color_print(C,X)):- 
    is_color(C), 
    !, 
    write_nbsp, 
    color_print(C,X).

% Handle color_print if C is not a plain variable, printing with a non-breaking space.
wqs1(color_print(C,X)):- 
    \+ plain_var(C), 
    !, 
    write_nbsp, 
    color_print(C,X).

% Handle grid-like arguments with an area less than 5.
wqs1(X):- 
    into_f_arg1(X,_,Arg),
    is_gridoid(Arg),
    area_or_len(Arg,Area),
    Area < 5,
    writeq(X),
    !.

/* previously: wqs1(C):- callable(C), is_wqs(C), wots_vs(S,catch(C,_,fail)),write((S)).
   Comment: This was skipped because it's attempting to execute `C` as a goal and catch errors. 
   The logic is disabled but kept for potential future use.
*/

% Handle grid-like arguments using custom print logic for gridoid structures.
wqs1(X):- 
    is_gridoid_arg1(X), 
    print_gridoid_arg1(X).

/* 
  @predicate into_f_arg1/3
  @desc Decompose compound terms into functor and single argument.
  @example into_f_arg1(foo(bar), F, Arg) results in F = foo, Arg = bar.
*/
% Decompose a compound term into its functor F and argument Arg.
into_f_arg1(X,F,Arg):- 
    compound(X), 
    compound_name_arguments(X,F,[Arg]), 
    compound(Arg).

/* 
  @predicate is_gridoid_arg1/1
  @desc Checks if the first argument of a term is a grid-like structure.
*/
% Check if the argument of X is a grid-like structure.
is_gridoid_arg1(X):- 
    into_f_arg1(X,_F,Arg), 
    is_gridoid(Arg).

/* 
  @predicate print_gridoid_arg1/1
  @desc Print a gridoid structure with its functor and formatted argument.
*/
% Print the functor and argument for grid-like structures.
print_gridoid_arg1(X):- 
    into_f_arg1(X,F,Arg), 
    print_gridoid_arg1(F,Arg).

% If HTML is not required, format and print the grid structure with padding.
print_gridoid_arg1(F,Arg):- 
    \+ wants_html, 
    !, 
    wots_vs(VS,wqs(Arg)), 
    writeq(F), 
    write('(`'), 
    !, 
    print_with_pad(write(VS)), 
    write('`)').

% If HTML is required, wrap the grid structure in a styled HTML span.
print_gridoid_arg1(F,Arg):- 
    wots_vs(VS,wqs(Arg)),
    with_tag_style(span,"display: inline; white-space: nowrap",(writeq(F),write('({'),!,write(VS),write('})'))).

/* 
  @predicate nl_needed/1
  @desc Check if a newline is required based on the current line position.
*/
% If the current line position is beyond N, a newline is needed.
nl_needed(N):- 
    line_position(current_output,L1), 
    L1 >= N.

/* 
  @predicate nl_now/0
  @desc Print a newline if necessary.
*/
% Handle newline based on whether HTML output is required.
nl_now :- 
    wants_html, 
    !, 
    nl_if_needed_ansi.
nl_now :- 
    nl.

/* 
  @predicate nl_if_needed/0
  @desc Prints a newline if required based on the current line formatting.
*/
% Output a newline if ANSI formatting is enabled, or if HTML is in use.
nl_if_needed :- 
    ansi_main, 
    !, 
    format('~N').
nl_if_needed :- 
    ansi_in_pre, 
    ignore((nl_needed(11),write('<br/>'))), 
    !.
nl_if_needed :- 
    wants_html, 
    !, 
    ignore((nl_needed(11),write('<br/>\n'))).
nl_if_needed :- 
    format('~N').

/* previously: nl_if_needed_ansi was skipped, logic preserved for special formatting cases */
nl_if_needed_ansi :- 
    \+ ansi_main, 
    wants_html, 
    !.
nl_if_needed_ansi :- 
    nl_if_needed.

/* 
  @predicate write_nbsp/0
  @desc Writes a non-breaking space depending on the output format (ANSI or HTML).
*/
% Output a non-breaking space based on the current output format.
write_nbsp :- 
    ansi_main, 
    !, 
    write(' ').
write_nbsp :- 
    wants_html, 
    !, 
    write('&nbsp;').
write_nbsp :- 
    write(' ').

/* 
  @predicate is_breaker/1
  @desc Determines if a term is considered a "breaker" (e.g., terms with arity >= 3).
*/
% A "breaker" is defined as a compound term with arity 3 or greater.
is_breaker(P):- 
    compound(P), 
    functor(P,_,A), 
    A >= 3.

/* 
  @predicate last_f/2
  @desc Extracts the functor and arity from a compound term.
*/
% Extract the functor F from a non-compound term.
last_f(H,F):- 
    \+ compound(H), 
    data_type(H,F).

% Extract the functor and arity from a compound term.
last_f(H,F/A):- 
    compound(H), 
    !, 
    functor(H,F,A).

/* 
  @predicate need_nl/2
  @desc Determines if a newline is needed between certain terms based on line positioning and patterns.
*/
% Insert a newline if necessary based on the structure of the terms.
need_nl(H0,[H1,H2|_]):- 
    H1 \= cc(_,_), 
    last_f(H0,F0), 
    last_f(H1,F1), 
    last_f(H2,F2), 
    F0 \== F1, 
    F1 == F2, 
    !, 
    format('~N  ').

/* previously: need_nl logic was extended to handle nested conditions; disabled sections for readability */ 

% No newline needed in this case.
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
/* File Directive: Ensure all predicates are documented and preserved for code history */

/**
 * dash_chars(+H, +C) is det.
 * 
 * Recursively prints characters 'C' for 'H' times.
 * If H < 1, the predicate cuts immediately.
 * 
 * @param H Number of repetitions.
 * @param C Character to be printed.
 * @example dash_chars(5, '-'). 
 * This will output '-----'.
 */
dash_chars(H, _) :- 
    % If H is less than 1, cut and do nothing
    H < 1, !.
dash_chars(H, C) :- 
    % For each number between 0 and H, call bformatc1 with character C
    forall(between(0, H, _), bformatc1(C)).

/* previously:  % section_break was supposed to add HTML formatting in a certain mode (wants_html) */
% The section_break predicate was originally meant to insert an HTML break, but that functionality was removed.
/**
 * section_break is det.
 * 
 * Outputs a section break, potentially useful for marking different parts of output.
 */
section_break.

/* previously: Attempt to insert a Unicode border in specific output conditions was disabled for simplicity */
/* The commented-out code for `dash_uborder_no_nl_1` would use different logic based on line positioning, 
   but now we use a simpler method that always prints a default upper border. */
dash_uborder_no_nl_1 :- 
    % Simply format and print the default Unicode upper border
    bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1 :- 
    % Use uborder to get the border style and print it with a space
    uborder(Short, Long), !, 
    bformatc1(Short), 
    bformatc1(Long), 
    write_nbsp.

/**
 * dash_uborder_no_nl(+Width) is det.
 * 
 * Prints a dashed upper border based on the given width.
 * Uses different strategies for width equal to 1 or greater.
 * 
 * @param Width The width of the upper border to print.
 * @example dash_uborder_no_nl(3). 
 * This prints a border of width 3 using the appropriate Unicode characters.
 */
dash_uborder_no_nl(1) :- !, dash_uborder_no_nl_1.
dash_uborder_no_nl(Width) :- 
    % When width is greater than 1, print spaces and Unicode characters
    WidthM1 is Width - 1, 
    uborder(Short, Long), 
    write_nbsp, 
    write(Short), 
    dash_chars(WidthM1, Long), !.
dash_uborder_no_nl(Width) :- 
    % Alternative border style
    WidthM1 is Width - 1, 
    write_nbsp, 
    bformat('\u00AF'), 
    dash_chars(WidthM1, '\u00AF\u00AF'), !.
dash_uborder_no_nl(Width) :- 
    % Another fallback option using different formatting rules
    nl_if_needed, 
    WidthM1 is Width - 1, 
    bformatc1(' \u00AF'), 
    dash_chars(WidthM1, '\u00AF\u00AF').

/**
 * dash_uborder(+Width) is det.
 * 
 * Prints a dashed upper border and ensures a newline afterward.
 * 
 * @param Width The width of the upper border.
 */
dash_uborder(Width) :- 
    % Print the border, ensuring a newline is added afterward
    nl_if_needed, 
    dash_uborder_no_nl(Width), 
    nl_now.

/**
 * uborder(?Short, ?Long) is det.
 * 
 * Determines the characters to use for borders based on stream encoding.
 * Falls back to different border styles based on whether the stream supports UTF-8.
 * 
 * @param Short The short (single character) border.
 * @param Long The long (repeated character) border.
 */
uborder('-', '--') :- 
    % If the current stream supports UTF-8 encoding, use Unicode borders
    stream_property(current_output, encoding(utf8)), !.
uborder('\u00AF', '\u00AF\u00AF') :- !.  % Use the Unicode character for the upper border

/* previously: The alternative case for non-UTF8 encodings was removed as it seems less useful nowadays */

/**
 * dash_border_no_nl(+Width) is det.
 * 
 * Prints a dashed bottom border with no newline.
 * 
 * @param Width The width of the bottom border.
 */
dash_border_no_nl(Width) :- 
    % Print a bottom border only if needed based on line position
    nl_if_needed, 
    WidthM1 is Width - 1, 
    bformatc1(' _'), 
    dash_chars(WidthM1, '__').

/**
 * dash_border(+Width) is det.
 * 
 * Prints a dashed bottom border and ensures a newline afterward.
 * 
 * @param Width The width of the bottom border.
 */
dash_border(Width) :- 
    % Call dash_border_no_nl and make sure to add a newline at the end
    !, dash_border_no_nl(Width), nl_now, !.

/**
 * functor_test_color(+TestResult, -Color) is det.
 * 
 * Maps a test result to a corresponding color.
 * 
 * @param TestResult The result of a test (e.g., pass, fail, warn).
 * @param Color The color associated with that result.
 */
functor_test_color(pass, green).
functor_test_color(fail, red).
functor_test_color(warn, yellow).

/**
 * arcdbg(+G) is det.
 * 
 * Debugging tool that prints a structured representation of the given goal or structure.
 * If the input is a virtual machine map, it prints the map. Otherwise, it uses colors to display compound terms.
 * 
 * @param G The goal or structure to debug.
 */
arcdbg(G) :- 
    % Check if G is a virtual machine map and print accordingly
    is_vm_map(G), !, 
    write_map(G, 'arcdbg').
arcdbg(G) :- 
    % If G is a compound term, get the functor and color it based on the functor's name
    compound(G), 
    compound_name_arity(G, F, _), 
    functor_test_color(F, C), 
    wots_hs(S, print(G)), 
    color_print(C, S), 
    !, 
    nl_if_needed_ansi.
arcdbg(G) :- 
    % Otherwise, just log the goal for debugging
    u_dmsg(G).

/* previously: The portray clauses for the user module were disabled for performance reasons */
/* This commented code seems to have been skipped in favor of less complex portray logic. 
   It was supposed to handle specific types of data structures like grids or objects. */

/**
 * n_times(+N, :Goal) is det.
 * 
 * Repeats a given goal N times.
 * 
 * @param N The number of times to repeat the goal.
 * @param Goal The goal to execute repeatedly.
 */
n_times(N, Goal) :- 
    % For every number between 1 and N, execute the goal, ignoring failures
    forall(between(1, N, _), ignore(Goal)).

/**
 * banner_lines(+Color, +N) is det.
 * 
 * Prints banner lines of a specified color and thickness (number of lines).
 * 
 * @param Color The color to use for the banners.
 * @param N The thickness of the banner in lines.
 */
banner_lines(Color) :- banner_lines(Color, 1).
banner_lines(Color, N) :- 
    % If HTML output is desired, format with HTML tags
    wants_html, !, 
    format('\n<hr style="border: ~wpx solid ~w">\n', [N, Color]), !.
banner_lines(Color, N) :- 
    % Otherwise, print using terminal output and colors
    must_det_ll((
        nl_if_needed,
        n_times(N, color_print(Color, '-------------------------------------------------')), nl_now,
        n_times(N, color_print(Color, '=================================================')), nl_now,
        n_times(N, color_print(Color, '-------------------------------------------------')), nl_now,
        n_times(N, color_print(Color, '=================================================')), nl_now,
        n_times(N, color_print(Color, '-------------------------------------------------')), nl_now
    )), !.

/**
 * print_sso(+A) is det.
 * 
 * Prints a specific structure 'A' unless it contains a grid-related object, in which case, 
 * it defers to a grid-printing utility.
 * 
 * @param A The structure to print.
 */
print_sso(A) :- 
    % If A is not a compound term or contains no grid-like subterms, log the message and print
    (\+ compound(A) ; \+ (sub_term(E, A), is_gridoid(E))), !, 
    u_dmsg(print_sso(A)), !.
print_sso(A) :- 
    % Otherwise, if it's a grid, print the grid and its footer
    grid_footer(A, G, W), 
    writeln(print_sso(W)), 
    print_grid(W, G), !.
/** 
 * @predicate print_sso(+A)
 * Print the structure and contents of a term after converting it to a string.
 * This predicate ensures that a new line is printed if needed, and it prints
 * each element of the term's structure.
 * 
 * @param A The input term to be printed.
 * 
 * @example 
 * ?- print_sso(some_term).
 * Output: 
 * print_sso(l([]))
 */
print_sso(A):- 
    % Ensure any needed newline is printed
    must_det_ll(( nl_if_needed, 
    % Convert the term A into a string representation
    into_ss_string(A,SS),!,
    % Decompose SS into its list structure
    SS = ss(L,Lst),
    % Print the list length or structure of L
    writeln(print_sso(l(L))),
    % Print each member of Lst
    forall(member(S,Lst),writeln(S)), 
    % Print a newline if needed
    nl_if_needed)),!.

/** 
 * @predicate var_or_number(+V)
 * Succeeds if the given term is either a variable or an integer.
 * 
 * @param V The input term to check.
 * 
 * @example 
 * ?- var_or_number(X).
 * true.
 * 
 * ?- var_or_number(5).
 * true.
 */
var_or_number(V):- 
    % Succeeds if V is a variable
    var(V),!.
var_or_number(V):- 
    % Succeeds if V is an integer
    integer(V),!.

/** 
 * @predicate find_longest_len(+SL, -L)
 * Find the longest string in a list of strings.
 * This variant defaults to using a starting comparison length of 10.
 * 
 * @param SL A list of strings to compare.
 * @param L The length of the longest string.
 * 
 * @example 
 * ?- find_longest_len(["apple", "banana", "cherry"], L).
 * L = 6.
 */
find_longest_len(SL,L):- 
    % Call helper with default initial value 10
    find_longest_len(SL,10,L),!.

/** 
 * @predicate find_longest_len(+SL, +N, -L)
 * Find the longest string in a list, comparing their lengths.
 * 
 * @param SL A list of strings.
 * @param N The current maximum length.
 * @param L The final maximum length.
 */
find_longest_len([],L,L).  % Base case: empty list, return current max length
find_longest_len([S|SS],N,L):- 
    % Find the length of string S
    print_length(S,N2),
    % Compare the current max length N with the new length N2, get max in NM
    max_min(N,N2,NM,_),
    % Recursively find the longest in the remaining strings
    find_longest_len(SS,NM,L).

/* 
 * Meta-predicate directive, specifying that print_with_pad takes a goal 
 * as an argument (i.e., a predicate that can be called within it).
 */
:- meta_predicate(print_with_pad(0)).
:- export(print_with_pad/1).  % Export this predicate for external use

/* 
 * previously: The following commented-out version of print_with_pad 
 * was likely used to calculate the line position manually, but was replaced 
 * due to more efficient or specialized logic in the active version.
 *
 * print_with_pad(Goal):-
 *   (line_position(current_output,O);O=0),!,
 *   O1 is O+1,
 *   call_w_pad(O1,Goal).
 */

/** 
 * @predicate print_with_pad(:Goal)
 * Print a padded output by adjusting the position of the text. 
 * 
 * @param Goal The goal or content to print with padding.
 */
print_with_pad(Goal):-
    % Get the current output position or default to 0
    (line_position(current_output,O);O=0),!,  
    % Increment the position for padding
    O1 is O+1,
    % Convert Goal into a string for printing
    wots(S,Goal),
    % Call print_w_pad with the new position and string
    print_w_pad(O1,S).

/** 
 * @predicate into_s(+Text, -S)
 * Converts various types of input (text or objects) into a string.
 * 
 * @param Text The input text or object to convert.
 * @param S The resulting string.
 */
into_s(Text,S):- 
    % Try to convert Text to a string, fail silently on errors
    notrace(catch(text_to_string(Text,S),_,fail)),!.
into_s(Obj,S):- 
    % Convert object Obj to string using wots_hs
    wots_hs(S,pp(Obj)),!.

/** 
 * @predicate print_w_pad(+Pad, +Text)
 * Print text with padding.
 * 
 * @param Pad The amount of padding before the text.
 * @param Text The text to print.
 */
print_w_pad(Pad,Text):- 
    % Convert Text to a string and split it by newline
    into_s(Text,S), 
    atomics_to_string(L,'\n',S) -> 
    % Map over each line, printing with padding
    my_maplist(print_w_pad0(Pad),L).

/** 
 * @predicate print_w_pad0(+Pad, +S)
 * Helper predicate to print a single line of text with padding.
 * 
 * @param Pad The amount of padding.
 * @param S The string to print.
 */
print_w_pad0(Pad,S):- 
    % Print newline if necessary
    nl_if_needed,
    % Print Pad number of dashes (' ') for padding
    dash_chars(Pad,' '), 
    % Write the string S
    write(S).

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



/* PLDoc header for `vars_to_dictation/3` predicate */
/**
 * vars_to_dictation(+Pairs, +TIn, -TOut).
 *
 * Converts a list of variable-value pairs into a dictionary structure.
 *
 * @param Pairs List of variable-value pairs (Name=Value format).
 * @param TIn Input dictionary.
 * @param TOut Output dictionary with added variables.
 *
 * @example
 * ?- vars_to_dictation([a=1, b=2], _{}, Dict).
 * Dict = _{a: 1, b: 2}.
 */

% Process the first element, ensuring it's a Name=Value pair
vars_to_dictation([Name=Value|Gotten], TIn, TOut):- !,
  my_assertion(atom(Name)),  % Ensure that Name is an atom
  vars_to_dictation(Gotten, TIn, TMid),  % Recur on the rest of the list
  to_prop_name(Name, UName),  % Convert the property name
  tio_tersify(Value, ValueT),!,  % Simplify the value
  put_dict(UName, TMid, ValueT, TOut).  % Add the variable to the dictionary

% Process cases where the NameValue is not in Name=Value format
vars_to_dictation([NameValue|Gotten], TIn, TOut):- !,
  vars_to_dictation(Gotten, TIn, TMid),  % Recur on the rest
  to_prop_name(NameValue, UName),  % Convert the name
  tio_tersify(NameValue, ValueT),!,  % Simplify the value
  put_dict(UName, TMid, ValueT, TOut).  % Add to the dictionary

% Handle compound NameValue cases (e.g., Name(Value)) by converting to Name=Value pair
vars_to_dictation([NameValue|Gotten], TIn, TOut):- compound(NameValue), compound_name_arguments(NameValue, Name, Value),!,
  vars_to_dictation([Name=Value|Gotten], TIn, TOut).  % Recur as a Name=Value pair

% Base case: when the list is empty, return the accumulated dictionary
vars_to_dictation([], T, T).

/* PLDoc header for `tio_tersify/2` predicate */
/**
 * tio_tersify(+Value, -ValueT).
 *
 * Simplifies a value if it matches specific conditions like being a grid.
 *
 * @param Value Input value.
 * @param ValueT Simplified output value.
 */

% Simplify the value if it is a grid
tio_tersify(Value, ValueT):- is_grid(Value),!, ValueT = _.  % If a grid, set ValueT to anonymous

% Otherwise, leave the value unchanged
tio_tersify(Value, Value).

/* Directive to export `copy_qq_//1` predicate */
:- export(copy_qq_//1).  % Make `copy_qq_//1` available for external modules

/* PLDoc header for `copy_qq_//1` DCG predicate */
/**
 * copy_qq_(+Codes)//
 *
 * A DCG (Definite Clause Grammar) rule to copy a list of codes.
 *
 * @param Codes List of character codes.
 */

% Base case: empty list
copy_qq_([]) --> [].

% Recursive case: copy the first code and then recur on the rest
copy_qq_([C|Cs]) --> [C], copy_qq_(Cs).

/* Directive to export `copy_qq//1` predicate */
:- export(copy_qq//1).  % Export the `copy_qq//1` DCG

/* PLDoc header for `muarc:copy_qq/1` predicate */
/**
 * muarc:copy_qq(+Atom, -Codes)//
 *
 * Converts an atom to a list of character codes using `copy_qq_//1`.
 *
 * @param Atom The input atom.
 * @param Codes The output list of character codes.
 */

muarc:copy_qq(A) --> copy_qq_(Cs), {atom_codes(A, Cs)}.  % Convert atom to list of codes

/* PLDoc header for `to_prop_name/2` predicate */
/**
 * to_prop_name(+Name, -UName).
 *
 * Converts a Name into a unified property name format.
 *
 * @param Name Input name.
 * @param UName Unified output name.
 */

% Case when Name is in Name=Value format, ignore the Value and process the Name
to_prop_name(Name=_, UName):- nonvar(Name),!, to_prop_name(Name, UName).

% If Name is a compound, extract the functor and process it
to_prop_name(Name, UName):- compound(Name), compound_name_arity(Name, F, _),!, to_prop_name(F, UName).

% Default case: convert the name to a series of case breaks, then make it atomic
to_prop_name(Name, UName):- to_case_breaks(Name, Breaks), xtis_to_atomic(Breaks, UName).

/* Additional helper predicates related to name conversion */

/* PLDoc header for `xtis_to_atomic/2` predicate */
/**
 * xtis_to_atomic(+Breaks, -Atomic).
 *
 * Converts case breaks into an atomic name.
 *
 * @param Breaks List of case breaks.
 * @param Atomic Output atomic name.
 */
xtis_to_atomic([xti(Str, upper), xti(StrL, lower)|Breaks], StrO):- 
    string_upper(Str, Str),  % Ensure the first part is upper case
    symbol_chars(Str, CharsList), append(Left, [U], CharsList),
    name(S1, Left), symbolic_list_concat([S1, '_', U, StrL], '', StrUL),!,
    xtis_to_atomic([xti(StrUL, lower)|Breaks], StrO).

% Base case for xtis_to_atomic
xtis_to_atomic([], '').
xtis_to_atomic([xti(Str, _)], Lower):- downcase_atom(Str, Lower).  % Convert to lower case

% Recursive case to concatenate parts
xtis_to_atomic([XTI|Breaks], Atomic):- 
    xtis_to_atomic([XTI], S1), xtis_to_atomic(Breaks, S2),!,
    symbolic_list_concat([S1, S2], '_', Atomic).

/* PLDoc header for `share_vars/2` predicate */
/**
 * share_vars(+Vs, +Pair).
 *
 * Ensures variables in `Vs` are shared with the given `Name=Value` pair.
 *
 * @param Vs List of variable-value pairs.
 * @param Pair A `Name=Value` pair to check for sharing.
 */

% If Name matches a variable in Vs, ensure Value is shared
share_vars(Vs, Name=Value):- 
    member(VName=VValue, Vs), VName == Name,!, 
    (Value = VValue -> true ; trace_or_throw(cant(share_vars(Vs, Name=Value)))). 

% Skip variables starting with `_` (hidden variables)
share_vars(_, Name=_):- string_concat('_', _, Name),!.

% Handle cases where the variable is missing in `Vs`
share_vars(V, Name=Value):- fbug(missing(share_vars(V, Name=Value))),!.


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
/* File Directives */
/** This file likely includes utility predicates that handle property 
 *  name-value pairs and operations on logical variables. */

% ----------------------------------------------------------------------
% Predicate: print_prop_val/1
% Purpose: Prints a property and its value in a formatted manner.
%
% @param N=V The input term where N is the property name and V is the value.
%            The property name is transformed using to_prop_name/2 before printing.
% @example
% ?- print_prop_val(name='John').
%         name = 'John'
%
% Previously: A direct printing predicate without formatting might have been used.
% ----------------------------------------------------------------------

print_prop_val(N=V) :-
    % Convert the name N to a property name P
    to_prop_name(N, P),
    % Print a tab before the property name and value, formatted as P = V
    format('~N\t\t'),
    % Print the property name and value
    print(P=V),
    % Print a newline for formatting
    nl.

% ----------------------------------------------------------------------
% Predicate: ignore_numvars/1
% Purpose: Ignores logical variables by matching the '$VAR' functor,
%          which is used internally by the Prolog engine to denote anonymous variables.
%
% @param Name The input variable name (which can be a Prolog '$VAR'(Name)).
% @example
% ?- ignore_numvars('$VAR'(X)).
% true.
%
% Previously: A more complex handling of variables might have been needed,
% but here we focus on ignoring them for simplicity.
% ----------------------------------------------------------------------

ignore_numvars(Name='$VAR'(Name)).

% ----------------------------------------------------------------------
% Dead Code: This section has been commented out because it was part of an older implementation
% or approach that has since been replaced. Keeping it for reference.
% 
% previously: print_old_version(N=V) :- write(N), write(' = '), write(V), nl.
% This older version was too simplistic and didn't format the output as well as the current approach.
%
% ----------------------------------------------------------------------

/* previously:
print_old_version(N=V) :- write(N), write(' = '), write(V), nl.
*/

