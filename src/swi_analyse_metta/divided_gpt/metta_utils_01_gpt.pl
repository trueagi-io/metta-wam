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