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