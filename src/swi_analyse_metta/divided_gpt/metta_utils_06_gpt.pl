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