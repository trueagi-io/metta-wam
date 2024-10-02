/* 
   @predicate is_ain_pool_empty
   @desc This predicate checks if the `ain_pool` is empty. Currently, the body is missing.
   @example 
     ?- is_ain_pool_empty.
     true.
*/
is_ain_pool_empty.

/* 
   @predicate show_ain_pool
   @desc This predicate prints all properties of the `ain_pool` thread pool using `thread_pool_property/2`.
   @example 
     ?- show_ain_pool.
     show_ain_pool(pool_size(4)).
*/
show_ain_pool:- 
  /* Calls `forall/2` to iterate over every property of `ain_pool` and display it using `fmt/1`. */
  forall(thread_pool_property(ain_pool, PP), fmt(show_ain_pool(PP))).

/* 
   @predicate await_ain_pool
   @desc This predicate waits until the `ain_pool` is empty, using a combination of `repeat`, `sleep`, and `is_ain_pool_empty`.
   @example 
     ?- await_ain_pool.
     true.
*/
await_ain_pool :- 
  /* If the pool is empty, succeed, otherwise loop indefinitely, checking every 0.005 seconds. */
  is_ain_pool_empty -> true ; 
  (repeat, sleep(0.005), is_ain_pool_empty).

/* 
   @predicate ain_in_thread/1
   @param MAIN The task to be processed in the thread.
   @desc Strips the module from the input term and adds the resulting AIN to the thread pool using `call_in_thread`.
   @example 
     ?- ain_in_thread(foo:bar).
*/
ain_in_thread(MAIN) :- 
  /* Removes the module wrapper from the term */
  strip_module(MAIN, M, AIN), 
  /* Calls the module-specific predicate `pfcAdd` in a thread. */
  call_in_thread(M:pfcAdd(AIN)).

/* 
   @predicate call_in_thread/1
   @param MG The goal to be called in a thread.
   @desc Strips the module, serializes the goal, and runs it in a thread. 
   `copy_term/3`, `numbervars/3`, and `term_to_atom/2` are used to uniquely name the thread.
   @example 
     ?- call_in_thread(foo:bar).
*/
call_in_thread(MG) :- 
  /* Strips the module from the goal */
  strip_module(MG, M, G), 
  /* Non-traceable block to copy the goal and assign numbers to variables for serialization */
  notrace((
    copy_term(M:G, GG, _), 
    numbervars(GG, 0, _, [attvar(skip), singletons(true)]), 
    term_to_atom(GG, TN))),
  /* Initiates thread with unique name `TN` */
  call_in_thread(TN, M, G),
  /* Pretty-prints the call for debugging */
  dmsg_pretty(call_in_thread(TN, M, G)).

/* 
   @predicate call_in_thread/3
   @param TN The unique thread name.
   @param M The module the goal belongs to.
   @param G The goal to be executed.
   @desc Checks if the thread is already running; if not, creates a new one.
   @example 
     ?- call_in_thread(foo:bar).
*/
call_in_thread(TN, M, G) :- 
  /* Checks if a thread with alias `TN` exists */
  thread_property(_, alias(TN)), !, 
  /* If it exists, report it as already queued */
  dmsg_pretty(already_queued(M, G)).
call_in_thread(TN, M, G) :- 
  /* Calls `current_why/1` to get context for debugging purposes */
  must_ex(current_why(Why)), 
  /* Creates a new thread in `ain_pool` */
  thread_create_in_pool(ain_pool, call_in_thread_code(M, G, Why, TN), _Id, [alias(TN)]).

/* 
   @predicate call_in_thread_code/4
   @param M The module.
   @param G The goal.
   @param Why Context for debugging.
   @param TN The unique thread name.
   @desc Executes the goal in a thread and logs success, failure, or exceptions.
*/
call_in_thread_code(M, G, Why, TN) :- 
  /* Wraps the goal in the current `Why` context for debugging */
  with_only_current_why(Why, 
    /* Attempts to execute the goal and log outcomes (success, failure, or exception) */
    catch((
      M:G -> nop(dmsg_pretty(succeeded(exit, TN))) ; 
      dmsg_pretty(failed(exit, TN))
    ), 
    E, dmsg_pretty(error(E --> TN)))).

%   File: pfc
%   Author: Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: Consult system file for ensuring rules are properly loaded and managed.

/* 
   @predicate pfcVersion/1
   @param 3.0 The version of PFC system.
   @desc Declares the current version of the PFC system.
*/
pfcVersion(3.0).

/* previously: pfcFile('pfcsyntax') etc.
   These were directives to load PFC system files. Commented out as loading is now handled differently. 
   If needed in the future, uncomment for manual loading.
*/

/* previously: pfcLoad
   This predicate was responsible for loading all PFC system files but has been skipped. 
   This might be obsolete in systems where modules and files are automatically handled.
*/

/* previously: pfcFcompile
   This was used to compile PFC files but is commented out, likely due to modern systems favoring dynamic loading over pre-compilation.
*/

/* previously: call_in_thread(fbugio(call_in_thread))
   This was an example invocation of `call_in_thread` for debugging, but commented out to prevent unnecessary execution during runtime.
*/

/* 
   The following are operator definitions and syntactic sugar for PFC terms.
*/

/* 
   Declares the negation operator `~` at precedence 500.
*/
:- op(500, fx, '~').

/* 
   Declares logical implication operators for PFC rules at precedence 1050.
*/
:- op(1050, xfx, ('==>')).
:- op(1050, xfx, '<==>').
:- op(1050, xfx, ('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- dynamic(pfctmp:knows_will_table_as/2).

/* previously: The following operator definitions were used in different parts of the PFC system.
   They are preserved for potential legacy support but are commented out due to redundancy in modern usage.
*/

/* 
   @predicate will_table_as/2
   @param Stuff The term to be tabled.
   @param As The tabling mode.
   @desc This predicate declares that `Stuff` should be treated as a table with mode `As`.
   @example 
     ?- will_table_as(foo, dynamic).
*/
will_table_as(Stuff, As) :- 
  /* Checks if the tabling is already known */
  pfctmp:knows_will_table_as(Stuff, As), !.
will_table_as(Stuff, As) :- 
  /* Asserts the new tabling rule and reacts to it */
  assert(pfctmp:knows_will_table_as(Stuff, As)),
  must_ex(react_tabling(Stuff, As)), !, fail.

/* 
   @predicate react_tabling/2
   @desc Ensures the term is dynamic once it is marked for tabling.
*/
react_tabling(Stuff, _) :- 
  /* Declares the term `Stuff` as dynamic */
  dynamic(Stuff).

:- dynamic(lmconf:is_treated_like_pfc_file/1).
:- dynamic(lmconf:is_pfc_module/1).

/* 
   Conditional checks if the current file/module should be treated like a PFC file based on naming conventions or module declarations.
*/
if_pfc_indicated :- 
  /* Checks the source file extension for `.pfc` */
  source_location(F, _), (sub_string(F, _, _, _, '.pfc') -> true ; lmconf:is_treated_like_pfc_file(F)), !.
if_pfc_indicated :- 
  /* Checks if the module is a PFC module */
  prolog_load_context(module, M), lmconf:is_pfc_module(M), !.

/* 
   @predicate skip_pfc_term_expansion/1
   @desc Skips certain terms from PFC expansion, like variables or file markers.
*/
skip_pfc_term_expansion(Var) :- 
  /* Skips if the input is a variable */
  var(Var), !.
skip_pfc_term_expansion(begin_of_file). 
skip_pfc_term_expansion(end_of_file).

/* 
   Directive to export the `pfc_term_expansion/2` predicate, making it available for other modules.
*/
:- export(pfc_term_expansion/2).