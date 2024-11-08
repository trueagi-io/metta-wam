%:- set_prolog_flag(pfc_shared_module,baseKB).
% This line sets a Prolog flag to use the 'baseKB' as a shared module for PFC (Prolog Forward Chaining).
% It is commented out, likely for debugging purposes or because it's set elsewhere in the code.

% Predicate: must_ex/1
% Executes the goal X and catches any exceptions. If the goal succeeds, it does nothing further.
% If it fails, it traces the error and retries the goal X with debugging enabled.
% @param X The goal to be executed.
% @example must_ex(my_goal).
must_ex(X) :-
    % Try to execute X and catch any exceptions (E) if they occur.
    catch(X, E, rtrace(E)) 
    % If X succeeds, succeed silently.
    *-> true ;
    % If X fails, log the failure and trace the execution of X.
    (dmsg(failed(must_ex(X))), rtrace(X)).

% Predicate: quietly_ex/1
% Calls the goal X silently without any additional output or side effects.
% @param X The goal to be executed.
% @example quietly_ex(my_goal).
quietly_ex(X) :- call(X).

% Predicate: control_arg_types/2
% This predicate was intended to control and check argument types, but it is currently disabled.
% The 'fail' predicate causes this version to always fail. It can be re-enabled after defining into_type/3.
% @param A Input argument
% @param B Output argument
% @TODO Re-enable this once into_type/3 is defined to avoid failure.
control_arg_types(A, B) :- 
    fail, 
    once(control_arg_types1(20, [], A, B)), 
    A \== B, 
    !.

%:- listing(control_arg_types/3).
% This directive lists the predicates and clauses related to control_arg_types when executed. It is commented out for now.

% Predicate: control_arg_types1/4
% Recursive function that processes and compares types for arguments.
% @param Max The maximum depth of recursion allowed.
% @param Pre A list of previously processed arguments.
% @param A The first argument to check.
% @param B The second argument to check.
control_arg_types1(Max, _, A, B) :-
    % If Max depth is exceeded, unify A with B.
    Max < 1, !, A = B.
control_arg_types1(_, _, A, B) :-
    % If A is not a compound term, unify A with B.
    \+ compound(A), !, A = B.
control_arg_types1(_, _, A, B) :-
    % If A is a cons (non-list structure), unify A with B.
    iz_conz(A), \+ is_list(A), !, A = B.
control_arg_types1(_, _, A, B) :-
    % Use the current_predicate check_args/2 if it exists, to check arguments.
    (current_predicate(check_args/2) -> check_args(A, B) -> A \=@= B), !.
% The following line was disabled since it’s unnecessary when A is a list.
%control_arg_types1(Max, Pre, A, B) :- is_list(A), !, maplist(control_arg_types1(Max, Pre), A, B).

control_arg_types1(Max, Pre, A, B) :-
    % Process compound arguments by breaking them into their components.
    Max0 is Max - 1,
    compound_name_arguments(A, F, AA),
    length(AA, N),
    do_control_arg_types1(Max0, F/N, 1, Pre, AA, BB),
    compound_name_arguments(B, F, BB).

% Predicate: do_control_arg_types1/6
% Helper function that processes arguments recursively and performs type checking.
% @param Max Maximum depth of recursion.
% @param FofN Functor and arity.
% @param ArgN Argument number.
% @param Pre List of previous arguments.
% @param AA List of arguments for term A.
% @param BB List of arguments for term B.
do_control_arg_types1(_Max, _FofN, _ArgNp1, _Pre, [], []) :- !.
do_control_arg_types1(Max, FofN, ArgN, Pre, [A | AA], [B | BB]) :-
    % Process each argument recursively.
    do_control_1arg_type(Max, FofN, ArgN, Pre, A, B),
    ArgNp1 is ArgN + 1,
    do_control_arg_types1(Max, FofN, ArgNp1, Pre, AA, BB).

% Predicate: do_control_1arg_type/6
% Helper function that processes a single argument and performs type checking.
% @param Max Maximum depth of recursion.
% @param F Functor name.
% @param N Argument number.
% @param Pre List of previous arguments.
% @param A The first argument.
% @param B The second argument.
do_control_1arg_type(_Max, _FN, _N, _Pre, A, B) :- var(A), !, B = A.
do_control_1arg_type(_Max, F/_, N, _Pre, A, B) :-
    % Call into_type/3 based on the functor and argument position.
    arg_n_isa(F, N, ISA), into_type(ISA, A, B), !.
do_control_1arg_type(Max, FofN, _, Pre, A, B) :-
    Max0 is Max - 1,
    control_arg_types1(Max0, [FofN | Pre], A, B).

% Predicate: arg_n_isa/3
% Finds the type (ISA) for a particular argument based on its functor and argument position.
% @param F Functor name.
% @param N Argument position.
% @param ISA The type (ISA) of the argument.
% This was disabled with fail but now works by clause_b.
arg_n_isa(F, N, ISA) :- clause_b(argIsa(F, N, ISA)).

% Predicate: save_pfc_state/0
% Saves the current state of PFC-related predicates to a file.
save_pfc_state :-
    % tell(pfcState),  % This would direct output to a file, but it's commented out.
    forall(
        (pfcStateTerm(F/A), current_predicate(F/A)),
        listing(F/A)
    ),
    % told.  % Closes the output stream (also commented out).
    !.

% Predicate: pfcDoAll/1
% Calls the goal for all results, ensuring each succeeds without failing.
% @param Goal The goal to execute for each result.
pfcDoAll(Goal) :- forall(call(Goal), true).

% Predicate: pfcStateTerm/1
% Identifies which predicates represent PFC-related state.
% @param F/A The functor and arity of the predicate.
pfcStateTerm(F/A) :- pfcDatabaseTerm(F/A).
pfcStateTerm(F/A) :-
    % List of functors and arities related to PFC state.
    member(
        (F/A), [
            fcUndoMethod/2,
            fcAction/2,
            fcTmsMode/1,
            pfcQueue/1,
            pfcCurrentDb/1,
            pfcHaltSignal/1,
            pfcDebugging/0,
            pfcSelect/1,
            pfcSearch/1
        ]
    ).

% This directive is executed conditionally if the 'xref' flag or module context is active.
:- if((current_prolog_flag(xref, true) ; ('$current_source_module'(SM), 'context_module'(M), '$current_typein_module'(CM), current_prolog_flag(pfc_shared_module, BaseKB), asserta(BaseKB:'wusing_pfc'(M, CM, SM, pfc_rt))))).
:- endif.

% The following section is executed if the 'xref' flag is true (cross-referencing).
:- if(current_prolog_flag(xref, true)).
% This line defines the 'pfc_rt' module (commented out).
%:- module(pfc_rt, []).
:- endif.

% This section handles file-specific contexts for loading/unloading files.
:- if((prolog_load_context(source, File), prolog_load_context(file, File))).
% This unloads the current file if it's being loaded again.
%:- prolog_load_context(file, File), unload_file(File).
:- use_module(library(logicmoo_utils)).  % This imports utilities from the logicmoo_utils library.
:- endif.

% This section is executed if the 'xref' flag is not true (normal mode).
:- if( \+  current_prolog_flag(xref,true)).
:- current_prolog_flag(pfc_shared_module,BaseKB),
   must_ex(retract(BaseKB:'wusing_pfc'(M,CM,SM,pfc_rt))),
   nop(fbugio(BaseKB:'chusing_pfc'(M,CM,SM,pfc_rt))),
   (M==SM ->
     (nop(maybe_ensure_abox(SM)),nop((M:ain(genlMt(SM,BaseKB)))));
     nop(fbugio(BaseKB:'lusing_pfc'(M,CM,SM,pfc_rt)))),
   assert(BaseKB:'$using_pfc'(M,CM,SM,pfc_rt)),
   asserta(SM:'$does_use_pfc_mod'(M,CM,SM,pfc_rt)).
   %backtrace(200).

/*
:- multifile '$exported_op'/3.
:- dynamic '$exported_op'/3.
:- discontiguous '$exported_op'/3.