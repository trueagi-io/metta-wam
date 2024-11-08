/* 
   Directive: multifile/2 allows multiple files to define facts for this predicate. 
   This is useful for predicates like is_pre_statistic/2 that can be defined in different modules.
*/
:- multifile(is_pre_statistic/2).

/* 
   Directive: dynamic/2 declares the predicate as dynamic, meaning it can be modified during execution (assert/retract).
*/
:- dynamic(is_pre_statistic/2).

/**
 * save_pre_statistic/1
 * Save a pre-statistic state if it has not been saved before.
 * 
 * @param Name Name of the statistic.
 */
save_pre_statistic(Name) :-
    /* Check if the statistic already exists, if so, do nothing. */
    is_pre_statistic(Name, _) -> true ;
    (
        /* If not, get the current statistics for the given name and store it as a pre-statistic. */
        statistics(Name, AS),
        term_number(AS, FN),
        pfcAdd_Now(is_pre_statistic(Name, FN))
    ).

/**
 * pre_statistic/2
 * Retrieve the previously saved statistic or return 0 if it does not exist.
 * 
 * @param N Name of the statistic.
 * @param V Value of the statistic.
 */
pre_statistic(N, V) :-
    /* If the pre-statistic exists, return its value. Otherwise, return 0. */
    is_pre_statistic(N, V) -> true ; V = 0.

/**
 * post_statistic/2
 * Compute the difference between the current statistic and the pre-statistic value.
 * 
 * @param N Name of the statistic.
 * @param V The computed difference value.
 */
post_statistic(N, V) :-
    /* Get the current value of the statistic. */
    statistics(N, VV),
    term_number(VV, FV),
    /* Retrieve the pre-statistic value, and calculate the difference. */
    pre_statistic(N, WV),
    V0 is FV - WV,
    /* If the difference is negative, set the result to 0, otherwise use the difference. */
    (V0 < 0 -> V = 0 ; V0 = V).

/**
 * term_number/2
 * Extract a number from a term.
 * 
 * @param T The term.
 * @param N The extracted number.
 */
term_number(T, N) :-
    /* Use sub_term/2 to find any subterm that is a number within the term T. */
    sub_term(N, T),
    number(N).

/**
 * call_match/1
 * Execute a list of goals sequentially or a single goal.
 * 
 * @param G The goal(s) to be executed.
 * @example call_match([write('Hello'), nl]).
 */
call_match([G]) :- !,
    /* If there's only one goal in the list, call it. */
    call(G).

call_match([G|GG]) :- !,
    /* Call the first goal and then recursively call the remaining goals. */
    call(G),
    call_match(GG).

call_match(G) :-
    /* If a single goal (not a list), simply call it. */
    call(G).

/**
 * 'save-space!'/2
 * Save atoms from a space to a file.
 * 
 * @param Space The space from which to get atoms.
 * @param File The file to save the atoms to.
 */
'save-space!'(Space, File) :-
    /* Use setup_call_cleanup/3 to ensure resources are properly cleaned up after use. */
    setup_call_cleanup(
        /* Open the file for writing. */
        open(File, write, Out, []),
        /* Write all atoms from the space to the file. */
        with_output_to(Out, forall(get_atoms(Space, Atom), write_src(Atom))),
        /* Ensure the file is closed after writing. */
        close(Out)
    ).

/* 
   Directive: dynamic/1 declares these predicates as dynamic.
   This allows us to modify them during the program execution.
*/
:- dynamic(repeats/1).
:- dynamic(not_repeats/1).

/**
 * assert_new/1
 * Assert a fact if it does not already exist.
 * 
 * @param P The fact to assert.
 */
assert_new(P) :-
    /* First, try to call the fact and succeed if it already exists. */
    notrace(catch(call(P), _, fail)), !,
    /* If it exists, mark it as a repeated fact. */
    assert_new1(repeats(P)).

assert_new(P) :-
    /* Otherwise, add the fact and update the assert counter. */
    pfcAdd_Now(P),
    flag(assert_new, TA, TA + 1),
    assert_new1(not_repeats(P)), !.

/**
 * retract1/1
 * Retract a fact only if it exists.
 * 
 * @param P The fact to retract.
 */
retract1(P) :-
    /* If the fact does not exist, do nothing. */
    \+ call(P), !.

retract1(P) :-
    /* Otherwise, retract the fact. */
    ignore(\+ retract(P)).

/**
 * assert_new1/1
 * Helper predicate to assert a fact if it is not already asserted.
 * 
 * @param P The fact to assert.
 */
assert_new1(P) :-
    /* If the fact is already true, do nothing. */
    \+ \+ call(P), !.

assert_new1(P) :-
    /* Otherwise, assert the fact. */
    pfcAdd_Now(P).

/* 
   Directive: dynamic/1 declares these predicates as dynamic. 
   They can be asserted and retracted at runtime.
*/
:- dynamic(fb_pred/3).
:- dynamic(mod_f_a/3).

/**
 * decl_m_fb_pred/3
 * Declare a module predicate, ensuring it's marked as dynamic.
 * 
 * @param Mod The module.
 * @param Fn The predicate name.
 * @param A The arity of the predicate.
 */
decl_m_fb_pred(Mod, Fn, A) :-
    /* If Mod is a variable, retrieve it from mod_f_a/3. */
    var(Mod), !,
    mod_f_a(Mod, Fn, A).

decl_m_fb_pred(Mod, Fn, A) :-
    /* If the predicate already exists, do nothing. */
    mod_f_a(Mod, Fn, A) -> true ;
    /* Otherwise, declare it as dynamic and assert it. */
    (dynamic(Mod:Fn/A), pfcAdd_Now(mod_f_a(Mod, Fn, A))).

/**
 * decl_fb_pred/2
 * Declare a fact as a FlyBase predicate and track its originating file.
 * 
 * @param Fn The predicate name.
 * @param A The arity of the predicate.
 */
:- dynamic(fb_pred_file/3).
decl_fb_pred(Fn, A) :-
    /* If the predicate is already declared, do nothing. */
    fb_pred(Fn, A) -> true ;
    (
        /* Otherwise, declare it as dynamic and add it to the fb_pred database. */
        dynamic(Fn/A),
        pfcAdd_Now(fb_pred(Fn, A))
    ),
    /* Optionally track the file it was loaded from. */
    ignore((nb_current(loading_file, File),
        (fb_pred_file(Fn, A, File) -> true ; pfcAdd_Now(fb_pred_file(Fn, A, File)))
    )).

/* 
   Directive: use_module/1 imports the readutil library, which provides predicates for reading input.
*/
:- use_module(library(readutil)).

/**
 * skip/1
 * Skip execution of a term. Used for commenting out code while keeping it for reference.
 * 
 * @param _ Ignored argument.
 */
skip(_) :- true. % This predicate is used to skip over certain blocks of code.

/* =============================== */
/* MeTTa Python incoming interface */
/* =============================== */

/* ============================ */
/* %%%% Atom Manipulations */
/* ============================ */

/**
 * 'clear-atoms'/1
 * Clear all atoms from the specified space.
 * 
 * @param SpaceNameOrInstance The space from which to clear atoms.
 */
'clear-atoms'(SpaceNameOrInstance) :-
    /* Send a message to the output indicating which space is being cleared. */
    dout(space, ['clear-atoms', SpaceNameOrInstance]),
    /* Find the method to clear the space based on its type, then call it. */
    space_type_method(Type, clear_space, Method),
    call(Type, SpaceNameOrInstance), !,
    dout(space, ['type-method', Type, Method]),
    call(Method, SpaceNameOrInstance).

/**
 * 'add-atom'/2
 * Add an atom to the specified space.
 * 
 * @param SpaceNameOrInstance The space to which the atom is added.
 * @param Atom The atom to add.
 */
'add-atom'(SpaceNameOrInstance, Atom) :-
    /* Find the method to add an atom based on the space type, then call it. */
    space_type_method(Type, add_atom, Method),
    call(Type, SpaceNameOrInstance), !,
    /* If the space is not a special type, log the action. */
    if_t((SpaceNameOrInstance \== '&self' ; Type \== 'is_asserted_space'),
        dout(space, ['type-method', Type, Method, SpaceNameOrInstance, Atom])),
    call(Method, SpaceNameOrInstance, Atom).

/**
 * 'add-atom'/3
 * Add an atom to an environment and return the result.
 * 
 * @param Environment The environment to add the atom to.
 * @param AtomDeclaration The atom declaration.
 * @param Result The result after adding the atom.
 */
'add-atom'(Environment, AtomDeclaration, Result) :-
    /* Evaluate the 'add-atom' command with the given arguments. */
    eval_args(['add-atom', Environment, AtomDeclaration], Result).

/**
 * 'remove-atom'/2
 * Remove an atom from the specified space.
 * 
 * @param SpaceNameOrInstance The space from which the atom is removed.
 * @param Atom The atom to remove.
 */
'remove-atom'(SpaceNameOrInstance, Atom) :-
    /* Send a message to the output indicating the atom is being removed. */
    dout(space, ['remove-atom', SpaceNameOrInstance, Atom]),
    /* Find the method to remove an atom based on the space type, then call it. */
    space_type_method(Type, remove_atom, Method),
    call(Type, SpaceNameOrInstance), !,