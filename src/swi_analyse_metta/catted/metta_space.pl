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

% Directive specifying the encoding format to ensure correct handling of special characters.
:- encoding(iso_latin_1).

% Directive to flush the output buffer, ensuring all output is written immediately.
:- flush_output.

% Directive to set the environment variable 'RUST_BACKTRACE' to 'full' to enable complete backtraces during debugging.
:- setenv('RUST_BACKTRACE', full).

% Directive to load the 'metta_compiler' module, which likely contains the core logic for transpiling MeTTa to Prolog.
:- ensure_loaded(metta_compiler).

% The following line is commented out since it duplicates the previous directive.
% This line was not removed to preserve the intention of loading the compiler, but since the loading occurs in the line above, this is unnecessary.
% If this were uncommented, it would reload the 'metta_compiler', which could be redundant or lead to unnecessary overhead.
% :- ensure_loaded(metta_compiler).
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
    dout(space,['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom).
% Remove Atom


'remove-atom'(Environment, AtomDeclaration, Result) :- 
    eval_args(['remove-atom', Environment, AtomDeclaration], Result).

% Add or replace an atom in the space
/** 'replace-atom'(+SpaceNameOrInstance, +Atom, +New) is det.
    Replaces an atom in the given space with a new one.

    @param SpaceNameOrInstance The identifier of the space or instance.
    @param Atom The existing atom to be replaced.
    @param New The new atom to replace the old one.

    @example
    ?- 'replace-atom'('space1', oldAtom, newAtom).
    true.
*/
'replace-atom'(SpaceNameOrInstance, Atom, New) :-
    dout(space,['replace-atom',SpaceNameOrInstance, Atom, New]),
    space_type_method(Type,replace_atom,Method), 
    call(Type,SpaceNameOrInstance),!,  % Check if the type matches
    dout(space,['type-method',Type,Method]), 
    call(Method,SpaceNameOrInstance,Atom, New).

% Replace Atom within an environment
/** 'atom-replace'(+Environment, +OldAtom, +NewAtom, -Result) is det.
    Replaces the OldAtom with NewAtom in the given environment.

    @param Environment The environment where the atom resides.
    @param OldAtom The atom to be replaced.
    @param NewAtom The new atom to insert.
    @param Result The result of the replacement operation.
*/
'atom-replace'(Environment, OldAtom, NewAtom, Result) :- 
    eval_args(['atom-replace', Environment, OldAtom, NewAtom], Result).

% Count the number of atoms in the space
/** 'atom-count'(+SpaceNameOrInstance, -Count) is det.
    Counts the number of atoms in a given space.

    @param SpaceNameOrInstance The identifier of the space or instance.
    @param Count The number of atoms in the space.

    @example
    ?- 'atom-count'('space1', Count).
    Count = 42.
*/
'atom-count'(SpaceNameOrInstance, Count) :-
    dout(space,['atom-count',SpaceNameOrInstance]),  % Log the request
    space_type_method(Type,atom_count,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance,Count),  % Execute the method
    dout(space,['type-method-result',Type,Method,Count]).  % Log the result

% Alternate method for counting atoms in an environment
/** 'atom-count'(+Environment, -Count) is det.
    Evaluates the number of atoms in an environment.

    @param Environment The environment to check.
    @param Count The number of atoms found.
*/
'atom-count'(Environment, Count):- 
    eval_args(['atom-count', Environment], Count).

% Fetch all atoms from the space
/** 'get-atoms'(+SpaceNameOrInstance, -AtomsL) is det.
    Retrieves all atoms from a space and stores them in AtomsL.

    @param SpaceNameOrInstance The identifier of the space.
    @param AtomsL The list of atoms in the space.

    @example
    ?- 'get-atoms'('space1', Atoms).
    Atoms = [atom1, atom2, atom3].
*/
'get-atoms'(SpaceNameOrInstance, AtomsL) :-
    dout(space,['get-atoms',SpaceNameOrInstance]),  % Log the request
    space_type_method(Type,get_atoms,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, AtomsL),  % Fetch the atoms
    true.  % Prevent backtracking

% Alternate method to fetch atoms in an environment
/** 'get-atoms'(+Environment, -Atoms) is det.
    Evaluates and fetches atoms from an environment.

    @param Environment The environment to check.
    @param Atoms The list of atoms in the environment.
*/
'get-atoms'(Environment, Atoms):- 
    eval_args(['get-atoms', Environment], Atoms).

% Iterate through all atoms in a space
/** 'atoms_iter'(+SpaceNameOrInstance, -Iter) is det.
    Creates an iterator for the atoms in a space.

    @param SpaceNameOrInstance The identifier of the space.
    @param Iter The iterator for atoms.

    @example
    ?- 'atoms_iter'('space1', Iter).
    Iter = iterator_handle.
*/
'atoms_iter'(SpaceNameOrInstance, Iter) :-
    dout(space,['atoms_iter',SpaceNameOrInstance]),  % Log the request
    space_type_method(Type,atoms_iter,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, Iter),  % Create the iterator
    dout(space,['type-method-result',Type,Method,Iter]).  % Log the result

% Match atoms in the space to a template
/** 'atoms_match'(+SpaceNameOrInstance, -Atoms, +Template, +Else) is det.
    Matches all atoms in a space to a template, with a fallback option.

    @param SpaceNameOrInstance The identifier of the space.
    @param Atoms The matched atoms.
    @param Template The template to match against.
    @param Else The fallback in case no match is found.
*/
'atoms_match'(SpaceNameOrInstance, Atoms, Template, Else) :-
    space_type_method(Type,atoms_match,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, Atoms, Template, Else),  % Perform the match
    dout(space,['type-method-result',Type,Method,Atoms, Template, Else]).  % Log the result

% Query atoms in a space
/** 'space_query'(+SpaceNameOrInstance, +QueryAtom, -Result) is det.
    Queries atoms in a space to find matching results.

    @param SpaceNameOrInstance The identifier of the space.
    @param QueryAtom The atom to query.
    @param Result The result of the query.
*/
'space_query'(SpaceNameOrInstance, QueryAtom, Result) :-
    space_type_method(Type,query,Method), 
    call(Type,SpaceNameOrInstance),!,  % Ensure we can call the method
    call(Method,SpaceNameOrInstance, QueryAtom, Result),  % Perform the query
    dout(space,['type-method-result',Type,Method,Result]).  % Log the result

% Substitute a pattern with a template in a space
/** subst_pattern_template(+SpaceNameOrInstance, +Pattern, +Template) is det.
    Substitutes a pattern with a template in the given space.

    @param SpaceNameOrInstance The identifier of the space.
    @param Pattern The pattern to search for.
    @param Template The template to replace the pattern with.

    @example
    ?- subst_pattern_template('space1', patternX, templateY).
    true.
*/
subst_pattern_template(SpaceNameOrInstance, Pattern, Template) :-
    dout(space,[subst_pattern_template,SpaceNameOrInstance, Pattern, Template]),  % Log the operation
    'atoms_match'(SpaceNameOrInstance, Pattern, Template, []).  % Perform the substitution

/*
Previously: space_query_vars(+SpaceNameOrInstance, +Query, -Vars)
The following code was skipped as it calls a space-specific function, likely outside the current context. The predicate fetch_or_create_space may rely on specific implementations.

space_query_vars(SpaceNameOrInstance, Query, Vars) :- is_as_nb_space(SpaceNameOrInstance),!,
    fetch_or_create_space(SpaceNameOrInstance, Space),
    call_metta(Space,Query,Vars).
*/

% Register space assertions
/** was_asserted_space(+Space) is det.
    Registers that a space was asserted.

    @param Space The space that was asserted.
*/
:- dynamic(was_asserted_space/1).

was_asserted_space('&self').  % Self space assertion
was_asserted_space('&stdlib').  % Standard library space assertion
was_asserted_space('&corelib').
was_asserted_space('&flybase').
/*
was_asserted_space('&attentional_focus').
was_asserted_space('&belief_events').
was_asserted_space('&goal_events').
was_asserted_space('&tempset').
was_asserted_space('&concepts').
was_asserted_space('&belief_events').
*/
is_asserted_space(X):- was_asserted_space(X).
is_asserted_space(X):-          \+ is_as_nb_space(X), \+ py_named_space(X),!.

is_python_space_not_prolog(X):- \+ is_as_nb_space(X), \+ is_asserted_space(X).

:- dynamic(is_python_space/1).

:- dynamic(py_named_space/1).

%py_named_space('&self').
%py_named_space('&vspace').
% Function to check if an atom is registered as a space name
:- dynamic is_registered_space_name/1.




is_as_nb_space('&nb').
is_as_nb_space(G) :-
    % Verifies if it's a valid notebook space or a registered space name
    is_valid_nb_space(G)
    -> true ;
    is_registered_space_name(G),
    nb_current(G, S),
    is_valid_nb_space(S).

% Predicate to check if a space is an NB (Notebook) space, ensures it's non-variable
% @example is_nb_space('&nb').
is_nb_space(G) :-
    nonvar(G),
    is_as_nb_space(G).



% ============================
% %%%% Pattern Matching
% ============================
% Pattern Matching with an else branch
%'match'(Environment, Pattern, Template, ElseBranch, Result):-
%  eval_args(['match', Environment, Pattern, Template, ElseBranch], Result).
% Pattern Matching without an else branch
'match'(Environment, Pattern, Template, Result):-
  eval_args(['match', Environment, Pattern, Template], Result).
%'match'(_Environment, Pattern, Template, Result):- callable(Pattern),!, call(Pattern),Result=Template.
%'match'(_Environment, Pattern, Template, Result):- !, is_True(Pattern),Result=Template.


'new-space'(Space):- gensym('hyperon::space::DynSpace@_',Name),
   fetch_or_create_space(Name, Space).

:- dynamic(is_python_space/1).
% ===============================
% MeTTa Python incoming interface
% ===============================

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).

space_type_method(is_as_nb_space, new_space, init_space).
space_type_method(is_as_nb_space, clear_space, clear_nb_atoms).
space_type_method(is_as_nb_space, add_atom, add_nb_atom).
space_type_method(is_as_nb_space, remove_atom, remove_nb_atom).
space_type_method(is_as_nb_space, replace_atom, replace_nb_atom).
space_type_method(is_as_nb_space, atom_count, atom_nb_count).
space_type_method(is_as_nb_space, get_atoms, get_nb_atoms).
% previously: This method is not required anymore as it might have been redundant
% space_type_method(is_as_nb_space, get_atoms, arg(1)).
space_type_method(is_as_nb_space, atom_iter, atom_nb_iter).
% previously: Commented out space query method which might be used in the future
% space_type_method(is_as_nb_space, query, space_nb_query).

% Clears all atoms in a given space (either by name or instance)
% @example clear_nb_atoms('&nb').
clear_nb_atoms(SpaceNameOrInstance) :-
    % Fetch or create the space instance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Clear the atoms by setting the space's atom list to an empty list
    nb_setarg(1, Space, []).

% Adds an atom to a space (either by name or instance)
% @example add_nb_atom('&nb', my_atom).
add_nb_atom(SpaceNameOrInstance, Atom) :-
    % Fetch or create the space instance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the current list of atoms in the space
    arg(1, Space, Atoms),
    % Add the new atom to the list
    NewAtoms = [Atom | Atoms],
    % Update the space with the new atom list
    nb_setarg(1, Space, NewAtoms).

% Counts the number of atoms in a space (either by name or instance)
% @example atom_nb_count('&nb', Count).
atom_nb_count(SpaceNameOrInstance, Count) :-
    % Fetch or create the space instance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Calculate the number of atoms in the space
arg(1, Space, Atoms),
% Get the length of the Atoms list.
length(Atoms, Count).

/* 
    PLDoc header for removing an atom from a space.
    @param SpaceNameOrInstance The name or instance of the space
    @param Atom The atom to be removed from the space
    @example 
      ?- remove_nb_atom(my_space, a).
      true.
*/
% Remove an atom from a space
remove_nb_atom(SpaceNameOrInstance, Atom) :-
    % Fetch or create a space based on SpaceNameOrInstance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the atoms list from the space
    arg(1, Space, Atoms),
    % Select and remove the Atom from the list
    select(Atom, Atoms, UpdatedAtoms),
    % Update the space with the modified list of atoms
    nb_setarg(1, Space, UpdatedAtoms).

/* 
    PLDoc header for fetching all atoms from a space.
    @param SpaceNameOrInstance The name or instance of the space
    @param Atoms The list of atoms found in the space
    @example 
      ?- get_nb_atoms(my_space, Atoms).
      Atoms = [a, b, c].
*/
% Fetch all atoms from a space
get_nb_atoms(SpaceNameOrInstance, Atoms) :-
    % Fetch or create a space based on SpaceNameOrInstance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the atoms from the space
    arg(1, Space, Atoms).

/* 
    PLDoc header for replacing an atom in a space.
    @param SpaceNameOrInstance The name or instance of the space
    @param OldAtom The atom to be replaced
    @param NewAtom The new atom to insert
    @example 
      ?- replace_nb_atom(my_space, a, x).
      true.
*/
% Replace an atom in the space
replace_nb_atom(SpaceNameOrInstance, OldAtom, NewAtom) :-
    % Fetch or create a space based on SpaceNameOrInstance
    fetch_or_create_space(SpaceNameOrInstance, Space),
    % Retrieve the atoms list from the space
    arg(1, Space, Atoms),
    % Check if the OldAtom matches any in the space
    ( (select(Found, Atoms, TempAtoms),OldAtom=@=Found)
    % If found, replace OldAtom with NewAtom
    ->  NewAtoms = [NewAtom | TempAtoms],
        % Update the space with the new list of atoms
        nb_setarg(1, Space, NewAtoms)
    ;   false  % If OldAtom is not found, fail the operation
    ).

/* 
    PLDoc header for validating if a term is a space.
    @param Space The term to check
    @example 
      ?- is_valid_nb_space(my_space).
      true.
*/
% Confirm if a term represents a space by checking its structure
is_valid_nb_space(Space):- compound(Space),functor(Space,'Space',_).

/* 
    PLDoc header for finding the original name of a space.
    @param Space The space instance
    @param Name The name of the space
    @example 
      ?- space_original_name(SpaceInstance, Name).
      Name = my_space.
*/
% Find the original name of a given space
space_original_name(Space, Name) :-
    % Check if the name is a registered space name
    is_registered_space_name(Name),
    % Fetch the space instance for the given name
    nb_current(Name, Space).

/* 
    PLDoc header for initializing a new space.
    @param Name The name of the space to initialize
    @example 
      ?- init_space(my_new_space).
      true.
*/
% Register and initialize a new space
init_space(Name) :-
    % Create a new space instance with an empty list of atoms
    Space = 'Space'([]),
    % Register the name as a valid space name
    asserta(is_registered_space_name(Name)),
    % Set the space instance in a non-backtrackable store
    nb_setval(Name, Space).

/* 
    PLDoc header for fetching or creating a space.
    @param NameOrInstance The name or instance of the space
    @param Space The space instance fetched or created
    @example 
      ?- fetch_or_create_space(my_space).
      true.
*/

fetch_or_create_space(Name):- fetch_or_create_space(Name,_).
% Fetch an existing space or create a new one
fetch_or_create_space(NameOrInstance, Space) :-
    (   atom(NameOrInstance)
    ->  (is_registered_space_name(NameOrInstance)
        ->  nb_current(NameOrInstance, Space)
        ;   init_space(NameOrInstance),
            nb_current(NameOrInstance, Space))
    ;   is_valid_nb_space(NameOrInstance)
    ->  Space = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_space(Space).





% Match Pattern in Space and produce Template
'match'(Space, Pattern, Template) :-
    % Fetch the atoms from the space
    'get-atoms'(Space, Atoms),
    % Match the pattern within the atoms and generate the template
    'match-pattern'(Atoms, Pattern, Template).

/* previously: had a more complex pattern matcher but simplified */

% Simple pattern match
'match-pattern'([], _, []).  % Base case: empty list, no match.
'match-pattern'([H |_T], H, H) :- !.  % Match head of the list with the pattern.
'match-pattern'([_H| T], Pattern, Template) :- 
    % Recursively match the pattern in the rest of the list
    'match-pattern'(T, Pattern, Template).

% previously: commented out as this may relate to Python interface (not needed in current use case)
/*
% is_python_space(X):- python_object(X).
*/

% previously: alternative space handling method using Python, not needed in this context

% Ensure space using Python method with a catch for failure.
ensure_space(X,Y):- catch(ensure_space_py(X,Y),_,fail),!.
% Default case: always fail if the above doesn't work
ensure_space(_N,_V):- fail.

/* 
    File directive for handling debugging and output related clauses.
*/

/* PLDoc headers and skipped blocks explained above for predicates. */





% ===============================
% Clause Database interface
% ===============================
%dout(space,Call):- skip(Call).
if_metta_debug(Goal):- getenv('VSPACE_VERBOSE','2'),!,ignore(call(Goal)).
if_metta_debug(_):-!.
if_metta_debug(Goal):- !,ignore(call(Goal)).
dout(_,_):-!.
dout(W,Term):- notrace(if_metta_debug((format('~N; ~w ~@~n',[W,write_src(Term)])))).

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_asserted_space,new_space,init_space).
space_type_method(is_asserted_space,clear_space,clear_nb_atoms).
space_type_method(is_asserted_space,add_atom,metta_assertdb_add).
space_type_method(is_asserted_space,remove_atom,metta_assertdb_rem).
space_type_method(is_asserted_space,replace_atom,metta_assertdb_replace).


% PLDoc Header for space_type_method/3
% Defines methods associated with different types of spaces.
% 
% @param Type The type of space (e.g., 'is_asserted_space').
% @param Action The action to be performed in this space.
% @param Method The method associated with the action.
space_type_method(is_asserted_space, atom_count, metta_assertdb_count).
space_type_method(is_asserted_space, get_atoms, metta_assertdb_get_atoms).
space_type_method(is_asserted_space, atom_iter, metta_assertdb_iter).

% The query method is commented out because it is not implemented yet.
% This line represents a mapping for a query action in asserted space
% but the corresponding method 'space_nb_query' is either unimplemented or deprecated.
% space_type_method(is_asserted_space, query, space_nb_query).

% Declaring a predicate that might be dynamically modified.
% @comment previously: The dynamic directive is commented out to avoid unexpected dynamic behavior.
% :- dynamic(for_metta/2).

% Unused for_metta/2 rule
% @comment previously: This clause is skipped because it isn't called. It checks 
% if a given functor matches the predicate in the knowledge base (KB) and calls it.
% for_metta(_,T):- fb_pred(F,A), functor(T,F,A), call(T).

% Lists all asserted atoms in the given knowledge base (KB).
% @param KB The knowledge base whose atoms are being listed.
% Uses 'listing/1' to print all terms related to the KB in the metta_atom_asserted predicate.
metta_assertdb_ls(KB):-
    AMA = metta_atom_asserted, % Define the predicate for asserted atoms.
    decl_m_fb_pred(user, AMA, 2), % Ensure the predicate is declared.
    MP =.. [AMA, KB, _], % Create a metaterm representing 'metta_atom_asserted(KB, _)'. 
    listing(MP). % List all matching terms.

% Adds a new atom to the asserted knowledge base (KB).
% @param KB The knowledge base.
% @param AtomIn The atom to be added to the knowledge base.
metta_assertdb_add(KB, AtomIn):-
    must_det_ll((subst_vars(AtomIn, Atom), % Replace any variables in AtomIn.
        AMA = metta_atom_asserted, % Use the asserted atom predicate.
        decl_m_fb_pred(user, AMA, 2), % Declare the predicate for KB and atoms.
        MP =.. [AMA, KB, Atom], % Build the predicate term.
        assert_new(MP))). % Assert the new fact.

% Removes an atom from the knowledge base (KB).
% @param KB The knowledge base.
% @param Old The atom to be removed.
metta_assertdb_rem(KB, Old):- metta_assertdb_del(KB, Old). % Remaps to deletion.

% Deletes an atom from the knowledge base (KB).
% @param KB The knowledge base.
% @param Atom The atom to be deleted.
% Deletes the atom if it exists using 'erase/1' after copying it.
metta_assertdb_del(KB, Atom):- 
    subst_vars(Atom, Old), % Replace any variables in the atom.
    decl_m_fb_pred(user, metta_atom_asserted, 2), % Declare the predicate.
    MP = metta_atom(KB, Old), % Construct the term.
    copy_term(MP, Copy), clause(MP, true, Ref), MP =@= Copy, !, % Ensure identical copy.
    erase(Ref). % Remove the clause reference.

% Replaces an old atom with a new one in the knowledge base (KB).
% @param KB The knowledge base.
% @param Old The old atom to be replaced.
% @param New The new atom to replace the old one.
metta_assertdb_replace(KB, Old, New):- 
    metta_assertdb_del(KB, Old), % First delete the old atom.
    metta_assertdb_add(KB, New). % Then add the new one.

% Provides the count of atoms for a specific knowledge base.
% This particular clause checks if the knowledge base is already loaded.
% @param Self The knowledge base.
% @param Count The number of atoms in the knowledge base.
atom_count_provider(Self, Count):-
    user:loaded_into_kb(Self, Filename), % Check if the KB is loaded.
    once(user:asserted_metta_pred(Mangle, Filename)), % Retrieve mangled predicates.
    mangle_iz(Mangle, Iz), % Generate another mangled version.
    member(P, [Mangle, Iz]), % Choose one of the mangled forms.
    between(2, 8, Arity), % Iterate over possible arities.
    functor(Data, P, Arity), % Create a term for the predicate.
    predicate_property(Data, number_of_clauses(CC)), % Retrieve the number of clauses.
    predicate_property(Data, number_of_rules(RC)), % Retrieve the number of rules.
    Count is CC - RC. % Calculate the number of atoms.

% Provides the count of atoms for a specific knowledge base.
% This clause uses the number of clauses and rules from the metta_asserted predicate.
% @param KB The knowledge base.
% @param Count The number of atoms in the knowledge base.
atom_count_provider(KB, Count):- 
    must_det_ll((
        AMA = metta_atom_asserted, % Use the asserted atom predicate.
        decl_m_fb_pred(user, AMA, 2), % Declare the predicate.
        MP =.. [AMA, KB, _], % Construct a predicate for the KB.
        predicate_property(MP, number_of_clauses(SL2)), % Get clause count.
        predicate_property(MP, number_of_rules(SL3)), % Get rule count.
        full_atom_count(SL1), % Fetch additional atom counts.
        Count is SL1 + SL2 - SL3)), !. % Sum counts to get total.

% Counts all atoms in the knowledge base (KB).
% @param KB The knowledge base.
% @param Count The total number of atoms.
metta_assertdb_count(KB, Count):-
    findall(C, atom_count_provider(KB, C), CL), % Find counts from all providers.
    sumlist(CL, Count). % Sum the counts.

% @comment previously: This commented out clause used an alternate counting method 
% using for_metta/2 to gather all atoms, but it was skipped for efficiency.
% metta_assertdb_count(KB, Count):- writeln(metta_assertdb_count_in(KB, Count)),
%     findall(Atom, for_metta(KB, Atom), AtomsL), length(AtomsL, Count),
%     writeln(metta_assertdb_count_out(KB, Count)).

% Iterates over all atoms in the knowledge base (KB).
% @param KB The knowledge base.
% @param Atoms The atoms being iterated.
metta_assertdb_iter(KB, Atoms):-
    MP =.. [metta_atom, KB, Atoms], % Construct the metta_atom term.
    call(MP). % Call the predicate to iterate over atoms.

% Binds variables in a query to a knowledge base (KB) and returns them.
% @param KB The knowledge base.
% @param Query The query to be matched.
% @param Vars The variables to bind.
% @param VarNames The names of the variables.
metta_iter_bind(KB, Query, Vars, VarNames):-
    term_variables(Query, QVars), % Get the variables from the query.
    align_varnames(VarNames, Vars), % Align variable names.
    TV = dout(space, ['match', KB, Query, QVars, Vars, VarNames]), % Build a debug term.
    ignore(QVars = Vars), % Ignore non-bound variables.
    \+ \+ (writeq(av=TV), nl), % Write the aligned variables.
    space_query_vars(KB, Query, TF), % Execute the query.
    TF \= 'False'. % Ensure the query is successful.

% Queries the space and retrieves variables.
% @param KB The knowledge base.
% @param Query The query to be executed.
% @param Vars The variables to retrieve.
space_query_vars(KB, Query, Vars):- 
    is_asserted_space(KB), !, % Check if the KB is asserted.
    decl_m_fb_pred(user, metta_atom_asserted, 2), % Declare the predicate.
    call_metta(KB, Query, Vars), % Call the query with variables.
    dout('RES', space_query_vars(KB, Query, Vars)). % Output the result.

% Retrieves atoms from the knowledge base (KB).
% @param KB The knowledge base.
% @param Atom The atom to retrieve.
metta_assertdb_get_atoms(KB, Atom):- 
    metta_atom(KB, Atom). % Call the metta_atom predicate.

/*

%metta_assertdb_iter_bind(KB,Query,Template,AtomsL):-
decl_m_fb_pred(user,metta_atom_asserted,2), findall(Template,metta_atom(KB,Query),AtomsL).




metta_assertdb_iter_bind(KB,Query,Vars):-
  ignore(term_variables(Query,Vars)),
  print(metta_assertdb(['match',KB,Query,Vars])),nl,
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,Query],

  (MP*->true;call_metta_assertdb(KB,Query,Vars)),
  metta_assertdb('RES',metta_assertdb_iter_bind(KB,Query,Vars)).
%metta_assertdb_iter_bind(KB,Atom,Template):- metta_assertdb_stats, findall(Template,metta_assertdb_iter(KB,Atom),VarList).

metta_assertdb_iter_bind(KB,Atoms,Vars):-
  metta_assertdb_stats,
  term_variables(Atoms,AVars),
  metta_assertdb_iter(KB,Atoms), ignore(AVars = Vars).
*/





/**
 * align_varnames/2
 * 
 * Aligns variable names to a list of variables.
 * 
 * @param VarNames List of variable names to align
 * @param Vars List of aligned variables
 */
align_varnames(VarNames, Vars) :-
    % Convert the list of variable names to a set to eliminate duplicates
    list_to_set(VarNames, NameSet),

    % Merge the named variables
    merge_named_vars(NameSet, VarNames, Vars).

/**
 * merge_named_vars/3
 * 
 * Merges a set of named variables with their corresponding values.
 * 
 * @param NameSet Set of names
 * @param VarNames List of variable names
 * @param Vars List of variable values
 */
merge_named_vars([], _VarNames, _Vars) :- !.  % Base case, do nothing when the name set is empty
merge_named_vars([N | NameSet], VarNames, Vars) :-
    % Merge the named variable N with its value
    merge_named(N, _V, VarNames, Vars),

    % Recursively process the remaining names in the set
    merge_named_vars(NameSet, VarNames, Vars).

/* previously: 
 * The following line was part of the original logic but was commented out due to optimization reasons. 
 * Keeping it for reference:
 * 
 * merge_named_vars(_,_,_).
 */

/**
 * merge_named/4
 * 
 * Merges a named variable with its value in a list of variables.
 * 
 * @param N Name of the variable to merge
 * @param V Value of the variable
 * @param VarNames List of variable names
 * @param Vars List of variable values
 */
merge_named(_, _, [], []) :- !.  % Base case, no more names or values to process
merge_named(N, V, [N | VarNames], [V | Vars]) :-
    % Recursively merge the rest of the variables
    merge_named(N, V, VarNames, Vars).

/**
 * call_metta/3
 * 
 * Calls a metta query either directly or through the Python SWI-Prolog interface.
 * 
 * @param KB Knowledge base to use (may be ignored)
 * @param Query Query to be executed
 * @param _Vars Unused list of variables
 */
call_metta(KB, Query, _Vars) :-
    % If the query is already defined in the metta_atom, call it directly
    metta_atom(KB, Query).
call_metta(_KB, Query, _Vars) :-
    % Otherwise, translate the query for the Python SWI-Prolog interface and call it
    metta_to_pyswip([], Query, Call), !,
    % Execute the translated Python call
    user:call(Call).

/**
 * metta_to_pyswip/3
 * 
 * Translates a Prolog query into a format that can be executed by Python SWI-Prolog.
 * 
 * @param PS Prefix stack (for context)
 * @param Query The Prolog query to translate
 * @param Call The resulting translated call
 */
metta_to_pyswip(_PS, Query, Call) :-
    % If the query is a variable, return it directly
    var(Query), !, Call = Query.
metta_to_pyswip(_PS, Query, Call) :-
    % If the query is not compound (e.g., an atom or number), return it directly
    \+ compound(Query), !, Call = Query, !.
metta_to_pyswip(PS, Query, Call) :-
    % If the query is a list, translate the head and tail
    is_list(Query), Query = [Q | Uery], !, cmpd_to_pyswip(PS, Q, Uery, Call).
metta_to_pyswip(PS, Query, Call) :-
    % Otherwise, break the query into its functor and arguments and translate them
    Query =.. [Q | Uery], cmpd_to_pyswip(PS, Q, Uery, Call).

/**
 * cmpd_to_pyswip/4
 * 
 * Translates a compound Prolog term into a format that can be executed in Python.
 * 
 * @param PS Prefix stack (for context)
 * @param Q Functor of the compound term
 * @param Uery Arguments of the compound term
 * @param Call The resulting translated call
 */
cmpd_to_pyswip(PS, Q, Uery, Call) :-
    % If the functor is an atom, translate each argument and reconstruct the term
    atom(Q), maplist(metta_to_pyswip([Q | PS]), Uery, Cery), Call =.. [Q | Cery].
cmpd_to_pyswip(PS, "and", Uery, Call) :-
    % Special case for 'and', translate to a conjunction of arguments
    maplist(metta_to_pyswip(PS), Uery, Args), list_to_conjuncts(Args, Call).

/**
 * show-metta-def/2
 * 
 * Displays the source code for a metta definition.
 * 
 * @param Pred Predicate whose definition is to be shown
 * @param [] Empty list as placeholder for future arguments
 */
'show-metta-def'(Pred, []) :-
    % Retrieve the source code for the given predicate
    'get-metta-src'(Pred, [_ | SrcL]),

    % Write each source line to the output
    maplist(write_src_nl, SrcL).

/**
 * write_src_nl/1
 * 
 * Writes a source line with newlines before and after.
 * 
 * @param Src Source line to write
 */
write_src_nl(Src) :-
    format('~N'), write_src(Src), format('~N').

/* previously:
 * The following commented-out version included the AtomDef wrapper, which was removed 
 * for simplicity and direct handling:
 *
 * 'get-metta-src'(Pred,[Len|SrcL]):- 
 *     findall(['AtomDef',Src], 'get-metta-src1'(Pred, Src), SrcL), 
 *     length(SrcL, Len).
 */

/**
 * get-metta-src/2
 * 
 * Retrieves the source code for a metta predicate.
 * 
 * @param Pred Predicate whose source code is being retrieved
 * @param [Len|SrcL] The length of the source list and the list of source code lines
 */
'get-metta-src'(Pred, [Len | SrcL]) :-
    % Collect all source code lines for the given predicate
    findall(Src, 'get-metta-src1'(Pred, Src), SrcL),

    % Get the length of the source list
    length(SrcL, Len).

/**
 * get-metta-src1/2
 * 
 * Retrieves a single source line for a metta predicate.
 * 
 * @param Pred Predicate being queried
 * @param Src Source line corresponding to the predicate
 */
'get-metta-src1'(Pred, Src) :-
    % Get the current working space for the predicate
    current_self(Space),

    % Find the atom definition in the space
    metta_atom(Space, F, A, List),

    % Return the source line if it matches the predicate
    once((sub_var(Pred, A) -> Src = [F, A, List] ; sub_var(Pred, F) -> Src = [F, A | List])).

/* previously:
 * This code block defined a quine-like structure, which is preserved but commented 
 * because it might not be necessary for all use cases:
 *
 * 'AtomDef'(X, ['AtomDef', X]).
 */

/**
 * sort_on/4
 * 
 * Sorts elements based on a custom comparison predicate.
 * 
 * @param C Custom comparator
 * @param R Result of the comparison (=, <, or >)
 * @param A First element
 * @param B Second element
 */
sort_on(C, R, A, B) :-
    % If A and B are the same, they are equal
    (A == B -> R = (=) 
    % Otherwise, compare them using the custom comparator
    ; must_det_ll((call(C, A, AA), call(C, B, BB), !, compare(R, AA + A, BB + B)))), !.

/**
 * tokens/2
 * 
 * Tokenizes an input string into a sorted list of tokens.
 * 
 * @param X Input string
 * @param VL The last (longest) token in the sorted list
 */
tokens(X, VL) :-
    % Convert the string to its unaccented form
    unaccent_atom(X, A), !,

    % Find all tokens using the available tokenizers
    findall(E, (is_tokenizer(T), call(T, A, E)), L),

    % Sort the tokens by length
    predsort(sort_on(length_fw_len), L, S),

    % Get the last token in the sorted list
    last(S, VL).

/**
 * length_fw_len/2
 * 
 * Custom sorting predicate based on the length of the token and the remaining list.
 * 
 * @param [W|List] Token and its list
 * @param L+WL Combined length of the list and token
 */
length_fw_len([W | List], L + WL) :-
    % Get the length of the list
    length(List, L),

    % Get the length of the token
    atom_length(W, WL).

/**
 * print_token_args/0
 * 
 * Prints arguments and their tokenized forms.
 */
print_token_args :-
    % Rebuilds the environment to ensure tokens are fresh
    make,

    % For each argument X, get its tokens and write the result
    fb_arg(X), tokens(X, A0),
    exclude(is_dash, A0, A), tterm(A, AT),
    writeq(X), write('    '), writeq(AT), write('  '), write_src(A), nl, fail.

/**
 * is_dash/1
 * 
 * Checks if a token is a dash character.
 * 
 * @param Char Character to check
 */
is_dash('_').
is_dash('-').

/**
 * tterm/2
 * 
 * Converts a list of terms into a structured term.
 * 
 * @param [A] Single term case
 * @param A The result when there is only one term
 */
tterm([A], A) :- !.

/**
 * tterm/2 (complex case)
 * 
 * Converts a list of terms into a structured term with functor and arguments.
 * 
 * @param [A,':',B|M] Functor and arguments to be structured
 * @param BA The resulting structured term
 */
tterm([A, ':', B | M], BA) :-
    % If the first element is an atom, construct the term
    atom(A), !, BA =.. [A, B | M].

/**
 * tterm/2 (fallback case)
 * 
 * Handles cases where a term has more than two elements.
 * 
 * @param [A, B | M] Functor and arguments
 * @param BA The resulting structured term
 */
tterm([A, B | M], BA) :-
    % If the second element is an atom, construct the term
    atom(B), !, BA =.. [B, A | M].

/**
 * tterm/2 (general case)
 * 
 * Converts a list of terms into a structured term.
 * 
 * @param [A | B] List of terms to convert
 * @param BA The resulting structured term
 */
tterm([A | B], BA) :-
    % If the first element is an atom, construct the term
    atom(A), !, BA =.. [B | A].
% tterm/2: Unifies two terms.
% @param A - A term
% @param A - The same term, to ensure unification
% @example tterm(X, X).  % X will unify with X
tterm(A, A).

% Define different tokenizer predicates.
% These tokenizers are used to transform input into specific formats.
% Currently active tokenizers are listed, with one commented out.

% Predicate for tokenizer that converts input into a list.
is_tokenizer(into_list).

% Predicate for tokenizer that breaks atoms based on case.
is_tokenizer(to_case_break_atoms).

% Predicate for tokenizer that stems words from atoms.
is_tokenizer(atom_to_stem_list).

% Predicate for tokenizer that tokenizes an atom into words or subcomponents.
is_tokenizer(tokenize_atom).

% Predicate for tokenizer based on the Double Metaphone algorithm (currently skipped).
% This is commented out likely because it's not currently needed, but kept for future use.
% is_tokenizer(double_metaphone).

% is_an_arg_type/2: Checks if the first argument is of a recognized type.
% Uses flybase_identifier to identify the argument type.
% @param S - The argument to check
% @param T - The recognized type
% @example is_an_arg_type('FBgn0001234', Type).
is_an_arg_type(S, T) :- 
    flybase_identifier(S, T), 
    !.

% has_type/2: Checks if the first four characters of a string match a known identifier.
% It ensures the identifier starts at the beginning and that there is more after.
% @param S - The string to check
% @param Type - The resulting type if found
% @example has_type('FBgn0001234', Type).
has_type(S, Type) :- 
    sub_atom(S, 0, 4, Aft, FB),
    flybase_identifier(FB, Type), 
    !, 
    Aft > 0.

% call_sexpr/1: Calls an S-expression and writes it once, formatted with writeq_nl.
% @param S - The S-expression to call and write
% @example call_sexpr((foo(bar))).
call_sexpr(S) :- 
    once_writeq_nl(call_sexpr(S)).

% call_sexpr/3: This version of call_sexpr was previously defined, but skipped.
% It's commented out because it's either incomplete or replaced by a simpler version.
% call_sexpr(Space, Expr, Result):-

% Declare dynamic predicate fb_pred/2 for FlyBase predicates.
% This directive allows fb_pred/2 to be modified dynamically during runtime.
:- dynamic(fb_pred/2).

% full_atom_count/1: Gets the total count of atoms.
% If total_loaded_atoms flag exists, it retrieves the count.
% Otherwise, it calculates the sum of all metta_stats results.
% @param SL - The total count of atoms
% @example full_atom_count(Count).
full_atom_count(SL) :- 
    flag(total_loaded_atoms, SL, SL), 
    SL > 1, 
    !.

% Fallback to manually summing atom counts if the flag isn't set.
full_atom_count(SL) :- 
    findall(NC, (fb_pred(F, A), metta_stats(F, A, NC)), Each), 
    sumlist(Each, SL).

% heartbeat/0: Periodically prints a heartbeat message if more than 60 seconds have passed.
% Keeps track of last printed time using nb_setval and nb_getval.
% @example heartbeat.
heartbeat :- 
    % Get the current system time
    get_time(CurrentTime),

    % If last_printed_time doesn't exist, set it to the current time
    (   nb_current(last_printed_time, _) 
    ->  true
    ;   nb_setval(last_printed_time, CurrentTime)
    ),

    % Retrieve the last printed time
    nb_getval(last_printed_time, LastPrintedTime),

    % Calculate time difference since the last heartbeat
    Diff is CurrentTime - LastPrintedTime,

    % If more than 60 seconds have passed, print the heartbeat
    (   Diff >= 60
    ->  metta_stats  % Print stats
    ;   true  % Otherwise, do nothing
    ).

% metta_stats/0: Performs garbage collection and prints detailed memory usage statistics.
% @example metta_stats.
metta_stats :- 
    % Perform garbage collection
    gc_now,

    % Print headers to delineate the stats
    writeln('\n\n\n\n\n\n;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
    writeln(';~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),

    % Calculate and display the total atom count
    full_atom_count(SL),
    format("~N~n; Total\t\tAtoms (Atomspace size): ~`.t ~D~108|~n", [SL]),

    % Get the current system time and update the last printed time
    get_time(CurrentTime), 
    nb_setval(last_printed_time, CurrentTime),

    % Post different statistics
    post_statistic(memory, Mem),
    post_statistic(atom_space, AS),
    post_statistic(cputime, TotalSeconds),
    post_statistic(atoms, Concepts),
    flag(assert_new, CTs, CTs),
    post_statistic(stack, StackMem),

    % Perform various calculations
    PM is Mem + StackMem,
    RM is Mem - AS,
    PA is RM // (SL + 1),
    APS is 60 * floor(SL / (TotalSeconds + 1)),
    ACS is AS // (Concepts + 1),

    % Display specific stats
    pl_stats('SymbolAtoms', Concepts),
    pl_stats('Random samples', CTs),

    % These stats are currently skipped due to some reasons (likely not essential).
    % skip/1 is used to temporarily bypass calculations or stats output.
    skip((pl_stats('Bytes Per Atom (Average)', PA), 
          pl_stats('Bytes Per ConceptNode (Average)', ACS))),
    skip((pl_stats('Relational Memory', RM), 
          pl_stats('ConceptNode Memory', AS))),

    % Other potential stats that were disabled (could be useful for specific cases).
    % pl_stats('Queryspace Memory', StackMem),
    % CPU is CPUTime-57600,

    % Format and display runtime in days:hh:mm:ss format
    format_time(TotalSeconds, Formatted),

    % Skipped atoms per minute stat.
    skip((pl_stats('Atoms per minute', APS))),

    % Display memory and runtime stats
    pl_stats('Total Memory Used', PM),
    pl_stats('Runtime (days:hh:mm:ss)', Formatted),

    nl, nl, 
    !.

% metta_stats/1: Gathers stats for a specific predicate.
% @param F - The predicate
% @example metta_stats(fb_pred).
metta_stats(F) :- 
    for_all(fb_pred(F, A), metta_stats(F, A)).

% metta_stats/2: Gathers stats for a predicate with a specific arity.
% @param F - The predicate
% @param A - The arity
% @example metta_stats(fb_pred, 2).
metta_stats(F, A) :- 
    metta_stats(F, A, NC), 
    pl_stats(F / A, NC).

% metta_stats/3: Retrieves the number of clauses for a predicate.
% @param F - The predicate
% @param A - The arity
% @param NC - The number of clauses
% @example metta_stats(fb_pred, 2, ClauseCount).
metta_stats(F, A, NC) :- 
    functor(P, F, A), 
    predicate_property(P, number_of_clauses(NC)).

% pl_stats/1: Retrieves and displays statistics based on the input Stat.
% @param Stat - The statistic to retrieve
% @example pl_stats(memory).
pl_stats(Stat) :- 
    statistics(Stat, Value), 
    pl_stats(Stat, Value).

% pl_stats/2: Handles cases where Stat has a list of values.
% @param Stat - The statistic
% @param Value - The value or first element of a list of values
pl_stats(Stat, [Value|_]) :- 
    nonvar(Value), 
    !, 
    pl_stats(Stat, Value).

% pl_stats/2: Formats and displays a specific statistic and its value.
% @param Stat - The statistic
% @param Value - The value to display
pl_stats(Stat, Value) :- 
    format("~N;\t\t~@: ~`.t ~@~100|", [format_value(Stat), format_value(Value)]), 
    !.

% format_value/1: Helper predicate to format a value (floats only).
% @param Value - The value to format
% @example format_value(3.14159).
format_value(Value) :- 
    float(Value), 
    !, 
    format("~2f", [Value]), 
    !.
/* This is a predicate to format a value (Bytes or general terms) and print it.
 * The first clause handles the case where the input is an integer (representing bytes).
 * The second clause handles all other types of terms.
 * @param Bytes The input value in bytes (if it's an integer) or other terms.
 * @example format_value(2048) would print "2K".
 */
format_value(Bytes) :- 
    % Check if Bytes is an integer (i.e., represents byte size)
    integer(Bytes), 
    % Call helper predicate to format the byte value
    format_bytes(Bytes, Formatted), 
    % Print the formatted value
    write(Formatted).

% Handle general term formatting
format_value(Term) :- 
    % Print the term as-is
    format("~w", [Term]).

/* Predicate to format bytes into a human-readable string representation (G, M, K, B).
 * It chooses the appropriate unit based on the size of the bytes.
 * @param Bytes The number of bytes to format.
 * @param Formatted The resulting human-readable string.
 * @example format_bytes(1073741824, Formatted) would return '1.00G'.
 */
% Base case: If the number is 1G or more, show it in gigabytes (G).
format_bytes(Bytes, Formatted) :-  
    Bytes >= 1073741824, 
    % Convert Bytes to Gigabytes
    GB is Bytes / 1073741824, 
    % Format the result with two decimal places and 'G'
    format(string(Formatted), '~2fG', [GB]).

% Case where the number is less than 1G but 100MB or more, display in megabytes (M).
format_bytes(Bytes, Formatted) :- 
    Bytes >= 104857600, Bytes < 1073741824, !, 
    % Convert Bytes to Megabytes
    MB is Bytes / 1048576, 
    % Round to the nearest integer for display
    D is floor(MB), 
    % Format the result in 'M'
    format(string(Formatted), '~DM', [D]).

% Case where the number is less than 1K, display in bytes (B).
format_bytes(Bytes, Formatted) :- 
    % Format the result directly as Bytes
    format(string(Formatted), '~D', [Bytes]).

/* previously: This clause handled formatting for kilobytes (K), but was commented out. 
 * It was skipped because it's less frequently used, and the current implementation 
 * prioritizes higher units like MB and GB for clarity. 
 * You could uncomment and adjust this if K units are needed.
 */
% % If the number is less than 1M, show it in kilobytes (K).
% format_bytes(Bytes, Formatted) :- 
%     Bytes >= 1024, Bytes < 1048576, !, 
%     KB is Bytes / 1024, 
%     format(string(Formatted), '~0fK', [KB]).

/* Predicate to format total seconds into a readable time (days, hours, minutes, seconds).
 * @param TotalSeconds The input time in seconds.
 * @param Formatted The resulting formatted string.
 * @example format_time(90061, Formatted) would return '1:01:00:01' (1 day, 1 hour, 1 second).
 */
format_time(TotalSeconds, Formatted) :-
    % Convert TotalSeconds to an integer (floor value)
    Seconds is floor(TotalSeconds),
    % Extract the number of days
    Days is div(Seconds, 86400),
    % Get the remaining seconds after subtracting full days
    Remain1 is mod(Seconds, 86400)-57600,
    % Format the remaining seconds as time (hours:minutes:seconds)
    format_time(string(Out), '%T', Remain1),
    % Combine days and formatted time into the result string
    format(string(Formatted), '~w:~w', [Days, Out]).

/* Predicate to print the formatted time.
 * @param TotalSeconds The time in seconds.
 * @example print_formatted_time(3600) would print '0:01:00:00' (1 hour).
 */
print_formatted_time(TotalSeconds) :-
    % Format the total seconds into a human-readable time string
    format_time(TotalSeconds, Formatted),
    % Print the formatted time
    writeln(Formatted).

/* A metapredicate to save pre-execution statistics.
 * This is likely to track system memory, atom usage, and space in a knowledge base.
 * This might be part of some performance logging or debugging process.
 */
metta_final :-
    save_pre_statistic(memory),
    save_pre_statistic(atoms),
    save_pre_statistic(atom_space).

/* previously: The following block of predicates handled symbolic manipulations with atoms.
 * This code has been commented out, possibly because it was not needed or replaced by more efficient methods.
 * These predicates could be uncommented if symbolic processing is required again.
 */
/*
symbol(X):- atom(X).
symbol_number(S,N):- atom_number(S,N).
symbol_string(S,N):- atom_string(S,N).
symbol_chars(S,N):- atom_chars(S,N).
symbol_length(S,N):- atom_length(S,N).
symbol_concat(A,B,C):- atom_concat(A,B,C).
symbolic_list_concat(A,B,C):- atomic_list_concat(A,B,C).
symbolic_list_concat(A,B):- atomic_list_concat(A,B).
symbol_contains(T,TT):- atom_contains(T,TT).
*/

/* Predicate to search for an atom in the knowledge base (KB) that contains a specific variable.
 * @param X The variable to search for within the KB.
 * This uses a forall loop to find every instance of X contained in atoms and print them.
 */
search_for1(X) :-
    forall((metta_atom(_Where, What), contains_var(X, What)),
           (nl, write_src_nl(What))).

/* Another search predicate that looks for a variable within file sources.
 * @param X The variable to search for in file source facts.
 * This also uses a forall loop to find and print matching occurrences.
 */
search_for2(X) :-
    forall((metta_file_src(_Where, What), contains_var(X, What)),
           (nl, write_src_nl(What))).

/* Predicate that retrieves source code from a file in the knowledge base (KB).
 * This fetches source code loaded into the KB and the associated variables.
 * @param Where The location within the KB.
 * @param What The specific source code or content.
 */
metta_file_src(Where, What) :-
    loaded_into_kb(Where, File), 
    metta_file_buffer(_, What, Vars, File, _Loc),
    % Attempt to name variables for better readability
    ignore(maplist(name_the_var, Vars)).

/* Predicate to guess variables within a block of code.
 * This is useful for tracking unification or variable names in the source.
 * @param What The term to match and unify variables.
 */
guess_metta_vars(What) :-
    ignore(once((metta_file_buffer(_, What0, Vars, _File, _Loc),
                 alpha_unify(What, What0),
                 maplist(name_the_var, Vars)))).

/* Helper predicate to name variables in a structured way.
 * It converts system-generated variable names ('$VAR') to readable ones.
 * @param N=V The variable name and value.
 */
name_the_var(N=V) :- 
    % Convert the variable name by stripping the underscore
    ignore((atom_concat('_', NV, N), V = '$VAR'(NV))).

/* Predicate to perform alpha-unification.
 * This checks if two terms are structurally identical while considering variable names.
 * @param What The first term.
 * @param What0 The second term for unification.
 */
alpha_unify(What, What0) :- 
    % Check if terms are identical
    What =@= What0, 
    % Perform strict equality if non-variable or shallow comparison if both are variables
    (nonvar(What) -> What = What0 ; What == What0).
