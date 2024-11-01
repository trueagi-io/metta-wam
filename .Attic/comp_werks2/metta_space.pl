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
% PROGRAM FUNCTION: provides predicates for managing and querying atoms/facts in different 
% types of spaces along with various utility functions for statistics tracking and debugging.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_compiler).

%:- ensure_loaded(metta_compiler).
% TODO move non flybase specific code between here and the compiler
%:- ensure_loaded(flybase_main).

% Declare 'is_pre_statistic/2' as a multifile predicate, allowing it to be defined across multiple files.
:- multifile(is_pre_statistic/2).
% Declare 'is_pre_statistic/2' as a dynamic predicate, meaning it can be modified during execution (e.g., asserted or retracted).
:- dynamic(is_pre_statistic/2).

%!  save_pre_statistic(+Name) is det.
%
%   Saves the current value of a given statistic. If a previous value for 
%   the statistic `Name` is already stored, the predicate does nothing.
%   Otherwise, it fetches the current value using `statistics/2`, converts 
%   it to a number with `term_number/2`, and stores it with `pfcAdd_Now/1`.
%
%   @arg Name The name of the statistic to be saved.
%
%   @example
%     % Save the statistic for memory usage.
%     ?- save_pre_statistic(memory).
%
save_pre_statistic(Name) :- 
    is_pre_statistic(Name, _) -> true ; 
    (statistics(Name, AS), term_number(AS, FN), pfcAdd_Now(is_pre_statistic(Name, FN))).

%!  pre_statistic(+Name, -Value) is det.
%
%   Retrieves the previously saved value for the statistic `Name`.
%   If the statistic exists, the value is unified with `Value`. 
%   If not, `Value` is set to 0.
%
%   @arg Name  The name of the statistic to retrieve.
%   @arg Value The value of the statistic, or 0 if it does not exist.
%
%   @example
%     % Get the saved memory statistic.
%     ?- pre_statistic(memory, Value).
%     Value = 1024.
%
pre_statistic(N, V) :- is_pre_statistic(N, V) -> true ; V = 0.

%!  post_statistic(+Name, -Value) is det.
%
%   Computes the difference between the current and previously saved 
%   value of the statistic `Name`. If the difference is negative, 
%   `Value` is set to 0; otherwise, the difference is assigned to `Value`.
%
%   @arg Name  The name of the statistic to compute.
%   @arg Value The computed difference, or 0 if the difference is negative.
%
%   @example
%     % Calculate the change in memory usage since the last save.
%     ?- post_statistic(memory, Value).
%     Value = 256.
%
post_statistic(N, V) :- 
    statistics(N, VV), term_number(VV, FV), pre_statistic(N, WV), 
    V0 is FV - WV, (V0 < 0 -> V = 0 ; V0 = V).

%!  term_number(+Term, -Number) is nondet.
%
%   Extracts a numeric subterm from a given term. If `Term` contains 
%   a numeric subterm, it is unified with `Number`. This predicate 
%   succeeds for each numeric subterm found within the term.
%
%   @arg Term   The term to search for numeric subterms.
%   @arg Number A numeric subterm found within the given term.
%
%   @example
%     % Extract numbers from a term.
%     ?- term_number(foo(1, bar(2)), N).
%     N = 1 ;
%     N = 2.
%
term_number(T, N) :- sub_term(N, T), number(N).

%!  call_match(+Goals) is det.
%
%   Executes a goal or a list of goals sequentially. If the input is a single 
%   goal, it calls that goal directly. If the input is a list, it recursively 
%   executes each goal in the list in order. This predicate ensures all goals 
%   are executed in sequence using `call/1`.
%
%   @arg Goals A goal or a list of goals to be executed.
%
%   @example
%     % Call a list of goals.
%     ?- call_match([write('Hello'), nl, write('World')]).
%     Hello
%     World
%
% Call a single goal from a list containing only one element.
call_match([G]) :- !, call(G).
% Call the first goal in the list, then recursively call the rest of the goals.
call_match([G | GG]) :- !, call(G), call_match(GG).
% Call a single goal that is not part of a list.
call_match(G) :- call(G).

%!  'save-space!'(+Space, +File) is det.
%
%   Saves all atoms from the given `Space` to the specified `File`. 
%   This predicate ensures the file is properly opened, written to, 
%   and closed, using `setup_call_cleanup/3` to handle resources safely.
%
%   @arg Space The space from which atoms are retrieved.
%   @arg File  The file where the atoms will be saved.
%
%   @example
%     % Save atoms from a space to a file.
%     ?- 'save-space!'(my_space, 'output.txt').
%
% Use setup_call_cleanup to ensure the file is opened and closed properly.
'save-space!'(Space, File) :-
    setup_call_cleanup(
        % Open the specified File in write mode.
        open(File, write, Out, []),
        % Write all atoms from the Space to the file using with_output_to/2.
        with_output_to(Out,
            % For each atom retrieved from the space, write it to the file.
            forall(get_atoms(Space, Atom), write_src(Atom))),
        % Ensure the output stream is closed after writing.
        close(Out)).

% declare dynamic predicates, meaning they can be modified during execution (e.g., asserted or retracted).
:- dynamic(repeats/1).
:- dynamic(not_repeats/1).

%!  assert_new(+P) is det.
%
%   Asserts a new fact or rule if it is not already present in the database.
%   If the fact already exists, it asserts it as a repeated fact with `repeats/1`.
%   If it is new, the predicate is added using `pfcAdd_Now/1`, and a counter is updated.
%
%   @arg P The fact or rule to be asserted.
%
%   @example
%     % Assert a new fact.
%     ?- assert_new(my_fact).
%
%     % Assert an already existing fact to track repeats.
%     ?- assert_new(my_fact).
%     % Fact is now marked as repeated with repeats(my_fact).
%
% If the fact P exists, assert it as a repeated fact.
assert_new(P) :- 
    notrace(catch(call(P), _, fail)), !,
    assert_new1(repeats(P)).
% If the fact P does not exist, assert it as new and increment the counter.
assert_new(P) :- 
    pfcAdd_Now(P),
    flag(assert_new, TA, TA + 1),
    assert_new1(not_repeats(P)), !.

%!  retract1(+P) is det.
%
%   Retracts a fact or rule from the database if it exists. If it does not exist,
%   the predicate succeeds without error. This predicate ensures that if the 
%   retraction fails, it is ignored to avoid unnecessary failures.
%
%   @arg P The fact or rule to be retracted.
%
%   @example
%     % Retract a fact if it exists.
%     ?- retract1(my_fact).
%
%     % Attempting to retract a non-existent fact succeeds silently.
%     ?- retract1(non_existent_fact).
%     true.
%
% If the fact P does not exist, succeed without action.
retract1(P) :- \+ call(P), !.
% If the fact P exists, retract it, ignoring any failures during retraction.
retract1(P) :- ignore(\+ retract(P)).

%!  assert_new1(+P) is det.
%
%   Asserts a fact or rule only if it does not already exist. This is a helper 
%   predicate used by `assert_new/1` to ensure that duplicates are not added.
%
%   @arg P The fact or rule to be asserted.
%
%   @example
%     % Assert a helper fact.
%     ?- assert_new1(helper_fact).
%
% If the fact P exists, do nothing.
assert_new1(P) :- \+ \+ call(P), !.
% If the fact P does not exist, assert it.
assert_new1(P) :- pfcAdd_Now(P).

% dynamic predicates, meaning they can be modified during execution (e.g., asserted or retracted).
:- dynamic(fb_pred/3).
:- dynamic(mod_f_a/3).

%!  decl_m_fb_pred(+Mod, +Fn, +A) is det.
%
%   Declares a module-function-arity predicate if not already defined.
%
%   This predicate ensures that the given module, function name, and arity 
%   combination is declared. If the module is not instantiated (i.e., it is a variable),
%   it binds the `Mod` to the value obtained from `mod_f_a/3`. Otherwise, it checks if the 
%   predicate is already known using `mod_f_a/3`. If it exists, it succeeds. If not, 
%   it declares the predicate as `dynamic` and uses `pfcAdd_Now/1` to add the new definition.
%
%   @arg Mod The module where the predicate belongs.
%   @arg Fn  The name of the function or predicate.
%   @arg A   The arity (number of arguments) of the function or predicate.
%
%   @example Declare a new predicate:
%     ?- decl_m_fb_pred(my_module, my_predicate, 2).
%
%   @example If the module is a variable, bind it:
%     ?- decl_m_fb_pred(Mod, my_predicate, 2).
%     Mod = inferred_module.
%
decl_m_fb_pred(Mod, Fn, A) :-
    % If Mod is unbound (a variable), bind it using mod_f_a/3.
    var(Mod),!,mod_f_a(Mod, Fn, A).
decl_m_fb_pred(Mod, Fn, A) :-
    % If mod_f_a/3 succeeds, the predicate is already declared; do nothing.
    mod_f_a(Mod, Fn, A) -> true ;
    % Otherwise, declare it as dynamic and add it using pfcAdd_Now/1.
    (dynamic(Mod:Fn/A),pfcAdd_Now(mod_f_a(Mod, Fn, A))).

% a dynamic predicate can be modified during execution (e.g., asserted or retracted).
:- dynamic(fb_pred_file/3).

%!  decl_fb_pred(+Fn, +A) is det.
%
%   Declares a function-arity predicate if not already defined and associates it with the current loading file.
%
%   This predicate ensures that the function name and arity combination is declared, and it also records
%   the file from which the predicate is being loaded. If the predicate is already known via `fb_pred/2`, 
%   it succeeds without changes. Otherwise, it declares the predicate as `dynamic` and adds it using 
%   `pfcAdd_Now/1`. Additionally, if the `loading_file` is defined in the current environment, 
%   it associates the predicate with the file using `fb_pred_file/3`.
%
%   @arg Fn  The name of the function or predicate.
%   @arg A   The arity (number of arguments) of the function or predicate.
%
%   @example Declare a new function-arity predicate:
%     ?- decl_fb_pred(my_predicate, 2).
%
%   @example Associate a predicate with the current loading file:
%     ?- nb_setval(loading_file, 'my_file.pl'),
%        decl_fb_pred(my_predicate, 2).
%
decl_fb_pred(Fn, A) :-
    % Check if the predicate is already known. If not, declare it as dynamic and add it.
    (fb_pred(Fn, A) -> true ;
     (dynamic(Fn/A), pfcAdd_Now(fb_pred(Fn, A)))),
    % Attempt to associate the predicate with the current loading file, if available.
    ignore((
        nb_current(loading_file, File),  % Get the current loading file, if set.
        % If the predicate-file association doesn't exist, add it.
        (fb_pred_file(Fn, A, File) -> true ; 
         pfcAdd_Now(fb_pred_file(Fn, A, File))))).

% Import necessary libraries
:- use_module(library(readutil)).

%!  skip(+X) is det.
%
%   A no-op (no-operation) predicate that succeeds for any input.
%
%   This predicate always succeeds regardless of the argument provided. It is typically used 
%   as a placeholder or to ignore certain inputs or steps in a larger computation.
%
%   @arg X Any input, which is ignored by the predicate.
%
%   @example Demonstrate that `skip/1` succeeds for any input:
%     ?- skip(42).
%     true.
%
%     ?- skip(foo).
%     true.
%
%     ?- skip(_).
%     true.
%
skip(_).

% ===============================
% MeTTa Python incoming interface
% ===============================

% ============================
% %%%% Atom Manipulations
% ============================

%!  'clear-atoms'(+SpaceNameOrInstance) is det.
%
%   Clears all atoms from the specified space.
%
%   This predicate removes all atoms from a given space, which can be identified
%   either by its name or by an instance. It logs the operation using `dout/2` and
%   invokes the appropriate method for the space's type using `space_type_method/3`.
%
%   @arg SpaceNameOrInstance The name or instance of the space to be cleared.
%
%   @example Clear all atoms from a space:
%     ?- 'clear-atoms'('my_space').
%
'clear-atoms'(SpaceNameOrInstance) :-
    % Log the operation of clearing atoms from the specified space.
    dout(space, ['clear-atoms', SpaceNameOrInstance]),
    % Retrieve the appropriate method for clearing the space based on its type.
    space_type_method(Type, clear_space, Method),
    % Call the type predicate to ensure the space type matches.
    call(Type, SpaceNameOrInstance), 
    !,
    % Log the type-method used.
    dout(space, ['type-method', Type, Method]),
    % Invoke the method to clear the space.
    call(Method, SpaceNameOrInstance).

%!  'add-atom'(+SpaceNameOrInstance, +Atom) is det.
%
%   Adds an atom to the specified space.
%
%   This predicate adds an atom to a given space by invoking the appropriate
%   method for the space's type. It conditionally logs the operation depending 
%   on whether the space is self-referential or asserted.
%
%   @arg SpaceNameOrInstance The name or instance of the space to which the atom is added.
%   @arg Atom The atom to be added to the space.
%
%   @example Add an atom to a space:
%     ?- 'add-atom'('my_space', my_atom).
%
'add-atom'(SpaceNameOrInstance, Atom) :-
    % Retrieve the method for adding an atom based on the space type.
    space_type_method(Type, add_atom, Method),
    % Ensure the space type matches by calling the type predicate.
    call(Type, SpaceNameOrInstance),
    !,
    % Log the operation if the space is not self-referential or asserted.
    if_t((SpaceNameOrInstance \== '&self' ; Type \== 'is_asserted_space'),
         dout(space, ['type-method', Type, Method, SpaceNameOrInstance, Atom])),
    % Invoke the method to add the atom to the space.
    call(Method, SpaceNameOrInstance, Atom).

%!  'add-atom'(+Environment, +AtomDeclaration, -Result) is det.
%
%   Adds an atom to the environment using the given declaration and returns the result.
%
%   This variant of `add-atom/2` allows adding an atom to the environment by evaluating
%   a set of arguments. The result of the operation is returned in `Result`.
%
%   @arg Environment The environment in which the atom is added.
%   @arg AtomDeclaration The declaration of the atom to be added.
%   @arg Result The result of the evaluation after adding the atom.
%
%   @example Add an atom to an environment:
%     ?- 'add-atom'(env, my_atom_declaration, Result).
%
'add-atom'(Environment, AtomDeclaration, Result) :-
    % Evaluate the arguments to perform the add-atom operation.
    eval_args(['add-atom', Environment, AtomDeclaration], Result).

%!  'remove-atom'(+SpaceNameOrInstance, +Atom) is det.
%
%   Removes an atom from the specified space.
%
%   This predicate removes an atom from a given space by invoking the appropriate
%   method for the space's type. It logs the operation for traceability.
%
%   @arg SpaceNameOrInstance The name or instance of the space from which the atom is removed.
%   @arg Atom The atom to be removed from the space.
%
%   @example Remove an atom from a space:
%     ?- 'remove-atom'('my_space', my_atom).
%
'remove-atom'(SpaceNameOrInstance, Atom) :-
    % Log the operation of removing an atom from the specified space.
    dout(space, ['remove-atom', SpaceNameOrInstance, Atom]),
    % Retrieve the method for removing an atom based on the space type.
    space_type_method(Type, remove_atom, Method),
    % Ensure the space type matches by calling the type predicate.
    call(Type, SpaceNameOrInstance),
    !,
    % Log the type-method used.
    dout(space, ['type-method', Type, Method]),
    % Invoke the method to remove the atom from the space.
    call(Method, SpaceNameOrInstance, Atom).

%!  'remove-atom'(+Environment, +AtomDeclaration, -Result) is det.
%
%   Removes an atom from the environment using the given declaration and returns the result.
%
%   This variant of `remove-atom/2` allows removing an atom from the environment by evaluating
%   a set of arguments. The result of the operation is returned in `Result`.
%
%   @arg Environment The environment from which the atom is removed.
%   @arg AtomDeclaration The declaration of the atom to be removed.
%   @arg Result The result of the evaluation after removing the atom.
%
%   @example Remove an atom from an environment:
%     ?- 'remove-atom'(env, my_atom_declaration, Result).
%
'remove-atom'(Environment, AtomDeclaration, Result) :-
    % Evaluate the arguments to perform the remove-atom operation.
    eval_args(['remove-atom', Environment, AtomDeclaration], Result).


%!  'replace-atom'(+SpaceNameOrInstance, +Atom, +New) is det.
%
%   Replaces an existing atom with a new atom in the specified space.
%
%   This predicate finds and replaces an atom in a space, identified by 
%   its name or instance. It logs the operation and invokes the appropriate 
%   method for the space's type using `space_type_method/3`.
%
%   @arg SpaceNameOrInstance The name or instance of the space where the atom is replaced.
%   @arg Atom The atom to be replaced.
%   @arg New The new atom to replace the old one.
%
%   @example Replace an atom in a space:
%     ?- 'replace-atom'('my_space', old_atom, new_atom).
%
'replace-atom'(SpaceNameOrInstance, Atom, New) :-
    dout(space, ['replace-atom', SpaceNameOrInstance, Atom, New]),
    space_type_method(Type, replace_atom, Method), 
    call(Type, SpaceNameOrInstance),
    !,
    dout(space, ['type-method', Type, Method]),
    call(Method, SpaceNameOrInstance, Atom, New).

%!  'atom-replace'(+Environment, +OldAtom, +NewAtom, -Result) is det.
%
%   Replaces an atom in an environment by evaluating the given arguments.
%
%   @arg Environment The environment where the replacement occurs.
%   @arg OldAtom The old atom to be replaced.
%   @arg NewAtom The new atom to replace the old one.
%   @arg Result The result of the replacement operation.
%
%   @example Replace an atom in an environment:
%     ?- 'atom-replace'(env, old_atom, new_atom, Result).
%
'atom-replace'(Environment, OldAtom, NewAtom, Result) :-
    eval_args(['atom-replace', Environment, OldAtom, NewAtom], Result).

%!  'atom-count'(+SpaceNameOrInstance, -Count) is det.
%
%   Counts the number of atoms in the specified space.
%
%   This predicate retrieves the atom count from the given space by invoking the 
%   appropriate method for the space's type. The result is logged for traceability.
%
%   @arg SpaceNameOrInstance The name or instance of the space to be queried.
%   @arg Count The number of atoms in the space.
%
%   @example Count the atoms in a space:
%     ?- 'atom-count'('my_space', Count).
%
'atom-count'(SpaceNameOrInstance, Count) :-
    dout(space, ['atom-count', SpaceNameOrInstance]),
    space_type_method(Type, atom_count, Method), 
    call(Type, SpaceNameOrInstance),
    !,
    call(Method, SpaceNameOrInstance, Count),
    dout(space, ['type-method-result', Type, Method, Count]).
'atom-count'(Environment, Count) :-
    eval_args(['atom-count', Environment], Count).

%!  'get-atoms'(+SpaceNameOrInstance, -AtomsL) is det.
%
%   Fetches all atoms from the specified space.
%
%   This predicate retrieves all atoms from a space, identified by its name or instance.
%   It logs the operation for traceability.
%
%   @arg SpaceNameOrInstance The name or instance of the space to be queried.
%   @arg AtomsL The list of atoms retrieved from the space.
%
%   @example Get atoms from a space:
%     ?- 'get-atoms'('my_space', Atoms).
%
'get-atoms'(SpaceNameOrInstance, AtomsL) :-
    dout(space, ['get-atoms', SpaceNameOrInstance]),
    space_type_method(Type, get_atoms, Method), 
    call(Type, SpaceNameOrInstance),
    !,
    call(Method, SpaceNameOrInstance, AtomsL).
'get-atoms'(Environment, Atoms) :-
    eval_args(['get-atoms', Environment], Atoms).

%!  'atoms_iter'(+SpaceNameOrInstance, -Iter) is det.
%
%   Iterates over all atoms in the specified space.
%
%   This predicate retrieves an iterator for the atoms in a space, identified by
%   its name or instance. The operation is logged for traceability.
%
%   @arg SpaceNameOrInstance The name or instance of the space to iterate.
%   @arg Iter The iterator over the atoms in the space.
%
%   @example Iterate over atoms in a space:
%     ?- 'atoms_iter'('my_space', Iter).
%
'atoms_iter'(SpaceNameOrInstance, Iter) :-
    dout(space, ['atoms_iter', SpaceNameOrInstance]),
    space_type_method(Type, atoms_iter, Method), 
    call(Type, SpaceNameOrInstance),
    !,
    call(Method, SpaceNameOrInstance, Iter),
    dout(space, ['type-method-result', Type, Method, Iter]).

%!  'atoms_match'(+SpaceNameOrInstance, -Atoms, +Template, +Else) is det.
%
%   Matches all atoms in the specified space against a template.
%
%   This predicate retrieves atoms from a space that match a given template. If no 
%   matches are found, it executes an alternative `Else` clause.
%
%   @arg SpaceNameOrInstance The name or instance of the space to search.
%   @arg Atoms The list of atoms that match the template.
%   @arg Template The template used to match the atoms.
%   @arg Else An alternative clause if no matches are found.
%
%   @example Match atoms in a space:
%     ?- 'atoms_match'('my_space', Atoms, my_template, else_clause).
%
'atoms_match'(SpaceNameOrInstance, Atoms, Template, Else) :-
    space_type_method(Type, atoms_match, Method), 
    call(Type, SpaceNameOrInstance),
    !,
    call(Method, SpaceNameOrInstance, Atoms, Template, Else),
    dout(space, ['type-method-result', Type, Method, Atoms, Template, Else]).

%!  'space_query'(+SpaceNameOrInstance, +QueryAtom, -Result) is det.
%
%   Queries the specified space for a matching atom and returns the result.
%
%   This predicate executes a query in a space, identified by its name or instance, 
%   and logs the result for traceability.
%
%   @arg SpaceNameOrInstance The name or instance of the space to query.
%   @arg QueryAtom The atom used as the query.
%   @arg Result The result of the query.
%
%   @example Query a space for an atom:
%     ?- 'space_query'('my_space', query_atom, Result).
%
'space_query'(SpaceNameOrInstance, QueryAtom, Result) :-
    space_type_method(Type, query, Method), 
    call(Type, SpaceNameOrInstance),
    !,
    call(Method, SpaceNameOrInstance, QueryAtom, Result),
    dout(space, ['type-method-result', Type, Method, Result]).


subst_pattern_template(SpaceNameOrInstance, Pattern, Template) :-
    dout(space,[subst_pattern_template,SpaceNameOrInstance, Pattern, Template]),
    'atoms_match'(SpaceNameOrInstance, Pattern, Template, []).

/*
space_query_vars(SpaceNameOrInstance, Query, Vars) :- is_as_nb_space(SpaceNameOrInstance),!,
    fetch_or_create_space(SpaceNameOrInstance, Space),
    call_metta(Space,Query,Vars).
*/ :- dynamic(was_asserted_space/1).

was_asserted_space('&self').
was_asserted_space('&stdlib').
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
is_as_nb_space(G):- is_valid_nb_space(G) -> true ;
                 is_registered_space_name(G),nb_current(G,S),is_valid_nb_space(S).

is_nb_space(G):- nonvar(G), is_as_nb_space(G).
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
space_type_method(is_as_nb_space,new_space,init_space).
space_type_method(is_as_nb_space,clear_space,clear_nb_atoms).
space_type_method(is_as_nb_space,add_atom,add_nb_atom).
space_type_method(is_as_nb_space,remove_atom,remove_nb_atom).
space_type_method(is_as_nb_space,replace_atom,replace_nb_atom).
space_type_method(is_as_nb_space,atom_count,atom_nb_count).
space_type_method(is_as_nb_space,get_atoms,get_nb_atoms).
%space_type_method(is_as_nb_space,get_atoms,arg(1)).
space_type_method(is_as_nb_space,atom_iter,atom_nb_iter).
%space_type_method(is_as_nb_space,query,space_nb_query).

% Clear all atoms from a space
clear_nb_atoms(SpaceNameOrInstance) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    nb_setarg(1, Space, []).

% Add an atom to the space
add_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    NewAtoms = [Atom | Atoms],
    nb_setarg(1, Space, NewAtoms).

% Count atoms in a space
atom_nb_count(SpaceNameOrInstance, Count) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    length(Atoms, Count).

% Remove an atom from a space
remove_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    select(Atom, Atoms, UpdatedAtoms),
    nb_setarg(1, Space, UpdatedAtoms).

% Fetch all atoms from a space
get_nb_atoms(SpaceNameOrInstance, Atoms) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms).

% Replace an atom in the space
replace_nb_atom(SpaceNameOrInstance, OldAtom, NewAtom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    ( (select(Found, Atoms, TempAtoms),OldAtom=@=Found)
    ->  NewAtoms = [NewAtom | TempAtoms],
        nb_setarg(1, Space, NewAtoms)
    ;   false
    ).



% Function to confirm if a term represents a space
is_valid_nb_space(Space):- compound(Space),functor(Space,'Space',_).

% Find the original name of a given space
space_original_name(Space, Name) :-
    is_registered_space_name(Name),
    nb_current(Name, Space).

% Register and initialize a new space
init_space(Name) :-
    Space = 'Space'([]),
    asserta(is_registered_space_name(Name)),
    nb_setval(Name, Space).

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
    'get-atoms'(Space, Atoms),
    'match-pattern'(Atoms, Pattern, Template).

% Simple pattern match
'match-pattern'([], _, []).
'match-pattern'([H |_T], H, H) :- !.
'match-pattern'([_H| T], Pattern, Template) :- 'match-pattern'(T, Pattern, Template).

%is_python_space(X):- python_object(X).

ensure_space(X,Y):- catch(ensure_space_py(X,Y),_,fail),!.
ensure_space(_N,_V):- fail.

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
space_type_method(is_asserted_space,atom_count,metta_assertdb_count).
space_type_method(is_asserted_space,get_atoms,metta_assertdb_get_atoms).
space_type_method(is_asserted_space,atom_iter,metta_assertdb_iter).
%space_type_method(is_asserted_space,query,space_nb_query).

%:- dynamic(for_metta/2).
%for_metta(_,T):- fb_pred(F,A),functor(T,F,A),call(T).
metta_assertdb_ls(KB):-
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,_],
  listing(MP).

metta_assertdb_add(KB,AtomIn):-
 must_det_ll((subst_vars(AtomIn,Atom),
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,Atom],
  assert_new(MP))).
metta_assertdb_rem(KB,Old):- metta_assertdb_del(KB,Old).
metta_assertdb_del(KB,Atom):- subst_vars(Atom,Old),
  decl_m_fb_pred(user,metta_atom_asserted,2),
   MP = metta_atom(KB,Old),
  copy_term(MP,Copy), clause(MP,true,Ref), MP=@= Copy, !, erase(Ref). % ,metta_assertdb('DEL',Old).
metta_assertdb_replace(KB,Old,New):- metta_assertdb_del(KB,Old), metta_assertdb_add(KB,New).



atom_count_provider(Self,Count):-
    user:loaded_into_kb(Self,Filename),
     once(user:asserted_metta_pred(Mangle,Filename)),
     mangle_iz(Mangle,Iz),
     member(P,[Mangle,Iz]),
     between(2,8,Arity),
     functor(Data,P,Arity),
     predicate_property(Data,number_of_clauses(CC)),
     predicate_property(Data,number_of_rules(RC)),
     Count is CC - RC.

atom_count_provider(KB,Count):-
 must_det_ll((
  AMA = metta_atom_asserted,
  decl_m_fb_pred(user,AMA,2),
  MP =.. [AMA,KB,_],
  predicate_property(MP,number_of_clauses(SL2)),
  predicate_property(MP,number_of_rules(SL3)),
  %metta_assertdb_ls(KB),
      full_atom_count(SL1),
  Count is SL1 + SL2 - SL3)),!.

metta_assertdb_count(KB,Count):-
    findall(C,atom_count_provider(KB,C),CL),
    sumlist(CL,Count).



%metta_assertdb_count(KB,Count):- writeln(metta_assertdb_count_in(KB,Count)), findall(Atom,for_metta(KB,Atom),AtomsL),length(AtomsL,Count),writeln(metta_assertdb_count_out(KB,Count)).
metta_assertdb_iter(KB,Atoms):-
     MP =.. [metta_atom,KB,Atoms],
     call(MP).



metta_iter_bind(KB,Query,Vars,VarNames):-
  term_variables(Query,QVars),
  align_varnames(VarNames,Vars),
  TV = dout(space,['match',KB,Query,QVars,Vars,VarNames]),
%  \+ \+ (numbervars(TV,0,_,[]),print(tv=TV),nl),
  ignore(QVars=Vars),
%  \+ \+ (numbervars(TV,0,_,[]),print(qv=TV),nl),
  \+ \+ (%numbervars(TV,0,_,[]),
         writeq(av=TV),nl),
  space_query_vars(KB,Query,TF),TF\=='False'.


% Query from hyperon.base.GroundingSpace
space_query_vars(KB,Query,Vars):- is_asserted_space(KB),!,
    decl_m_fb_pred(user,metta_atom_asserted,2),
    call_metta(KB,Query,Vars),
    dout('RES',space_query_vars(KB,Query,Vars)).


metta_assertdb_get_atoms(KB,Atom):- metta_atom(KB,Atom).
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


align_varnames(VarNames,Vars):-
  list_to_set(VarNames,NameSet),
  merge_named_vars(NameSet,VarNames,Vars).

merge_named_vars([],_VarNames,_Vars):-!.
merge_named_vars([N|NameSet],VarNames,Vars):-
  merge_named(N,_V,VarNames,Vars),
  merge_named_vars(NameSet,VarNames,Vars).
%merge_named_vars(_,_,_).

merge_named(_,_,[],[]):-!.
merge_named(N,V,[N|VarNames],[V|Vars]):-
  merge_named(N,V,VarNames,Vars).


call_metta( KB,Query,_Vars):- metta_atom(KB,Query).
call_metta(_KB,Query,_Vars):- metta_to_pyswip([],Query,Call),!,
  %print(user:Call),nl,
    user:call(Call).

metta_to_pyswip(_PS,Query,Call):- var(Query),!,Call=Query.
metta_to_pyswip(_PS,Query,Call):- \+ compound(Query),!,Call=Query,!.
metta_to_pyswip(PS,Query,Call):- is_list(Query),Query=[Q|Uery],!,cmpd_to_pyswip(PS,Q,Uery,Call).
metta_to_pyswip(PS,Query,Call):- Query=..[Q|Uery], cmpd_to_pyswip(PS,Q,Uery,Call).

cmpd_to_pyswip(PS,Q,Uery,Call):- atom(Q),maplist(metta_to_pyswip([Q|PS]),Uery,Cery),Call=..[Q|Cery].
cmpd_to_pyswip(PS,"and",Uery,Call):- maplist(metta_to_pyswip(PS),Uery,Args),list_to_conjuncts(Args,Call).


'show-metta-def'(Pred, []):-
  'get-metta-src'(Pred,[_|SrcL]),
  maplist(write_src_nl,SrcL).

write_src_nl(Src):- format('~N'),write_src(Src),format('~N').

%'get-metta-src'(Pred,[Len|SrcL]):- findall(['AtomDef',Src],'get-metta-src1'(Pred,Src),SrcL), length(SrcL,Len).
'get-metta-src'(Pred,[Len|SrcL]):- findall(Src,'get-metta-src1'(Pred,Src),SrcL), length(SrcL,Len).
'get-metta-src1'(Pred,Src):-
  current_self(Space),
  metta_atom(Space,F,A,List),
  once((sub_var(Pred,A)->Src = [F,A,List];sub_var(Pred,F)->Src = [F,A|List])).

% is a quine
'AtomDef'(X,['AtomDef',X]).


sort_on(C,R,A,B):- (A==B-> R= (=) ; must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA+A,BB+B)))),!.
tokens(X,VL):- unaccent_atom(X,A),!, findall(E,(is_tokenizer(T),call(T,A,E)),L),predsort(sort_on(length_fw_len),L,S),last(S,VL).

length_fw_len([W|List],L+WL):- length(List,L),atom_length(W,WL).

print_token_args:- make,
   fb_arg(X),tokens(X,A0),
   exclude(is_dash,A0,A),tterm(A,AT),writeq(X),write('    '),writeq(AT),write('  '),write_src(A),nl,fail.
is_dash('_').
is_dash('-').
tterm([A],A):-!.
tterm([A,':',B|M],BA):- atom(A),!,BA=..[A,B|M].
tterm([A,B|M],BA):- atom(B),!,BA=..[B,A|M].
tterm([A|B],BA):- atom(A),!,BA=..[B|A].
tterm(A,A).

is_tokenizer(into_list).
is_tokenizer(to_case_break_atoms).
is_tokenizer(atom_to_stem_list).
is_tokenizer(tokenize_atom).
%is_tokenizer(double_metaphone).



is_an_arg_type(S,T):- flybase_identifier(S,T),!.
has_type(S,Type):- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,Aft>0.


call_sexpr(S):- once_writeq_nl(call_sexpr(S)).
%call_sexpr(Space,Expr,Result):-

:- dynamic(fb_pred/2).

full_atom_count(SL):- flag(total_loaded_atoms,SL,SL),SL>1,!.
full_atom_count(SL):- findall(NC,(fb_pred(F,A),metta_stats(F,A,NC)),Each), sumlist(Each,SL).

heartbeat :-
    % Get the current time and the last printed time
    get_time(CurrentTime),
    % Check if the global variable is set
    (   nb_current(last_printed_time, _)
    ->  true
    ;   nb_setval(last_printed_time, CurrentTime)
    ),

    nb_getval(last_printed_time, LastPrintedTime),

    % Calculate the difference
    Diff is CurrentTime - LastPrintedTime,

    % If the difference is greater than or equal to 60 seconds (1 minute)
    (   Diff >= 60
    ->  % Print the heartbeat message and update the last printed time
        metta_stats
    ;   % Otherwise, do nothing
        true
    ).

metta_stats:- gc_now,
   writeln('\n\n\n\n\n\n;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   writeln(';~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   full_atom_count(SL),
   format("~N~n; Total\t\tAtoms (Atomspace size): ~`.t ~D~108|~n",[SL]),
   get_time(CurrentTime), nb_setval(last_printed_time, CurrentTime),
   post_statistic(memory,Mem),
   post_statistic(atom_space,AS),
   post_statistic(cputime,TotalSeconds),
   post_statistic(atoms,Concepts),
   flag(assert_new,CTs,CTs),
   post_statistic(stack,StackMem),


   PM is Mem + StackMem,
   RM is Mem-AS,
   PA is RM//(SL+1),
   APS is 60*floor(SL/(TotalSeconds+1)),
   ACS is AS//(Concepts+1),

   pl_stats('SymbolAtoms',Concepts),
   pl_stats('Random samples',CTs),
   skip((pl_stats('Bytes Per Atom (Average)',PA), pl_stats('Bytes Per ConceptNode (Average)',ACS))),
   skip((pl_stats('Relational Memory',RM), pl_stats('ConceptNode Memory',AS))),
   %pl_stats('Queryspace Memory',StackMem),
   %CPU is CPUTime-57600,
   format_time(TotalSeconds, Formatted),
   skip((pl_stats('Atoms per minute',APS))),
   pl_stats('Total Memory Used',PM),
   pl_stats('Runtime (days:hh:mm:ss)',Formatted),
   nl,nl,!.
metta_stats(F):- for_all(fb_pred(F,A),metta_stats(F,A)).
metta_stats(F,A):- metta_stats(F,A,NC), pl_stats(F/A,NC).
metta_stats(F,A,NC):- functor(P,F,A),predicate_property(P,number_of_clauses(NC)).
pl_stats(Stat):- statistics(Stat,Value),pl_stats(Stat,Value).
pl_stats(Stat,[Value|_]):- nonvar(Value),!, pl_stats(Stat,Value).
pl_stats(Stat,Value):- format("~N;\t\t~@: ~`.t ~@~100|",[format_value(Stat),format_value(Value)]),!.


% AsPred to print the formatted result.
format_value(Value) :- float(Value),!,format("~2f",[Value]),!.
format_value(Bytes) :- integer(Bytes),format_bytes(Bytes, Formatted), write(Formatted).
format_value(Term)  :- format("~w",[Term]).
%  Base case: If the number is 1G or more, show it in gigabytes (G).
format_bytes(Bytes, Formatted) :-  Bytes >= 1073741824, GB is Bytes / 1073741824, format(string(Formatted), '~2fG', [GB]).
% If the number is less than 1G, show it in megabytes (M).
format_bytes(Bytes, Formatted) :- Bytes >= 104857600, Bytes < 1073741824, !, MB is Bytes / 1048576, D is floor(MB), format(string(Formatted), '~DM', [D]).
% If the number is less than 1K, show it in bytes (B).
format_bytes(Bytes, Formatted) :- format(string(Formatted), '~D', [Bytes]).
% % If the number is less than 1M, show it in kilobytes (K).
%format_bytes(Bytes, Formatted) :- Bytes >= 1024, Bytes < 1048576, !, KB is Bytes / 1024, format(string(Formatted), '~0fK', [KB]).

% Convert total seconds to days, hours, minutes, seconds, and milliseconds.
format_time(TotalSeconds, Formatted) :-
    Seconds is floor(TotalSeconds),
    % Get days, remaining seconds
    Days is div(Seconds, 86400),
    Remain1 is mod(Seconds, 86400)-57600,
    format_time(string(Out),'%T',Remain1),
    % Format the result
    format(string(Formatted), '~w:~w', [Days, Out]).

% AsPred to print the formatted time.
print_formatted_time(TotalSeconds) :-
    format_time(TotalSeconds, Formatted),
    writeln(Formatted).


metta_final:-
    save_pre_statistic(memory),
    save_pre_statistic(atoms),
    save_pre_statistic(atom_space).
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
search_for1(X):-
  forall((metta_atom(_Where,What),contains_var(X,What)),
    (nl,write_src_nl(What))).

search_for2(X):-
  forall((metta_file_src(_Where,What),contains_var(X,What)),
    (nl,write_src_nl(What))).


metta_file_src(Where,What):-
  loaded_into_kb(Where,File), metta_file_buffer(0,_Ord,_Kind,What,Vars,File,_Loc),
  ignore(maplist(name_the_var,Vars)).


guess_metta_vars(What):-
  ignore(once((metta_file_buffer(0,_Ord,_Kind,What0,Vars,_File,_Loc),
     alpha_unify(What,What0),
     maplist(name_the_var,Vars)))).
name_the_var(N=V):- ignore((atom_concat('_',NV,N),V='$VAR'(NV))).

alpha_unify(What,What0):- What=@=What0,(nonvar(What)->What=What0;What==What0).
