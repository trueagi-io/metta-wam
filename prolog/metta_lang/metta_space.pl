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

:- discontiguous(user:metta_file_buffer/7).
:-     multifile(user:metta_file_buffer/7).
:-       dynamic(user:metta_file_buffer/7).

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
term_number(T, N) :- sub_term_safely(N, T), number(N).

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
'clear-atoms'(DynSpace) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
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
'add-atom'(DynSpace, Atom) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
    % Retrieve the method for adding an atom based on the space type.
  ((space_type_method(Type, add_atom, Method),
    % Ensure the space type matches by calling the type predicate.
    call(Type, SpaceNameOrInstance),
    !,
    % Log the operation if the space is not self-referential or asserted.
    if_t((SpaceNameOrInstance \== '&self' ; Type \== 'is_asserted_space'),
         dout(space, ['type-method', Type, Method, SpaceNameOrInstance, Atom])),
    % Invoke the method to add the atom to the space.
    call(Method, SpaceNameOrInstance, Atom))).

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
'remove-atom'(DynSpace, Atom) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
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
'replace-atom'(DynSpace, Atom, New) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
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

%!  'atom-count'(+Input, -Count) is det.
%
%   Counts the atoms in a space or environment. If `Input` is a space, it uses the
%   space's type-specific method. If it is an environment, it evaluates the arguments
%   to determine the count.
%
%   @arg Input A space name, space instance, or environment identifier.
%   @arg Count The number of atoms found.
%
%   @example
%     % Count the atoms in a space named 'my_space'.
%     ?- 'atom-count'('my_space', Count).
%     Count = 42.
%
%   @example
%     % Count the atoms in an environment named 'env'.
%     ?- 'atom-count'(env, Count).
%     Count = 10.
%
'atom-count'(DynSpace, Count) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
    dout(space, ['atom-count', SpaceNameOrInstance]),
    space_type_method(Type, atom_count, Method),
    call(Type, SpaceNameOrInstance), !,
    call(Method, SpaceNameOrInstance, Count),
    dout(space, ['type-method-result', Type, Method, Count]).
'atom-count'(Environment, Count) :-
    eval_args(['atom-count', Environment], Count).

%!  'get-atoms'(+Input, -Atoms) is det.
%
%   Retrieves atoms from either a space or an environment. The behavior depends on
%   the type of `Input`. If the input corresponds to a space (by name or instance),
%   it fetches atoms from that space by determining the appropriate retrieval method.
%   If the input corresponds to an environment, it evaluates the relevant arguments
%   to obtain the list of atoms.
%
%   @arg Input Can be either:
%        - A `SpaceNameOrInstance`: The name or instance of a space from which atoms are retrieved.
%        - An `Environment`: An environment identifier from which atoms will be fetched.
%   @arg Atoms A list that will unify with the atoms retrieved from the specified input.
%
%   @example
%     % Retrieve atoms from a space named 'example_space'.
%     ?- get-atoms('example_space', Atoms).
%     Atoms = [atom1, atom2, atom3].
%
%   @example
%     % Retrieve atoms from an environment named 'env1'.
%     ?- get-atoms('env1', Atoms).
%     Atoms = [atomA, atomB, atomC].
%
'get-atoms'(DynSpace, AtomsL) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
    % Output a debug message indicating the 'get-atoms' request to the space.
    dout(space, ['get-atoms', SpaceNameOrInstance]),
    % Determine the method for retrieving atoms based on the space type.
    space_type_method(Type, get_atoms, Method),
    % Call the type predicate to ensure the space is valid.
    call(Type, SpaceNameOrInstance),
    !,
    % Invoke the method to retrieve the atoms.
    call(Method, SpaceNameOrInstance, AtomsL),
    %dout(space,['type-method-result',Type,Method,Count]).
    %length(AtomsL,Count),
    true.
'get-atoms'(Environment, Atoms) :-
    % Evaluate the arguments to retrieve the atoms from the given environment.
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
'atoms_iter'(DynSpace, Iter) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
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
'atoms_match'(DynSpace, Atoms, Template, Else) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
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
'space_query'(DynSpace, QueryAtom, Result) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
    space_type_method(Type, query, Method),
    call(Type, SpaceNameOrInstance),
    !,
    call(Method, SpaceNameOrInstance, QueryAtom, Result),
    dout(space, ['type-method-result', Type, Method, Result]).

%!  subst_pattern_template(+SpaceNameOrInstance, +Pattern, -Template) is det.
%
%   Substitutes atoms in the given space according to the specified pattern, producing a template.
%   The matching process is handled by the `'atoms_match'/4` predicate.
%
%   @arg SpaceNameOrInstance The name or instance of the space to be processed.
%   @arg Pattern The pattern used to match atoms in the space.
%   @arg Template The result after applying the substitution based on the pattern.
%
%   @example
%     % Apply a pattern substitution in 'example_space'.
%     ?- subst_pattern_template('example_space', some_pattern, Template).
%     Template = [substituted_atom1, substituted_atom2].
%
subst_pattern_template(DynSpace, Pattern, Template) :-
    into_top_self(DynSpace, SpaceNameOrInstance),
    % Log the operation for traceability.
    dout(space, [subst_pattern_template, SpaceNameOrInstance, Pattern, Template]),
    % Match and substitute atoms in the given space according to the pattern.
    'atoms_match'(SpaceNameOrInstance, Pattern, Template, []).

/*
space_query_vars(SpaceNameOrInstance, Query, Vars) :- is_as_nb_space(SpaceNameOrInstance),!,
    fetch_or_create_space(SpaceNameOrInstance, Space),
    call_metta(Space,Query,Vars).
*/

:- dynamic(was_asserted_space/1).

%!  was_asserted_space(+SpaceName) is nondet.
%
%   Checks if the specified space was asserted. These facts indicate whether certain
%   spaces, identified by their names, have been registered or asserted in the system.
%
%   @arg SpaceName The name of the space being checked.
%
%   @example
%     % Check if the '&self' space was asserted.
%     ?- was_asserted_space('&self').
%     true.
%
was_asserted_space('&self'):- current_self(X), nocut, (X=='&self'->true;was_asserted_space(X)).
was_asserted_space('&stdlib').
was_asserted_space('&corelib').
was_asserted_space('&flybase').
was_asserted_space('&top').
was_asserted_space('&catalog').
was_asserted_space(ObjectID):- o_f_v(ObjectID, type, 'space').

/*
was_asserted_space('&attentional_focus').
was_asserted_space('&belief_events').
was_asserted_space('&goal_events').
was_asserted_space('&tempset').
was_asserted_space('&concepts').
was_asserted_space('&belief_events').
*/

%!  is_asserted_space(+Space) is nondet.
%
%   Succeeds if the given space has been asserted. A space is considered asserted if
%   it matches one of the known asserted spaces or does not qualify as a notebook
%   or Python-named space.
%
%   @arg Space The space to check.
%
%   @example
%     % Check if '&self' is an asserted space.
%     ?- is_asserted_space('&self').
%     true.
%
is_asserted_space(X) :- was_asserted_space(X).

/*
is_asserted_space(X) :-
    \+ is_as_nb_space(X),
    \+ py_named_space(X),
    !.
*/

%!  is_python_space_not_prolog(+Space) is nondet.
%
%   Succeeds if the given space is neither a Prolog notebook space nor an asserted space.
%   This predicate is used to identify spaces that are likely Python spaces.
%
%   @arg Space The space to check.
%
%   @example
%     % Check if 'unknown_space' is a Python space, not a Prolog space.
%     ?- is_python_space_not_prolog('unknown_space').
%     true.
%
is_python_space_not_prolog(X) :- is_python_space(X),
    \+ is_as_nb_space(X),
    \+ is_asserted_space(X).

% dynamic predicates, meaning they can be modified during execution (e.g., asserted or retracted).
:- dynamic(is_python_space/1).
:- dynamic(py_named_space/1).

%py_named_space('&self').
%py_named_space('&vspace').

% Function to check if an atom is registered as a space name

:- dynamic is_registered_space_name/1.

%!  is_as_nb_space(+Space) is nondet.
%
%   Succeeds if the given space is recognized as a notebook space. A space is
%   considered a notebook space if it matches the predefined `&nb` space,
%   or if it is a registered space name associated with a valid notebook space.
%
%   @arg Space The space to check.
%
%   @example
%     % Check if '&nb' is a notebook space.
%     ?- is_as_nb_space('&nb').
%     true.
%

% Check if the space is the predefined `&nb` space.
is_as_nb_space('&nb').
% Check if the space is valid through multiple conditions.
is_as_nb_space(G) :- is_valid_nb_space(G) -> true ;
    is_registered_space_name(G), nb_current(G, S), is_valid_nb_space(S).

%!  is_nb_space(+Space) is nondet.
%
%   Succeeds if the given space is a notebook space and the input is not a variable.
%
%   @arg Space The space to check.
%
%   @example
%     % Check if a given space 'G' is a notebook space.
%     ?- is_nb_space('&nb').
%     true.
%

% Ensure the input is non-variable and check if it is a notebook space.
is_nb_space(G) :- nonvar(G), is_as_nb_space(G).

% ============================
% %%%% Pattern Matching
% ============================
% Pattern Matching with an else branch
%'match'(Environment, Pattern, Template, ElseBranch, Result):-
%  eval_args(['match', Environment, Pattern, Template, ElseBranch], Result).
% Pattern Matching without an else branch

%!  'match'(+Environment, +Pattern, +Template, -Result) is det.
%
%   Matches the given pattern and template within the specified environment,
%   returning the result of the operation.
%
%   @arg Environment The environment where the matching takes place.
%   @arg Pattern The pattern to be matched.
%   @arg Template The template used for the match.
%   @arg Result The result of the match operation.
%
%   @example
%     % Perform a match operation within 'env1' with a pattern and template.
%     ?- 'match'('env1', some_pattern, some_template, Result).
%     Result = matched_result.
%

% Evaluate arguments for the 'match' operation within the environment.
'match'(Environment, Pattern, Template, Result) :-
    eval_args(['match', Environment, Pattern, Template], Result).
%'match'(_Environment, Pattern, Template, Result):- callable(Pattern),!, call(Pattern),Result=Template.
%'match'(_Environment, Pattern, Template, Result):- !, is_True(Pattern),Result=Template.

%!  'new-space'(-Space) is det.
%
%   Creates a new dynamic space with a unique name. The name is generated using
%   the `gensym/2` predicate and the space is either fetched or created.
%
%   @arg Space The newly created or fetched space.
%
%   @example
%     % Create a new dynamic space.
%     ?- 'new-space'(Space).
%     Space = some_unique_space_name.
%
% Generate a unique name and fetch or create the space.
'new-space'(Space) :-
    gensym('hyperon::space::DynSpace@_', Name),
    fetch_or_create_space(Name, Space).

:- dynamic(is_python_space/1).

% ===============================
% MeTTa Python incoming interface
% ===============================

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).

%!  space_type_method(+SpaceType, +Operation, -Method) is det.
%
%   Associates a space type with a specific operation and its corresponding method.
%   This mapping defines how different operations are handled for notebook spaces.
%
%   @arg SpaceType The type of space, such as `is_as_nb_space`.
%   @arg Operation The operation to be performed (e.g., `new_space`, `add_atom`).
%   @arg Method The method that implements the operation.
%
%   @example
%     % Find the method for adding an atom in a notebook space.
%     ?- space_type_method(is_as_nb_space, add_atom, Method).
%     Method = add_nb_atom.
%

% Define methods for various operations on notebook spaces.
space_type_method(is_as_nb_space, new_space, init_space).
space_type_method(is_as_nb_space, clear_space, clear_nb_atoms).
space_type_method(is_as_nb_space, add_atom, add_nb_atom).
space_type_method(is_as_nb_space, remove_atom, remove_nb_atom).
space_type_method(is_as_nb_space, replace_atom, replace_nb_atom).
space_type_method(is_as_nb_space, atom_count, atom_nb_count).
space_type_method(is_as_nb_space, get_atoms, get_nb_atoms).
% space_type_method(is_as_nb_space, get_atoms, arg(1)).
space_type_method(is_as_nb_space, atom_iter, atom_nb_iter).
% space_type_method(is_as_nb_space, query, space_nb_query).

%!  clear_nb_atoms(+SpaceNameOrInstance) is det.
%
%   Clears all atoms from the specified space by resetting its atom list to empty.
%
%   @arg SpaceNameOrInstance The space to clear.
%
%   @example
%     % Clear all atoms from 'example_space'.
%     ?- clear_nb_atoms('example_space').
%

% Clear all atoms from a space.
clear_nb_atoms(SpaceNameOrInstance) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    nb_setarg(1, Space, []).

%!  add_nb_atom(+SpaceNameOrInstance, +Atom) is det.
%
%   Adds an atom to the specified space by prepending it to the atom list.
%
%   @arg SpaceNameOrInstance The space to which the atom is added.
%   @arg Atom The atom to add.
%
%   @example
%     % Add an atom to 'example_space'.
%     ?- add_nb_atom('example_space', my_atom).
%

% Add an atom to the space.
add_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    NewAtoms = [Atom | Atoms],
    nb_setarg(1, Space, NewAtoms).

%!  atom_nb_count(+SpaceNameOrInstance, -Count) is det.
%
%   Counts the number of atoms in the specified space.
%
%   @arg SpaceNameOrInstance The space to query.
%   @arg Count The number of atoms found.
%
%   @example
%     % Count the atoms in 'example_space'.
%     ?- atom_nb_count('example_space', Count).
%     Count = 3.
%

% Count atoms in a space.
atom_nb_count(SpaceNameOrInstance, Count) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    length(Atoms, Count).

%!  remove_nb_atom(+SpaceNameOrInstance, +Atom) is nondet.
%
%   Removes the specified atom from the space's atom list if present.
%
%   @arg SpaceNameOrInstance The space from which to remove the atom.
%   @arg Atom The atom to remove.
%
%   @example
%     % Remove an atom from 'example_space'.
%     ?- remove_nb_atom('example_space', my_atom).
%
remove_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    select(Atom, Atoms, UpdatedAtoms),
    nb_setarg(1, Space, UpdatedAtoms).

%!  get_nb_atoms(+SpaceNameOrInstance, -Atoms) is det.
%
%   Retrieves all atoms from the specified space.
%
%   @arg SpaceNameOrInstance The space to query.
%   @arg Atoms The list of atoms retrieved.
%
%   @example
%     % Fetch atoms from 'example_space'.
%     ?- get_nb_atoms('example_space', Atoms).
%     Atoms = [atom1, atom2].
%
get_nb_atoms(SpaceNameOrInstance, Atoms) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms).

%!  replace_nb_atom(+SpaceNameOrInstance, +OldAtom, +NewAtom) is nondet.
%
%   Replaces an old atom with a new one in the space's atom list if the old atom is found.
%
%   @arg SpaceNameOrInstance The space where the replacement takes place.
%   @arg OldAtom The atom to be replaced.
%   @arg NewAtom The atom to insert in place of the old atom.
%
%   @example
%     % Replace an atom in 'example_space'.
%     ?- replace_nb_atom('example_space', old_atom, new_atom).
%
replace_nb_atom(SpaceNameOrInstance, OldAtom, NewAtom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    ( (select(Found, Atoms, TempAtoms), OldAtom =@= Found)
    ->  NewAtoms = [NewAtom | TempAtoms],
        nb_setarg(1, Space, NewAtoms)
    ;   false
    ).

%!  is_valid_nb_space(+Space) is nondet.
%
%   Confirms if the given term represents a valid notebook space.
%   A valid space must be a compound term with the functor 'Space'.
%
%   @arg Space The term to check.
%
%   @example
%     % Check if a term is a valid notebook space.
%     ?- is_valid_nb_space('Space'([])).
%     true.
%
is_valid_nb_space(Space) :-
    compound(Space),
    functor(Space, 'Space', _).

%!  space_original_name(+Space, -Name) is nondet.
%
%   Retrieves the original name of a given space by checking if the name
%   is registered and corresponds to the provided space instance.
%
%   @arg Space The space instance whose original name is to be found.
%   @arg Name The original name of the space.
%
%   @example
%     % Find the original name of a space.
%     ?- space_original_name('Space'([]), Name).
%
space_original_name(Space, Name) :-is_registered_space_name(Name),nb_current(Name, Space).

%!  init_space(+Name) is det.
%
%   Registers and initializes a new space with the given name.
%   The new space is represented as 'Space'([]).
%
%   @arg Name The name of the space to be initialized.
%
%   @example
%     % Initialize a new space named 'example_space'.
%     ?- init_space('example_space').
%
init_space(Name) :-
    Space = 'Space'([]),
    % Register the space name in the system.
    asserta(is_registered_space_name(Name)),
    % Store the newly created space under the registered name.
    nb_setval(Name, Space).

%!  fetch_or_create_space(+NameOrInstance) is det.
%
%   Fetches an existing space or creates a new one if it doesn't exist.
%   The space can be referred to by its name or passed directly as an instance.
%
%   @arg NameOrInstance The name or instance of the space to fetch or create.
%
%   @example
%     % Fetch or create a space named 'example_space'.
%     ?- fetch_or_create_space('example_space').
%
fetch_or_create_space(Name) :- fetch_or_create_space(Name, _).

%!  fetch_or_create_space(+NameOrInstance, -Space) is det.
%
%   Fetches the specified space or creates a new one if it does not exist.
%   If the input is an atom (name), it checks if the space is registered
%   and initializes it if necessary. If the input is already a valid space
%   instance, it is returned directly.
%
%   @arg NameOrInstance The name or instance of the space to fetch or create.
%   @arg Space The fetched or created space instance.
%
%   @example
%     % Fetch an existing space or create a new one if necessary.
%     ?- fetch_or_create_space('example_space', Space).
%     Space = 'Space'([]).
%
fetch_or_create_space(NameOrInstance, Space) :-
    (   atom(NameOrInstance)
    % If the name is registered, fetch the associated space.
    ->  (is_registered_space_name(NameOrInstance)
        ->  nb_current(NameOrInstance, Space)
        % Otherwise, initialize a new space and fetch it.
        ;   init_space(NameOrInstance),
            nb_current(NameOrInstance, Space))
    % If the input is already a valid space instance, return it directly.
    ;   is_valid_nb_space(NameOrInstance)
    ->  Space = NameOrInstance
    % Print an error message for invalid input.
    ;   writeln('Error: Invalid input.')
    ),
    % Ensure the fetched or created entity is a valid space.
    is_valid_nb_space(Space).

%!  'match'(+Space, +Pattern, -Template) is det.
%
%   Matches a pattern against the atoms in the specified space and produces a template.
%
%   @arg Space The space in which to match the pattern.
%   @arg Pattern The pattern to match against the atoms.
%   @arg Template The result template produced from the match.
%
%   @example
%     % Match a pattern in a space and produce a template.
%     ?- 'match'('example_space', some_pattern, Template).
%

% Retrieve all atoms from the space and perform the pattern match.
'match'(Space, Pattern, Template) :-
    'get-atoms'(Space, Atoms),
    'match-pattern'(Atoms, Pattern, Template).

%!  'match-pattern'(+Atoms, +Pattern, -Template) is nondet.
%
%   Performs a simple pattern match on a list of atoms. If the pattern matches
%   any atom in the list, it produces the matched atom as the template.
%
%   @arg Atoms The list of atoms to search through.
%   @arg Pattern The pattern to be matched.
%   @arg Template The result of the match, if found.
%
%   @example
%     % Perform a simple match on a list of atoms.
%     ?- 'match-pattern'([a, b, c], b, Template).
%     Template = b.
%

% If the atom list is empty, the match fails, and the template is empty.
'match-pattern'([], _, []).
% If the head of the list matches the pattern, return it as the template.
'match-pattern'([H |_T], H, H) :-
    !.  % Cut to prevent backtracking after a successful match.
% If the head does not match, continue matching with the tail.
'match-pattern'([_H| T], Pattern, Template) :-
    'match-pattern'(T, Pattern, Template).

%is_python_space(X):- python_object(X).

%!  ensure_space(+X, -Y) is nondet.
%
%   Ensures that the given space is available by attempting to execute
%   `ensure_space_py/2`. If an exception occurs, it fails gracefully.
%
%   @arg X The input related to the space being ensured.
%   @arg Y The result or output associated with the space operation.
%
%   @example
%     % Ensure the space 'example_space' is available.
%     ?- ensure_space('example_space', Result).
%

% Attempt to ensure the space using `ensure_space_py/2`.
% Catch any exception and fail gracefully.
ensure_space(X, Y) :- catch(ensure_space_py(X, Y), _, fail),!.
% If the first clause fails, this fallback clause ensures that
% the predicate as a whole fails.
ensure_space(_N, _V) :- fail.

% ===============================
% Clause Database interface
% ===============================
%dout(space,Call):- skip(Call).

%!  if_metta_debug(+Goal) is det.
%
%   Executes the given `Goal` if the environment variable `VSPACE_VERBOSE` is set to '2'.
%   Otherwise, the goal is ignored. This predicate is useful for conditional debugging.
%
%   @arg Goal The goal to be executed for debugging purposes.
%
%   @example
%     % Run a goal in debug mode if the environment allows it.
%     ?- if_metta_debug(write('Debug message')).
%

% Check if 'VSPACE_VERBOSE' is set to '2', and if so, call the goal.
if_metta_debug(Goal) :- getenv('VSPACE_VERBOSE', '2'), !, ignore(call(Goal)).
% If the condition fails, succeed silently.
if_metta_debug(_) :- !.
% If the condition is bypassed by previous clauses, call the goal and ignore failures.
if_metta_debug(Goal) :- !, ignore(call(Goal)).

%!  dout(+Tag, +Term) is det.
%
%   Outputs debugging information if `if_metta_debug/1` allows it.
%   Uses `notrace/1` to ensure no side effects from tracing.
%
%   @arg Tag A tag or label to identify the type of debug output.
%   @arg Term The term to be printed.
%
%   @example
%     % Print debug information with a tag.
%     ?- dout(info, some_debug_data).
%

% Succeed silently if debugging is not active.
dout(_, _) :- !.
% Output formatted debugging information if enabled.
dout(W, Term) :- notrace(if_metta_debug((format('~N; ~w ~@~n', [W, write_src(Term)])))).

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).

%!  space_type_method(+SpaceType, +Operation, -Method) is det.
%
%   Defines the mapping between a space type (`is_asserted_space`) and the corresponding
%   methods used to handle various operations on that space. This mapping ensures that
%   operations like adding, removing, or retrieving atoms are correctly delegated to
%   the appropriate methods.
%
%   @arg SpaceType The type of the space (e.g., `is_asserted_space`).
%   @arg Operation The operation to perform (e.g., `new_space`, `add_atom`).
%   @arg Method The method implementing the operation.
%
%   @example
%     % Find the method for adding an atom in an asserted space.
%     ?- space_type_method(is_asserted_space, add_atom, Method).
%     Method = metta_assertdb_add.
%

% Map the 'new_space' operation to the 'init_space' method for asserted spaces.
space_type_method(is_asserted_space, new_space, init_space).
% Map the 'clear_space' operation to the 'clear_nb_atoms' method for asserted spaces.
space_type_method(is_asserted_space, clear_space, clear_nb_atoms).
% Map the 'add_atom' operation to the 'metta_assertdb_add' method for asserted spaces.
space_type_method(is_asserted_space, add_atom, metta_assertdb_add).
% Map the 'remove_atom' operation to the 'metta_assertdb_rem' method for asserted spaces.
space_type_method(is_asserted_space, remove_atom, metta_assertdb_rem).
% Map the 'replace_atom' operation to the 'metta_assertdb_replace' method for asserted spaces.
space_type_method(is_asserted_space, replace_atom, metta_assertdb_replace).
% Map the 'atom_count' operation to the 'metta_assertdb_count' method for asserted spaces.
space_type_method(is_asserted_space, atom_count, metta_assertdb_count).
% Map the 'get_atoms' operation to the 'metta_assertdb_get_atoms' method for asserted spaces.
space_type_method(is_asserted_space, get_atoms, metta_assertdb_get_atoms).
% Map the 'atom_iter' operation to the 'metta_assertdb_iter' method for asserted spaces.
space_type_method(is_asserted_space, atom_iter, metta_assertdb_iter).
%space_type_method(is_asserted_space,query,space_nb_query).

%:- dynamic(for_metta/2).
%for_metta(_,T):- fb_pred(F,A),functor(T,F,A),call(T).

%!  metta_assertdb_ls(+KB) is det.
%
%   Lists all asserted atoms in the specified knowledge base (KB).
%
%   @arg KB The knowledge base from which to list asserted atoms.
%
%   @example
%     % List atoms in the 'example_kb' knowledge base.
%     ?- metta_assertdb_ls('example_kb').
%
metta_assertdb_ls(KB) :-
    % Predicate for asserted atoms.
    AMA = metta_atom_asserted,
    % Declare the predicate for the asserted atoms with arity 2.
    decl_m_fb_pred(user, AMA, 2),
    % Construct the predicate term dynamically.
    MP =.. [AMA, KB, _],
    % List the matching predicates.
    listing(MP).

%!  metta_assertdb_add(+KB, +AtomIn) is det.
%
%   Adds a new atom to the specified knowledge base (KB).
%
%   @arg KB The knowledge base to which the atom is added.
%   @arg AtomIn The atom to add, with variables substituted as needed.
%
%   @example
%     % Add an atom to the 'example_kb' knowledge base.
%     ?- metta_assertdb_add('example_kb', my_atom).
%
metta_assertdb_add(KB, AtomIn) :-
    must_det_ll((
        % Substitute variables in the atom to produce a standardized version.
        subst_vars(AtomIn, Atom),
        % Predicate for asserted atoms.
        AMA = metta_atom_asserted,
        % Declare the predicate for the asserted atoms with arity 2.
        decl_m_fb_pred(user, AMA, 2),
        % Construct the predicate term dynamically.
        MP =.. [AMA, KB, Atom],
        % Assert the new predicate.
        assert_new(MP)
    )).

%!  metta_assertdb_rem(+KB, +Old) is det.
%
%   Removes an atom from the specified knowledge base (KB).
%
%   @arg KB The knowledge base from which the atom is removed.
%   @arg Old The atom to remove.
%
%   @example
%     % Remove an atom from the 'example_kb' knowledge base.
%     ?- metta_assertdb_rem('example_kb', my_atom).
%
metta_assertdb_rem(KB, Old) :-
    % Delegate to metta_assertdb_del/2 for removal.
    metta_assertdb_del(KB, Old).

%!  metta_assertdb_del(+KB, +Atom) is det.
%
%   Deletes the specified atom from the knowledge base (KB) if it exists.
%
%   @arg KB The knowledge base from which the atom is deleted.
%   @arg Atom The atom to delete, with variables substituted.
%

% Substitute variables and delete the atom from the knowledge base.
metta_assertdb_del(KB, Atom) :-
    % Substitute variables in the atom.
    subst_vars(Atom, Old),
    % Declare the predicate for the asserted atoms with arity 2.
    decl_m_fb_pred(user, metta_atom_asserted, 2),
    % Construct the predicate term dynamically.
    MP = metta_atom(KB, Old),
    % Create a copy of the term for comparison.
    copy_term(MP, Copy),
    % Retrieve the clause reference.
    clause(MP, true, Ref),
    % Ensure structural equality between terms.
    MP =@= Copy,
    !,  % Cut to prevent backtracking.
    % Erase the clause reference to remove the atom.
    erase(Ref).
    % ,metta_assertdb('DEL',Old).  % (Commented-out code left untouched)

%!  metta_assertdb_replace(+KB, +Old, +New) is det.
%
%   Replaces an old atom with a new one in the specified knowledge base (KB).
%
%   @arg KB The knowledge base in which the replacement occurs.
%   @arg Old The atom to be replaced.
%   @arg New The atom to insert in place of the old one.
%
metta_assertdb_replace(KB, Old, New) :-
    % Delete the old atom.
    metta_assertdb_del(KB, Old),
    % Add the new atom.
    metta_assertdb_add(KB, New).

%!  atom_count_provider(+Input, -Count) is det.
%
%   Provides the atom count for the given knowledge base (KB) or context.
%   If the input refers to a context, it calculates the count based on the
%   predicates associated with that context. If the input is a KB, it calculates
%   the total count using clause and rule properties, along with other atom counts.
%
%   @arg Input The knowledge base or context to query.
%   @arg Count The number of atoms found.
%
%   @example
%     % Get the atom count for a specific context.
%     ?- atom_count_provider('example_kb', Count).
%     Count = 42.
%
%   @example
%     % Get the atom count for a loaded context.
%     ?- atom_count_provider(some_context, Count).
%


atom_count_provider(SpaceNameOrInstance, Count) :-
    into_top_self(SpaceNameOrInstance, DynSpace),
    % Check if the context has been loaded into a knowledge base (KB).
    user:loaded_into_kb(DynSpace, Filename),
    % Retrieve the associated predicate for the given filename.
    once(user:asserted_metta_pred(Mangle, Filename)),
    % Derive a related predicate from the original.
    mangle_iz(Mangle, Iz),
    % Select a predicate from the possible options.
    member(P, [Mangle, Iz]),
    % Ensure the arity is between 2 and 8.
    between(2, 8, Arity),
    % Construct the predicate term dynamically.
    functor(Data, P, Arity),
    % Retrieve the number of clauses for the predicate.
    predicate_property(Data, number_of_clauses(CC)),
    % Retrieve the number of rules for the predicate.
    predicate_property(Data, number_of_rules(RC)),
    % Calculate the atom count as the difference between clauses and rules.
    Count is CC - RC, !.
atom_count_provider(SpaceNameOrInstance, Count) :-
    must_det_ll((
        into_top_self(SpaceNameOrInstance, KB),
        % Predicate for asserted atoms.
        AMA = metta_atom_asserted,
        % Declare the predicate with arity 2.
        decl_m_fb_pred(user, AMA, 2),
        % Construct the predicate term dynamically.
        MP =.. [AMA, KB, _],
        % Get the number of clauses for the predicate.
        predicate_property(MP, number_of_clauses(SL2)),
        % Get the number of rules for the predicate.
        predicate_property(MP, number_of_rules(SL3)),
        % Retrieve the full atom count.
        %metta_assertdb_ls(KB),  % (Commented-out code left unchanged)
        full_atom_count(SL1),
        % Calculate the final atom count.
        Count is SL1 + SL2 - SL3
    )), !.

%!  metta_assertdb_count(+KB, -Count) is det.
%
%   Counts the total number of atoms in the specified knowledge base (KB).
%   It aggregates the counts from multiple sources using `atom_count_provider/2`.
%
%   @arg KB The knowledge base to query.
%   @arg Count The total number of atoms found.
%
%   @example
%     % Get the total atom count for a knowledge base.
%     ?- metta_assertdb_count('example_kb', Count).
%
metta_assertdb_count(KB, Count) :-
    % Collect all counts from the knowledge base.
    findall(C, atom_count_provider(KB, C), CL),
    % Sum the counts to get the total atom count.
    sumlist(CL, Count).
%metta_assertdb_count(KB,Count):- writeln(metta_assertdb_count_in(KB,Count)), findall(Atom,for_metta(KB,Atom),AtomsL),length(AtomsL,Count),writeln(metta_assertdb_count_out(KB,Count)).

%!  metta_assertdb_iter(+KB, -Atoms) is nondet.
%
%   Iterates over the atoms in the specified knowledge base (KB).
%   This predicate dynamically constructs the predicate for atoms
%   in the given KB and calls it to retrieve matching atoms.
%
%   @arg KB The knowledge base to iterate over.
%   @arg Atoms The atoms found during the iteration.
%
%   @example
%     % Iterate over atoms in 'example_kb' and retrieve them.
%     ?- metta_assertdb_iter('example_kb', Atom).
%
metta_assertdb_iter(SpaceNameOrInstance, Atoms) :-
    into_top_self(SpaceNameOrInstance, KB),
    % Dynamically construct the predicate for the given KB.
    MP =.. [metta_atom, KB, Atoms],
    % Call the constructed predicate to retrieve atoms.
    call(MP).

%!  metta_iter_bind(+KB, +Query, -Vars, +VarNames) is det.
%
%   Binds the variables in a query against the specified knowledge base (KB).
%   It aligns the variable names with the variables from the query and logs
%   the matching process.
%
%   @arg KB The knowledge base to query.
%   @arg Query The query to execute against the knowledge base.
%   @arg Vars The variables bound by the query.
%   @arg VarNames The names of the variables to align with the query variables.
%
%   @example
%     % Execute a query against the KB and bind variables.
%     ?- metta_iter_bind('example_kb', my_query(X), Vars, ['X']).
%
metta_iter_bind(SpaceNameOrInstance, Query, Vars, VarNames) :-
    into_top_self(SpaceNameOrInstance, KB),
    % Extract all variables from the query.
    term_variables(Query, QVars),
    % Align the provided variable names with the query variables.
    align_varnames(VarNames, Vars),
    % Log the query matching process (used for debugging).
    TV = dout(space, ['match', KB, Query, QVars, Vars, VarNames]),
    % \+ \+ (numbervars(TV, 0, _, []), print(tv = TV), nl),
    % Ensure the query variables match the provided variables.
    ignore(QVars = Vars),
    % \+ \+ (numbervars(TV, 0, _, []), print(qv = TV), nl),
    % Log the aligned variables and ensure the query succeeds.
    \+ \+ (
        % numbervars(TV, 0, _, []),  % Another debugging block (commented-out).
        writeq(av = TV), nl
    ),
    % Execute the query against the KB and ensure the result is not 'False'.
    space_query_vars(KB, Query, TF), TF \== 'False'.

%!  space_query_vars(+KB, +Query, -Vars) is det.
%
%   Queries the specified knowledge base (KB) and retrieves the variables
%   bound by the query. If the KB is an asserted space, it uses the predicate
%   `metta_atom_asserted/2` to execute the query.
%
%   @arg KB The knowledge base to query.
%   @arg Query The query to execute.
%   @arg Vars The variables bound by the query result.
%
%   @example
%     % Query the KB and retrieve bound variables.
%     ?- space_query_vars('example_kb', my_query(X), Vars).
%
space_query_vars(SpaceNameOrInstance, Query, Vars) :-
    into_top_self(SpaceNameOrInstance, KB),
    % Check if the knowledge base is an asserted space.
    is_asserted_space(KB), !,
    % Declare the predicate for asserted atoms with arity 2.
    decl_m_fb_pred(user, metta_atom_asserted, 2),
    % Execute the query with the provided variables.
    call_metta(KB, Query, Vars),
    % Log the result of the query.
    dout('RES', space_query_vars(KB, Query, Vars)).


%!  metta_assertdb_get_atoms(+KB, -Atom) is nondet.
%
%   Retrieves atoms from the specified knowledge base (KB). This predicate
%   iterates over the atoms in the KB by calling the `metta_atom/2` predicate.
%
%   @arg KB The knowledge base from which to retrieve atoms.
%   @arg Atom The atoms found in the specified KB.
%
%   @example
%     % Retrieve atoms from the 'example_kb' knowledge base.
%     ?- metta_assertdb_get_atoms('example_kb', Atom).
%
metta_assertdb_get_atoms(KB, Atom) :-
    % Call the predicate to retrieve atoms from the KB.
    metta_atom(KB, Atom).

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


%!  align_varnames(+VarNames, -Vars) is det.
%
%   Aligns the given variable names with the variables provided.
%   Converts the list of variable names into a set and merges them with
%   the variables.
%
%   @arg VarNames The list of variable names to align.
%   @arg Vars The aligned variables.
%
%   @example
%     % Align variables with names.
%     ?- align_varnames(['X', 'Y'], Vars).
%
align_varnames(VarNames, Vars) :-
    % Convert the list of variable names to a set.
    list_to_set(VarNames, NameSet),
    % Merge the named variables with the variable names.
    merge_named_vars(NameSet, VarNames, Vars).

%!  merge_named_vars(+NameSet, +VarNames, -Vars) is det.
%
%   Merges a set of variable names with their corresponding variables.
%
%   @arg NameSet The set of unique variable names.
%   @arg VarNames The list of variable names.
%   @arg Vars The resulting list of variables.
%
merge_named_vars([], _VarNames, _Vars) :- !.
merge_named_vars([N | NameSet], VarNames, Vars) :-
    % Merge the current variable name with its corresponding variable.
    merge_named(N, _V, VarNames, Vars),
    % Recursively merge the remaining variable names.
    merge_named_vars(NameSet, VarNames, Vars).
%merge_named_vars(_, _, _).  % (Commented-out code left unchanged)

%!  merge_named(+Name, -Var, +VarNames, -Vars) is det.
%
%   Merges a single named variable with the corresponding variable list.
%   It matches the given name with elements from the variable names list
%   and binds the corresponding variables.
%
%   @arg Name The name of the variable to merge.
%   @arg Var The variable associated with the name.
%   @arg VarNames The list of variable names.
%   @arg Vars The list of variables corresponding to the names.
%
%   @example
%     % Merge a variable named 'X' with its corresponding variables.
%     ?- merge_named('X', V, ['X', 'Y'], [VarX, VarY]).
%     V = VarX.
%
merge_named(_, _, [], []) :-
    % Base case: When both lists are empty, stop merging.
    !.
merge_named(N, V, [N | VarNames], [V | Vars]) :-
    % When the head of the names list matches the given name,
    % associate the corresponding variable and continue merging.
    merge_named(N, V, VarNames, Vars).

%!  call_metta(+KB, +Query, -Vars) is det.
%
%   Executes a query against the specified knowledge base (KB).
%   If the KB contains the query as an asserted atom, it succeeds.
%   Otherwise, it converts the query to a PySWIP call and executes it.
%
%   @arg KB The knowledge base to query.
%   @arg Query The query to execute.
%   @arg Vars The variables involved in the query.
%
call_metta(KB, Query, _Vars) :-
    % Execute the query directly if it matches an asserted atom.
    metta_atom(KB, Query), nocut.
call_metta(_KB, Query, _Vars) :-
    % Convert the query to a PySWIP-compatible call.
    metta_to_pyswip([], Query, Call), !,
    % Execute the PySWIP call.
    %print(user:Call), nl,  % (Commented-out code left unchanged)
    user:call(Call).

%!  metta_to_pyswip(+PS, +Query, -Call) is det.
%
%   Converts a query into a PySWIP-compatible call.
%   Handles variables, non-compound terms, lists, and compound terms.
%
%   @arg PS A list of predicates used during conversion.
%   @arg Query The query to convert.
%   @arg Call The resulting PySWIP-compatible call.
%
metta_to_pyswip(_PS, Query, Call) :-
    % If the query is a variable, return it directly.
    var(Query), !, Call = Query.
metta_to_pyswip(_PS, Query, Call) :-
    % If the query is not compound, return it directly.
    \+ compound(Query), !, Call = Query, !.
metta_to_pyswip(PS, Query, Call) :-
    % Handle list queries by converting the head and tail.
    is_list(Query), Query = [Q | Uery], !,cmpd_to_pyswip(PS, Q, Uery, Call).
metta_to_pyswip(PS, Query, Call) :-
    % Handle compound queries by decomposing them into functors and arguments.
    Query =.. [Q | Uery], cmpd_to_pyswip(PS, Q, Uery, Call).

%!  cmpd_to_pyswip(+PS, +Q, +Uery, -Call) is det.
%
%   Converts a compound term into a PySWIP-compatible call.
%   Handles atom-based queries and "and" conjunctions.
%
%   @arg PS A list of predicates used during conversion.
%   @arg Q The functor of the compound term.
%   @arg Uery The arguments of the compound term.
%   @arg Call The resulting PySWIP-compatible call.
%
cmpd_to_pyswip(PS, Q, Uery, Call) :-
    % Handle atom-based queries by mapping their arguments recursively.
    atom(Q), maplist(metta_to_pyswip([Q | PS]), Uery, Cery), Call =.. [Q | Cery],!.
cmpd_to_pyswip(PS, "and", Uery, Call) :-
    % Handle "and" conjunctions by converting the list to conjuncts.
    maplist(metta_to_pyswip(PS), Uery, Args),list_to_conjuncts(Args, Call).

%!  'show-metta-def'(+Pred, +Args) is det.
%
%   Displays the Metta definition of the given predicate.
%   It retrieves the source code for the predicate and prints each line.
%
%   @arg Pred The predicate whose definition is displayed.
%   @arg Args Additional arguments (usually an empty list).
%
%   @example
%     % Show the Metta definition for a predicate.
%     ?- 'show-metta-def'(some_predicate, []).
%
'show-metta-def'(Pred, []) :-
    % Retrieve the Metta source code for the predicate.
    'get-metta-src'(Pred, [_ | SrcL]),
    % Print each line of the source code.
    maplist(write_src_nl, SrcL).


%'get-metta-src'(Pred,[Len|SrcL]):-
%    findall(['AtomDef',Src], 'get-metta-src1'(Pred,Src), SrcL),
%    length(SrcL, Len).

%!  'get-metta-src'(+Pred, -Src) is det.
%
%   Retrieves the source code for the given predicate.
%   It collects all matching sources into a list and calculates the length.
%
%   @arg Pred The predicate whose source is being retrieved.
%   @arg Src The list containing the length of the source and the source lines.
%
'get-metta-src'(Pred, [Len | SrcL]) :-
    % Find all source lines matching the predicate.
    findall(Src, 'get-metta-src1'(Pred, Src), SrcL),
    % Calculate the length of the source list.
    length(SrcL, Len).

%!  'get-metta-src1'(+Pred, -Src) is det.
%
%   Retrieves individual source lines for the given predicate.
%   It checks if the predicate matches the functor or arguments of an atom.
%
%   @arg Pred The predicate to search for.
%   @arg Src The source line corresponding to the predicate.
%
'get-metta-src1'(Pred, Src) :-
    % Retrieve the current space.
    current_self(Space),
    % Retrieve an atom from the current space.
    metta_atom(Space, F, A, List),
    % Check if the predicate matches the arguments or the functor.
    once((
        sub_var_safely(Pred, A) -> Src = [F, A, List]
    ;   sub_var_safely(Pred, F) -> Src = [F, A | List]
    )).

%!  'AtomDef'(+X, -Def) is det.
%
%   Creates a self-referential structure (a quine) for the given term.
%
%   @arg X The input term.
%   @arg Def The quine structure containing the input term.
%
'AtomDef'(X, ['AtomDef', X]).

%!  sort_on(+Comparator, -Relation, +A, +B) is det.
%
%   Compares two elements `A` and `B` using the provided comparator.
%   If the elements are equal, the result is `=`. Otherwise, the comparator
%   is applied to both elements, and the results are compared lexicographically.
%
%   @arg Comparator The predicate used to compare the elements.
%   @arg Relation The resulting relation (`<`, `=`, or `>`).
%   @arg A The first element to compare.
%   @arg B The second element to compare.
%
%   @example
%     % Compare two atoms by length using atom_length/2 as the comparator.
%     ?- sort_on(atom_length, R, 'hello', 'hi').
%     R = (>).
%
%   @example
%     % Compare two lists based on their length.
%     ?- sort_on(length, R, [1,2,3], [1,2]).
%     R = (>).
%
sort_on(C, R, A, B) :-
    (A == B -> R = (=)
    ;   must_det_ll((
            % Apply the comparator to both elements.
            call(C, A, AA),
            call(C, B, BB),
            % Compare the results lexicographically.
            !, compare(R, AA + A, BB + B)
        ))
    ),
    !.

%!  tokens(+Input, -Result) is det.
%
%   Tokenizes the given input atom using multiple tokenizers.
%   The most relevant tokenized result is selected based on sorting by length.
%
%   @arg Input The input atom to tokenize.
%   @arg Result The most relevant tokenized result.
%
%   @example
%     % Tokenize the input atom 'hello-world'.
%     ?- tokens('hello-world', VL).
%     VL = ['hello', 'world'].
%
%   @example
%     % Tokenize the input 'Prolog'.
%     ?- tokens('Prolog', VL).
%     VL = ['Prolog'].
%
tokens(X, VL) :-
    % Unaccent the input atom.
    unaccent_atom(X, A),
    !,
    % Collect tokenized results using all available tokenizers.
    findall(E, (is_tokenizer(T), call(T, A, E)), L),
    % Sort the tokens based on their length.
    predsort(sort_on(length_fw_len), L, S),
    % Select the last (best) tokenized result.
    last(S, VL).

%!  length_fw_len(+List, -Score) is det.
%
%   Computes a sorting score for a tokenized list. The score is the sum
%   of the length of the list and the length of the first element.
%
%   @arg List The tokenized list to score.
%   @arg Score The computed score.
%
%   @example
%     % Compute the score for a tokenized list.
%     ?- length_fw_len(['hello', 'world'], Score).
%     Score = 2 + 5.
%
%   @example
%     % Compute the score for a single-element list.
%     ?- length_fw_len(['Prolog'], Score).
%     Score = 0 + 6.
%
length_fw_len([W | List], L + WL) :-
    % Calculate the length of the list (excluding the first element).
    length(List, L),
    % Calculate the length of the first element.
    atom_length(W, WL).

%!  print_token_args is det.
%
%   Prints tokenized arguments from the knowledge base, excluding dashes.
%   It retrieves arguments, tokenizes them, excludes dash tokens, converts
%   them to terms, and prints the results.
%
%   @example
%     % Execute the printing of tokenized arguments.
%     ?- print_token_args.
%
print_token_args :-
    make,  % Compile necessary files.
    fb_arg(X),  % Retrieve arguments from the knowledge base.
    tokens(X, A0),  % Tokenize the arguments.
    exclude(is_dash, A0, A),  % Exclude dash tokens.
    tterm(A, AT),  % Convert the tokens to a term.
    % Print the argument, tokenized term, and original token list.
    writeq(X), write('    '), writeq(AT), write('  '), write_src(A), nl,
    fail.

%!  is_dash(+Atom) is nondet.
%
%   Checks if the given atom is a dash ('_' or '-').
%
%   @arg Atom The atom to check.
%
%   @example
%     % Check if an atom is a dash.
%     ?- is_dash('_').
%     true.
%
%     ?- is_dash('x').
%     false.
%
is_dash('_').
is_dash('-').

%!  tterm(+List, -Term) is det.
%
%   Converts a list of tokens into a term. If the list contains specific
%   separators like colons, the tokens are rearranged into terms accordingly.
%
%   @arg List The list of tokens to convert.
%   @arg Term The resulting term.
%
%   @example
%     % Convert a list with a colon separator to a term.
%     ?- tterm(['fun', ':', 'arg'], Term).
%     Term = fun(arg).
%
%     % Convert a list of tokens to a term.
%     ?- tterm(['hello', 'world'], Term).
%     Term = world(hello).
%

% If the list contains a single element, return it as the term.
tterm([A], A) :- !.
% If the list starts with an atom followed by a colon,
% construct a term using the atom as the functor and the rest as arguments.
tterm([A, ':', B | M], BA) :- atom(A), !, BA =.. [A, B | M].
% If the list starts with two atoms, use the second atom as the functor
% and the first as the first argument, followed by the rest.
tterm([A, B | M], BA) :- atom(B), !, BA =.. [B, A | M].
% If the list starts with an atom, build a term with the atom and the rest of the list.
tterm([A | B], BA) :- atom(A), !, BA =.. [B | A].
% If none of the above patterns match, return the input as-is.
tterm(A, A).

%!  is_tokenizer(+Tokenizer) is nondet.
%
%   Defines available tokenizers that can be used on input atoms.
%   These tokenizers process input atoms in various ways to extract meaningful tokens.
%
%   @example
%     % Check if 'tokenize_atom' is a valid tokenizer.
%     ?- is_tokenizer(tokenize_atom).
%     true.
%
is_tokenizer(into_list).
is_tokenizer(to_case_break_atoms).
is_tokenizer(atom_to_stem_list).
is_tokenizer(tokenize_atom).
%is_tokenizer(double_metaphone).

%!  is_an_arg_type(+String, -Type) is nondet.
%
%   Checks if the given string corresponds to a known argument type by querying
%   the FlyBase system.
%
%   @arg String The input string to check.
%   @arg Type The identified type of the argument.
%
%   @example
%     % Check if 'FBgn12345' is a valid FlyBase identifier.
%     ?- is_an_arg_type('FBgn12345', Type).
%
is_an_arg_type(S, T) :-
    % Look up the identifier in the FlyBase system.
    flybase_identifier(S, T),
    !.

%!  has_type(+String, -Type) is nondet.
%
%   Checks if the given string has a specific type by matching its prefix.
%   It extracts the first four characters of the input string and checks
%   against known FlyBase types.
%
%   @arg String The input string to check.
%   @arg Type The identified type.
%
%   @example
%     % Check if 'FBgn...' has a valid type.
%     ?- has_type('FBgn12345', Type).
%
has_type(S, Type) :-
    % Extract the first 4 characters (prefix).
    sub_atom(S, 0, 4, Aft, FB),
    % Check if the prefix matches a known FlyBase type.
    flybase_identifier(FB, Type),
    !,
    % Ensure there is more to the string beyond the prefix.
    Aft > 0.

%!  call_sexpr(+SExpr) is det.
%
%   Executes a symbolic expression (SExpr) and prints the result.
%   This predicate ensures that the expression is only printed once.
%
%   @arg SExpr The symbolic expression to execute.
%
%   @example
%     % Execute and print a symbolic expression.
%     ?- call_sexpr('(print "Hello World")').
%
call_sexpr(S) :-
    % Print the result only once.
    once_writeq_nl(call_sexpr(S)).
%call_sexpr(Space, Expr, Result) :-

:- dynamic(fb_pred/2).

%!  full_atom_count(-Count) is det.
%
%   Retrieves the total number of loaded atoms. If the flag `total_loaded_atoms`
%   is greater than 1, it returns that count. Otherwise, it calculates the count
%   by summing all predicate statistics.
%
%   @arg Count The total number of atoms loaded.
%
%   @example
%     % Get the total count of atoms.
%     ?- full_atom_count(Count).
%
full_atom_count(SL) :-
    % Retrieve the current atom count from the flag.
    flag(total_loaded_atoms, SL, SL),
    % Ensure there is more than one loaded atom.
    SL > 1,!.
full_atom_count(SL) :-
    % Retrieve the stats for each predicate and sum them.
    findall(NC, (fb_pred(F, A), metta_stats(F, A, NC)), Each),
    sumlist(Each, SL).

%!  heartbeat is det.
%
%   Prints a heartbeat message every 60 seconds to track activity.
%   It checks the difference between the current time and the last
%   recorded time. If the difference is greater than or equal to
%   60 seconds, it prints a status message using `metta_stats/0`.
%
%   This predicate uses global variables to store and retrieve the
%   last printed time efficiently.
%
%   @example
%     % Trigger the heartbeat check.
%     ?- heartbeat.
%
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

metta_stats :-
    % Run garbage collection to free memory.
    gc_now,

    % Print decorative separator lines.
    writeln('\n\n\n\n\n\n;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
    writeln(';~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
    full_atom_count(SL),
    format("~N~n; Total\t\tAtoms (Atomspace size): ~`.t ~D~108|~n",[SL]),

    % Update the last printed time with the current time.
    get_time(CurrentTime),
    nb_setval(last_printed_time, CurrentTime),

    % Collect statistics for memory, atom space, and CPU time.
    post_statistic(memory, Mem),
    post_statistic(atom_space, AS),
    post_statistic(cputime, TotalSeconds),
    post_statistic(atoms, Concepts),
    flag(assert_new, CTs, CTs),  % Retrieve the assert_new counter.
    post_statistic(stack, StackMem),

    % Calculate various memory metrics.
    PM is Mem + StackMem,  % Total memory used.
    RM is Mem - AS,  % Relational memory.
    PA is RM // (SL + 1),  % Bytes per atom (average).
    APS is 60 * floor(SL / (TotalSeconds + 1)),  % Atoms per minute.
    ACS is AS // (Concepts + 1),  % Bytes per concept node (average).

    % Print collected statistics.
    pl_stats('SymbolAtoms', Concepts),
    pl_stats('Random samples', CTs),
    skip((
        pl_stats('Bytes Per Atom (Average)', PA),
        pl_stats('Bytes Per ConceptNode (Average)', ACS)
    )),
    skip((
        pl_stats('Relational Memory', RM),
        pl_stats('ConceptNode Memory', AS)
    )),
    % pl_stats('Queryspace Memory', StackMem),
    % Format the total CPU time into days, hours, minutes, and seconds.
    format_time(TotalSeconds, Formatted),
    skip((pl_stats('Atoms per minute', APS))),
    % Print the total memory used.
    pl_stats('Total Memory Used', PM),
    % Print the formatted runtime.
    pl_stats('Runtime (days:hh:mm:ss)', Formatted),
    % Print two newlines and cut to prevent backtracking.
    nl, nl, !.

%!  metta_stats(+F) is det.
%
%   Collects statistics for all predicates with the specified functor `F`.
%   It applies `metta_stats/2` for each matching predicate.
%
%   @arg F The functor whose predicates are analyzed.
%
%   @example
%     % Collect statistics for predicates with the functor 'my_pred'.
%     ?- metta_stats(my_pred).
%
metta_stats(F) :-
    % Iterate over all predicates with the given functor.
    for_all(fb_pred(F, A), metta_stats(F, A)).

%!  metta_stats(+F, +A) is det.
%
%   Collects and prints statistics for the predicate with functor `F`
%   and arity `A`.
%
%   @arg F The functor of the predicate.
%   @arg A The arity of the predicate.
%
metta_stats(F, A) :-
    % Retrieve the number of clauses for the predicate.
    metta_stats(F, A, NC),
    % Print the statistic.
    pl_stats(F / A, NC).

%!  metta_stats(+F, +A, -NC) is det.
%
%   Retrieves the number of clauses for the predicate with functor `F`
%   and arity `A`.
%
%   @arg F The functor of the predicate.
%   @arg A The arity of the predicate.
%   @arg NC The number of clauses in the predicate.
%
metta_stats(F, A, NC) :-
    % Create a term with the specified functor and arity.
    functor(P, F, A),
    % Retrieve the number of clauses for the term.
    predicate_property(P, number_of_clauses(NC)).

%!  pl_stats(+Stat) is det.
%
%   Collects and prints statistics for a given statistic type.
%
%   @arg Stat The statistic type to collect.
%
pl_stats(Stat) :-
    % Retrieve statistics for the specified type.
    statistics(Stat, Value),
    % Print the statistic value.
    pl_stats(Stat, Value).

%!  pl_stats(+Stat, +Value) is det.
%
%   Prints the formatted result for a statistic and its value.
%
%   @arg Stat The statistic type.
%   @arg Value The value of the statistic.
%
pl_stats(Stat, [Value | _]) :-
    % If the value is non-variable, print it.
    nonvar(Value), !, pl_stats(Stat, Value).
pl_stats(Stat, Value) :-
    % Format and print the statistic and its value.
    format("~N;\t\t~@: ~`.t ~@~100|", [format_value(Stat), format_value(Value)]), !.

%!  format_value(+Value) is det.
%
%   Formats a value for printing based on its type (float, integer, or term).
%
%   @arg Value The value to format.
%
format_value(Value) :-
    float(Value),  % Format as a float with two decimal places.
    !, format("~2f", [Value]), !.
format_value(Bytes) :-
    integer(Bytes),  % If it's an integer, format it as bytes.
    format_bytes(Bytes, Formatted), !, write(Formatted).
format_value(Term) :-
    % Format any other term as a string.
    format("~w", [Term]).

%!  format_bytes(+Bytes, -Formatted) is det.
%
%   Converts a byte count into a human-readable format (GB, MB, or B).
%
%   @arg Bytes The byte count to format.
%   @arg Formatted The resulting formatted string.
%
%   @example
%     % Format 1073741824 bytes as 1G.
%     ?- format_bytes(1073741824, F).
%     F = "1.00G".
%
format_bytes(Bytes, Formatted) :-
    Bytes >= 1073741824,  % If it's 1G or more, format as gigabytes.
    GB is Bytes / 1073741824, format(string(Formatted), '~2fG', [GB]), !.
format_bytes(Bytes, Formatted) :-
    Bytes >= 104857600, Bytes < 1073741824,  % Format as megabytes if < 1G.
    !, MB is Bytes / 1048576, D is floor(MB), format(string(Formatted), '~DM', [D]).
format_bytes(Bytes, Formatted) :-
    % If the value is less than 1K, show it in bytes.
    format(string(Formatted), '~D', [Bytes]).

% % If the number is less than 1M, show it in kilobytes (K).
%format_bytes(Bytes, Formatted) :- Bytes >= 1024, Bytes < 1048576, !, KB is Bytes / 1024, format(string(Formatted), '~0fK', [KB]).

%!  format_time(+TotalSeconds, -Formatted) is det.
%
%   Converts a total number of seconds into a formatted string with
%   days and time in `HH:MM:SS` format. Handles cases where the time
%   exceeds one day by computing the number of days and the remaining time.
%
%   @arg TotalSeconds The total number of seconds to format.
%   @arg Formatted The resulting formatted time string.
%
%   @example
%     % Convert 90061 seconds to a formatted string.
%     ?- format_time(90061, Formatted).
%     Formatted = "1:01:01:01".
%
format_time(TotalSeconds, Formatted) :-
    % Convert total seconds to an integer (discard decimals).
    Seconds is floor(TotalSeconds),
        % Calculate the number of days.
    Days is div(Seconds, 86400),  % 86400 seconds in a day.
    % Calculate the remaining seconds after accounting for days.
    Remain1 is mod(Seconds, 86400) - 57600,  % Adjusting time zone (offset by 16 hours).
    % Format the remaining time into HH:MM:SS format.
    format_time(string(Out), '%T', Remain1),
    % Combine the days and formatted time into the final result.
    format(string(Formatted), '~w:~w', [Days, Out]).

%!  print_formatted_time(+TotalSeconds) is det.
%
%   Prints the formatted time string for a given total number of seconds.
%   It converts the total seconds into a readable format using `format_time/2`.
%
%   @arg TotalSeconds The total number of seconds to format and print.
%
%   @example
%     % Print the formatted time for 90061 seconds.
%     ?- print_formatted_time(90061).
%     1:01:01:01
%
print_formatted_time(TotalSeconds) :-
    % Format the total seconds into a readable string.
    format_time(TotalSeconds, Formatted),
    % Print the formatted time string.
    writeln(Formatted).

%!  metta_final is det.
%
%   Performs final actions by saving statistics related to memory,
%   atoms, and the atom space.
%
%   @example
%     % Execute the final statistics saving process.
%     ?- metta_final.
%
metta_final :-
    % Save statistics for memory usage.
    save_pre_statistic(memory),
    % Save statistics for the total number of atoms.
    save_pre_statistic(atoms),
    % Save statistics for the atom space.
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
%!  search_for1(+X) is det.
%
%   Searches for occurrences of the variable `X` within all atoms in the
%   Metta knowledge base and prints each matching atom.
%
%   @arg X The variable to search for.
%
%   @example
%     % Search for a variable in the knowledge base.
%     ?- search_for1('$VAR(A)').
%
search_for1(X) :-
    % Iterate over all atoms, checking if they contain the variable X.
    forall(
        (metta_atom(_Where, What), contains_var(X, What)),
        (nl, write_src_nl(What))  % Print matching atoms.
    ).

%!  search_for2(+X) is det.
%
%   Searches for occurrences of the variable `X` within all source files
%   loaded into the Metta system and prints each matching entry.
%
%   @arg X The variable to search for.
%
%   @example
%     % Search for a variable in loaded source files.
%     ?- search_for2('$VAR(A)').
%
search_for2(X) :-
    % Iterate over all source entries, checking if they contain the variable X.
    forall(
        (metta_file_src(_Where, What), contains_var(X, What)),
        (nl, write_src_nl(What))  % Print matching entries.
    ).

%!  metta_file_src(-Where, -What) is nondet.
%
%   Retrieves source entries from loaded files in the Metta system. Each
%   source entry is associated with a specific location and buffer.
%
%   @arg Where The location where the source entry was loaded.
%   @arg What The content of the source entry.
%
metta_file_src(Where, What) :-
    % Retrieve the loaded file and its buffer information.
    loaded_into_kb(Where, File),
    user:metta_file_buffer(0, _Ord, _Kind, What, Vars, File, _Loc),
    % Attempt to name the variables within the entry.
    ignore(maplist(name_the_var, Vars)).

%!  guess_metta_vars(+What) is det.
%
%   Attempts to guess the variables in a given term by unifying it with
%   entries in the Metta file buffer.
%
%   @arg What The term to analyze and match.
%
%   @example
%     % Guess variables within a term.
%     ?- guess_metta_vars(my_term).
%
guess_metta_vars(What) :-
    % Attempt to match the term with entries from the Metta file buffer.
    ignore(
        once((
            user:metta_file_buffer(0, _Ord, _Kind, What0, Vars, _File, _Loc),
            alpha_unify(What, What0),  % Perform alpha-unification.
            maplist(name_the_var, Vars)  % Name the variables.
        ))
    ).

%!  name_the_var(+Pair) is det.
%
%   Assigns a name to a variable if it is unnamed, following the
%   convention of prefixing the name with an underscore.
%
%   @arg Pair A variable and its name in the form `N=V`.
%
%   @example
%     % Name a variable following the convention.
%     ?- name_the_var('_X' = V).
%
name_the_var(N = V) :-
    ignore((
        atom_concat('_', NV, N),  % Create a new variable name.
        V = '$VAR'(NV)  % Assign the new name to the variable.
    )).

%!  alpha_unify(+Term1, +Term2) is nondet.
%
%   Checks if two terms are structurally identical, accounting for
%   alpha-equivalence. If they are, it unifies them.
%
%   @arg Term1 The first term to compare.
%   @arg Term2 The second term to compare.
%
%   @example
%     % Perform alpha-unification on two terms.
%     ?- alpha_unify(foo(X), foo(Y)).
%     X = Y.
%
alpha_unify(What, What0) :-
    What =@= What0,  % Check structural equivalence.
    % If the terms are non-variable, unify them.
    (nonvar(What) -> What = What0; What == What0).


%:- find_missing_cuts.
