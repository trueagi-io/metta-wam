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