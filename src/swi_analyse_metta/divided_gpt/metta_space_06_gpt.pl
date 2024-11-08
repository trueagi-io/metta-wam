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


