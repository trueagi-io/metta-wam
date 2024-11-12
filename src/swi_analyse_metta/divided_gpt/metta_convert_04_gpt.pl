/**
 * @predicate sexpr_s2p/4
 * Transforms an S-expression (a Lisp-like structure) into a Prolog term.
 * @param Fn - Function name or context.
 * @param Nth - Position within arguments (used for recursive calls).
 * @param [S|STERM0] - List structure representing an S-expression.
 * @param PTERM - The resulting Prolog term.
 *
 * This predicate recursively processes S-expressions, transforming them into
 * appropriate Prolog terms based on their structure.
 *
 * @example
 * ?- sexpr_s2p(myFunc, 1, ['quote', 'X'], P).
 * P = quote('X').
 */
sexpr_s2p(Fn, Nth, [S|STERM0], PTERM) :-
    % Handles preprocessing of the S-expression list, 
    % e.g., ensures it is a valid structure to proceed.
    sexpr_s2p_pre_list(Fn, Nth, STERM0, STERM),
    % Processes the argument list of the expression and constructs the term.
    sexpr_s2p_arglist(S, 1, STERM, PLIST), 
    % Combines function and arguments into the resulting Prolog term.
    z_univ(Fn, Nth, PTERM, [S|PLIST]),
    !.

/**
 * Fallback case when the third argument is a variable; 
 * it simply passes the variable as the result.
 */
sexpr_s2p(_Fn, _Nth, VAR, VAR).

/**
 * @predicate expects_type/3
 * Determines the expected type of a function argument.
 * @param Fn - The function or operator in question.
 * @param Nth - The position of the argument in question.
 * @param Type - The type expected for the argument.
 *
 * Retrieves the type definition of an operator, using nth0 to fetch the type 
 * for the given argument position.
 */
expects_type(Fn, Nth, Type) :-
    % Gets the operator's type signature (return type and parameter types).
    get_operator_typedef(Self, Fn, Params, RetType),
    % Retrieves the type corresponding to the Nth position.
    nth0(Nth, [RetType|Params], Type),
    % Ensures the type is not a variable (has been set).
    nonvar(Type).

/**
 * @predicate will_become_type/3
 * Attempts to coerce or match a term to a given type.
 * @param Type - The type to which S should conform.
 * @param S - The term to coerce.
 * @param P - The resulting term after type coercion.
 *
 * If S is a variable or already of the correct type, it is returned unchanged.
 * Otherwise, coercion is attempted.
 */
will_become_type(Type, S, P) :-
    % Attempt to adjust the argument types and unify P with the result.
    try_adjust_arg_types(=, _RetType, 88, _Self, [Type], [S], [PS]),
    PS = P,
    !.

% If S is a free variable, return it unchanged as P.
will_become_type(Type, S, P) :- is_ftVar(S), !, P = S.

% If S already has a type, check if it's a subtype or coerce it.
will_become_type(Type, S, P) :-
    get_type(S, T),
    !,
    (is_subtype(T, Type) -> S = P ; P = coerce(Type, S)).

% If no specific type handling is needed, return S as P.
will_become_type(_Type, S, P) :- !, S = P.

/**
 * @predicate is_subtype/2
 * Checks whether T is a subtype of TT.
 * @param T - The subtype.
 * @param TT - The supertype.
 *
 * Compares two types to determine if T is a subtype of TT.
 */
is_subtype(T, TT) :- T =@= TT, !, T = TT.
is_subtype(T, TT) :- T = TT, !.

/**
 * @predicate iz_quoter/1
 * Determines whether a symbol is a quoter in the target language (like Lisp).
 * @param Symbol - The symbol in question.
 *
 * Recognizes various quoting mechanisms, including Common Lisp syntax.
 */
iz_quoter('#BQ') :- iz_common_lisp.
iz_quoter('#COMMA') :- iz_common_lisp.
iz_quoter('quote').
iz_quoter(superpose).

/**
 * @predicate iz_fun_argz/2
 * Determines the expected number of arguments for a given function.
 * @param F - The function.
 * @param ArgCount - The expected number of arguments.
 */
iz_fun_argz(list(_), _).  % List functions can have arbitrary arity.
iz_fun_argz(defmacro, 2). % defmacro takes two arguments.
iz_fun_argz(defun, 2).    % defun takes two arguments.
iz_fun_argz(let, 1).      % let takes one argument.
iz_fun_argz('let*', 1).   % let* takes one argument.
iz_fun_argz('member', 2). % member takes two arguments.
% Previously: iz_fun_argz('let*', 2). Skipped, as let* typically has arity 1.
iz_fun_argz(F, 1) :- iz_quoter(F).  % Quoting functions take one argument.

/**
 * @predicate z_functor/1
 * Checks if F is a valid functor (ignores variables and reserved symbols).
 * @param F - The functor to check.
 */
z_functor(F) :- \+ atom(F), !, fail.           % F must be an atom.
z_functor(F) :- \+ atom_concat('?', _, F).     % Ignore functors starting with '?'.
z_functor(F) :- \+ atom_concat('$', _, F).     % Ignore functors starting with '$'.

/**
 * @predicate z_univ/4
 * Creates a Prolog term by combining a functor and its arguments.
 * @param Fn - Contextual function name (not used in most cases).
 * @param Nth - Position in the argument list.
 * @param P - The resulting Prolog term.
 * @param [F|ARGS] - Functor and arguments in list form.
 *
 * This is a variant of =../2 (univ) to handle S-expression processing.
 */
z_univ(_Fn, _, P, [F|ARGS]) :-
    % Check if F is a valid functor and ARGS is a list.
    z_functor(F), is_list(ARGS),
    % Get the arity of F and construct the compound term.
    length(ARGS, A), l_arity_l(F, A),
    compound_name_arguments(P, F, ARGS),
    !.

% Fallback case if no transformation is needed, unify P and S directly.
z_univ(_Fn, _Nth, P, S) :- P = S.

/**
 * @predicate l_arity_l/2
 * Determines the arity of a functor F.
 * @param F - The functor.
 * @param A - The arity of the functor.
 */
l_arity_l(F, A) :- clause_b(arity(F, A)).
l_arity_l(function, 1).
l_arity_l(quote, 1).
l_arity_l('#BQ', 1) :- iz_common_lisp.
l_arity_l(F, A) :- current_predicate(F/A).  % Check if F/A exists as a predicate.
l_arity_l(_, 1).  % Default arity is 1.

/**
 * @predicate sexpr_s2p_arglist/4
 * Processes the argument list of an S-expression and transforms each argument into a Prolog term.
 * @param Fn - Function name or context.
 * @param Nth - Position in the argument list.
 * @param [S|SList] - List of arguments to process.
 * @param [P|PList] - List of resulting Prolog terms.
 */
sexpr_s2p_arglist(_Fn, _, VAR, VAR) :- is_ftVar(VAR), !.  % If it's a free variable, return unchanged.

sexpr_s2p_arglist(Fn, Nth, [S|SList], [P|PList]) :-
    % Recursively process each argument in the list.
    sexpr_s2p(Fn, Nth, S, P),
    % Update Nth for the next argument.
    (Nth > 0 -> Nth2 is Nth + 1; Nth2 = 0),
    % Process the rest of the list.
    sexpr_s2p_arglist(Fn, Nth2, SList, PList),
    !.

sexpr_s2p_arglist(Fn, Nth, S, P) :- sexpr_s2p(Fn, Nth, S, P), !.

sexpr_s2p_arglist(_Fn, _Nth, VAR, VAR).  % Base case: free variable.

/**
 * @predicate sexpr_s2p_pre_list/4
 * Preprocesses the list form of an S-expression before transforming it into Prolog terms.
 * @param Fn - Function name or context.
 * @param _ - Placeholder for position.
 * @param STERM - The list structure.
 * @param STERM - Unchanged list structure.
 */
sexpr_s2p_pre_list(_Fn, _, STERM, STERM) :- \+ compound(STERM), !.  % If not a compound term, return unchanged.
sexpr_s2p_pre_list(_Fn, _, STERM, STERM) :- \+ is_list(STERM), !.   % If not a list, return unchanged.

/* Previously: A clause to handle empty lists as a special case, which has been skipped to keep consistency. 
 sexpr_s2p_pre_list(Fn,_,[S|STERM],[S|STERM]):- STERM == [], !.


*/