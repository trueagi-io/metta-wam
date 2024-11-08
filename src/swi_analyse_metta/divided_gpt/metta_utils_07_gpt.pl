/* previously: goal_expansion for handling must_det_l and must_det_ll was skipped 
   Explanation: The commented-out goal_expansion clauses were likely designed for transforming specific goals during compilation. 
   These clauses might have been skipped to avoid conflicts or unnecessary processing in the current context. */

/**
 * get_setarg_p1(+P3, +E, +Cmpd, -SA) is det.
 *
 * Retrieves the argument from the compound term Cmpd and sets the argument using a predicate P3.
 *
 * @param P3    Predicate to set the argument.
 * @param E     The element to find within the compound term.
 * @param Cmpd  The compound term being processed.
 * @param SA    The result after applying the predicate P3 to the argument in Cmpd.
 *
 * @example
 * ?- get_setarg_p1(setarg, member, some_term(member, data), SA).
 */
get_setarg_p1(P3, E, Cmpd, SA) :-
    % Check if Cmpd is a compound term and call get_setarg_p2 to find and set the argument.
    compound(Cmpd), 
    get_setarg_p2(P3, E, Cmpd, SA).

/**
 * get_setarg_p2(+P3, +E, +Cmpd, -SA) is det.
 *
 * Retrieves the argument in Cmpd at position N1 and sets it using the predicate P3.
 *
 * @param P3    Predicate to set the argument.
 * @param E     The element to match within the compound term.
 * @param Cmpd  The compound term being processed.
 * @param SA    The result after applying the predicate P3 to the argument in Cmpd.
 */
get_setarg_p2(P3, E, Cmpd, SA) :-
    % Find the argument position in the compound term and apply P3 to set the argument.
    arg(N1, Cmpd, E), 
    SA = call(P3, N1, Cmpd).

get_setarg_p2(P3, E, Cmpd, SA) :-
    % Recursively search for the argument in the compound term's arguments if the first attempt fails.
    arg(_, Cmpd, Arg), 
    get_setarg_p1(P3, E, Arg, SA).

/**
 * my_b_set_dict(+Member, +Obj, +Var) is det.
 *
 * Set the value of a member in an object using b_set_dict.
 *
 * @param Member  The member key to set.
 * @param Obj     The object (e.g., dictionary) where the member resides.
 * @param Var     The value to set for the member.
 */
my_b_set_dict(Member, Obj, Var) :-
    % Set the member's value in the object using the auxiliary set_omemberh predicate.
    set_omemberh(b, Member, Obj, Var).

/**
 * set_omemberh(+How, +Member, +Obj, +Var) is det.
 *
 * Handles setting member values in objects based on different modes (b, nb, link).
 *
 * @param How     The method used for setting (e.g., 'b', 'nb').
 * @param Member  The member key to set.
 * @param Obj     The object where the member resides.
 * @param Var     The value to set for the member.
 */
set_omemberh(_, Member, Obj, Var) :-
    % Use arc_setval to set the member's value in the object.
    !, arc_setval(Obj, Member, Var).

/* previously: Alternative set_omemberh methods were commented out.
   Explanation: Other methods such as nb_set_dict and nb_link_dict are specialized ways to set dictionary values without backtracking. 
   These might have been disabled to maintain consistency or because they were not needed in the current context. */

/**
 * set_omember(+Member, +Obj, +Var) is det.
 *
 * Sets the value of a member in an object using a default mode.
 *
 * @param Member  The member key to set.
 * @param Obj     The object where the member resides.
 * @param Var     The value to set for the member.
 */
set_omember(Member, Obj, Var) :-
    % Use the default method 'b' to set the member value.
    set_omember(b, Member, Obj, Var).

/**
 * set_omember(+How, +Member, +Obj, +Var) is det.
 *
 * Sets the value of a member in an object with a specified method.
 *
 * @param How     The method used for setting (e.g., 'b', 'nb').
 * @param Member  The member key to set.
 * @param Obj     The object where the member resides.
 * @param Var     The value to set for the member.
 */
set_omember(How, Member, Obj, Var) :-
    % Ensure that the necessary arguments are nonvar (i.e., instantiated) before proceeding.
    must_be_nonvar(Member),
    must_be_nonvar(Obj),
    must_be_nonvar(How),
    !,
    set_omemberh(How, Member, Obj, Var),
    !.

/**
 * get_kov(+K, +O, -V) is semidet.
 *
 * Retrieves the value associated with key K in object O, with a fallback to handle nested structures.
 *
 * @param K  The key to look up.
 * @param O  The object in which to search for the key.
 * @param V  The value associated with the key.
 */
get_kov(K, O, V) :-
    % Check if the object is a dot-hook, and retrieve the associated value if so.
    dictoo:is_dot_hook(user, O, K, V), 
    !, 
    o_m_v(O, K, V).

get_kov(K, O, V) :-
    % Fallback to retrieve values from nested objects or properties if direct retrieval fails.
    (get_kov1(K, O, V) *-> true ; (get_kov1(props, O, VV), get_kov1(K, VV, V))).

/* Explanation: The commented-out clause that directly accesses rbtree nodes 
   was likely skipped to avoid direct manipulation of red-black tree nodes. This approach can be risky or unnecessary. */

% Directives to export and import the term_expansion_setter/2 predicate.
:- export(term_expansion_setter/2).
:- system:import(term_expansion_setter/2).

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).
