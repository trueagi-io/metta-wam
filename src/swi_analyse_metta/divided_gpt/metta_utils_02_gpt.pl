% Predicate to unset a value for a user by its name `N`.
% @param N The name of the value to be unset.
luser_unsetval(N):-
    % Ignore if there is no such value in the non-backtrackable storage
    ignore(nb_delete(N)),
    % Get the current user ID and unset the value for the user
    arc_user(ID),
    % Call `luser_unsetval/2` to unset the value for the specific user and name
    luser_unsetval(ID,N),
    % Cut to stop backtracking once successful
    !.

% Predicate to unset a value for a specific user ID and name `N`.
% @param ID The user ID.
% @param N The name of the value to be unset.
luser_unsetval(ID,N):-
    % Retract all properties associated with the user and the specific value
    retractall(arc_user_prop(ID,N,_)).

% Predicate to set a default value for a user globally.
% @param N The name of the value.
% @param V The value to be set.
set_luser_default(N,V):- 
    % Set the value globally for the user
    luser_setval(global,N,V).

% Predicate to get or set a default value for a user.
% @param N The name of the value.
% @param V The value to be retrieved or set.
luser_default(N,V):- 
    % If the value `V` is unbound (variable), get the value.
    var(V),!, 
    luser_getval(N,V).
luser_default(N,V):- 
    % Otherwise, set the default value.
    set_luser_default(N,V).

% Predicate to link a value to the current user or a specific ID.
% @param N The name of the value.
% @param V The value to be linked.
luser_linkval(N,V):- 
    % Get the current user ID and link the value
    arc_user(ID),
    luser_linkval(ID,N,V),
    % Cut to stop backtracking
    !.

% Predicate to link a value for a specific user ID.
% Skips linking if `V` is a sensical term and traces a warning if not.
% @param ID The user ID.
% @param N The name of the value.
% @param V The value to be linked.
luser_linkval(ID,N,V):- 
    % Ensure the value `V` is not a variable and both `N` and `V` are not "sensical terms"
    \+ var(V), \+ (arc_sensical_term(N), arc_sensical_term(V)),
    % Trace and skip execution if terms are not sensical
    trace,
    warn_skip(not_arc_sensical_term(luser_linkval(ID,N,V))).

% Predicate to link a value for a user and store it persistently.
% @param ID The user ID.
% @param N The name of the value.
% @param V The value to be linked.
luser_linkval(ID,N,V):- 
    % If the name `N` is an atom, use `nb_linkval` to store the value persistently
    (atom(N) -> nb_linkval(N,V); true),
    % Remove any existing property associated with the user and the value `N`
    retractall(arc_user_prop(ID,N,_)),
    % Assert the new property for the user and value `N`
    asserta(arc_user_prop(ID,N,V)).

% Predicate to check if a term is "sensical", meaning it is neither empty nor a special case.
% @param O The term to be checked.
arc_sensical_term(O):- 
    % Ensure the term `O` is not a variable and is not an empty list, string, or specific ignored structures
    nonvar(O), O \== [], O \== '', O \= (_ - _), O \== end_of_file.

% Predicate to check if a term is sensical and return it in the second argument.
% @param V The term to check.
% @param O The output term.
arc_sensical_term(V,O):- 
    % Reuse the `arc_sensical_term/1` check and unify the output
    arc_sensical_term(V), !, O = V.

% Option predicate, used for configuration.
% Currently disabled and returns failure.
% @example arc_option(grid_size_only) fails.
%arc_option(grid_size_only):- !,fail.

% Option predicate to check if an option is set.
% @param O The option name.
arc_option(O):- 
    % Get the value associated with the option `O`
    luser_getval(O,t).

% Conditional execution if an arc option is set.
% @param O The option to check.
% @param G The goal to execute if the option is set.
if_arc_option(O,G):- 
    % If the option `O` is set, execute `G`; otherwise, do nothing
    (arc_option(O) -> must_det_ll(G); true).

% Execute a goal while temporarily changing a user's value.
% @param N The name of the value to be changed.
% @param V The new value to be set.
% @param Goal The goal to execute with the changed value.
with_luser(N,V,Goal):- 
    % Get the current value of `N` for the user or default to an empty list
    (luser_getval(N,OV); OV = []),
    % Set up the environment, execute the goal, and clean up by restoring the original value
    setup_call_cleanup(
        luser_setval(N,V),
        once(Goal),
        luser_setval(N,OV)
    ).

% Old predicate for getting a value. Commented out in favor of a newer version.
%luser_getval(N,V):- nb_current(N,VVV),arc_sensical_term(VVV,VV),!,V=VV.

% Predicate to get a user's value. Uses caching.
% @param N The name of the value.
% @param V The retrieved value.
luser_getval(N,V):- 
    % Get the initial value and ensure it is sensical
    luser_getval_0(N,VV), 
    VV = V, 
    arc_sensical_term(V), 
    !.

% Predicate to get the value of `arc_user`.
% @param arc_user The name of the value to get.
luser_getval_0(arc_user,V):- 
    % Get the current user ID
    arc_user(V).

% Predicate to retrieve a value using the first available method.
% @param N The name of the value.
% @param V The retrieved value.
luser_getval_0(N,V):- 
    luser_getval_1(N,V).

% First attempt to retrieve a value using method 1.
luser_getval_1(N,V):- 
    luser_getval_2(N,V).

% Fall back to method 2 if method 1 fails.
luser_getval_1(N,V):- 
    luser_getval_3(N,V), 
    \+ (luser_getval_2(N,VV), nop(VV \= V)).

% Continue to attempt to retrieve the value from defaults or other methods.
luser_getval_1(N,V):- 
    get_luser_default(N,V), 
    \+ (luser_getval_3(N,VV), nop(VV \= V)), 
    \+ (luser_getval_2(N,VV), nop(VV \= V)).

% Older predicates commented out for now.
% previously: luser_getval_0(N,V):- luser_getval_2(N,V), \+ luser_getval_1(N,_).
% previously: luser_getval_0(N,V):- luser_getval_3(N,V), \+ luser_getval_2(N,_), \+ luser_getval_1(N,_).

% Fetch value from HTTP request parameters when not on the main thread.
luser_getval_2(N,V):- 
    \+ main_thread, 
    atom(N), 
    httpd_wrapper:http_current_request(Request), 
    member(search(List), Request), 
    member(N = VV, List), 
    url_decode_term(VV, V), 
    arc_sensical_term(V), 
    !.

% Retrieve value from the non-backtrackable storage if available.
luser_getval_2(N,V):- 
    atom(N), 
    nb_current(N, ValV), 
    arc_sensical_term(ValV, Val), 
    Val = V.

% Retrieve value from user properties.
luser_getval_3(N,V):- 
    arc_user(ID), 
    arc_user_prop(ID, N, V).

% Return failure if not in CGI context.
luser_getval_3(_,_):- 
    \+ is_cgi, 
    !, 
    fail.

% Retrieve value from session parameters in a non-main thread.
luser_getval_3(N,V):- 
    \+ main_thread, 
    atom(N), 
    current_predicate(get_param_sess/2), 
    get_param_sess(N, M), 
    url_decode_term(M, V), 
    arc_sensical_term(V).

% Fetch user defaults if available.
get_luser_default(N,V):- 
    arc_user_prop(global, N, VV), 
    VV = V, 
    arc_sensical_term(V), 
    !.

% Fetch defaults from Prolog flags if necessary.
get_luser_default(N,V):- 
    atom(N), 
    current_prolog_flag(N, VV), 
    VV = V, 
    arc_sensical_term(V), 
    !.

% Previously skipped older method of fetching values.
% previously: luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.

% Skip previously used fallback mechanism.
% previously: luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).

% Main predicate that checks if the current thread is the main thread.
ansi_main:- 
    thread_self(main), 
    nop(is_cgi), 
    !.

% Predicate to check if the current thread is the main thread.
main_thread:- 
    thread_self(main), 
    !.

% Conditionally execute a goal if in the main thread.
if_thread_main(G):- 
    main_thread -> call(G); true.

% File directive to ensure `fbug/1` is only defined once.
:- if(\+ current_predicate(fbug/1)).