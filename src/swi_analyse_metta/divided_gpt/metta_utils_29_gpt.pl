/* PLDoc header for pp_hook_g1a/1
   This predicate processes the term `G` and applies the `fch/1` predicate to it.
   It uses cut (!) to ensure the processing stops after the first match.
   @param G The term to be processed.
   @example pp_hook_g1a(example_term). */
pp_hook_g1a(G) :- 
    % Applies the fch/1 predicate to the input term G
    fch(G), 
    % Cut (!) ensures that no further rules are considered
    !.

/* previously: pp_hook_g2/1 was intended to process output terms using colorization,
   but it's commented out, possibly due to dependency on a predicate `colorize_oterms/2`.
   Dead code explanation: Skipped due to reliance on a condition `current_predicate(colorize_oterms/2)`
   which may not always be true or may no longer be needed. */
%pp_hook_g2(O):- current_predicate(colorize_oterms/2),colorize_oterms(O,C), notrace(catch(fch(C),_,fail)),! .

/* PLDoc header for fch/1
   This predicate is used for formatting or outputting a term `O`. Currently,
   it applies the `wqs1/1` predicate to the input term `O`.
   @param O The term to be output or formatted.
   @example fch(example_term). */
fch(O) :- 
    % Applies the wqs1/1 predicate to the input term O
    wqs1(O).

/* previously: Other variations of `fch/1` were used for different printing mechanisms 
   such as `pp_no_nl/1` for printing without newline, but they are now commented out 
   and not in use, possibly for simplifying the output format. */
%fch(O):- pp_no_nl(O).
%fch(O):- print(O).
%fch(O):- p_p_t_no_nl(O).

/* PLDoc header for wotsq/2
   This predicate takes a term `O` and a second argument `Q` and processes them 
   using the `wots_hs/2` and `wqnl/1` predicates.
   @param O The term to be processed.
   @param Q The second argument used for processing.
   @example wotsq(term1, term2). */
wotsq(O, Q) :- 
    % Calls the wots_hs/2 predicate with the second argument Q 
    % and the result of wqnl/1 applied to O.
    wots_hs(Q, wqnl(O)).

/* PLDoc header for has_goals/1
   This predicate checks if a term `G` has goals by examining its attributed 
   variables (attvars) or if its variables and singletons differ.
   @param G The term to be examined.
   @example has_goals(example_term). */
has_goals(G) :- 
    % Check if the term G has attributed variables (attvars).
    term_attvars(G, AV), 
    AV \== [].
has_goals(G) :- 
    % Check if the term G has variables that are not singletons.
    term_variables(G, TV), 
    term_singletons(G, SV), 
    TV \== SV.

/* PLDoc header for maybe_term_goals/3
   This predicate examines a term and produces its attributed variables and goals,
   copying terms and applying numbervars to its variables.
   @param Term The original term.
   @param TermC The copied term after processing.
   @param Goals The list of goals associated with the term.
   @example maybe_term_goals(example_term, CopiedTerm, Goals). */
maybe_term_goals(Term, TermC, Goals) :- 
    % Extract attributed variables from the term
    term_attvars(Term, Attvars), 
    Attvars \== [], 
    !,  % Cut to prevent further backtracking if attributed variables are found
    term_variables(Term, Vars),
    % Filter out variables that are not in the attributed variables list
    include(not_in(Attvars), Vars, PlainVars),
    % Copy term along with attributed and plain variables
    copy_term((Attvars + PlainVars + Term), (AttvarsC + PlainVarsC + TermC), Goals),
    % Number the variables starting from 10, skipping attributed variables
    numbervars(PlainVarsC, 10, Ten1, [singletons(true), attvar(skip)]),
    % Number the attributed variables and goals
    numbervars(AttvarsC + Goals, Ten1, _Ten, [attvar(bind), singletons(false)]).

/* PLDoc header for maybe_replace_vars/5
   This predicate replaces variables in goals if necessary, using sub_var and 
   freeze for goal evaluation.
   @param VarsC The list of variables to replace.
   @param SGoals The original goals.
   @param TermC The copied term.
   @param RSGoals The resulting goals after replacement.
   @param RTermC The resulting term after replacement.
   @example maybe_replace_vars([Var1, Var2], Goals, CopiedTerm, ResultGoals, ResultTerm). */
maybe_replace_vars([], SGoals, TermC, SGoals, TermC) :- 
    !.  % If there are no variables to replace, return the original goals and term.
maybe_replace_vars([V | VarsC], SGoals, TermC, RSGoals, RTermC) :- 
    % Partition the goals into those containing the variable V and those without
    my_partition(sub_var(V), SGoals, Withvar, WithoutVar),
    % Ensure that only one goal contains the variable
    Withvar = [OneGoal],
    % Use freeze to delay evaluation of the goal until it's not null
    freeze(OneGoal, (OneGoal \== null, OneGoal \== @(null))),
    % Ensure the variable appears only once in the term
    findall(_, sub_var(V, TermC), LL), 
    LL = [_], 
    !,
    % Substitute the variable V with the goal in the term and goals
    subst([WithoutVar, TermC], V, {OneGoal}, [SGoalsM, TermCM]),
    % Recursively replace remaining variables
    maybe_replace_vars(VarsC, SGoalsM, TermCM, RSGoals, RTermC).
maybe_replace_vars([_ | VarsC], SGoals, TermC, RSGoals, RTermC) :- 
    % If the variable is not found, continue with the next variable
    maybe_replace_vars(VarsC, SGoals, TermC, RSGoals, RTermC).

/* PLDoc header for src_sameish/2
   This predicate checks if two terms are "sameish", i.e., structurally equivalent.
   @param Orig The original term.
   @param Find The term to compare with.
   @example src_sameish(term1, term2). */
src_sameish(Orig, Find) :- 
    % Copy the original term to a new variable COrig
    copy_term(Orig, COrig), 
    % Set Find to Orig and check if Orig and COrig are structurally equivalent
    Find = Orig, 
    Orig =@= COrig.

/* PLDoc header for number_vars_calc_goals/3
   This predicate calculates goals and assigns numbered variables to a term, 
   taking into account its singletons and attributed variables.
   @param Term The original term.
   @param SSRTermC The term after processing with numbered variables.
   @param SRSGoals The sorted list of goals.
   @example number_vars_calc_goals(example_term, ProcessedTerm, Goals). */
number_vars_calc_goals(Term, SSRTermC, [1 | SRSGoals]) :- 
    % Extract singletons and attributed variables from the term
    term_singletons(Term, Singles),
    term_attvars(Term, Vars),
    % Copy the term, variables, and singletons
    copy_term(Term + Vars + Singles, TermC + VarsC + SinglesC, Goals),
    % Number the variables and goals, skipping attributed variables
    notrace(catch(numbervars(TermC + Goals, 0, _Ten1, [singletons(false), attvar(skip)]), _, fail)),
    % Sort the goals based on variables
    sort_goals(Goals, VarsC, SGoals),
    % Replace variables in the goals and term if necessary
    maybe_replace_vars(VarsC, SGoals, TermC, RSGoals, RTermC),
    % Filter out non-substituted singletons
    include(not_sub_var(RSGoals), SinglesC, KSingles),
    % Create placeholder variables for remaining singletons
    length(KSingles, SL), 
    length(VSingles, SL), 
    my_maplist(=('$VAR'('__')), VSingles),
    % Substitute the singletons and variables in the term and goals
    subst_2L(KSingles, VSingles, [RTermC, RSGoals], [SRTermC, SRSGoals]),
    % Apply specific substitutions based on matching patterns
    subst_1L_p2(src_sameish, [
        {dif('$VAR'('__'), RED)} = dif(RED),
        {cbg('$VAR'('__'))} = cbg
    ], SRTermC, SSRTermC), 
    !.

number_vars_calc_goals(Term,SSRTermC,[3|SRSGoals]):-
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),


