
user:file_search_path(gencode, './metta_out_v4_pl/gencode').
user:file_search_path(uniprot, './metta_out_v4_pl/uniprot').
user:file_search_path(ontology, './metta_out_v4_pl/ontology').
user:file_search_path(gaf, './metta_out_v4_pl/gaf').
user:file_search_path(cellxgene, './metta_out_v4_pl/cellxgene').
user:file_search_path(eqtl, './metta_out_v4_pl/gtex/eqtl').
user:file_search_path(tadmap, './metta_out_v4_pl/tadmap').
user:file_search_path(refseq, './metta_out_v4_pl/refseq').

fact_files(eqtl(edges)).
fact_files(gencode(nodes)).  fact_files(gencode(edges)).
fact_files(uniprot(nodes)).  fact_files(uniprot(edges)).
fact_files(ontology(nodes)). fact_files(ontology(edges)).
fact_files(gaf(edges)).
fact_files(cellxgene(edges)).
fact_files(tadmap(edges)).
fact_files(refseq(edges)).


:- ensure_loaded(library(clpfd)).

% :- use_module(library(sldnfdraw)).
% :- sldnf.

codes_for(G, P) :- certain(1.0),
    transcribed_to(G, T),
    translates_to(T, P).

in_tad_with(S, G1) :-
    in_tad_region(G1, T),
    in_tad_region(G2, T),
    closest_gene(S, G2).

relevant_gene(G, S) :-
    in_tad_with(S, G),
    eqtl(S, G).

relevant_gene_coexpression(G1, S) :- % certain(1.0),
    relevant_gene(G2, S),
    coexpressed_with(G1, G2).

member(G, O, 0) :-
    codes_for(G, P),
    go_gene_product(O, P).

member(G, O, D) :-
    D #= D0 + 1,
    rel_type(ontology_relationship(X, O), subclass),
    member(G, X, D0).

relevant_go(O, S) :-
    (relevant_gene(G, S)
    ; relevant_gene_coexpression(G, S)),
    D #< 3,
    member(G, O, D).


/*
rule_head(member(_,_,_)).
rule_head(relevant_go(_, _)).
rule_head(relevant_gene_coexpression(_, _)).
rule_head(relevant_gene(_, _)).
rule_head(in_tad_with(_, _)).
rule_head(codes_for(_, _)).
*/
% Get these introspectively
% Define a predicate to find the head of rules defined in the same file as 'rule_head'.
rule_head(Head):-
    % Get the line number where 'rule_head(_)' is defined.
    predicate_property(rule_head(_), line_count(ThisLine)),
    % Get the file in which 'rule_head(_)' is defined.
    source_file(rule_head(_), ThisFile), source_file(Head, ThisFile),
    % Ensure 'Head' is defined in the same file as 'rule_head'.
    % This line is commented out but represents an alternative method to check file equality.
    %predicate_property(rule_head(_),file(ThisFile)), predicate_property(Head,file(ThisFile)),
    % Get the line number where 'Head' is defined.
    predicate_property(Head, line_count(LineNo)), LineNo < ThisLine,
    % Ensure that 'Head' has at least one rule associated with it.
    \+ predicate_property(Head, number_rules(0)).

/*
fact_head(gene(_)).
fact_head(closest_gene(_, _)).
fact_head(in_tad_region(_, _)).
fact_head(transcribed_to(_, _)).
fact_head(translates_to(_, _)).
fact_head(rel_type(_, _)).
fact_head(eqtl(_, _)).
fact_head(go_gene_product(_, _)).
*/
% Get these introspectively
fact_head(Head):-
   is_fact_file_name(File),
   %predicate_property(Head,file(File)).
   source_file(Head,File).


load_fact_files:- forall(fact_files(Spec),load_fact_file(Spec)).
load_fact_file(Spec):-
  absolute_file_name(Spec,File,
     [access(read), file_errors(fail), file_type(prolog)]),
load_fact_file_name(File).

:- dynamic(is_fact_file_name/1).
% already loaded
load_fact_file_name(File):- is_fact_file_name(File),!.
load_fact_file_name(File):-
  assert(is_fact_file_name(File)),
  time(ensure_loaded(File)).

:- load_fact_files.



% Hypothesis prioritization based on grounding and number of clauses

rule((Hypo :- Premise), CF_rule):- clause_or_inline(Hypo, Body),
  body_to_cf_premise(Body, CF_rule, Premise).

fact(Hypo, CF):- fact_or_inline(Hypo, Body),
  body_to_cf_premise(Body, CF, true).

body_to_cf_premise((certain(CF_rule), Premise), CF_rule, Premise):- !.
body_to_cf_premise((certain(CF_rule)), CF_rule, true):- !.
body_to_cf_premise(Premise, 1.0, Premise).

clause_or_inline(Hypo,(certain(CF_rule), do_ss(CF_rule,HSB,SizeSorted))):-
  rule_head(Hypo), inline(Hypo,SizeSorted),
  term_variables(Hypo,Hv), term_variables(SizeSorted,Bv),
  partition_eq(Hv,Bv,HO,BO,Shared),
  count_occur_l(SizeSorted,[HO,Shared,BO],[HOC,SharedC,BOC]),
  v_ify([HOC,SharedC,BOC],HSB),
  predicate_size(Hypo, Size), CF_rule is 1.0 - 1/Size.


fact_or_inline(Hypo,certain(CF_rule)):- fact_head(Hypo),
  predicate_property(Hypo, number_of_clauses(_)),
  \+ \+ clause(Hypo,true), predicate_size(Hypo, Size),
  CF_rule is Size. % 1.0 - 1/Size.

do_ss(_CF_rule,_HSB,[H]):-!, do_cf(H).
do_ss( CF_rule, HSB,[H|SizeSorted]):- do_cf(H),do_ss(CF_rule,HSB,SizeSorted).
do_cf(cf(_,H)):- call(H).

v_ify(List,HSB):- is_list(List),!,maplist(v_ify,List,VList),HSB=..[v|VList].
v_ify(NonL,NonL).

count_occur_l(SizeSorted,List,Counts):- is_list(List), !, maplist(count_occur_l(SizeSorted),List,Counts).
count_occur_l(SizeSorted,Sub,cf(Count,Sub)):- occurrences_of_var(Sub, SizeSorted, Count),!.
count_occur_l(_SizeSorted,Sub,cf(0,Sub)).
partition_eq(Hv,Bv,HO,BO,[H|Shared]):- select(H,Hv,Hr),select(B,Bv,Br), H==B,!,
   partition_eq(Hr,Br,HO,BO,Shared).
partition_eq(Hr,Br,Hr,Br,[]).

%call_g2(G1,G2):- (call(G1),call(G2))*->

certain(_).


% Hypothesis prioritization based on grounding and number of clauses
second_is_better_than_first(Hypo1, Hypo2) :-
    ground(Hypo2), !, \+ ground(Hypo1).

second_is_better_than_first(Hypo1, Hypo2) :-
    predicate_size(Hypo1, Size1),
    predicate_size(Hypo2, Size2), !,
    Size2 < Size1.

% Calculate domain size

predicate_size(Hypo, Size):-
  predicate_size([], Hypo, Size).

predicate_size(_Completed, Hypo, Size) :- \+ callable(Hypo), !, Size=1.
predicate_size(Completed, Hypo, Size):- f_memberchk(Hypo, Completed), !, Size=0.
predicate_size(_Completed, Hypo, Size):- ground(Hypo),!, Size=1.
predicate_size(Completed, (Hypo1, Hypo2), Size):- !,
  predicate_size(Completed, Hypo1, S1),
  predicate_size(Completed, Hypo2, S2),
  size_sm(S1, S2, SizeM), Size is SizeM + 1,!.
predicate_size(Completed, (Hypo1;Hypo2), Size):-
  predicate_size(Completed, Hypo1, S1),
  predicate_size(Completed, Hypo2, S2),
  Size is S1+S2,!.
predicate_size(Completed, Hypo, Size):-
   predicate_property(Hypo, number_of_clauses(F)),
   predicate_property(Hypo, number_of_rules(R)),
   predicate_size(Completed, Hypo, F, R, Size),!.
predicate_size(_Completed, _Hypo, 1).

size_sm(A, B, A) :- B =:= 0, !.
size_sm(A, B, B) :- A =:= 0, !.
size_sm(A, B, A) :- A =< B.
size_sm(A, B, B) :- B < A.


% all facts
predicate_size(_Completed, _, F, 0, F):-!.
% all rules
predicate_size(Completed, Hypo, R, R, Size):- !, rule_extent(Completed, Hypo, Size).
% mized rules and facts
predicate_size(Completed, Hypo, F, R, Size):-
   rule_extent(Completed, Hypo, RSize),
   Size is F - R + RSize.

rule_extent(Hypo, Size):-
  rule_extent([], Hypo, Size).

rule_extent(Completed, Hypo, Size):- f_memberchk(Hypo, Completed), !, Size=0.
rule_extent(Completed, Hypo, Size):- findall(S1ze, (clause(Hypo, Body), Body\==true, predicate_size([Hypo|Completed], Body, S1ze)), SizeL),
   sumlist(SizeL, Size), !.
rule_extent(_,_Hypo, 0).


a:- b.
b:- c, d.
d:- e. d:- f.
c.  e. f.

% Your rules and facts with variables:
a(X, Y) :- b(X, Y).
b(X, Y) :- c(X), d(Y).
c(1-1). c(1-2).
d(X) :- e(X).
d(X) :- f(X).
e(X) :- g(X).
e(2-1). e(2-2).
f(3-1). f(3-2).
g(_) :- f(_).

    % Your example facts and rules, now can handle variables:

:- set_prolog_flag(verbose_autoload,false),
   set_prolog_flag(verbose_load,false),
   ensure_loaded(library(logicmoo_common)).
% Entry point for inline with initial empty accumulators.
/*
    ?- inline(a, X).

    X = [c, e].
    X = [c, f].
*/
inline(Hypo, SizedAndSorted) :-
    no_repeats_var(NRV), !,
    inline(Hypo, [], List, [], _),
    once((once(include(is_base_fact, List, FactsOnly)), FactsOnly\==[],
    sort(FactsOnly, Sorted))),
    NRV=Sorted,
    once(sort_by_size(FactsOnly, SizeSorted)),
    once(maplist(add_size,SizeSorted,SizeSortedSized)),
    sort(SizeSortedSized,SizedAndSorted).
add_size(Hypo,cf(Size,Hypo)):- predicate_size(Hypo,Size).

sort_by_size(List, Sorted):- predsort(pred_size, List, Sorted).

pred_size(Hypo1, Hypo2, R):- second_is_better_than_first(Hypo1, Hypo2), R = (>), !.
pred_size(Hypo1, Hypo2, R):- second_is_better_than_first(Hypo2, Hypo1), R = (<), !.
pred_size(Hypo1, Hypo2, R):- compare(Hypo1, Hypo2, R).

inline(Var, InputList, InputList,  Seen, Seen):- var(Var), !.
% Handle a conjunction of goals in the rule body.
inline((Hypo1, Hypo2), InputList, OutputList, Seen, OutSeen) :- !,
    inline(Hypo1, InputList, MidList,  Seen, MidSeen),
    inline(Hypo2, MidList, OutputList, MidSeen, OutSeen).
% Handle a disjunction of goals in the rule body.
inline((Hypo1 ; Hypo2), InputList, OutputList, Seen, OutSeen) :- !,
    (inline(Hypo1,  InputList, OutputList, Seen, OutSeen);
     inline(Hypo2,  InputList, OutputList, Seen, OutSeen)).
% Base case: true is always a stop point.
inline(true, InputList, InputList,  Seen, Seen) :- !.
% Detect cycles: If the current hypothesis has already been processed, report a cycle to avoid infinite recursion.
inline(Hypo, InputList, InputList , Seen, Seen) :- f_memberchk(Hypo, InputList), !.
inline(Hypo, InputList, InputList , Seen, Seen) :- f_memberchk(Hypo, Seen), !.
inline(Hypo, InputList, [Hypo|InputList] , Seen, [Hypo|Seen]) :- is_base_fact(Hypo).
% Recursive case: If Hypo is a rule, process each condition in its body.
inline(Hypo, InputList, OutputList, Seen, OutSeen) :-
    predicate_property(Hypo, number_of_rules(RC)),!,RC>0,
    clause(Hypo, Body), Body \== true,
    inline(Body, InputList, OutputList, [Hypo|Seen], OutSeen).

% Helper to check if a hypothesis is a base fact.
is_base_fact(Hypo) :- predicate_property(Hypo, number_of_clauses(C)), !, \+ predicate_property(Hypo, number_of_rules(C)).
is_base_fact(Hypo) :- predicate_property(Hypo, undefined), !.
is_base_fact(Hypo) :- predicate_property(Hypo, built_in), !.
%is_base_fact(Hypo) :- clause(Hypo, true), !.

f_memberchk(F,List):- \+ \+ (member(E,List),E=@=F).


proof_rewrite(P,P).

solv(Goal, Proof):-
  solve(Goal, Proof),
  portray_clause((Goal :- Proof)).


% solve(Goal, Proof): Goal is a goal to be proven, Proof is the proof of the goal.
solve(Goal, Proof):- var(Goal),rule_head(Goal),solve(Goal, Proof).
solve(Goal, Proof):- var(Goal),!,fact_head(Goal),solve(Goal, Proof).
solve(true, true) :- !.  % Base case: the empty goal is trivially solved.
solve(not(Goal), not(Proof)) :- solve(Goal, Proof).

solve((Goal1, Goal2), Proof) :-
  second_is_better_than_first(Goal1,Goal2),!,
  solve((Goal2, Goal1), Proof).
solve((Goal1; Goal2), Proof) :-
  second_is_better_than_first(Goal1,Goal2),!,
  solve((Goal2; Goal1), Proof).

% Conjunction: solve each goal in the conjunction.
solve((Goal1, Goal2), (Proof1, Proof2)) :- !,
    solve(Goal1, Proof1), solve(Goal2, Proof2).
% Disjunction: solve one of the goals in the disjunction.
solve((Goal1 ; _), Proof1) :-    solve(Goal1, Proof1).
solve((_ ; Goal2), Proof2) :- !, solve(Goal2, Proof2).

solve(Goal, built_in(Goal)) :- % Check if the goal is a built-in predicate.
    predicate_property(Goal, built_in),!,
    call(Goal). % Directly call the built-in predicate.

solve(Goal, Proof) :-  % For any other goal: find a clause that can solve the goal.
    predicate_property(Goal, number_of_clauses(_)), % Ensure it is a user-defined predicate.
    clause(Goal, Body),  % Find a clause in the program where Goal is the head and Body is the body.
    solve_body(Goal, Body, Proof).  % Solve the body of the clause.

solve_body( Goal, true, fact(Goal)):-!.
solve_body(_Goal, Body, Proof):- solve(Body, Proof).


% solve_cert/2 succeeds with
%   arg-1 bound to a hypothesis proven true using the current knowledge base
%   arg-2 bound to the confidence in that hypothesis.
%
% solve_cert/2 calls solve/4 with appropriate arguments.  After solve/4 has completed,
% it writes the conclusions and prints a trace.

:- dynamic(known/2).
solve_cert(Hypo, CF) :-
    retractall(known(_, _)),
    print_instructions,
    solve(Hypo, CF, [], 0.2),
    write(Hypo), write(' was concluded with certainty '), write(CF), nl, nl,
    build_proof(Hypo, _, Proof), nl,
    write('The proof is '), nl, nl,
    write_proof(Proof, 0), nl, nl.

%solve/4 succeeds with
%   arg-1 bound to a hypothesis proven true using the current knowledge base
%   arg-2 bound to the confidence in that hypothesis.
%   arg-3 bound to the current rule stack
%   arg-4 bound to the threshold for pruning rules.
%
%solve/4 is the heart of exshell.  In this version, I have gone back to the
% simpler version.  It still has problems with negation, but I think that
% this is more a result of problems with the semantics of Stanford Certainty
% factors than a bug in the program.
% The pruning threshold will vary between 0.2 and -0.2, depending whether,
% we are trying to prove the current hypothesis true or false.
% solve/4 handles conjunctive predicates, rules, user queries and negation.
% If a predicate cannot be solved using rules, it will call it as a PROLOG predicate.

% Case 1: truth value of hypothesis is already known
solve(Hypo, CF, _, Threshold) :-
    known(Hypo, CF), !,
    above_threshold(CF, Threshold).

% Case 2: negated hypothesis
solve( not(Hypo), CF, Rules, Threshold) :- !,
    invert_threshold(Threshold, New_threshold),
    solve(Hypo, CF_hypothesis, Rules, New_threshold),
    negate_cf(CF_hypothesis, CF).

% Case 3: switch around
solve((Hypo1, Hypo2), MinCF, Rules, Threshold) :-
    second_is_better_than_first(Hypo1, Hypo2), !,
    solve((Hypo2, Hypo1), MinCF, Rules, Threshold).

% Case 4: conjunctive hypotheses
solve((Hypo_1, Hypo_2), CF, Rules, Threshold) :- !,
    solve(Hypo_1, CF_1, Rules, Threshold),
    above_threshold(CF_1, Threshold),
    solve(Hypo_2, CF_2, Rules, Threshold),
    above_threshold(CF_2, Threshold),
    min_cf(CF_1, CF_2, CF).

% Case 5: disjunctive hypotheses
solve((Hypo_1;Hypo_2), CF, Rules, Threshold) :- !,
   (solve(Hypo_1, CF, Rules, Threshold) ; solve(Hypo_2, CF, Rules, Threshold)).

%Case 6: backchain on a rule in knowledge base
solve(Hypo, CF, Rules, Threshold) :-
    rule((Hypo :- (Premise)), CF_rule),
    solve(Premise, CF_premise,
        [rule((Hypo :- Premise), CF_rule)|Rules], Threshold),
    rule_cf(CF_rule, CF_premise, CF),
    above_threshold(CF, Threshold).

%Case 7: fact assertion in knowledge base
solve(Hypo, CF, _, Threshold) :-
    fact(Hypo, CF),
    above_threshold(CF, Threshold).

:- dynamic(askable/1).

% Case 8: ask user
solve(Hypo, CF, Rules, Threshold) :-
    askable(Hypo),
    askuser(Hypo, CF, Rules), !,
    assert(known(Hypo, CF)),
    above_threshold(CF, Threshold).

% Case 9: All else fails, see if hypothesis can be solved in prolog.
solve(Hypo, 1.0, _, _) :-
    call(Hypo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
% Certainty factor predicates.  Currently, these implement a variation of
% the certainty factor algebra.
% The certainty algebra may be changed by modifying these predicates.

% negate_cf/2
%   arg-1 is a certainty factor
%   arg-2 is the negation of that certainty factor

negate_cf(CF, Negated_CF) :-
    Negated_CF is -1 * CF.

% min_cf/3
%   arguments 1 & 2 are certainty factors of conjoined predicates
%   arg-3 is the certainty factor of the conjunction

min_cf(A, B, A) :- A =< B.
min_cf(A, B, B) :- B < A.

%rule_cf/3
%   arg-1 is the confidence factor given with a rule
%   arg-2 is the confidence inferred for the premise
%   arg-3 is the confidence inferred for the conclusion

rule_cf(CF_rule, CF_premise, CF) :-
    CF is CF_rule * CF_premise/1.0.

%above_threshold/2
%   arg-1 is a certainty factor
%   arg-2 is a threshold for pruning
%
% If the threshold, T, is positive, assume we are trying to prove the hypothesis
% true.  Succeed if CF >= T.
% If T is negative, assume we are trying to prove the hypothesis
% false.  Succeed if CF <= T.

above_threshold(CF, T) :-
    T >= 0, CF >= T.
above_threshold(CF, T) :-
    T < 0, CF =< T.

%invert_threshold/2
%   arg-1 is a threshold
%   arg-2 is that threshold inverted to account for a negated hypothesis.
%
% If we are trying to prove not(p), then we want to prove p false.
% Hyposequently, we should prune proofs of p if they cannot prove it
% false.  This is the role of threshold inversion.

invert_threshold(Threshold, New_threshold) :-
    New_threshold is -1 * Threshold.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%
% Predicates to handle user interactions.  As is typical, these
% constitute the greatest bulk of the program.
%
% askuser/3
%   arg-1 is a hypothesis whose truth is to be asked of the user.
%   arg-2 is the confidence the user has in that hypothesis
%   arg-3 is the current rule stack (used for why queries).
%
% askuser prints the query, followed by a set of instructions.
% it reads the response and calls respond/4 to handle that response

askuser(Hypo, CF, Rules) :-
    nl, write('User query:'), write(Hypo), nl,
    write('? '),
    read(Answer),
    respond(Answer, Hypo, CF, Rules).

%respond/4
%   arg-1 is the user response
%   arg-2 is the hypothesis presented to the user
%   arg-3 is the CF obtained for that hypothesis
%   arg-4 is the current rule stack (used for why queries).
%
% The basic scheme of respond/4 is to examine the response and return
% the certainty for the hypothesis if possible.
% If the response is a why query, how query, etc., it processes the query
% and then calls askuser to re prompt the user.

% Case 1: user enters a valid confidence factor.
respond(CF, _, CF, _) :-
    number(CF),
    CF =< 1.0, CF >= -1.0.

% Case 2: user enters 'n' for no.  Return a confidence factor of -1.0
respond(n, _, -1.0, _).

% Case 3: user enters 'y' for yes.  Return a confidence factor of 1.0
respond(y, _, 1.0, _).

% Case 4: user enters a pattern that matches the hypothesis.  This is useful if
% the hypothesis has variables that need to be bound.
respond(Hypo, Hypo, CF, _) :-
    write('Enter confidence in answer'), nl,
    write('?'),
    read(CF).

% Case 5: user enters a why query
respond(why, Hypo, CF, [Rule|Rules]) :-
    write_rule(Rule),
    askuser(Hypo, CF, Rules).

respond(why, Hypo, CF, []) :-
    write('Back to top of rule stack.'),
    askuser(Hypo, CF, []).

% Case 6: User enters a how query.  Build and print a proof
respond(how(X), Hypo, CF, Rules) :-
    build_proof(X, CF_X, Proof), !,
    write(X), write(' was concluded with certainty '), write(CF_X), nl, nl,
    write('The proof is '), nl, nl,
    write_proof(Proof, 0), nl, nl,
    askuser(Hypo, CF, Rules).

% User enters how query, could not build proof
respond(how(X), Hypo, CF, Rules):-
    write('The truth of '), write(X), nl,
    write('is not yet known.'), nl,
    askuser(Hypo, CF, Rules).

% Case 7: User asks for the rules that conclude a certain predicate
respond(rule(X), _, _, _) :-
    write('The following rules conclude about '), write(X), nl, nl,
    rule((X :- Premise), CF),
    write(rule((X :- Premise), CF)), nl,
    fail.

respond(rule(_), Hypo, CF, Rules) :-
    askuser(Hypo, CF, Rules).

% Case 8: User asks for help.
respond(help, Hypo, CF, Rules) :-
    print_instructions,
    askuser(Hypo, CF, Rules).

%Case 9: User wants to quit.
respond(quit, _, _, _) :- quit.

%Case 10: Unrecognized input
respond(_, Hypo, CF, Rules) :-
    write('Unrecognized response.'), nl,
    askuser(Hypo, CF, Rules).

quit.

%build_proof/3
%   arg-1 is the hypothesis being traced.
%   arg-2 is the CF of that hypothesis
%   arg-3 is the proof tree
%
% build_proof does not do threshold pruning, so it can show
% the proof for even hypotheses that would not succeed.
build_proof(Hypo, CF, ((Hypo, CF) :- given)) :-
    known(Hypo, CF), !.

build_proof(not(Hypo), CF, not(Proof)) :- !,
    build_proof(Hypo, CF_hypothesis, Proof),
    negate_cf(CF_hypothesis, CF).

build_proof((Hypo_1, Hypo_2), CF, (Proof_1, Proof_2)) :- !,
    build_proof(Hypo_1, CF_1, Proof_1),
    build_proof(Hypo_2, CF_2, Proof_2),
    min_cf(CF_1, CF_2, CF).


build_proof(do_ss(CF_rule,_HSB,List), CF_rule, List) :- !.

build_proof(Hypo, CF, ((Hypo, CF) :- Proof)) :-
    rule((Hypo :- (Premise)), CF_rule),
    build_proof(Premise, CF_premise, Proof),
    rule_cf(CF_rule, CF_premise, CF).

build_proof(Hypo, CF, ((Hypo, CF):- fact)) :-
    rule(Hypo, CF).

build_proof(Hypo, 1, ((Hypo, 1):- call)) :-
    call(Hypo).


% write_proof/2
%   arg-1 is a portion of a proof tree
%   arg-2 is the depth of that portion (for indentation)
%
% writes out a proof tree in a readable format
write_proof(((Hypo, CF) :- given), Level) :-
    indent(Level),
    write(Hypo), write(' CF= '), write(CF),
    write(' was given by the user'), nl, !.


write_proof(((Hypo, CF):- fact), Level) :-
    indent(Level),
    write(Hypo), write(' CF= '), write(CF),
    write(' was a fact in the knowledge base'), nl, !.

write_proof(((Hypo, CF) :- Proof), Level) :-
    indent(Level),
    write(Hypo), write(' CF= '), write(CF), write(' :-'), nl,
    New_level is Level + 1,
    write_proof(Proof, New_level), !.

write_proof(not(Proof), Level) :-
    indent(Level),
    write('not'), nl,
    New_level is Level + 1,
    write_proof(Proof, New_level), !.

write_proof((Proof_1, Proof_2), Level) :-
    write_proof(Proof_1, Level),
    write_proof(Proof_2, Level), !.

write_proof(((Hypo, CF):- call), Level) :-
    indent(Level),
    write(Hypo), write(' CF= '), write(CF),
    write(' was proven by a call to prolog'), nl, !.



write_proof_level(Level,List) :-
  ignore(write_proof(List,Level)).

% indent/1
%   arg-1 is the number of units to indent
indent(0).
indent(I) :-
    write('     '),
    I_new is I - 1,
    indent(I_new).

%print_instructions/0
% Prints all options for user responses
print_instructions :-
    nl, write('Response must be either:'), nl,
    write('    A confidence in the truth of the query.'), nl,
    write('      This is a number between -1.0 and 1.0.'), nl,
    write('    y or n, where y is equivalent to a confidence of 1.0 and'), nl,
    write('                  n is equivalent to a confidence of -1.0.'), nl,
    write('    Hypo, where Hypo is a pattern that will unify with the query'), nl,
    write('    why.'), nl,
    write('    how(X), where X is a hypothesis'), nl,
    write('    rule(X) to display all rules that conclude about X.'), nl,
    write('    quit, to terminate consultation'), nl,
    write('    help, to print this message'), nl.


% write_rule/1
%   arg-1 is a rule specification
% writes out the rule in a readable format
write_rule(rule((Hypo :- (Premise)), CF)) :-
    write(Hypo), write('if'), nl,
    write_premise(Premise), nl,
    write('CF = '), write(CF), nl.

write_rule(rule(Hypo, CF)) :-
    write(Hypo), nl,
    write('CF = '), write(CF), nl.


% write_premise
%   arg-1 is a rule premise
% writes it in a readable format.
write_premise((Premise_1, Premise_2)) :-
    !, write_premise(Premise_1),
    write_premise(Premise_2).
write_premise(not(Premise)) :-
    !, write('     '), write('not'), write(' '), write(Premise), nl.
write_premise(Premise) :-
    write('     '), write(Premise), nl.

