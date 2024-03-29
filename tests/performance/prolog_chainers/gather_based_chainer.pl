
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

codes_for(G, P) :-
    transcribed_to(G, T),
    translates_to(T, P).

in_tad_with(S, G1) :-
    freeze(G2,G1\==G2),
    in_tad_region(G1, T),
    in_tad_region(G2, T),
    closest_gene(S, G2).

relevant_gene(G, S) :-
    in_tad_with(S, G),
    eqtl(S, G).

relevant_gene_coexpression(G1, S) :-
    relevant_gene(G2, S),
    different(G1,G2),
    coexpressed_with(G1, G2).

member(G, O, 0) :-
    codes_for(G, P),
    go_gene_product(O, P).

member(G, O, D) :-
    once((D #= D0 + 1,  D0 #>= 0,   D #< 4)),
    rel_type(ontology_relationship(X, O), subclass),
    member(G, X, D0).

relevant_go(O, S) :-
   member(G, O, _D),
    (relevant_gene(G, S)
    ; relevant_gene_coexpression(G, S)),
  member(G, O, _D).



different(G1,G2):-
  freeze(G1, G1\==G2),
  freeze(G2, G2\==G1).

/*O = ontology_term(go_0010629),
S = sequence_variant(rs11075915),
*/
/*
rule_head(member(_,_,_)).
rule_head(relevant_go(_, _)).
rule_head(relevant_gene_coexpression(_, _)).
rule_head(relevant_gene(_, _)).
rule_head(in_tad_with(_, _)).
rule_head(codes_for(_, _)).
*/
% Get these introspectively
rule_head(Head):-
    predicate_property(rule_head(_),line_count(ThisLine)),
    source_file(rule_head(_),ThisFile),source_file(Head,ThisFile),
    predicate_property(Head,line_count(LineNo)),LineNo<ThisLine,
   \+ predicate_property(Head,number_rules(0)).


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




solv(Goal, Proof):-
  solve(Goal, Proof).

 % nl,print_tree(proof=Proof),nl,nl.

% solve(Goal, Proof): Goal is a goal to be proven, Proof is the proof of the goal.
%solve(Goal, Proof):- var(Goal),rule_head(Goal),solve(Goal, Proof).
%solve(Goal, Proof):- var(Goal),!,fact_head(Goal),solve(Goal, Proof).
solve(true, true) :- !.  % Base case: the empty goal is trivially solved.
solve(not(Goal), not(Proof)) :- solve(Goal, Proof).

% optimization
solve((Goal1, Goal2), [reorder_goals|Proof]) :- second_is_better_than_first(Goal1,Goal2),!, solve((Goal2, Goal1), Proof).
solve((Goal1; Goal2), swap_or(Proof)) :- second_is_better_than_first(Goal1,Goal2),!, solve((Goal2; Goal1), Proof).

% Conjunction: solve each goal in the conjunction.
solve((Goal1, Goal2), [Proof1 | Proof2]) :- !, % stop code search of _this_ solve/2
    solve(Goal1, Proof1),
    solve(Goal2, Proof2),!. % stops extra backtracking bindings of _other_ solve/2s


% Disjunction: solve one of the goals in the disjunction  (Goal1;Goal2).
solve((Goal1 ; _), left(Proof1)) :-    solve(Goal1, Proof1).
solve((_ ; Goal2), right(Proof2)) :- !, solve(Goal2, Proof2).

% Check if the goal is a built-in predicate.
solve(Goal, built_in(Goal)) :-
    is_cpu_goal(Goal),
    call(Goal). % Directly call the built-in predicate.

% For any other goal: find a clause that can solve the goal.
solve(Goal, Proof) :-
   % predicate_property(Goal, number_of_clauses(_)), % Ensure it is a user-defined predicate.
   % call(Goal),
    clause(Goal, Body),  % Find a clause in the program where Goal is the head and Body is the body.
    solve_body(Goal, Body, Proof).  % Solve the body of the clause.

solve_body( Goal, true, Goal):-!.
solve_body(Goal, Body, [implies,Proof,Goal]):- solve(Body, Proof).

is_cpu_goal(Goal):- predicate_property(Goal, built_in),!.
is_cpu_goal(Goal):- \+ rule_head(Goal), !.
is_cpu_goal(Goal):- predicate_property(Goal, number_of_rules(Z)), !,Z==0.










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



