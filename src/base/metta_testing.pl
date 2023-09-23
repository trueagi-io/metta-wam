/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).

:- ensure_loaded(library(occurs)).
:- ensure_loaded(metta_utils).

% Reset loonit counters
loonit_reset :-
    flag(loonit_failure, _, 0),
    flag(loonit_success, _, 0).

% Increment loonit counters based on goal evaluation


color_g_mesg(C,G):-
  wots(S,user:call(G)), ansi_format([fg(C)], '~N~w~n', [S]),!.

loonit_asserts(Pre,G):- once(Pre),once(loonit_asserts1(Pre,G)).
loonit_asserts1(Pre,G) :- nop(Pre),
    call(G), !, color_g_mesg(cyan,write_src(loonit_success(G))),!,
    flag(loonit_success, X, X+1),!.
loonit_asserts1(_Pre,G) :-
    color_g_mesg(red,write_src(loonit_failure(G))),!,
    flag(loonit_failure, X, X+1), !. %itrace, G.
    %(thread_self(main)->trace;sleep(0.3))

% Generate loonit report with colorized output
loonit_report :-
    flag(loonit_success, Successes, Successes),
    flag(loonit_failure, Failures, Failures),
    ansi_format([bold], 'LoonIt Report~n',[]),
    format('------------~n'),
    loonit_report(Successes,Failures).

loonit_report(0,0):-!, ansi_format([fg(yellow)], 'Nothing to report~n', []).
loonit_report(Successes,Failures):-
    ansi_format([fg(green)], 'Successes: ~w~n', [Successes]),
    ansi_format([fg(red)], 'Failures: ~w~n', [Failures]).

% Resets loonit counters, consults the given file, and prints the status report.
loon_metta(File) :-
    flag(loonit_success, WasSuccesses, 0),
    flag(loonit_failure, WasFailures, 0),
    load_metta(File),
    loonit_report,
    flag(loonit_success, _, WasSuccesses),
    flag(loonit_failure, _, WasFailures),!.


quick_test:-
  set_prolog_flag(encoding,iso_latin_1),
   forall(quick_test(Test),
                  forall(open_string(Test,Stream),
                    load_metta_stream('&self',Stream))).

/*
 tests for term expander


*/
:- if(debugging(term_expansion)).
:- enable_arc_expansion.
:- style_check(-singleton).
dte:- set(_X.local) = val.
dte:- gset(_X.global) = gval.
dte:- must_det_ll((set(_X.a) = b)).
dte:- must_det_ll(locally(nb_setval(e,X.locally),dte([foo|set(X.tail)]))).
dte:- member(set(V.element),set(V.list)).
dte(set(E.v)):- set(E.that)=v.
:- style_check(+singleton).
:- disable_arc_expansion.
:- listing(dte).
:- endif.



% 1. Recursive Approach
factorial_recursive(0, 1).
factorial_recursive(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial_recursive(N1, Result1),
    Result is N * Result1.

% 2. Tail Recursive Approach
factorial_tail_recursive(N, Result) :- factorial_tail_helper(N, 1, Result).

factorial_tail_helper(0, Acc, Acc).
factorial_tail_helper(N, Acc, Result) :-
    N > 0,
    NewAcc is Acc * N,
    N1 is N - 1,
    factorial_tail_helper(N1, NewAcc, Result).

% 3. Accumulator Approach
factorial_accumulator(N, Result) :- factorial_acc(N, 1, Result).

factorial_acc(0, Result, Result).
factorial_acc(N, Acc, Result) :-
    N > 0,
    NewAcc is Acc * N,
    N1 is N - 1,
    factorial_acc(N1, NewAcc, Result).

% You can test each one by querying, for example:
% ?- factorial_recursive(5, X






% Example-usage
example_usages :-
    fetch_or_create_space(newSpace,Space),  % Assuming fetch_or_create_space/1 is defined to initialize a space
    'add-atom'(Space, a),
    'add-atom'(Space, b),
    'add-atom'(Space, c),
    'match'(Space, a, Template),
    write('Matched template: '), writeln(Template),


    write('Initial space: '), writeln(Space),

    'add-atom'(Space, a),
    write('Space after adding "a": '), writeln(Space),

    'add-atom'(Space, b),
    write('Space after adding "b": '), writeln(Space),

    'replace-atom'(Space, a, c),
    write('Space after replacing "a" with "c": '), writeln(Space),

    'get-atoms'(Space, Atoms),
    write('Atoms in space: '), writeln(Atoms),

    'atom-count'(Space, Count),
    write('Number of atoms in space: '), writeln(Count).

% Test case for clearing a space
test_clear_space :-
    writeln('Test: Clearing a space'),
    init_space('&kb1'),
    'add-atom'('&kb1', a),
    'add-atom'('&kb1', b),
    writeln('Expected Count Before Clearing: 2'),
    'atom-count'('&kb1', CountBefore), writeln('Actual Count:'), writeln(CountBefore),
    writeln('Expected Atoms Before Clearing: [b, a]'),
    'get-atoms'('&kb1', AtomsBefore), writeln('Actual Atoms:'), writeln(AtomsBefore),
    'clear-atoms'('&kb1'),
    writeln('Expected Count After Clearing: 0'),
    'atom-count'('&kb1', CountAfter), writeln('Actual Count:'), writeln(CountAfter),
    writeln('Expected Atoms After Clearing: []'),
    'get-atoms'('&kb1', AtomsAfter), writeln('Actual Atoms:'), writeln(AtomsAfter).

% Test case for various operations on a space
test_operations :-
    writeln('Test: Various Operations on a Space'),
    init_space('&kb2'),
    'add-atom'('&kb2', a),
    'add-atom'('&kb2', b),
    writeln('Expected Count After Adding: 2'),
    'atom-count'('&kb2', Count1), writeln('Actual Count:'), writeln(Count1),
    writeln('Expected Atoms After Adding: [b, a]'),
    'get-atoms'('&kb2', Atoms1), writeln('Actual Atoms:'), writeln(Atoms1),
    'remove-atom'('&kb2', a),
    writeln('Expected Atoms After Removing a: [b]'),
    'get-atoms'('&kb2', Atoms2), writeln('Actual Atoms:'), writeln(Atoms2),
    'replace-atom'('&kb2', b, c),
    writeln('Expected Atoms After Replacing b with c: [c]'),
    'get-atoms'('&kb2', Atoms3), writeln('Actual Atoms:'), writeln(Atoms3).

% Run the test cases
run_tests :-
    writeln('Running test_clear_space:'),
    test_clear_space,
    writeln('---'),
    writeln('Running test_operations:'),
    test_operations.


% Test case for various operations on a space
test_my_space :-
    fetch_or_create_space('&KB', InstanceOfKB),
    'clear-atoms'('&KB'),
    'add-atom'(InstanceOfKB, a),
    'add-atom'(InstanceOfKB, b),
    'atom-count'(InstanceOfKB, Count1),
    writeln('Should print 2: ' : Count1),

    'get-atoms'(InstanceOfKB, Atoms1),
    writeln('Should print [b, a]: ' : Atoms1),

    'remove-atom'(InstanceOfKB, a),
    'get-atoms'(InstanceOfKB, Atoms2),
    writeln('Should print [b]: ' : Atoms2),

    'replace-atom'(InstanceOfKB, b, c),
    'get-atoms'(InstanceOfKB, Atoms3),
    writeln('Should print [c]: ' : Atoms3),

    space_original_name(InstanceOfKB, OriginalName),
    writeln('Should print &KB':OriginalName),

    fetch_or_create_space('&KB'),
    'add-atom'('&KB', x),
    'add-atom'('&KB', y),
    'atom-count'('&KB', Count2),
    writeln('Should print 3: ' : Count2),

    'get-atoms'('&KB', Atoms4),
    writeln('Should print [c, y, x]: ' : Atoms4),

    'remove-atom'('&KB', x),
    'get-atoms'('&KB', Atoms5),
    writeln('Should print [c,y]: ' : Atoms5),

    'replace-atom'('&KB', y, z),
    'get-atoms'(InstanceOfKB, Atoms6),
    writeln('Should print [c,z]: ' : Atoms6).


% Test the code
test_clr_my_kb22 :-
    fetch_or_create_space('&kb22'),
    'add-atom'('&kb22', a),
    'add-atom'('&kb22', b),
    'atom-count'('&kb22', Count1), writeln(Count1),
    'get-atoms'('&kb22', Atoms1), writeln(Atoms1),
    'clear-atoms'('&kb22'),
    'atom-count'('&kb22', Count2), writeln(Count2),
    'get-atoms'('&kb22', Atoms2), writeln(Atoms2).

% Test the code
test_my_kb2:-
   fetch_or_create_space('&kb1', InstanceOfKB),
   \+ \+ ('add-atom'('&kb1', a)),
   \+ \+ ('add-atom'('&kb1', b)),
   \+ \+ ('atom-count'('&kb1', Count), writeln(Count)),
   \+ \+ ('get-atoms'('&kb1', Atoms), writeln(Atoms)),
   \+ \+ ('remove-atom'(InstanceOfKB, a)),
   \+ \+ ('get-atoms'('&kb1', NewAtoms), writeln(NewAtoms)),
   \+ \+ ('replace-atom'('&kb1', b, c)),
   \+ \+ ('get-atoms'('&kb1', FinalAtoms), writeln(FinalAtoms)),
   \+ \+ (space_original_name(InstanceOfKB, OriginalName), writeln(OriginalName)),
   \+ \+ (fetch_or_create_space('&kb2',_)),  % Creating a new space with a different name
   \+ \+ ('add-atom'('&kb2', a)),
   \+ \+ ('add-atom'('&kb2', b)),
   \+ \+ ('atom-count'('&kb2', Count), writeln(Count)),
   \+ \+ ('get-atoms'('&kb2', Atoms), writeln(Atoms)),
   \+ \+ ('remove-atom'('&kb2', a)),
   \+ \+ ('get-atoms'('&kb2', NewAtoms), writeln(NewAtoms)),
   \+ \+ ('replace-atom'('&kb2', b, c)),
   \+ \+ ('get-atoms'('&kb2', FinalAtoms), writeln(FinalAtoms)).




end_of_file. % comment this out once to get these files in your readline history
mf('./1-VSpaceTest.metta').
mf('./2-VSpaceTest.metta').
mf('./3-Learn-Rules.metta').
mf('./4-VSpaceTest.metta').
mf('./5-Learn-Flybase.metta').
mf('./6-Learn-Flybase-Full.metta').
mf('./8-VSpaceTest.metta').
mf('./autoexec.metta').
mf('./data/OBO-Metta/export/Alliance_of_Genome_Resources.metta').
mf('./data/OBO-Metta/export/biosapiens.metta').
mf('./data/OBO-Metta/export/chebi_fb_2023_04.metta').
mf('./data/OBO-Metta/export/DBVAR.metta').
mf('./data/OBO-Metta/export/doid.metta').
mf('./data/OBO-Metta/export/flybase_controlled_vocabulary.metta').
mf('./data/OBO-Metta/export/flybase_stock_vocabulary.metta').
mf('./data/OBO-Metta/export/fly_anatomy.metta').
mf('./data/OBO-Metta/export/fly_development.metta').
mf('./data/OBO-Metta/export/gene_group_FB2023_04.metta').
mf('./data/OBO-Metta/export/go-basic.metta').
mf('./data/OBO-Metta/export/image.metta').
mf('./data/OBO-Metta/export/psi-mi.metta').
mf('./data/OBO-Metta/export/slice.chebi.metta').
mf('./data/OBO-Metta/export/so-simple.metta').
mf('./data/OBO-Metta/export/so.metta').
mf('./data/OBO-Metta/export/SOFA.metta').
mf('./examples/compat/common/BelieveMe.metta').
mf('./examples/compat/common/EqualityType.metta').
mf('./examples/compat/common/EqualityTypeTest.metta').
mf('./examples/compat/common/formula/DeductionFormula.metta').
mf('./examples/compat/common/formula/DeductionFormulaTest.metta').
mf('./examples/compat/common/formula/ImplicationDirectIntroductionFormula.metta').
mf('./examples/compat/common/formula/ModusPonensFormula.metta').
mf('./examples/compat/common/In.metta').
mf('./examples/compat/common/InTest.metta').
mf('./examples/compat/common/List.metta').
mf('./examples/compat/common/ListTest.metta').
mf('./examples/compat/common/Maybe.metta').
mf('./examples/compat/common/MaybeTest.metta').
mf('./examples/compat/common/Num.metta').
mf('./examples/compat/common/NumTest.metta').
mf('./examples/compat/common/OrderedSet.metta').
mf('./examples/compat/common/OrderedSetTest.metta').
mf('./examples/compat/common/Record.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValue.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValueTest.metta').
mf('./examples/compat/common/truthvalue/MeasEq.metta').
mf('./examples/compat/common/truthvalue/TemporalTruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValueTest.metta').
mf('./examples/compat/dependent-types/DeductionDTL.metta').
mf('./examples/compat/dependent-types/DeductionDTLTest.metta').
mf('./examples/compat/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTL.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ModusPonensDTL.metta').
mf('./examples/compat/dependent-types/ModusPonensDTLTest.metta').
mf('./examples/compat/entail/DeductionEntail.metta').
mf('./examples/compat/entail/DeductionEntailTest.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntail.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntailTest.metta').
mf('./examples/compat/equal/DeductionEqual.metta').
mf('./examples/compat/equal/DeductionEqualTest.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqual.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqualTest.metta').
mf('./examples/compat/match/DeductionImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/match/DeductionMatch.metta').
mf('./examples/compat/match/DeductionMatchTest.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatch.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/prob-dep-types/inf_order_probs.metta').
mf('./examples/compat/prob-dep-types/prob_dep_types.metta').
mf('./examples/compat/recursion-schemes/src/base.metta').
mf('./examples/compat/recursion-schemes/src/examples/benchmark.metta').
mf('./examples/compat/recursion-schemes/src/examples/expression.metta').
mf('./examples/compat/recursion-schemes/src/schemes.metta').
mf('./examples/compat/synthesis/experiments/non-determinism.metta').
mf('./examples/compat/synthesis/experiments/self-contained-synthesize.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-superpose.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-type-checking.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify.metta').
mf('./examples/compat/synthesis/experiments/unify-via-case.metta').
mf('./examples/compat/synthesis/experiments/unify-via-let.metta').
mf('./examples/compat/synthesis/Synthesize.metta').
mf('./examples/compat/synthesis/SynthesizeTest.metta').
mf('./examples/compat/synthesis/Unify.metta').
mf('./examples/compat/synthesis/UnifyTest.metta').
mf('./examples/compat/test_scripts/a1_symbols.metta').
mf('./examples/compat/test_scripts/a2_opencoggy.metta').
mf('./examples/compat/test_scripts/a3_twoside.metta').
mf('./examples/compat/test_scripts/b0_chaining_prelim.metta').
mf('./examples/compat/test_scripts/b1_equal_chain.metta').
mf('./examples/compat/test_scripts/b2_backchain.metta').
mf('./examples/compat/test_scripts/b3_direct.metta').
mf('./examples/compat/test_scripts/b4_nondeterm.metta').
mf('./examples/compat/test_scripts/b5_types_prelim.metta').
mf('./examples/compat/test_scripts/c1_grounded_basic.metta').
mf('./examples/compat/test_scripts/c2_spaces.metta').
mf('./examples/compat/test_scripts/c2_spaces_kb.metta').
mf('./examples/compat/test_scripts/c3_pln_stv.metta').
mf('./examples/compat/test_scripts/d1_gadt.metta').
mf('./examples/compat/test_scripts/d2_higherfunc.metta').
mf('./examples/compat/test_scripts/d3_deptypes.metta').
mf('./examples/compat/test_scripts/d4_type_prop.metta').
mf('./examples/compat/test_scripts/d5_auto_types.metta').
mf('./examples/compat/test_scripts/e1_kb_write.metta').
mf('./examples/compat/test_scripts/e2_states.metta').
mf('./examples/compat/test_scripts/e3_match_states.metta').
mf('./examples/compat/test_scripts/f1_imports.metta').
mf('./examples/compat/test_scripts/f1_moduleA.metta').
mf('./examples/compat/test_scripts/f1_moduleB.metta').
mf('./examples/compat/test_scripts/f1_moduleC.metta').
mf('./examples/compat/test_scripts/_e2_states_dia.metta').
mf('./examples/fibo.metta').
mf('./examples/fwgc.metta').
mf('./examples/httpclient.metta').
mf('./examples/NARS.metta').
mf('./examples/NARS_listing.metta').
mf('./examples/RUN_minnars.metta').
mf('./examples/RUN_tests0.metta').
mf('./examples/RUN_tests1.metta').
mf('./examples/RUN_tests2.metta').
mf('./examples/RUN_tests3.metta').
mf('./examples/send-more.metta').
mf('./examples/talk80.metta').
mf('./examples/VRUN_tests0.metta').
mf('./examples/VRUN_tests1.metta').
mf('./examples/VRUN_tests2.metta').
mf('./examples/VRUN_tests3.metta').
mf('./metta_vspace/nm_test.metta').
mf('./metta_vspace/r.metta').
mf('./metta_vspace/test_nspace.metta').
:- forall(mf(H),add_history1(load_metta(H))).
%:- load_metta





end_of_file.



parsing(String, Expr) :- string(String),!,string_codes(String,Codes),phrase(expressions(Expr), Codes).
parsing(String, Expr) :- phrase(expressions(Expr), String).

expressions([E|Es]) -->
    ws, expression(E), ws,
    !, % single solution: longest input match
    expressions(Es).
expressions([]) --> [].

% ws --> ";",until_eol,
ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% A number N is represented as n(N), a symbol S as s(S).

expression(s(A))         --> symbol(Cs), { atom_codes(A, Cs) }.
expression(n(N))         --> number(Cs), { number_codes(N, Cs) }.
expression(List)         --> [L],{is_bracket_lr(L,R)},expressions(List), [R].
expression([s(quote),Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

symbol([A|As]) -->
    [A],
    { is_ok_symbolchar(A) },
    symbolr(As).

symbolr([A|As]) -->
    [A],
    { is_ok_symbolchar(A) ; code_type(A, alnum) },
    symbolr(As).
symbolr([]) --> [].

is_bracket_lr(L,R):- member(LR,["()","{}","[]","\"\""]), nth0(0,LR,L),nth0(1,LR,R).
is_ok_symbolchar(A):- \+ code_type(A, space), \+ code_type(A, white), \+ is_bracket_lr(A,_), \+ is_bracket_lr(_,A).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

codelist_to_forms_i(AsciiCodesList,FormsOut):-
    parsing(AsciiCodesList, Forms0),
    compile_all(Forms0, FormsOut),!.

run(Program, Values) :-
    parsing(Program, Forms0),
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).

unfunc(s(S), S).
unfunc(t, t).
unfunc(n(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).

fold([], _, V, n(V)).
fold([n(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).

compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

compile(F0, F) :-
    (   F0 = n(_)   -> F = F0
    ;   F0 = s(t)   -> F = t
    ;   F0 = s(nil) -> F = []
    ;   F0 = s(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = [s(quote),Arg] -> F = [quote,Arg]
    ;   F0 = [s(setq),s(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = [s(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = [s(defun),s(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = [s(Op)|Args0] -> compile_all(Args0, Args), F = [user(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval(n(N), n(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval(s(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, [n(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], s(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval(user(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval('bind!', [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate goal_truth(0,*,//,//).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.

bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).

run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).

%if_script_file_time(X):-if_startup_script(time(X)).
if_script_file_time(_):-!.
%if_script_file_time(X):- nop(time(X)).

% Append:
    :- if_script_file_time(run("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].


% Fibonacci, naive version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].


% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].


% Fibonacci, iterative version:
    :- if_script_file_time(run("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 30,794 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 12831368 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].



% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 350)")).

    %@ % 44,595 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 14526532 Lips)
    %@ V = [fib, fib1, 6254449428820551641549772190170184190608177514674331726439961915653414425].


% Higher-order programming and eval:
    :- if_script_file_time(run("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].

%:- ensure_loaded(metta_reader).



#[test]
fn test_case_operation() {
    let metta = new_metta_rust();
    let result = metta.run(&mut SExprParser::new("
    "));

    let expected = metta.run(&mut SExprParser::new("
        ! OK
        ! 7
        ! (superpose (OK-3 OK-4))
        ! (superpose (3 4 5))
        ! (superpose ())
    "));
    assert_eq!(result, expected);

    let metta = new_metta_rust();
    let result = metta.run(&mut SExprParser::new("
        (Rel-P A B)
        (Rel-Q A C)

        ; cases can be used for deconstruction
        !(case (match &self ($rel A $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))))

        ; %void% can be used to capture empty results
        !(case (match &self ($rel B $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))
            (%void% no-match)))

        ; a functional example
        (= (maybe-inc $x)
            (case $x
            (((Just $v) (Just (+ 1 $v)))
                (Nothing Nothing)))
        )
        !(maybe-inc Nothing)
        !(maybe-inc (Just 2))
    "));
    let expected = metta.run(&mut SExprParser::new("
        ! (superpose ((Q C) (P B)))
        ! no-match
        ! Nothing
        ! (Just 3)
    "));
    assert_eq_metta_results!(result, expected);
}



use hyperon::metta::text::*;
use hyperon::metta::runner::new_metta_rust;

#[test]
fn test_reduce_higher_order() {
    let program = "
        ; Curried plus
        (: plus (-> Number (-> Number Number)))
        (= ((plus $x) $y) (+ $x $y))
        ; Define inc as partial evaluation of plus
        (: inc (-> (-> Number Number)))
        (= (inc) (plus 1))

        !(assertEqualToResult ((inc) 2) (3))
    ";
    let metta = new_metta_rust();

    let result = metta.run(&mut SExprParser::new(program));

    assert_eq!(result, Ok(vec![vec![]]));
}



use hyperon::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_custom_match_with_space() {
    let mut main_space = GroundingSpace::new();
    let mut inserted_space = GroundingSpace::new();
    inserted_space.add(expr!("implies" ("B" x) ("C" x)));
    inserted_space.add(expr!("implies" ("A" x) ("B" x)));
    inserted_space.add(expr!("A" "Sam"));
    main_space.add(Atom::gnd(inserted_space));
    let result = main_space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].resolve(&VariableAtom::new("y")), Some(expr!("B" "Sam")));
    assert_eq!(result[0].resolve(&VariableAtom::new("z")), Some(expr!("C" "Sam")));
}



use hyperon::*;
use hyperon::common::*;
use hyperon::metta::interpreter::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_types_in_metta() {
    let mut space = GroundingSpace::new();
    space.add(expr!("=" ("check" (":" n "Int")) ({IS_INT} n)));
    space.add(expr!("=" ("check" (":" n "Nat")) ({AND} ("check" (":" n "Int")) ({GT} n {0}))));
    space.add(expr!("=" ("if" {true} then else) then));
    space.add(expr!("=" ("if" {false} then else) else));
    space.add(expr!(":" "if" ("->" "bool" "Atom" "Atom" "Atom")));
    space.add(expr!("=" ("fac" n) ("if" ("check" (":" n "Nat")) ("if" ({EQ} n {1}) {1} ({MUL} n ("fac" ({SUB} n {1})))) ({ERR}))));

    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Nat"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Nat"))), Ok(vec![expr!({false})]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(3)} "Nat")) "ok" "nok")), Ok(vec![expr!("ok")]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(-3)} "Nat")) "ok" "nok")), Ok(vec![expr!("nok")]));
    assert_eq!(interpret(&space, &expr!("fac" {1})), Ok(vec![expr!({1})]));
    assert_eq!(interpret(&space, &expr!("fac" {3})), Ok(vec![expr!({6})]));
}








    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" b c))),
        bind_set![{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" a c))), BindingsSet::empty());
    }

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals" x x));

        let result = space.query(&expr!("equals" y z));
        assert_eq!(result, bind_set![{ y: expr!(z) }]);
    }

    #[test]
    fn test_match_query_variable_via_data_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!(x x));
        assert_eq!(space.query(&expr!(y (z))), bind_set![{y: expr!((z))}]);
    }

    #[test]
    fn test_match_if_then_with_x() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" then) then));
        assert_eq!(space.query(&expr!("=" ("if" "True" "42") X)),
        bind_set![{X: expr!("42")}]);
    }

    #[test]
    fn test_match_combined_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!("posesses" "Sam" "baloon"));
        space.add(expr!("likes" "Sam" ("blue" "stuff")));
        space.add(expr!("has-color" "baloon" "blue"));

        let result = space.query(&expr!("," ("posesses" "Sam" object)
        ("likes" "Sam" (color "stuff"))
        ("has-color" object color)));
        assert_eq!(result, bind_set![{object: expr!("baloon"), color: expr!("blue")}]);
    }

