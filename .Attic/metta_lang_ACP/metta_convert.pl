/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpeter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming functional/logic programs. It handles different
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
 */

%*********************************************************************************************
% PROGRAM FUNCTION: Translate Prolog code to MeTTa code
%*********************************************************************************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Defines a custom operator =~ with precedence 700 and xfx type, meaning it is a non-associative infix operator.
% This operator could be used for a specialized equality or pattern-matching operation in the program.
:- op(700,xfx,'=~').

% When the the `metta_interp` library is loaded, it makes sure the rest of the files are intially loaded in
% the correct order independent of which file is loaded first the needed predicates and ops are defined.
:- ensure_loaded(metta_interp).

% ===============================
%    TESTING
% ===============================

%!  fb is det.
%
%   This predicate is used for testing purposes. It first compiles the program by calling 'make'.
%   After that, it writes information for each clause of 'fb0', a test case rule, and executes the goals.
%
%   The 'make' predicate is commonly used in Prolog for recompiling any modified predicates.
%   The 'forall' loop iterates over all clauses of 'fb0', writing and calling the goals in the clauses.
%
%   @example
%   ?- fb.
%   ;; ===========================================
%   ;; ?- (two_pi(R) :- (pi(A), +(A, A, B), R is B)).
%   ;; ?- factorial_tail_basic.
%   ;; ?- funct.
%   ;; ===========================================
fb :-
    make,  % Recompiles the program.
    writeln(';; ==========================================='),
    forall(
        % Retrieve and execute each clause of 'fb0'.
        (clause(fb0, Goal), write(';; '), writeq(?- Goal), nl, call(Goal)),
        % After processing each clause, print a separator.
        writeln(';; ===========================================')
    ).

%!  fb0 is det.
%
%   This rule provides test cases by showing mettalog sources. Each clause of 'fb0' uses 'show_mettalog_src'
%   to display specific mettalog definitions. These are examples of how Prolog can show the structure of logical
%   rules or concepts using 'show_mettalog_src/1'.
%
%   @example
%   ?- fb0.
%   This will show the mettalog source for two_pi/1, factorial_tail_basic/0, and funct/0.
fb0 :- show_mettalog_src((two_pi(R) :- (pi(A), +(A, A, B), R is B))).
fb0 :- show_mettalog_src(factorial_tail_basic).
fb0 :- show_mettalog_src(funct).

%!  print_metta_src is det.
%   Displays mettalog sources for files or predicates that contain 'metta'.
%   This predicate calls 'show_mettalog_src/0' to display all mettalog sources in the program.
%   The 'show_mettalog_src' rule compiles the program and shows the mettalog sources for each
%   source file that contains 'metta' in its name or content.
%
%   This is useful for listing all relevant mettalog definitions after ensuring that the program is up-to-date.
%
%   @example
%   ?- print_metta_src.
print_metta_src :- show_mettalog_src.

%!  show_mettalog_src is det.
%
%   Compiles the Prolog program and displays the mettalog sources for each source file
%   containing 'metta' in its file name.
%
%   This predicate uses `make/0` to recompile the program, then iterates through all
%   the source files, checking if the file name contains 'metta'. If so, it calls
%   `show_mettalog_src/1` with the corresponding predicate.
%
%   @example Compile and show mettalog sources:
%     ?- show_mettalog_src.
%     % This will compile the current Prolog program and print mettalog source details.
%
show_mettalog_src:-
    % Recompile the program with make/0 to ensure all source changes are applied.
    make,
    % For all source files whose file names contain 'metta', display the mettalog source.
    forall(
        (
            % Retrieve the predicate and its corresponding source file.
            source_file(AsPred, File),
            % Check if the file name contains 'metta'.
            symbol_contains(File, metta)
        ),
        % Show the mettalog source for the retrieved predicate.
        show_mettalog_src(AsPred)
    ).

%!  show_mettalog_src(+Spec) is det.
%
%   Displays the mettalog source for predicates that match the specification `Spec`.
%   This predicate handles various types of input to identify the predicates whose
%   source should be displayed:
%
%   - If `Spec` is in the form `F/A` (functor and arity), it directly shows the mettalog
%     source for all predicates with functor `F` and arity `A`.
%   - If `Spec` is a predicate term, it extracts the functor and arity and shows
%     the source for predicates matching the functor and arity of the term.
%   - If `Spec` is just the functor `F`, it shows the source for all predicates with
%     that functor, regardless of arity.
%   - If `Spec` is an atom `C` and matches part of a predicate functor, it shows the
%     source for all predicates whose functor contains the atom `C`.
%   - If none of the above match, it uses `show_cvts/1` as a fallback to display
%     the mettalog source.
%
%   @arg Spec The specification of the predicate(s) whose mettalog source is to be shown.
%        This can be of the form `F/A` (functor and arity), a predicate term, or an atom.
%
%   @example Show the mettalog source for a specific functor and arity:
%     ?- show_mettalog_src(member/2).
%
%   @example Show the mettalog source for all predicates with a specific functor:
%     ?- show_mettalog_src(member).
%
%   @example Show the mettalog source for all predicates whose functor contains 'metta':
%     ?- show_mettalog_src('metta').
%
show_mettalog_src(F/A):- nonvar(F), !,
    forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(AsPred):- functor(AsPred, F, A),\+ \+ current_predicate(F/A),!,forall(current_predicate(F/A),
    show_mettalog_src(F,A)).
show_mettalog_src(F):- atom(F),\+ \+ current_predicate(F/_),!,forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(C):- atom(C),\+ \+ (current_predicate(F/_), once(atom_contains(F,C))),!,
    forall((current_predicate(F/A), once(atom_contains(F,C))), show_mettalog_src(F,A)).
show_mettalog_src(C):- show_cvts(C), !.

% The 'show_space_src' rules compile the program and show space sources for each space predicate.

%!  show_space_src is det.
%
%   This predicate compiles the program using `make/0` and then iterates over all space predicates.
%   For each space predicate, it calls `show_space_src/1` to display the space-related source code.
%
%   This is useful for debugging or inspecting the Prolog predicates related to "space" concepts in the system.
%
%   @example
%   ?- show_space_src.
%   % This will compile the program and display the source code for all space-related predicates.
%
show_space_src :-
    % Recompile the program with make/0 to ensure all source changes are applied.
    make,
    % For all space predicates, display the source code.
    forall(
        space_preds(AsPred),
        show_space_src(AsPred)
    ).

% Similar to the 'show_mettalog_src' rules, these rules handle different cases for 'show_space_src'
% with different input parameters and perform various checks and actions based on the type and value of the input.

%!  show_space_src(+Spec) is det.
%
%   This predicate displays the source code for space-related predicates that match the provided specification `Spec`.
%   It handles different types of input to identify the relevant predicates:
%
%   - If `Spec` is in the form `F/A` (functor and arity), it shows the source code for all predicates with functor `F` and arity `A`.
%   - If `Spec` is a predicate term, it extracts the functor and arity, and shows the source code for predicates with matching functor and arity.
%   - If `Spec` is just the functor `F`, it shows the source for all predicates with that functor, regardless of arity.
%   - If `Spec` is an atom `C` and matches part of a predicate functor, it shows the source for all predicates whose functor contains `C`.
%   - If none of the above match, it uses `show_cvts/1` as a fallback to display converted terms.
%
%   @arg Spec The specification of the predicate(s) whose space-related source code is to be shown.
%        This can be in the form `F/A` (functor and arity), a predicate term, or an atom.
%
%   @example Show the space-related source code for a specific functor and arity:
%     ?- show_space_src(member/2).
%
%   @example Show the space-related source for all predicates with a specific functor:
%     ?- show_space_src(member).
%
%   @example Show the space-related source for all predicates whose functor contains 'space':
%     ?- show_space_src('space').
%
show_space_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A),
    show_space_src(F,A)).
show_space_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!,
    forall((current_predicate(F/A),once(atom_contains(F,C))), show_space_src(F,A)).
show_space_src(C):- show_cvts(C),!.

% 'show_cvts' rule processes a term, performing different actions based on the structure of the term.

%!  show_cvts(+Term) is det.
%
%   Processes the given `Term`, converting lists in S-expression form to Prolog terms using `sexpr_s2p/2`.
%   If the result is not a list, recursively processes the converted term.
%
%   @arg Term The input term to process, which can be a list or any Prolog term.
%
show_cvts(Term) :-
    once((is_list(Term), sexpr_s2p(Term, PF))),
    \+ is_list(PF), !,
    show_cvts(PF).
show_cvts(Term):- iz_conz(Term),!, ppc(orig,Term),Term = FunctForm,
  functs_to_preds(FunctForm,Prolog), ppc(preds,Prolog),
  preds_to_functs(Prolog,NFunctForm), ppc(functs,NFunctForm).
show_cvts(Term):- ppc(orig,Term),
  preds_to_functs(Term,FunctForm), ppc(functs,FunctForm),
  functs_to_preds(FunctForm,Prolog), ppc(preds,Prolog).

% 'show_mettalog_src' for specific predicate, prints metta clauses if they exist in the source file containing 'metta'.

%!  show_mettalog_src(+F, +A) is det.
%
%   Displays the mettalog clauses for a specific predicate with functor `F` and arity `A` if they exist in
%   a source file that contains 'metta' in its name.
%
%   - It first checks if the predicate has clauses.
%   - Then, it checks if the source file for the predicate contains 'metta' in its name.
%   - If these conditions are met, it retrieves and prints all the clauses for the predicate.
%
%   @arg F The functor of the predicate.
%   @arg A The arity of the predicate.
%
%   @example Show the mettalog source for a predicate:
%     ?- show_mettalog_src(my_predicate, 2).
%
show_mettalog_src(F, A) :-
    functor(Head, F, A),
    ignore((
        % Check if the predicate has clauses and its source file contains 'metta'.
        predicate_property(Head, number_of_clauses(_)),source_file(Head, File),atom_contains(File, metta),!,nl,
        % Find all clauses of the predicate and print them.
        findall((Head :- Body), clause(Head, Body), Clauses),print_metta_clauses(Clauses))),
    nl.

% 'print_metta_clauses' rule is handling the printing of metta clauses.
% It checks the form of the input clauses and calls 'print_metta_clause' accordingly.

%!  print_metta_clauses(+Clauses) is det.
%
%   Handles the printing of metta clauses. It checks the form of the input clauses and delegates
%   to `print_metta_clause/2` for each clause.
%
%   - If the input is an empty list, it does nothing.
%   - If the input is a single clause, it prints it directly.
%   - If the input is a list of clauses, it combines them into a single clause before printing.
%
%   @arg Clauses The list of clauses to be printed.
%
print_metta_clauses([]) :- !.
print_metta_clauses([Head :- Body]) :- !,
    print_metta_clause(Head, Body).
print_metta_clauses(Clauses) :-
    combine_clauses(Clauses, Head, Body), !,
    print_metta_clause(Head, Body).

%!  print_metta_clause(+Head, +Body) is det.
%
%   Prints a single metta clause consisting of a head and body, then processes it further using `show_cvts/1`.
%
%   @arg Head The head of the clause.
%   @arg Body The body of the clause.
%
print_metta_clause(Head, Body) :-
    print_metta_clause0(Head, Body),
    show_cvts(Head :- Body).

% 'print_metta_clause0' rule prints metta clauses based on the body.
% It transforms the body to a list, if needed, and prints it in a sequential form.

%!  print_metta_clause0(+Head, +Body) is det.
%
%   Prints metta clauses by transforming the body as needed and outputting it in a sequential form.
%
%   - If the body is `true`, it prints the clause as `Head = True`.
%   - If the body is `false`, it prints the clause as `Head = False`.
%   - If the body contains multiple conjuncts, it converts the body to a list, processes it into sequential form,
%     and prints the result.
%
%   @arg Head The head of the clause.
%   @arg Body The body of the clause, which can be a single condition or a conjunction.
%
%   @example Print a clause with a true body:
%     ?- print_metta_clause0(my_pred, true).
%
print_metta_clause0(Head,Body):- Body == true,!, pp_metta([=,Head,'True']).
print_metta_clause0(Head,Body):- Body == false,!, pp_metta([=,Head,'False']).
print_metta_clause0(Head,Body):- conjuncts_to_list(Body,List), into_sequential([':-'],List,SP),pp_metta([=,Head,SP]).

% =========================================
%  STERM -> PTERM
% =========================================

%!  iz_exact_symbol(+N, ?P) is nondet.
%
%   This predicate checks whether `N` is an exact symbol and optionally unifies it with `P`.
%   It handles specific symbols like ':-', '?-', and '??' directly, and fails if `N` is not an atom.
%
%   - If `N` is not an atom, the predicate fails.
%   - If `P` is provided and nonvar, it recursively checks if `P` matches the exact symbol corresponding to `N`.
%   - For certain predefined symbols (`':-'`, `'?-'`, and `'??'`), it returns their matching symbols.
%
%   @arg N The symbol to check or convert.
%   @arg P Optionally, the corresponding symbol to unify with `N`.
%
%   @example Check if ':-' is an exact symbol:
%     ?- iz_exact_symbol(':-', X).
%     X = ':-'.
%
%   @example Check if '??' has an exact match:
%     ?- iz_exact_symbol('??', X).
%     true.
%
iz_exact_symbol(N,_):- \+ atom(N),!,fail.
iz_exact_symbol(N,P):- nonvar(P),!,iz_exact_symbol(N,PP),zalwayz(P=PP).
iz_exact_symbol(':-',':-').
iz_exact_symbol('?-','?-').
iz_exact_symbol('??',_).

%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

%!  maybe_varz(+S, +Name, -Var) is nondet.
%
%   This predicate checks if `S` is the symbol `'?'` and `Name` is an atom. If both conditions are true, it unifies `Var` with the Prolog variable notation `'$VAR'(Name)`.
%
%   - If `S` is the symbol `'?'` and `Name` is an atom, it succeeds and binds `Var` to `'$VAR'(Name)`.
%   - Otherwise, it fails.
%
%   @arg S The symbol to check (expected to be `'?'`).
%   @arg Name The name of the variable, expected to be an atom.
%   @arg Var The resulting Prolog variable notation `'$VAR'(Name)`.
%
%   @example
%   ?- maybe_varz('?', x, Var).
%   Var = '$VAR'(x).
%
maybe_varz(S, Name, '$VAR'(Name)) :- S == '?',atom(Name), !.

%% sexpr_s2p(Fn,?VAR, ?V) is det.
%

%!  sexpr_s2p(+Sterm, -Pterm) is det.
%
%   Converts an S-expression (`Sterm`) into a Prolog term (`Pterm`).
%
%   - If `Sterm` is a compound term in the form `H=B`, it attempts to compile the term using `compile_for_assert/3`,
%     and then converts the compiled clause into a Prolog term.
%   - Otherwise, it delegates the conversion to `sexpr_s2p/4`, starting with the default function `progn` and position `1`.
%
%   @arg Sterm The S-expression input to be converted (can be a compound or other form).
%   @arg Pterm The resulting Prolog term.
%
%   @example Convert an S-expression:
%     ?- sexpr_s2p((pi(A) = B), P).
%     P = ... (Prolog term).
%
sexpr_s2p(HB, P) :- fail,compound(HB),HB =~ (H = B),compile_for_assert(H, B, Cl),clause_to_code(Cl, P), !.
sexpr_s2p(S, P) :- sexpr_s2p(progn, 1, S, P).

%!  clause_to_code(+Clause, -Code) is det.
%
%   Converts a Prolog clause into its code representation. This handles different cases:
%
%   - If `Clause` is a free term variable (`ftVar`), it is left unchanged.
%   - If the body (`B`) of the clause is `true`, it combines the head (`H`) and body into the code form.
%   - In all other cases, the clause is returned unchanged.
%
%   @arg Clause The input Prolog clause to be converted.
%   @arg Code The resulting code representation of the clause.
%
clause_to_code(P,P):- is_ftVar(P),!.
%clause_to_code(P:-True,P):- True == true,!.
clause_to_code((H:-B),P):- B==true, !, combine_code(B,H,P).
clause_to_code(P,P).

%
%%%%%%%%%%%%%%%%%%%% START sexpr_s2p clauses %%%%%%%%%%%
%

%!  sexpr_s2p(+Fn, +Nth, +Sterm, -Pterm) is det.
%
%   Converts an S-expression (`Sterm`) into a Prolog term (`Pterm`), handling various cases like free variables,
%   exact symbols, and atom-to-variable mappings.
%
%   @arg Fn   The function name associated with the S-expression (used in specific cases).
%   @arg Nth  The argument position being processed (used in specific cases).
%   @arg Sterm The S-expression input to be converted.
%   @arg Pterm The resulting Prolog term.
%
% If the S-expression is a free term variable (ftVar), it is returned unchanged.
sexpr_s2p(_Fn, _Nth, VAR, VAR) :- is_ftVar(VAR), !.
% If the S-expression is an exact symbol, it is converted using iz_exact_symbol/2.
sexpr_s2p(_Fn, _Nth, S, P) :- iz_exact_symbol(S, P), !.
% If the S-expression is of the form #(S), it is treated as a special exact symbol.
sexpr_s2p(_Fn, _Nth, '#'(S), P) :- iz_exact_symbol(S, P), !.
% If the S-expression is an atom, it is converted into a Prolog variable with '$VAR'(Name).
sexpr_s2p(_Fn, _Nth, VAR, '$VAR'(Name)) :- atom(VAR), svar(VAR, Name), !.
% If the S-expression is an empty list and the function allows lists, return the empty list.
sexpr_s2p(Fn, Nth, S, P) :- S == [],iz_fun_argz(Fn, Nth), !, P = S.

%sexpr_s2p(Fn,Nth,S,P):- expects_type(Fn,Nth,Type),will_become_type(Type,S,P),!.

% - If `Sterm` is a list, it checks if `F` is a system predicate with the appropriate arity.
% - It then recursively converts the arguments (`SList`) and constructs the Prolog term using `P =.. [Pred|PList]`.
sexpr_s2p(_Fn,_Nth,[F|SList],P):- is_list(SList), length(SList,Len),is_syspred(F,Len,Pred),
    sexpr_s2p_arglist(F,1,SList,PList), !, P=..[Pred|PList].

% Disable singleton variable warnings.
%
% The directive `:- style_check(-singleton)` is used to turn off warnings
% related to singleton variables in Prolog. Singleton variables are those
% that appear only once in a clause, which can sometimes indicate a typo or
% an unused variable.
%
% This is useful when singleton variables are intentional and no warning is needed.
%
:- style_check(-singleton).

% If the function and argument position allow, convert the first element of the list (`S`)
% and the remaining list (`SList`) recursively.
sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- iz_fun_argz(Fn,Nth),!,sexpr_s2p(S,P), sexpr_s2p(Fn,Nth,SList,PList).
% If the first element is not an atom or `SList` is not a list, recursively convert both `S` and `SList` with `list(Fn)` context.
sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- ( \+ atom(S) ; \+ is_list(SList)), !,sexpr_s2p(list(Fn),Nth,S,P),
    sexpr_s2p(list(Fn),Nth,SList,PList).
% If the first element `S` is a known quoter, convert the subsequent list (`STERM0`) and combine it into a Prolog term.
sexpr_s2p(_Fn,_Nth,[S,STERM0],PTERM):- iz_quoter(S),sexpr_s2p_pre_list(S,0,STERM0,STERM), !,PTERM=..[S,STERM],!.
% If `S` is an atom and `SList` is empty, construct a compound term with arity 0.
sexpr_s2p(_Fn,_Nth,[S|SList],P):- atom(S), SList == [], compound_name_arity(P,S,0).

% sexpr_s2p(Fn,Nth,List,PTERM):- append(Left,[S,Name|TERM],List),maybe_varz(S,Name,Var),!,append(Left,[Var|TERM],NewList), sexpr_s2p(Fn,Nth,NewList,PTERM).
% sexpr_s2p(Fn,Nth,[S|TERM],dot_holds(PTERM)):- \+ (is_list(TERM)),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- \+ atom(S),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.
/*
sexpr_s2p(Fn,Nth,[S,Vars|TERM],PTERM):- nonvar(S),
   call_if_defined(common_logic_snark:iz_quantifier(S)),
   zalwayz((sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),
   PTERM =~ [S,Vars|PLIST])),!.
*/
% sexpr_s2p(progn,_,[S|TERM],PTERM):- S==AND,!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- (number(S);  (atom(S),fail,atom_concat_or_rtrace(_,'Fn',S))),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.
%sexpr_s2p(Fn,Nth,[S],O):- is_ftVar(S),sexpr_s2p(Fn,Nth,S,Y),!,z_univ(Fn,Nth,O,[Y]),!.
%sexpr_s2p(Fn,Nth,[S],O):- nonvar(S),sexpr_s2p(Fn,Nth,S,Y),!,z_univ(Fn,Nth,O,[Y]),!.
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- S==and,!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
% sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- iz_va_relation(S),!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(S,PLIST,PTERM))).
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- iz_relation_sexpr(S),zalwayz((sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),PTERM =~ [S|PLIST])),!.
%sexpr_s2p(Fn,Nth,STERM,PTERM):- STERM =~ [S|TERM],sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),z_univ(Fn,Nth,PTERM,[S|PLIST]),!.

% Convert the list `[S|STERM0]` by processing the rest of the list with `sexpr_s2p_pre_list/4`
% and then converting the arguments (`STERM`) into a Prolog term list (`PLIST`).
sexpr_s2p(Fn,Nth,[S|STERM0],PTERM):-
  sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM),
  sexpr_s2p_arglist(S,1,STERM,PLIST), z_univ(Fn,Nth,PTERM,[S|PLIST]),!.
% If the input `VAR` is already in Prolog term format, return it unchanged.
sexpr_s2p(_Fn,_Nth,VAR,VAR).

%
%%%%%%%%%%%%%%%%%%%% END sexpr_s2p clauses %%%%%%%%%%%
%

%!  expects_type(+Fn, +Nth, -Type) is nondet.
%
%   Determines the expected type of the Nth argument for the function Fn.
%   Uses `get_operator_typedef/4` to get the parameters and return type, and
%   selects the type at position Nth.
%
%   @arg Fn   The function to check.
%   @arg Nth  The index of the argument (0-based).
%   @arg Type The expected type of the argument.
%
%   @example
%     ?- expects_type(my_function, 1, Type).
%     Type = param1_type.
%
expects_type(Fn, Nth, Type) :-
    get_operator_typedef(Self, Fn, Params, RetType),
    nth0(Nth, [RetType | Params], Type),
    nonvar(Type).

%!  will_become_type(+Type, +S, -P) is det.
%
%   Ensures S becomes of the specified Type, possibly coercing it.
%
%   @arg Type The target type.
%   @arg S    The input value.
%   @arg P    The result after type adjustment.
%
will_become_type(Type, S, P) :-
    % Try adjusting the argument types using try_adjust_arg_types/7.
    try_adjust_arg_types(=, _RetType, 88, _Self, [Type], [S], [PS]),PS = P, !.
will_become_type(Type, S, P) :-
    % If S is a free type variable, unify P with S directly.
    is_ftVar(S), !, P = S.
will_become_type(Type, S, P) :-
    % If S has a type, check if it is a subtype of the target Type.
    % If it is, unify P with S, otherwise, coerce S to the target Type.
    get_type(S, T), !,(is_subtype(T, Type) -> S = P ; P = coerce(Type, S)).
will_become_type(_Type, S, P) :-
    % If no other conditions apply, simply unify S with P.
    !, S = P.

%!  is_subtype(+T, +TT) is nondet.
%
%   Checks if T is a subtype of TT. Unifies T and TT if they are considered equivalent.
%
%   @arg T  The type to check.
%   @arg TT The target type to compare against.
%
is_subtype(T, TT) :-
    % If T and TT are structurally identical, unify them.
    T =@= TT, !, T = TT.
is_subtype(T, TT) :-
    % If T is already equal to TT, succeed.
    T = TT, !.

%!  iz_quoter(+Quoter) is nondet.
%
%   Checks if Quoter is a valid quoting operator.
%   This predicate defines different quoting operators based on certain conditions.
%
%   @arg Quoter The quoter to check.
%
iz_quoter('#BQ') :-
    % '#BQ' is a quoter if the system is in Common Lisp mode.
    iz_common_lisp.
iz_quoter('#COMMA') :-
    % '#COMMA' is a quoter if the system is in Common Lisp mode.
    iz_common_lisp.
iz_quoter('quote').
    % 'quote' is always considered a quoter.
iz_quoter(superpose).
    % 'superpose' is always considered a quoter.

%!  iz_fun_argz(+Fun, +ArgCount) is nondet.
%
%   Determines the number of arguments (ArgCount) that a given function or construct (Fun) expects.
%
%   @arg Fun      The function or construct whose argument count is being checked.
%   @arg ArgCount The number of expected arguments for Fun.
%
iz_fun_argz(list(_), _).
    % The 'list' construct can take any number of arguments.
iz_fun_argz(defmacro, 2).
    % 'defmacro' expects 2 arguments.
iz_fun_argz(defun, 2).
    % 'defun' expects 2 arguments.
iz_fun_argz(let, 1).
    % 'let' expects 1 argument.
iz_fun_argz('let*', 1).
    % 'let*' expects 1 argument.
iz_fun_argz('member', 2).
    % 'member' expects 2 arguments.
% iz_fun_argz('let*', 2).
iz_fun_argz(F, 1) :-
    % Any function that is a quoter (checked via iz_quoter/1) expects 1 argument.
    iz_quoter(F).

%!  z_functor(+F) is nondet.
%
%   Checks if F is a valid functor by ensuring it is an atom and does not start
%   with specific characters ('?' or '$').
%
%   @arg F The functor to check.
%
z_functor(F) :-
    % Fail if F is not an atom.
    \+ atom(F), !, fail.
z_functor(F) :-
    % Succeed if F does not start with '?'.
    \+ atom_concat('?', _, F).
z_functor(F) :-
    % Succeed if F does not start with '$'.
    \+ atom_concat('$', _, F).

%!  z_univ(+Fn, +Nth, ?P, ?S) is det.
%
%   Unifies the term `P` with a compound term based on `F` and `ARGS`, or directly with `S`.
%   Handles different cases depending on `Nth`.
%
%   @arg Fn   Functor (not directly used).
%   @arg Nth  Arity or position indicator.
%   @arg P    Term to unify with a compound term or `S`.
%   @arg S    List or term to unify with `P`.
%
%   @example
%   ?- z_univ(_, _, P, [functor, [arg1, arg2]]).
%   P = functor(arg1, arg2).
%
% z_univ(_Fn, 1, S, S) :- !.
%
z_univ(_Fn, _, P, [F|ARGS]) :-
    % Unify P with a compound term of functor F and its arguments.
    z_functor(F), is_list(ARGS), length(ARGS, A), l_arity_l(F, A),compound_name_arguments(P, F, ARGS), !.
z_univ(_Fn, 0, P, [F|ARGS]) :-
    % Similar to previous, but Nth is 0.
    z_functor(F), is_list(ARGS), compound_name_arguments(P, F, ARGS), !.
z_univ(_Fn, _Nth, P, [F|ARGS]) :-
  % General case for any Nth, unifies P with a compound term.
  z_functor(F), is_list(ARGS), compound_name_arguments(P, F, ARGS), !.
z_univ(_Fn, _Nth, P, S) :-
  % Fallback: unify P with S directly.
  P = S.

%!  l_arity_l(+F, +A) is det.
%
%   Checks or assigns the arity `A` of the functor `F`. The predicate determines
%   the arity of specific functors or uses rules to infer the arity if possible.
%   Default arity is 1 if no specific case is matched.
%
%   @arg F    The functor whose arity is being checked or assigned.
%   @arg A    The arity (number of arguments) of the functor.
%
%   @example
%   ?- l_arity_l(function, A).
%   A = 1.
l_arity_l(F, A) :-
    % Check if the arity of functor F matches A by calling arity/2 predicate.
    clause_b(arity(F, A)).
l_arity_l(function, 1).
    % Functor 'function' always has arity 1.
l_arity_l(quote, 1).
    % Functor 'quote' always has arity 1.
l_arity_l('#BQ', 1) :-
    % Functor '#BQ' has arity 1 in Common Lisp context.
    iz_common_lisp.
l_arity_l(F, A) :-
    % If F/A is a current predicate, arity is accepted as valid.
    current_predicate(F/A).
l_arity_l(_, 1).
    % Default case: assume arity of 1.

%!  sexpr_s2p_arglist(+Fn, +Nth, ?SExpr, ?PExpr) is det.
%
%   Converts a list of S-expressions (`SExpr`) into Prolog expressions (`PExpr`).
%   Handles different cases depending on the structure of `SExpr` and whether it is
%   a variable or a list. The conversion uses `sexpr_s2p/4` to handle individual elements.
%
%   @arg Fn   Functor used for conversion (not directly used in all clauses).
%   @arg Nth  Position or index during processing, adjusted as needed.
%   @arg SExpr The input S-expression (can be a variable or a list).
%   @arg PExpr The output Prolog expression corresponding to `SExpr`.
%
%   @example
%   ?- sexpr_s2p_arglist(_, 1, [a, b, c], PList).
%   PList = [a, b, c].
sexpr_s2p_arglist(_Fn, _, VAR, VAR) :-
    % Base case: if `VAR` is a free term variable, leave it unchanged.
    is_ftVar(VAR), !.
sexpr_s2p_arglist(Fn, Nth, [S|SList], [P|PList]) :-
    % Recursively process list: convert head `S` to `P`, then process the tail.
    sexpr_s2p(Fn, Nth, S, P),
    (Nth > 0 -> Nth2 is Nth + 1 ; Nth2 = 0),  % Increment Nth if positive, else set to 0.
    sexpr_s2p_arglist(Fn, Nth2, SList, PList), !.
sexpr_s2p_arglist(Fn, Nth, S, P) :-
    % Convert a single S-expression `S` into a Prolog expression `P`.
    sexpr_s2p(Fn, Nth, S, P), !.
sexpr_s2p_arglist(_Fn, _Nth, VAR, VAR).
    % Fallback: if `VAR` is neither a list nor an S-expression, leave it unchanged.

%!  sexpr_s2p_pre_list(+Fn, +Nth, ?STerm, ?PTerm) is det.
%
%   Pre-processes a list of S-expressions (`STerm`) to produce corresponding Prolog terms (`PTerm`).
%   Handles different cases, including non-compound terms, non-list structures, and recursively
%   processing nested lists. Uses `sexpr_s2p/4` for the actual conversion.
%
%   @arg Fn    Functor used during processing.
%   @arg Nth   Position or index used during processing (not always used).
%   @arg STerm Input S-expression (could be a term or a list).
%   @arg PTerm Resulting Prolog term after conversion.
%
%   @example
%   ?- sexpr_s2p_pre_list(_, _, [a, [b, c], d], PTerm).
%   PTerm = [a, [b, c], d].
sexpr_s2p_pre_list(_Fn, _, STERM, STERM) :-
    % If `STERM` is not compound, return it unchanged.
    \+ compound(STERM), !.
sexpr_s2p_pre_list(_Fn, _, STERM, STERM) :-
    % If `STERM` is not a list, return it unchanged.
    \+ is_list(STERM), !.
% sexpr_s2p_pre_list(Fn, _, [S|STERM], [S|STERM]) :- STERM == [], !.
% (Commented-out clause for empty lists, may have been used for list termination.)
sexpr_s2p_pre_list(Fn, Nth, [S0|STERM0], [S|STERM]) :-
    % Recursively process the list: if `S0` is a list, convert it using `sexpr_s2p/4`; otherwise, recurse.
    (is_list(S0) -> sexpr_s2p(Fn, Nth, S0, S) ; sexpr_s2p_pre_list(Fn, Nth, S0, S)),
    % Process the tail of the list recursively.
    sexpr_s2p_pre_list(Fn, Nth, STERM0, STERM), !.
sexpr_s2p_pre_list(_Fn, _, STERM, STERM).  % Fallback: return `STERM` unchanged.

%
%%%%%%%%%%%%%%%%%%%% START P2m clauses %%%%%%%%%%%
%

% p2m/2 is a translation utility to convert Prolog constructs to MeTTa constructs.
% It handles a variety of cases, including different types of compound terms,
% control structures, and predicate definitions.
% The first argument is the input in Prolog syntax,
% and the second argument is the output converted to MeTTa syntax.

%!  p2m(+I) is det.
%
%   A utility predicate to translate Prolog predicates (`I`) into MeTTa constructs.
%   It retrieves all clauses for the given predicate, processes them, and writes
%   the translated result in MeTTa syntax.
%
%
%   @arg I The input predicate name in Prolog syntax to be converted.
%
%   @example
%   ?- p2m(my_predicate).
%   This would translate the clauses of `my_predicate/Arity` into MeTTa syntax.
p2m(I):-forall(
  no_repeats(current_predicate(I/A)),
   (functor(P,I,A),
   forall(clause(P,Body),
     (numbervars(P+Body,0,_,[]),
     write_src(=(P,'call!'(Body))))))).

%!  p2m(+I, -O) is det.
%
%   Translates Prolog predicates (`I`) into MeTTa constructs, outputting the result in `O`.
%   It uses an initial context, `[progn]`, for the translation process.
%
%   @arg I The input predicate name in Prolog syntax.
%   @arg O The output converted into MeTTa syntax.
%
%   @example
%   ?- p2m(my_predicate, Output).
%   Output will contain the MeTTa translation of `my_predicate`.
p2m(I, O) :-
    % Initiate translation of `I` to `O` with the starting context `[progn]`.
    p2m([progn], I, O).

%!  p2m(+OC, +NC, -O) is det.
%
%   Translates Prolog constructs (`NC`) into MeTTa constructs (`O`), using an optional context (`OC`).
%   Handles a variety of Prolog terms, lists, and control structures.
%
%   @arg OC The context for translation, often involving specific translation rules.
%   @arg NC The input term in Prolog syntax.
%   @arg O  The output term converted into MeTTa syntax.
%
%   @example
%   ?- p2m(_, my_predicate, Output).
%   Output will contain the MeTTa translation of `my_predicate`.
p2m(_OC, NC, NC) :-
    % If NC is a variable, do not translate.
    var(NC), !.
p2m(_OC, NC, NC) :-
    % If NC is a free term variable, do not translate.
    is_ftVar(NC), !.
p2m(OC, [H|T], '::'(L)) :-
    % If NC is a list, map each element of the list from Prolog to MeTTa.
    is_list([H|T]), maplist(p2m(OC), [H|T], L).
p2m(OC, [H|T], 'Cons'(OH, OT)) :-
    % Translate a list to MeTTa 'Cons' structure.
    p2m(OC, H, OH), p2m(OC, T, OT).
p2m(_OC, A, A) :-
    % Conversion for any atomic term.
    string(A), !.
p2m(_OC, [], 'Nil').         % Empty list is translated to 'Nil'.
p2m(_OC, '[|]', 'Cons').     % Translate '[|]' operator to 'Cons'.
p2m(_OC, !, ['set-det']).    % Translate the cut operation.
p2m(_OC,!, '!').             % Translate the cut operation directly.
p2m(_OC, false, 'False').    % Translate Prolog's false to MeTTa's False.
p2m(_OC, true, 'True').      % Translate Prolog's true to MeTTa's True.
p2m([progn|_], Atom, [O]) :-
    % Translate atoms using hyphenated format.
    atom(Atom), !, p2m([arg], Atom, O), !.
p2m(_OC, (';'), 'xor').      % Translate ';' (or) to 'xor'.
p2m(_OC, (','), 'and2').     % Translate ',' (and) to 'and2'.
p2m(_OC, '=..', 'atom_2_list').   % Translate '=..' (univ operator) to 'atom_2_list'.
p2m([progn|_], (fail), [empty]).  % Translate Prolog's fail to MeTTa's empty (False).
p2m(_OC, 'atom', 'is-symbol').    % Translate 'atom' predicate to 'is-symbol'.
p2m(_OC, 'atomic', 'symbolic').   % Translate 'atomic' to 'symbolic'.
p2m(OC, ASymbolProc, O) :-
    % Translate atoms with '$' or '%' concatenation into hyphenated format.
    atom(ASymbolProc),symbolic_list_concat(LS,'$',ASymbolProc),LS\==[],LS\=[_],!,
    symbolic_list_concat(LS,'%',SymbolProc),into_hyphens(SymbolProc,O).
p2m(OC,ASymbolProc,O):- atom(ASymbolProc),into_hyphens(ASymbolProc,O).
p2m(_, A, H) :-
    % Translate any atomic symbol into hyphenated format.
    atom(A), into_hyphens(A, H), !.
p2m(_OC,A, A):- atomic(A).
p2m(_OC, NC, NC) :-
    % If NC is not a compound term, return it unchanged.
    \+ compound(NC), !.
p2m(_OC, NC, [F]) :-
    % If NC is a functor with arity 0, convert it to a list.
    compound_name_arity(NC, F, 0), !.
p2m(OC, M:I, O) :-
    % Skip module qualifier if it is 'user'.
    M == user, !, p2m(OC, I, O),!.
p2m(_OC, M:I, 'scoped'(N, O)) :-
    % Translate a module-scoped term.
    p2m(OC, M, N), p2m(I, O).
p2m(OC, NC, OO) :-
    % If NC is a list, map each element of the list from Prolog to MeTTa
    is_list(NC),!,
    maplist(p2m(OC), NC, OO).
    p2m([progn|_], (!,fail), [empty]).  % Translate Prolog?s fail to MeTTa?s False.
% p2m(_OC,fail, 'False').  % Translate Prolog?s fail to MeTTa?s False.
% p2m(_OC,prolog, meTTa).  % Translate the atom prolog to meTTa.
p2m([progn|_],A, [H]):-
    atom(A),into_hyphens(A,H),!.
p2m(_OC, (\+ A), O) :-
    % Translate negation as failure (\+ A).
    !, p2m(_OC, naf(A), O).
p2m(OC, (G, E), O) :-
    % Translate conjunctions (G, E).
    conjuncts_to_list((G, E), List), !, into_sequential(OC, List, O), !.
p2m(_OC,(Head:-Body),O):-
    Body == true,!, O = (=(Head,'True')).
p2m(_OC,(Head:-Body),O):-
    Body == fail,!, O = (=(Head,[empty])).
p2m(OC,(Head:-Body),O):-
    p2m(Head,H),conjuncts_to_list(Body,List),maplist(p2m([progn|OC]),List,SP),!,
    O =  ['=',H|SP].
p2m(OC, (:- Body), O) :-
    % Translate directives (:- Body).
    !, conjuncts_to_list(Body, List), into_sequential([progn|OC], List, SP), !, O = exec(SP).
p2m(OC, (? - Body), O) :-
    % Translate queries (? - Body).
    !, conjuncts_to_list(Body, List), into_sequential([progn|OC], List, SP), !, O = exec('?-'(SP)).
p2m(OC, (A->B;C), O) :-
    % Translate if-then-else (A -> B ; C).
    !, p2m(OC, det_if_then_else(A, B, C), O).
p2m(OC, (A;B), O) :-
    % Translate disjunction (A ; B).
    !, p2m(OC, or(A, B), O).
p2m(OC,(A*->B;C),O):-
    !, p2m(OC,if(A,B,C),O).
p2m(OC,(A->B),O):-
    !, p2m(OC,det_if_then(A,B),O).
p2m(OC,(A*->B),O):-
    !, p2m(OC,if(A,B),O).
% Translate common Prolog database operations and metta constructs to MeTTa equivalents.
% This includes adding atoms, retrieving types and atoms, and handling assertions and retractions.
p2m(_OC,metta_defn(Eq,Self,H,B),'add-atom'(Self,[Eq,H,B])).
p2m(_OC,metta_type,'get-type').
p2m(_OC,metta_atom,'get-atoms').
%p2m(_OC,get_metta_atom,'get-atoms').
p2m(_OC,clause(H,B), ==([=,H,B],'get-atoms'('&self'))).
p2m(_OC,assert(X),'add-atom'('&self',X)).
p2m(_OC,assertz(X),'add-atom'('&self',X)).
p2m(_OC,asserta(X),'add-atom'('&self',X)).
p2m(_OC,retract(X),'remove-atom'('&self',X)).
p2m(_OC,retractall(X),'remove-all-atoms'('&self',X)).
% The catch-all case for the other compound terms.
%p2m(_OC,I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=..OO.

% Catch-all clause for compound terms. This rule breaks down a compound term `I` into its functor `F`
% and arguments `II`, recursively applies `p2m` on each argument, and then reconstructs the term
% in MeTTa format, converting the functor `F` into a hyphenated form.
p2m(OC,I, O):-
    compound(I),
    I =.. [F|II], % univ operator to convert between a term and a list consisting of functor name and arguments
    maplist(p2m([F|OC]), II, OO), % applying p2m recursively on each argument of the compound term
    into_hyphens(F,FF),
    O = [FF|OO]. % constructing the output term with the converted arguments

%
%%%%%%%%%%%%%%%%%%%% END P2m clauses %%%%%%%%%%%
%

% In the context of this conversion predicate, each branch of the p2m predicate
% is handling a different type or structure of term, translating it into its
% equivalent representation in another logic programming language named MeTTa.
% The actual transformations are dependent on the correspondence between Prolog
% constructs and MeTTa constructs, as defined by the specific implementations
% of Prolog and MeTTa being used.

%!  prolog_to_metta(+PrologTerm, -MeTTaTerm) is det.
%
%   Translates a given Prolog term into its corresponding representation in MeTTa.
%   This conversion is dependent on the structure of the Prolog term and its
%   mapping to MeTTa constructs. The transformation rules are governed by the
%   relationship between Prolog and MeTTa, a different logic programming language.
%
%   @arg PrologTerm The Prolog term that is to be converted.
%   @arg MeTTaTerm  The resulting term in MeTTa after the translation.
%
%   @example Translating a Prolog term to MeTTa:
%       ?- prolog_to_metta(some_prolog_term, MeTTaRepresentation).
%       MeTTaRepresentation = corresponding_metta_term.
%
prolog_to_metta(V, D) :-
    % Calls the internal p2m predicate to perform the actual translation.
    % The [progn] list element may signify the starting construct or context
    % for the MeTTa language.
    p2m([progn], V, D), !.

%!  into_sequential(+Context, +Body, -MeTTaRepresentation) is det.
%
%   Transforms a Prolog term that represents a conjunction of terms into a sequential
%   representation in MeTTa syntax. The predicate handles cases where the body is a
%   conjunction, converting it into a MeTTa-friendly list, or if it is a single term,
%   directly translating it using `prolog_to_metta/2`.
%
%   @arg Context  The context for the MeTTa transformation (e.g., [progn] to signify sequential execution).
%   @arg Body     The Prolog term to be transformed. This can be a conjunction, a list of terms, or a single term.
%   @arg MeTTaRepresentation The resulting representation in MeTTa.
%
%   @example Converting a conjunction of terms into a sequential MeTTa list:
%       ?- into_sequential([progn], (term1, term2), MeTTaList).
%       MeTTaList = ['progn', MeTTaTerm1, MeTTaTerm2].
%
into_sequential(OC, Body, SP) :-
    % Check if Body is not already a list, then convert conjunctions into a list of conjuncts.
    \+ is_list(Body),
    conjuncts_to_list(Body, List),
    % Ensure the resulting List is a valid list and recursively process it into sequential MeTTa representation.
    is_list(List),
    into_sequential(OC, List, SP), !.
into_sequential([progn|_], Nothing, 'True') :-
    % Handle the case where Body is empty (Nothing) by returning 'True' in MeTTa.
    Nothing == [], !.
% Handle the case where Body is empty (Nothing) by returning 'Nil' in MeTTa.
into_sequential(_OC, Nothing, 'Nil') :-
    Nothing == [], !.
into_sequential(_, [SP], O) :-
    % If there is only one element, directly translate it using `prolog_to_metta/2`.
    prolog_to_metta(SP, O).
into_sequential([progn|_], List, SPList) :-
    % Otherwise, translate the list into sequential MeTTa representation using `progn`.
    maplist(prolog_to_metta, List, SPList), !.
into_sequential(_CA, List, [AND|SPList]) :-
    % If AND conjunction is used, translate the list with 'AND' in MeTTa.
    is_compiled_and(AND),
    maplist(prolog_to_metta, List, SPList), !.

%!  list_direct_subdirectories(+Directory, -DirectSubdirectories) is det.
%
%   Retrieves the direct subdirectories of a given directory.
%   This predicate returns a list of all subdirectories within the specified directory,
%   excluding the special entries '.' and '..'.
%
%   @arg Directory The directory path whose direct subdirectories are to be listed.
%   @arg DirectSubdirectories A list of paths corresponding to the direct subdirectories
%        of the given Directory.
%
%   @example Listing direct subdirectories of '/home/user':
%       ?- list_direct_subdirectories('/home/user', Subdirs).
%       Subdirs = ['/home/user/dir1', '/home/user/dir2'].
%
list_direct_subdirectories(Directory, DirectSubdirectories) :-
    % Retrieve all files and directories in the given Directory.
    directory_files(Directory, Entries),
    % Find all subdirectories, excluding '.' and '..'.
    findall(Path,
            (member(Entry, Entries),
             \+ member(Entry, ['.', '..']),    % Exclude special entries '.' and '..'
             symbolic_list_concat([Directory, '/', Entry], Path),
             is_directory(Path)),
            DirectSubdirectories).

%!  list_all_subdirectories(+Directory, -AllSubdirectories) is det.
%
%   Recursively lists all subdirectories of a given directory. This predicate
%   traverses the directory structure and accumulates both direct and nested
%   subdirectories, returning them as a flat list.
%
%   @arg Directory The directory whose subdirectories (including nested ones) are to be listed.
%   @arg AllSubdirectories A list of paths corresponding to all subdirectories of the given Directory, recursively.
%
%   @example Listing all subdirectories recursively from '/home/user':
%       ?- list_all_subdirectories('/home/user', Subdirs).
%       Subdirs = ['/home/user/dir1', '/home/user/dir2', '/home/user/dir1/subdir1', ...].
%
list_all_subdirectories(Directory, AllSubdirectories) :-
    % Retrieve the direct subdirectories of the given Directory.
    list_direct_subdirectories(Directory, DirectSubdirectories),
    % Recursively find subdirectories within each direct subdirectory.
    findall(Sub,
            (member(SubDir, DirectSubdirectories),
             list_all_subdirectories(SubDir, Subs),
             member(Sub, Subs)),
            NestedSubdirectories),
    % Combine direct subdirectories with recursively found nested subdirectories.
    append(DirectSubdirectories, NestedSubdirectories, AllSubdirectories).

%!  with_file_lists(+Rel, +P1, +FileSpec) is det.
%
%   Processes a list of filenames or directories, applying the predicate P1 to each.
%   It supports various file types, directories, and wildcard patterns, handling
%   both recursive directory traversals and file lists.
%
%   @arg Rel The base directory relative to which paths are resolved.
%   @arg P1  The predicate to apply to each file or directory.
%   @arg FileSpec The file specification (a filename, directory, or wildcard).
%
%   @example Apply a predicate to all Prolog files in a directory:
%       ?- with_file_lists('.', process_file, '*.pl').
%
with_file_lists(Rel, P1, FileSpec) :-
    % If FileSpec is '.pl', stop processing.
    FileSpec == '.pl', !.
with_file_lists(Rel, P1, FileSpec) :-
    % If FileSpec is a list, recursively process each item.
    is_list(FileSpec), !, ignore(maplist(with_file_lists(Rel, P1), FileSpec)).
with_file_lists(Rel, P1, Filename) :-
    % If Filename is an existing file, apply P1 to it.
    atomic(Filename), exists_file(Filename), !, ignore(call(P1, Filename)).
with_file_lists(Rel, P1, Filename) :-
    % Process Filename relative to an absolute directory.
    absolute_file_name(Rel, Dir, [access(read), file_errors(fail), file_type(directory)]),
    Rel \=@= Dir, !, with_file_lists(Dir, P1, Filename).
with_file_lists(Rel, P1, Filename) :-
    % If Rel is not a valid directory, use the current directory.
    \+ exists_directory(Rel), !, with_file_lists('.', P1, Filename).
with_file_lists(Rel, P1, File) :-
    % Resolve a compound File with file extensions and apply P1.
    compound(File),
    absolute_file_name(File, Dir, [access(read), relative_to(Rel), file_errors(fail), extensions(['pl', 'prolog', 'pfc'])]),
    '\\=@='(Dir, File), !, with_file_lists(Rel, P1, Dir).
with_file_lists(Rel, P1, File) :-
    % Handle compound File for directory resolution.
    compound(File),
    absolute_file_name(File, Dir, [access(read), file_errors(fail), relative_to(Rel), file_type(directory)]),
    '\\=@='(Dir, File), !, with_file_lists(Rel, P1, Dir).
/*
with_file_lists(Rel,P1, File) :-
      compound(File),
      absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(directory)]),
      '\\=@='(Dir, File), !,
      with_file_lists(Rel,P1, Dir).
with_file_lists(Rel,P1, File) :-
      compound(File), !,
      absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(['csv', 'tsv', ''])]),
      '\\=@='(Dir, File), !,
      with_file_lists(Rel,P1, Dir).
with_file_lists(Rel,P1, File) :-
      symbol_contains(File, '*'),
      expand_file_name(File, List),List\==[],  !,
      maplist(with_wild_path(Fnicate), List).
with_file_lists(Rel,P1, File) :-
      exists_directory(File),
      directory_file_path(File, '*.*sv', Wildcard),
      expand_file_name(Wildcard, List), !,
      maplist(Fnicate, List).
*/
with_file_lists(Rel, P1, Wildcard) :-
    % Expand wildcard patterns and process files accordingly.
    atom(Wildcard),
    \+ exists_file(Wildcard),
    once(atom_contains(Wildcard, '*'); atom_contains(Wildcard, '?'); atom_contains(Wildcard, '|')),
    expand_file_name(Wildcard, Files), Files \== [], !, ignore(maplist(with_file_lists(Rel, P1), Files)).
with_file_lists(Rel, P1, Wildcard) :-
    % Handle wildcard with absolute file resolution and expansion.
    atom(Wildcard),once(atom_contains(Wildcard, '*'); atom_contains(Wildcard, '?'); atom_contains(Wildcard, '|')),
    \+ exists_file(Wildcard),
    absolute_file_name(Wildcard, AbsWildcard, [relative_to(Rel)]),
    \+ exists_file(AbsWildcard),
    expand_file_name(AbsWildcard, Files), Files \== [], !, ignore(maplist(with_file_lists(Rel, P1), Files)).
/*
with_file_lists(Rel,P1,Local):- (Local=='.';Local=='';Local=='*.pl'),Directory = Rel,
    absolute_file_name(Directory,AbsDirectory,[relative_to(Rel),file_type(directory)]),
    exists_directory(AbsDirectory),
    findall(File,directory_source_files(AbsDirectory, File, [recursive(false),if(true)]),Files),
    ignore(maplist(with_file_lists(Rel,P1),Files)),!.
*/
with_file_lists(Rel,P1,Local):- (Local=='**';Local=='**.pl'),
    must_det_ll((absolute_file_name(Directory,AbsDirectory,[file_type(directory)]),
    exists_directory(AbsDirectory))),
    findall(File,directory_source_files(AbsDirectory, File, [recursive(true),if(true)]),Files),!,
    ignore(maplist(with_file_lists(Rel,P1),Files)).
with_file_lists(Rel, P1, Filename) :-
    % Handle '**' pattern with subdirectory traversal.
    symbolic_list_concat(['**', S | More], '/', Filename),symbolic_list_concat([S | More], '/', Rest),
    list_all_subdirectories(Rel, AllSubdirectories), !,forall(member(SubDir, AllSubdirectories),
    with_file_lists(SubDir, P1, Rest)).
with_file_lists(Rel, P1, Filename) :-
    % Handle wildcards with directory expansion and traversal.
    symbolic_list_concat([WildDir, S | More], '/', Filename),symbolic_list_concat([Rel, WildDir, ''], '/', WildMaskDir),
    expand_file_name(WildMaskDir, AllSubdirectories),symbolic_list_concat([S | More], '/', Rest), !,
    forall(member(SubDir, AllSubdirectories), with_file_lists(SubDir, P1, Rest)).
with_file_lists(Rel, P1, FileSpec) :-
    % Process atomic FileSpec as a file.
    atomic(FileSpec),absolute_file_name(FileSpec, AbsFile, [relative_to(Rel), access(read), file_errors(fail)]),
    exists_file(AbsFile), !, ignore(call(P1, AbsFile)).
with_file_lists(Rel, P1, Directory) :-
    % Process atomic Directory and recursively apply P1 to files within it.
    atomic(Directory),
    absolute_file_name(Directory, AbsDirectory, [relative_to(Rel), access(read), file_errors(fail), file_type(directory)]),
    exists_directory(AbsDirectory), !,
    findall(File, directory_source_files(AbsDirectory, File, [recursive(true), if(true)]), Files), !,
    ignore(maplist(with_file_lists(Rel, P1), Files)).
with_file_lists(Rel, P1, Wildcard) :-
    % Process wildcard patterns as files.
    atom(Wildcard),
    absolute_file_name(Wildcard, AbsWildcard, [relative_to(Rel)]),
    \+ exists_file(AbsWildcard),
    expand_file_name(AbsWildcard, Files), Files \== [], !,
    ignore(maplist(with_file_lists(Rel, P1), Files)).
with_file_lists(Rel, P1, Filename) :-
    % Log processing of Filename.
    write_src(with_file_lists(Rel, P1, Filename)), nl.


    % Entry point for printing to Metta format. It clears the screen, sets the working directory,
    % expands the filenames with a specific extension, and processes each file.
     % cls, % Clears the screen (assumes a custom or system-specific implementation).
     % with_pwd(
      %   '/opt/logicmoo_opencog/hyperon-wam/tests/gpt2-like/language_models/',
     %Filt = 'tests/gpt2-like/language_models/*.pl',
    % Filt = '/opt/logicmoo_opencog/hyperon-wam/tests/performance/nondet_unify/*.pl',
       % Finds all Prolog files in the specified directory.
     %  convert_to_metta(Filt),  % Processes each found file.
      % MC = '/opt/logicmoo_opencog/hyperon-wam/src/main/metta_convert.pl',
      % convert_to_metta(MC), % Processes each found file.
    % Example of a no-operation (nop) call for a specific file path, indicating a placeholder or unused example.
    %$nop(convert_to_metta('/opt/logicmoo_opencog/hyperon-wam/src/main/metta_convert.pl')).

%!  default_pl_mask(-Mask) is det.
%
%   Provides the default pattern mask used to match Prolog files for processing.
%   The first clause defines a specific set of file path patterns to be used, and the
%   second clause falls back to a simpler recursive wildcard pattern if the first clause
%   does not apply.
%
%   @arg Mask A list of file patterns (wildcards) used to match Prolog files.
%
%   @example Get the default Prolog file mask:
%       ?- default_pl_mask(Mask).
%       Mask = ['*/*.pl', '*/*/*.pl', ..., '*.pl'].
%
default_pl_mask(Mask) :-
    % Provide a specific set of file patterns for Prolog files.
    Mask = [
        % 'src/main/metta_*.pl',
        % 'src/main/flybase_*.pl',
        '*/*.pl',
        '*/*/*.pl',
        '*/*/*/.pl',
        '*/*/*/*/.pl',
        '*/*/*/*/*/.pl',
        '*/*/*/*/*/*.pl',
        '*.pl'
    ], !.
default_pl_mask(Mask) :-
    % Fallback to a recursive wildcard pattern for matching Prolog files.
    Mask = ['**/*.pl'].

%!  convert_to_metta_console is det.
%
%   Initiates the conversion process for Prolog files to MeTTa representation
%   using a default file mask. This predicate retrieves the default file mask
%   using `default_pl_mask/1` and applies the conversion process. The process
%   runs quietly, ignoring errors, and prints a message upon completion.
%
%   @example Convert files to MeTTa using the default mask:
%       ?- convert_to_metta_console.
%       ;; convert_to_metta_console.
%
convert_to_metta_console :-
    % Retrieve the default Prolog file mask.
    default_pl_mask(Mask),
    % Ignore errors during the conversion process and apply the conversion.
    ignore(convert_to_metta_console(Mask)),
    % Print a completion message.
    !, writeln(';; convert_to_metta_console.').

%!  convert_to_metta_file is det.
%
%   Initiates the conversion process for Prolog files to MeTTa representation
%   using a default file mask. This predicate retrieves the default file mask
%   using `default_pl_mask/1` and applies the conversion process. It ensures
%   that the conversion proceeds without halting on errors using `ignore/1`.
%   A message is printed upon successful completion of the process.
%
%   @example Convert files to MeTTa using the default mask:
%       ?- convert_to_metta_file.
%       ;; convert_to_metta_file.
%
convert_to_metta_file :-
    % Retrieve the default Prolog file mask.
    default_pl_mask(Mask),
    % Ignore any errors during the file conversion process.
    ignore(convert_to_metta_file(Mask)),
    % Print a completion message.
    !, writeln(';; convert_to_metta_file.').

%!  convert_to_metta is det.
%
%   Converts Prolog files to MeTTa representation using the default file mask.
%   This predicate retrieves the default file mask using `default_pl_mask/1` and
%   applies the conversion process, ensuring that any errors during the conversion
%   are ignored. After the conversion is completed, a message is printed to indicate
%   successful completion.
%
%   @example Convert Prolog files to MeTTa using the default mask:
%       ?- convert_to_metta.
%       ;; convert_to_metta.
%
convert_to_metta :-
    % Retrieve the default Prolog file mask.
    default_pl_mask(Mask),
    % Apply the conversion process, ignoring errors.
    % The call to 'ignore' ensures the conversion process will not fail due to individual errors.
    call(ignore(convert_to_metta(Mask))),
    % Print a completion message.
    !, writeln(';; convert_to_metta.').

%!  ctm is det.
%
%   A shorthand predicate to invoke `convert_to_metta/0`, which converts Prolog files
%   to MeTTa representation using the default file mask. This predicate is simply a
%   shortcut to run the conversion process and prints a completion message.
%
%   @example Shorthand for converting Prolog files to MeTTa:
%       ?- ctm.
%       ;; convert_to_metta.
%
ctm :-
    % Invoke the convert_to_metta/0 predicate.
    convert_to_metta.

%!  convert_to_metta_console(+FileSpec) is det.
%
%   Processes a list of filenames specified by `FileSpec`, applying the
%   `convert_to_metta_now/2` predicate to each file and directing the output
%   to the console (`user_output`).
%
%   @arg FileSpec The file specification, which can be a filename, directory,
%        or wildcard pattern to match multiple files.
%
%   @example Convert files specified by a file pattern to MeTTa and output to console:
%       ?- convert_to_metta_console('*.pl').
%
convert_to_metta_console(FileSpec) :-
    % Process files in the current directory and convert them, outputting results to the console.
    with_file_lists('.', convert_to_metta_now(user_output), FileSpec).

%!  convert_to_metta_file(+FileSpec) is det.
%
%   Processes a list of filenames specified by `FileSpec`, applying the
%   `convert_to_metta_now/2` predicate to each file. This version handles file
%   conversion without outputting results to the console.
%
%   @arg FileSpec The file specification, which can be a filename, directory,
%        or wildcard pattern to match multiple files.
%
%   @example Convert files specified by a file pattern to MeTTa without console output:
%       ?- convert_to_metta_file('*.pl').
%
convert_to_metta_file(FileSpec) :-
    % Process files in the current directory and convert them without console output.
    with_file_lists('.', convert_to_metta_now(_Create), FileSpec).

%!  convert_to_metta(+Filename) is det.
%
%   Converts a single file to MeTTa format if `Filename` is an atomic and existing file.
%   The conversion is applied using both `convert_to_metta_file/1` and `convert_to_metta_console/1`.
%
%   @arg Filename The name of the file to be converted.
%
%   @example Convert a single Prolog file to MeTTa:
%       ?- convert_to_metta('example.pl').
%
convert_to_metta(Filename) :-
    % If Filename is an atomic value and exists, convert it.
    atomic(Filename), exists_file(Filename), !,
    ignore(convert_to_metta_file(Filename)),
    ignore(convert_to_metta_console(Filename)), !.
convert_to_metta(FileSpec) :-
    % Process files or directories recursively and convert them.
    with_file_lists('.', convert_to_metta, FileSpec).

%!  convert_to_metta_now(+OutputIn, +Filename) is det.
%
%   Processes a given filename by calling `convert_to_metta_now_out/2` within the context
%   of user I/O. This predicate is responsible for managing the conversion of a file
%   to its MeTTa equivalent, preparing the output, and ensuring proper I/O handling.
%
%   @arg OutputIn  The output stream or destination for the converted content.
%   @arg Filename  The name of the file to be processed and converted to MeTTa format.
%
%   @example Convert 'example.pl' to MeTTa format:
%       ?- convert_to_metta_now(user_output, 'example.pl').
%
convert_to_metta_now(OutputIn, Filename) :-
    % Execute the conversion within the user_io context.
    user_io(convert_to_metta_now_out(OutputIn, Filename)).

%!  convert_to_metta_now_out(+OutputIn, +Filename) is det.
%
%   Processes a single filename by opening it, translating its content to MeTTa,
%   and writing the result to a new file with a `.metta` extension. This predicate
%   performs the actual file processing, including setup and cleanup of input/output streams.
%
%   @arg OutputIn  The output stream or destination for the converted content.
%   @arg Filename  The name of the Prolog file to be translated to MeTTa.
%
%   @example Process a file and write its MeTTa version:
%       ?- convert_to_metta_now_out(user_output, 'example.pl').
%
convert_to_metta_now_out(OutputIn, Filename) :-
    atom(Filename),  % Verify that the filename is an atom.
    % Generate the new filename with the .metta extension.
    file_name_extension(Base, _OldExt, Filename),
    file_name_extension(Base, metta, NewFilename),
    file_base_name(Base, Module),
    % Perform the conversion from Prolog to MeTTa.
    convert_to_metta_file(Module, OutputIn, Filename, NewFilename).

%!  write_src_cmt(+G) is det.
%
%   Outputs a source code comment for the given goal `G`. It writes the result
%   of `write_src/1` into a string, then wraps that string in a comment format
%   using `in_cmt/1`.
%
%   @arg G The goal whose source code is being written as a comment.
%
write_src_cmt(G) :-
    ignore((
        with_output_to(string(S), write_src(G)),  % Write the source of G to a string.
        in_cmt(write(S))  % Wrap the output string in a comment format.
    )).

%!  convert_to_metta_file(+Module, +OutputIn, +Filename, +NewFilename) is det.
%
%   Converts the given Prolog file to its corresponding MeTTa file. This predicate
%   handles the setup, translation, and cleanup phases for converting Prolog source code
%   into MeTTa code. The conversion process opens the input and output files, translates
%   the content, and ensures proper closure of the streams.
%
%   @arg Module The module name derived from the file base.
%   @arg OutputIn The output stream or destination for the converted content.
%   @arg Filename The name of the input Prolog file.
%   @arg NewFilename The name of the output MeTTa file.
%
%   @example Convert a Prolog file to a MeTTa file:
%       ?- convert_to_metta_file('example', user_output, 'example.pl', 'example.metta').
%
convert_to_metta_file(Module, OutputIn, Filename, NewFilename) :-
    copy_term(OutputIn, Output),
    % If OutputIn is a variable, write a comment with the conversion process details.
    if_t(var(OutputIn),
        user_io(write_src_cmt(convert_to_metta_file(Module, OutputIn, Filename, NewFilename)))),
    % Setup and cleanup process for input and output file handling.
    setup_call_cleanup(
        open(Filename, read, Input, [encoding(iso_latin_1)]),  % Open input file with ISO Latin-1 encoding.
        setup_call_cleanup(
            (if_t(var(Output), open(NewFilename, write, Output, [encoding(utf8)]))),  % Open output file in UTF-8.
            with_output_to(Output, (
                write_src_cmt(convert_to_metta_file(Module, OutputIn, Filename, NewFilename)),  % Write source comment.
                translate_to_metta(Module, Input)  % Translate content to MeTTa.
            )),
            close(Output)  % Close the output stream.
        ),
        close(Input)  % Close the input stream.
    ).

%!  into_namings(+N) is det.
%
%   Ensures that the variable `V` is converted into a named variable if `V` is an anonymous variable (i.e., `$VAR(N)`).
%   This predicate is commonly used in cases where variables need to be transformed into named variables for readability or debugging purposes.
%   If `V` is already a named variable, it ignores the transformation.
%
%   @arg N A term where `N` is the name and `V` is the variable to be possibly transformed.
%
%   @example Ensure that `X` is named:
%       ?- into_namings(X=Y).
%
into_namings(N=V) :-
    % If V is an anonymous variable, assign it the name N.
    ignore(V='$VAR'(N)).

%!  translate_to_metta(+Module, +Input) is det.
%
%   Recursively translates the content of the input stream to MeTTa format.
%   This predicate handles different types of content such as whitespace characters,
%   Prolog comments (both `%` and `#` styles), and clauses, ensuring proper conversion
%   to MeTTa syntax. The process continues until the end of the file.
%
%   @arg Module The module name associated with the input being translated.
%   @arg Input The input stream representing the file to be translated.
%
%   @example Translate content from an input file to MeTTa:
%       ?- open('example_file.pl', read, InputStream),
%          translate_to_metta('example_module', InputStream),
%          close(InputStream).
%
translate_to_metta(Module, Input) :-
    % Stop at end of stream.
    at_end_of_stream(Input), !, nl.
translate_to_metta(Module, Input) :-
    % Process reprintable characters.
    peek_char(Input, Char), is_reprint_char(Char), !,
    get_char(Input, _), put_char(Char),
    translate_to_metta(Module, Input).
translate_to_metta(Module, Input) :-
    % Convert Prolog '%' comments to MeTTa.
    peek_char(Input, Char), Char == '%',
    get_char(Input, _), put_char(';'),
    read_line_to_string(Input, Cmt),
    print_metta_comments(Cmt), nl,
    translate_to_metta(Module, Input).
translate_to_metta(Module, Input) :-
    % Convert Prolog '#' comments to MeTTa.
    peek_char(Input, Char), Char == '#',
    get_char(Input, _), put_char(';'),
    read_line_to_string(Input, Cmt),
    print_metta_comments(Cmt), nl,
    translate_to_metta(Module, Input).
translate_to_metta(Module, Input) :-
    % Read and process a Prolog clause.
    read_clause_with_info(Input), !,
    translate_to_metta(Module, Input).

% Helper predicates and processing functions follow...

%!  is_reprint_char(+Char) is nondet.
%
%   Determines if a character should be reprinted. A character is considered
%   reprintable if it is a space or a period ('.').
%
%   @arg Char The character to check.
%
%   @example Check if a space is reprintable:
%       ?- is_reprint_char(' ').
%       true.
%
is_reprint_char(Char) :-
    char_type(Char, space).
is_reprint_char(Char) :-
    Char == '.'.

%!  translate_comment(+Cmt, -Str) is det.
%
%   Translates Prolog-style comments to MeTTa-style comments by replacing specific
%   terms such as "prolog" with "MeTTa" and converting the comment marker '%' to ';'.
%
%   @arg Cmt The original Prolog comment string.
%   @arg Str The translated MeTTa comment string.
%
%   @example Translate a Prolog comment to MeTTa:
%       ?- translate_comment("% This is a Prolog comment", Str).
%       Str = "; This is a MeTTa comment".
%
translate_comment(Cmt, Str) :-
    replace_in_string(["%" = ";",
                       "prolog" = "MeTTa",
                       "PROLOG" = "MeTTa",
                       "Prolog" = "MeTTa"], Cmt, Str).

%!  read_clause_with_info(+Stream) is det.
%
%   Reads a Prolog clause from the provided stream while capturing metadata such as
%   variable bindings, term positions, subterm positions, and comments. This is useful
%   for further analysis or debugging purposes.
%
%   @arg Stream The input stream from which the clause is read.
%
%   @example Read a clause from a file stream and capture metadata:
%       ?- open('example_file.pl', read, Stream),
%          read_clause_with_info(Stream),
%          close(Stream).
%
read_clause_with_info(Stream) :-
    % Stop if at the end of the stream.
    at_end_of_stream(Stream), !.
read_clause_with_info(Stream) :-
    % Handle errors while reading a clause.
    catch(read_clause_with_info_0(Stream), E,
          ((user_io(write_src_cmt(E)), write_src_cmt(E)))).
read_clause_with_info_0(Stream) :-
    % Set options for reading the clause with metadata.
    Options = [ variable_names(Bindings),
                term_position(Pos),
                subterm_positions(RawLayout),
                syntax_errors(error),
                comments(Comments),
                module(trans_mod)],
    % Read the term with the specified options.
    read_term(Stream, Term, Options),
    (   (fail, Term == end_of_file)
    ->  true
    ;   % Store term position and variable names.
        b_setval('$term_position', Pos),
        b_setval('$variable_names', Bindings),
        % Display information about the term.
        display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments)).

%!  display_term_info(+Stream, +Term, +Bindings, +Pos, +RawLayout, +Comments) is det.
%
%   Displays information about a Prolog term, processes the term, and prints associated comments.
%   It processes variable bindings, term position, and layout information, while also printing
%   comments in MeTTa format.
%
%   @arg Stream The input stream from which the term was read.
%   @arg Term The Prolog term that was read and is being processed.
%   @arg Bindings A list of variable bindings associated with the term.
%   @arg Pos The position of the term in the source code.
%   @arg RawLayout The layout information of the term.
%   @arg Comments A list of comments associated with the term.
%
%   @example Process a term and display its information:
%       ?- open('example_file.pl', read, Stream),
%          read_term(Stream, Term, [variable_names(Bindings), term_position(Pos), comments(Comments)]),
%          display_term_info(Stream, Term, Bindings, Pos, _, Comments),
%          close(Stream).
%
display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments) :-
    % Process the variable bindings into human-readable names.
    maplist(into_namings, Bindings),
    % Process the term, ignoring errors.
    ignore(process_term(Stream, Term)),
    % Print the associated comments in MeTTa format.
    print_metta_comments(Comments), !.

%!  print_metta_comments(+Comments) is det.
%
%   Prints a list of comments in MeTTa format. This predicate ensures that
%   each comment is properly formatted and output.
%
%   @arg Comments A list of comments to be printed in MeTTa format.
%
%   @example Print a list of comments:
%       ?- print_metta_comments(["This is a comment.", "Another comment."]).
%
print_metta_comments(Comments) :-
    % Print each comment in the provided list.
    print_metta_comment(Comments).

%!  print_metta_comment(+Comments) is det.
%
%   Recursively processes and prints comments in MeTTa format. Handles cases where
%   comments are provided in different formats (lists or pairs) and ensures that
%   all comments are translated and printed correctly.
%
%   @arg Comments The comment or list of comments to be processed and printed.
%
%   @example Print a single comment:
%       ?- print_metta_comment("This is a comment.").
%
%   @example Print a list of comments:
%       ?- print_metta_comment(["Comment 1", "Comment 2"]).
%
print_metta_comment([]) :-
    % Base case: stop when there are no more comments.
    !.
print_metta_comment(_TP-Cmt) :-
    % If the comment is a pair (_TP-Cmt), process the comment part.
    !, print_metta_comment(Cmt).
print_metta_comment([Cmt|Cs]) :-
    % Recursively process a list of comments.
    !, print_metta_comment(Cmt), !, print_metta_comment(Cs).
print_metta_comment(Cmt) :-
    % Translate and print a single comment.
    translate_comment(Cmt, String), print_cmt_lines(String).

%!  print_cmt_lines(+String) is det.
%
%   Normalize the input string, split it by newline, and print each line as a comment.
%   This predicate processes a multi-line string, normalizing the spacing and splitting
%   it by newline characters. It then calls `print_cmt_line/1` to print each individual
%   line as a comment.
%
%   @arg String The input string to be processed and printed as comments.
print_cmt_lines(String):-
    % Normalize the input string to remove excess whitespace and newline characters.
    normalize_space(string(TaxM),String),
    % Split the normalized string into lines by newline character.
    atomics_to_string(List,'\n',TaxM), !,
    % For each line in the list, print it as a comment.
    maplist(print_cmt_line,List).

%!  print_cmt_line(+Str) is det.
%
%   Print a single string as a comment.
%   This predicate takes a single string and prints it in a specific format
%   intended for comments.
%
%   @arg Str The string to be printed as a comment.
print_cmt_line(Str):-
    % Print the string prefixed with a semicolon (comment notation).
    format('~N; ~w',[Str]).

%!  echo_as_commnents_until_eof(+Stream) is det.
%
%   Continuously read from the stream and print each line as a comment until EOF.
%   This predicate reads lines from the given stream and prints them as comments
%   using `print_metta_comments/1` until the end of the stream is reached.
%
%   @arg Stream The input stream to read from.
echo_as_commnents_until_eof(Stream):-
    repeat,
    (at_end_of_stream(Stream) -> !;  % Stop when end of file is reached.
     (read_line_to_string(Stream,Cmt),
      % Ignore any errors and process each line by printing it as a comment.
      ignore((print_metta_comments(Cmt))),
      fail)).

%!  process_term(+Stream, +Term) is det.
%
%   Process a term, either handling it as a directive or converting it into a clause.
%   If the term is an end-of-file marker, it calls `echo_as_commnents_until_eof/1`
%   to print comments from the stream. For directives, it attempts to execute and print
%   them, and for other terms, it expands them into head-body form and writes them.
%
%   @arg Stream The input stream being processed.
%   @arg Term The term to process (either a directive or a Prolog clause).
process_term(Stream,end_of_file):-
    !, echo_as_commnents_until_eof(Stream).  % Handle end of file.
process_term(Stream,Term):-
    % Check if the term is a directive and process it accordingly.
    is_directive(Term),
    ignore(maybe_call_directive(Stream,Term)),
    !, ignore(print_directive(Term)).
process_term(_,Term):-
    % Expand a term into head-body form and push the context for term tracking.
    expand_to_hb(Term,H,B),
    p2m((H:-B),STerm),
    push_term_ctx(Term),
    write_pl_metta(STerm).

%!  maybe_call_directive(+Stream, +Directive) is det.
%
%   Executes or processes specific directives.
%   This predicate handles different types of directives, such as operator definitions
%   or module imports, and ensures they are applied accordingly.
%
%   @arg Stream The input stream associated with the directive.
%   @arg Directive The directive to be processed.
maybe_call_directive(Stream,(:- X)):-
    !, maybe_call_directive(Stream,X).  % Recursive handling for nested directives.
maybe_call_directive(_Stream,op(X,F,Y)):-
    trans_mod:op(X,F,Y).  % Handle operator declarations.
maybe_call_directive(_Stream,use_module(library(W))):-
    trans_mod:use_module(library(W)).  % Handle module imports.
maybe_call_directive(Stream,encoding(Enc)):-
    % Set the encoding of the stream.
    set_stream(Stream,encoding(Enc)).

%!  is_directive(+Term) is nondet.
%
%   Checks if a term is a directive.
%   A term is considered a directive if it has the form `(:- _)`.
%
%   @arg Term The term to check.
is_directive((:- _)).

%!  push_term_ctx(+Term) is det.
%
%   Updates the term context with the current term.
%   This predicate stores the term context to track the current term being processed,
%   ensuring that terms are pushed into the correct context.
%
%   @arg Term The term to push into the context.
push_term_ctx(X):-
    \+ compound(X), !,
    (nb_current(term_ctx,Was) -> true ; Was = []),
    (Was =@= X -> true ; (nb_setval(term_ctx,X),nl)).
push_term_ctx((X:-_)):-
    !, push_term_ctx(X).
push_term_ctx(X):-
    compound_name_arity(X,F,_A),
    push_term_ctx(F).

%!  print_directive(+Directive) is det.
%
%   Print a Prolog directive in a specific format.
%   This predicate converts a directive into a metta-compatible format and
%   prints it as an executable statement.
%
%   @arg Directive The directive to be printed.
print_directive((:- Directive)):-
    % Push the context for directives.
    push_term_ctx(exec),
    % Convert the directive into a metta-compatible format.
    p2m([':-'],Directive,STerm),
    % Print the directive in the desired format.
    write_pl_metta(exec(STerm)).

%!  write_pl_metta(+STerm) is det.
%
%   Write a metta-compatible term to the output.
%   This predicate ensures that the term is properly instantiated before printing it.
%
%   @arg STerm The metta-compatible term to be written.
write_pl_metta(STerm):-
    \+ \+ write_pl_metta_0(STerm).
write_pl_metta_0(STerm):-
    % Instantiate variables and write the source.
    numbervars(STerm,0,_,[singletons(true),attvar(skip)]),
    write_src(STerm).

% Ensure that the MeTTa compiler module is loaded.
:- ensure_loaded(metta_compiler).

% Ensure that the MeTTa conversion utilities are loaded.
:- ensure_loaded(metta_convert).

% Ensure that the MeTTa types system module is loaded.
:- ensure_loaded(metta_types).

% Ensure that the MeTTa space handling module is loaded.
:- ensure_loaded(metta_space).

% Ensure that the MeTTa testing utilities are loaded.
:- ensure_loaded(metta_testing).

% Ensure that the MeTTa utility functions are loaded.
:- ensure_loaded(metta_utils).

% Ensure that the MeTTa printer module is loaded.
:- ensure_loaded(metta_printer).

% Ensure that the MeTTa evaluation logic is loaded.
%:- ensure_loaded(metta_eval).


