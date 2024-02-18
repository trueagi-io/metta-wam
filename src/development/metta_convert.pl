:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- op(700,xfx,'=~').

% ===============================
%    TESTING
% ===============================
% Define 'fb', a rule that calls 'make' and writes information for each clause of 'fb0'.
% 'make' compiles the program.
% The 'forall' loop will write and call all goals of the 'fb0' clauses.


fb:- make,
   writeln(';; ==========================================='),
   forall((clause(fb0,Goal),write(';; '),writeq(?- Goal),nl,call(Goal)),
   writeln(';; ===========================================')).

% The 'fb0' rule showing mettalog sources with specific definitions.
fb0:- show_mettalog_src((two_pi(R):-(pi(A), +(A, A, B), R is B))).
fb0:- show_mettalog_src(factorial_tail_basic).
fb0:- show_mettalog_src(funct).

print_metta_src :-  show_mettalog_src.
% 'show_mettalog_src' rule compiles the program and shows mettalog sources for each source file containing 'metta'.
show_mettalog_src:- make,
  forall((source_file(AsPred,File),
          symbol_contains(File,metta)),
         show_mettalog_src(AsPred)).


% Handling different cases for 'show_mettalog_src' with different input parameters.
% These rules use nonvar, current_predicate, and other built-ins to perform various checks and actions
% based on the type and value of the input to 'show_mettalog_src'.
show_mettalog_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_mettalog_src(F,A)).
show_mettalog_src(C):- show_cvts(C),!.

% The 'show_space_src' rules compile the program and show space sources for each space predicate.
show_space_src:- make,
  forall(space_preds(AsPred),show_space_src(AsPred)).


% Similar to the 'show_mettalog_src' rules, these rules handle different cases for 'show_space_src'
% with different input parameters and perform various checks and actions based on the type and value of the input.
show_space_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_space_src(F,A)).
show_space_src(C):- show_cvts(C),!.

% 'show_cvts' rule processes a term, performing different actions based on the structure of the term.
show_cvts(Term):-
  once((is_list(Term), sexpr_s2p(Term,PF))), \+ is_list(PF),!,show_cvts(PF).

% 'show_cvts' continues processing, performing conversions between predicates and functions,
% and pretty-printing original terms, function forms, and Prolog forms.
show_cvts(Term):- iz_conz(Term),!, ppc(orig,Term),Term = FunctForm,
  functs_to_preds(FunctForm,Prolog), ppc(preds,Prolog),
  preds_to_functs(Prolog,NFunctForm), ppc(functs,NFunctForm).
show_cvts(Term):- ppc(orig,Term),
  preds_to_functs(Term,FunctForm), ppc(functs,FunctForm),
  functs_to_preds(FunctForm,Prolog), ppc(preds,Prolog).

% 'show_mettalog_src' for specific predicate, prints metta clauses if they exist in the source file containing 'metta'.
show_mettalog_src(F,A):- functor(Head,F,A),
  ignore((predicate_property(Head,number_of_clauses(_)),
    source_file(Head,File),atom_contains(File,metta),!,
    nl,findall((Head:-Body),
       clause(Head,Body), Clauses),
    print_metta_clauses(Clauses))),nl.

% 'print_metta_clauses' rule is handling the printing of metta clauses.
% It checks the form of the input clauses and calls 'print_metta_clause' accordingly.
print_metta_clauses([]):- !.
print_metta_clauses([Head:-Body]):- !, print_metta_clause(Head,Body).
print_metta_clauses(Clauses):- combine_clauses(Clauses,Head,Body),!,print_metta_clause(Head,Body).
print_metta_clause(Head,Body):-
  print_metta_clause0(Head,Body),
  show_cvts(Head:-Body).

% 'print_metta_clause0' rule prints metta clauses based on the body.
% It transforms the body to a list, if needed, and prints it in a sequential form.
print_metta_clause0(Head,Body):- Body == true,!, pp_metta([=,Head,'True']).
print_metta_clause0(Head,Body):- Body == false,!, pp_metta([=,Head,'False']).
print_metta_clause0(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP), pp_metta([=,Head,SP]).



% =========================================
%  STERM -> PTERM
% =========================================

iz_exact_symbol(N,_):- \+ atom(N),!,fail.
iz_exact_symbol(N,P):- nonvar(P),!,iz_exact_symbol(N,PP),zalwayz(P=PP).
iz_exact_symbol(':-',':-').
iz_exact_symbol('?-','?-').
iz_exact_symbol('??',_).

%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

maybe_varz(S,Name,'$VAR'(Name)):- S=='?',atom(Name),!.

%% sexpr_s2p(Fn,?VAR, ?V) is det.
%
% S-expression Sterm Converted To Pterm.
%
sexpr_s2p(HB,P):- fail, compound(HB), HB=~ (H=B), compile_for_assert(H,B,Cl),
   clause_to_code(Cl,P),!.
sexpr_s2p(S,P):- sexpr_s2p(progn,1,S,P).


clause_to_code(P,P):- is_ftVar(P),!.
%clause_to_code(P:-True,P):- True == true,!.
clause_to_code((H:-B),P):- B==true, !, combine_code(B,H,P).
clause_to_code(P,P).

sexpr_s2p(_Fn,_Nth,VAR,VAR):-is_ftVar(VAR),!.
sexpr_s2p(_Fn,_Nth,S,P):- iz_exact_symbol(S,P),!.
sexpr_s2p(_Fn,_Nth,'#'(S),P):- iz_exact_symbol(S,P),!.
sexpr_s2p(_Fn,_Nth,VAR,'$VAR'(Name)):- atom(VAR),svar(VAR,Name),!.
sexpr_s2p(Fn,Nth,S,P):- S==[], iz_fun_argz(Fn,Nth),!,P=S.

sexpr_s2p(Fn,Nth,S,P):- expects_type(Fn,Nth,Type),will_become_type(Type,S,P),!.

sexpr_s2p(_Fn,_Nth,[F|SList],P):- is_list(SList), length(SList,Len),is_syspred(F,Len,Pred), sexpr_s2p_arglist(F,1,SList,PList), !, P=..[Pred|PList].
:- style_check(-singleton).

sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- iz_fun_argz(Fn,Nth),!,sexpr_s2p(S,P), sexpr_s2p(Fn,Nth,SList,PList).
sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- ( \+ atom(S) ; \+ is_list(SList)), !,sexpr_s2p(list(Fn),Nth,S,P), sexpr_s2p(list(Fn),Nth,SList,PList).
sexpr_s2p(_Fn,_Nth,[S,STERM0],PTERM):- iz_quoter(S),sexpr_s2p_pre_list(S,0,STERM0,STERM), !,PTERM=..[S,STERM],!.
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
sexpr_s2p(Fn,Nth,[S|STERM0],PTERM):-
  sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM),
  sexpr_s2p_arglist(S,1,STERM,PLIST), z_univ(Fn,Nth,PTERM,[S|PLIST]),!.
sexpr_s2p(_Fn,_Nth,VAR,VAR).


expects_type(Fn,Nth,Type):-
  get_operator_typedef(Self,Fn,Params,RetType),
  nth0(Nth,[RetType|Params],Type),nonvar(Type).

will_become_type(Type,S,P):- try_adjust_arg_types(=,_RetType,88,_Self,[Type],[S],[PS]),PS=P,!.
will_become_type(Type,S,P):- is_ftVar(S),!,P=S.
will_become_type(Type,S,P):-
   get_type(S,T),!,
     (is_subtype(T,Type)->S=P; P=coerce(Type,S)).
will_become_type(_Type,S,P):-!,S=P.

is_subtype(T,TT):- T=@=TT,!,T=TT.
is_subtype(T,TT):- T=TT,!.

iz_quoter('#BQ'):- iz_common_lisp.
iz_quoter('#COMMA'):- iz_common_lisp.
iz_quoter('quote').
iz_quoter(superpose).

iz_fun_argz(list(_),_).
iz_fun_argz(defmacro,2).
iz_fun_argz(defun,2).
iz_fun_argz(let,1).
iz_fun_argz('let*',1).
iz_fun_argz('member',2).
%iz_fun_argz('let*',2).
iz_fun_argz(F,1):- iz_quoter(F).

z_functor(F):- \+ atom(F), !,fail.
z_functor(F):- \+ atom_concat('?',_,F).
z_functor(F):- \+ atom_concat('$',_,F).

%z_univ(_Fn,1,S,S):-!.
z_univ(_Fn,_,P,[F|ARGS]):- z_functor(F),is_list(ARGS),length(ARGS,A),l_arity_l(F,A),compound_name_arguments(P,F,ARGS),!.
z_univ(_Fn,0,P,[F|ARGS]):- z_functor(F),is_list(ARGS),compound_name_arguments(P,F,ARGS),!.
z_univ(_Fn,_Nth,P,[F|ARGS]):- z_functor(F),is_list(ARGS),compound_name_arguments(P,F,ARGS),!.
z_univ(_Fn,_Nth,P,S):-P=S.

l_arity_l(F,A):- clause_b(arity(F,A)).
l_arity_l(function,1).
l_arity_l(quote,1).
l_arity_l('#BQ',1):- iz_common_lisp.
l_arity_l(F,A):-current_predicate(F/A).
l_arity_l(_,1).

sexpr_s2p_arglist(_Fn,_,VAR,VAR):-is_ftVar(VAR),!.
sexpr_s2p_arglist(Fn,Nth,[S|SList],[P|PList]):-sexpr_s2p(Fn,Nth,S,P),
  (Nth>0->Nth2 is Nth+1;Nth2=0),sexpr_s2p_arglist(Fn,Nth2,SList,PList),!.
sexpr_s2p_arglist(Fn,Nth,S,P):-sexpr_s2p(Fn,Nth,S,P),!.
sexpr_s2p_arglist(_Fn,_Nth,VAR,VAR).

sexpr_s2p_pre_list(_Fn,_,STERM,STERM):- \+ compound(STERM), !.
sexpr_s2p_pre_list(_Fn,_,STERM,STERM):- \+ is_list(STERM), !.
% sexpr_s2p_pre_list(Fn,_,[S|STERM],[S|STERM]):- STERM == [], !.
sexpr_s2p_pre_list(Fn,Nth,[S0|STERM0],[S|STERM]):-
 (is_list(S0)->sexpr_s2p(Fn,Nth,S0,S);sexpr_s2p_pre_list(Fn,Nth,S0,S)),
 sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM),!.
sexpr_s2p_pre_list(_Fn,_,STERM,STERM).



into_hyphens(D,U):- atom(D),!,atomic_list_concat(L,'_',D),atomic_list_concat(L,'-',U).
into_hyphens(D,U):- descend_and_transform(into_hyphens,D,U),!.

% p2m/2 is a translation utility to convert Prolog constructs to MeTTa constructs.
% It handles a variety of cases, including different types of compound terms,
% control structures, and predicate definitions.
% The first argument is the input in Prolog syntax,
% and the second argument is the output converted to MeTTa syntax.


p2m(I,O):- p2m([progn],I,O).

p2m(_OC,NC, NC) :- var(NC), !.  % If NC is a variable, do not translate.
p2m(_OC,NC, NC) :- is_ftVar(NC), !.  % If NC is a free term variable, do not translate.
p2m(_OC,[], 'Nil'). % empty list
p2m(_OC,M:I, with_self(N,O)):-  p2m(OC,M,N),p2m(I,O).
% Conversion for lists
p2m(OC,[H|T],['::'|L]):- is_list([H|T]),maplist(p2m(OC),[H|T],L).
p2m(OC,[H|T], 'Cons'(OH, OT)):- p2m(OC,H, OH), p2m(OC,T, OT).
p2m(OC,NC, OO) :-
    % If NC is a list, map each element of the list from Prolog to MeTTa
    is_list(NC),!,
    maplist(p2m(OC), NC, OO).
p2m(_OC,!, ['set-det']).  % Translate the cut operation directly.
p2m(_OC,!, '!').  % Translate the cut operation directly.
p2m(_OC,false, 'False').
p2m([progn|_], (!,fail), [empty]).  % Translate Prolog�s fail to MeTTa�s False.
% p2m(_OC,fail, 'False').  % Translate Prolog�s fail to MeTTa�s False.
p2m(_OC,true, 'True').  % Translate Prolog�s true to MeTTa�s True.
% p2m(_OC,prolog, meTTa).  % Translate the atom prolog to meTTa.

p2m(_OC,'[|]','Cons').
p2m(_OC,( ';' ),or).
%p2m(_OC,( ',' ),and).
%p2m(_OC,( '\\+' ),unless).
%p2m(_OC,( ':-' ),entailed_by).
%p2m(_OC,'=..','atom_2_list').

% Conversion for any atomic term
p2m(_OC,A, A):- string(A),!.
p2m([progn|_],A, [H]):- atom(A),into_hyphens(A,H),!.
p2m(_,A, H):- atom(A),into_hyphens(A,H),!.
p2m(_OC,A, A):- atomic(A).

p2m(_OC,NC,NC):- \+ compound(NC),!.
p2m(_OC,NC,[F]):- compound_name_arity(NC,F,0),!.

% Conversion for the negation as failure
p2m(_OC,(\+ A), O):- !, p2m(_OC,not(A), O).

p2m(_OC,(G,E),O):-  conjuncts_to_list((G,E),List),!,into_sequential(List,O),!.

% Conversion for arithmetic evaluation
%p2m(_OC,is(A, B), O):- !, p2m(_OC,eval(B, A), O).
%p2m(_OC,is(V,Expr),let(V,Expr,'True')).
p2m(_OC,(Head:-Body),O):- Body == true,!, O = (=(Head,'True')).
p2m(_OC,(Head:-Body),O):- Body == fail,!, O = (=(Head,[empty])).
p2m(_OC,(Head:-Body),O):- 
   p2m(Head,H),conjuncts_to_list(Body,List),into_sequential(List,SP),!,
   O =  (=(H,SP)).

p2m(_OC,(:-Body),O):- 
   conjuncts_to_list(Body,List),into_sequential(List,SP),!, O= exec(SP).

%p2m(_OC,(Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(List,SP),!,O=(=(Head,SP)).

% Conversion for if-then-else constructs
p2m(_OC,(A->B;C),O):- !, p2m(_OC,if_then_else(A,B,C),O).
p2m(_OC,(A;B),O):- !, p2m(_OC,or(A,B),O).
p2m(_OC,(A*->B;C),O):- !, p2m(_OC,each_then_otherwise(A,B,C),O).
p2m(_OC,(A->B),O):- !, p2m(_OC,if_then(A,B),O).
p2m(_OC,(A*->B),O):- !, p2m(_OC,each_then(A,B),O).
p2m(_OC,metta_defn(Eq,Self,H,B),'add-atom'(Self,[Eq,H,B])).
p2m(_OC,metta_type,'add-atom').
p2m(_OC,get_metta_atom,'add-atom').
p2m(_OC,retractall(X),'remove-all-atoms'('&self',X)).
p2m(_OC,clause(H,B),'get-atoms'('&self',[=,H,B])).
p2m(_OC,retract(X),'remove-atom'('&self',X)).
p2m(_OC,assert(X),'add-atom'('&self',X)).
% The catch-all case for the other compound terms.
%p2m(_OC,I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=..OO.

% It will break down compound terms into their functor and arguments and apply p2m recursively
p2m(OC,I, O):-
    compound(I),
    I =.. [F|II], % univ operator to convert between a term and a list consisting of functor name and arguments
    maplist(p2m([F|OC]), II, OO), % applying p2m recursively on each argument of the compound term
    into_hyphens(F,FF),
    O = [FF|OO]. % constructing the output term with the converted arguments


% In the context of this conversion predicate, each branch of the p2m predicate
% is handling a different type or structure of term, translating it into its
% equivalent representation in another logic programming language named MeTTa.
% The actual transformations are dependent on the correspondence between Prolog
% constructs and MeTTa constructs, as defined by the specific implementations
% of Prolog and MeTTa being used.
prolog_to_metta(V, D) :-
    % Perform the translation from Prolog to MeTTa
    p2m([progn], V, D),!.


% Define predicates to support the transformation from Prolog to MeTTa syntax
% (Continuing the translation from Prolog to MeTTa syntax as per the given code)
% Handle the case where the body is a conjunction of terms
into_sequential(Body, SP) :-
    % Check if Body is not a list and convert conjunctions in Body to a list of conjuncts.
    \+ is_list(Body),
    conjuncts_to_list(Body, List),
    is_list(List), % Converts a list of conjunctions into a sequential representation in MeTTa
    into_sequential(List, SP), !.
into_sequential(Nothing,'True'):- Nothing ==[],!.
 % If there's only one element
into_sequential([SP],O):- prolog_to_metta(SP,O).
% Otherwise, construct sequential representation using AND.
into_sequential(List, [AND|SPList]) :- is_compiled_and(AND), maplist(prolog_to_metta, List, SPList),!.



% Entry point for printing to Metta format. It clears the screen, sets the working directory,
% expands the filenames with a specific extension, and processes each file.
print_to_metta :-
 % cls, % Clears the screen (assumes a custom or system-specific implementation).
 % with_pwd(
  %   '/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/examples/gpt2-like/language_models/',
 %Filt = 'examples/gpt2-like/language_models/*.pl',
 Filt = '/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/examples/performance/nondet_unify/*.pl',

   % Finds all Prolog files in the specified directory.
   print_to_metta(Filt),  % Processes each found file.
  % MC = '/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/metta_vspace/pyswip/metta_convert.pl',
  % print_to_metta(MC), % Processes each found file.
   !.
% Example of a no-operation (nop) call for a specific file path, indicating a placeholder or unused example.
%$nop(print_to_metta('/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/vspace-metta/metta_vspace/pyswip/metta_convert.pl')).

% Processes a list of filenames, applying 'print_to_metta' to each.
with_file_lists(P1,FileSpec):- is_list(FileSpec),!,maplist(with_file_lists(P1),FileSpec).
with_file_lists(P1,Directory):- atom(Directory), exists_directory(Directory), 
  findall(File,directory_source_files(Directory, File, [recursive(true),if(true)]),Files),
  maplist(with_file_lists(P1),Files).
with_file_lists(P1,Mask):- atom(Mask), \+ exists_file(Mask), 
  expand_file_name(Mask, Files), Files\==[],!,maplist(with_file_lists(P1),Files).
with_file_lists(P1,Filename):- ignore(call(P1,Filename)).
 

print_to_metta(Filename):- 
  ignore(print_to_metta_file(Filename)),
  ignore(print_to_metta_console(Filename)),!.
  

% Processes a list of filenames, applying 'print_to_metta' to each.
print_to_metta_console(FileSpec):-  with_file_lists(print_to_metta_now(user_output),FileSpec).
print_to_metta_file(FileSpec):-  with_file_lists(print_to_metta_now(_Create),FileSpec).

% Processes a single filename by opening the file, translating its content, and then closing the file.
print_to_metta_now(OutputIn,Filename):-
    atom(Filename),  % Verifies that the filename is an atom.
    % Generate the new filename with .metta extension.
    file_name_extension(Base, _OldExt, Filename),
    file_name_extension(Base, metta, NewFilename),
    % Setup step: open both the input and output files.
    format('~N~n~w~n', [print_to_metta(Filename,NewFilename)]), % Prints the action being performed.
    %Output = user_output,
    copy_term(OutputIn,Output),
    setup_call_cleanup(
        open(Filename, read, Input, [encoding(utf8)]),
        % Call step: perform the translation and write to the output file.
        setup_call_cleanup(
            (if_t(var(Output),open(NewFilename, write, Output, [encoding(utf8)]))),
            with_output_to(Output,translate_to_metta(Input)),
            % Cleanup step for the output file: close the output stream.
            close(Output)
        ),
        % Cleanup step for the input file: close the input stream.
        close(Input)
    ).

into_namings(N=V):- ignore(V='$VAR'(N)).

% Recursively translates content, stopping at the end of the file.
translate_to_metta(Input):- 
    at_end_of_stream(Input),  % Checks for the end of the file.
    !, nl.

% Processes whitespace characters, maintaining their presence in the output.
translate_to_metta(Input):- 
    peek_char(Input, Char),  % Peeks at the next character without consuming it.
    is_reprint_char(Char), !,
    get_char(Input, _),  % Consumes the character.
    put_char(Char),  % Prints the character.
    translate_to_metta(Input).

% Converts Prolog comments to Metta-style comments, then continues processing.
translate_to_metta(Input):- 
    peek_char(Input, Char), 
    Char == '%', % Checks for Prolog comment start.
    get_char(Input, _), put_char(';'),  
    read_line_to_string(Input, Cmt),  % Reads the comment line.
    print_metta_comments(Cmt),nl, % Converts and prints the comment in Metta style.
    translate_to_metta(Input).  % Continues with the next line.

% Reads a clause along with its metadata, then continues translation.
translate_to_metta(Input):- 
  read_clause_with_info(Input),!,
  translate_to_metta(Input).

% Helper predicates and processing functions follow...

% Determines if a character should be reprinted (spaces and period).
is_reprint_char(Char):- char_type(Char, space).
is_reprint_char(Char):- Char == '.'.

% Translates Prolog comments to Metta comments, applying string replacements.
translate_comment(Cmt,Str):- replace_in_string(["%"=";","prolog"="MeTTa","Prolog"="MeTTa"],Cmt,Str).

% Reads a clause while capturing various pieces of metadata.
read_clause_with_info(Stream) :-
    Options = [ variable_names(Bindings),
                    term_position(Pos),
                    subterm_positions(RawLayout),
                    syntax_errors(error), 
                    comments(Comments),
                    module(trans_mod)],
    read_term(Stream, Term, Options),
    (   Term == end_of_file
    ->  true
    ;   b_setval('$term_position', Pos),
        b_setval('$variable_names', Bindings),
        display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments)).

% Displays term information and processes comments.
display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments):-
   maplist(into_namings,Bindings),
   process_term(Term),
   print_metta_comments(Comments).


print_metta_comments([]):-!.
print_metta_comments(_TP-Cmt):-!, print_metta_comments(Cmt).
print_metta_comments([Cmt|Cs]):- !, print_metta_comments(Cmt),nl,!, print_metta_comments(Cs).
print_metta_comments(Cmt):- translate_comment(Cmt,String),write(String).

% Processes each term based on its type (directive or other).
process_term(end_of_file):- !.
process_term(Term):-
    is_directive(Term), 
    ignore(maybe_call_directive(Term)),
    !, print_directive(Term).
process_term(Term):-
  expand_to_hb(Term,H,B),
  p2m((H:-B),STerm),
  push_term_ctx(Term),
  write_src(STerm).

maybe_call_directive((:- op(X,F,Y))):- trans_mod:op(X,F,Y).

% Checks if a term is a directive.
is_directive((:- _)).

push_term_ctx(X):- \+ compound(X),!,
  (nb_current(term_ctx,Was)->true;Was=[]),
  (Was =@= X -> true; (nb_setval(term_ctx,X),nl)).
push_term_ctx((X:-_)):- !, push_term_ctx(X).
push_term_ctx(X):- compound_name_arity(X,F,_A),push_term_ctx(F).
% Print a Prolog directive in a specific format.
print_directive((:- Directive)):- 
  push_term_ctx(exec), % pc
  p2m(Directive,STerm), % p2m
  write_exec(STerm). %we






