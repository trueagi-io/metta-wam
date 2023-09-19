:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_interp).
%:- ensure_loaded(metta_compiler).
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(swi_flybase).
% =======================================
% Scryer Rust Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% =======================================
:- set_option_value(encoding,iso_latin_1).

:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).


:- multifile(is_pre_statistic/2).
:- dynamic(is_pre_statistic/2).
save_pre_statistic(Name):- is_pre_statistic(Name,_)-> true; (statistics(Name,AS),term_number(AS,FN),assert(is_pre_statistic(Name,FN))).
pre_statistic(N,V):- is_pre_statistic(N,V)-> true ; V = 0.
post_statistic(N,V):- statistics(N,VV),term_number(VV,FV),pre_statistic(N,WV), V0 is FV-WV, (V0<0 -> V = 0 ; V0=V).
term_number(T,N):- sub_term(N,T),number(N).


call_match([G]):-!, call(G).
call_match([G|GG]):- !, call(G), call_match(GG).
call_match(G):- call(G).


:- dynamic(repeats/1).
:- dynamic(not_repeats/1).
assert_new(P):- call(P),!,assert_new1(repeats(P)).
assert_new(P):- assert(P), flag(assert_new,TA,TA+1),assert_new1(not_repeats(P)),!.

retract1(P):- \+ call(P),!.
retract1(P):- ignore(\+ retract(P)).

assert_new1(P):- \+ \+ call(P),!.
assert_new1(P):- assert(P).


:- dynamic(fb_pred/3).
:- dynamic(mod_f_a/3).
decl_m_fb_pred(Mod,Fn,A):- var(Mod),!,mod_f_a(Mod,Fn,A).
decl_m_fb_pred(Mod,Fn,A):- mod_f_a(Mod,Fn,A)->true;(dynamic(Mod:Fn/A),assert(mod_f_a(Mod,Fn,A))).
:- dynamic(fb_pred_file/3).
decl_fb_pred(Fn,A):-
   (fb_pred(Fn,A)-> true; (dynamic(Fn/A),assert(fb_pred(Fn,A)))),
   ignore((nb_current(loading_file,File),
    (fb_pred_file(Fn,A,File)-> true; assert(fb_pred_file(Fn,A,File))))).
% Import necessary libraries
:- use_module(library(readutil)).


skip(_).

% ===============================
% MeTTa Python incoming interface
% ===============================
%debug_metta(Call):- skip(Call).
if_metta_debug(Goal):- getenv('VSPACE_VERBOSE','2'),!,ignore(call(Goal)).
if_metta_debug(_).
debug_metta(Term):- if_metta_debug((format('~N; ~@~n',[write_src(Term)]))).
debug_metta(Msg,Term):- if_metta_debug((format('~N; ~w: ~@~n',[Msg,write_src(Term)]))),!.

%:- dynamic(for_metta/2).
%for_metta(_,T):- fb_pred(F,A),functor(T,F,A),call(T).
metta_ls(KB):-
  listing(KB:for_metta/2).
metta_add(KB,New):- decl_m_fb_pred(KB,for_metta,2), MP = KB:for_metta(KB,New), assert_new(MP), debug_metta(['add-atom',KB,New]).
metta_rem(KB,Old):- debug_metta(['remove-atom',KB,Old]),metta_del(KB,Old).
metta_del(KB,Old):- decl_m_fb_pred(KB,for_metta,2), MP = KB:for_metta(KB,Old),
  copy_term(MP,Copy), clause(MP,true,Ref), MP =@= Copy, !, erase(Ref). % ,debug_metta('DEL',Old).
metta_replace(KB,Old,New):- debug_metta(['atom-replace',KB,Old,New]),!, metta_del(KB,Old), metta_add(KB,New).
metta_count(KB,Count):-
 must_det_ll((
  debug_metta(['atom-count',KB]),
  decl_m_fb_pred(KB,for_metta,2), full_symbol_count(SL1),
  MP = KB:for_metta(_,_),
  predicate_property(MP,number_of_clauses(SL2)),
  predicate_property(MP,number_of_rules(SL3)),
  %metta_ls(KB),
  Count is SL1 + SL2 - SL3)),!.
metta_count(_KB,0):-!.
%metta_count(KB,Count):- writeln(metta_count_in(KB,Count)), findall(Atom,for_metta(KB,Atom),AtomsL),length(AtomsL,Count),writeln(metta_count_out(KB,Count)).
metta_iter(KB,Atoms):- decl_m_fb_pred(KB,for_metta,2), KB:for_metta(KB,Atoms).
metta_atoms(KB,AtomsL):- debug_metta(['get-atoms',KB]), decl_m_fb_pred(KB,for_metta,2), findall(Atom,KB:for_metta(KB,Atom),AtomsL).
%metta_iter_bind(KB,Query,Template,AtomsL):- decl_m_fb_pred(KB,for_metta,2), findall(Template,KB:for_metta(KB,Query),AtomsL).
metta_iter_bind(KB,Query,Vars):-
  ignore(term_variables(Query,Vars)),
  print(debug_metta(['match',KB,Query,Vars])),nl,
  decl_m_fb_pred(KB,for_metta,2), (KB:for_metta(KB,Query)*->true;call_metta(KB,Query,Vars)),
  debug_metta('RES',metta_iter_bind(KB,Query,Vars)).
%metta_iter_bind(KB,Atom,Template):- metta_stats, findall(Template,metta_iter(KB,Atom),VarList).
/*
metta_iter_bind(KB,Atoms,Vars):-
  metta_stats,
  term_variables(Atoms,AVars),
  metta_iter(KB,Atoms), ignore(AVars = Vars).
*/

call_metta(_KB,Query,_Vars):- metta_to_pyswip([],Query,Call),!,
  print(user:Call),nl,user:call(Call).

metta_to_pyswip(_PS,Query,Call):- var(Query),!,Call=Query.
metta_to_pyswip(_PS,Query,Call):- \+ compound(Query),!,Call=Query,!.
metta_to_pyswip(PS,Query,Call):- is_list(Query),Query=[Q|Uery],!,cmpd_to_pyswip(PS,Q,Uery,Call).
metta_to_pyswip(PS,Query,Call):- Query=..[Q|Uery], cmpd_to_pyswip(PS,Q,Uery,Call).

cmpd_to_pyswip(PS,Q,Uery,Call):- atom(Q),maplist(metta_to_pyswip([Q|PS]),Uery,Cery),Call=..[Q|Cery].
cmpd_to_pyswip(PS,"and",Uery,Call):- maplist(metta_to_pyswip(PS),Uery,Args),list_to_conjuncts(Args,Call).

/*
symbol(X):- atom(X).
symbol_number(S,N):- atom_number(S,N).
symbol_string(S,N):- atom_string(S,N).
symbol_chars(S,N):- atom_chars(S,N).
symbol_length(S,N):- atom_length(S,N).
symbol_concat(A,B,C):- atom_concat(A,B,C).
symbolic_list_concat(A,B,C):- atomic_list_concat(A,B,C).
symbol_contains(T,TT):- atom_contains(T,TT).
*/

allow_concepts:- option_else(concepts,TF,true), \+ TF == false.
with_concepts(TF,Goal):- with_option(concepts,TF,Goal).

direct_mapping(NC,NC):- var(NC),!.
direct_mapping(NC,OO):- is_list(NC),!,maplist(direct_mapping,NC,OO).
direct_mapping(!,'!').
direct_mapping(fail,'False').
direct_mapping(true,'True').
direct_mapping(prolog,meTTa).
direct_mapping('[|]','Cons').
%direct_mapping(( ';' ),or).
%direct_mapping(( ',' ),and).
direct_mapping(( '\\+' ),unless).
%direct_mapping(( ':-' ),entailed_by).
direct_mapping('=..','atom_2_list').
direct_mapping(NC,NC):- \+ compound(NC),!.
direct_mapping((G,E),O):- conjuncts_to_list((G,E),List), into_sequential(List,O),!.
direct_mapping((A->B;C),O):- !, direct_mapping(if_then_else(A,B,C),O).
direct_mapping((A*->B;C),O):- !, direct_mapping(each_then_otherwise(A,B,C),O).
direct_mapping((A->B),O):- !, direct_mapping(if_then(A,B),O).
direct_mapping((A*->B),O):- !, direct_mapping(each_then(A,B),O).
direct_mapping(I,O):- I=..[F|II],maplist(direct_mapping,[F|II],OO),O=..OO.
direct_mapping(retract(X),'remove-atom'('&self',X)).
direct_mapping(metta_defn(Self,H,B),'add-atom'(Self,[=,H,B])).
direct_mapping(metta_type,'add-atom').
direct_mapping(metta_atom,'add-atom').
direct_mapping(assert(X),'add-atom'('&self',X)).
direct_mapping(retractall(X),'remove-all-atoms'('&self',X)).
direct_mapping(clause(H,B),'get-atoms'('&self',[=,H,B])).



print_metta_src:- mmake,
  for_all((source_file(Pred,File),
          symbol_contains(File,flybase)),
         print_metta_src(Pred)).

print_metta_src(F/A):- !, print_metta_src(F,A).
print_metta_src(Pred):- functor(Pred,F,A), print_metta_src(F,A).

print_metta_src(F,A):- functor(Head,F,A),
  nl,nl,nl,
  for_all(clause(Head,Body), pp_metta(Head,Body)).
pp_metta(Head,Body):- Body == true,!, pp_metta(=(Head,'True')).
pp_metta(Head,Body):- Body == false,!, pp_metta(=(Head,'False')).
pp_metta(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP),!,
  pp_metta(=(Head,SP)).


pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

into_sequential(Body,SP):- \+ is_list(Body), conjuncts_to_list(Body,List),into_sequential(List,SP).
into_sequential(List,SP):- length(List,L),L>1, SP =.. [','|List],!.
into_sequential([SP],SP):-!.
into_sequential([],'True').

write_src(V):- allow_concepts,!,with_concepts(false,write_src1(V)),flush_output.
write_src(V):- write_src1(V),!.

write_src1(V):- var(V),!, ignore(pp_sex(V)).
write_src1(''):- !, writeq('').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).
write_src1(V):- symbol(V),needs_quoted_in_metta(V,_),!, symbol_string(V,S),writeq(S).
write_src1(V):- symbol(V),!,write(V).
write_src1(V):- pp_sex(V),!.

needs_quoted_in_metta('','"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V," ").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"/").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,'"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V,'"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V,',').
%needs_quoted_in_metta(V,"'"):- symbol_length(V,L),L==1.
%needs_quoted_in_metta(V,"'"):- symbol_contains(V,")").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"|").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"'").

pp_sax(S) :-  \+ allow_concepts,!, write_src(S).
pp_sax(S) :- is_englishy(S),!,print_concept("StringValue",S).
pp_sax(S) :- symbol_length(S,1),symbol_string(S,SS),!,print_concept("StringValue",SS).
pp_sax(S) :- is_an_arg_type(S,T),!,print_concept("TypeNode",T).
pp_sax(S) :- has_type(S,T),!,format('(~wValueNode "~w")',[T,S]).
pp_sax(S) :- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,(Aft>0->format('(~wValueNode "~w")',[Type,S]);format('(TypeNode "~w")',[Type])).
pp_sax(S) :- print_concept("ConceptNode",S).

print_concept( CType,V):- allow_concepts, !, write("("),write(CType),write(" "),ignore(with_concepts(false,write_src(V))),write(")").
print_concept(_CType,V):- ignore(write_src(V)).
write_val(V):- number(V),!, write_src(V).
write_val(V):- compound(V),!, write_src(V).
write_val(V):- write('"'),write(V),write('"').

% Base case: atoms are printed as-is.
pp_as(V) :- \+ \+ pp_sex(V),flush_output.
pp_sex(V) :- var(V), !, format('$~p',[V]).
pp_sex('!'(V)) :- write('!'),!,pp_sex(V).
pp_sex(V) :- direct_mapping(V,D),V\==D,!,pp_sex(D).
%pp_sex('') :- format('(EmptyNode null)',[]).
pp_sex('') :- format('()',[]).
pp_sex([]):-  !, write('()').
pp_sex('='(N,V)):- allow_concepts, !, format("~N;; ~w == ~n",[N]),!,pp_sex(V).
pp_sex(V) :- (number(V) ; is_dict(V)), !, print_concept('ValueAtom',V).
pp_sex(V) :- (symbol(V),symbol_number(V,N)), !, print_concept('ValueAtom',N).
pp_sex(S) :- symbol(S), always_dash_functor(S,D), pp_sax(D),!.
pp_sex(S) :- string(S),!, print_concept('StringValue',S).
% Lists are printed with parentheses.
pp_sex(V) :- \+ compound(V), !, format('~p',[V]).
pp_sex(V) :- V = '$VAR'(_), !, format('$~p',[V]).
pp_sex('!'(V)) :- write('!'),!,pp_sex(V).
pp_sex(listOf(S,_)) :- !,pp_sex(listOf(S)).
pp_sex(listOf(S)) :- !,format('(ListValue ~@)',[pp_sex(S)]).

pp_sex(V) :- w_proper_indent(1,w_in_p(pp_sexi(V))).

pp_sexi([H|T]) :- is_list(T),!,
   write('('), pp_sex(H), print_list_as_sexpression(T), write(')').
% Compound terms.
%pp_sex(Term) :- compound(Term), Term =.. [Functor|Args], write('('),format('(~w ',[Functor]), write_args_as_sexpression(Args), write(')').
%pp_sex(Term) :- Term =.. ['=',H|Args], length(Args,L),L>2, write('(= '),  pp_sex(H), write('\n\t\t'), maplist(pp_sex(2),Args).
pp_sexi(Term) :- Term =.. [Functor|Args], always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.
pp_sexi(Term) :- allow_concepts, Term =.. [Functor|Args], format('(EvaluationLink (PredicateNode "~w") (ListLink ',[Functor]), write_args_as_sexpression(Args), write('))'),!.
pp_sexi(Term) :- Term =.. [Functor|Args],
   always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.

pp_sex(2,Arg):- write('\t\t'),pp_sex(Arg).


current_column(Column) :- current_output(Stream), line_position(Stream, Column),!.
current_column(Column) :- stream_property(current_output, position(Position)), stream_position_data(column, Position, Column).
min_indent(Sz):- current_column(Col),Col>Sz,nl,indent_len(Sz).
min_indent(Sz):- current_column(Col),Need is Sz-Col,indent_len(Need),!.
min_indent(Sz):- nl, indent_len(Sz).
indent_len(Need):- forall(between(1,Need,_),write(' ')).

w_proper_indent(N,G):-
  flag(w_in_p,X,X), (X==0->nl;true),
  XX is X+N,setup_call_cleanup(min_indent(XX),G,true).
w_in_p(G):- setup_call_cleanup(flag(w_in_p,X,X+1),G,flag(w_in_p,_,X)).


always_dash_functor(A,B):- once(dash_functor(A,B)),A\=@=B,!.
always_dash_functor(A,A).

dash_functor(A,C):- \+ symbol(A),!,C=A.
dash_functor(A,C):- direct_mapping(A,B),A\==B,!,always_dash_functor(B,C).
dash_functor(Functor,DFunctor):-
   symbol(Functor), atomic_list_concat(L,'-',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'-',DFunctor).
dash_functor(Functor,DFunctor):- fail,
   symbol(Functor), atomic_list_concat(L,'_',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'-',DFunctor).
dash_functor(Functor,DFunctor):-
   symbol(Functor), atomic_list_concat(L,'_',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'_',DFunctor).


is_an_arg_type(S,T):- flybase_identifier(S,T),!.
has_type(S,Type):- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,Aft>0.

% Print arguments of a compound term.
write_args_as_sexpression([]).
write_args_as_sexpression([H|T]) :- write(' '), pp_sex(H), write_args_as_sexpression(T).

% Print the rest of the list.
print_list_as_sexpression([]).
%print_list_as_sexpression([H]):- w_proper_indent(pp_sex(H)),!.
print_list_as_sexpression([H|T]):- write(' '), pp_sex(H), print_list_as_sexpression(T).

call_sexpr(S):- writeln(call=S).


:- dynamic(fb_pred/2).

full_symbol_count(SL):- flag(total_loaded_atoms,SL,SL),SL>1,!.
full_symbol_count(SL):- findall(NC,(fb_pred(F,A),metta_stats(F,A,NC)),Each), sumlist(Each,SL).

heartbeat :-
    % Get the current time and the last printed time
    get_time(CurrentTime),
    % Check if the global variable is set
    (   nb_current(last_printed_time, _)
    ->  true
    ;   nb_setval(last_printed_time, CurrentTime)
    ),

    nb_getval(last_printed_time, LastPrintedTime),

    % Calculate the difference
    Diff is CurrentTime - LastPrintedTime,

    % If the difference is greater than or equal to 60 seconds (1 minute)
    (   Diff >= 60
    ->  % Print the heartbeat message and update the last printed time
        metta_stats
    ;   % Otherwise, do nothing
        true
    ).

metta_stats:- gc_now,
   writeln('\n\n\n\n\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   writeln('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   full_symbol_count(SL),
   format("~N~n; Total\t\tAtoms (Atomspace size): ~`.t ~D~108|~n",[SL]),
   get_time(CurrentTime), nb_setval(last_printed_time, CurrentTime),
   post_statistic(memory,Mem),
   post_statistic(atom_space,AS),
   post_statistic(cputime,TotalSeconds),
   post_statistic(atoms,Concepts),
   flag(assert_new,CTs,CTs),
   post_statistic(stack,StackMem),


   PM is Mem + StackMem,
   RM is Mem-AS,
   PA is RM//(SL+1),
   APS is 60*floor(SL/(TotalSeconds+1)),
   ACS is AS//(Concepts+1),     

   pl_stats('SymbolAtoms',Concepts),
   pl_stats('Random samples',CTs),
   skip((pl_stats('Bytes Per Atom (Average)',PA), pl_stats('Bytes Per ConceptNode (Average)',ACS))),
   skip((pl_stats('Relational Memory',RM), pl_stats('ConceptNode Memory',AS))),
   %pl_stats('Queryspace Memory',StackMem),
   %CPU is CPUTime-57600,
   format_time(TotalSeconds, Formatted),
   skip((pl_stats('Atoms per minute',APS))),
   pl_stats('Total Memory Used',PM),
   pl_stats('Runtime (days:hh:mm:ss)',Formatted),
   nl,nl,!.
metta_stats(F):- for_all(fb_pred(F,A),metta_stats(F,A)).
metta_stats(F,A):- metta_stats(F,A,NC), pl_stats(F/A,NC).
metta_stats(F,A,NC):- functor(P,F,A),predicate_property(P,number_of_clauses(NC)).
pl_stats(Stat):- statistics(Stat,Value),pl_stats(Stat,Value).
pl_stats(Stat,[Value|_]):- nonvar(Value),!, pl_stats(Stat,Value).
pl_stats(Stat,Value):- format("~N;\t\t~@: ~`.t ~@~100|",[format_value(Stat),format_value(Value)]),!.


% Predicate to print the formatted result.
format_value(Value) :- float(Value),!,format("~2f",[Value]),!.
format_value(Bytes) :- integer(Bytes),format_bytes(Bytes, Formatted), write(Formatted).
format_value(Term)  :- format("~w",[Term]).
%  Base case: If the number is 1G or more, show it in gigabytes (G).
format_bytes(Bytes, Formatted) :-  Bytes >= 1073741824, GB is Bytes / 1073741824, format(string(Formatted), '~2fG', [GB]).
% If the number is less than 1G, show it in megabytes (M).
format_bytes(Bytes, Formatted) :- Bytes >= 104857600, Bytes < 1073741824, !, MB is Bytes / 1048576, D is floor(MB), format(string(Formatted), '~DM', [D]).
% If the number is less than 1K, show it in bytes (B).
format_bytes(Bytes, Formatted) :- format(string(Formatted), '~D', [Bytes]).
% % If the number is less than 1M, show it in kilobytes (K).
%format_bytes(Bytes, Formatted) :- Bytes >= 1024, Bytes < 1048576, !, KB is Bytes / 1024, format(string(Formatted), '~0fK', [KB]).

% Convert total seconds to days, hours, minutes, seconds, and milliseconds.
format_time(TotalSeconds, Formatted) :-
    Seconds is floor(TotalSeconds),
    % Get days, remaining seconds
    Days is div(Seconds, 86400),
    Remain1 is mod(Seconds, 86400)-57600,
    format_time(string(Out),'%T',Remain1),
    % Format the result
    format(string(Formatted), '~w:~w', [Days, Out]).

% Predicate to print the formatted time.
print_formatted_time(TotalSeconds) :-
    format_time(TotalSeconds, Formatted),
    writeln(Formatted).

metta_final:-
    save_pre_statistic(memory),
    save_pre_statistic(atoms),
    save_pre_statistic(atom_space).

