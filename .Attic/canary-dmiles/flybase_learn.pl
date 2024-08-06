
:-
  multifile(table_n_type/3),
  multifile(load_state/2),
  multifile(is_loaded_from_file_count/2),
  multifile(fb_pred_nr/2),
  multifile(fb_arg_type/1),
  multifile(fb_arg_table_n/3),
  multifile(fb_arg/1),
  multifile(done_reading/1).

 :- discontiguous fb_metta_query/1.









%./KBs/SUMO-OBO/gene-merged-SUMO.kif
%
%FBbt_00051628

concept_type(Arg,Type):-
   fb_arg(Arg),
   fb_arg_table_n(Arg,Fn,N),
   table_n_type(Fn,N,Type).

good_concept(E1):- var(E1),!,table_columns(F1,P1),nth1(N1,P1,E2),(E1=E2;E1=nth(N1,F1)).
good_concept(E1):- symbol(E1),!, is_good_symbol_name(E1).
good_concept(E1):- number(E1),!, E1>300.
good_concept(listOf(E1,_)):- good_concept(E1),symbol(E1).
good_concept(listOf(E1)):- good_concept(E1),symbol(E1).

%:- abolish(maybe_corisponds/2).
:- dynamic(maybe_corisponds/2).

is_good_symbol_name(E1):- symbol(E1), symbol_length(E1,L),L>=2, \+ symbol_number(E1,_).

fb_pred_g(F,A):-fb_pred_nr(F,A), \+ skipped_anotations(F), A>0, A<20.

mine_corisponds(Concept1,Corispondance):-
 fb_arg_table_n(Concept1,Fn1,Nth1),is_good_symbol_name(Concept1),
 fb_arg_table_n(Concept1,Fn2,Nth2),
 (Fn1+Nth1)@>(Fn2+Nth2),
 tables_can_join(Fn1,Fn2),
 once((table_colnum_type(Fn1,Nth1,Type1),nonvar(Type1),
       table_colnum_type(Fn2,Nth2,Type2),nonvar(Type2))),
 (maybe_corisponds('ConceptMapFn'(Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Type2,Nth2,Fn2/*Arity2*/))
  = Corispondance).

mine_overlaps:-
  retractall(maybe_corisponds(_,_)),
  time(once(mine_overlaps1)),
  skip(mine_overlaps2).

fbel:- ensure_loaded('flybase.metta.qlf').
mine_overlaps1:-
  for_all(mine_corisponds(Concept1,How), assert_progress(mine_overlaps1(Concept1),How)).

mine_overlaps2_slow:-
 % for_all(mine_typelevel_overlaps,true),
  for_all(mine_symbolspace_overlaps,true).




mine_typelevel_overlaps:-
  for_all(mine_typelevel_overlaps(Concept1,SC1,SC2),
    assert_progress(mine_typelevel_overlaps(Concept1),maybe_corisponds(SC1,SC2))).

tables_can_join(Fn1,Fn2):- Fn1@>Fn2, can_join_using(Fn1),can_join_using(Fn2).

can_join_using(V):- var(V),!.
can_join_using(fbgn_exons2affy1_overlaps):- !, fail.
can_join_using(fbgn_exons2affy2_overlaps):- !, fail.
can_join_using(_).

fb_data_template(T1,Data):-
   fb_pred_g(T1,Arity), Arity>1,Arity<20,functor(Data,T1,Arity),
   current_predicate(T1/Arity).
fb_data(T1,Data):- fb_data_template(T1,Data),call(Data).

:- multifile(fb_arg_table_n/3).
fb_arg_table_n(Arg,Fn,N):- fb_data(Fn,Data),arg(N,Data,Arg).

querymaker2(CrossType,Inst,[Type1,V1],[Type2,V2],Query):-
   xref_class(CrossType),
   table_colnum_type(T1,CN1,CrossType),
   table_colnum_type(T2,CN2,CrossType),
  T1\==T2,
   fb_data_template(T1,Data1),
   fb_data_template(T2,Data2),
   arg(CN1,Data1,Inst),arg(CN2,Data2,Inst),
   once((Data1,Data2,is_good_symbol_name(Inst),

   table_colnum_type(T1,Nth1,Type1),Type1\==CrossType,
   table_colnum_type(T2,Nth2,Type2),Type2\==CrossType,Type1\==Type2,
   arg(Nth1,Data1,V1),
   arg(Nth2,Data2,V2), CN1\==Nth1,CN2\==Nth2)),
   sort([Type1-V1,CrossType-Inst,Type2-V2],Sorted),
   reverse(Sorted,SortedR),
   maplist(arg(1),SortedR,Sorted1),
   maplist(arg(2),SortedR,Sorted2),

   symbolic_list_concat(Sorted1,'-',QPD),
   into_hyphens(QPD,QP),

   Self = '&self',
  Query =
    [match,Self,
       [(','), Data1,Data2],
       [QP| Sorted2]],
  CQuery =
    [match,Self,
       [(','), _CData1,_CData2],
       [QP,CInst, CV1, CV2 ]],
    copy_term(Query,CQuery),
      atom_concat(Type2,'_2',Type22),
       CV1 = '$VAR'(Type1), CV2 = '$VAR'(Type22), CInst = '$VAR'(CrossType),
       numbervars(CQuery,0,_,[]),
       nl,
       format('~n~n;;; ~w~n~n',[QP]),
        write_exec(CQuery),nl,nl,
        \+ \+ ((once((Data1,Data2)),write_src(Data1),
                         if_t((Data1\==Data2),(nl, write_src(Data2))))),
        nl,nl.


querymaker:-
  forall(querymaker(CrossType,Inst,[Type1,V1],[Type2,V2],Query),
      write_src(querymaker(CrossType,Inst,[Type1,V1],[Type2,V2],Query))).
querymaker(CrossType,Inst,[Type1,V1],[Type2,V2],Query):-
   xref_class(CrossType),
  fb_data_template(T1,Data1),
  fb_data_template(T2,Data2),
  T1\==T2,
   table_colnum_type(T1,CN1,CrossType),
   table_colnum_type(T2,CN2,CrossType),
   table_colnum_type(T1,Nth1,Type1),Type1\==CrossType,
   table_colnum_type(T2,Nth2,Type2),Type2\==CrossType,Type1\==Type2,
   arg(Nth1,Data1,V1),arg(CN1,Data1,Inst),
   arg(Nth2,Data2,V2),arg(CN2,Data2,Inst),
   sort([Type1-V1,CrossType-Inst,Type2-V2],Sorted),
   reverse(Sorted,SortedR),
   maplist(arg(1),SortedR,Sorted1),
   maplist(arg(2),SortedR,Sorted2),
   symbolic_list_concat(Sorted1,'-',QPD),
   into_hyphens(QPD,QP),

   Self = '&self',
  Query =
    [match,Self,
       [(','), Data1,Data2],
       [QP|Sorted2]],
  CQuery =
    [match,Self,
       [(','), _CData1,_CData2],
       [QP,CInst, CV1, CV2 ]],
    copy_term(Query,CQuery),
      atom_concat(Type2,'_2',Type22),
       CV1 = '$VAR'(Type1), CV2 = '$VAR'(Type22), CInst = '$VAR'(CrossType),
       numbervars(CQuery,0,_,[]),
       nl,
       format('~n~n;;; ~w~n~n',[QP]),
        write_exec(CQuery),nl,nl,
        \+ \+ ((once((Data1,Data2, Inst\=="")),write_src(Data1),
                         if_t((Data1\==Data2),(nl, write_src(Data2))))),
        nl,nl.

querymaker3(CrossType,Inst,[Type1,V1],[Type2,V2],Query):-


   call_nth(fb_data(T1,Data1),3), arg(CN1,Data1,Inst), is_good_symbol_name(Inst),
   fb_data_template(T2,Data2),T1\==T2,
   arg(CN2,Data2,Inst),
   once(Data2),
   arg(Nth1,Data1,V1), Nth1\==CN1,
   arg(Nth2,Data2,V2), Nth2\==CN2,
   table_colnum_type(T1,CN1,CrossType),
   table_colnum_type(T1,Nth1,Type1),Type1\==CrossType,
   table_colnum_type(T2,Nth2,Type2),Type2\==CrossType,Type1\==Type2,
   sort([Type1,CrossType,Type2],Sorted),
   reverse(Sorted,SortedR),
   symbolic_list_concat(SortedR,'-',QPD),
   into_hyphens(QPD,QP),

   Self = '&self',
  Query =
    [match,Self,
       [(','), Data1,Data2],
       [QP,Inst, V1, V2 ]],

  \+ \+
   ((user:once((Data1,Data2)),

  CQuery =
    [match,Self,
       [(','), _CData1,_CData2],
       [QP,CInst, CV1, CV2 ]],
    copy_term(Query,CQuery),
      atom_concat(Type2,'_2',Type22),
       CV1 = '$VAR'(Type1), CV2 = '$VAR'(Type22), CInst = '$VAR'(CrossType),
       numbervars(CQuery,0,_,[]),
       nl,
       format('~n~n;;; ~w~n~n',[QP]),
        write_exec(CQuery),nl,nl,
         ((write_src(Data1),
                         if_t((Data1\==Data2),(nl, write_src(Data2))))),
        nl,nl)).

interesting_inst(II):- \+ var(II),  \+ number(II), II\=='',II\=="".

same_values(Inst,T1,K1,CN1,T2,K2,CN2):-
   fb_pred_g(T1,Arity1),Arity1>1,Arity1<10,
   fb_pred_g(T2,Arity2),Arity2>1,Arity2<10,
   functor(Data1,T1,Arity1),
   functor(Data2,T2,Arity2),
   arg(CN1,Data1,Inst),
   arg(CN2,Data2,Inst),
   ((T1,CN1)\==(T2,CN2)),
   call_nth((Data1,Data2),Nth),
   (Nth=2->!;true),
   ignore(table_colnum_type(T1,CN1,K1)),
   ignore(table_colnum_type(T2,CN2,K2)).

xref_class(CrossType):- no_repeats(CrossType,rep_xref_class(CrossType)).
fb_class(T):- no_repeats(T,table_colnum_type(_,_,T)).

fb_inst_class(I,IT):- no_repeats(IIT,(fb_data(T,Data),arg(Nth,Data,I),table_colnum_type(T,Nth,IT),(IIT=I+IT))).
rep_xref_class(CrossType):-  table_colnum_type(T1,_,CrossType), table_colnum_type(T2,_,CrossType),T1\==T2.

mine_typelevel_overlaps(Concept1,'ConceptMapFn'(Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Type2,Nth2,Fn2/*Arity2*/)):-

  %fail, % Skip over simple type named things

  Type1=Concept1,Type2=Concept1,
  table_columns(Fn1,Atom1), table_columns(Fn2,Atom2),
  fb_pred_g(Fn1,Arity1), fb_pred_g(Fn2,Arity2),
  Fn1@>Fn2, nth1(Nth1,Atom1,Concept1),
  good_concept(Concept1),
  once((nth1(Nth2,Atom2,Concept1),length(Atom1,Arity1),length(Atom2,Arity2))).

mine_symbolspace_overlaps:-
  fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2),
  once((functor(Atom1,Fn1,Arity1),functor(Atom2,Fn2,Arity2),
  tables_can_join(Fn1,Fn2),
  call(Atom1), arg(Nth1,Atom1,Concept1),good_concept(Concept1), arg(Nth2,Atom2,Concept1),call(Atom2))),
  once((
    table_colnum_type(Fn1,Nth1,Type1),nonvar(Type1),
  table_colnum_type(Fn2,Nth2,Type2),nonvar(Type1))),
  assert_progress(Concept1,maybe_corisponds('ConceptMapFn'(Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Type2,Nth2,Fn2/*Arity2*/))).

mine_unif_overlap:-
  forall((fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2),
  once((functor(Atom1,Fn1,Arity1),functor(Atom2,Fn2,Arity2),
  arg(Nth1,Atom1,Concept1), arg(Nth2,Atom2,Concept1),
  call(Atom1),call(Atom2),
  interesting_to_unify(Concept1)))),

  assert_progress(Concept1,maybe_corisponds('ConceptMapFn'(Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Nth2,Fn2/*Arity2*/)))).

interesting_to_unify(Concept1):- string(Concept1),!,symbol_length(Concept1,L),L>3.
interesting_to_unify(Concept1):- good_concept(Concept1).
interesting_to_unify(Number):- number(Number),Number>1000.


fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2):- !,
  fb_pred_g(Fn1,Arity1), fb_pred_g(Fn2,Arity2),
  tables_can_join(Fn1,Fn2),
  between(1,Arity1,Nth1),Nth1<20,between(1,Arity2,Nth2),Nth2<20,
  (Fn1==Fn2-> (Nth1>Nth2); true).

fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2):-
  fb_pred_g(Fn1,Arity1), fb_pred_g(Fn2,Arity2),Fn1@>Fn2,
  mine_typelevel_overlaps(_,'ConceptMapFn'(_Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(_Type2,Nth2,Fn2/*Arity2*/)).

table_colnum_type(Fn,Nth,Type):- table_n_type(Fn,Nth,TypeC,TypeB),(nonvar(TypeB)->Type=TypeB;Type=TypeC).

synth_conj(QV,(Atom1),(Atom2)):-
  maybe_corisponds('ConceptMapFn'(Type1,Nth1,Fn1),'ConceptMapFn'(Type2,Nth2,Fn2)),
  make_symbol(Fn1,Nth1,Atom1,Arg1),
  make_symbol(Fn2,Nth2,Atom2,Arg2),
  Fn1\=@=Fn2,
  skip(Type1),skip(Type2),
  Arg1=Arg2,QV=Arg1.

synth_query(Len,Query):- synth_query(_,Len,Query).

synth_query(_,1,[Atom]):- !, make_symbol(Atom).
synth_query(QV,N,[Q1,Q2|Query]):-
   M is N -1,
   synth_conj(QV,Q1,Q2),
   (M>1 -> dif(QV,QV2) ; true),
   synth_query(QV2,M,[Q2|Query]),
   all_dif_functors([Q1,Q2|Query]).

all_dif_functors(List):- \+ (select(Q1,List,Rest),member(Q2,Rest),functor(Q1,F1,_),functor(Q2,F2,_), F1==F2, \+ (ok_if_dupped(F1))).

make_symbol(Atom):- fb_pred_g(F,A),functor(Atom,F,A).
make_symbol(Fn,Nth,Atom,Arg):- fb_pred_g(Fn,Arity),functor(Atom,Fn,Arity),arg(Nth,Atom,Arg).

ok_if_dupped(best_gene_summary).

try_overlaps:- try_overlaps(5).

try_overlaps(N):-
  synth_query(N,Query),
  \+ \+ (call_match(Query),
         pp_fb(grounded=Query),
         ignore(maybe_english(Query))),nl,nl,
  AQ = [','|Query],
  pp_fb('!'(match('&flybase',AQ,AQ))),nl,nl,nl.

no_english(fbrf_pmid_pmcid_doi,_).
no_english(physical_interactions_mitab,8).

maybe_english(Query):-
         extract_concepts(Query,Concepts),!,
         ignore((maybe_english(Query,Concepts))),!.

maybe_english(_Query,Concepts):- select(C,Concepts,Rest),is_englishy(C),member(C2,Rest),is_englishy(C2),!, pp_fb(english=[C,C2]).
maybe_english(_Query,Concepts):- pp_fb(concepts=Concepts), maplist(some_english,Concepts).

is_englishy(C):- \+ symbol(C), \+ string(C), !, fail.
is_englishy(C):- split_string(C, ". ", " ", [_,_,_|_]).
is_englishy(C):- symbol_contains(C,". ").

some_english(Term):-
  ignore((fb_arg_table_n(C,Fn1,Nth1), \+ no_english(Fn1,Nth1),is_englishy(C),
  make_symbol(Fn1,Nth1,Atom,English),
  arg(Nth2,Atom,Term),Nth2\==Nth1,
  call(Atom),English\=='',!,
  pp_fb(Term=English))).

extract_concepts(Query,Concepts):-
   findall(C,(sub_term(C,Query),symbolic(C),good_concept(C)),L),
   predsort(longest_first,L,Concepts).

longest_first(R,A,B):- into_len(A,L1),into_len(B,L2),compare(R,L2,L1).
into_len(A,0):- var(A),!.
into_len(A,L):- \+ string(A), !, sformat(S,"~w",[A]),into_len(S,L).
into_len(A,0+A):- symbol_contains(A," ").
into_len(A,L+A):- symbol_length(A,L1), (L1 == 11 -> L = 0 ; L is - L1).

assert_progress(Concept,Atom):- Atom=..[OP,A1,A2], A1@>A2,!,AtomSwp=..[OP,A2,A1],!,assert_progress(Concept,AtomSwp).
assert_progress(Concept,Atom):- call(Atom),!,pp_fb(already(Concept)=Atom).
assert_progress(Concept,Atom):- pp_fb(assert_progress(Concept)=Atom),assert(Atom).



pfb:-
  setenv('DISPLAY','10.0.0.122:0.0'),
  profile(load_flybase_tiny).

pfb1:-
  setenv('DISPLAY','10.0.0.122:0.0'),
  profile(load_flybase(100_000)).

pfb2:-
  setenv('DISPLAY','10.0.0.122:0.0'),
  profile(load_flybase(1_000_000)).

pfb3:-
  setenv('DISPLAY','10.0.0.122:0.0'),
  profile(load_flybase_full).


% Convert a function and its arguments into a compound term
into_datum(Fn, [D|DataL], Data):-
    (nb_current(pred_va, 'True') -> Data =.. [Fn,D,DataL]; Data =.. [Fn,D|DataL]).

% Create a new assertion from old data
make_assertion4(Fn, Cols, NewData, OldData):- fail,
  OldData=Cols,
  NewData =..[Fn|Cols],!.
make_assertion4(Fn, Cols, NewData, OldData):-
    into_datum(Fn, Cols, OldData),
    OldData =.. [Fn|Args],
    % skip(if_t(var(ArgTypes), must_det_ll_r((once((length(Args,Len),length(ArgTypes,Len),once((table_columns(Fn,ArgTypes);table_columns(F,ArgTypes))))))))),
    maybe_fix_args(Fn, Args, NewArgs),
    maybe_sample(Fn, NewArgs),
    NewData =.. [Fn|NewArgs], !.

maybe_fix_args( Fn,Args,NewArgs):- do_fix_fast_args( Fn,1,Args,NewArgs),!.
maybe_fix_args( Fn,Args,NewArgs):- should_fix_args,
  nb_current(fb_argtypes,ArgTypes), fix_list_args(Fn,ArgTypes,Args,NewArgs),!.
maybe_fix_args(_Fn,Args,Args).

do_fix_fast_args( Fn,Nth,[A|Args],[New|NewArgs]):- maybe_fix_columns_nth(Fn,Nth,A,New),
  Nth2 is Nth+1, !, do_fix_fast_args( Fn,Nth2,Args,NewArgs).
do_fix_fast_args(_,_,A,A).

maybe_fix_columns_nth(Fn,Nth,A,New):- fix_columns_nth(Fn,Nth), fix_concept(A,New),!.
maybe_fix_columns_nth(_,_,A,A).


cleanup_arities:- for_all((fb_pred_nr(F,2),fb_pred_nr(F,N),N>2),retract(fb_pred(F,2))).




:- discontiguous column_names_ext/2.
:- discontiguous primary_column/2.

must_det_ll_r((G1,G2)):- !, once(G1),must_det_ll_r(G2).
must_det_ll_r(G):- call(G).

% Safely executes the given Goal and prints any exception raised.
% Usage: safe(+Goal, +Info).
safe(Goal, Info) :-
    % Try to call Goal. If an exception is raised, unify Exception with the exception.
    catch(Goal, Exception,
        % If an exception is raised, portray the clause (Info :- Goal)
        % along with the exception, then rethrow the exception.
        (catch_ignore(portray_clause(exception:Exception:(Info:- Goal))), throw(Exception))
    ).
% Safely executes the given Goal and prints any exception raised.
% Usage: safe(+Goal).
safe(Goal) :- safe(Goal,safe/1).


skipped_anotations(fbgn_exons2affy1_overlaps).
skipped_anotations(fbgn_exons2affy2_overlaps).
skipped_anotations(gene_rpkm_matrix).
skipped_anotations(dmel_gene_sequence_ontology_annotations).
%kipped_anotations(fbgn_annotation_ID).
skipped_anotations(transposon_sequence_set).

gc_now:- set_option_value(gc,true), garbage_collect,garbage_collect_atoms,garbage_collect_clauses.

extreme_debug(_).

numbervars_w_singles(P):- term_singletons(P, Vars),
  numbervars(Vars,260,_,[attvar(bind),singletons(false)]),
  numbervars(P,14,_,[attvar(bind),singletons(true)]).



pp_fb(P):- format("~N "),  \+ \+ (numbervars_w_singles(P), pp_fb1(P)),flush_output.
pp_fb1(P):- write_src(P),!,nl.
:- if(current_predicate(pp_ilp/1)).
pp_fb1(P):- pp_as(P),!,format("~N"),pp_ilp(P),!.
:- endif.
pp_fb1(P):- pp_as(P),!.
pp_fb1(P):- print(P),!,nl.
pp_fb1(P):- fbdebug1(P),!,nl.


fbgn_exons2affy1_overlaps_each(Gene,At):-
   fb_pred_nr(fbgn_exons2affy1_overlaps, Arity),
   functor(Pred,fbgn_exons2affy1_overlaps, Arity),
   arg(1,Pred,Gene),
   call(Pred),
   arg(N,Pred,At),N>1.

fbgn_exons2affy1_overlaps_start_end(Gene,Start,End):-
   fbgn_exons2affy1_overlaps_each(Gene,At),into_start_end(At,Start,End).


into_start_end(s_e(S,E),S,E):- nonvar(S),!.
into_start_end('..'(S,E),S,E):- nonvar(S),!.
into_start_end(at(S,E),S,E):- nonvar(S),!.
into_start_end(At,S,E):- symbolic_list_concat([SS,EE],'..',At),
   into_number_or_symbol(SS,S), into_number_or_symbol(EE,E).
into_start_end(At,S,E):- symbolic_list_concat([SS,EE],'_at_',At),
   into_number_or_symbol(SS,S), into_number_or_symbol(EE,E).


%into_fb_term(Atom,Term):- compound(Atom),!,Term=Atom.
into_fb_term(Atom,Term):- \+ atom(Atom), \+ string(Atom),!,Term=Atom.
into_fb_term(Atom,'..'(S,E)):- into_start_end(Atom,S,E),!.
into_fb_term(Atom,Term):- into_number_or_symbol(Atom,Term),!.

fb_member(E,L):- as_list([],L,LL),member(E,LL).

into_number_or_symbol(Atom,Term):- symbolic_list_concat(List,'|',Atom),List\=[_],!,maplist(into_fb_term,List,Term).
%into_number_or_symbol(Atom,Term):- atom_number(Atom, Term),!,Term= Term.
into_number_or_symbol(Atom,Term):- catch(atom_to_term(Atom,Term,Vars),_,fail),maplist(a2t_assign_var,Vars).
into_number_or_symbol(Atom,Term):- Term=Atom.

a2t_assign_var(N=V):- N=V.

fbgn_exons2affy2_overlaps_each(Gene,At):-
   fb_pred_nr(fbgn_exons2affy2_overlaps, Arity),
   functor(Pred,fbgn_exons2affy2_overlaps, Arity),
   arg(1,Pred,Gene),
   call(Pred),
   arg(N,Pred,At),N>1.

fbgn_exons2affy2_overlaps_start_end(Gene,Start,End):-
   fbgn_exons2affy2_overlaps_each(Gene,At),into_start_end(At,Start,End).

some_xref_ids(Id):- member(Id,['FBgn0001301']).

findall_flat_set(Arg,Goal,FlatSet):-
  findall(Arg,Goal,List),flatten(List,Flat),list_to_set(Flat,FlatSet),!.

expand_xref(Id,N,SetOfArgs):-
  expand_xref_excpt([[]],Id,N,SetOfArgs).

expand_xref_excpt(_Xcept,Id,_N,SetOfArgs):- compound(Id),!,SetOfArgs=[].
expand_xref_excpt(Except,Id,N,SetOfArgs):- var(N),!,between(0,5,N),expand_xref_excpt(Except,Id,N,SetOfArgs).
expand_xref_excpt(Except,Id,N,SetOfArgs):- N=<1,!,
  findall_flat_set(SoFar,expand_xref_once_except(Except,Id,N,SoFar),SetOfArgs).
expand_xref_excpt(Except,Id,N,SetOfArgs):- Nm1 is N -1,
  expand_xref_once_except(Except,Id,1,SetOfArgs1),
  findall_flat_set(EArgs,(member(E,SetOfArgs1),expand_xref_excpt([Id|Except],E,Nm1,EArgs)),SetOfArgs).

gather_args(Except,F,Pred,Args):- findall_flat_set(Arg,gather_args(Except,F,Pred,Arg),Args).
gather_each_args(Except,F,Pred,Ele):- arg(N,Pred,Arg), \+ member(Arg,Except),
   (number(Arg)-> Ele = is_nthOf(Arg,F,N) ; Ele = Arg).

%   findall_flat_set([Pred|Args],
%    (call(Pred),
%     (N=0 -> Args = [] ; gather_args([Id|Except],F,Pred,Args))),SetOfArgs).


expand_xref_once_except(Except,Id,P1):- nonvar(P1), \+ is_list(P1),
  forall(between(1,6,N),
     expand_xref_once_except_each(Except,Id,N,P1)).

expand_xref_once_except(Except,Id,Set):-
  ((between(1,6,N),expand_xref_once_except_each(Except,Id,N,nop),fail)
    ->true;Set=Except).

expand_xref_once_except_each(Except,Id,N,P1):-
  fb_pred_nr(F, Arity),
  xgc,
  \+ member(argNOf(N,F/Arity),Except),   \+ member(F/Arity,Except),
  Arity>=N,
  expand_xref_once_except_each_fa(Except,F,Arity,Id,N,P1).


expand_xref_once_except_each_fa(Except,F,Arity,Id,N,P1):-
  functor(Pred,F, Arity),
  arg(N,Pred,Id),
  call(Pred),
  add_to_except(argNOf(N,F/Arity),Except),
  % \+ member(Pred,Except),
  % add_to_except(Pred,Except),
  call(P1,Pred),
  xgc.

xgc:-
  garbage_collect,
  garbage_collect_atoms,
  garbage_collect_clauses,
  sleep(0.033).

add_to_except(Pred,Except):- arg(2,Except,T), nb_setarg(2,Except,[Pred|T]).

sx1:- xinfo(_Id).

xinfo(Id):- var(Id),!,some_xref_ids(Id), xinfo(Id).
xinfo(Id):- Id=='',!.
xinfo(Id):- number(Id),!.
xinfo(Id):- expand_xref_once_except([Id],Id,my_write_src_nl).

my_write_src_nl(X):-!, write_src_nl(X).
my_write_src_nl(X):-
  must_det_ll((X=..[F|L], maplist(fast_column,L,LL),!,write_src_nl([F|LL]))).

/*

xinfo(Id, N):- var(Id),!,some_xref_ids(Id), xinfo(Id,N).
xinfo(Id,N):-
   expand_xref(Id,N,Args),
   maplist(write_src_nl,Args).


xinfo(Id):-
  expand_xref_once_except(
       [argNOf(1,entity_publication/4),
        'obo-is-a'/2,
        'obo-synonym'/4,
        'obo-charge'/2,
        fbal_to_fbgn/4,
        gene_genetic_interactions/6,
        fbgn_gleanr/4],Id,Set),
  nop(maplist(write_src_nl,Set)),fail.
xinfo(Id):-
   N=1,
   forall(member(F/A,
       [entity_publication/4,
        'obo-is-a'/2,
        'obo-charge'/2,
        gene_genetic_interactions/6,
        fbgn_gleanr/4]),
   expand_xref_once_except_each_fa([[]],F,A,Id,N,_Pred)).
*/
:- dynamic fb_tsv_pred_stats/3.

fb_tsv_pred_stats('num-columns', allele_genetic_interactions, [4]).
fb_tsv_pred_stats('duplicated-rows', allele_genetic_interactions, [21]).
fb_tsv_pred_stats('total-rows', allele_genetic_interactions, [363452]).
fb_tsv_pred_stats('unique-values', allele_genetic_interactions, [1, 28688, object]).
fb_tsv_pred_stats('unique-values', allele_genetic_interactions, [2, 28685, object]).
fb_tsv_pred_stats('unique-values', allele_genetic_interactions, [3, 227000, object]).
fb_tsv_pred_stats('unique-values', allele_genetic_interactions, [4, 8034, object]).
fb_tsv_pred_stats('missing-values', allele_genetic_interactions, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', allele_genetic_interactions, [2, '', 19]).
fb_tsv_pred_stats('missing-values', allele_genetic_interactions, [2, 19, [#, ''], [#, 19]]).
fb_tsv_pred_stats('null-value-count', allele_genetic_interactions, [3, '', 19]).
fb_tsv_pred_stats('missing-values', allele_genetic_interactions, [3, 19, [#, ''], [#, 19]]).
fb_tsv_pred_stats('null-value-count', allele_genetic_interactions, [4, '', 38]).
fb_tsv_pred_stats('missing-values', allele_genetic_interactions, [4, 38, [#, ''], [#, 38]]).
fb_tsv_pred_stats('most-frequent', allele_genetic_interactions, [1, [#, [#, 'Scer\\GAL4[hs.2sev]', 1793], [#, 'Scer\\GAL4[elav.PU]', 1893], [#, 'Scer\\GAL4[Bx-MS1096]', 1990], [#, 'Scer\\GAL4[ey.PH]', 2227], [#, 'Scer\\GAL4[elav-C155]', 2526], [#, 'Scer\\GAL4[GMR.PU]', 9205], [#, 'Scer\\GAL4[GMR.PF]', 14267]]]).
fb_tsv_pred_stats('less-frequent', allele_genetic_interactions, [1, [#, [#, 'Su(mg)2-1[2-1]', 1], [#, 'dnt[ap.ME4]', 1], [#, 'Fas2[unspecified]', 1], [#, 'dock[R336Q.UAS]', 1], [#, 'dock[W151K.UAS]', 1], [#, 'dock[W225K.UAS]', 1], [#, 'dock[W48K.UAS]', 1]]]).
fb_tsv_pred_stats('most-frequent', allele_genetic_interactions, [2, [#, [#, 'FBal0042580', 1793], [#, 'FBal0241645', 1893], [#, 'FBal0040476', 1990], [#, 'FBal0093300', 2227], [#, 'FBal0047071', 2526], [#, 'FBal0240667', 9205], [#, 'FBal0244011', 14267]]]).
fb_tsv_pred_stats('less-frequent', allele_genetic_interactions, [2, [#, [#, 'FBal0083171', 1], [#, 'FBal0094549', 1], [#, 'FBal0104139', 1], [#, 'FBal0089352', 1], [#, 'FBal0089350', 1], [#, 'FBal0089348', 1], [#, 'FBal0089346', 1]]]).
fb_tsv_pred_stats('most-frequent', allele_genetic_interactions, [3, [#, [#, 'Scer\\GAL4[Bx-MS1096], mir-286[UAS.cluster.cLc], mir-309[UAS.cluster.cLc], mir-3[UAS.cluster.cLc], mir-4[UAS.cluster.cLc], mir-5[UAS.cluster.cLc], mir-6-1[UAS.cluster.cLc], mir-6-2[UAS.cluster.cLc], mir-6-3[UAS.cluster.cLc] has wing phenotype', 8], [#, 'Scer\\GAL4[Bx-MS1096], mir-286[UAS.cluster.cLc], mir-309[UAS.cluster.cLc], mir-3[UAS.cluster.cLc], mir-4[UAS.cluster.cLc], mir-5[UAS.cluster.cLc], mir-6-1[UAS.cluster.cLc], mir-6-2[UAS.cluster.cLc], mir-6-3[UAS.cluster.cLc] has lethal phenotype', 8], [#, 'Delta[UAS.cDa], Scer\\GAL4[ey.PH], lola[GS88A8], psq[GS88A8] has neoplasia phenotype', 9], [#, 'rho[ve-1], vn[1] has wing vein phenotype', 10], [#, 'Dp[GMR.PD], E2f1[GMR.PD] has eye phenotype', 10], [#, 'Delta[UAS.cDa], Scer\\GAL4[ey.PH], lola[GS88A8], psq[GS88A8] has eye phenotype', 18]]]).
fb_tsv_pred_stats('less-frequent', allele_genetic_interactions, [3, [#, [#, '1038[1038] is an enhancer of visible phenotype of vg[83b27]/vg[1]', 1], [#, 'bas[1] is an enhancer of paralytic phenotype of para[bss1]', 1], [#, 'baz[4] has abnormal cell polarity phenotype, enhanceable by Dhc64C[6-6]/Dhc64C[6-8]', 1], [#, 'baz[4] has abnormal neuroanatomy phenotype, non-enhanceable by aPKC[Exc55]', 1], [#, 'baz[4] has abnormal neuroanatomy phenotype, non-suppressible by aPKC[Exc55]', 1], [#, 'baz[4] has bouton phenotype, non-enhanceable by aPKC[Exc55]', 1], [#, 'baz[4] has bouton phenotype, non-suppressible by aPKC[Exc55]', 1]]]).
fb_tsv_pred_stats('most-frequent', allele_genetic_interactions, [4, [#, [#, 'FBrf0179402', 1369], [#, 'FBrf0237532', 1506], [#, 'FBrf0147055', 1723], [#, 'FBrf0246190', 1852], [#, 'FBrf0190765', 1868], [#, 'FBrf0199094', 1940], [#, 'FBrf0187664', 2514]]]).
fb_tsv_pred_stats('less-frequent', allele_genetic_interactions, [4, [#, [#, 'FBrf0158937', 1], [#, 'FBrf0159209', 1], [#, 'FBrf0192307', 1], [#, 'FBrf0226217', 1], [#, 'FBrf0160985', 1], [#, 'FBrf0134563', 1], [#, 'FBrf0235180', 1]]]).
fb_tsv_pred_stats('num-columns', fbal_to_fbgn, [4]).
fb_tsv_pred_stats('duplicated-rows', fbal_to_fbgn, [0]).
fb_tsv_pred_stats('total-rows', fbal_to_fbgn, [288468]).
fb_tsv_pred_stats('unique-values', fbal_to_fbgn, [1, 288468, object]).
fb_tsv_pred_stats('unique-values', fbal_to_fbgn, [2, 288454, object]).
fb_tsv_pred_stats('unique-values', fbal_to_fbgn, [3, 33136, object]).
fb_tsv_pred_stats('unique-values', fbal_to_fbgn, [4, 33136, object]).
fb_tsv_pred_stats('missing-values', fbal_to_fbgn, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbal_to_fbgn, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbal_to_fbgn, [3, '', 58]).
fb_tsv_pred_stats('missing-values', fbal_to_fbgn, [3, 58, [#, ''], [#, 58]]).
fb_tsv_pred_stats('null-value-count', fbal_to_fbgn, [4, '', 58]).
fb_tsv_pred_stats('missing-values', fbal_to_fbgn, [4, 58, [#, ''], [#, 58]]).
fb_tsv_pred_stats('most-frequent', fbal_to_fbgn, [2, [#, [#, 'abd-A[HCJ32', 2], [#, 'Ecol\\lacZ[HCJ32', 2], [#, 'Ecol\\lacZ[Dora-PL29', 2], [#, 'abd-A[HC184B-HCJ32', 2], [#, 'Mhc[', 2], [#, 'y[2', 3], [#, 'y[1', 5]]]).
fb_tsv_pred_stats('less-frequent', fbal_to_fbgn, [2, [#, [#, 'Xrp1[142]', 1], [#, 'Apc2[45]', 1], [#, 'Apc2[5-3]', 1], [#, 'Apc2[Delta20rep.Tag:HA]', 1], [#, 'TBC1d7[Apc2-79]', 1], [#, 'mRpS24[Apc2-79]', 1], [#, 'mRpS24[+t3.78]', 1]]]).
fb_tsv_pred_stats('most-frequent', fbal_to_fbgn, [3, [#, [#, 'FBgn0004034', 989], [#, 'FBgn0003996', 1474], [#, 'FBgn0015250', 2466], [#, 'FBgn0262639', 3340], [#, 'FBgn0014446', 4731], [#, 'FBgn0014447', 12710], [#, 'FBgn0014445', 30381]]]).
fb_tsv_pred_stats('less-frequent', fbal_to_fbgn, [3, [#, [#, 'FBgn0267024', 1], [#, 'FBgn0044109', 1], [#, 'FBgn0018700', 1], [#, 'FBgn0014164', 1], [#, 'FBgn0003537', 1], [#, 'FBgn0002275', 1], [#, 'FBgn0001163', 1]]]).
fb_tsv_pred_stats('most-frequent', fbal_to_fbgn, [4, [#, [#, y, 989], [#, w, 1474], [#, 'Ecol\\lexA', 2466], [#, 'Hsap\\RELA', 3340], [#, 'Avic\\GFP', 4731], [#, 'Ecol\\lacZ', 12710], [#, 'Scer\\GAL4', 30381]]]).
fb_tsv_pred_stats('less-frequent', fbal_to_fbgn, [4, [#, [#, 'asRNA:CR45468', 1], [#, 'E(rst)C383', 1], [#, 'Dpau\\v', 1], [#, fez, 1], [#, 'Stp-1', 1], [#, 'l(3)70Ac', 1], [#, 'GustR', 1]]]).
fb_tsv_pred_stats('num-columns', genotype_phenotype_data, [7]).
fb_tsv_pred_stats('duplicated-rows', genotype_phenotype_data, [1032]).
fb_tsv_pred_stats('total-rows', genotype_phenotype_data, [384206]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [1, 155953, object]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [2, 155919, object]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [3, 3603, object]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [4, 3608, object]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [5, 1340, object]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [6, 1348, object]).
fb_tsv_pred_stats('unique-values', genotype_phenotype_data, [7, 19007, object]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', genotype_phenotype_data, [2, '', 321]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [2, 321, [#, ''], [#, 321]]).
fb_tsv_pred_stats('null-value-count', genotype_phenotype_data, [3, '', 321]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [3, 321, [#, ''], [#, 321]]).
fb_tsv_pred_stats('null-value-count', genotype_phenotype_data, [4, '', 321]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [4, 321, [#, ''], [#, 321]]).
fb_tsv_pred_stats('null-value-count', genotype_phenotype_data, [5, '', 242201]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [5, 242201, [#, ''], [#, 242201]]).
fb_tsv_pred_stats('null-value-count', genotype_phenotype_data, [6, '', 242201]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [6, 242201, [#, ''], [#, 242201]]).
fb_tsv_pred_stats('null-value-count', genotype_phenotype_data, [7, '', 321]).
fb_tsv_pred_stats('missing-values', genotype_phenotype_data, [7, 321, [#, ''], [#, 321]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [1, [#, [#, 'hid[GMR.PG]', 177], [#, 'numb[1]', 190], [#, 'wg[l-17]', 198], [#, 'N[l1N-ts1]', 238], [#, 'y[1', 242], [#, 'N[55e11]', 261], [#, 'Pink1[B9]', 316]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [1, [#, [#, 'Scer\\GAL4[elav.PLu] y[GD1564] Dcr-2[UAS.cDa]', 1], [#, 'Scer\\GAL4[nub-AC-62] Slu7[HMC04054]', 1], [#, 'Scer\\GAL4[nub-AC-62] Sf3b6[HMS02566]', 1], [#, 'Scer\\GAL4[nub-AC-62] Sf3b5[HMS00097]', 1], [#, 'Scer\\GAL4[nub-AC-62] Sf3b1[HMS00055]', 1], [#, 'Scer\\GAL4[nub-AC-62] Sf3a2[HMC03799]', 1], [#, 'Scer\\GAL4[nub-AC-62] Sf3a1[HMS00157]', 1]]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [2, [#, [#, 'FBal0265023', 177], [#, 'FBal0013186', 190], [#, 'FBal0018509', 198], [#, 'FBal0012887', 238], [#, 'FBal0012701', 261], [#, 'FBal0193144', 316]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [2, [#, [#, 'FBal0244778', 1], [#, 'FBal0058766 FBal0150204', 1], [#, 'FBal0058766 FBal0150205', 1], [#, 'FBal0147425 FBal0241645 FBal0299692', 1], [#, 'FBal0058766 FBal0215661', 1], [#, 'FBal0147425 FBal0241645 FBal0290792', 1], [#, 'FBal0058766 FBal0277700', 1]]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [3, [#, [#, 'lethal - all die during embryonic stage', 5395], [#, eye, 8272], [#, 'abnormal neuroanatomy', 9153], [#, wing, 10701], [#, visible, 24435], [#, lethal, 35360], [#, viable, 47447]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [3, [#, [#, 'rough endoplasmic reticulum membrane', 1], [#, 'muscle cell of basalar muscle 49', 1], [#, 'gonadal sheath', 1], [#, 'visceral muscle cell', 1], [#, 'A1-7 ventral acute muscle cell', 1], [#, 'pupal s-LNv neuron', 1], [#, 'larval s-LNv neuron', 1]]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [4, [#, [#, 'FBcv0002033', 5395], [#, 'FBbt00004508', 8272], [#, 'FBcv0000435', 9153], [#, 'FBbt00004729', 10701], [#, 'FBcv0000354', 24435], [#, 'FBcv0000351', 35360], [#, 'FBcv0000349', 47447]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [4, [#, [#, 'FBbt00001314', 1], [#, 'FBbt00110023', 1], [#, 'FBbt00110022', 1], [#, 'FBbt00002511', 1], [#, 'FBbt00067044', 1], [#, 'FBbt00067062', 1], [#, 'FBbt00017021', 1]]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [5, [#, [#, 'embryonic stage', 4459], [#, 'somatic clone', 8931], [#, 'larval stage', 9821], [#, 'third instar larval stage', 9888], [#, 'adult stage', 16290], [#, recessive, 40476]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [5, [#, [#, 'germline clone|embryonic stage 13', 1], [#, 'somatic clone - tissue specific|decreased number', 1], [#, 'oogenesis|decreased number|germline clone', 1], [#, 'germline clone|decreased number', 1], [#, 'recessive|first instar larval stage|maternal effect', 1], [#, 'maternal effect|second instar larval stage|recessive', 1], [#, 'larval stage|somatic clone|cell autonomous|decreased number', 1]]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [6, [#, [#, 'FBdv00005289', 4459], [#, 'FBcv0000336', 8931], [#, 'FBdv00005336', 9821], [#, 'FBdv00005339', 9888], [#, 'FBdv00005369', 16290], [#, 'FBcv0000298', 40476]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [6, [#, [#, 'FBcv0000732', 1], [#, 'FBcv0000333|FBdv00005369|FBcv0000793', 1], [#, 'FBcv0000153|FBcv0000068', 1], [#, 'FBcv0000058|FBcv0000061', 1], [#, 'FBcv0000336|FBdv00005339|FBcv0000061', 1], [#, 'FBcv0007050|FBdv00007001|FBcv0000068', 1], [#, 'FBcv0007048|FBdv00005338', 1]]]).
fb_tsv_pred_stats('most-frequent', genotype_phenotype_data, [7, [#, [#, 'FBrf0132177', 2452], [#, 'FBrf0179797', 2682], [#, 'FBrf0210524', 7345], [#, 'FBrf0212295', 11407], [#, 'FBrf0210226', 11838], [#, unattributed, 15059], [#, 'FBrf0214518', 19955]]]).
fb_tsv_pred_stats('less-frequent', genotype_phenotype_data, [7, [#, [#, 'FBrf0189827', 1], [#, 'FBrf0179942', 1], [#, 'FBrf0206908', 1], [#, 'FBrf0183397', 1], [#, 'FBrf0023153', 1], [#, 'FBrf0221688', 1], [#, 'FBrf0237982', 1]]]).
fb_tsv_pred_stats('num-columns', cDNA_clone_data, [6]).
fb_tsv_pred_stats('duplicated-rows', cDNA_clone_data, [0]).
fb_tsv_pred_stats('total-rows', cDNA_clone_data, [722570]).
fb_tsv_pred_stats('unique-values', cDNA_clone_data, [1, 722570, object]).
fb_tsv_pred_stats('unique-values', cDNA_clone_data, [2, 4, object]).
fb_tsv_pred_stats('unique-values', cDNA_clone_data, [3, 722570, object]).
fb_tsv_pred_stats('unique-values', cDNA_clone_data, [4, 50, object]).
fb_tsv_pred_stats('unique-values', cDNA_clone_data, [5, 22711, object]).
fb_tsv_pred_stats('unique-values', cDNA_clone_data, [6, 696400, object]).
fb_tsv_pred_stats('missing-values', cDNA_clone_data, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', cDNA_clone_data, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', cDNA_clone_data, [3, unknown, 1]).
fb_tsv_pred_stats('missing-values', cDNA_clone_data, [3, 1, [#, unknown], [#, 1]]).
fb_tsv_pred_stats('null-value-count', cDNA_clone_data, [4, '', 354]).
fb_tsv_pred_stats('missing-values', cDNA_clone_data, [4, 354, [#, ''], [#, 354]]).
fb_tsv_pred_stats('null-value-count', cDNA_clone_data, [5, '', 696444]).
fb_tsv_pred_stats('missing-values', cDNA_clone_data, [5, 696444, [#, ''], [#, 696444]]).
fb_tsv_pred_stats('null-value-count', cDNA_clone_data, [6, '', 26168]).
fb_tsv_pred_stats('missing-values', cDNA_clone_data, [6, 26168, [#, ''], [#, 26168]]).
fb_tsv_pred_stats('most-frequent', cDNA_clone_data, [2, [#, [#, 'Dsim', 239], [#, 'Dyak', 426], [#, 'Dpse', 13648], [#, 'Dmel', 708257]]]).
fb_tsv_pred_stats('less-frequent', cDNA_clone_data, [2, [#, [#, 'Dsim', 239], [#, 'Dyak', 426], [#, 'Dpse', 13648], [#, 'Dmel', 708257]]]).
fb_tsv_pred_stats('most-frequent', cDNA_clone_data, [4, [#, [#, 'SD_cDNA', 18885], [#, 'GH_cDNA', 20768], [#, 'AT_cDNA', 22564], [#, 'RH_cDNA', 52475], [#, 'RE_cDNA', 58036], [#, 'EK_EP_cDNA', 161904], [#, 'RP_cDNA', 220083]]]).
fb_tsv_pred_stats('less-frequent', cDNA_clone_data, [4, [#, [#, 'TB_cDNA', 3], [#, 'TA_cDNA', 36], [#, 'DE-ORESTES_cDNA', 61], [#, 'CB_cDNA', 61], [#, 'AM_cDNA', 84], [#, 'ST_cDNA', 85], [#, 'RT_pCR8_cDNA', 156]]]).
fb_tsv_pred_stats('most-frequent', cDNA_clone_data, [5, [#, [#, 'AY069671', 2], [#, 'AY051957', 2], [#, 'AY061414', 2], [#, 'AY069707', 2], [#, 'AY051914', 2], [#, 'AY231748', 3]]]).
fb_tsv_pred_stats('less-frequent', cDNA_clone_data, [5, [#, [#, 'BT150453', 1], [#, 'BT050556', 1], [#, 'BT010219', 1], [#, 'BT125004', 1], [#, 'BT006005', 1], [#, 'BT099857', 1], [#, 'BT100119', 1]]]).
fb_tsv_pred_stats('most-frequent', cDNA_clone_data, [6, [#, [#, 'GH897620', 1], [#, 'GH934971', 1], [#, 'CA807794', 1], [#, 'AI256966', 2], [#, 'BI636404', 2], [#, 'BI633401', 2]]]).
fb_tsv_pred_stats('less-frequent', cDNA_clone_data, [6, [#, [#, 'CA807794', 1], [#, 'GH872553', 1], [#, 'GH881842', 1], [#, 'GH911192', 1], [#, 'GH817175', 1], [#, 'GH899993', 1], [#, 'GH918491', 1]]]).
fb_tsv_pred_stats('num-columns', genomic_clone_data, [4]).
fb_tsv_pred_stats('duplicated-rows', genomic_clone_data, [0]).
fb_tsv_pred_stats('total-rows', genomic_clone_data, [50400]).
fb_tsv_pred_stats('unique-values', genomic_clone_data, [1, 50400, object]).
fb_tsv_pred_stats('unique-values', genomic_clone_data, [2, 1, object]).
fb_tsv_pred_stats('unique-values', genomic_clone_data, [3, 50400, object]).
fb_tsv_pred_stats('unique-values', genomic_clone_data, [4, 970, object]).
fb_tsv_pred_stats('missing-values', genomic_clone_data, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', genomic_clone_data, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', genomic_clone_data, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', genomic_clone_data, [4, '', 49428]).
fb_tsv_pred_stats('missing-values', genomic_clone_data, [4, 49428, [#, ''], [#, 49428]]).
fb_tsv_pred_stats('most-frequent', genomic_clone_data, [2, [#, [#, 'Dmel', 50400]]]).
fb_tsv_pred_stats('less-frequent', genomic_clone_data, [2, [#, [#, 'Dmel', 50400]]]).
fb_tsv_pred_stats('most-frequent', genomic_clone_data, [4, [#, [#, 'AC104703', 1], [#, 'AC010031', 1], [#, 'AC069405', 1], [#, 'AC011662', 2], [#, 'AC098575', 2], [#, 'AC009888', 2]]]).
fb_tsv_pred_stats('less-frequent', genomic_clone_data, [4, [#, [#, 'AC105774', 1], [#, 'AC091226', 1], [#, 'AC069405', 1], [#, 'AC154046', 1], [#, 'AC010031', 1], [#, 'AC104703', 1], [#, 'AC010573', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_uniprot, [4]).
fb_tsv_pred_stats('duplicated-rows', fbgn_uniprot, [0]).
fb_tsv_pred_stats('total-rows', fbgn_uniprot, [32529]).
fb_tsv_pred_stats('unique-values', fbgn_uniprot, [1, 17130, object]).
fb_tsv_pred_stats('unique-values', fbgn_uniprot, [2, 17130, object]).
fb_tsv_pred_stats('unique-values', fbgn_uniprot, [3, 13964, object]).
fb_tsv_pred_stats('unique-values', fbgn_uniprot, [4, 32341, object]).
fb_tsv_pred_stats('missing-values', fbgn_uniprot, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_uniprot, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_uniprot, [3, '', 4918]).
fb_tsv_pred_stats('missing-values', fbgn_uniprot, [3, 4918, [#, ''], [#, 4918]]).
fb_tsv_pred_stats('missing-values', fbgn_uniprot, [4, 0, [], []]).
fb_tsv_pred_stats('most-frequent', fbgn_uniprot, [1, [#, [#, 'FBgn0265045', 40], [#, 'FBgn0013885', 49], [#, 'FBgn0002022', 50], [#, 'FBgn0285944', 56], [#, 'FBgn0033159', 79], [#, 'FBgn0082496', 101], [#, 'FBgn0003731', 267]]]).
fb_tsv_pred_stats('less-frequent', fbgn_uniprot, [1, [#, [#, 'FBgn0000008', 1], [#, 'FBgn0040211', 1], [#, 'FBgn0040235', 1], [#, 'FBgn0040239', 1], [#, 'FBgn0040250', 1], [#, 'FBgn0040251', 1], [#, 'FBgn0040252', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_uniprot, [2, [#, [#, 'Strn-Mlck', 40], [#, 'Dsub\\Acph-1', 49], [#, 'Catsup', 50], [#, para, 56], [#, 'Dscam1', 79], [#, 'Dpmo\\bi', 101], [#, 'Egfr', 267]]]).
fb_tsv_pred_stats('less-frequent', fbgn_uniprot, [2, [#, [#, a, 1], [#, hgo, 1], [#, 'c12.1', 1], [#, bc10, 1], [#, 'Ugt304A1', 1], [#, 'Ugt302K1', 1], [#, 'Ugt303A1', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_uniprot, [3, [#, [#, 'CG33950', 32], [#, 'CG44162', 40], [#, 'CG10449', 50], [#, 'CG9907', 56], [#, 'CG17800', 79], [#, 'CG10079', 267]]]).
fb_tsv_pred_stats('less-frequent', fbgn_uniprot, [3, [#, [#, 'CG6741', 1], [#, 'CG9913', 1], [#, 'CG3172', 1], [#, 'CG14356', 1], [#, 'CG14355', 1], [#, 'CG9722', 1], [#, 'CG9649', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_uniprot, [4, [#, [#, 'Q7KV12', 5], [#, 'Q5F4J7', 6], [#, 'P02255', 18], [#, 'P84051', 18], [#, 'P02299', 24], [#, 'P02283', 24], [#, 'P84040', 24]]]).
fb_tsv_pred_stats('less-frequent', fbgn_uniprot, [4, [#, [#, 'A0A0B4LG21', 1], [#, 'Q9GST6', 1], [#, 'Q9GST1', 1], [#, 'Q9GN59', 1], [#, 'Q9GNH0', 1], [#, 'Q9GPD9', 1], [#, 'Q9GNH4', 1]]]).
fb_tsv_pred_stats('num-columns', gp_informatio, [2]).
fb_tsv_pred_stats('duplicated-rows', gp_informatio, [8413]).
fb_tsv_pred_stats('total-rows', gp_informatio, [11695]).
fb_tsv_pred_stats('unique-values', gp_informatio, [1, 1885, object]).
fb_tsv_pred_stats('unique-values', gp_informatio, [2, 1851, object]).
fb_tsv_pred_stats('missing-values', gp_informatio, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gp_informatio, [2, 0, [], []]).
fb_tsv_pred_stats('most-frequent', gp_informatio, [1, [#, [#, at, 24], [#, kinase, 35], [#, factor, 47], [#, of, 60], [#, 'P450', 82], [#, receptor, 203], [#, protein, 334]]]).
fb_tsv_pred_stats('less-frequent', gp_informatio, [1, [#, [#, '07/24/2023', 1], [#, 'receptor\\CG30106\\protein\\taxon:7227\\\\UniProtKB:A1ZAX0\\', 1], [#, '1b\\CG6446\\protein\\taxon:7227\\\\UniProtKB:O44253|UniProtKB:Q7KK54\\', 1], [#, '55B\\CG5765\\protein\\taxon:7227\\\\UniProtKB:Q8SXS5|UniProtKB:A1ZB24\\', 1], [#, 'K[+]', 1], [#, shock, 1], [#, 'MCU', 1]]]).
fb_tsv_pred_stats('most-frequent', gp_informatio, [2, [#, [#, '1\\CG17725\\protein\\taxon:7227\\\\UniProtKB:P20007\\', 1], [#, '6\\CG15068\\protein\\taxon:7227\\\\UniProtKB:A1ZB64\\', 1], [#, '2\\CG15067\\protein\\taxon:7227\\\\UniProtKB:Q8SYA7|UniProtKB:A1ZB61\\', 1], [#, '1\\CG15066\\protein\\taxon:7227\\\\UniProtKB:Q9V8F5\\', 1], [#, 'regulator\\CG17680\\protein\\taxon:7227\\\\UniProtKB:Q7JX57\\', 1], [#, 'channel\\CG5076\\protein\\taxon:7227\\\\UniProtKB:Q23974|UniProtKB:A1ZB14|UniProtKB:A0A0B4LGW2\\', 1], [#, _, 1]]]).
fb_tsv_pred_stats('less-frequent', gp_informatio, [2, [#, [#, _, 1], [#, '1\\CG15066\\protein\\taxon:7227\\\\UniProtKB:Q9V8F5\\', 1], [#, '2\\CG15067\\protein\\taxon:7227\\\\UniProtKB:Q8SYA7|UniProtKB:A1ZB61\\', 1], [#, '6\\CG15068\\protein\\taxon:7227\\\\UniProtKB:A1ZB64\\', 1], [#, '1\\CG17725\\protein\\taxon:7227\\\\UniProtKB:P20007\\', 1], [#, '2\\CG10924\\protein\\taxon:7227\\\\UniProtKB:Q7JXB5|UniProtKB:A8DYI3\\', 1], [#, '1\\CG5170\\protein\\taxon:7227\\\\UniProtKB:Q7KN84|UniProtKB:Q95T04|UniProtKB:Q9U982|UniProtKB:Q7KN75\\', 1]]]).
fb_tsv_pred_stats('num-columns', pmid_fbgn_uniprot, [5]).
fb_tsv_pred_stats('duplicated-rows', pmid_fbgn_uniprot, [0]).
fb_tsv_pred_stats('total-rows', pmid_fbgn_uniprot, [2262948]).
fb_tsv_pred_stats('unique-values', pmid_fbgn_uniprot, [1, 57873, object]).
fb_tsv_pred_stats('unique-values', pmid_fbgn_uniprot, [2, 57872, int64]).
fb_tsv_pred_stats('unique-values', pmid_fbgn_uniprot, [3, 146121, object]).
fb_tsv_pred_stats('unique-values', pmid_fbgn_uniprot, [4, 2, object]).
fb_tsv_pred_stats('unique-values', pmid_fbgn_uniprot, [5, 184918, object]).
fb_tsv_pred_stats('missing-values', pmid_fbgn_uniprot, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', pmid_fbgn_uniprot, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', pmid_fbgn_uniprot, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', pmid_fbgn_uniprot, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', pmid_fbgn_uniprot, [5, 0, [], []]).
fb_tsv_pred_stats('most-frequent', pmid_fbgn_uniprot, [1, [#, [#, 'FBrf0244508', 11253], [#, 'FBrf0229340', 12087], [#, 'FBrf0210524', 14897], [#, 'FBrf0212295', 20997], [#, 'FBrf0210226', 21057], [#, 'FBrf0219727', 27601], [#, 'FBrf0200326', 154679]]]).
fb_tsv_pred_stats('less-frequent', pmid_fbgn_uniprot, [1, [#, [#, 'FBrf0207593', 1], [#, 'FBrf0073396', 1], [#, 'FBrf0216372', 1], [#, 'FBrf0050713', 1], [#, 'FBrf0215265', 1], [#, 'FBrf0228014', 1], [#, 'FBrf0084265', 1]]]).
fb_tsv_pred_stats('most-frequent', pmid_fbgn_uniprot, [2, [#, [#, 31722958, 11253], [#, 25312911, 12087], [#, 20371351, 14897], [#, 21074052, 20997], [#, 20220848, 21057], [#, 23071443, 27601], [#, 17994087, 154679]]]).
fb_tsv_pred_stats('less-frequent', pmid_fbgn_uniprot, [2, [#, [#, 19226322, 1], [#, 21035367, 1], [#, 3938362, 1], [#, 7641726, 1], [#, 31992709, 1], [#, 2504633, 1], [#, 7924995, 1]]]).
fb_tsv_pred_stats('most-frequent', pmid_fbgn_uniprot, [3, [#, [#, 'FBgn0003371', 11920], [#, 'FBgn0001624', 12980], [#, 'FBgn0264255', 17080], [#, 'FBgn0004647', 18088], [#, 'FBgn0033159', 22357], [#, 'FBgn0285944', 22904], [#, 'FBgn0003731', 395427]]]).
fb_tsv_pred_stats('less-frequent', pmid_fbgn_uniprot, [3, [#, [#, 'FBgn0182483', 1], [#, 'FBgn0152605', 1], [#, 'FBgn0102225', 1], [#, 'FBgn0191710', 1], [#, 'FBgn0186768', 1], [#, 'FBgn0138504', 1], [#, 'FBgn0147962', 1]]]).
fb_tsv_pred_stats('most-frequent', pmid_fbgn_uniprot, [4, [#, [#, 'UniProt/Swiss-Prot', 394838], [#, 'UniProt/TrEMBL', 1868110]]]).
fb_tsv_pred_stats('less-frequent', pmid_fbgn_uniprot, [4, [#, [#, 'UniProt/Swiss-Prot', 394838], [#, 'UniProt/TrEMBL', 1868110]]]).
fb_tsv_pred_stats('most-frequent', pmid_fbgn_uniprot, [5, [#, [#, 'Q7YSQ1', 2584], [#, 'Q7Z1J1', 2584], [#, 'Q7YSR5', 2584], [#, 'Q7YSU6', 2584], [#, 'M9NE67', 2584], [#, 'P10090', 3804], [#, 'P09615', 4838]]]).
fb_tsv_pred_stats('less-frequent', pmid_fbgn_uniprot, [5, [#, [#, 'B4QI97', 1], [#, 'B4IFF0', 1], [#, 'B3MJC2', 1], [#, 'B4NVB3', 1], [#, 'A0A0Q9WJA5', 1], [#, 'B4H241', 1], [#, 'B3NEU8', 1]]]).
fb_tsv_pred_stats('num-columns', automated_gene_summaries, [2]).
fb_tsv_pred_stats('duplicated-rows', automated_gene_summaries, [0]).
fb_tsv_pred_stats('total-rows', automated_gene_summaries, [237237]).
fb_tsv_pred_stats('unique-values', automated_gene_summaries, [1, 237237, object]).
fb_tsv_pred_stats('unique-values', automated_gene_summaries, [2, 237237, object]).
fb_tsv_pred_stats('missing-values', automated_gene_summaries, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', automated_gene_summaries, [2, 0, [], []]).
fb_tsv_pred_stats('num-columns', best_gene_summary, [4]).
fb_tsv_pred_stats('duplicated-rows', best_gene_summary, [0]).
fb_tsv_pred_stats('total-rows', best_gene_summary, [32142]).
fb_tsv_pred_stats('unique-values', best_gene_summary, [1, 32142, object]).
fb_tsv_pred_stats('unique-values', best_gene_summary, [2, 32142, object]).
fb_tsv_pred_stats('unique-values', best_gene_summary, [3, 5, object]).
fb_tsv_pred_stats('unique-values', best_gene_summary, [4, 29750, object]).
fb_tsv_pred_stats('missing-values', best_gene_summary, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', best_gene_summary, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', best_gene_summary, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', best_gene_summary, [4, '', 21]).
fb_tsv_pred_stats('missing-values', best_gene_summary, [4, 21, [#, ''], [#, 21]]).
fb_tsv_pred_stats('most-frequent', best_gene_summary, [3, [#, [#, 'Interactive Fly', 64], [#, 'UniProtKB', 1737], [#, 'FlyBase Gene Snapshot', 3655], [#, 'Alliance', 8284], [#, 'FlyBase Auto Summary', 18402]]]).
fb_tsv_pred_stats('less-frequent', best_gene_summary, [3, [#, [#, 'Interactive Fly', 64], [#, 'UniProtKB', 1737], [#, 'FlyBase Gene Snapshot', 3655], [#, 'Alliance', 8284], [#, 'FlyBase Auto Summary', 18402]]]).
fb_tsv_pred_stats('most-frequent', best_gene_summary, [4, [#, [#, 'Predicted to enable chitin binding activity. Predicted to be located in extracellular region.', 48], [#, 'Predicted to enable serine-type endopeptidase activity. Predicted to be involved in proteolysis.', 56], [#, 'Is expressed in spermatozoon.', 63], [#, 'Predicted to be involved in RNA processing. Predicted to be located in nucleolus.', 72], [#, 'Predicted to be a structural constituent of ribosome. Predicted to be involved in translation. Predicted to be located in cytosolic ribosome.', 96], [#, 'Is expressed in organism.', 107], [#, 'Is expressed in adult head.', 114]]]).
fb_tsv_pred_stats('less-frequent', best_gene_summary, [4, [#, [#, 'The gene lethal (1) 291-68 is referred to in FlyBase by the symbol Dmel\\l(1)291-68 (FBgn0011177). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is unknown. The biological processes in which it is involved are not known. One allele is reported. No phenotypic data is available. The phenotypic class of allele includes: lethal.', 1], [#, 'The gene E(csw)3B is referred to in FlyBase by the symbol Dmel\\E(csw)3B (FBgn0041256). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is described by: . The biological processes in which it is involved are not known. No alleles are reported.', 1], [#, 'The gene E3 is referred to in FlyBase by the symbol Dmel\\E3 (FBgn0041255). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is unknown. The biological processes in which it is involved are not known. No alleles are reported.', 1], [#, 'The gene H23-14 is referred to in FlyBase by the symbol Dmel\\H23-14 (FBgn0041211). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is unknown. The biological processes in which it is involved are not known. One allele is reported. No phenotypic data is available. The phenotypic class of allele includes: auxotroph.', 1], [#, 'The gene Lipocalin is referred to in FlyBase by the symbol Dmel\\Lipocalin (FBgn0041202). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is unknown. The biological processes in which it is involved are not known. No alleles are reported.', 1], [#, 'The gene prolyl-4-hydroxylase-a related protein is referred to in FlyBase by the symbol Dmel\\PH4\\03b1-RP (FBgn0041197). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is unknown. The biological processes in which it is involved are not known. No alleles are reported.', 1], [#, 'The gene SSP6107 is referred to in FlyBase by the symbol Dmel\\SSP6107 (FBgn0041189). It is a gene from Dmel. Gene has not been localized to the genome sequence. Its molecular function is unknown. The biological processes in which it is involved are not known. No alleles are reported.', 1]]]).
fb_tsv_pred_stats('num-columns', 'Dmel_enzyme_data', [11]).
fb_tsv_pred_stats('duplicated-rows', 'Dmel_enzyme_data', [0]).
fb_tsv_pred_stats('total-rows', 'Dmel_enzyme_data', [3969]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [1, 572, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [2, 572, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [3, 441, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [4, 441, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [5, 281, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [6, 182, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [7, 3736, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [8, 3736, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [9, 2454, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [10, 860, object]).
fb_tsv_pred_stats('unique-values', 'Dmel_enzyme_data', [11, 860, object]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [4, 0, [], []]).
fb_tsv_pred_stats('null-value-count', 'Dmel_enzyme_data', [5, '', 1642]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [5, 1642, [#, ''], [#, 1642]]).
fb_tsv_pred_stats('null-value-count', 'Dmel_enzyme_data', [6, '', 2724]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [6, 2724, [#, ''], [#, 2724]]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [7, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [8, 0, [], []]).
fb_tsv_pred_stats('null-value-count', 'Dmel_enzyme_data', [9, '', 1328]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [9, 1328, [#, ''], [#, 1328]]).
fb_tsv_pred_stats('null-value-count', 'Dmel_enzyme_data', [10, '', 1609]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [10, 1609, [#, ''], [#, 1609]]).
fb_tsv_pred_stats('null-value-count', 'Dmel_enzyme_data', [11, '', 1609]).
fb_tsv_pred_stats('missing-values', 'Dmel_enzyme_data', [11, 1609, [#, ''], [#, 1609]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [1, [#, [#, 'FBgg0001478', 36], [#, 'FBgg0001695', 36], [#, 'FBgg0001207', 45], [#, 'FBgg0001618', 51], [#, 'FBgg0001079', 61], [#, 'FBgg0000128', 98], [#, 'FBgg0001078', 114]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [1, [#, [#, 'FBgg0001510', 1], [#, 'FBgg0001617', 1], [#, 'FBgg0001419', 1], [#, 'FBgg0001420', 1], [#, 'FBgg0001622', 1], [#, 'FBgg0001246', 1], [#, 'FBgg0000869', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [2, [#, [#, 'UNCLASSIFIED RNA ENDONUCLEASES', 36], [#, 'CYTOCHROME P450 - CYP3 CLAN', 36], [#, 'UNCLASSIFIED NON-AMINOACYL ACYLTRANSFERASES', 45], [#, 'ECDYSTEROID KINASE-LIKE', 51], [#, 'S1A SERINE PROTEASES - CHYMOTRYPSIN-LIKE', 61], [#, 'UNCLASSIFIED RING DOMAIN UBIQUITIN LIGASES', 98], [#, 'S1A SERINE PROTEASES - TRYPSIN-LIKE', 114]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [2, [#, [#, 'TRANSGLUTAMINASES', 1], [#, 'BETA-ALANYL-DOPAMINE/HISTAMINE HYDROLASES', 1], [#, 'SITE 2 PEPTIDASES', 1], [#, 'M76 METALLOENDOPEPTIDASES', 1], [#, 'EYA FAMILY PROTEIN TYROSINE PHOSPHATASES', 1], [#, 'DEOXYCYTIDYLTRANSFERASES', 1], [#, 'OXO-ACID-LYASES', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [3, [#, [#, 'GO:0004806', 55], [#, 'GO:0003724', 59], [#, 'GO:0004674', 90], [#, 'GO:0004497', 93], [#, 'GO:0003924', 146], [#, 'GO:0061630', 150], [#, 'GO:0004252', 205]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [3, [#, [#, 'GO:0071566', 1], [#, 'GO:0003810', 1], [#, 'GO:0003923', 1], [#, 'GO:0016642', 1], [#, 'GO:0061863', 1], [#, 'GO:0008970', 1], [#, 'GO:0016872', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [4, [#, [#, 'triglyceride lipase activity', 55], [#, 'RNA helicase activity', 59], [#, 'protein serine/threonine kinase activity', 90], [#, 'monooxygenase activity', 93], [#, 'GTPase activity', 146], [#, 'ubiquitin protein ligase activity', 150], [#, 'serine-type endopeptidase activity', 205]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [4, [#, [#, 'UFM1 activating enzyme activity', 1], [#, 'protein-glutamine gamma-glutamyltransferase activity', 1], [#, 'GPI-anchor transamidase activity', 1], [#, 'oxidoreductase activity, acting on the CH-NH2 group of donors, disulfide as acceptor', 1], [#, 'microtubule plus end polymerase', 1], [#, 'phospholipase A1 activity', 1], [#, 'intramolecular lyase activity', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [5, [#, [#, '3.1.3.16', 47], [#, '3.4.24.-', 47], [#, '3.1.1.3', 55], [#, '3.6.4.13', 59], [#, '2.7.11.1', 90], [#, '3.4.21.-', 209]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [5, [#, [#, '1.16.1.-', 1], [#, '3.4.19.5', 1], [#, '3.2.1.130', 1], [#, '3.5.1.89', 1], [#, '4.3.1.-|4.3.-.-', 1], [#, '7.2.2.9', 1], [#, '4.1.99.22', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [6, [#, [#, 'ubiquitinyl hydrolase 1', 36], [#, 'glutathione transferase', 38], [#, 'protein-serine/threonine phosphatase', 47], [#, 'triacylglycerol lipase', 55], [#, 'RNA helicase', 59], [#, 'non-specific serine/threonine protein kinase', 90]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [6, [#, [#, 'ribonuclease T2', 1], [#, 'tripeptidyl-peptidase I|tripeptidyl-peptidase II', 1], [#, 'phospholipase A1', 1], [#, 'deoxyribonuclease II', 1], [#, 'N-acetylglucosamine-6-phosphate deacetylase', 1], [#, 'molybdopterin molybdotransferase', 1], [#, 'protoporphyrin ferrochelatase', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [7, [#, [#, 'FBgn0028916', 3], [#, 'FBgn0002905', 3], [#, 'FBgn0030731', 3], [#, 'FBgn0011768', 3], [#, 'FBgn0034246', 3], [#, 'FBgn0263831', 4], [#, 'FBgn0010355', 4]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [7, [#, [#, 'FBgn0025186', 1], [#, 'FBgn0043576', 1], [#, 'FBgn0043577', 1], [#, 'FBgn0043578', 1], [#, 'FBgn0029689', 1], [#, 'FBgn0037759', 1], [#, 'FBgn0027538', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [8, [#, [#, 'CG33090', 3], [#, 'PolQ', 3], [#, 'Mfe2', 3], [#, 'Fdh', 3], [#, 'Dcr-2', 3], [#, 'Gen', 4], [#, 'Taf1', 4]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [8, [#, [#, 'ari-2', 1], [#, 'PGRP-SC1a', 1], [#, 'PGRP-SB2', 1], [#, 'PGRP-SB1', 1], [#, 'CG6428', 1], [#, 'CG8526', 1], [#, beta4GalNAcTA, 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [9, [#, [#, rudimentary, 3], [#, 'Trehalose-6-phosphate synthase 1', 3], [#, 'peroxisomal Multifunctional enzyme type 2', 3], [#, sepia, 3], [#, 'XPG-like endonuclease', 4], [#, 'TBP-associated factor 1', 4]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [9, [#, [#, 'ariadne 2', 1], [#, 'Cardiolipin synthase', 1], [#, 'Sphingomyelin synthase related', 1], [#, 'Phosphatidylserine synthase', 1], [#, 'Pyruvate kinase', 1], [#, 'easily shocked', 1], [#, fumble, 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [10, [#, [#, '5.2.1.8', 33], [#, '3.4.19.12', 36], [#, '3.1.3.16', 38], [#, '3.1.1.3', 51], [#, '3.6.4.13', 54], [#, '2.7.11.1', 104]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [10, [#, [#, '2.5.1.16', 1], [#, '2.1.1.62', 1], [#, '2.1.1.56', 1], [#, '2.1.1.386', 1], [#, '2.3.3.10', 1], [#, '2.3.3.8', 1], [#, '2.3.1.168', 1]]]).
fb_tsv_pred_stats('most-frequent', 'Dmel_enzyme_data', [11, [#, [#, 'peptidylprolyl isomerase', 33], [#, 'ubiquitinyl hydrolase 1', 36], [#, 'protein-serine/threonine phosphatase', 38], [#, 'triacylglycerol lipase', 51], [#, 'RNA helicase', 54], [#, 'non-specific serine/threonine protein kinase', 104]]]).
fb_tsv_pred_stats('less-frequent', 'Dmel_enzyme_data', [11, [#, [#, 'spermidine synthase', 1], [#, 'mRNA (2\'-O-methyladenosine-N(6)-)-methyltransferase', 1], [#, 'mRNA (guanine-N(7))-methyltransferase', 1], [#, 'small RNA 2\'-O-methyltransferase', 1], [#, 'hydroxymethylglutaryl-CoA synthase', 1], [#, 'ATP citrate synthase', 1], [#, 'dihydrolipoyllysine-residue (2-methylpropanoyl)transferase', 1]]]).
fb_tsv_pred_stats('num-columns', dmel_gene_sequence_ontology_annotations, [4]).
fb_tsv_pred_stats('duplicated-rows', dmel_gene_sequence_ontology_annotations, [0]).
fb_tsv_pred_stats('total-rows', dmel_gene_sequence_ontology_annotations, [38590]).
fb_tsv_pred_stats('unique-values', dmel_gene_sequence_ontology_annotations, [1, 17902, object]).
fb_tsv_pred_stats('unique-values', dmel_gene_sequence_ontology_annotations, [2, 17902, object]).
fb_tsv_pred_stats('unique-values', dmel_gene_sequence_ontology_annotations, [3, 44, object]).
fb_tsv_pred_stats('unique-values', dmel_gene_sequence_ontology_annotations, [4, 44, object]).
fb_tsv_pred_stats('missing-values', dmel_gene_sequence_ontology_annotations, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_gene_sequence_ontology_annotations, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_gene_sequence_ontology_annotations, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_gene_sequence_ontology_annotations, [4, 0, [], []]).
fb_tsv_pred_stats('most-frequent', dmel_gene_sequence_ontology_annotations, [1, [#, [#, 'FBgn0263102', 5], [#, 'FBgn0264308', 5], [#, 'FBgn0285944', 5], [#, 'FBgn0000448', 5], [#, 'FBgn0266019', 5], [#, 'FBgn0260866', 5], [#, 'FBgn0283521', 5]]]).
fb_tsv_pred_stats('less-frequent', dmel_gene_sequence_ontology_annotations, [1, [#, [#, 'FBgn0000003', 1], [#, 'FBgn0085282', 1], [#, 'FBgn0085280', 1], [#, 'FBgn0085279', 1], [#, 'FBgn0085277', 1], [#, 'FBgn0085276', 1], [#, 'FBgn0085274', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_gene_sequence_ontology_annotations, [2, [#, [#, psq, 5], [#, hbt, 5], [#, para, 5], [#, 'Hr3', 5], [#, rudhira, 5], [#, dnr1, 5], [#, lola, 5]]]).
fb_tsv_pred_stats('less-frequent', dmel_gene_sequence_ontology_annotations, [2, [#, [#, '7SLRNA:CR32864', 1], [#, 'CG34253', 1], [#, 'CG34251', 1], [#, 'CG34250', 1], [#, 'CG34248', 1], [#, 'CG34247', 1], [#, 'CG34245', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_gene_sequence_ontology_annotations, [3, [#, [#, gene_with_stop_codon_read_through, 402], [#, antisense_lncRNA_gene, 507], [#, gene, 767], [#, gene_with_edited_transcript, 810], [#, lncRNA_gene, 1996], [#, protein_coding_gene, 13986], [#, nuclear_gene, 17707]]]).
fb_tsv_pred_stats('less-frequent', dmel_gene_sequence_ontology_annotations, [3, [#, [#, sbRNA_gene, 1], [#, 'SRP_RNA_gene', 1], [#, 'RNase_MRP_RNA_gene', 1], [#, 'RNase_P_RNA_gene', 1], [#, mt_SSU_rRNA_gene, 1], [#, mt_LSU_rRNA_gene, 1], [#, gene_with_recoded_mRNA, 2]]]).
fb_tsv_pred_stats('most-frequent', dmel_gene_sequence_ontology_annotations, [4, [#, [#, 'SO:0000697', 402], [#, 'SO:0002182', 507], [#, 'SO:0000704', 767], [#, 'SO:0000548', 810], [#, 'SO:0002127', 1996], [#, 'SO:0001217', 13986], [#, 'SO:0000087', 17707]]]).
fb_tsv_pred_stats('less-frequent', dmel_gene_sequence_ontology_annotations, [4, [#, [#, 'SO:0002353', 1], [#, 'SO:0001269', 1], [#, 'SO:0001640', 1], [#, 'SO:0001639', 1], [#, 'SO:0002365', 1], [#, 'SO:0002364', 1], [#, 'SO:0000693', 2]]]).
fb_tsv_pred_stats('num-columns', dmel_unique_protein_isoforms, [4]).
fb_tsv_pred_stats('duplicated-rows', dmel_unique_protein_isoforms, [0]).
fb_tsv_pred_stats('total-rows', dmel_unique_protein_isoforms, [22452]).
fb_tsv_pred_stats('unique-values', dmel_unique_protein_isoforms, [1, 13986, object]).
fb_tsv_pred_stats('unique-values', dmel_unique_protein_isoforms, [2, 13986, object]).
fb_tsv_pred_stats('unique-values', dmel_unique_protein_isoforms, [3, 22452, object]).
fb_tsv_pred_stats('unique-values', dmel_unique_protein_isoforms, [4, 5633, object]).
fb_tsv_pred_stats('missing-values', dmel_unique_protein_isoforms, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_unique_protein_isoforms, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_unique_protein_isoforms, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', dmel_unique_protein_isoforms, [4, '', 16820]).
fb_tsv_pred_stats('missing-values', dmel_unique_protein_isoforms, [4, 16820, [#, ''], [#, 16820]]).
fb_tsv_pred_stats('most-frequent', dmel_unique_protein_isoforms, [1, [#, [#, 'FBgn0003429', 23], [#, 'FBgn0033504', 25], [#, 'FBgn0266696', 25], [#, 'FBgn0284408', 25], [#, 'FBgn0002781', 31], [#, 'FBgn0285944', 57], [#, 'FBgn0033159', 75]]]).
fb_tsv_pred_stats('less-frequent', dmel_unique_protein_isoforms, [1, [#, [#, 'FBgn0000008', 1], [#, 'FBgn0038579', 1], [#, 'FBgn0038581', 1], [#, 'FBgn0038582', 1], [#, 'FBgn0038583', 1], [#, 'FBgn0038584', 1], [#, 'FBgn0038585', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_unique_protein_isoforms, [2, [#, [#, slo, 23], [#, 'CAP', 25], [#, 'Svil', 25], [#, trol, 25], [#, 'mod(mdg4)', 31], [#, para, 57], [#, 'Dscam1', 75]]]).
fb_tsv_pred_stats('less-frequent', dmel_unique_protein_isoforms, [2, [#, [#, a, 1], [#, 'CG14313', 1], [#, 'CG14314', 1], [#, 'CG7988', 1], [#, 'CG7183', 1], [#, mTerf5, 1], [#, 'Non3', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_unique_protein_isoforms, [4, [#, [#, 'Saf-B-PF', 1], [#, 'sosie-PC', 1], [#, 'Nct-PC', 1], [#, 'CG3744-PG', 1], [#, 'CG3744-PF', 1], [#, 'Not11-PC', 1]]]).
fb_tsv_pred_stats('less-frequent', dmel_unique_protein_isoforms, [4, [#, [#, 'MAN1-PB,MAN1-PC', 1], [#, 'Nct-PC', 1], [#, 'sosie-PC', 1], [#, 'Saf-B-PF', 1], [#, 'Ude-PC,Ude-PD', 1], [#, 'CG5805-PB', 1], [#, 'atl-PB,atl-PC', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_annotation_ID, [6]).
fb_tsv_pred_stats('duplicated-rows', fbgn_annotation_ID, [0]).
fb_tsv_pred_stats('total-rows', fbgn_annotation_ID, [17901]).
fb_tsv_pred_stats('unique-values', fbgn_annotation_ID, [1, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_annotation_ID, [2, 1, object]).
fb_tsv_pred_stats('unique-values', fbgn_annotation_ID, [3, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_annotation_ID, [4, 6498, object]).
fb_tsv_pred_stats('unique-values', fbgn_annotation_ID, [5, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_annotation_ID, [6, 2214, object]).
fb_tsv_pred_stats('missing-values', fbgn_annotation_ID, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_annotation_ID, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_annotation_ID, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_annotation_ID, [4, '', 11059]).
fb_tsv_pred_stats('missing-values', fbgn_annotation_ID, [4, 11059, [#, ''], [#, 11059]]).
fb_tsv_pred_stats('missing-values', fbgn_annotation_ID, [5, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_annotation_ID, [6, '', 15268]).
fb_tsv_pred_stats('missing-values', fbgn_annotation_ID, [6, 15268, [#, ''], [#, 15268]]).
fb_tsv_pred_stats('most-frequent', fbgn_annotation_ID, [2, [#, [#, 'Dmel', 17901]]]).
fb_tsv_pred_stats('less-frequent', fbgn_annotation_ID, [2, [#, [#, 'Dmel', 17901]]]).
fb_tsv_pred_stats('most-frequent', fbgn_annotation_ID, [4, [#, [#, 'FBgn0031342', 4], [#, 'FBgn0034648', 4], [#, 'FBgn0037173', 6], [#, 'FBgn0003932', 6], [#, 'FBgn0052605', 12], [#, 'FBgn0002867', 32]]]).
fb_tsv_pred_stats('less-frequent', fbgn_annotation_ID, [4, [#, [#, 'FBgn0034701,FBgn0034702', 1], [#, 'FBgn0036269,FBgn0036270', 1], [#, 'FBgn0062499', 1], [#, 'FBgn0045834,FBgn0046807', 1], [#, 'FBgn0060100', 1], [#, 'FBgn0036185,FBgn0040818', 1], [#, 'FBgn0036175', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_annotation_ID, [6, [#, [#, 'CG14398', 4], [#, 'CG15675', 4], [#, 'CG10450', 4], [#, 'CG14458', 6], [#, 'CG2149', 6], [#, 'CG32605', 12]]]).
fb_tsv_pred_stats('less-frequent', fbgn_annotation_ID, [6, [#, [#, 'CG13505', 1], [#, 'CG15275,CG15276,CG15277,CG33515', 1], [#, 'CG31537,CG2534', 1], [#, 'CG30111,CG5058', 1], [#, 'CG30164,CG30165,CG30166,CG4556', 1], [#, 'CG30179,CG30174,CG33149,CG3220', 1], [#, 'CG14428', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_exons2affy1_overlaps, [30]).
fb_tsv_pred_stats('duplicated-rows', fbgn_exons2affy1_overlaps, [0]).
fb_tsv_pred_stats('total-rows', fbgn_exons2affy1_overlaps, [12082]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [1, 12082, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [2, 11598, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [3, 11576, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [4, 11569, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [5, 11538, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [6, 11509, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [7, 11474, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [8, 11449, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [9, 11414, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [10, 11387, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [11, 11345, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [12, 11314, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [13, 11244, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [14, 11171, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [15, 10830, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [16, 1808, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [17, 1555, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [18, 1450, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [19, 1358, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [20, 1308, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [21, 1258, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [22, 1213, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [23, 1183, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [24, 1139, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [25, 1102, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [26, 1058, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [27, 994, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [28, 922, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [29, 743, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy1_overlaps, [30, 34, object]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [3, '', 74]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [3, 74, [#, ''], [#, 74]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [4, '', 105]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [4, 105, [#, ''], [#, 105]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [5, '', 149]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [5, 149, [#, ''], [#, 149]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [6, '', 184]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [6, 184, [#, ''], [#, 184]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [7, '', 232]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [7, 232, [#, ''], [#, 232]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [8, '', 265]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [8, 265, [#, ''], [#, 265]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [9, '', 305]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [9, 305, [#, ''], [#, 305]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [10, '', 341]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [10, 341, [#, ''], [#, 341]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [11, '', 392]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [11, 392, [#, ''], [#, 392]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [12, '', 425]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [12, 425, [#, ''], [#, 425]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [13, '', 506]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [13, 506, [#, ''], [#, 506]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [14, '', 587]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [14, 587, [#, ''], [#, 587]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [15, '', 947]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [15, 947, [#, ''], [#, 947]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [16, '', 10174]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [16, 10174, [#, ''], [#, 10174]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [17, '', 10457]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [17, 10457, [#, ''], [#, 10457]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [18, '', 10572]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [18, 10572, [#, ''], [#, 10572]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [19, '', 10667]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [19, 10667, [#, ''], [#, 10667]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [20, '', 10722]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [20, 10722, [#, ''], [#, 10722]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [21, '', 10774]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [21, 10774, [#, ''], [#, 10774]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [22, '', 10821]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [22, 10821, [#, ''], [#, 10821]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [23, '', 10853]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [23, 10853, [#, ''], [#, 10853]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [24, '', 10897]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [24, 10897, [#, ''], [#, 10897]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [25, '', 10935]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [25, 10935, [#, ''], [#, 10935]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [26, '', 10982]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [26, 10982, [#, ''], [#, 10982]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [27, '', 11048]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [27, 11048, [#, ''], [#, 11048]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [28, '', 11122]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [28, 11122, [#, ''], [#, 11122]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [29, '', 11305]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [29, 11305, [#, ''], [#, 11305]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy1_overlaps, [30, '', 12047]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy1_overlaps, [30, 12047, [#, ''], [#, 12047]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [2, [#, [#, '151241_f_at_37', 4], [#, '152093_at_1981', 4], [#, '150941_f_at_119', 5], [#, '151193_f_at_78', 6], [#, '151224_f_at_40', 6], [#, '150311_at_95', 7], [#, '150941_f_at_115', 13]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [2, [#, [#, '143052_at_1832', 1], [#, '154468_at_260', 1], [#, '152374_at_1177', 1], [#, '151894_at_1465', 1], [#, '154463_at_1950', 1], [#, '154463_at_2378', 1], [#, '154482_at_2296', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [3, [#, [#, '141538_at_2427', 3], [#, '147260_at_95', 3], [#, '155060_at_1425', 3], [#, '152093_at_2013', 4], [#, '151873_at_2152', 4], [#, '150311_at_124', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [3, [#, [#, '143052_at_1816', 1], [#, '149464_at_1100', 1], [#, '142785_at_1174', 1], [#, '142029_at_45', 1], [#, '149466_at_2832', 1], [#, '149467_at_1882', 1], [#, '149468_at_757', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [4, [#, [#, '147260_at_80', 3], [#, '147730_at_550', 3], [#, '155060_at_1472', 3], [#, '151873_at_2043', 4], [#, '152093_at_2053', 4], [#, '150311_at_247', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [4, [#, [#, '143052_at_1742', 1], [#, '152622_at_824', 1], [#, '149458_at_1158', 1], [#, '149459_at_906', 1], [#, '154507_at_1873', 1], [#, '149463_at_2017', 1], [#, '149464_at_1134', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [5, [#, [#, '147260_at_76', 3], [#, '155060_at_1504', 3], [#, '148466_at_864', 3], [#, '151873_at_2023', 4], [#, '152093_at_2081', 4], [#, '150311_at_271', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [5, [#, [#, '143052_at_1727', 1], [#, '154719_at_2453', 1], [#, '141210_at_404', 1], [#, '153896_at_1100', 1], [#, '149435_at_300', 1], [#, '149436_at_386', 1], [#, '149441_at_1003', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [6, [#, [#, '142689_at_2404', 3], [#, '151429_at_230', 3], [#, '147260_at_69', 3], [#, '152093_at_2097', 4], [#, '151873_at_2014', 4], [#, '150311_at_292', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [6, [#, [#, '143052_at_1717', 1], [#, '151951_at_2428', 1], [#, '154803_at_2317', 1], [#, '149420_at_517', 1], [#, '149421_at_366', 1], [#, '154555_at_1035', 1], [#, '154555_at_1375', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [7, [#, [#, '151429_at_248', 3], [#, '141538_at_2265', 3], [#, '155060_at_1543', 3], [#, '151873_at_1834', 4], [#, '152093_at_2142', 4], [#, '150311_at_321', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [7, [#, [#, '143052_at_1703', 1], [#, '154319_at_927', 1], [#, '154993_at_1959', 1], [#, '149402_at_2878', 1], [#, '149403_at_625', 1], [#, '149408_at_1010', 1], [#, '149409_at_619', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [8, [#, [#, '151429_at_270', 3], [#, '155060_at_1574', 3], [#, '142689_at_2342', 3], [#, '151873_at_1804', 4], [#, '152093_at_2177', 4], [#, '150311_at_350', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [8, [#, [#, '150941_f_at_115', 1], [#, '142317_at_420', 1], [#, '141883_at_323', 1], [#, '149397_at_943', 1], [#, '149398_at_568', 1], [#, '152411_at_1798', 1], [#, '149399_at_1928', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [9, [#, [#, '155060_at_1631', 3], [#, '151429_at_293', 3], [#, '147260_at_36', 3], [#, '152093_at_2220', 4], [#, '151873_at_1786', 4], [#, '150311_at_358', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [9, [#, [#, '150941_f_at_113', 1], [#, '149380_at_1052', 1], [#, '149381_at_595', 1], [#, '149382_at_343', 1], [#, '149384_at_626', 1], [#, '149385_at_909', 1], [#, '149386_at_153', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [10, [#, [#, '142689_at_2195', 3], [#, '147260_at_34', 3], [#, '141538_at_2111', 3], [#, '152093_at_2221', 4], [#, '151873_at_1747', 4], [#, '150311_at_405', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [10, [#, [#, '150941_f_at_111', 1], [#, '149364_at_1814', 1], [#, '149365_at_276', 1], [#, '149368_at_173', 1], [#, '149371_at_1969', 1], [#, '149372_at_632', 1], [#, '149374_at_1908', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [11, [#, [#, '151429_at_351', 3], [#, '142689_at_2154', 3], [#, '141538_at_2086', 3], [#, '152093_at_2253', 4], [#, '151873_at_1723', 4], [#, '150311_at_406', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [11, [#, [#, '151224_f_at_40', 1], [#, '149352_at_2845', 1], [#, '155039_at_1822', 1], [#, '149354_at_228', 1], [#, '153107_at_1501', 1], [#, '149355_at_446', 1], [#, '152216_at_1907', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [12, [#, [#, '141538_at_2050', 3], [#, '147260_at_29', 3], [#, '151429_at_444', 3], [#, '151873_at_1696', 4], [#, '152093_at_2286', 4], [#, '150311_at_428', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [12, [#, [#, '143052_at_1570', 1], [#, '153297_at_1146', 1], [#, '141292_at_956', 1], [#, '155136_at_3280', 1], [#, '151994_at_924', 1], [#, '154820_at_469', 1], [#, '152906_at_2090', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [13, [#, [#, '155060_at_1834', 3], [#, '147260_at_27', 3], [#, '142689_at_2057', 3], [#, '151873_at_1681', 4], [#, '152093_at_2365', 4], [#, '150311_at_513', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [13, [#, [#, '143052_at_1561', 1], [#, '149339_at_461', 1], [#, '142566_at_6907', 1], [#, '149340_at_389', 1], [#, '149341_at_1305', 1], [#, '149343_at_547', 1], [#, '153316_at_970', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [14, [#, [#, '141538_at_1972', 3], [#, '155060_at_1844', 3], [#, '147260_at_21', 3], [#, '152093_at_2369', 4], [#, '151873_at_1656', 4], [#, '150311_at_569', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [14, [#, [#, '143052_at_1547', 1], [#, '153677_at_593', 1], [#, '142725_at_3484', 1], [#, '154418_at_2357', 1], [#, '149337_at_1771', 1], [#, '149338_at_114', 1], [#, '153664_at_1123', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [15, [#, [#, '155060_at_1866', 3], [#, '147260_at_19', 3], [#, '141538_at_1905', 3], [#, '151873_at_1630', 4], [#, '152093_at_2425', 4], [#, '150311_at_588', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [15, [#, [#, '143052_at_1488', 1], [#, '142725_at_3437', 1], [#, '154418_at_2421', 1], [#, '149337_at_1722', 1], [#, '149338_at_101', 1], [#, '153664_at_1167', 1], [#, '149339_at_383', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [16, [#, [#, '151193_f_at_66', 3], [#, '151241_f_at_31', 5], [#, '151193_f_at_70', 6], [#, '151224_f_at_40', 7], [#, '150941_f_at_119', 7], [#, '150941_f_at_115', 7]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [16, [#, [#, '143052_at_1399', 1], [#, '154464_at_2230', 1], [#, '152047_at_1146', 1], [#, '150835_at_1439', 1], [#, '154168_at_1052', 1], [#, '153210_at_1520', 1], [#, '150817_at_565', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [17, [#, [#, '142169_at_450', 2], [#, '146155_at_129', 2], [#, '152590_at_972', 2], [#, '142115_at_370', 2], [#, '143730_at_1670', 2], [#, '150941_f_at_115', 9]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [17, [#, [#, '143052_at_1395', 1], [#, '151270_r_at_25', 1], [#, '151257_at_31', 1], [#, '151250_at_222', 1], [#, '151229_at_145', 1], [#, '151212_i_at_63', 1], [#, '151207_r_at_79', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [18, [#, [#, '142667_at_1666', 2], [#, '142844_at_1801', 2], [#, '143730_at_1655', 2], [#, '146200_at_740', 2], [#, '145665_at_260', 2], [#, '150941_f_at_113', 3]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [18, [#, [#, '143052_at_1376', 1], [#, '146970_at_795', 1], [#, '142323_at_349', 1], [#, '152548_at_1257', 1], [#, '142085_at_360', 1], [#, '151839_at_572', 1], [#, '147900_at_1316', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [19, [#, [#, '141971_at_425', 2], [#, '152590_at_890', 2], [#, '150643_at_593', 2], [#, '150117_at_272', 2], [#, '146155_at_95', 2], [#, '141331_at_764', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [19, [#, [#, '143052_at_1311', 1], [#, '146844_at_45', 1], [#, '152619_at_980', 1], [#, '147717_at_296', 1], [#, '141654_at_949', 1], [#, '147704_at_309', 1], [#, '151140_at_34', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [20, [#, [#, '152322_at_1081', 2], [#, '150574_at_674', 2], [#, '142012_at_515', 2], [#, '150611_at_681', 2], [#, '146094_at_3507', 2], [#, '143040_at_1502', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [20, [#, [#, '141890_at_423', 1], [#, '146741_at_332', 1], [#, '146978_s_at_613', 1], [#, '142001_r_at_17', 1], [#, '152057_at_746', 1], [#, '153061_at_3626', 1], [#, '147846_at_517', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [21, [#, [#, '143730_at_1571', 2], [#, '143071_at_199', 2], [#, '153690_at_1496', 2], [#, '146572_at_1248', 2], [#, '151196_r_at_69', 2], [#, '142734_at_582', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [21, [#, [#, '154290_at_1576', 1], [#, '150536_at_1846', 1], [#, '142057_at_306', 1], [#, '149536_at_287', 1], [#, '142647_at_483', 1], [#, '152342_at_1443', 1], [#, '150858_at_1203', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [22, [#, [#, '152828_at_1192', 2], [#, '145513_at_1169', 2], [#, '142641_at_1007', 2], [#, '145665_at_185', 2], [#, '145804_at_113', 2], [#, '144663_at_167', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [22, [#, [#, '152590_at_807', 1], [#, '150536_at_1823', 1], [#, '142057_at_329', 1], [#, '149536_at_257', 1], [#, '142647_at_451', 1], [#, '152342_at_1508', 1], [#, '150858_at_1226', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [23, [#, [#, '145121_at_953', 2], [#, '152828_at_1286', 2], [#, '145513_at_1144', 2], [#, '145665_at_164', 2], [#, '145804_at_98', 2], [#, '146744_at_188', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [23, [#, [#, '153298_at_1470', 1], [#, '150321_at_1006', 1], [#, '150383_at_2084', 1], [#, '149454_at_1935', 1], [#, '150142_at_1363', 1], [#, '150536_at_1771', 1], [#, '142057_at_344', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [24, [#, [#, '152828_at_1322', 2], [#, '145513_at_1132', 2], [#, '145665_at_155', 2], [#, '145804_at_94', 2], [#, '146094_at_3402', 2], [#, '151831_at_920', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [24, [#, [#, '152253_at_1253', 1], [#, '151549_i_at_495', 1], [#, '142287_at_5287', 1], [#, '150858_at_1274', 1], [#, '152342_at_1587', 1], [#, '142647_at_365', 1], [#, '149536_at_82', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [25, [#, [#, '145121_at_1044', 2], [#, '152828_at_1374', 2], [#, '145513_at_1115', 2], [#, '145665_at_112', 2], [#, '145804_at_88', 2], [#, '143367_at_1268', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [25, [#, [#, '152253_at_1277', 1], [#, '152342_at_1612', 1], [#, '142647_at_311', 1], [#, '149536_at_79', 1], [#, '142057_at_440', 1], [#, '150536_at_1708', 1], [#, '150142_at_1308', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [26, [#, [#, '142968_at_1016', 2], [#, '147307_at_1054', 2], [#, '152828_at_1410', 2], [#, '145513_at_1081', 2], [#, '145804_at_83', 2], [#, '152030_at_2454', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [26, [#, [#, '151555_at_452', 1], [#, '150321_at_786', 1], [#, '149984_at_574', 1], [#, '150288_at_440', 1], [#, '154729_at_1256', 1], [#, '150235_at_482', 1], [#, '153559_at_1188', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [27, [#, [#, '142051_at_710', 2], [#, '143071_at_395', 2], [#, '153690_at_1701', 2], [#, '152030_at_2472', 2], [#, '148950_at_1101', 2], [#, '146572_at_1489', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [27, [#, [#, '151555_at_477', 1], [#, '143703_at_549', 1], [#, '150198_at_145', 1], [#, '153568_at_2595', 1], [#, '153408_at_895', 1], [#, '154631_at_2296', 1], [#, '149802_at_35', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [28, [#, [#, '151831_at_1098', 2], [#, '143071_at_435', 2], [#, '142051_at_734', 2], [#, '153690_at_1718', 2], [#, '142946_at_186', 2], [#, '148950_at_1027', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [28, [#, [#, '154840_at_235', 1], [#, '149689_at_838', 1], [#, '152285_at_955', 1], [#, '151626_at_139', 1], [#, '151157_at_129', 1], [#, '151986_at_1676', 1], [#, '149373_at_477', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [29, [#, [#, '143040_at_1070', 2], [#, '148950_at_1005', 2], [#, '152322_at_1333', 2], [#, '142209_at_639', 2], [#, '143367_at_1495', 2], [#, '153005_at_1202', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [29, [#, [#, '154840_at_217', 1], [#, '151546_i_at_74', 1], [#, '145929_at_1640', 1], [#, '151417_at_35', 1], [#, '145917_at_142', 1], [#, '154703_at_1319', 1], [#, '145904_at_869', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy1_overlaps, [30, [#, [#, '151241_f_at_37', 1], [#, '145456_at_322', 1], [#, '142630_at_415', 1], [#, '148015_at_1581', 1], [#, '153230_at_2930', 2], [#, '154681_at_1547', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy1_overlaps, [30, [#, [#, '153483_at_641', 1], [#, '150941_f_at_90', 1], [#, '144004_at_5787', 1], [#, '144775_at_57', 1], [#, '144816_at_1278', 1], [#, '143155_at_2478', 1], [#, '148015_at_1581', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_exons2affy2_overlaps, [14]).
fb_tsv_pred_stats('duplicated-rows', fbgn_exons2affy2_overlaps, [4149]).
fb_tsv_pred_stats('total-rows', fbgn_exons2affy2_overlaps, [12905]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [1, 8719, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [2, 1595, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [3, 1248, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [4, 1045, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [5, 922, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [6, 830, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [7, 752, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [8, 699, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [9, 639, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [10, 573, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [11, 508, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [12, 442, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [13, 358, object]).
fb_tsv_pred_stats('unique-values', fbgn_exons2affy2_overlaps, [14, 207, object]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [1, '', 3969]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [1, 3969, [#, ''], [#, 3969]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [2, '', 11248]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [2, 11248, [#, ''], [#, 11248]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [3, '', 11607]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [3, 11607, [#, ''], [#, 11607]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [4, '', 11816]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [4, 11816, [#, ''], [#, 11816]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [5, '', 11943]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [5, 11943, [#, ''], [#, 11943]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [6, '', 12036]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [6, 12036, [#, ''], [#, 12036]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [7, '', 12116]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [7, 12116, [#, ''], [#, 12116]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [8, '', 12171]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [8, 12171, [#, ''], [#, 12171]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [9, '', 12231]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [9, 12231, [#, ''], [#, 12231]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [10, '', 12300]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [10, 12300, [#, ''], [#, 12300]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [11, '', 12366]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [11, 12366, [#, ''], [#, 12366]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [12, '', 12437]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [12, 12437, [#, ''], [#, 12437]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [13, '', 12525]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [13, 12525, [#, ''], [#, 12525]]).
fb_tsv_pred_stats('null-value-count', fbgn_exons2affy2_overlaps, [14, '', 12686]).
fb_tsv_pred_stats('missing-values', fbgn_exons2affy2_overlaps, [14, 12686, [#, ''], [#, 12686]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [1, [#, [#, '1630536_at_1664', 3], [#, '1633941_a_at_2332', 3], [#, '1632394_s_at_2089', 3], [#, '1625692_s_at_1888', 4], [#, '1627164_at_3410', 4], [#, '1625897_s_at_1066', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [1, [#, [#, '1637813_at_4186', 1], [#, '1638283_at_2407', 1], [#, '1639086_s_at_1819', 1], [#, '1631434_at_334', 1], [#, '1634855_s_at_1654', 1], [#, '1636657_at_473', 1], [#, '1641712_at_1957', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [2, [#, [#, '1635578_at_1116', 2], [#, '1633941_a_at_2295', 3], [#, '1631833_at_1721', 3], [#, '1625692_s_at_1832', 4], [#, '1635572_at_641', 4], [#, '1625897_s_at_1127', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [2, [#, [#, '1636558_a_at_1881', 1], [#, '1628490_at_857', 1], [#, '1641656_at_713', 1], [#, '1630618_at_2197', 1], [#, '1625101_at_335', 1], [#, '1635901_at_939', 1], [#, '1631798_s_at_1021', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [3, [#, [#, '1631351_s_at_1022', 2], [#, '1626491_at_1540', 2], [#, '1635572_at_608', 3], [#, '1633941_a_at_2232', 3], [#, '1631833_at_1705', 3], [#, '1625897_s_at_1142', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [3, [#, [#, '1636558_a_at_1850', 1], [#, '1631522_x_at_66', 1], [#, '1632440_at_286', 1], [#, '1625460_s_at_89', 1], [#, '1625783_at_502', 1], [#, '1626735_at_200', 1], [#, '1628502_at_164', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [4, [#, [#, '1632950_at_1094', 2], [#, '1629590_at_807', 2], [#, '1635578_at_1046', 2], [#, '1635572_at_529', 3], [#, '1631833_at_1657', 3], [#, '1625897_s_at_1160', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [4, [#, [#, '1636558_a_at_1823', 1], [#, '1629951_at_775', 1], [#, '1627721_at_1681', 1], [#, '1632946_at_1358', 1], [#, '1630788_at_1473', 1], [#, '1629262_at_502', 1], [#, '1628625_at_1245', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [5, [#, [#, '1636445_at_196', 2], [#, '1635578_at_1031', 2], [#, '1634667_at_2047', 2], [#, '1631833_at_1617', 3], [#, '1635572_at_481', 3], [#, '1625897_s_at_1229', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [5, [#, [#, '1636558_a_at_1742', 1], [#, '1636314_at_387', 1], [#, '1625214_at_592', 1], [#, '1633462_at_1757', 1], [#, '1638793_at_441', 1], [#, '1630993_at_814', 1], [#, '1637693_at_840', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [6, [#, [#, '1636445_at_205', 2], [#, '1628766_at_1592', 2], [#, '1627958_at_364', 2], [#, '1631833_at_1553', 3], [#, '1635572_at_448', 3], [#, '1625897_s_at_1282', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [6, [#, [#, '1636558_a_at_1725', 1], [#, '1629127_at_687', 1], [#, '1632948_x_at_741', 1], [#, '1631596_at_2406', 1], [#, '1623590_s_at_2676', 1], [#, '1638445_a_at_4372', 1], [#, '1632739_at_505', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [7, [#, [#, '1627427_s_at_1090', 2], [#, '1626491_at_1395', 2], [#, '1634933_s_at_300', 2], [#, '1635572_at_432', 3], [#, '1631833_at_1528', 3], [#, '1625897_s_at_1292', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [7, [#, [#, '1636558_a_at_1709', 1], [#, '1625051_at_611', 1], [#, '1634488_s_at_341', 1], [#, '1630421_at_597', 1], [#, '1628563_at_1739', 1], [#, '1636412_at_2651', 1], [#, '1634418_at_1457', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [8, [#, [#, '1627958_at_416', 2], [#, '1626732_at_695', 2], [#, '1626038_at_1253', 2], [#, '1635572_at_393', 3], [#, '1631833_at_1513', 3], [#, '1625897_s_at_1337', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [8, [#, [#, '1636558_a_at_1571', 1], [#, '1632338_a_at_1205', 1], [#, '1641119_at_364', 1], [#, '1627824_at_101', 1], [#, '1632926_at_75', 1], [#, '1631364_at_518', 1], [#, '1639058_at_120', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [9, [#, [#, '1630406_at_385', 2], [#, '1631351_s_at_947', 2], [#, '1628766_at_1690', 2], [#, '1631833_at_1487', 3], [#, '1635572_at_378', 3], [#, '1625897_s_at_1362', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [9, [#, [#, '1636558_a_at_1561', 1], [#, '1632926_at_17', 1], [#, '1631364_at_547', 1], [#, '1639058_at_103', 1], [#, '1628447_at_492', 1], [#, '1626455_s_at_399', 1], [#, '1638766_at_109', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [10, [#, [#, '1630406_at_337', 2], [#, '1629590_at_1045', 2], [#, '1633270_at_394', 2], [#, '1631833_at_1469', 3], [#, '1635572_at_297', 3], [#, '1625897_s_at_1444', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [10, [#, [#, '1636558_a_at_1482', 1], [#, '1635345_at_50', 1], [#, '1634677_at_643', 1], [#, '1635112_at_279', 1], [#, '1641708_at_3032', 1], [#, '1629586_at_1283', 1], [#, '1640473_at_4678', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [11, [#, [#, '1632147_at_448', 2], [#, '1629590_at_1085', 2], [#, '1634176_a_at_438', 2], [#, '1635572_at_234', 3], [#, '1631833_at_1450', 3], [#, '1625897_s_at_1488', 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [11, [#, [#, '1636558_a_at_1425', 1], [#, '1638214_at_467', 1], [#, '1637070_at_754', 1], [#, '1636893_at_55', 1], [#, '1639804_at_1221', 1], [#, '1628992_at_20', 1], [#, '1627697_at_763', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [12, [#, [#, '1634933_s_at_139', 2], [#, '1627958_at_512', 2], [#, '1627427_s_at_891', 2], [#, '1627339_at_219', 2], [#, '1631833_at_1435', 3], [#, '1635572_at_209', 3]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [12, [#, [#, '1636558_a_at_1397', 1], [#, '1638214_at_453', 1], [#, '1637070_at_778', 1], [#, '1636893_at_43', 1], [#, '1639804_at_1258', 1], [#, '1628992_at_17', 1], [#, '1627697_at_738', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [13, [#, [#, '1634933_s_at_106', 2], [#, '1632147_at_514', 2], [#, '1633837_at_546', 2], [#, '1627339_at_124', 2], [#, '1631833_at_1404', 3], [#, '1635572_at_157', 3]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [13, [#, [#, '1636558_a_at_1379', 1], [#, '1638214_at_432', 1], [#, '1636893_at_32', 1], [#, '1639804_at_1271', 1], [#, '1628992_at_15', 1], [#, '1627697_at_714', 1], [#, '1635345_at_43', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_exons2affy2_overlaps, [14, [#, [#, '1627339_at_73', 2], [#, '1626491_at_1063', 2], [#, '1634159_s_at_35', 2], [#, '1626732_at_641', 2], [#, '1631833_at_1383', 3], [#, '1635572_at_129', 3]]]).
fb_tsv_pred_stats('less-frequent', fbgn_exons2affy2_overlaps, [14, [#, [#, '1636558_a_at_1365', 1], [#, '1628563_at_1922', 1], [#, '1636412_at_2551', 1], [#, '1634418_at_1571', 1], [#, '1632338_a_at_1456', 1], [#, '1641119_at_581', 1], [#, '1626455_s_at_584', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_fbtr_fbpp_expanded, [11]).
fb_tsv_pred_stats('duplicated-rows', fbgn_fbtr_fbpp_expanded, [0]).
fb_tsv_pred_stats('total-rows', fbgn_fbtr_fbpp_expanded, [35732]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [1, 1, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [2, 27, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [3, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [4, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [5, 11358, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [6, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [7, 9, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [8, 35732, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [9, 35732, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [10, 30803, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp_expanded, [11, 30803, object]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [4, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_fbtr_fbpp_expanded, [5, '', 11174]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [5, 11174, [#, ''], [#, 11174]]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [6, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [7, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [8, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [9, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_fbtr_fbpp_expanded, [10, '', 4930]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [10, 4930, [#, ''], [#, 4930]]).
fb_tsv_pred_stats('null-value-count', fbgn_fbtr_fbpp_expanded, [11, '', 4930]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp_expanded, [11, 4930, [#, ''], [#, 4930]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [1, [#, [#, 'Dmel', 35732]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [1, [#, [#, 'Dmel', 35732]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [2, [#, [#, 'C_D_box_snoRNA_gene', 145], [#, tRNA_gene, 312], [#, pseudogene, 365], [#, antisense_lncRNA_gene, 621], [#, miRNA_gene, 747], [#, lncRNA_gene, 2374], [#, protein_coding_gene, 30802]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [2, [#, [#, sbRNA_gene, 1], [#, 'SRP_RNA_gene', 1], [#, mt_LSU_rRNA_gene, 1], [#, mt_SSU_rRNA_gene, 1], [#, 'RNase_MRP_RNA_gene', 1], [#, cytosolic_rRNA_28S_gene, 2], [#, 'RNA_7SK_gene', 2]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [3, [#, [#, 'FBgn0011224', 25], [#, 'FBgn0284408', 25], [#, 'FBgn0033504', 28], [#, 'FBgn0266696', 31], [#, 'FBgn0002781', 31], [#, 'FBgn0285944', 60], [#, 'FBgn0033159', 75]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [3, [#, [#, 'FBgn0038809', 1], [#, 'FBgn0051812', 1], [#, 'FBgn0051815', 1], [#, 'FBgn0051816', 1], [#, 'FBgn0051822', 1], [#, 'FBgn0051823', 1], [#, 'FBgn0051824', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [4, [#, [#, heph, 25], [#, trol, 25], [#, 'CAP', 28], [#, 'Svil', 31], [#, 'mod(mdg4)', 31], [#, para, 60], [#, 'Dscam1', 75]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [4, [#, [#, 'CG16953', 1], [#, 'Tsen2', 1], [#, 'CG31815', 1], [#, 'CG31816', 1], [#, 'CR31822', 1], [#, 'CG31823', 1], [#, 'CR31824', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [5, [#, [#, hephaestus, 25], [#, 'CAP', 28], [#, 'modifier of mdg4', 31], [#, 'Supervillin', 31], [#, paralytic, 60], [#, 'Down syndrome cell adhesion molecule 1', 75]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [5, [#, [#, 'long non-coding RNA:CR46535', 1], [#, 'Gustatory receptor 22e', 1], [#, 'long non-coding RNA:CR45471', 1], [#, 'Gustatory receptor 36a', 1], [#, 'Gustatory receptor 36b', 1], [#, 'Gustatory receptor 36c', 1], [#, 'Gustatory receptor 59a', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [6, [#, [#, 'CG31000', 25], [#, 'CG33950', 25], [#, 'CG18408', 28], [#, 'CG45186', 31], [#, 'CG32491', 31], [#, 'CG9907', 60], [#, 'CG17800', 75]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [6, [#, [#, 'CG16953', 1], [#, 'CG31812', 1], [#, 'CG31815', 1], [#, 'CG31816', 1], [#, 'CR31822', 1], [#, 'CG31823', 1], [#, 'CR31824', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [7, [#, [#, pre_miRNA, 262], [#, snoRNA, 300], [#, tRNA, 312], [#, pseudogene, 365], [#, miRNA, 485], [#, ncRNA, 3059], [#, mRNA, 30802]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [7, [#, [#, snRNA, 32], [#, rRNA, 115], [#, pre_miRNA, 262], [#, snoRNA, 300], [#, tRNA, 312], [#, pseudogene, 365], [#, miRNA, 485]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [10, [#, [#, 'FBpp0309148', 1], [#, 'FBpp0074581', 1], [#, 'FBpp0290917', 1], [#, 'FBpp0308522', 1], [#, 'FBpp0304236', 1], [#, 'FBpp0071677', 1]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [10, [#, [#, 'FBpp0071677', 1], [#, 'FBpp0308522', 1], [#, 'FBpp0290917', 1], [#, 'FBpp0074581', 1], [#, 'FBpp0309148', 1], [#, 'FBpp0071045', 1], [#, 'FBpp0084559', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp_expanded, [11, [#, [#, 'p115-PB', 1], [#, 'meso18E-PA', 1], [#, 'meso18E-PB', 1], [#, 'meso18E-PC', 1], [#, 'rept-PB', 1], [#, 'a-PA', 1]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp_expanded, [11, [#, [#, 'a-PA', 1], [#, 'meso18E-PC', 1], [#, 'meso18E-PB', 1], [#, 'meso18E-PA', 1], [#, 'p115-PB', 1], [#, 'p115-PA', 1], [#, 'pins-PA', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_fbtr_fbpp, [3]).
fb_tsv_pred_stats('duplicated-rows', fbgn_fbtr_fbpp, [0]).
fb_tsv_pred_stats('total-rows', fbgn_fbtr_fbpp, [35732]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp, [1, 17901, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp, [2, 35732, object]).
fb_tsv_pred_stats('unique-values', fbgn_fbtr_fbpp, [3, 30803, object]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_fbtr_fbpp, [3, '', 4930]).
fb_tsv_pred_stats('missing-values', fbgn_fbtr_fbpp, [3, 4930, [#, ''], [#, 4930]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp, [1, [#, [#, 'FBgn0011224', 25], [#, 'FBgn0284408', 25], [#, 'FBgn0033504', 28], [#, 'FBgn0266696', 31], [#, 'FBgn0002781', 31], [#, 'FBgn0285944', 60], [#, 'FBgn0033159', 75]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp, [1, [#, [#, 'FBgn0038809', 1], [#, 'FBgn0051812', 1], [#, 'FBgn0051815', 1], [#, 'FBgn0051816', 1], [#, 'FBgn0051822', 1], [#, 'FBgn0051823', 1], [#, 'FBgn0051824', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_fbtr_fbpp, [3, [#, [#, 'FBpp0309148', 1], [#, 'FBpp0074581', 1], [#, 'FBpp0290917', 1], [#, 'FBpp0308522', 1], [#, 'FBpp0304236', 1], [#, 'FBpp0071677', 1]]]).
fb_tsv_pred_stats('less-frequent', fbgn_fbtr_fbpp, [3, [#, [#, 'FBpp0071677', 1], [#, 'FBpp0308522', 1], [#, 'FBpp0290917', 1], [#, 'FBpp0074581', 1], [#, 'FBpp0309148', 1], [#, 'FBpp0071045', 1], [#, 'FBpp0084559', 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_gleanr, [4]).
fb_tsv_pred_stats('duplicated-rows', fbgn_gleanr, [535]).
fb_tsv_pred_stats('total-rows', fbgn_gleanr, [174893]).
fb_tsv_pred_stats('unique-values', fbgn_gleanr, [1, 11, object]).
fb_tsv_pred_stats('unique-values', fbgn_gleanr, [2, 174055, object]).
fb_tsv_pred_stats('unique-values', fbgn_gleanr, [3, 174055, object]).
fb_tsv_pred_stats('unique-values', fbgn_gleanr, [4, 174322, object]).
fb_tsv_pred_stats('missing-values', fbgn_gleanr, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_gleanr, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_gleanr, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_gleanr, [4, 0, [], []]).
fb_tsv_pred_stats('most-frequent', fbgn_gleanr, [1, [#, [#, 'Dere', 15380], [#, 'Dwil', 15874], [#, 'Dsim', 16038], [#, 'Dpse', 16475], [#, 'Dyak', 16483], [#, 'Dsec', 16945], [#, 'Dper', 17373]]]).
fb_tsv_pred_stats('less-frequent', fbgn_gleanr, [1, [#, [#, 'Dvir', 14755], [#, 'Dmoj', 14922], [#, 'Dgri', 15319], [#, 'Dana', 15329], [#, 'Dere', 15380], [#, 'Dwil', 15874], [#, 'Dsim', 16038]]]).
fb_tsv_pred_stats('most-frequent', fbgn_gleanr, [2, [#, [#, 'Dpse\\GA30155', 4], [#, 'Dpse\\GA30065', 4], [#, 'Dpse\\GA30499', 4], [#, 'Dpse\\GA30462', 4], [#, 'Dpse\\GA30119', 5], [#, 'Dpse\\GA30479', 5], [#, 'Dpse\\GA30442', 11]]]).
fb_tsv_pred_stats('less-frequent', fbgn_gleanr, [2, [#, [#, 'Dper\\GL22947', 1], [#, 'Dwil\\GK16453', 1], [#, 'Dwil\\GK11213', 1], [#, 'Dwil\\GK16551', 1], [#, 'Dwil\\GK11264', 1], [#, 'Dwil\\GK11214', 1], [#, 'Dwil\\GK11265', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_gleanr, [3, [#, [#, 'FBgn0263057', 4], [#, 'FBgn0263238', 4], [#, 'FBgn0265036', 4], [#, 'FBgn0264756', 4], [#, 'FBgn0262499', 5], [#, 'FBgn0264773', 5], [#, 'FBgn0264579', 11]]]).
fb_tsv_pred_stats('less-frequent', fbgn_gleanr, [3, [#, [#, 'FBgn0160537', 1], [#, 'FBgn0218455', 1], [#, 'FBgn0213224', 1], [#, 'FBgn0218553', 1], [#, 'FBgn0213275', 1], [#, 'FBgn0213225', 1], [#, 'FBgn0213276', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_gleanr, [4, [#, [#, dgri_GLEANR_4274, 2], [#, dana_GLEANR_19894, 2], [#, dgri_GLEANR_7321, 2], [#, dgri_GLEANR_14174, 2], [#, dpse_GLEANR_5123, 3], [#, dpse_GLEANR_4701, 4], [#, dpse_GLEANR_8878, 4]]]).
fb_tsv_pred_stats('less-frequent', fbgn_gleanr, [4, [#, [#, dsim_GLEANR_6342, 1], [#, dwil_GLEANR_16628, 1], [#, dwil_GLEANR_11555, 1], [#, dwil_GLEANR_11556, 1], [#, dwil_GLEANR_11460, 1], [#, dwil_GLEANR_11461, 1], [#, dwil_GLEANR_11557, 1]]]).
fb_tsv_pred_stats('num-columns', fbgn_NAseq_Uniprot, [9]).
fb_tsv_pred_stats('duplicated-rows', fbgn_NAseq_Uniprot, [0]).
fb_tsv_pred_stats('total-rows', fbgn_NAseq_Uniprot, [1316132]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [1, 214968, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [2, 557, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [3, 214968, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [4, 762485, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [5, 69341, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [6, 193395, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [7, 208900, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [8, 34349, object]).
fb_tsv_pred_stats('unique-values', fbgn_NAseq_Uniprot, [9, 30241, object]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbgn_NAseq_Uniprot, [4, '', 438122]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [4, 438122, [#, ''], [#, 438122]]).
fb_tsv_pred_stats('null-value-count', fbgn_NAseq_Uniprot, [5, '', 1246520]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [5, 1246520, [#, ''], [#, 1246520]]).
fb_tsv_pred_stats('null-value-count', fbgn_NAseq_Uniprot, [6, '', 1121258]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [6, 1121258, [#, ''], [#, 1121258]]).
fb_tsv_pred_stats('null-value-count', fbgn_NAseq_Uniprot, [7, '', 1107233]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [7, 1107233, [#, ''], [#, 1107233]]).
fb_tsv_pred_stats('null-value-count', fbgn_NAseq_Uniprot, [8, '', 1281783]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [8, 1281783, [#, ''], [#, 1281783]]).
fb_tsv_pred_stats('null-value-count', fbgn_NAseq_Uniprot, [9, '', 1285892]).
fb_tsv_pred_stats('missing-values', fbgn_NAseq_Uniprot, [9, 1285892, [#, ''], [#, 1285892]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [1, [#, [#, sta, 1332], [#, 'RpL5', 1362], [#, alphaTub84B, 1442], [#, 'Hsc70-4', 1611], [#, ninaE, 1699], [#, eEF2, 2370], [#, eEF1alpha1, 2383]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [1, [#, [#, 'Hsap\\FSBP', 1], [#, 'Dyak\\GE14653', 1], [#, 'Dyak\\GE14652', 1], [#, 'Dere\\GG16178', 1], [#, 'Dyak\\GE14651', 1], [#, 'Dyak\\GE14650', 1], [#, 'Dyak\\GE14649', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [2, [#, [#, 'Dper', 34397], [#, 'Dmoj', 34929], [#, 'Dvir', 35196], [#, 'Dsim', 37463], [#, 'Dana', 38929], [#, 'Dyak', 39546], [#, 'Dmel', 938032]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [2, [#, [#, 'Cpac', 1], [#, 'Hvul', 1], [#, 'Zghe', 1], [#, 'Dhye', 1], [#, 'Dabu', 1], [#, 'Xlae', 1], [#, 'Cfum', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [3, [#, [#, 'FBgn0003517', 1332], [#, 'FBgn0064225', 1362], [#, 'FBgn0003884', 1442], [#, 'FBgn0266599', 1611], [#, 'FBgn0002940', 1699], [#, 'FBgn0000559', 2370], [#, 'FBgn0284245', 2383]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [3, [#, [#, 'FBgn0289485', 1], [#, 'FBgn0232248', 1], [#, 'FBgn0232247', 1], [#, 'FBgn0108413', 1], [#, 'FBgn0232246', 1], [#, 'FBgn0232245', 1], [#, 'FBgn0232244', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [4, [#, [#, 'CH379071', 439], [#, 'AE014298', 5350], [#, 'AE014134', 5651], [#, 'AE014296', 5875], [#, 'AE013599', 6054], [#, 'AE014297', 7206]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [4, [#, [#, 'J01122', 1], [#, 'GH739917', 1], [#, 'GH740238', 1], [#, 'GH740504', 1], [#, 'GH740582', 1], [#, 'GH740722', 1], [#, 'GH741128', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [5, [#, [#, 'AAA28968', 9], [#, 'CAA38960', 9], [#, 'AAA28969', 9], [#, 'AAA28966', 9], [#, 'AAC47002', 9], [#, 'AAA28425', 10]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [5, [#, [#, 'AFH08132', 1], [#, 'AAG22409', 1], [#, 'AAL28803', 1], [#, 'AAL57756', 1], [#, 'AAF46261', 1], [#, 'AAF46262', 1], [#, 'AAM29624', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [6, [#, [#, 'P02283', 24], [#, 'P84052', 31], [#, 'Q76FD7', 40], [#, 'Q76FE2', 47], [#, 'Q76N00', 50], [#, 'Q76FD9', 65]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [6, [#, [#, 'A1ZAK1', 1], [#, 'B4R037', 1], [#, 'B4R038', 1], [#, 'B4R040', 1], [#, 'B4R043', 1], [#, 'B4R044', 1], [#, 'B4R047', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [7, [#, [#, 6632090.0, 1], [#, 6627682.0, 1], [#, 6631334.0, 1], [#, 6631316.0, 1], [#, 6634144.0, 1], [#, 6624472.0, 1]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [7, [#, [#, 6611558.0, 1], [#, 6634144.0, 1], [#, 6631316.0, 1], [#, 6631334.0, 1], [#, 6627682.0, 1], [#, 6632090.0, 1], [#, 6632175.0, 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [8, [#, [#, 'NM_168111', 1], [#, 'NM_168136', 1], [#, 'NM_168145', 1], [#, 'NM_168144', 1], [#, 'NM_001169879', 1], [#, 'NM_134950', 2]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [8, [#, [#, 'NM_141332', 1], [#, 'NM_001274510', 1], [#, 'NM_001274509', 1], [#, 'NM_001259707', 1], [#, 'NM_001169879', 1], [#, 'NM_168112', 1], [#, 'NM_168113', 1]]]).
fb_tsv_pred_stats('most-frequent', fbgn_NAseq_Uniprot, [9, [#, [#, 'NP_001137539', 1], [#, 'NP_001287647', 1], [#, 'NP_001287648', 1], [#, 'NP_001104474', 1], [#, 'NP_001188370', 1], [#, 'NP_001287646', 1]]]).
fb_tsv_pred_stats('less-frequent', fbgn_NAseq_Uniprot, [9, [#, [#, 'NP_648102', 1], [#, 'NP_001287648', 1], [#, 'NP_001287647', 1], [#, 'NP_001137539', 1], [#, 'NP_001104473', 1], [#, 'NP_001104472', 1], [#, 'NP_001104471', 1]]]).
fb_tsv_pred_stats('num-columns', gene_functional_complementation, [5]).
fb_tsv_pred_stats('duplicated-rows', gene_functional_complementation, [0]).
fb_tsv_pred_stats('total-rows', gene_functional_complementation, [463]).
fb_tsv_pred_stats('unique-values', gene_functional_complementation, [1, 319, object]).
fb_tsv_pred_stats('unique-values', gene_functional_complementation, [2, 319, object]).
fb_tsv_pred_stats('unique-values', gene_functional_complementation, [3, 376, object]).
fb_tsv_pred_stats('unique-values', gene_functional_complementation, [4, 376, object]).
fb_tsv_pred_stats('unique-values', gene_functional_complementation, [5, 392, object]).
fb_tsv_pred_stats('missing-values', gene_functional_complementation, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_functional_complementation, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_functional_complementation, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_functional_complementation, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_functional_complementation, [5, 0, [], []]).
fb_tsv_pred_stats('most-frequent', gene_functional_complementation, [1, [#, [#, 'Lrrk', 5], [#, 'Marf', 5], [#, 'Pink1', 5], [#, oc, 7], [#, 'Nrg', 7], [#, cpx, 8], [#, 'TBPH', 8]]]).
fb_tsv_pred_stats('less-frequent', gene_functional_complementation, [1, [#, [#, 'ATP6AP2', 1], [#, ci, 1], [#, cindr, 1], [#, clu, 1], [#, cn, 1], [#, cpa, 1], [#, csw, 1]]]).
fb_tsv_pred_stats('most-frequent', gene_functional_complementation, [2, [#, [#, 'FBgn0038816', 5], [#, 'FBgn0029870', 5], [#, 'FBgn0029891', 5], [#, 'FBgn0004102', 7], [#, 'FBgn0264975', 7], [#, 'FBgn0041605', 8], [#, 'FBgn0025790', 8]]]).
fb_tsv_pred_stats('less-frequent', gene_functional_complementation, [2, [#, [#, 'FBgn0037671', 1], [#, 'FBgn0004859', 1], [#, 'FBgn0027598', 1], [#, 'FBgn0034087', 1], [#, 'FBgn0000337', 1], [#, 'FBgn0034577', 1], [#, 'FBgn0000382', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_functional_complementation, [3, [#, [#, 'Hsap\\MFN2', 4], [#, 'Hsap\\SOD1', 4], [#, 'Hsap\\L1CAM', 4], [#, 'Hsap\\PARK7', 4], [#, 'Hsap\\PINK1', 5], [#, 'Hsap\\LRRK2', 5], [#, 'Hsap\\TARDBP', 7]]]).
fb_tsv_pred_stats('less-frequent', gene_functional_complementation, [3, [#, [#, 'Hsap\\ATP6AP2', 1], [#, 'Hsap\\EIF4A2', 1], [#, 'Hsap\\DROSHA', 1], [#, 'Hsap\\GRAP', 1], [#, 'Hsap\\SUPT16H', 1], [#, 'Hsap\\SRCAP', 1], [#, 'Hsap\\PIN1', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_functional_complementation, [4, [#, [#, 'FBgn0265221', 4], [#, 'FBgn0024960', 4], [#, 'FBgn0026071', 4], [#, 'FBgn0086735', 4], [#, 'FBgn0086479', 5], [#, 'FBgn0262113', 5], [#, 'FBgn0261084', 7]]]).
fb_tsv_pred_stats('less-frequent', gene_functional_complementation, [4, [#, [#, 'FBgn0262153', 1], [#, 'FBgn0289163', 1], [#, 'FBgn0289120', 1], [#, 'FBgn0286886', 1], [#, 'FBgn0289345', 1], [#, 'FBgn0283711', 1], [#, 'FBgn0288530', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_functional_complementation, [5, [#, [#, 'FBrf0216943', 3], [#, 'FBrf0210782', 3], [#, 'FBrf0146918', 3], [#, 'FBrf0242427', 3], [#, 'FBrf0184161', 4], [#, 'FBrf0252066', 4], [#, 'FBrf0212111', 4]]]).
fb_tsv_pred_stats('less-frequent', gene_functional_complementation, [5, [#, [#, 'FBrf0211368', 1], [#, 'FBrf0101988', 1], [#, 'FBrf0193931', 1], [#, 'FBrf0227402', 1], [#, 'FBrf0230449', 1], [#, 'FBrf0188025', 1], [#, 'FBrf0174932', 1]]]).
fb_tsv_pred_stats('num-columns', gene_genetic_interactions, [6]).
fb_tsv_pred_stats('duplicated-rows', gene_genetic_interactions, [0]).
fb_tsv_pred_stats('total-rows', gene_genetic_interactions, [20321]).
fb_tsv_pred_stats('unique-values', gene_genetic_interactions, [1, 2958, object]).
fb_tsv_pred_stats('unique-values', gene_genetic_interactions, [2, 2958, object]).
fb_tsv_pred_stats('unique-values', gene_genetic_interactions, [3, 4615, object]).
fb_tsv_pred_stats('unique-values', gene_genetic_interactions, [4, 4615, object]).
fb_tsv_pred_stats('unique-values', gene_genetic_interactions, [5, 2, object]).
fb_tsv_pred_stats('unique-values', gene_genetic_interactions, [6, 5428, object]).
fb_tsv_pred_stats('missing-values', gene_genetic_interactions, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_genetic_interactions, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_genetic_interactions, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_genetic_interactions, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_genetic_interactions, [5, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_genetic_interactions, [6, 0, [], []]).
fb_tsv_pred_stats('most-frequent', gene_genetic_interactions, [1, [#, [#, 'Egfr', 168], [#, pbl, 173], [#, 'H', 173], [#, pnr, 174], [#, w, 181], [#, 'Ras85D', 365], [#, 'N', 384]]]).
fb_tsv_pred_stats('less-frequent', gene_genetic_interactions, [1, [#, [#, 'Fit1', 1], [#, 'Gtpx', 1], [#, 'kat-60L1', 1], [#, 'Abl|fax', 1], [#, 'rpr|hid', 1], [#, 'Gfrl', 1], [#, 'mod(mdg4)|sc', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_genetic_interactions, [2, [#, [#, 'FBgn0003731', 168], [#, 'FBgn0003041', 173], [#, 'FBgn0001169', 173], [#, 'FBgn0003117', 174], [#, 'FBgn0003996', 181], [#, 'FBgn0003205', 365], [#, 'FBgn0004647', 384]]]).
fb_tsv_pred_stats('less-frequent', gene_genetic_interactions, [2, [#, [#, 'FBgn0035498', 1], [#, 'FBgn0035438', 1], [#, 'FBgn0037375', 1], [#, 'FBgn0000017|FBgn0014163', 1], [#, 'FBgn0011706|FBgn0003997', 1], [#, 'FBgn0262869', 1], [#, 'FBgn0002781|FBgn0004170', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_genetic_interactions, [3, [#, [#, wg, 110], [#, puc, 117], [#, 'Ras85D', 141], [#, 'Egfr', 158], [#, 'Diap1', 163], [#, 'N', 174], [#, bsk, 181]]]).
fb_tsv_pred_stats('less-frequent', gene_genetic_interactions, [3, [#, [#, 'Su(tor)278-22', 1], [#, 'CG32982', 1], [#, 'CG15599', 1], [#, 'Su(E1)-A', 1], [#, 'mir-13b-2', 1], [#, 'EP1095', 1], [#, 'S(Sev-CycE)D28', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_genetic_interactions, [4, [#, [#, 'FBgn0284084', 110], [#, 'FBgn0243512', 117], [#, 'FBgn0003205', 141], [#, 'FBgn0003731', 158], [#, 'FBgn0260635', 163], [#, 'FBgn0004647', 174], [#, 'FBgn0000229', 181]]]).
fb_tsv_pred_stats('less-frequent', gene_genetic_interactions, [4, [#, [#, 'FBgn0026335', 1], [#, 'FBgn0052982', 1], [#, 'FBgn0030667', 1], [#, 'FBgn0262767', 1], [#, 'FBgn0262421', 1], [#, 'FBgn0062701', 1], [#, 'FBgn0041783', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_genetic_interactions, [5, [#, [#, enhanceable, 7874], [#, suppressible, 12447]]]).
fb_tsv_pred_stats('less-frequent', gene_genetic_interactions, [5, [#, [#, enhanceable, 7874], [#, suppressible, 12447]]]).
fb_tsv_pred_stats('most-frequent', gene_genetic_interactions, [6, [#, [#, 'FBrf0190765', 83], [#, 'FBrf0188528', 88], [#, 'FBrf0190751', 89], [#, 'FBrf0187664', 105], [#, 'FBrf0147055', 149], [#, 'FBrf0199094', 151], [#, 'FBrf0241270', 160]]]).
fb_tsv_pred_stats('less-frequent', gene_genetic_interactions, [6, [#, [#, 'FBrf0221584', 1], [#, 'FBrf0231072', 1], [#, 'FBrf0102734', 1], [#, 'FBrf0190193', 1], [#, 'FBrf0229219', 1], [#, 'FBrf0226148', 1], [#, 'FBrf0208690', 1]]]).
fb_tsv_pred_stats('num-columns', gene_groups_HGNC, [4]).
fb_tsv_pred_stats('duplicated-rows', gene_groups_HGNC, [0]).
fb_tsv_pred_stats('total-rows', gene_groups_HGNC, [1711]).
fb_tsv_pred_stats('unique-values', gene_groups_HGNC, [1, 1699, object]).
fb_tsv_pred_stats('unique-values', gene_groups_HGNC, [2, 1699, object]).
fb_tsv_pred_stats('unique-values', gene_groups_HGNC, [3, 1699, object]).
fb_tsv_pred_stats('unique-values', gene_groups_HGNC, [4, 506, object]).
fb_tsv_pred_stats('missing-values', gene_groups_HGNC, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_groups_HGNC, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_groups_HGNC, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', gene_groups_HGNC, [4, '', 1192]).
fb_tsv_pred_stats('missing-values', gene_groups_HGNC, [4, 1192, [#, ''], [#, 1192]]).
fb_tsv_pred_stats('most-frequent', gene_groups_HGNC, [1, [#, [#, 'FBgg0000526', 2], [#, 'FBgg0000112', 2], [#, 'FBgg0000738', 2], [#, 'FBgg0000588', 2], [#, 'FBgg0000792', 2], [#, 'FBgg0000083', 3], [#, 'FBgg0000279', 3]]]).
fb_tsv_pred_stats('less-frequent', gene_groups_HGNC, [1, [#, [#, 'FBgg0000451', 1], [#, 'FBgg0000651', 1], [#, 'FBgg0001307', 1], [#, 'FBgg0000697', 1], [#, 'FBgg0000072', 1], [#, 'FBgg0000437', 1], [#, 'FBgg0001352', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_groups_HGNC, [2, [#, [#, 'SM', 2], [#, 'INX', 2], [#, 'PRDTF', 2], [#, 'CAC-U', 2], [#, 'E2FTF', 2], [#, 'SYT', 3], [#, 'RPD3-HDA1', 3]]]).
fb_tsv_pred_stats('less-frequent', gene_groups_HGNC, [2, [#, [#, 'TRNA-C-THR-CGT', 1], [#, 'NAT', 1], [#, 'APAS', 1], [#, 'SLC33', 1], [#, 'ADAM', 1], [#, 'TRNA-C-LEU-TAA', 1], [#, 'N-GH', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_groups_HGNC, [3, [#, [#, 'SPLICEOSOMAL SM PROTEINS', 2], [#, 'INNEXINS', 2], [#, 'PAIRED TRANSCRIPTION FACTORS', 2], [#, 'UNCLASSIFIED CALCIUM CHANNEL-FORMING SUBUNITS', 2], [#, 'E2F TRANSCRIPTION FACTORS', 2], [#, 'SYNAPTOTAGMINS & SYNAPTOTAGMIN-LIKE PROTEINS', 3], [#, 'RPD3/HDA1 LYSINE DEACETYLASES', 3]]]).
fb_tsv_pred_stats('less-frequent', gene_groups_HGNC, [3, [#, [#, 'CYTOSOLIC THREONINE-CGT TRANSFER RNAS', 1], [#, 'N-TERMINAL ACETYLTRANSFERASE CATALYTIC SUBUNITS', 1], [#, 'ASPARAGINASES', 1], [#, 'SLC33 ACETYL-COA TRANSPORTERS', 1], [#, 'ADAM METALLOPROTEASES', 1], [#, 'CYTOSOLIC LEUCINE-TAA TRANSFER RNAS', 1], [#, 'N-GLYCOSYL HYDROLASES', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_groups_HGNC, [4, [#, [#, 1281.0, 2], [#, 363.0, 2], [#, 434.0, 2], [#, 816.0, 2], [#, 1254.0, 2], [#, 669.0, 2]]]).
fb_tsv_pred_stats('less-frequent', gene_groups_HGNC, [4, [#, [#, 346.0, 1], [#, 1654.0, 1], [#, 1585.0, 1], [#, 1419.0, 1], [#, 2154.0, 1], [#, 262.0, 1], [#, 1027.0, 1]]]).
fb_tsv_pred_stats('num-columns', gene_group_data, [7]).
fb_tsv_pred_stats('duplicated-rows', gene_group_data, [0]).
fb_tsv_pred_stats('total-rows', gene_group_data, [10957]).
fb_tsv_pred_stats('unique-values', gene_group_data, [1, 1629, object]).
fb_tsv_pred_stats('unique-values', gene_group_data, [2, 1629, object]).
fb_tsv_pred_stats('unique-values', gene_group_data, [3, 1629, object]).
fb_tsv_pred_stats('unique-values', gene_group_data, [4, 389, object]).
fb_tsv_pred_stats('unique-values', gene_group_data, [5, 389, object]).
fb_tsv_pred_stats('unique-values', gene_group_data, [6, 8251, object]).
fb_tsv_pred_stats('unique-values', gene_group_data, [7, 8251, object]).
fb_tsv_pred_stats('missing-values', gene_group_data, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_group_data, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_group_data, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', gene_group_data, [4, '', 1106]).
fb_tsv_pred_stats('missing-values', gene_group_data, [4, 1106, [#, ''], [#, 1106]]).
fb_tsv_pred_stats('null-value-count', gene_group_data, [5, '', 1106]).
fb_tsv_pred_stats('missing-values', gene_group_data, [5, 1106, [#, ''], [#, 1106]]).
fb_tsv_pred_stats('null-value-count', gene_group_data, [6, '', 405]).
fb_tsv_pred_stats('missing-values', gene_group_data, [6, 405, [#, ''], [#, 405]]).
fb_tsv_pred_stats('null-value-count', gene_group_data, [7, '', 405]).
fb_tsv_pred_stats('missing-values', gene_group_data, [7, 405, [#, ''], [#, 405]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [1, [#, [#, 'FBgg0000128', 98], [#, 'FBgg0001176', 105], [#, 'FBgg0001186', 106], [#, 'FBgg0001078', 114], [#, 'FBgg0000097', 120], [#, 'FBgg0001077', 136], [#, 'FBgg0000732', 272]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [1, [#, [#, 'FBgg0001756', 1], [#, 'FBgg0000834', 1], [#, 'FBgg0001677', 1], [#, 'FBgg0000396', 1], [#, 'FBgg0000921', 1], [#, 'FBgg0000512', 1], [#, 'FBgg0000662', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [2, [#, [#, 'E3-RING-U', 98], [#, 'CPR', 105], [#, 'CBD', 106], [#, 'SP-T', 114], [#, 'OR', 120], [#, 'S1A-NPH', 136], [#, 'ZF-C2H2', 272]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [2, [#, [#, 'UFM1', 1], [#, 'GH47', 1], [#, 'GH99', 1], [#, 'TRNA-C-ASP', 1], [#, 'MDH', 1], [#, 'KCN', 1], [#, 'SLC7', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [3, [#, [#, 'UNCLASSIFIED RING DOMAIN UBIQUITIN LIGASES', 98], [#, 'CPR CUTICLE PROTEIN FAMILY', 105], [#, 'CHITIN BINDING DOMAIN-CONTAINING PROTEINS', 106], [#, 'S1A SERINE PROTEASES - TRYPSIN-LIKE', 114], [#, 'ODORANT RECEPTORS', 120], [#, 'S1A NON-PEPTIDASE HOMOLOGS', 136], [#, 'C2H2 ZINC FINGER TRANSCRIPTION FACTORS', 272]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [3, [#, [#, 'UFM1 PROTEINS', 1], [#, 'CLASS I ALPHA-MANNOSIDASES', 1], [#, 'ENDO-ALPHA MANNOSIDASES', 1], [#, 'CYTOSOLIC ASPARTIC ACID TRANSFER RNAS', 1], [#, 'MALATE DEHYDROGENASES', 1], [#, 'POTASSIUM CHANNEL SUBUNITS', 1], [#, 'SLC7 FAMILY OF AMINO ACID TRANSPORTERS', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [4, [#, [#, 'FBgg0001178', 162], [#, 'FBgg0001841', 178], [#, 'FBgg0001076', 190], [#, 'FBgg0000729', 277], [#, 'FBgg0000537', 318], [#, 'FBgg0000686', 392]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [4, [#, [#, 'FBgg0001357', 1], [#, 'FBgg0000408', 1], [#, 'FBgg0001039', 1], [#, 'FBgg0001844', 2], [#, 'FBgg0001637', 2], [#, 'FBgg0000582', 2], [#, 'FBgg0000130', 2]]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [5, [#, [#, 'CP', 162], [#, 'OXPHOS-V', 178], [#, 'S1AP', 190], [#, 'ZN-TF', 277], [#, 'SPL-INT', 318], [#, 'SLC', 392]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [5, [#, [#, 'ETH', 1], [#, 'TRNA-C-SEC', 1], [#, 'OXRPX', 1], [#, 'PSM-G', 2], [#, 'LADAC', 2], [#, 'IC', 2], [#, 'RP', 2]]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [6, [#, [#, 'FBgn0261933', 10], [#, 'FBgn0000426', 13], [#, 'FBgn0261790', 13], [#, 'FBgn0261791', 13], [#, 'FBgn0023167', 13], [#, 'FBgn0262601', 13]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [6, [#, [#, 'FBgn0002565', 1], [#, 'FBgn0052671', 1], [#, 'FBgn0262518', 1], [#, 'FBgn0037364', 1], [#, 'FBgn0005586', 1], [#, 'FBgn0015791', 1], [#, 'FBgn0015795', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_group_data, [7, [#, [#, 'SmD1', 10], [#, 'SmF', 13], [#, 'SmE', 13], [#, 'SNRPG', 13], [#, 'SmD3', 13], [#, 'SmB', 13]]]).
fb_tsv_pred_stats('less-frequent', gene_group_data, [7, [#, [#, 'Lsp2', 1], [#, 'Rab9Fa', 1], [#, 'Rab8', 1], [#, 'Rab23', 1], [#, 'Rab3', 1], [#, 'Rab14', 1], [#, 'Rab7', 1]]]).
fb_tsv_pred_stats('num-columns', gene_map_table, [6]).
fb_tsv_pred_stats('duplicated-rows', gene_map_table, [0]).
fb_tsv_pred_stats('total-rows', gene_map_table, [245829]).
fb_tsv_pred_stats('unique-values', gene_map_table, [1, 542, object]).
fb_tsv_pred_stats('unique-values', gene_map_table, [2, 245829, object]).
fb_tsv_pred_stats('unique-values', gene_map_table, [3, 245829, object]).
fb_tsv_pred_stats('unique-values', gene_map_table, [4, 1716, object]).
fb_tsv_pred_stats('unique-values', gene_map_table, [5, 8138, object]).
fb_tsv_pred_stats('unique-values', gene_map_table, [6, 17721, object]).
fb_tsv_pred_stats('missing-values', gene_map_table, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_map_table, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_map_table, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', gene_map_table, [4, '', 208914]).
fb_tsv_pred_stats('missing-values', gene_map_table, [4, 208914, [#, ''], [#, 208914]]).
fb_tsv_pred_stats('null-value-count', gene_map_table, [5, '', 222613]).
fb_tsv_pred_stats('null-value-count', gene_map_table, [5, -, 105]).
fb_tsv_pred_stats('missing-values', gene_map_table, [5, 222718, [#, '', -], [#, 222613, 105]]).
fb_tsv_pred_stats('null-value-count', gene_map_table, [6, '', 227926]).
fb_tsv_pred_stats('missing-values', gene_map_table, [6, 227926, [#, ''], [#, 227926]]).
fb_tsv_pred_stats('most-frequent', gene_map_table, [1, [#, [#, 'Dper', 18137], [#, 'Dana', 18521], [#, 'Dwil', 18542], [#, 'Dyak', 19506], [#, 'Dsim', 20501], [#, 'Dpse', 22460], [#, 'Dmel', 31755]]]).
fb_tsv_pred_stats('less-frequent', gene_map_table, [1, [#, [#, 'Dmos', 1], [#, 'Dsch', 1], [#, 'Dprg', 1], [#, 'Dpme', 1], [#, 'Dmds', 1], [#, 'Dbis', 1], [#, 'Dsut', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_map_table, [4, [#, [#, '1-', 1161], [#, 'XL-', 1883], [#, '4-', 2182], [#, 'XR-', 2261], [#, '3-', 3567], [#, '2-', 4139]]]).
fb_tsv_pred_stats('less-frequent', gene_map_table, [4, [#, [#, '2-[90]', 1], [#, '3-50-62', 1], [#, '1-38.4', 1], [#, '3-[76]', 1], [#, '3-[25]', 1], [#, '1-[61]', 1], [#, '2-[24]', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_map_table, [5, [#, [#, '61-100', 47], [#, '39D5-39D5', 47], [#, '21E2-21E2', 73], [#, '56E2-56E2', 123], [#, '21-60', 132]]]).
fb_tsv_pred_stats('less-frequent', gene_map_table, [5, [#, [#, '92A2-92A3', 1], [#, '18E5-18F1', 1], [#, '78A2-78B1', 1], [#, '95C5-95C8', 1], [#, '18B8-18B8', 1], [#, '61F2-61F3', 1], [#, '53A2-53A3', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_map_table, [6, [#, [#, '2R:10351533..10352161(-1)', 3], [#, '2R:13209809..13212237(-1)', 3], [#, '3R:21365609..21368279(1)', 3], [#, '2R:21667541..21669916(-1)', 4], [#, '3R:13813109..13814648(1)', 4], [#, '3R:21355692..21359768(-1)', 5]]]).
fb_tsv_pred_stats('less-frequent', gene_map_table, [6, [#, [#, '3R:19373757..19374789(-1)', 1], [#, '3L:11574764..11577044(-1)', 1], [#, '3L:11572797..11574663(-1)', 1], [#, 'X:20178755..20179729(-1)', 1], [#, '3L:10692530..10693167(-1)', 1], [#, '3R:25493481..25495161(-1)', 1], [#, 'X:20186630..20187604(-1)', 1]]]).
fb_tsv_pred_stats('num-columns', gene_rpkm_matrix, [170]).
fb_tsv_pred_stats('duplicated-rows', gene_rpkm_matrix, [0]).
fb_tsv_pred_stats('total-rows', gene_rpkm_matrix, [17747]).
fb_tsv_pred_stats('unique-values', gene_rpkm_matrix, [1, 17747, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_matrix, [2, 17747, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_matrix, [3, 11214, object]).
fb_tsv_pred_stats('num-columns', gene_rpkm_report, [12]).
fb_tsv_pred_stats('duplicated-rows', gene_rpkm_report, [0]).
fb_tsv_pred_stats('total-rows', gene_rpkm_report, [2928711]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [1, 1, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [2, 17748, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [3, 17748, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [4, 7, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [5, 7, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [6, 166, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [7, 166, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [8, 5113, int64]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [9, 8, int64]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [10, 6776, object]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [11, 5412, int64]).
fb_tsv_pred_stats('unique-values', gene_rpkm_report, [12, 2, object]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [5, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [6, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [7, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [8, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [9, 0, [], []]).
fb_tsv_pred_stats('null-value-count', gene_rpkm_report, [10, '', 60536]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [10, 60536, [#, ''], [#, 60536]]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [11, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_rpkm_report, [12, 0, [], []]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [1, [#, [#, 'Dmel_R6.53', 2928711]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [1, [#, [#, 'Dmel_R6.53', 2928711]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [2, [#, [#, 'FBgn0053410', 166], [#, 'FBgn0053411', 166], [#, 'FBgn0053412', 166], [#, 'FBgn0053413', 166], [#, 'FBgn0053414', 166], [#, 'FBgn0053408', 166], [#, 'FBgn0038694', 166]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [2, [#, [#, 'FBgn0267595', 40], [#, 'FBgn0013691', 42], [#, 'FBgn0013673', 42], [#, 'FBgn0013701', 42], [#, 'FBgn0013700', 42], [#, 'FBgn0013674', 42], [#, 'FBgn0013675', 42]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [3, [#, [#, '5SrRNA:CR33410', 166], [#, '5SrRNA:CR33411', 166], [#, '5SrRNA:CR33412', 166], [#, '5SrRNA:CR33413', 166], [#, '5SrRNA:CR33414', 166], [#, '5SrRNA:CR33408', 166], [#, 'CG5217', 166]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [3, [#, [#, 'CR45933', 40], [#, 'mt:tRNA:Asp-GTC', 42], [#, 'mt:ATPase8', 42], [#, 'mt:tRNA:Asn-GTT', 42], [#, 'mt:tRNA:Met-CAT', 42], [#, 'mt:CoI', 42], [#, 'mt:CoII', 42]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [4, [#, [#, 'FBlc0003342', 35368], [#, 'FBlc0000060', 211421], [#, 'FBlc0000260', 424996], [#, 'FBlc0000236', 477917], [#, 'FBlc0000206', 513566], [#, 'FBlc0000085', 528553], [#, 'FBlc0003498', 736890]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [4, [#, [#, 'FBlc0003342', 35368], [#, 'FBlc0000060', 211421], [#, 'FBlc0000260', 424996], [#, 'FBlc0000236', 477917], [#, 'FBlc0000206', 513566], [#, 'FBlc0000085', 528553], [#, 'FBlc0003498', 736890]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [5, [#, [#, 'Knoblich_Neural_Cell_RNA-Seq', 35368], [#, 'BCM_1_RNAseq', 211421], [#, 'modENCODE_mRNA-Seq_cell.B', 424996], [#, 'modENCODE_mRNA-Seq_treatments', 477917], [#, 'modENCODE_mRNA-Seq_tissues', 513566], [#, 'modENCODE_mRNA-Seq_development', 528553], [#, 'FlyAtlas2', 736890]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [5, [#, [#, 'Knoblich_Neural_Cell_RNA-Seq', 35368], [#, 'BCM_1_RNAseq', 211421], [#, 'modENCODE_mRNA-Seq_cell.B', 424996], [#, 'modENCODE_mRNA-Seq_treatments', 477917], [#, 'modENCODE_mRNA-Seq_tissues', 513566], [#, 'modENCODE_mRNA-Seq_development', 528553], [#, 'FlyAtlas2', 736890]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [6, [#, [#, 'FBlc0000235', 17711], [#, 'FBlc0000234', 17711], [#, 'FBlc0000233', 17711], [#, 'FBlc0000229', 17711], [#, 'FBlc0000217', 17711], [#, 'FBlc0000251', 17711], [#, 'FBlc0000250', 17711]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [6, [#, [#, 'FBlc0003639', 17545], [#, 'FBlc0003637', 17545], [#, 'FBlc0003636', 17545], [#, 'FBlc0003635', 17545], [#, 'FBlc0003634', 17545], [#, 'FBlc0003633', 17545], [#, 'FBlc0003632', 17545]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [7, [#, [#, mE_mRNA_P8_fat, 17711], [#, mE_mRNA_WPP_saliv, 17711], [#, mE_mRNA_WPP_fat, 17711], [#, mE_mRNA_L3_Wand_imag_disc, 17711], [#, mE_mRNA_A_MateM_4d_testis, 17711], [#, mE_mRNA_A_4d_Paraquat_10mM, 17711], [#, mE_mRNA_A_4d_Paraquat_5mM, 17711]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [7, [#, [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_FatBody', 17545], [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Carcass', 17545], [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Crop', 17545], [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Brain', 17545], [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Whole', 17545], [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Virgin_Spermathecum', 17545], [#, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Mated_Spermathecum', 17545]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [8, [#, [#, 6, 67267], [#, 5, 76452], [#, 4, 89006], [#, 3, 106255], [#, 2, 139559], [#, 1, 256023], [#, 0, 1229976]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [8, [#, [#, 4755, 1], [#, 3974, 1], [#, 2566, 1], [#, 3991, 1], [#, 10214, 1], [#, 15694, 1], [#, 3308, 1]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [9, [#, [#, 6, 94410], [#, 5, 101416], [#, 4, 187170], [#, 3, 362858], [#, 2, 439025], [#, 1, 501837], [#, 0, 1229976]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [9, [#, [#, 7, 12019], [#, 6, 94410], [#, 5, 101416], [#, 4, 187170], [#, 3, 362858], [#, 2, 439025], [#, 1, 501837]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [10, [#, [#, 45.0, 5314], [#, 72, 7512], [#, 44.0, 8878], [#, 72.0, 11038], [#, 73, 11536], [#, 135.0, 17264]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [10, [#, [#, 2235, 15], [#, 939, 42], [#, 667, 42], [#, 155, 42], [#, 1536, 42], [#, 354, 42], [#, 290, 42]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [11, [#, [#, 595, 4980], [#, 74, 5098], [#, 45, 5872], [#, 44, 9390], [#, 135, 17014], [#, 73, 17014], [#, 72, 18668]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [11, [#, [#, 62, 42], [#, 63, 42], [#, 16924, 82], [#, 230, 82], [#, 3528, 82], [#, 193, 82], [#, 16779, 84]]]).
fb_tsv_pred_stats('most-frequent', gene_rpkm_report, [12, [#, [#, 'Total', 73206], [#, 'Unique', 2855505]]]).
fb_tsv_pred_stats('less-frequent', gene_rpkm_report, [12, [#, [#, 'Total', 73206], [#, 'Unique', 2855505]]]).
fb_tsv_pred_stats('num-columns', gene_snapshots, [5]).
fb_tsv_pred_stats('duplicated-rows', gene_snapshots, [0]).
fb_tsv_pred_stats('total-rows', gene_snapshots, [13985]).
fb_tsv_pred_stats('unique-values', gene_snapshots, [1, 13985, object]).
fb_tsv_pred_stats('unique-values', gene_snapshots, [2, 13985, object]).
fb_tsv_pred_stats('unique-values', gene_snapshots, [3, 7833, object]).
fb_tsv_pred_stats('unique-values', gene_snapshots, [4, 61, object]).
fb_tsv_pred_stats('unique-values', gene_snapshots, [5, 3652, object]).
fb_tsv_pred_stats('missing-values', gene_snapshots, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_snapshots, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', gene_snapshots, [3, -, 6153]).
fb_tsv_pred_stats('missing-values', gene_snapshots, [3, 6153, [#, -], [#, 6153]]).
fb_tsv_pred_stats('null-value-count', gene_snapshots, [4, -, 10334]).
fb_tsv_pred_stats('missing-values', gene_snapshots, [4, 10334, [#, -], [#, 10334]]).
fb_tsv_pred_stats('missing-values', gene_snapshots, [5, 0, [], []]).
fb_tsv_pred_stats('most-frequent', gene_snapshots, [3, [#, [#, 'Carbonic anhydrase 2', 1], [#, 'PRL-1 phosphatase', 1], [#, tricornered, 1], [#, kurz, 1], [#, 'beaten path IV', 1], [#, 'male sterile (2) 35Ci', 1]]]).
fb_tsv_pred_stats('less-frequent', gene_snapshots, [3, [#, [#, 'Receptor expression enhancing protein A', 1], [#, tricornered, 1], [#, 'PRL-1 phosphatase', 1], [#, 'Carbonic anhydrase 2', 1], [#, 'sugar transporter 3', 1], [#, spt4, 1], [#, 'Imitation SWI', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_snapshots, [4, [#, [#, 20190919.0, 144], [#, 20190926.0, 198], [#, 20190912.0, 206], [#, 20190321.0, 222], [#, 20190307.0, 862], [#, 20190314.0, 1067]]]).
fb_tsv_pred_stats('less-frequent', gene_snapshots, [4, [#, [#, 20220901.0, 1], [#, 20200924.0, 1], [#, 20200730.0, 1], [#, 20200709.0, 1], [#, 20190725.0, 1], [#, 20210204.0, 1], [#, 20160630.0, 1]]]).
fb_tsv_pred_stats('most-frequent', gene_snapshots, [5, [#, [#, 'stumps (stumps) encodes a fibroblast growth factor (FGF) scaffolding protein. Upon FGF receptor activation, it mediates the recruitment of the phosphatase encoded by csw, which is essential for the activation of the MAPK pathway.', 1], [#, 'lectin-24Db (lectin-24Db) encodes a protein that interacts specifically with fucose and mannose.', 1], [#, 'Apoptosis inducing factor (AIF) encodes a phylogenetically conserved redox-active flavoprotein that contributes to cell death and oxidative phosphorylation in mitochondria.', 1], [#, 'Lsm10 (Lsm10) encodes a core component of the U7 snRNP complex, which functions in 3\' end processing of the non-polyadenylated, replication-dependent histone mRNAs. The product of Lsm10 is part of the heptameric Sm ring complex that binds directly to U7 snRNA. It replaces the protein encoded by SmD1 in the canonical heptameric Sm protein ring that binds spliceosomal snRNAs.', 1], [#, 'prenyl protease type I (ste24a) encodes a protein that, along with the products of ste24b and ste24c, are paralogous (tandemly duplicated) type I prenyl proteases.', 1], [#, 'dachshund (dac) encodes a transcriptional cofactor that physically interacts with several other retinal determination proteins, including those encoded by eya and so, and regulates eye, leg, gonad, and brain development.', 1], [#, 'Contributions welcome.', 10334]]]).
fb_tsv_pred_stats('less-frequent', gene_snapshots, [5, [#, [#, 'Down syndrome cell adhesion molecule 2 (Dscam2) encodes a transmembrane protein. Its alternative splicing produces two biochemically distinct homophilic binding proteins expressed in different cells. It can mediate both repulsion and adhesion between neurons and contributes to boundary formation, neurite targeting and synapse formation in the brain.', 1], [#, 'prenyl protease type I (ste24a) encodes a protein that, along with the products of ste24b and ste24c, are paralogous (tandemly duplicated) type I prenyl proteases.', 1], [#, 'Lsm10 (Lsm10) encodes a core component of the U7 snRNP complex, which functions in 3\' end processing of the non-polyadenylated, replication-dependent histone mRNAs. The product of Lsm10 is part of the heptameric Sm ring complex that binds directly to U7 snRNA. It replaces the protein encoded by SmD1 in the canonical heptameric Sm protein ring that binds spliceosomal snRNAs.', 1], [#, 'Apoptosis inducing factor (AIF) encodes a phylogenetically conserved redox-active flavoprotein that contributes to cell death and oxidative phosphorylation in mitochondria.', 1], [#, 'lectin-24Db (lectin-24Db) encodes a protein that interacts specifically with fucose and mannose.', 1], [#, 'stumps (stumps) encodes a fibroblast growth factor (FGF) scaffolding protein. Upon FGF receptor activation, it mediates the recruitment of the phosphatase encoded by csw, which is essential for the activation of the MAPK pathway.', 1], [#, 'expanded (ex) encodes a FERM-domain containing protein that localizes to apical cell-cell junctions, where it promotes Hippo signaling by physically interacting with multiple Hippo pathway components. Through its regulation of Hippo signaling, the product of ex functions as an inhibitor of growth.', 1]]]).
fb_tsv_pred_stats('num-columns', ncRNA_genes_, [2]).
fb_tsv_pred_stats('duplicated-rows', ncRNA_genes_, [145327]).
fb_tsv_pred_stats('total-rows', ncRNA_genes_, [212303]).
fb_tsv_pred_stats('unique-values', ncRNA_genes_, [1, 66975, object]).
fb_tsv_pred_stats('unique-values', ncRNA_genes_, [2, 3, object]).
fb_tsv_pred_stats('missing-values', ncRNA_genes_, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', ncRNA_genes_, [2, '', 21161]).
fb_tsv_pred_stats('missing-values', ncRNA_genes_, [2, 21161, [#, ''], [#, 21161]]).
fb_tsv_pred_stats('most-frequent', ncRNA_genes_, [1, [#, [#, '                         \\gca_accession\\: \\GCA_000001215.4\\, ', 4566], [#, '                         \\exons\\: [', 4566], [#, '          ', 4566], [#, '                    }', 5311], [#, '                    ', 5534], [#, '                              ', 6494], [#, '               ], ', 13710]]]).
fb_tsv_pred_stats('less-frequent', ncRNA_genes_, [1, [#, [#, '     \\metaData\\: ', 1], [#, '                                   \\endPosition\\: 23114377', 1], [#, '                                   \\startPosition\\: 23113577, ', 1], [#, '                                   \\endPosition\\: 23114049', 1], [#, '                    \\geneId\\: \\FLYBASE:FBgn0266009\\, ', 1], [#, '                    \\symbol\\: \\lncRNA:CR44782\\, ', 1], [#, '                    \\url\\: \\http://flybase.org/reports/FBgn0266009.html\\, ', 1]]]).
fb_tsv_pred_stats('most-frequent', ncRNA_genes_, [2, [#, [#, '}RR42320\\', 1], [#, '}RR42263\\', 1]]]).
fb_tsv_pred_stats('less-frequent', ncRNA_genes_, [2, [#, [#, '}RR42263\\', 1], [#, '}RR42320\\', 1]]]).
fb_tsv_pred_stats('num-columns', pathway_group_data, [7]).
fb_tsv_pred_stats('duplicated-rows', pathway_group_data, [0]).
fb_tsv_pred_stats('total-rows', pathway_group_data, [992]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [1, 71, object]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [2, 71, object]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [3, 71, object]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [4, 18, object]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [5, 18, object]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [6, 658, object]).
fb_tsv_pred_stats('unique-values', pathway_group_data, [7, 658, object]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', pathway_group_data, [4, '', 17]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [4, 17, [#, ''], [#, 17]]).
fb_tsv_pred_stats('null-value-count', pathway_group_data, [5, '', 17]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [5, 17, [#, ''], [#, 17]]).
fb_tsv_pred_stats('null-value-count', pathway_group_data, [6, '', 17]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [6, 17, [#, ''], [#, 17]]).
fb_tsv_pred_stats('null-value-count', pathway_group_data, [7, '', 17]).
fb_tsv_pred_stats('missing-values', pathway_group_data, [7, 17, [#, ''], [#, 17]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [1, [#, [#, 'FBgg0001196', 38], [#, 'FBgg0001069', 42], [#, 'FBgg0000891', 42], [#, 'FBgg0000981', 43], [#, 'FBgg0000916', 44], [#, 'FBgg0000892', 45], [#, 'FBgg0000980', 56]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [1, [#, [#, 'FBgg0000941', 1], [#, 'FBgg0001093', 1], [#, 'FBgg0000917', 1], [#, 'FBgg0000910', 1], [#, 'FBgg0000975', 1], [#, 'FBgg0000958', 1], [#, 'FBgg0000889', 1]]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [2, [#, [#, 'IMD-N', 38], [#, 'NTCH-P', 42], [#, 'WNT-TCFN', 42], [#, 'HH-SMOP', 43], [#, 'SWH-N', 44], [#, 'WNT-TCFP', 45], [#, 'HH-SMON', 56]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [2, [#, [#, 'SEVL', 1], [#, 'ACT-SMAD2', 1], [#, 'SWH', 1], [#, 'INS', 1], [#, 'PVR-P', 1], [#, 'EGFR-RTK', 1], [#, 'WNT-TCF', 1]]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [3, [#, [#, 'Negative Regulators of Imd Signaling Pathway', 38], [#, 'Positive Regulators of Notch Signaling Pathway', 42], [#, 'Negative Regulators of Wnt-TCF Signaling Pathway', 42], [#, 'Positive Regulators of Hedgehog Signaling Pathway', 43], [#, 'Negative Regulators of Hippo Signaling Pathway', 44], [#, 'Positive Regulators of Wnt-TCF Signaling Pathway', 45], [#, 'Negative Regulators of Hedgehog Signaling Pathway', 56]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [3, [#, [#, 'Sevenless Signaling Pathway', 1], [#, 'Activin Signaling Pathway', 1], [#, 'Hippo Signaling Pathway', 1], [#, 'Insulin-like Receptor Signaling Pathway', 1], [#, 'Positive Regulators of Pvr Signaling Pathway', 1], [#, 'EGFR Signaling Pathway', 1], [#, 'Wnt-TCF Signaling Pathway', 1]]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [4, [#, [#, 'FBgg0000910', 77], [#, 'FBgg0001059', 79], [#, 'FBgg0000958', 83], [#, 'FBgg0000917', 87], [#, 'FBgg0001068', 89], [#, 'FBgg0000889', 100], [#, 'FBgg0000978', 108]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [4, [#, [#, 'FBgg0001792', 6], [#, 'FBgg0001093', 14], [#, 'FBgg0001560', 23], [#, 'FBgg0000941', 30], [#, 'FBgg0000934', 31], [#, 'FBgg0000967', 35]]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [5, [#, [#, 'INS', 77], [#, 'TL-CAN', 79], [#, 'EGFR-RTK', 83], [#, 'SWH', 87], [#, 'NTCH', 89], [#, 'WNT-TCF', 100], [#, 'HH-SMO', 108]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [5, [#, [#, 'CGAS-STING', 6], [#, 'ACT-SMAD2', 14], [#, 'TNF', 23], [#, 'SEVL', 30], [#, 'TORS', 31], [#, 'FGFR', 35]]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [6, [#, [#, 'FBgn0010269', 6], [#, 'FBgn0004177', 6], [#, 'FBgn0262432', 6], [#, 'FBgn0283468', 6], [#, 'FBgn0003256', 6], [#, 'FBgn0004638', 6]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [6, [#, [#, 'FBgn0262408', 1], [#, 'FBgn0027539', 1], [#, 'FBgn0001987', 1], [#, 'FBgn0005632', 1], [#, 'FBgn0043364', 1], [#, 'FBgn0000395', 1], [#, 'FBgn0261952', 1]]]).
fb_tsv_pred_stats('most-frequent', pathway_group_data, [7, [#, [#, 'Dsor1', 6], [#, mts, 6], [#, 'mir-8', 6], [#, slmb, 6], [#, rl, 6], [#, drk, 6]]]).
fb_tsv_pred_stats('less-frequent', pathway_group_data, [7, [#, [#, 'mir-5', 1], [#, lili, 1], [#, 'Gli', 1], [#, faf, 1], [#, cbt, 1], [#, 'cv-2', 1], [#, srw, 1]]]).
fb_tsv_pred_stats('num-columns', physical_interactions_mitab, [42]).
fb_tsv_pred_stats('duplicated-rows', physical_interactions_mitab, [0]).
fb_tsv_pred_stats('total-rows', physical_interactions_mitab, [51247]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [1, 4623, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [2, 5257, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [3, 4611, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [4, 5242, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [5, 4623, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [6, 5257, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [7, 111, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [8, 4823, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [9, 4941, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [10, 5, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [11, 4, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [12, 3, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [13, 1, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [14, 51247, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [15, 1, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [16, 1, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [17, 7, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [18, 8, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [19, 9, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [20, 9, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [21, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [22, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [23, 1, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [24, 1, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [25, 34325, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [26, 1037, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [27, 1040, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [28, 19939, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [29, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [30, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [31, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [32, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [33, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [34, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [35, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [36, 3, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [37, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [38, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [39, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [40, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [41, 2, object]).
fb_tsv_pred_stats('unique-values', physical_interactions_mitab, [42, 2, object]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [3, -, 23]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [3, 23, [#, -], [#, 23]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [4, -, 59]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [4, 59, [#, -], [#, 59]]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [5, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [6, 0, [], []]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [7, -, 231]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [7, 231, [#, -], [#, 231]]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [8, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [9, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [10, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [11, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [12, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [13, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [14, 0, [], []]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [15, -, 51247]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [15, 51247, [#, -], [#, 51247]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [16, -, 51247]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [16, 51247, [#, -], [#, 51247]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [17, -, 50635]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [17, 50635, [#, -], [#, 50635]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [18, -, 50634]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [18, 50634, [#, -], [#, 50634]]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [19, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [20, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [21, 0, [], []]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [22, 0, [], []]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [23, -, 51247]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [23, 51247, [#, -], [#, 51247]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [24, -, 51247]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [24, 51247, [#, -], [#, 51247]]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [25, 0, [], []]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [26, -, 27320]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [26, 27320, [#, -], [#, 27320]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [27, -, 27569]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [27, 27569, [#, -], [#, 27569]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [28, -, 180]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [28, 180, [#, -], [#, 180]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [29, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [29, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [29, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [30, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [30, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [30, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [31, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [31, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [31, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [32, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [32, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [32, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [33, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [33, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [33, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [34, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [34, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [34, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [35, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [35, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [35, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [36, '', 1]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [36, 1, [#, ''], [#, 1]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [37, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [37, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [37, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [38, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [38, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [38, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [39, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [39, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [39, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [40, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [40, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [40, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [41, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [41, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [41, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [42, '', 1]).
fb_tsv_pred_stats('null-value-count', physical_interactions_mitab, [42, -, 51246]).
fb_tsv_pred_stats('missing-values', physical_interactions_mitab, [42, 51247, [#, '', -], [#, 1, 51246]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [1, [#, [#, 'FBgn0001199', 188], [#, 'FBgn0000212', 204], [#, 'FBgn0015239', 209], [#, 'FBgn0003944', 220], [#, 'FBgn0000014', 237], [#, 'FBgn0000283', 259], [#, 'FBgn0003607', 275]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [1, [#, [#, 'FBgn0028412', 1], [#, 'FBgn0003312', 1], [#, 'FBgn0015773', 1], [#, 'FBgn0032644', 1], [#, 'FBgn0033685', 1], [#, 'FBgn0028847', 1], [#, 'FBgn0038880', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [2, [#, [#, 'FBgn0264922', 155], [#, 'FBgn0034970', 159], [#, 'FBgn0037555', 168], [#, 'FBgn0263979', 175], [#, 'FBgn0262866', 178], [#, 'FBgn0262739', 185], [#, 'FBgn0266411', 188]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [2, [#, [#, 'FBgn0036437', 1], [#, 'FBgn0066365', 1], [#, 'FBgn0040773', 1], [#, 'FBgn0019929', 1], [#, 'FBgn0033192', 1], [#, 'FBgn0000147', 1], [#, 'FBgn0052450', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [3, [#, [#, 'entrez gene/locuslink:318847', 188], [#, 'CG5942|entrez gene/locuslink:39744', 204], [#, 'CG7199|entrez gene/locuslink:40378', 209], [#, 'CG10388|entrez gene/locuslink:42034', 220], [#, 'CG10325|entrez gene/locuslink:42037', 237], [#, 'CG6384|entrez gene/locuslink:41848', 259], [#, 'CG8409|entrez gene/locuslink:34119', 275]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [3, [#, [#, 'CG6541|entrez gene/locuslink:34578', 1], [#, 'CG8611|entrez gene/locuslink:32725', 1], [#, 'CG11490|entrez gene/locuslink:33184', 1], [#, 'CG42341|entrez gene/locuslink:40305', 1], [#, 'CG32213|entrez gene/locuslink:317918', 1], [#, 'CG1976|entrez gene/locuslink:43758', 1], [#, 'CG2943|entrez gene/locuslink:40933', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [4, [#, [#, 'CG4494|entrez gene/locuslink:33981', 155], [#, 'CG4005|entrez gene/locuslink:37851', 159], [#, 'CG9638|entrez gene/locuslink:40966', 168], [#, 'CG4236|entrez gene/locuslink:41836', 175], [#, 'CG17596|entrez gene/locuslink:33139', 178], [#, 'CG6671|entrez gene/locuslink:36544', 185], [#, 'CG45051|entrez gene/locuslink:43580', 188]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [4, [#, [#, 'CG5048|entrez gene/locuslink:39598', 1], [#, 'CG4705|entrez gene/locuslink:34540', 1], [#, 'CG11659|entrez gene/locuslink:42352', 1], [#, 'CR43012|entrez gene/locuslink:12798490', 1], [#, 'CG44086|entrez gene/locuslink:34032', 1], [#, 'entrez gene/locuslink:3772715', 1], [#, 'CG12090|entrez gene/locuslink:38176', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [5, [#, [#, 'His3(gene name)', 188], [#, 'brm(gene name)', 204], [#, 'Hr78(gene name)', 209], [#, 'Ubx(gene name)', 220], [#, 'abd-A(gene name)', 237], [#, 'Cp190(gene name)', 259], [#, 'flybase:\\Su(var)205\\(gene name)', 275]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [5, [#, [#, 'Mst33A(gene name)', 1], [#, 'sad(gene name)', 1], [#, 'NetA(gene name)', 1], [#, 'CG5131(gene name)', 1], [#, 'OSCP1(gene name)', 1], [#, 'CG9014(gene name)', 1], [#, 'SIFaR(gene name)', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [6, [#, [#, 'Sumo(gene name)', 155], [#, 'yki(gene name)', 159], [#, 'Ada2b(gene name)', 168], [#, 'Caf1-55(gene name)', 175], [#, 'S6kII(gene name)', 178], [#, 'AGO1(gene name)', 185], [#, 'sima(gene name)', 188]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [6, [#, [#, 'Dnaaf6(gene name)', 1], [#, 'dyl(gene name)', 1], [#, 'COX7C(gene name)', 1], [#, 'Ser7(gene name)', 1], [#, 'Corin(gene name)', 1], [#, 'aurA(gene name)', 1], [#, 'CG32450(gene name)', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [7, [#, [#, 'psi-mi:\\MI:0397\\(two hybrid array)', 2067], [#, 'psi-mi:\\MI:0027\\(cosedimentation)', 4090], [#, 'psi-mi:\\MI:0096\\(pull down)', 4202], [#, 'psi-mi:\\MI:0006\\(anti bait coimmunoprecipitation)', 5251], [#, 'psi-mi:\\MI:0018\\(two hybrid)', 5958], [#, 'psi-mi:\\MI:0046\\(experimental knowledge based)', 10942], [#, 'psi-mi:\\MI:0007\\(anti tag coimmunoprecipitation)', 12395]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [7, [#, [#, 'psi-mi:\\MI:0042\\(electron paramagnetic resonance)', 1], [#, 'psi-mi:\\MI:0928\\(filter trap assay)', 1], [#, 'psi-mi:\\MI:1183\\(nuclease footprinting)', 1], [#, 'psi-mi:\\MI:1238\\(mass spectrometry studies of subunit exchange)', 1], [#, 'psi-mi:\\MI:0370\\(tox-r dimerization assay)', 1], [#, 'psi-mi:\\MI:0048\\(filamentous phage display)', 1], [#, 'psi-mi:\\MI:0605\\(enzymatic footprinting)', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [8, [#, [#, 'Friedman A.A. (2011)', 400], [#, 'Rhee D.Y. (2014)', 618], [#, 'Vinayagam A. (2016)', 1808], [#, 'Shokri L. (2019)', 2109], [#, 'Hu Y. (2017.6.13)', 3634], [#, 'Anger A.M. (2013)', 3828], [#, 'Guruharsha K.G. (2011)', 10947]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [8, [#, [#, 'Geng J. (2023)', 1], [#, 'Mellone B.G. (2011)', 1], [#, 'Lim Y.M. (2013)', 1], [#, 'Tran T.A. (2013)', 1], [#, 'Ooe N. (2007)', 1], [#, 'Jiang L. (2007)', 1], [#, 'Nagai H. (2021)', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [9, [#, [#, 'FBrf0224702|pubmed:22028469', 386], [#, 'FBrf0227292|pubmed:25242320', 618], [#, 'FBrf0233454|pubmed:27626673', 1808], [#, 'FBrf0241977|pubmed:30995488', 2109], [#, 'FBrf0235788', 3634], [#, 'FBrf0221476|pubmed:23636399', 3828], [#, 'FBrf0218395|pubmed:22036573', 10947]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [9, [#, [#, 'FBrf0057159|pubmed:1372522', 1], [#, 'FBrf0208241|pubmed:19348939', 1], [#, 'FBrf0204148|pubmed:17900877', 1], [#, 'FBrf0192396|pubmed:17224403', 1], [#, 'FBrf0192282|pubmed:17166919', 1], [#, 'FBrf0130030|pubmed:10801879', 1], [#, 'FBrf0134475|pubmed:11267682', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [10, [#, [#, 'taxid:10116(\\Rattus norvegicus\\)', 1], [#, 'taxid:46015(\\Nucleopolyhedrovirus Autographa californica nucleopolyhedrovirus\\)', 1], [#, 'taxid:11676(\\Lentivirus Human immunodeficiency virus type 1\\)', 2], [#, 'taxid:9606(\\Homo sapiens\\)', 27], [#, 'taxid:7227(\\Drosophila melanogaster\\)', 51216]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [10, [#, [#, 'taxid:46015(\\Nucleopolyhedrovirus Autographa californica nucleopolyhedrovirus\\)', 1], [#, 'taxid:10116(\\Rattus norvegicus\\)', 1], [#, 'taxid:11676(\\Lentivirus Human immunodeficiency virus type 1\\)', 2], [#, 'taxid:9606(\\Homo sapiens\\)', 27], [#, 'taxid:7227(\\Drosophila melanogaster\\)', 51216]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [11, [#, [#, 'taxid:32644(\\unknown unknown\\)', 1], [#, 'taxid:81077(\\artificial artificial\\)', 3], [#, 'taxid:9606(\\Homo sapiens\\)', 23], [#, 'taxid:7227(\\Drosophila melanogaster\\)', 51220]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [11, [#, [#, 'taxid:32644(\\unknown unknown\\)', 1], [#, 'taxid:81077(\\artificial artificial\\)', 3], [#, 'taxid:9606(\\Homo sapiens\\)', 23], [#, 'taxid:7227(\\Drosophila melanogaster\\)', 51220]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [12, [#, [#, 'psi-mi:\\MI:1110\\(predicted interaction)', 3], [#, 'psi-mi:\\MI:0403\\(colocalization)', 10], [#, 'psi-mi:\\MI:0915\\(physical association)', 51234]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [12, [#, [#, 'psi-mi:\\MI:1110\\(predicted interaction)', 3], [#, 'psi-mi:\\MI:0403\\(colocalization)', 10], [#, 'psi-mi:\\MI:0915\\(physical association)', 51234]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [13, [#, [#, 'psi-mi:\\MI:0478\\(flybase)', 51247]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [13, [#, [#, 'psi-mi:\\MI:0478\\(flybase)', 51247]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [15, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [15, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [16, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [16, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [17, [#, [#, 'psi-mi:\\MI:0918\\(donor)', 2], [#, 'psi-mi:\\MI:0840\\(stimulator)', 3], [#, 'psi-mi:\\MI:1343\\(enzyme regulator)', 5], [#, 'psi-mi:\\MI:0586\\(inhibitor)', 5], [#, 'psi-mi:\\MI:0501\\(enzyme)', 284], [#, 'psi-mi:\\MI:0502\\(enzyme target)', 313]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [17, [#, [#, 'psi-mi:\\MI:0918\\(donor)', 2], [#, 'psi-mi:\\MI:0840\\(stimulator)', 3], [#, 'psi-mi:\\MI:0586\\(inhibitor)', 5], [#, 'psi-mi:\\MI:1343\\(enzyme regulator)', 5], [#, 'psi-mi:\\MI:0501\\(enzyme)', 284], [#, 'psi-mi:\\MI:0502\\(enzyme target)', 313]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [18, [#, [#, 'psi-mi:\\MI:0918\\(donor)', 1], [#, 'psi-mi:\\MI:0840\\(stimulator)', 5], [#, 'psi-mi:\\MI:0586\\(inhibitor)', 13], [#, 'psi-mi:\\MI:1343\\(enzyme regulator)', 13], [#, 'psi-mi:\\MI:0502\\(enzyme target)', 230], [#, 'psi-mi:\\MI:0501\\(enzyme)', 350]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [18, [#, [#, 'psi-mi:\\MI:0918\\(donor)', 1], [#, 'psi-mi:\\MI:0919\\(acceptor)', 1], [#, 'psi-mi:\\MI:0840\\(stimulator)', 5], [#, 'psi-mi:\\MI:1343\\(enzyme regulator)', 13], [#, 'psi-mi:\\MI:0586\\(inhibitor)', 13], [#, 'psi-mi:\\MI:0502\\(enzyme target)', 230], [#, 'psi-mi:\\MI:0501\\(enzyme)', 350]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [19, [#, [#, 'psi-mi:\\MI:0584\\(fluorescence acceptor)', 33], [#, 'psi-mi:\\MI:0583\\(fluorescence donor)', 83], [#, 'psi-mi:\\MI:0503\\(self)', 346], [#, 'psi-mi:\\MI:0497\\(neutral component)', 5702], [#, 'psi-mi:\\MI:0499\\(unspecified role)', 12291], [#, 'psi-mi:\\MI:0498\\(prey)', 15993], [#, 'psi-mi:\\MI:0496\\(bait)', 16786]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [19, [#, [#, 'psi-mi:\\MI:0898\\(putative self)', 2], [#, 'psi-mi:\\MI:0684\\(ancillary)', 11], [#, 'psi-mi:\\MI:0584\\(fluorescence acceptor)', 33], [#, 'psi-mi:\\MI:0583\\(fluorescence donor)', 83], [#, 'psi-mi:\\MI:0503\\(self)', 346], [#, 'psi-mi:\\MI:0497\\(neutral component)', 5702], [#, 'psi-mi:\\MI:0499\\(unspecified role)', 12291]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [20, [#, [#, 'psi-mi:\\MI:0584\\(fluorescence acceptor)', 50], [#, 'psi-mi:\\MI:0583\\(fluorescence donor)', 66], [#, 'psi-mi:\\MI:0503\\(self)', 345], [#, 'psi-mi:\\MI:0497\\(neutral component)', 5699], [#, 'psi-mi:\\MI:0499\\(unspecified role)', 12290], [#, 'psi-mi:\\MI:0498\\(prey)', 15747], [#, 'psi-mi:\\MI:0496\\(bait)', 17033]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [20, [#, [#, 'psi-mi:\\MI:0898\\(putative self)', 2], [#, 'psi-mi:\\MI:0684\\(ancillary)', 15], [#, 'psi-mi:\\MI:0584\\(fluorescence acceptor)', 50], [#, 'psi-mi:\\MI:0583\\(fluorescence donor)', 66], [#, 'psi-mi:\\MI:0503\\(self)', 345], [#, 'psi-mi:\\MI:0497\\(neutral component)', 5699], [#, 'psi-mi:\\MI:0499\\(unspecified role)', 12290]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [21, [#, [#, 'psi-mi:\\MI:0320\\(ribonucleic acid)', 1861], [#, 'psi-mi:\\MI:0326\\(protein)', 49386]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [21, [#, [#, 'psi-mi:\\MI:0320\\(ribonucleic acid)', 1861], [#, 'psi-mi:\\MI:0326\\(protein)', 49386]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [22, [#, [#, 'psi-mi:\\MI:0320\\(ribonucleic acid)', 2024], [#, 'psi-mi:\\MI:0326\\(protein)', 49223]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [22, [#, [#, 'psi-mi:\\MI:0320\\(ribonucleic acid)', 2024], [#, 'psi-mi:\\MI:0326\\(protein)', 49223]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [23, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [23, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [24, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [24, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [25, [#, [#, 'FBig0000071265', 36], [#, 'FBig0000095560', 38], [#, 'FBig0000097702', 38], [#, 'FBig0000095735', 38], [#, 'FBig0000086228', 40], [#, 'FBig0000067005', 44], [#, 'FBig0000017021', 46]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [25, [#, [#, 'FBig0000000059', 1], [#, 'FBig0000108279', 1], [#, 'FBig0000108278', 1], [#, 'FBig0000108277', 1], [#, 'FBig0000108275', 1], [#, 'FBig0000108274', 1], [#, 'FBig0000108273', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [26, [#, [#, 'comment:\\HA tag\\', 1318], [#, 'comment:\\GFP tag\\', 1380], [#, 'comment:\\GST tag\\', 1618], [#, 'comment:\\FLAG tag\\', 1993], [#, 'comment:\\fused to GAL4 DNA-binding domain\\', 3270], [#, 'comment:\\fused to GAL4 activation domain\\', 3379]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [26, [#, [#, 'comment:\\45 unique peptides identified\\', 1], [#, 'comment:\\mRFP tag\\', 1], [#, 'comment:\\MBP tag, [3]H label\\', 1], [#, 'comment:\\AU5 tag\\', 1], [#, 'comment:\\fused to biotin\\', 1], [#, 'comment:\\isoform RA/D specific\\|comment:\\biotin label\\', 1], [#, 'comment:\\FLAG\\|comment:\\myc\\', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [27, [#, [#, 'comment:\\TAP tag\\', 1285], [#, 'comment:\\HA tag\\', 1378], [#, 'comment:\\GST tag\\', 1497], [#, 'comment:\\FLAG tag\\', 2115], [#, 'comment:\\fused to GAL4 activation domain\\', 3309], [#, 'comment:\\fused to GAL4 DNA-binding domain\\', 3313]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [27, [#, [#, 'comment:\\XL isoform specific\\', 1], [#, 'comment:\\His,SUMO tag\\', 1], [#, 'comment:\\SAINT score = 0.9673\\', 1], [#, 'comment:\\fused to &bgr;-gal\\', 1], [#, 'comment:\\Myc tag\\|comment:\\RFP tag\\', 1], [#, 'comment:\\38, 33 peptide counts\\', 1], [#, 'comment:\\99 peptide counts\\', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [28, [#, [#, 'comment:\\Source was embryos of wild-type fly line; bait produced from endogenous gene; prey produced from endogenous gene.\\', 984], [#, 'comment:\\Source was cell extract of S2 cell line; bait produced from transfected construct; prey produced from endogenous gene.\\', 1083], [#, 'comment:\\Source was cell extract of S2 cell line; bait produced from transfected construct; prey produced from transfected construct.\\', 1723], [#, 'comment:\\Source was yeast cell line; bait produced as transgenic fusion protein; prey produced as transgenic fusion protein.(Pooled)\\|comment:\\MORATORIUM:This data was made available \\\'pre-publication\\\' and, as such, its use is subject to the specific limitations described in FBrf0235788 for the period of 12 months. Moratorium expires on June 13, 2018.\\|comment:\\Two-hybrid system: yeast GAL4-BD/GAL4-AD\\', 1817], [#, 'comment:\\Source was yeast cell line; bait produced as transgenic fusion protein; prey produced as transgenic fusion protein.(Pairwise.)\\|comment:\\MORATORIUM:This data was made available \\\'pre-publication\\\' and, as such, its use is subject to the specific limitations described in FBrf0235788 for the period of 12 months. Moratorium expires on June 13, 2018.\\|comment:\\Two-hybrid system: yeast GAL4-BD/GAL4-AD\\', 1817], [#, 'comment:\\Source was yeast cell line; bait produced as transgenic fusion protein; prey produced as transgenic fusion protein (prey was previously cloned reagent).\\|comment:\\Two-hybrid system: yeast GAL4-BD/GAL4-AD\\', 2713], [#, 'comment:\\Source was embryos of wild-type fly line; proteins produced from endogenous genes.\\|comment:\\The structure of the Drosophila 80S ribosome with accessory proteins solved by cryo-EM. Complex captured as 3828 pairwise interactions which do not necessarily reflect direct contact.\\', 3403]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [28, [#, [#, 'comment:\\HGScore = 131.41663\\', 1], [#, 'comment:\\Interaction in vitro; bait produced as a recombinant fusion protein in bacterial system; prey produced as a recombinant fusion protein in bacterial system.\\|comment:\\@BicD@ binds to GTP-bound @Rab6@, not GDP-bound @Rab6@.\\|comment:\\Purified recombinant proteins were tested for interaction.\\|comment:\\There is overlap of @egl@ and @Rab6@ binding sites in @BicD@.\\', 1], [#, 'comment:\\Interaction in vitro; bait produced as a recombinant fusion protein in bacterial system; prey derived from S2 cell extract (endogenous gene).\\|comment:\\Constitutively active (GTP-locked) form of Rab was used for pull down. Parallel pull downs performed for several Rabs proteins in this study. Yield was measured using spectral counts. A specificity score (S score) was calculated, where greater weight was given to interactions seen with fewer baits.\\|comment:\\Specificity S-score=11.86.\\', 1], [#, 'comment:\\HGScore = 81.66328\\', 1], [#, 'comment:\\Interaction in vitro; bait produced as a recombinant fusion protein in HEK293F cells; prey produced as a recombinant fusion protein in S2 cells.\\|comment:\\Kd = 4.66uM\\', 1], [#, 'comment:\\Source was intact S2 cells; proteins produced from transfected constructs.\\|comment:\\Unlabeled S2 cells expressing @Nrg@ were mixed with dye labeled S2 cells expressing @ed@, and the formation of cell clusters containing both @Nrg@ and @ed@ expressing cells was assayed by immunofluorescence.\\', 1], [#, 'comment:\\HGScore = 61.53800\\', 1]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [29, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [29, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [30, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [30, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [31, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [31, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [32, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [32, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [33, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [33, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [34, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [34, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [35, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [35, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [36, [#, [#, 'FALSE', 16383], [#, 'False', 34863]]]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [36, [#, [#, 'FALSE', 16383], [#, 'False', 34863]]]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [37, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [37, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [38, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [38, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [39, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [39, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [40, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [40, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [41, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [41, []]).
fb_tsv_pred_stats('most-frequent', physical_interactions_mitab, [42, []]).
fb_tsv_pred_stats('less-frequent', physical_interactions_mitab, [42, []]).
fb_tsv_pred_stats('num-columns', 'scRNA-Seq_gene_expression', [15]).
fb_tsv_pred_stats('duplicated-rows', 'scRNA-Seq_gene_expression', [0]).
fb_tsv_pred_stats('total-rows', 'scRNA-Seq_gene_expression', [14319160]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [1, 13, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [2, 13, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [3, 89, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [4, 89, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [5, 4, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [6, 2, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [7, 29, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [8, 1690, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [9, 1690, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [10, 271, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [11, 271, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [12, 14295, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [13, 14295, object]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [14, 8174166, float64]).
fb_tsv_pred_stats('unique-values', 'scRNA-Seq_gene_expression', [15, 370225, float64]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [4, 0, [], []]).
fb_tsv_pred_stats('null-value-count', 'scRNA-Seq_gene_expression', [5, '', 5898802]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [5, 5898802, [#, ''], [#, 5898802]]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [6, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [7, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [8, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [9, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [10, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [11, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [12, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [13, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [14, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'scRNA-Seq_gene_expression', [15, 0, [], []]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [1, [#, [#, 'FBrf0244302', 177088], [#, 'FBrf0245499', 181659], [#, 'FBrf0246655', 191489], [#, 'FBrf0247762', 424264], [#, 'FBrf0251482', 602328], [#, 'FBrf0249767', 664303], [#, 'FBrf0252876', 11562417]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [1, [#, [#, 'FBrf0248210', 24480], [#, 'FBrf0245988', 38144], [#, 'FBrf0245616', 100103], [#, 'FBrf0248439', 114659], [#, 'FBrf0247435', 116094], [#, 'FBrf0247148', 122132], [#, 'FBrf0244302', 177088]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [2, [#, [#, 'Brunet Avalos et al., 2019, eLife 8: e50354', 177088], [#, 'Allen et al., 2020, eLife 9: e54074', 181659], [#, 'Cho et al., 2020, Nat. Commun. 11(1): 4483', 191489], [#, '\\00d6zel et al., 2021, Nature 589(7840): 88--95', 424264], [#, 'Baker et al., 2021, Genome Res. 31(10): 1927--1937', 602328], [#, 'Mokashi et al., 2021, Front. Psychiatry 12: 699033', 664303], [#, 'Li et al., 2022, Science 375(6584): eabk2432', 11562417]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [2, [#, [#, 'Shi et al., 2021, Curr. Biol. 31(4): 840--852.e5', 24480], [#, 'Cattenoz et al., 2020, EMBO J. 39(12): e104486', 38144], [#, 'Jevitt et al., 2020, PLoS Biol. 18(4): e3000538', 100103], [#, 'Tauc et al., 2021, eLife 10: e62250', 114659], [#, 'Zappia et al., 2020, EMBO Rep. 21(10): e49555', 116094], [#, 'Rust et al., 2020, Nat. Commun. 11(1): 5628', 122132], [#, 'Brunet Avalos et al., 2019, eLife 8: e50354', 177088]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [3, [#, [#, 'FBlc0005662', 424264], [#, 'FBlc0004310', 462839], [#, 'FBlc0003849', 485904], [#, 'FBlc0004788', 587089], [#, 'FBlc0004142', 1324076], [#, 'FBlc0004624', 1374828], [#, 'FBlc0005151', 1787268]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [3, [#, [#, 'FBlc0004140', 11271], [#, 'FBlc0004136', 11650], [#, 'FBlc0004622', 12336], [#, 'FBlc0004618', 12504], [#, 'FBlc0005149', 13127], [#, 'FBlc0005145', 13237], [#, 'FBlc0003731', 16857]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [4, [#, [#, scRNAseq_2021_Ozel_ADM1_seq_clustering, 424264], [#, scRNAseq_2022_FCA_MALE_HEAD_seq_clustering, 462839], [#, scRNAseq_2022_FCA_FEMALE_HEAD_seq_clustering, 485904], [#, scRNAseq_2022_FCA_MIXED_HEAD_seq_clustering, 587089], [#, scRNAseq_2022_FCA_FEMALE_FULL_seq_clustering, 1324076], [#, scRNAseq_2022_FCA_MALE_FULL_seq_clustering, 1374828], [#, scRNAseq_2022_FCA_MIXED_FULL_seq_clustering, 1787268]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [4, [#, [#, scRNAseq_2022_FCA_FEMALE_IPC_seq_clustering, 11271], [#, scRNAseq_2022_FCA_FEMALE_CC_seq_clustering, 11650], [#, scRNAseq_2022_FCA_MALE_IPC_seq_clustering, 12336], [#, scRNAseq_2022_FCA_MALE_CC_seq_clustering, 12504], [#, scRNAseq_2022_FCA_MIXED_IPC_seq_clustering, 13127], [#, scRNAseq_2022_FCA_MIXED_CC_seq_clustering, 13237], [#, scRNAseq_2020_Cattenoz_NI_seq_clustering, 16857]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [5, [#, [#, mixed, 445126], [#, female, 3955616], [#, male, 4019616]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [5, [#, [#, mixed, 445126], [#, female, 3955616], [#, male, 4019616]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [6, [#, [#, 'larval stage', 522815], [#, 'adult stage', 13796345]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [6, [#, [#, 'larval stage', 522815], [#, 'adult stage', 13796345]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [7, [#, [#, antenna, 537963], [#, ovary, 556379], [#, 'adult brain', 602328], [#, brain, 664303], [#, adult, 1157530], [#, 'adult head', 1535832], [#, mixed, 4486172]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [7, [#, [#, 'adult pars intercerebralis', 36734], [#, 'adult corpus cardiacum', 37391], [#, 'embryonic/larval hemolymph', 101153], [#, 'adult midgut', 114659], [#, 'wing disc', 116094], [#, 'adult abdomen', 120557], [#, 'primary lobe of embryonic/larval lymph gland', 128480]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [8, [#, [#, 'FBlc0004677', 13868], [#, 'FBlc0005192', 13876], [#, 'FBlc0004567', 13941], [#, 'FBlc0005094', 13941], [#, 'FBlc0004713', 13953], [#, 'FBlc0005247', 13956], [#, 'FBlc0005205', 13958]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [8, [#, [#, 'FBlc0003842', 762], [#, 'FBlc0003779', 765], [#, 'FBlc0003749', 872], [#, 'FBlc0003748', 1065], [#, 'FBlc0003833', 1100], [#, 'FBlc0003843', 2232], [#, 'FBlc0003733', 2255]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [9, [#, [#, scRNAseq_2022_FCA_MALE_FULL_seq_clustering_fat_cells, 13868], [#, scRNAseq_2022_FCA_MIXED_FULL_seq_clustering_neurons, 13876], [#, scRNAseq_2022_FCA_MALE_TESTIS_seq_clustering_spermatocytes, 13941], [#, scRNAseq_2022_FCA_MIXED_TESTIS_seq_clustering_spermatocytes, 13941], [#, scRNAseq_2022_FCA_MALE_FULL_seq_clustering_spermatocytes, 13953], [#, scRNAseq_2022_FCA_MIXED_FULL_seq_clustering_spermatocytes, 13956], [#, scRNAseq_2022_FCA_MIXED_FULL_seq_clustering_fat_cells, 13958]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [9, [#, [#, scRNAseq_2020_Zappia_AmaRNAi_seq_clustering_imaginal_tracheoblasts, 762], [#, scRNAseq_2020_Cho_NI96HL_seq_clustering_lamellocytes, 765], [#, scRNAseq_2020_Cho_NI72LG_seq_clustering_lamellocytes, 872], [#, scRNAseq_2020_Cho_NI72LG_seq_clustering_adipohemocytes, 1065], [#, scRNAseq_2020_Zappia_mCherryRNAi_seq_clustering_imaginal_tracheoblasts, 1100], [#, scRNAseq_2020_Zappia_AmaRNAi_seq_clustering_SOP_cells, 2232], [#, scRNAseq_2020_Cattenoz_NI_seq_clustering_lamellocytes, 2255]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [10, [#, [#, 'FBbt00003185', 202803], [#, 'FBbt00005040', 281977], [#, 'FBbt00047095', 365976], [#, 'FBbt00058143', 371275], [#, 'FBbt00058238', 373508], [#, 'FBbt00049949', 395912], [#, 'FBbt00058230', 396390]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [10, [#, [#, 'FBbt00003844', 3363], [#, 'FBbt00004868', 4017], [#, 'FBbt00003874', 4103], [#, 'FBbt00111746', 4323], [#, 'FBbt00004873', 4476], [#, 'FBbt00004905', 4559], [#, 'FBbt00003876', 4893]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [11, [#, [#, 'adult oenocyte', 202803], [#, 'adult tracheocyte', 281977], [#, 'adult neuron', 365976], [#, 'adult hemocyte', 371275], [#, 'adult epithelial cell', 373508], [#, 'adult fat cell', 395912], [#, 'adult muscle cell', 396390]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [11, [#, [#, 'medullary tangential neuron Mt8', 3363], [#, 'germarium cap cell', 4017], [#, 'lobula columnar neuron LC4', 4103], [#, 'lobula columnar neuron LC10c', 4323], [#, 'female germline stem cell', 4476], [#, 'border follicle cell', 4559], [#, 'lobula columnar neuron LC6', 4893]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [12, [#, [#, 'FBgn0267521', 1690], [#, 'FBgn0086472', 1690], [#, 'FBgn0002626', 1690], [#, 'FBgn0285947', 1690], [#, 'FBgn0003279', 1690], [#, 'FBgn0017579', 1690], [#, 'FBgn0016726', 1690]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [12, [#, [#, 'FBgn0284232', 2], [#, 'FBgn0267889', 4], [#, 'FBgn0045478', 4], [#, 'FBgn0039374', 4], [#, 'FBgn0028537', 4], [#, 'FBgn0267886', 4], [#, 'FBgn0085252', 4]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [13, [#, [#, '18SrRNA-Psi:CR45861', 1690], [#, 'RpS25', 1690], [#, 'RpL32', 1690], [#, 'RpS10b', 1690], [#, 'RpL4', 1690], [#, 'RpL14', 1690], [#, 'RpL29', 1690]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [13, [#, [#, 'CG46316', 2], [#, 'CR46177', 4], [#, 'Gr64b', 4], [#, 'CG17770', 4], [#, 'CG31775', 4], [#, 'CR46174', 4], [#, 'CG34223', 4]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [14, [#, [#, 1272.2646, 2315], [#, 1218.0267, 2325], [#, 1287.0013, 2439], [#, 1298.7013, 2444], [#, 1095.2902, 2445], [#, 24.43136, 2546], [#, 1191.8951, 2690]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [14, [#, [#, 269.05170000000004, 1], [#, 480.440107, 1], [#, 990.35309, 1], [#, 370.235704, 1], [#, 272.274411, 1], [#, 234.056368, 1], [#, 320.438443, 1]]]).
fb_tsv_pred_stats('most-frequent', 'scRNA-Seq_gene_expression', [15, [#, [#, 0.076923, 41235], [#, 0.0625, 43249], [#, 0.1, 44120], [#, 0.025, 44512], [#, 0.04, 47158], [#, 0.083333, 47452], [#, 0.05, 59787]]]).
fb_tsv_pred_stats('less-frequent', 'scRNA-Seq_gene_expression', [15, [#, [#, 0.366282, 1], [#, 0.774026, 1], [#, 0.318802, 1], [#, 0.173431, 1], [#, 0.334605, 1], [#, 0.770109, 1], [#, 0.517905, 1]]]).
fb_tsv_pred_stats('num-columns', sed5C, [11]).
fb_tsv_pred_stats('duplicated-rows', sed5C, [0]).
fb_tsv_pred_stats('total-rows', sed5C, [120]).
fb_tsv_pred_stats('unique-values', sed5C, [1, 10, object]).
fb_tsv_pred_stats('unique-values', sed5C, [2, 10, object]).
fb_tsv_pred_stats('unique-values', sed5C, [3, 5, object]).
fb_tsv_pred_stats('unique-values', sed5C, [4, 6, object]).
fb_tsv_pred_stats('unique-values', sed5C, [5, 3, object]).
fb_tsv_pred_stats('unique-values', sed5C, [6, 2, object]).
fb_tsv_pred_stats('unique-values', sed5C, [7, 119, object]).
fb_tsv_pred_stats('unique-values', sed5C, [8, 119, object]).
fb_tsv_pred_stats('unique-values', sed5C, [9, 79, object]).
fb_tsv_pred_stats('unique-values', sed5C, [10, 2, object]).
fb_tsv_pred_stats('unique-values', sed5C, [11, 2, object]).
fb_tsv_pred_stats('missing-values', sed5C, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', sed5C, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', sed5C, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', sed5C, [4, 0, [], []]).
fb_tsv_pred_stats('null-value-count', sed5C, [5, '', 114]).
fb_tsv_pred_stats('missing-values', sed5C, [5, 114, [#, ''], [#, 114]]).
fb_tsv_pred_stats('null-value-count', sed5C, [6, '', 118]).
fb_tsv_pred_stats('missing-values', sed5C, [6, 118, [#, ''], [#, 118]]).
fb_tsv_pred_stats('missing-values', sed5C, [7, 0, [], []]).
fb_tsv_pred_stats('missing-values', sed5C, [8, 0, [], []]).
fb_tsv_pred_stats('null-value-count', sed5C, [9, '', 41]).
fb_tsv_pred_stats('missing-values', sed5C, [9, 41, [#, ''], [#, 41]]).
fb_tsv_pred_stats('null-value-count', sed5C, [10, '', 118]).
fb_tsv_pred_stats('missing-values', sed5C, [10, 118, [#, ''], [#, 118]]).
fb_tsv_pred_stats('null-value-count', sed5C, [11, '', 118]).
fb_tsv_pred_stats('missing-values', sed5C, [11, 118, [#, ''], [#, 118]]).
fb_tsv_pred_stats('most-frequent', sed5C, [1, [#, [#, 'FBgg0000062', 3], [#, 'FBgg0000072', 4], [#, 'FBgg0000040', 5], [#, 'FBgg0000081', 6], [#, 'FBgg0000100', 7], [#, 'FBgg0000118', 14], [#, 'FBgg0000128', 75]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [1, [#, [#, 'FBgg0000117', 1], [#, 'FBgg0000104', 2], [#, 'FBgg0000062', 3], [#, 'FBgg0000114', 3], [#, 'FBgg0000072', 4], [#, 'FBgg0000040', 5], [#, 'FBgg0000081', 6]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [2, [#, [#, 'ROC UBIQUITIN LIGASES', 3], [#, 'ADAM METALLOPROTEASES', 4], [#, 'RING-BETWEEN-RING UBIQUITIN LIGASES', 5], [#, 'STRUCTURAL MAINTENANCE OF CHROMOSOMES GENE FAMILY', 6], [#, 'CASPASES', 7], [#, 'HECT UBIQUITIN LIGASES', 14], [#, 'UNCLASSIFIED RING DOMAIN UBIQUITIN LIGASES', 75]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [2, [#, [#, 'JOSEPHIN DEUBIQUITINASES', 1], [#, 'GOLIATH UBIQUITIN LIGASES', 2], [#, 'ROC UBIQUITIN LIGASES', 3], [#, 'N-ETHYLMALEIMIDE-SENSITIVE FACTORS', 3], [#, 'ADAM METALLOPROTEASES', 4], [#, 'RING-BETWEEN-RING UBIQUITIN LIGASES', 5], [#, 'STRUCTURAL MAINTENANCE OF CHROMOSOMES GENE FAMILY', 6]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [3, [#, [#, 'GO:0004843', 1], [#, 'GO:0004222', 4], [#, 'GO:0097153', 7], [#, 'GO:0016887', 9], [#, 'GO:0061630', 99]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [3, [#, [#, 'GO:0004843', 1], [#, 'GO:0004222', 4], [#, 'GO:0097153', 7], [#, 'GO:0016887', 9], [#, 'GO:0061630', 99]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [4, [#, [#, ubiquit, 1], [#, 'cysteine-type deubiquitinase activity', 1], [#, 'metalloendopeptidase activity', 4], [#, 'cysteine-type endopeptidase activity involved in apoptotic process', 7], [#, 'ATP hydrolysis activity', 9], [#, 'ubiquitin protein ligase activity', 98]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [4, [#, [#, 'cysteine-type deubiquitinase activity', 1], [#, ubiquit, 1], [#, 'metalloendopeptidase activity', 4], [#, 'cysteine-type endopeptidase activity involved in apoptotic process', 7], [#, 'ATP hydrolysis activity', 9], [#, 'ubiquitin protein ligase activity', 98]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [5, [#, [#, '3.4.19.12', 1], [#, '3.4.24.-', 4]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [5, [#, [#, '3.4.19.12', 1], [#, '3.4.24.-', 4]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [6, [#, [#, 'ubiquitinyl hydrolase 1', 1]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [6, [#, [#, 'ubiquitinyl hydrolase 1', 1]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [7, [#, [#, 'FBgn0034546', 1], [#, 'FBgn0034573', 1], [#, 'FBgn0035024', 1], [#, 'FBgn0035232', 1], [#, 'FBgn0035233', 1], [#, 'FBgn0032635', 1], [#, 'FBgn0025186', 1]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [7, [#, [#, 'FBgn0025186', 1], [#, 'FBgn0035232', 1], [#, 'FBgn0035024', 1], [#, 'FBgn0034573', 1], [#, 'FBgn0034546', 1], [#, 'FBgn0034314', 1], [#, 'FBgn0034312', 1]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [8, [#, [#, 'CG13442', 1], [#, 'Sou', 1], [#, 'CG11414', 1], [#, 'CG12099', 1], [#, 'Pex10', 1], [#, 'CG15141', 1], [#, 'ari-2', 1]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [8, [#, [#, 'ari-2', 1], [#, 'CG12099', 1], [#, 'CG11414', 1], [#, 'Sou', 1], [#, 'CG13442', 1], [#, nopo, 1], [#, 'CG10916', 1]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [9, [#, [#, 'Ubr1 ubiquitin ligase', 1], [#, 'Peroxin 12', 1], [#, 'Ring finger and CHY zinc finger domain containing 1', 1], [#, 'Non-SMC element 1', 1], [#, 'Protein interacting with Ttk69 and Sin3A', 1], [#, 'ariadne 2', 1]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [9, [#, [#, 'ariadne 2', 1], [#, 'Ring finger and CHY zinc finger domain containing 1', 1], [#, 'Peroxin 12', 1], [#, 'Ubr1 ubiquitin ligase', 1], [#, 'suppression of retinal degeneration disease 1 upon overexpression 1', 1], [#, highwire, 1], [#, 'Mahogunin ring finger 1', 1]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [10, [#, [#, '3.4.19.12', 1]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [10, [#, [#, '3.4.19.12', 1]]]).
fb_tsv_pred_stats('most-frequent', sed5C, [11, [#, [#, 'ubiquitinyl hydrolase 1', 1]]]).
fb_tsv_pred_stats('less-frequent', sed5C, [11, [#, [#, 'ubiquitinyl hydrolase 1', 1]]]).
fb_tsv_pred_stats('num-columns', sednJ, [2]).
fb_tsv_pred_stats('duplicated-rows', sednJ, [0]).
fb_tsv_pred_stats('total-rows', sednJ, [76447]).
fb_tsv_pred_stats('unique-values', sednJ, [1, 76447, object]).
fb_tsv_pred_stats('unique-values', sednJ, [2, 76447, object]).
fb_tsv_pred_stats('missing-values', sednJ, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', sednJ, [2, 0, [], []]).
fb_tsv_pred_stats('num-columns', gene_associatio, [2]).
fb_tsv_pred_stats('duplicated-rows', gene_associatio, [69]).
fb_tsv_pred_stats('total-rows', gene_associatio, [48039]).
fb_tsv_pred_stats('unique-values', gene_associatio, [1, 47970, object]).
fb_tsv_pred_stats('unique-values', gene_associatio, [2, 4664, object]).
fb_tsv_pred_stats('missing-values', gene_associatio, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', gene_associatio, [2, 0, [], []]).
fb_tsv_pred_stats('most-frequent', gene_associatio, [1, [#, [#, 'FB\\FBgn0053817\\His2A:CG33817\\involved_in\\GO:0006325\\FB:FBrf0255270|GO_REF:0000024\\ISS\\MGI:MGI:2448285\\P\\His2A:CG33817\\CG33817\\protein\\taxon:7227\\20160225\\FlyBase\\\\', 2], [#, 'FB\\FBgn0053817\\His2A:CG33817\\part_of\\GO:0000786\\FB:FBrf0255270|GO_REF:0000024\\ISS\\UniProtKB:Q7L7L0\\C\\His2A:CG33817\\CG33817\\protein\\taxon:7227\\20160225\\FlyBase\\\\', 2], [#, 'FB\\FBgn0053829\\His2A:CG33829\\involved_in\\GO:0006325\\FB:FBrf0255270|GO_REF:0000024\\ISS\\MGI:MGI:2448285\\P\\His2A:CG33829\\CG33829\\protein\\taxon:7227\\20160225\\FlyBase\\\\', 2], [#, 'FB\\FBgn0053829\\His2A:CG33829\\part_of\\GO:0000786\\FB:FBrf0255270|GO_REF:0000024\\ISS\\UniProtKB:Q7L7L0\\C\\His2A:CG33829\\CG33829\\protein\\taxon:7227\\20160225\\FlyBase\\\\', 2], [#, 'FB\\FBgn0053829\\His2A:CG33829\\enables\\GO:0003677\\FB:FBrf0255270|GO_REF:0000024\\ISS\\UniProtKB:Q7L7L0\\F\\His2A:CG33829\\CG33829\\protein\\taxon:7227\\20160225\\FlyBase\\\\', 2], [#, 'FB\\FBgn0053846\\His1:CG33846\\part_of\\GO:0000785\\FB:FBrf0255270|GO_REF:0000024\\ISS\\UniProtKB:P07305\\C\\His1:CG33846\\CG33846\\protein\\taxon:7227\\20161201\\FlyBase\\\\', 2], [#, 'FB\\FBgn0053843\\His1:CG33843\\part_of\\GO:0000785\\FB:FBrf0255270|GO_REF:0000024\\ISS\\UniProtKB:P07305\\C\\His1:CG33843\\CG33843\\protein\\taxon:7227\\20161201\\FlyBase\\\\', 2]]]).
fb_tsv_pred_stats('less-frequent', gene_associatio, [1, [#, [#, '!generated-by:', 1], [#, 'FB\\FBgn0027508\\Tnks\\involved_in\\GO:1904355\\FB:FBrf0239784|PMID:21873635\\IBA\\PANTHER:PTN000652929\\P\\tankyrase\\BcDNA:LD22548|CG17487|CG4719|DTNKS|Dm.pARTb|TNKS|Tankyrase|TkrsIR|dARTD5|dTANK|dTNKS|dTankyrase\\protein\\taxon:7227\\20170420\\GO_Central\\\\', 1], [#, 'FB\\FBgn0027508\\Tnks\\involved_in\\GO:0070198\\FB:FBrf0239784|PMID:21873635\\IBA\\PANTHER:PTN000652929\\P\\tankyrase\\BcDNA:LD22548|CG17487|CG4719|DTNKS|Dm.pARTb|TNKS|Tankyrase|TkrsIR|dARTD5|dTANK|dTNKS|dTankyrase\\protein\\taxon:7227\\20170420\\GO_Central\\\\', 1], [#, 'FB\\FBgn0027508\\Tnks\\enables\\GO:0003950\\FB:FBrf0241717|PMID:30593492\\IMP\\\\F\\tankyrase\\BcDNA:LD22548|CG17487|CG4719|DTNKS|Dm.pARTb|TNKS|Tankyrase|TkrsIR|dARTD5|dTANK|dTNKS|dTankyrase\\protein\\taxon:7227\\20190806\\FlyBase\\\\', 1], [#, 'FB\\FBgn0027508\\Tnks\\involved_in\\GO:0090263\\FB:FBrf0241717|PMID:30593492\\IMP\\\\P\\tankyrase\\BcDNA:LD22548|CG17487|CG4719|DTNKS|Dm.pARTb|TNKS|Tankyrase|TkrsIR|dARTD5|dTANK|dTNKS|dTankyrase\\protein\\taxon:7227\\20190806\\FlyBase\\\\', 1], [#, 'FB\\FBgn0027508\\Tnks\\involved_in\\GO:0032436\\FB:FBrf0241717|PMID:30593492\\IMP\\\\P\\tankyrase\\BcDNA:LD22548|CG17487|CG4719|DTNKS|Dm.pARTb|TNKS|Tankyrase|TkrsIR|dARTD5|dTANK|dTNKS|dTankyrase\\protein\\taxon:7227\\20190806\\FlyBase\\\\', 1], [#, 'FB\\FBgn0027508\\Tnks\\involved_in\\GO:0046330\\FB:FBrf0254888|PMID:36222503\\IDA\\\\P\\tankyrase\\BcDNA:LD22548|CG17487|CG4719|DTNKS|Dm.pARTb|TNKS|Tankyrase|TkrsIR|dARTD5|dTANK|dTNKS|dTankyrase\\protein\\taxon:7227\\20230307\\FlyBase\\\\', 1]]]).
fb_tsv_pred_stats('most-frequent', gene_associatio, [2, [#, [#, 'sex|dsxF|dsxM|intersex-62c|ix-62c\\protein\\taxon:7227\\20060803\\FlyBase\\\\', 19], [#, 'lipase\\protein\\taxon:7227\\20230703\\InterPro\\\\', 19], [#, 'factor|A7.1|ABF|Abf|CG3992|DmGATAb|GATA|GATAb|SERPENT|SRP|Serpent|Srp|abf|dGATAb|l(3)01549|l(3)89B2|l(3)neo45|serpentD|spt|srpD\\protein\\taxon:7227\\20060803\\FlyBase\\\\', 21], [#, 'protein\\protein\\taxon:7227\\20151015\\FlyBase\\\\', 21], [#, 'sterile(2)ltoRJ36|fs(2)ltoRJ36|no-relish\\protein\\taxon:7227\\20060803\\FlyBase\\\\', 23], [#, '1|Rhomboid-1|Rhomboid-I|Ve|Veinlet|iks|rho-1|rho1|rhom|rhomboid-1|rhomboid/veinlet|rhomboid1|ve|veinlet\\protein\\taxon:7227\\20200406\\FlyBase\\\\', 26], [#, '1.12|co|dNotch|fa|facet|l(1)3Cb|l(1)Ax|l(1)N|n[fah]|nd|notch|notchoid|shd|spl|split|strawberry|swb\\protein\\taxon:7227\\20060803\\FlyBase\\\\', 44]]]).
fb_tsv_pred_stats('less-frequent', gene_associatio, [2, [#, [#, 'FlyBase', 1], [#, 'B2-3|Sox21|SoxB2.3|sox21a\\protein\\taxon:7227\\20221006\\GO_Central\\\\', 1], [#, 'B2-2|Sox21B|SoxB2.2|sox-like\\protein\\taxon:7227\\20200725\\GO_Central\\\\', 1], [#, 'B2-2|Sox21B|SoxB2.2|sox-like\\protein\\taxon:7227\\20200311\\GO_Central\\\\', 1], [#, 'B2-2|Sox21B|SoxB2.2|sox-like\\protein\\taxon:7227\\20221006\\GO_Central\\\\', 1], [#, '212\\CG33329|ORE-5|SP212\\protein\\taxon:7227\\20060803\\FlyBase\\\\', 1], [#, '212\\CG33329|ORE-5|SP212\\protein\\taxon:7227\\20190311\\FlyBase\\\\', 1]]]).
fb_tsv_pred_stats('num-columns', disease_model_annotations, [12]).
fb_tsv_pred_stats('duplicated-rows', disease_model_annotations, [0]).
fb_tsv_pred_stats('total-rows', disease_model_annotations, [26750]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [1, 5165, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [2, 5165, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [3, 317, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [4, 6, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [5, 3554, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [6, 3554, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [7, 8303, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [8, 8303, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [9, 2657, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [10, 2657, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [11, 6770, object]).
fb_tsv_pred_stats('unique-values', disease_model_annotations, [12, 2675, object]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', disease_model_annotations, [3, '', 21466]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [3, 21466, [#, ''], [#, 21466]]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [5, 0, [], []]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [6, 0, [], []]).
fb_tsv_pred_stats('null-value-count', disease_model_annotations, [7, '', 6240]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [7, 6240, [#, ''], [#, 6240]]).
fb_tsv_pred_stats('null-value-count', disease_model_annotations, [8, '', 6240]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [8, 6240, [#, ''], [#, 6240]]).
fb_tsv_pred_stats('null-value-count', disease_model_annotations, [9, '', 20510]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [9, 20510, [#, ''], [#, 20510]]).
fb_tsv_pred_stats('null-value-count', disease_model_annotations, [10, '', 20510]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [10, 20510, [#, ''], [#, 20510]]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [11, 0, [], []]).
fb_tsv_pred_stats('missing-values', disease_model_annotations, [12, 0, [], []]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [1, [#, [#, 'FBgn0026420', 300], [#, 'FBgn0029891', 301], [#, 'FBgn0261084', 371], [#, 'FBgn0024961', 448], [#, 'FBgn0025823', 630], [#, 'FBgn0025587', 680], [#, 'FBgn0015394', 948]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [1, [#, [#, 'FBgn0033093', 1], [#, 'FBgn0031951', 1], [#, 'FBgn0031971', 1], [#, 'FBgn0031973', 1], [#, 'FBgn0266724', 1], [#, 'FBgn0050273', 1], [#, 'FBgn0031981', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [2, [#, [#, 'Hsap\\SNCA', 300], [#, 'Pink1', 301], [#, 'Hsap\\TARDBP', 371], [#, 'Hsap\\ATXN3', 448], [#, 'Hsap\\APP', 630], [#, 'Hsap\\HTT', 680], [#, 'Hsap\\MAPT', 948]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [2, [#, [#, 'CG3270', 1], [#, r2d2, 1], [#, 'Sirup', 1], [#, 'Spn28Dc', 1], [#, 'Trs20', 1], [#, 'CG30273', 1], [#, 'Megf8', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [3, [#, [#, 'HGNC:11138', 300], [#, 'HGNC:11571', 371], [#, 'HGNC:7106', 448], [#, 'HGNC:620', 630], [#, 'HGNC:4851', 680], [#, 'HGNC:6893', 948]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [3, [#, [#, 'HGNC:6239', 1], [#, 'HGNC:16854', 1], [#, 'HGNC:11118', 1], [#, 'HGNC:12013', 1], [#, 'HGNC:18222', 1], [#, 'HGNC:14973', 1], [#, 'HGNC:19245', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [4, [#, [#, 'DOES NOT exacerbate', 21], [#, 'DOES NOT ameliorate', 106], [#, 'DOES NOT model', 135], [#, exacerbates, 2650], [#, ameliorates, 4360], [#, 'model of', 19478]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [4, [#, [#, 'DOES NOT exacerbate', 21], [#, 'DOES NOT ameliorate', 106], [#, 'DOES NOT model', 135], [#, exacerbates, 2650], [#, ameliorates, 4360], [#, 'model of', 19478]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [5, [#, [#, 'DOID:10652', 652], [#, 'DOID:0060213', 692], [#, 'DOID:14330', 992], [#, 'DOID:1289', 1045], [#, 'DOID:12858', 1091], [#, 'DOID:162', 1305], [#, 'DOID:680', 1474]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [5, [#, [#, 'DOID:0111040', 1], [#, 'DOID:0080232', 1], [#, 'DOID:0110441', 1], [#, 'DOID:0111706', 1], [#, 'DOID:0080698', 1], [#, 'DOID:0080465', 1], [#, 'DOID:0060730', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [6, [#, [#, 'Alzheimer\'s disease', 652], [#, 'frontotemporal dementia and/or amyotrophic lateral sclerosis-1', 692], [#, 'Parkinson\'s disease', 992], [#, 'neurodegenerative disease', 1045], [#, 'Huntington\'s disease', 1091], [#, cancer, 1305], [#, tauopathy, 1474]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [6, [#, [#, 'glycogen storage disease IXd', 1], [#, 'autosomal dominant intellectual developmental disorder 51', 1], [#, 'dilated cardiomyopathy 2B', 1], [#, 'oblique facial clefting 1', 1], [#, 'Teebi hypertelorism syndrome 1', 1], [#, 'developmental and epileptic encephalopathy 30', 1], [#, 'torsion dystonia 1', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [7, [#, [#, 'FBal0060587', 153], [#, 'FBal0127292', 173], [#, 'FBal0193144', 193], [#, 'FBal0126526', 233], [#, 'FBal0126527', 239], [#, 'FBal0090630', 296]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [7, [#, [#, 'FBal0341584', 1], [#, 'FBal0265787', 1], [#, 'FBal0324663', 1], [#, 'FBal0262510', 1], [#, 'FBal0257944', 1], [#, 'FBal0235732', 1], [#, 'FBal0206026', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [8, [#, [#, 'Ras85D[V12.UAS]', 153], [#, 'Hsap\\HTT[Q93.ex1.UAS]', 173], [#, 'Pink1[B9]', 193], [#, 'Hsap\\MAPT[UAS.cWa]', 233], [#, 'Hsap\\MAPT[R406W.UAS]', 239], [#, 'Hsap\\ATXN3[tr.Q78.UAS.Tag:HA]', 296]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [8, [#, [#, 'Tom7[HMC06597]', 1], [#, 'Lkb1[UAS.cWa]', 1], [#, 'Lkb1[VSH330167]', 1], [#, 'Lkb1[GL00019]', 1], [#, 'Lkb1[HMS01351]', 1], [#, 'Lkb1[KK108675]', 1], [#, 'Droj2[GD14050]', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [9, [#, [#, 'HGNC:6677', 34], [#, 'HGNC:2638', 35], [#, 'HGNC:9475', 36], [#, 'HGNC:59', 44], [#, 'HGNC:10024', 45], [#, 'HGNC:12530', 48]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [9, [#, [#, 'HGNC:26013', 1], [#, 'HGNC:7703', 1], [#, 'HGNC:24846', 1], [#, 'HGNC:5201', 1], [#, 'HGNC:8966', 1], [#, 'HGNC:12756', 1], [#, 'HGNC:12974', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [10, [#, [#, 'LPL', 34], [#, 'CYP3A5', 35], [#, 'PRSS1', 36], [#, 'ABCC8', 44], [#, 'RLBP1', 45], [#, 'UGT1A1', 48]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [10, [#, [#, 'DNAAF5', 1], [#, 'NDUFB8', 1], [#, 'GAS2L2', 1], [#, 'HS6ST1', 1], [#, 'PIGL', 1], [#, 'WDR4', 1], [#, 'RNF113A', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [11, [#, [#, 'modeled by FLYBASE:Hsap\\HTT[128Q.1-336.UAS]; FB:FBal0294194', 129], [#, 'modeled by FLYBASE:Hsap\\MAPT[UAS.cWa]; FB:FBal0126526', 181], [#, 'modeled by FLYBASE:Hsap\\MAPT[R406W.UAS]; FB:FBal0126527', 189], [#, 'modeled by FLYBASE:Hsap\\ATXN3[tr.Q78.UAS.Tag:HA]; FB:FBal0090630', 241], [#, 'CEC', 361], [#, 'CEA', 4981], [#, 'IEA', 6240]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [11, [#, [#, 'is exacerbated by FLYBASE:Pka-C1[GD1276]; FB:FBal0208081', 1], [#, 'is ameliorated by FLYBASE:wg[l-12]; FB:FBal0018504', 1], [#, 'is ameliorated by FLYBASE:nej[Q7]; FB:FBal0094386', 1], [#, 'is ameliorated by FLYBASE:mam[8]; FB:FBal0012016', 1], [#, 'is ameliorated by FLYBASE:gro[1]; FB:FBal0005217', 1], [#, 'is ameliorated by FLYBASE:fz[EY13696]; FB:FBal0159195', 1], [#, 'is ameliorated by FLYBASE:fng[52]; FB:FBal0034614', 1]]]).
fb_tsv_pred_stats('most-frequent', disease_model_annotations, [12, [#, [#, 'FBrf0228684', 158], [#, 'FBrf0241270', 174], [#, 'FBrf0248245', 176], [#, 'FBrf0244983', 239], [#, 'FBrf0227999', 245], [#, 'FBrf0218446', 303], [#, 'FBrf0241599', 6240]]]).
fb_tsv_pred_stats('less-frequent', disease_model_annotations, [12, [#, [#, 'FBrf0217529', 1], [#, 'FBrf0230073', 1], [#, 'FBrf0205420', 1], [#, 'FBrf0190823', 1], [#, 'FBrf0221444', 1], [#, 'FBrf0211611', 1], [#, 'FBrf0194165', 1]]]).
fb_tsv_pred_stats('num-columns', fu_gal4_table_, [2]).
fb_tsv_pred_stats('duplicated-rows', fu_gal4_table_, [25810]).
fb_tsv_pred_stats('total-rows', fu_gal4_table_, [43801]).
fb_tsv_pred_stats('unique-values', fu_gal4_table_, [1, 17987, object]).
fb_tsv_pred_stats('unique-values', fu_gal4_table_, [2, 343, object]).
fb_tsv_pred_stats('missing-values', fu_gal4_table_, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fu_gal4_table_, [2, '', 2614]).
fb_tsv_pred_stats('missing-values', fu_gal4_table_, [2, 2614, [#, ''], [#, 2614]]).
fb_tsv_pred_stats('most-frequent', fu_gal4_table_, [1, [#, [#, '      \\driver\\: ', 319], [#, '    ', 319], [#, '      }', 319], [#, '        \\major_stages\\: ', 319], [#, '        \\transposons\\: ', 319], [#, '        \\major_tissues\\: ', 319], [#, '        },', 1580]]]).
fb_tsv_pred_stats('less-frequent', fu_gal4_table_, [1, [#, [#, '  \\metaData\\: ', 1], [#, '          \\FBst0067166\\: \\67166\\,', 1], [#, '          \\FBst0067167\\: \\67167\\,', 1], [#, '          \\FBst0067168\\: \\67168\\,', 1], [#, '          \\FBst0067175\\: \\67175\\,', 1], [#, '          \\FBst0067177\\: \\67177\\,', 1], [#, '          \\FBst0067178\\: \\67178\\,', 1]]]).
fb_tsv_pred_stats('most-frequent', fu_gal4_table_, [2, [#, [#, 'GawB}fru<up>NP0021</up>\\', 1], [#, 'Or47b-GAL4.7.467}\\', 1], [#, 'ato-GAL4.3.6}\\', 1], [#, 'GAL4}pnt<up>14-94</up>\\', 1], [#, 'en2.4-GAL4}\\', 3], [#, '},', 154]]]).
fb_tsv_pred_stats('less-frequent', fu_gal4_table_, [2, [#, [#, 'GawB}C587\\', 1], [#, 'GawB}fru<up>NP0021</up>\\', 1], [#, 'GMR9D11-GAL4}\\', 1], [#, 'alrm-GAL4.D}\\', 1], [#, 'GAL4-lacZ-Ubx(-FRT).dC}@; see @Scer\\\\GAL4[Ubx.PdC]@ elsewhere in this table.\\,', 1], [#, 'GAL4-lacZ-Ubx(FRT.f<up>+</up>).dC}\\', 1], [#, 'GAL4-\\03b1Tub84B(-FRT).P}@; see @Scer\\\\GAL4[\\03b1Tub84B.PP]@ elsewhere in this table.\\', 1]]]).
fb_tsv_pred_stats('num-columns', insertion_mapping, [7]).
fb_tsv_pred_stats('duplicated-rows', insertion_mapping, [92]).
fb_tsv_pred_stats('total-rows', insertion_mapping, [212010]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [1, 211918, object]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [2, 211884, object]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [3, 64780, object]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [4, 3, object]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [5, 4, object]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [6, 7609, object]).
fb_tsv_pred_stats('unique-values', insertion_mapping, [7, 3716, object]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [2, '', 127]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [2, 127, [#, ''], [#, 127]]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [3, '', 139921]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [3, 139921, [#, ''], [#, 139921]]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [4, '', 139921]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [4, 139921, [#, ''], [#, 139921]]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [5, '', 139921]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [5, 139921, [#, ''], [#, 139921]]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [6, '', 129488]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [6, 129488, [#, ''], [#, 129488]]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [7, '', 203698]).
fb_tsv_pred_stats('null-value-count', insertion_mapping, [7, ?, 70]).
fb_tsv_pred_stats('missing-values', insertion_mapping, [7, 203768, [#, '', ?], [#, 203698, 70]]).
fb_tsv_pred_stats('most-frequent', insertion_mapping, [1, [#, [#, 'P{Roc1b.+t1.17}', 4], [#, 'P{Mcp-wy', 4], [#, 'P{Fab7-w', 6], [#, 'P{SUPor-P.PRE', 9], [#, 'P{ush-lacZ.-2190.-1421}', 9], [#, 'P{Mcp-ff', 13], [#, 'P{Mcp-w', 44]]]).
fb_tsv_pred_stats('less-frequent', insertion_mapping, [1, [#, [#, 'P{UASp-GAP43.mEos}1', 1], [#, 'P{KK106851}VIE-260B', 1], [#, 'P{KK106853}VIE-260B', 1], [#, 'P{KK106855}VIE-260B', 1], [#, 'P{KK106857}VIE-260B', 1], [#, 'P{KK106859}VIE-260B', 1], [#, 'P{KK106861}VIE-260B', 1]]]).
fb_tsv_pred_stats('most-frequent', insertion_mapping, [2, [#, [#, 'FBti0120120', 1], [#, 'FBti0120119', 1], [#, 'FBti0120118', 1], [#, 'FBti0120117', 1], [#, 'FBti0120126', 1], [#, 'FBti0129988', 1]]]).
fb_tsv_pred_stats('less-frequent', insertion_mapping, [2, [#, [#, 'FBti0129988', 1], [#, 'FBti0120115', 1], [#, 'FBti0120116', 1], [#, 'FBti0120117', 1], [#, 'FBti0120118', 1], [#, 'FBti0120119', 1], [#, 'FBti0120120', 1]]]).
fb_tsv_pred_stats('most-frequent', insertion_mapping, [3, [#, [#, '3L:4692761..4692761', 19], [#, '3L:3250542..3250542', 20], [#, 'X:246305..246305', 21], [#, '3R:31883471..31883471', 21], [#, '2L:22237519..22237519', 37], [#, 'X:6861890..6861890', 53]]]).
fb_tsv_pred_stats('less-frequent', insertion_mapping, [3, [#, [#, '3R:5508394..5508394', 1], [#, '3R:9353290..9353290', 1], [#, '3R:8033496..8033496', 1], [#, '2L:17260772..17260772', 1], [#, '3L:540584..540584', 1], [#, '3L:8360935..8360935', 1], [#, '3L:11819914..11819914', 1]]]).
fb_tsv_pred_stats('most-frequent', insertion_mapping, [4, [#, [#, t, 7054], [#, f, 65035]]]).
fb_tsv_pred_stats('less-frequent', insertion_mapping, [4, [#, [#, t, 7054], [#, f, 65035]]]).
fb_tsv_pred_stats('most-frequent', insertion_mapping, [5, [#, [#, 0.0, 10777], [#, -1.0, 26604], [#, 1.0, 34708]]]).
fb_tsv_pred_stats('less-frequent', insertion_mapping, [5, [#, [#, 0.0, 10777], [#, -1.0, 26604], [#, 1.0, 34708]]]).
fb_tsv_pred_stats('most-frequent', insertion_mapping, [7, [#, [#, 75, 'B', 30], [#, '12C1-12C2', 41], [#, '35D1-35D2', 44], [#, '21B4-21B6', 48], [#, '47A11-47A14', 55]]]).
fb_tsv_pred_stats('less-frequent', insertion_mapping, [7, [#, [#, '59B4-59B5', 1], [#, '79D1-79D4', 1], [#, '50C4-50C5', 1], [#, '60D5-60D6', 1], [#, '47E4-47E5', 1], [#, '47A1-47B14', 1], [#, '46B13--46E4', 1]]]).
fb_tsv_pred_stats('num-columns', 'cyto-genetic-seq', [4]).
fb_tsv_pred_stats('duplicated-rows', 'cyto-genetic-seq', [0]).
fb_tsv_pred_stats('total-rows', 'cyto-genetic-seq', [600]).
fb_tsv_pred_stats('unique-values', 'cyto-genetic-seq', [1, 600, object]).
fb_tsv_pred_stats('unique-values', 'cyto-genetic-seq', [2, 292, object]).
fb_tsv_pred_stats('unique-values', 'cyto-genetic-seq', [3, 593, object]).
fb_tsv_pred_stats('unique-values', 'cyto-genetic-seq', [4, 8, object]).
fb_tsv_pred_stats('missing-values', 'cyto-genetic-seq', [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'cyto-genetic-seq', [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', 'cyto-genetic-seq', [3, '', 8]).
fb_tsv_pred_stats('missing-values', 'cyto-genetic-seq', [3, 8, [#, ''], [#, 8]]).
fb_tsv_pred_stats('null-value-count', 'cyto-genetic-seq', [4, '', 577]).
fb_tsv_pred_stats('missing-values', 'cyto-genetic-seq', [4, 577, [#, ''], [#, 577]]).
fb_tsv_pred_stats('most-frequent', 'cyto-genetic-seq', [2, [#, [#, '3-[43]', 7], [#, '3-[47.5]', 7], [#, '3-[48]', 10], [#, '2-[55]', 13], [#, '3-[45]', 17], [#, '2-[54]', 24], [#, '3-[47]', 24]]]).
fb_tsv_pred_stats('less-frequent', 'cyto-genetic-seq', [2, [#, [#, '3-[85]', 1], [#, '3-[84]', 1], [#, '3-[97]', 1], [#, '3-[96]', 1], [#, '3-[95]', 1], [#, '2-[69]', 1], [#, '2-[70]', 1]]]).
fb_tsv_pred_stats('most-frequent', 'cyto-genetic-seq', [3, [#, [#, '3L:10316396..10769950', 1], [#, '3L:9865311..10316395', 1], [#, '3L:9547270..9865310', 1], [#, '3L:9333838..9547269', 1], [#, '3L:11174026..11254920', 1], [#, 'X:408583..660483', 1]]]).
fb_tsv_pred_stats('less-frequent', 'cyto-genetic-seq', [3, [#, [#, 'X:408583..660483', 1], [#, '3L:8981731..9125749', 1], [#, '3L:9125750..9333837', 1], [#, '3L:9333838..9547269', 1], [#, '3L:9547270..9865310', 1], [#, '3L:9865311..10316395', 1], [#, '3L:10316396..10769950', 1]]]).
fb_tsv_pred_stats('most-frequent', 'cyto-genetic-seq', [4, [#, [#, 'includes 16 area(s) of non-identity', 1], [#, 'includes 24 area(s) of non-identity', 1], [#, 'failed to convert', 3], [#, 'in area of non-identity', 5], [#, inversion, 6], [#, 'includes 1 area of non-identity', 6]]]).
fb_tsv_pred_stats('less-frequent', 'cyto-genetic-seq', [4, [#, [#, 'includes 24 area(s) of non-identity', 1], [#, 'includes 16 area(s) of non-identity', 1], [#, 'includes 6 area(s) of non-identity', 1], [#, 'failed to convert', 3], [#, 'in area of non-identity', 5], [#, 'includes 1 area of non-identity', 6], [#, inversion, 6]]]).
fb_tsv_pred_stats('num-columns', 'genome-cyto-seq', [3]).
fb_tsv_pred_stats('duplicated-rows', 'genome-cyto-seq', [0]).
fb_tsv_pred_stats('total-rows', 'genome-cyto-seq', [5033]).
fb_tsv_pred_stats('unique-values', 'genome-cyto-seq', [1, 5033, object]).
fb_tsv_pred_stats('unique-values', 'genome-cyto-seq', [2, 5033, int64]).
fb_tsv_pred_stats('unique-values', 'genome-cyto-seq', [3, 5033, int64]).
fb_tsv_pred_stats('missing-values', 'genome-cyto-seq', [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'genome-cyto-seq', [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', 'genome-cyto-seq', [3, 0, [], []]).
fb_tsv_pred_stats('num-columns', dataset_metadata, [4]).
fb_tsv_pred_stats('duplicated-rows', dataset_metadata, [0]).
fb_tsv_pred_stats('total-rows', dataset_metadata, [22220266]).
fb_tsv_pred_stats('unique-values', dataset_metadata, [1, 3493, object]).
fb_tsv_pred_stats('unique-values', dataset_metadata, [2, 3493, object]).
fb_tsv_pred_stats('unique-values', dataset_metadata, [3, 1885456, object]).
fb_tsv_pred_stats('unique-values', dataset_metadata, [4, 1885453, object]).
fb_tsv_pred_stats('missing-values', dataset_metadata, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', dataset_metadata, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', dataset_metadata, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', dataset_metadata, [4, unknown, 1]).
fb_tsv_pred_stats('missing-values', dataset_metadata, [4, 1, [#, unknown], [#, 1]]).
fb_tsv_pred_stats('most-frequent', dataset_metadata, [1, [#, [#, 'FBlc0000146', 67302], [#, 'FBlc0000142', 67302], [#, 'FBlc0000143', 67302], [#, 'FBlc0000137', 67302], [#, 'FBlc0000005', 161905], [#, 'FBlc0000049', 220083], [#, 'FBlc0000029', 241818]]]).
fb_tsv_pred_stats('less-frequent', dataset_metadata, [1, [#, [#, 'FBlc0002175', 1], [#, 'FBlc0001943', 1], [#, 'FBlc0002094', 1], [#, 'FBlc0002095', 1], [#, 'FBlc0002096', 1], [#, 'FBlc0002097', 1], [#, 'FBlc0002098', 1]]]).
fb_tsv_pred_stats('most-frequent', dataset_metadata, [2, [#, [#, mE_mRNA_P15_junctions, 67302], [#, mE_mRNA_P5_junctions, 67302], [#, mE_mRNA_P6_junctions, 67302], [#, mE_mRNA_L3_12hr_junctions, 67302], [#, 'EK_EP_cDNA', 161905], [#, 'RP_cDNA', 220083], [#, 'Affymetrix_GeneChip_v2', 241818]]]).
fb_tsv_pred_stats('less-frequent', dataset_metadata, [2, [#, [#, 'ChIP-Seq_mE5125_HDAC1_E16', 1], [#, 'ChIP-chip_mE3893_Su(var)205_RNAi_Su(var)205_S2-DRSC', 1], [#, 'ChIP-chip_mE2667_Su(var)205_CME-W1-Cl.8+', 1], [#, 'ChIP-chip_mE2666_Su(var)205_ML-DmBG3-c2', 1], [#, 'ChIP-chip_mE4126_Su(var)205_ML-DmBG3-c2', 1], [#, 'ChIP-chip_mE6074_Su(var)205_S2-DRSC', 1], [#, 'ChIP-chip_mE3707_Su(var)205_S2-DRSC', 1]]]).
fb_tsv_pred_stats('most-frequent', dataset_metadata, [3, [#, [#, 'FBgn0011217', 1890], [#, 'FBgn0015790', 1890], [#, 'FBgn0040372', 1891], [#, 'FBgn0002781', 1892], [#, 'FBgn0013263', 1896], [#, 'FBgn0020412', 1909], [#, 'FBgn0261617', 1911]]]).
fb_tsv_pred_stats('less-frequent', dataset_metadata, [3, [#, [#, '1616608_a_at_1350', 1], [#, 'FBsf0000240631', 1], [#, 'FBsf0000240630', 1], [#, 'FBsf0000240629', 1], [#, 'FBsf0000240628', 1], [#, 'FBsf0000240627', 1], [#, 'FBsf0000240626', 1]]]).
fb_tsv_pred_stats('most-frequent', dataset_metadata, [4, [#, [#, 'Rab11', 1890], [#, eff, 1890], [#, 'G9a', 1891], [#, 'mod(mdg4)', 1892], [#, 'Trl', 1896], [#, 'JIL-1', 1909], [#, nej, 1911]]]).
fb_tsv_pred_stats('less-frequent', dataset_metadata, [4, [#, [#, '1616608_a_at_1350', 1], [#, 'TFBS_D_002907', 1], [#, 'TFBS_D_002906', 1], [#, 'TFBS_D_002905', 1], [#, 'TFBS_D_002904', 1], [#, 'TFBS_D_002903', 1], [#, 'TFBS_D_002902', 1]]]).
fb_tsv_pred_stats('num-columns', dmel_paralogs, [11]).
fb_tsv_pred_stats('duplicated-rows', dmel_paralogs, [0]).
fb_tsv_pred_stats('total-rows', dmel_paralogs, [223055]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [1, 10772, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [2, 10772, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [3, 12, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [4, 10693, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [5, 2, int64]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [6, 10772, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [7, 10772, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [8, 12, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [9, 10693, object]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [10, 2, int64]).
fb_tsv_pred_stats('unique-values', dmel_paralogs, [11, 13, int64]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [5, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [6, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [7, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [8, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [9, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [10, 0, [], []]).
fb_tsv_pred_stats('missing-values', dmel_paralogs, [11, 0, [], []]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [1, [#, [#, 'FBgn0036891', 247], [#, 'FBgn0033362', 247], [#, 'FBgn0042098', 248], [#, 'FBgn0034661', 249], [#, 'FBgn0039272', 249], [#, 'FBgn0031619', 250], [#, 'FBgn0023479', 279]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [1, [#, [#, 'FBgn0040606', 1], [#, 'FBgn0030608', 1], [#, 'FBgn0263086', 1], [#, 'FBgn0263094', 1], [#, 'FBgn0030613', 1], [#, 'FBgn0037719', 1], [#, 'FBgn0013949', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [2, [#, [#, 'CG9372', 247], [#, 'CG8172', 247], [#, 'CG18735', 248], [#, tpr, 249], [#, 'CG11836', 249], [#, 'CG3355', 250], [#, teq, 279]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [2, [#, [#, 'CG6503', 1], [#, 'Lsd-2', 1], [#, 'CG43354', 1], [#, 'CG43362', 1], [#, 'Rab3-GEF', 1], [#, bocks, 1], [#, 'Elal', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [3, [#, [#, 'Y', 220], [#, 4.0, 1269], [#, 'X', 27590], [#, 2, 'L', 38491], [#, 3, 'L', 43684], [#, 2, 'R', 52635], [#, 3, 'R', 59090]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [3, [#, [#, 211000022280494.0, 1], [#, 211000022278760.0, 17], [#, 211000022279188.0, 17], [#, 'Unmapped_Scaffold_8_D1580_D1567', 18], [#, rDNA, 23], [#, 'Y', 220], [#, 4.0, 1269]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [4, [#, [#, '9007234..9013526', 247], [#, '21798017..21799195', 248], [#, '21799330..21800904', 249], [#, '25071126..25072712', 249], [#, '4651403..4652892', 250], [#, '9074643..9092131', 279], [#, '3206949..3208985', 384]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [4, [#, [#, '7918517..7920131', 1], [#, '770363..770462', 1], [#, '11993989..11995432', 1], [#, '11997900..11999340', 1], [#, '22608670..22613656', 1], [#, '9839776..9841163', 1], [#, '5563977..5564815', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [5, [#, [#, 1, 107829], [#, -1, 115226]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [5, [#, [#, 1, 107829], [#, -1, 115226]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [6, [#, [#, 'FBgn0036891', 247], [#, 'FBgn0033362', 247], [#, 'FBgn0042098', 248], [#, 'FBgn0034661', 249], [#, 'FBgn0039272', 249], [#, 'FBgn0031619', 250], [#, 'FBgn0023479', 279]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [6, [#, [#, 'FBgn0004867', 1], [#, 'FBgn0031971', 1], [#, 'FBgn0046247', 1], [#, 'FBgn0026702', 1], [#, 'FBgn0032699', 1], [#, 'FBgn0039454', 1], [#, 'FBgn0031077', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [7, [#, [#, 'CG9372', 247], [#, 'CG8172', 247], [#, 'CG18735', 248], [#, tpr, 249], [#, 'CG11836', 249], [#, 'CG3355', 250], [#, teq, 279]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [7, [#, [#, 'RpS2', 1], [#, 'Sirup', 1], [#, 'CG5938', 1], [#, 'Pop1', 1], [#, 'CG10383', 1], [#, 'CG14247', 1], [#, 'THADA', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [8, [#, [#, 'Y', 220], [#, 4.0, 1269], [#, 'X', 27590], [#, 2, 'L', 38491], [#, 3, 'L', 43684], [#, 2, 'R', 52635], [#, 3, 'R', 59090]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [8, [#, [#, 211000022280494.0, 1], [#, 211000022278760.0, 17], [#, 211000022279188.0, 17], [#, 'Unmapped_Scaffold_8_D1580_D1567', 18], [#, rDNA, 23], [#, 'Y', 220], [#, 4.0, 1269]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [9, [#, [#, '9007234..9013526', 247], [#, '21798017..21799195', 248], [#, '25071126..25072712', 249], [#, '21799330..21800904', 249], [#, '4651403..4652892', 250], [#, '9074643..9092131', 279], [#, '3206949..3208985', 384]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [9, [#, [#, '9896265..9897552', 1], [#, '11126877..11129087', 1], [#, '2079919..2081286', 1], [#, '22080382..22083473', 1], [#, '17849727..17852453', 1], [#, '17830828..17841705', 1], [#, '5093976..5095910', 1]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [10, [#, [#, 1, 107830], [#, -1, 115225]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [10, [#, [#, 1, 107830], [#, -1, 115225]]]).
fb_tsv_pred_stats('most-frequent', dmel_paralogs, [11, [#, [#, 7, 4614], [#, 6, 6222], [#, 5, 8946], [#, 4, 14762], [#, 3, 27362], [#, 2, 39938], [#, 1, 105517]]]).
fb_tsv_pred_stats('less-frequent', dmel_paralogs, [11, [#, [#, 13, 230], [#, 12, 1190], [#, 11, 2910], [#, 9, 3614], [#, 8, 3650], [#, 10, 4100], [#, 7, 4614]]]).
fb_tsv_pred_stats('num-columns', entity_publication, [4]).
fb_tsv_pred_stats('duplicated-rows', entity_publication, [344]).
fb_tsv_pred_stats('total-rows', entity_publication, [5580889]).
fb_tsv_pred_stats('unique-values', entity_publication, [1, 1952735, object]).
fb_tsv_pred_stats('unique-values', entity_publication, [2, 1952521, object]).
fb_tsv_pred_stats('unique-values', entity_publication, [3, 105916, object]).
fb_tsv_pred_stats('unique-values', entity_publication, [4, 63490, object]).
fb_tsv_pred_stats('missing-values', entity_publication, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', entity_publication, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', entity_publication, [3, '', 644]).
fb_tsv_pred_stats('missing-values', entity_publication, [3, 644, [#, ''], [#, 644]]).
fb_tsv_pred_stats('null-value-count', entity_publication, [4, '', 3444472]).
fb_tsv_pred_stats('missing-values', entity_publication, [4, 3444472, [#, ''], [#, 3444472]]).
fb_tsv_pred_stats('most-frequent', entity_publication, [1, [#, [#, 'FBgn0003996', 4465], [#, 'FBgn0014446', 4694], [#, 'FBto0000342', 4872], [#, 'FBgn0014447', 5750], [#, 'FBto0000180', 5793], [#, 'FBte0000037', 11131], [#, 'FBgn0014445', 17375]]]).
fb_tsv_pred_stats('less-frequent', entity_publication, [1, [#, [#, 'FBtr0334316', 1], [#, 'FBig0000117350', 1], [#, 'FBig0000117351', 1], [#, 'FBig0000117352', 1], [#, 'FBig0000117353', 1], [#, 'FBig0000117354', 1], [#, 'FBig0000117355', 1]]]).
fb_tsv_pred_stats('most-frequent', entity_publication, [2, [#, [#, w, 4465], [#, 'Avic\\GFP', 4694], [#, 'UASt', 4872], [#, 'Ecol\\lacZ', 5750], [#, 'UAS', 5793], [#, 'P-element', 11131], [#, 'Scer\\GAL4', 17375]]]).
fb_tsv_pred_stats('less-frequent', entity_publication, [2, [#, [#, 'lncRNA:Hsromega-RG', 1], [#, 'FBgn0037913-FBgn0286818', 1], [#, 'FBgn0025885-FBgn0286818', 1], [#, pA_site_034101, 1], [#, 'FBgn0003944-FBgn0013263', 1], [#, 'FBgn0003545-FBgn0260639', 1], [#, 'FBgn0014189-FBgn0035110', 1]]]).
fb_tsv_pred_stats('most-frequent', entity_publication, [3, [#, [#, 'FBrf0104946', 130692], [#, 'FBrf0213603', 147340], [#, 'FBrf0239962', 162644], [#, 'FBrf0241309', 162865], [#, 'FBrf0200326', 176400], [#, 'FBrf0202436', 182332], [#, 'FBrf0105495', 545468]]]).
fb_tsv_pred_stats('less-frequent', entity_publication, [3, [#, [#, 'FBrf0201074', 1], [#, 'FBrf0126212', 1], [#, 'FBrf0134967', 1], [#, 'FBrf0144507', 1], [#, 'FBrf0125535', 1], [#, 'FBrf0155006', 1], [#, 'FBrf0086185', 1]]]).
fb_tsv_pred_stats('most-frequent', entity_publication, [4, [#, [#, 15238527.0, 40234], [#, 22936248.0, 43755], [#, 19465919.0, 49687], [#, 28851752.0, 57661], [#, 29191225.0, 75081], [#, 17994087.0, 176400]]]).
fb_tsv_pred_stats('less-frequent', entity_publication, [4, [#, [#, 17435764.0, 1], [#, 9345112.0, 1], [#, 29709602.0, 1], [#, 9013669.0, 1], [#, 12921737.0, 1], [#, 27591190.0, 1], [#, 25713358.0, 1]]]).
fb_tsv_pred_stats('num-columns', fbrf_pmid_pmcid_doi, [7]).
fb_tsv_pred_stats('duplicated-rows', fbrf_pmid_pmcid_doi, [0]).
fb_tsv_pred_stats('total-rows', fbrf_pmid_pmcid_doi, [99294]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [1, 99294, object]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [2, 99294, int64]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [3, 43548, object]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [4, 91433, object]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [5, 27, object]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [6, 99240, object]).
fb_tsv_pred_stats('unique-values', fbrf_pmid_pmcid_doi, [7, 71, object]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [2, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fbrf_pmid_pmcid_doi, [3, '', 55747]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [3, 55747, [#, ''], [#, 55747]]).
fb_tsv_pred_stats('null-value-count', fbrf_pmid_pmcid_doi, [4, '', 7862]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [4, 7862, [#, ''], [#, 7862]]).
fb_tsv_pred_stats('null-value-count', fbrf_pmid_pmcid_doi, [5, '', 10]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [5, 10, [#, ''], [#, 10]]).
fb_tsv_pred_stats('null-value-count', fbrf_pmid_pmcid_doi, [6, '', 10]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [6, 10, [#, ''], [#, 10]]).
fb_tsv_pred_stats('null-value-count', fbrf_pmid_pmcid_doi, [7, '', 10]).
fb_tsv_pred_stats('missing-values', fbrf_pmid_pmcid_doi, [7, 10, [#, ''], [#, 10]]).
fb_tsv_pred_stats('most-frequent', fbrf_pmid_pmcid_doi, [3, [#, [#, 'PMC365244', 1], [#, 'PMC365050', 1], [#, 'PMC365176', 1], [#, 'PMC254114', 1], [#, 'PMC6569114', 1], [#, 'PMC365333', 1]]]).
fb_tsv_pred_stats('less-frequent', fbrf_pmid_pmcid_doi, [3, [#, [#, 'PMC5728115', 1], [#, 'PMC6569114', 1], [#, 'PMC254114', 1], [#, 'PMC365176', 1], [#, 'PMC365050', 1], [#, 'PMC365244', 1], [#, 'PMC365271', 1]]]).
fb_tsv_pred_stats('most-frequent', fbrf_pmid_pmcid_doi, [4, [#, [#, '10.1002/(SICI)1097-0169(1997)37:4<300::AID-CM2>3.0.CO;2-8', 1], [#, '10.1006/excr.1997.3618', 1], [#, '10.1126/science.277.5327.825', 1], [#, '10.1016/s0169-4758(97)01058-2', 1], [#, '10.1007/pl00006177', 1], [#, '10.1111/j.1440-169X.1977.00345.x', 1]]]).
fb_tsv_pred_stats('less-frequent', fbrf_pmid_pmcid_doi, [4, [#, [#, '10.1111/j.1440-169X.1977.00345.x', 1], [#, '10.1126/science.277.5327.825', 1], [#, '10.1006/excr.1997.3618', 1], [#, '10.1002/(SICI)1097-0169(1997)37:4<300::AID-CM2>3.0.CO;2-8', 1], [#, '10.1002/(SICI)1520-6327(1997)36:1<51::AID-ARCH5>3.0.CO;2-Z', 1], [#, '10.1021/bi970460d', 1], [#, '10.1002/dvg.1020150103', 1]]]).
fb_tsv_pred_stats('most-frequent', fbrf_pmid_pmcid_doi, [5, [#, [#, 'conference report', 178], [#, abstract, 191], [#, erratum, 306], [#, letter, 308], [#, note, 2322], [#, review, 13466], [#, paper, 81917]]]).
fb_tsv_pred_stats('less-frequent', fbrf_pmid_pmcid_doi, [5, [#, [#, book, 1], [#, 'bibliographic list', 1], [#, poster, 1], [#, website, 1], [#, lecture, 1], [#, preprint, 1], [#, thesis, 1]]]).
fb_tsv_pred_stats('most-frequent', fbrf_pmid_pmcid_doi, [6, [#, [#, 'Dernburg, 2011, Cold Spring Harb. Protoc. 2011(12): ', 3], [#, 'Shen, 2012, Cold Spring Harb. Protoc. 2012(5): ', 3], [#, 'Marley and Baines, 2011, Cold Spring Harb. Protoc. 2011(9): ', 3], [#, 'Sweeney et al., 2012, Cold Spring Harb. Protoc. 2012(2): ', 3], [#, 'Andlauer and Sigrist, 2012, Cold Spring Harb. Protoc. 2012(4): ', 4], [#, 'Macleod, 2012, Cold Spring Harb. Protoc. 2012(7): ', 5]]]).
fb_tsv_pred_stats('less-frequent', fbrf_pmid_pmcid_doi, [6, [#, [#, 'Rizki et al., 1977, Dev. Growth Differ. 19(4): 345--356', 1], [#, 'Kutskova and Mamon, 1996, Genetika, Moscow 32(10): 1406--1416', 1], [#, 'Kuromi et al., 1997, Neurosci. Res. 27(2): 101--113', 1], [#, 'Kuhl and Wedlich, 1997, BioEssays 19(2): 101--104', 1], [#, 'Kosuda, 1996, Zool. Sci., Tokyo 13(6): 821--824', 1], [#, 'Kosman and Small, 1997, Development 124(7): 1343--1354', 1], [#, 'Kidd and Raff, 1997, J. Cell Sci. 110(2): 209--219', 1]]]).
fb_tsv_pred_stats('most-frequent', fbrf_pmid_pmcid_doi, [7, [#, [#, fb_2019_01, 667], [#, fb_2015_01, 799], [#, fb_2017_04, 959], [#, fb_2016_05, 1908], [#, fb_2021_05, 2210], [#, fb_2018_03, 6651], [#, fb_2011_10, 53869]]]).
fb_tsv_pred_stats('less-frequent', fbrf_pmid_pmcid_doi, [7, [#, [#, fb_2018_02, 363], [#, fb_2012_06, 375], [#, fb_2019_02, 394], [#, fb_2018_06, 402], [#, fb_2012_01, 407], [#, fb_2013_03, 410]]]).
fb_tsv_pred_stats('num-columns', organism_list, [6]).
fb_tsv_pred_stats('duplicated-rows', organism_list, [0]).
fb_tsv_pred_stats('total-rows', organism_list, [57986]).
fb_tsv_pred_stats('unique-values', organism_list, [1, 19142, object]).
fb_tsv_pred_stats('unique-values', organism_list, [2, 31712, object]).
fb_tsv_pred_stats('unique-values', organism_list, [3, 1044, object]).
fb_tsv_pred_stats('unique-values', organism_list, [4, 103, object]).
fb_tsv_pred_stats('unique-values', organism_list, [5, 960, object]).
fb_tsv_pred_stats('unique-values', organism_list, [6, 2, object]).
fb_tsv_pred_stats('null-value-count', organism_list, [1, unknown, 9]).
fb_tsv_pred_stats('missing-values', organism_list, [1, 9, [#, unknown], [#, 9]]).
fb_tsv_pred_stats('null-value-count', organism_list, [2, '', 1]).
fb_tsv_pred_stats('null-value-count', organism_list, [2, unknown, 7]).
fb_tsv_pred_stats('null-value-count', organism_list, [2, '.', 13]).
fb_tsv_pred_stats('missing-values', organism_list, [2, 21, [#, '', unknown, '.'], [#, 1, 7, 13]]).
fb_tsv_pred_stats('null-value-count', organism_list, [3, '', 56943]).
fb_tsv_pred_stats('missing-values', organism_list, [3, 56943, [#, ''], [#, 56943]]).
fb_tsv_pred_stats('null-value-count', organism_list, [4, '', 57883]).
fb_tsv_pred_stats('missing-values', organism_list, [4, 57883, [#, ''], [#, 57883]]).
fb_tsv_pred_stats('null-value-count', organism_list, [5, '', 57027]).
fb_tsv_pred_stats('missing-values', organism_list, [5, 57027, [#, ''], [#, 57027]]).
fb_tsv_pred_stats('null-value-count', organism_list, [6, '', 57353]).
fb_tsv_pred_stats('missing-values', organism_list, [6, 57353, [#, ''], [#, 57353]]).
fb_tsv_pred_stats('most-frequent', organism_list, [1, [#, [#, 'Anopheles', 200], [#, 'Bombus', 206], [#, 'Bactrocera', 208], [#, 'Trigonopterus', 269], [#, 'Carabus', 364], [#, 'Bembidion', 399], [#, 'Drosophila', 801]]]).
fb_tsv_pred_stats('less-frequent', organism_list, [1, [#, [#, 'Aaages', 1], [#, 'Mononeuron', 1], [#, 'Mononychus', 1], [#, 'Monophadnus', 1], [#, 'Monophlebulus', 1], [#, 'Monoplistes', 1], [#, 'Monosiga', 1]]]).
fb_tsv_pred_stats('most-frequent', organism_list, [2, [#, [#, orientalis, 59], [#, 'n. sp.', 63], [#, affinis, 63], [#, japonica, 71], [#, bicolor, 73], [#, 'gen. sp.', 94], [#, 'sp.', 6204]]]).
fb_tsv_pred_stats('less-frequent', organism_list, [2, [#, [#, prior, 1], [#, 'nr. argyropleura AHD-2011', 1], [#, argyropleura, 1], [#, ericae, 1], [#, pictellus, 1], [#, triplex, 1], [#, caudex, 1]]]).
fb_tsv_pred_stats('most-frequent', organism_list, [3, [#, [#, 'Dvar', 1], [#, 'Dval', 1], [#, 'Dust', 1], [#, 'Duni', 1], [#, 'Duns', 1], [#, 'Dunm', 1]]]).
fb_tsv_pred_stats('less-frequent', organism_list, [3, [#, [#, 'Dpac', 1], [#, 'Duns', 1], [#, 'Duni', 1], [#, 'Dust', 1], [#, 'Dval', 1], [#, 'Dvar', 1], [#, 'Dvnz', 1]]]).
fb_tsv_pred_stats('most-frequent', organism_list, [4, [#, [#, 'eastern newt', 1], [#, rice, 1], [#, 'Japanese medaka', 1], [#, 'red mason bee', 1], [#, 'laboratory mouse', 1], [#, 'fruit fly', 2]]]).
fb_tsv_pred_stats('less-frequent', organism_list, [4, [#, [#, 'Chilean rose tarantula', 1], [#, 'Japanese medaka', 1], [#, rice, 1], [#, 'eastern newt', 1], [#, 'brown planthopper', 1], [#, 'common tobacco', 1], [#, 'starlet sea anemone', 1]]]).
fb_tsv_pred_stats('most-frequent', organism_list, [5, [#, [#, 132243.0, 1], [#, 7244.0, 1], [#, 306030.0, 1], [#, 95458.0, 1], [#, 30050.0, 1], [#, 62873.0, 1]]]).
fb_tsv_pred_stats('less-frequent', organism_list, [5, [#, [#, 103846.0, 1], [#, 30050.0, 1], [#, 95458.0, 1], [#, 306030.0, 1], [#, 7244.0, 1], [#, 132243.0, 1], [#, 252922.0, 1]]]).
fb_tsv_pred_stats('most-frequent', organism_list, [6, [#, [#, y, 633]]]).
fb_tsv_pred_stats('less-frequent', organism_list, [6, [#, [#, y, 633]]]).
fb_tsv_pred_stats('num-columns', stocks_FB2023_04, [7]).
fb_tsv_pred_stats('duplicated-rows', stocks_FB2023_04, [0]).
fb_tsv_pred_stats('total-rows', stocks_FB2023_04, [147006]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [1, 147006, object]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [2, 6, object]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [3, 3, object]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [4, 276, object]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [5, 142808, object]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [6, 143601, object]).
fb_tsv_pred_stats('unique-values', stocks_FB2023_04, [7, 146977, object]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [2, '', 1]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [2, 1, [#, ''], [#, 1]]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [3, '', 1]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [3, 1, [#, ''], [#, 1]]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [4, '', 1]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [4, 1, [#, ''], [#, 1]]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [5, '', 21]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [5, ?, 1]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [5, 22, [#, '', ?], [#, 21, 1]]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [6, '', 28]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [6, 28, [#, ''], [#, 28]]).
fb_tsv_pred_stats('null-value-count', stocks_FB2023_04, [7, '', 30]).
fb_tsv_pred_stats('missing-values', stocks_FB2023_04, [7, 30, [#, ''], [#, 30]]).
fb_tsv_pred_stats('most-frequent', stocks_FB2023_04, [2, [#, [#, 'NDSSC', 2072], [#, 'FlyORF', 3059], [#, 'Kyoto', 25744], [#, 'Vienna', 38016], [#, 'Bloomington', 78114]]]).
fb_tsv_pred_stats('less-frequent', stocks_FB2023_04, [2, [#, [#, 'NDSSC', 2072], [#, 'FlyORF', 3059], [#, 'Kyoto', 25744], [#, 'Vienna', 38016], [#, 'Bloomington', 78114]]]).
fb_tsv_pred_stats('most-frequent', stocks_FB2023_04, [3, [#, [#, 'ethanol-preserved specimen ; FBsv0000007', 265], [#, 'living stock ; FBsv0000002', 146740]]]).
fb_tsv_pred_stats('less-frequent', stocks_FB2023_04, [3, [#, [#, 'ethanol-preserved specimen ; FBsv0000007', 265], [#, 'living stock ; FBsv0000002', 146740]]]).
fb_tsv_pred_stats('most-frequent', stocks_FB2023_04, [4, [#, [#, 'Dwil', 40], [#, 'Dmer', 46], [#, 'Dmau', 52], [#, 'Dvir', 77], [#, 'Dpse', 188], [#, 'Dsim', 225], [#, 'Dmel', 145074]]]).
fb_tsv_pred_stats('less-frequent', stocks_FB2023_04, [4, [#, [#, 'Dora', 1], [#, 'Dgrs', 1], [#, 'Dgun', 1], [#, 'Dnec', 1], [#, 'Dprc', 1], [#, 'Dcai', 1]]]).
fb_tsv_pred_stats('most-frequent', stocks_FB2023_04, [5, [#, [#, 'st[1]', 6], [#, 'w[1118]', 7], [#, 'y[1]', 10], [#, 'v[1]', 11], [#, 'w[1]', 21], [#, 'wild-type', 1649]]]).
fb_tsv_pred_stats('less-frequent', stocks_FB2023_04, [5, [#, [#, 'y[1]; M{v[+t1.8]=WKO.1-B11}ZH-86Fb', 1], [#, 'y[1] w[67c23]; P{w[+mC]=GSV7}GS23035/SM1', 1], [#, 'y[1] w[67c23]; P{w[+mC]=GSV7}GS23034/TM3, Sb[1] Ser[1]', 1], [#, 'y[1] w[67c23]; P{w[+mC]=GSV7}GS23027/SM1', 1], [#, 'y[1] w[*]; P{w[+mC]=GSV7}GS23024', 1], [#, 'y[1] w[67c23]; P{w[+mC]=GSV7}GS23023/SM1', 1], [#, 'y[1] w[67c23]; P{w[+mC]=GSV7}GS23022/TM3, Sb[1] Ser[1]', 1]]]).
fb_tsv_pred_stats('most-frequent', stocks_FB2023_04, [6, [#, [#, 'Dana\\wild-type', 29], [#, 'Dari\\wild-type', 31], [#, 'Dwil\\wild-type', 32], [#, 'Dsim\\wild-type', 71], [#, 'Dpse\\wild-type', 95], [#, 'Dmel\\wild-type', 96], [#, 'wild-type', 235]]]).
fb_tsv_pred_stats('less-frequent', stocks_FB2023_04, [6, [#, [#, 'y[1] v[1]; P{y[+t7.7] v[+t1.8]=TKO.GS03376}attP40', 1], [#, 'y[1]w[67c23];P{w[+mC]=GSV7}GS22586/SM1', 1], [#, 'y[1]w[67c23];P{w[+mC]=GSV7}GS22584/SM1', 1], [#, 'y[1] w[67c23] P{w[+mC]=GSV7}GS22580 / Binsinscy', 1], [#, 'y[1] w[67c23] P{w[+mC]=GSV7}GS22579 / Binsinscy', 1], [#, 'y[1] w[67c23] P{w[+mC]=GSV7}GS22578 / Binsinscy', 1], [#, 'y[1] w[67c23] P{w[+mC]=GSV7}GS22577 / Binsinscy', 1]]]).
fb_tsv_pred_stats('most-frequent', stocks_FB2023_04, [7, [#, [#, 201049.0, 1], [#, 201043.0, 1], [#, 201040.0, 1], [#, 201037.0, 1], [#, 201059.0, 1], [#, 2.0, 1]]]).
fb_tsv_pred_stats('less-frequent', stocks_FB2023_04, [7, [#, [#, 2.0, 1], [#, 201073.0, 1], [#, 201072.0, 1], [#, 201070.0, 1], [#, 201063.0, 1], [#, 201062.0, 1], [#, 201059.0, 1]]]).
fb_tsv_pred_stats('num-columns', fb_synonym, [6]).
fb_tsv_pred_stats('duplicated-rows', fb_synonym, [0]).
fb_tsv_pred_stats('total-rows', fb_synonym, [1001254]).
fb_tsv_pred_stats('unique-values', fb_synonym, [1, 1001254, object]).
fb_tsv_pred_stats('unique-values', fb_synonym, [2, 906, object]).
fb_tsv_pred_stats('unique-values', fb_synonym, [3, 1001062, object]).
fb_tsv_pred_stats('unique-values', fb_synonym, [4, 37453, object]).
fb_tsv_pred_stats('unique-values', fb_synonym, [5, 13823, object]).
fb_tsv_pred_stats('unique-values', fb_synonym, [6, 604083, object]).
fb_tsv_pred_stats('missing-values', fb_synonym, [1, 0, [], []]).
fb_tsv_pred_stats('missing-values', fb_synonym, [2, 0, [], []]).
fb_tsv_pred_stats('missing-values', fb_synonym, [3, 0, [], []]).
fb_tsv_pred_stats('null-value-count', fb_synonym, [4, '', 947217]).
fb_tsv_pred_stats('missing-values', fb_synonym, [4, 947217, [#, ''], [#, 947217]]).
fb_tsv_pred_stats('null-value-count', fb_synonym, [5, '', 985999]).
fb_tsv_pred_stats('missing-values', fb_synonym, [5, 985999, [#, ''], [#, 985999]]).
fb_tsv_pred_stats('null-value-count', fb_synonym, [6, '', 370940]).
fb_tsv_pred_stats('null-value-count', fb_synonym, [6, none, 1]).
fb_tsv_pred_stats('missing-values', fb_synonym, [6, 370941, [#, '', none], [#, 370940, 1]]).
fb_tsv_pred_stats('most-frequent', fb_synonym, [2, [#, [#, 'Dana', 19302], [#, 'Dyak', 19532], [#, 'Dsim', 21050], [#, 'Dpse', 22860], [#, 'Scer', 30891], [#, 'Ssss', 158119], [#, 'Dmel', 551458]]]).
fb_tsv_pred_stats('less-frequent', fb_synonym, [2, [#, [#, 'Ppla', 1], [#, 'Dsch', 1], [#, 'Dmar', 1], [#, 'Dlev', 1], [#, 'Dalo', 1], [#, 'Abelson', 1], [#, 'Amag', 1]]]).
fb_tsv_pred_stats('most-frequent', fb_synonym, [3, [#, [#, 'P{eve-lacZ', 8], [#, 'P{ush-lacZ.-2190.-1421}', 10], [#, 'T(2;3)ul10', 11], [#, 'P{SUPor-P.PRE', 11], [#, 'P{Fab7-w', 17], [#, 'P{Mcp-ff', 18], [#, 'P{Mcp-w', 55]]]).
fb_tsv_pred_stats('less-frequent', fb_synonym, [3, [#, [#, '\\03b1-Spec[2]', 1], [#, 'Dmoj\\GI20484', 1], [#, 'Dmoj\\GI20485', 1], [#, 'Dmoj\\GI20486', 1], [#, 'Dmoj\\GI20487', 1], [#, 'Dmoj\\GI20488', 1], [#, 'Dmoj\\GI20489', 1]]]).
fb_tsv_pred_stats('most-frequent', fb_synonym, [4, [#, [#, 'Amyrel', 174], [#, 'Saccharomyces cerevisiae UAS construct a of Schertel', 180], [#, 'mt:CoII', 208], [#, 'Saccharomyces cerevisiae UAS construct a of Unknown', 215], [#, 'UAS construct a of Unknown', 235], [#, 'Deficiency (1) yellow Terminal', 251]]]).
fb_tsv_pred_stats('less-frequent', fb_synonym, [4, [#, [#, 'lethal (3) rI327', 1], [#, 'Secretory Pathway Calcium atpase', 1], [#, 'transfer RNA:Cysteine-GCA 4-1', 1], [#, 'Antioxidant 1 copper chaperone', 1], [#, 'ACAT-related protein required for viability 1', 1], [#, 'ER membrane protein complex subunit 10', 1], [#, 'Structural maintenance of chromosomes 5', 1]]]).
fb_tsv_pred_stats('most-frequent', fb_synonym, [5, [#, [#, 'cytochrome oxidase II', 33], [#, microsatellite, 39], [#, 'yolk protein 1', 43], [#, 'faint little ball', 43], [#, '28S ribosomal RNA', 75], [#, 'alcohol dehydrogenase', 89]]]).
fb_tsv_pred_stats('less-frequent', fb_synonym, [5, [#, [#, 'Golgin-84 ortholog|Golgin84', 1], [#, 'INACTIVE|Inactive|inactive', 1], [#, 'lethal (3) 01640', 1], [#, 'female sterile(3)272-9|humpty dumpty|lethal (3) 82Ff|lethal (3) 82Fk', 1], [#, fritz, 1], [#, 'Or46a', 1], [#, 'Gr36a', 1]]]).
fb_tsv_pred_stats('most-frequent', fb_synonym, [6, [#, [#, 'mt:CoII', 159], [#, 'Amyrel', 164], [#, 1360.0, 208], [#, 'Adh', 232], [#, unspecified, 259], [#, 'INE-1', 2235]]]).
fb_tsv_pred_stats('less-frequent', fb_synonym, [6, [#, [#, 'GA15920|dpse_GLEANR_14759', 1], [#, 'GM24315|dsec_GLEANR_7324', 1], [#, 'GM24316|dsec_GLEANR_7325', 1], [#, 'GM24317|dsec_GLEANR_7326', 1], [#, 'GM24318|dsec_GLEANR_7327', 1], [#, 'GM24319|dsec_GLEANR_7328', 1], [#, 'GM24320|dsec_GLEANR_7329', 1]]]).
fb_tsv_pred_stats('num-columns', transposon_sequence_se, [6]).
fb_tsv_pred_stats('duplicated-rows', transposon_sequence_se, [13]).
fb_tsv_pred_stats('total-rows', transposon_sequence_se, [13564]).
fb_tsv_pred_stats('unique-values', transposon_sequence_se, [1, 13382, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_se, [2, 1, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_se, [3, 1, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_se, [4, 27, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_se, [5, 121, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_se, [6, 22, object]).
fb_tsv_pred_stats('missing-values', transposon_sequence_se, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_se, [2, '', 170]).
fb_tsv_pred_stats('missing-values', transposon_sequence_se, [2, 170, [#, ''], [#, 170]]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_se, [3, '', 170]).
fb_tsv_pred_stats('missing-values', transposon_sequence_se, [3, 170, [#, ''], [#, 170]]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_se, [4, '', 127]).
fb_tsv_pred_stats('missing-values', transposon_sequence_se, [4, 127, [#, ''], [#, 127]]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_se, [5, '', 17]).
fb_tsv_pred_stats('missing-values', transposon_sequence_se, [5, 17, [#, ''], [#, 17]]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_se, [6, '', 16]).
fb_tsv_pred_stats('missing-values', transposon_sequence_se, [6, 16, [#, ''], [#, 16]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_se, [1, [#, [#, 'AGCATTTGGCCGGAAGCTCATGCATAGCCGGCAGAAGCTCTGCGCATTGGCAGAGGCCGC', 2], [#, 'TG', 2], [#, 'AGGTCGAGCTAAATACTTTTCATGCCTTGACTTGATGTCAGGTTTCCATCAAATAGAACT', 2], [#, 'GCGGGTTAGCTGAACCCAACTTCAGCACACTTTGATCATTCGAATAAACAGATTCAAACA', 2], [#, 'AA', 3], [#, 'A', 3], [#, '>FBte', 170]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_se, [1, [#, [#, 'GTGACATATCCATAAGTCCCTAAGACTTAAGCATATGCCTACATACTAATACACTTACAA', 1], [#, 'CCTTTATTTAAAGACAAAACAAATTGGAACATGTTTCGCGACATTTTGGAACAGAATAGG', 1], [#, 'AATATAAATATCTCTCTAAAGACAAACAACGCACTAGACTCCGGAGTAGCATATTTAAAT', 1], [#, 'GAGAACATCATAGATGCAGCAACGCAATCGACACCATCTATAAAAATGAAATGAGAAAAA', 1], [#, 'TCAGGCAAAAGCGTACACTTAGGAGGATACGGCAAAGGACTAGGCATCCAGAAGATAAAA', 1], [#, 'ACAAACTAAATAGAGCAACAGACGAGCTCAAGAGAACTCTCAGGGAAGACAAAGATAACC', 1], [#, 'GACTTCAATACTACCTTAGCAAACTTGAGACTACCTTATCTACAAATTATTCCCTGTGGA', 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_se, [2, []]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_se, [2, []]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_se, [3, []]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_se, [3, []]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_se, [4, [#, [#, 1483.0, 1], [#, 1481.0, 1], [#, 1316.0, 1], [#, 1144.0, 1], [#, 12.0, 3], [#, 1.0, 16]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_se, [4, [#, [#, 1136.0, 1], [#, 1481.0, 1], [#, 1483.0, 1], [#, 1482.0, 1], [#, 1122.0, 1], [#, 1188.0, 1], [#, 1487.0, 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_se, [5, [#, [#, 66.0, 2], [#, 7.0, 2], [#, 8.0, 2], [#, 3.0, 3], [#, 6.0, 3], [#, 1.0, 3]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_se, [5, [#, [#, 958.0, 1], [#, 158.0, 1], [#, 536.0, 1], [#, 19.0, 1], [#, 284.0, 1], [#, 588.0, 1], [#, 647.0, 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_se, [6, [#, [#, 23.0, 1], [#, 88.0, 1], [#, 8.0, 2], [#, 2.0, 2], [#, 3.0, 2], [#, 9.0, 2]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_se, [6, [#, [#, 7.0, 1], [#, 39.0, 1], [#, 4.0, 1], [#, 6.0, 1], [#, 17.0, 1], [#, 44.0, 1], [#, 5.0, 1]]]).
fb_tsv_pred_stats('num-columns', transposon_sequence_set, [9]).
fb_tsv_pred_stats('duplicated-rows', transposon_sequence_set, [1]).
fb_tsv_pred_stats('total-rows', transposon_sequence_set, [749]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [1, 173, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [2, 1, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [3, 32, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [4, 389, int64]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [5, 600, int64]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [6, 1, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [7, 3, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [8, 2, object]).
fb_tsv_pred_stats('unique-values', transposon_sequence_set, [9, 344, object]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [1, 0, [], []]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_set, [2, '.', 749]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [2, 749, [#, '.'], [#, 749]]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [3, 0, [], []]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [4, 0, [], []]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [5, 0, [], []]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_set, [6, '.', 749]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [6, 749, [#, '.'], [#, 749]]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_set, [7, -, 2]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_set, [7, +, 576]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_set, [7, '.', 171]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [7, 749, [#, -, +, '.'], [#, 2, 576, 171]]).
fb_tsv_pred_stats('null-value-count', transposon_sequence_set, [8, '.', 171]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [8, 171, [#, '.'], [#, 171]]).
fb_tsv_pred_stats('missing-values', transposon_sequence_set, [9, 0, [], []]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [1, [#, [#, 'FBte0000176', 11], [#, 'FBte0001207', 12], [#, 'FBte0000021', 12], [#, 'FBte0000109', 12], [#, 'FBte0000037', 13], [#, 'FBte0000675', 14], [#, 'FBte0000322', 17]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [1, [#, [#, 'FBte0000948', 1], [#, 'FBte0001188', 1], [#, 'FBte0000890', 1], [#, 'FBte0000599', 1], [#, 'FBte0000793', 1], [#, 'FBte0000139', 1], [#, 'FBte0000407', 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [2, []]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [2, []]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [3, [#, [#, start_codon, 50], [#, non_LTR_retrotransposon, 53], [#, terminal_inverted_repeat, 56], [#, three_prime_LTR, 66], [#, 'LTR_retrotransposon', 67], [#, five_prime_LTR, 67], [#, 'CDS', 227]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [3, [#, [#, region, 1], [#, dinucleotide_repeat_microsatellite_feature, 1], [#, retrotransposon, 1], [#, pseudogene, 1], [#, tetranucleotide_repeat_microsatellite_feature, 1], [#, transposable_element, 1], [#, 'SINE-like elements', 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [4, [#, [#, 1074, 3], [#, 8236, 3], [#, 172, 3], [#, 2, 3], [#, 5145, 4], [#, 331, 4], [#, 1, 285]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [4, [#, [#, 1919, 1], [#, 282, 1], [#, 946, 1], [#, 7248, 1], [#, 6957, 1], [#, 5953, 1], [#, 3811, 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [5, [#, [#, 1286, 3], [#, 1728, 3], [#, 452, 3], [#, 12, 3], [#, 5034, 3], [#, 9092, 3], [#, 5519, 3]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [5, [#, [#, 600, 1], [#, 119, 1], [#, 424, 1], [#, 1593, 1], [#, 5642, 1], [#, 2624, 1], [#, 2562, 1]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [6, []]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [6, []]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [7, []]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [7, []]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [8, [#, [#, 1.0, 578]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [8, [#, [#, 1.0, 578]]]).
fb_tsv_pred_stats('most-frequent', transposon_sequence_set, [9, [#, [#, 'Ontology_term=SO:0000205', 12], [#, 'Ontology_term=SO:0000551', 24], [#, 'Ontology_term=SO:0000318', 50], [#, 'Ontology_term=SO:0000481', 56], [#, 'Ontology_term=SO:0000316', 62], [#, 'Ontology_term=SO:0000426', 67], [#, 'Ontology_term=SO:0000425', 68]]]).
fb_tsv_pred_stats('less-frequent', transposon_sequence_set, [9, [#, [#, 'ID=FBte0001033;name=Dmel\\mariner2;source=?;type=DNA;subtype=Tc1-Mariner', 1], [#, 'ID=FBte0000773;name=Dana\\Tom;source=Z24451;type=?;subtype=?', 1], [#, 'ID=FBte0000591;name=Dmel\\invader6;source=NT_033778;type=LTR;subtype=Gypsy', 1], [#, 'Ontology_term=SO:0000316;db_xref=FLYBASE:FBgnXXXXXXX;name=Dmel\\gypsy12\\pol;translation=KKCKASLDYISSIPTGPRDPRPFLPMRLLNCLVYGLLDSGASISCIGGGVVQAAMENEKFKSLIGEAATADGNSQRIVGLLKIEVEYGDIKKLLKLYVVPSLKQDLYLGIDFWKLYDLLPANLKIAEILSPEPNQQTVVDQHELCEGDKAKLANVINCFPSFSQEGLGKTNLVSHSIDVGTARPVKQRHFPVSPAVEKAMYAEIDRMLRLGVIGESESAWSSPIVMVTKPGKVRICLECRKVNSFTEMDAYPLPQINGILSRLPRAEYISSLDLKDAYWQVPLDPKSRDKTAFTVPGRPLYQFKVMPFGLCNATSTMSRLMDKVVPAHLRNEVFIYLDDLLIVSSCFESHLNVLRELALQIKRAGLTLNVAKSHFCMRRVRYLGHIIGDGGIRTDPEKVSAITDFPLPKSLKSLRSFMGLCGWYRKFVANFATLSAPLTDLMTTKRKFLLTKEAIEAFSKLKECLSKAPVLCSPDFAKPFAIHCDASKSGVGAVLVQVSEEGDERPIAFVSKKLNKAQRNYTVTEQECLAAIVALKNFRAYVEGLPFKIITDHASLKWLMSNHDLNSRLARWALALQRFKFEIEHRKGSLNVVPDTLSRVNEEIVAAMDLQEDLIVDFDSEFFQSGDYVKLVETVKENTSNFSDLKVESGFLYRKAEHLTGERMHDEYAWKLWVPKELVSKILARAHDSPLAAHGGIHKTLERIRRYYFWPGLVSDVRAYISACEVCKSTKSQNFTLRPPLGKAPESQRFFQRLFIDFLGPYPRSRSGNIGIFIVLDHFSKYVFLKPVKKIDSSVVIKYLEDELFMTFGVPEVILSDNGSQFRARTFQRLIRYGVKHTLTAVHSPQANASERVNRSVIAAIRAYLRLDQKDWDEFLSRICCALRSAVHSSIGTSPYYMVFGQHMITSGSTYSLIRRLNLLDDRSLKFDRHESFEIMRKQAVDQMRNKHNENEKRCNIRSRVVSFVEGQEVYREISSQAVSKPVTTPSLDRRS', 1], [#, 'Ontology_term=SO:0000316;db_xref=FLYBASE:FBgnXXXXXXX;name=Dmel\\gypsy12\\gag;translation=MGLDRSPTRKSPSVSNPVCKLCAAEISTQDLYVTTCHHEFYRECIGNHFKKSEICSRCKLTCRPPAEATERVGRETRSKTKNRRNSRRGSFDISQRCGEKLAVKLKIAATVDGGPSTSASGANANEASSSAVSANAALLAMERRLLATLSEKMADLVQNAITSSMQRIMPTPSPAVVVTASEMSADHPNAYERQYLASPNPVPSPRSASSDLFDRPDKVVHILNGWKIKYSGVGVSVDNFIYRVEAVTRQTLNGNFNLLCRNISVLFEGKANDFFWRYHKFDRVATMGTERFCTALRLQFRQSRDDGDIEELIRNTKQKPNETFDSFYDTVSELVDQLEQPWTANKLVRVLRNNLRPEIRHEILNLDVRTVSELREICKRREAFLADVRRCSSYAKDTPFKREISEVCHESEDEVRSTYEAENDIESFSLVCWNCRIEGHRYQECIAERRVFCYGCGAANTYKPSCRKCSKNFKVGMSKLPVKPKTSNAARNQSTMTDQ', 1], [#, 'ID=FBte0001136;name=Dmel\\gypsy12;source=AE003789;type=LTR;subtype=Gypsy', 1], [#, 'ID=FBte0001041;name=Dmel\\gypsy11;source=?;type=LTR;subtype=Gypsy', 1]]]).

:- forall(fb_tsv_pred_stats(P,A1,Rest),  (G=..[P,A1|Rest],assert(G))).
