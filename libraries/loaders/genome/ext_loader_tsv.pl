


fix_header_names(Fn,Header,GNames):-
   maplist(fix_header_names(Header,Fn),Header,ArgTypes),
   include( \=(''),ArgTypes,GNames).


%fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_','_id','_ID']),symbol_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(_FL,_Fn,ID,Out):- number(ID),!,Out=ID.
fix_header_names(FL,Fn,listOf(ID),listOf(Out)):- fix_header_names(FL,Fn,ID,Out),!.
fix_header_names(FL,Fn,listOf(ID,Sep),listOf(Out,Sep)):- fix_header_names(FL,Fn,ID,Out),!.
fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_']),symbol_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_']),symbol_concat(RF,MID,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,Out):- member(RF,['__',' ']),symbolic_list_concat(MIDL,RF,ID),MIDL\=[_],symbolic_list_concat(MIDL,'_',MID),!,
   fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,listOf(AOut)):- member(RF,['(es)','(s)','ids']),symbolic_list_concat([Left,Right],RF,ID),symbolic_list_concat([Left,Right],'_',MID),!,
   fix_header_names(FL,Fn,MID,AOut),!. % symbol_concat('ListOf_',AOut,Out),!.
fix_header_names(FL,Fn,TT,listOf(AOut)):-
   member(IDs=ID,['IDs'='ID']),
   symbol_concat(Type,IDs,TT),
   symbol_concat(Type,ID,MID),
   fix_header_names(FL,Fn,MID,AOut),!.
fix_header_names(FL,Fn,ID,listOf(AOut)):- member(RFS=RF,['_IDs'='_ID','IDs'='ID']),
   symbolic_list_concat([Left,Right],RFS,ID),
   symbolic_list_concat([Left,Right],RF,MID),!,
   fix_header_names(FL,Fn,MID,AOut),!. % symbol_concat('ListOf_',AOut,Out),!.


fix_header_names(_,_,Name,Name):- \+ too_generic(Name),!.
fix_header_names(_,_,Name,Name):- symbolic_list_concat([_,_|_],'_',Name),!.
%fix_header_names(_,Fn,ID,Out):- symbolic_list_concat([Fn,ID],'_column_',Out).
%fix_header_names(FieldList,Fn,ID,Out):- symbolic_list_concat([Fn,ID],'_',Out), \+ member(Out,FieldList).
fix_header_names(_,_,Name,Name).


pmt :-flybase_tables(FBT),for_all(member(T,FBT), ( '\\+'(flybase_cols(T,_)) -> format('~N~q.~n',[get_fbt(T)]);true)).
use_flybase_cols(Table,Columns):-
 must_det_ll((
  maplist(fix_header_names(Columns,Table),Columns,ArgTypes),
  assert(flybase_col_names(Table,Columns,ArgTypes)),
  do_arity_2_names(Table,ArgTypes))).

do_arity_2_names(Table,[ID|ArgTypes]):-
  must_det_ll((
  symbol_concat('data_',Table,F),
  length([ID|ArgTypes],Arity),
  length(Args,Arity),
  DataCall=..[F|Args],
  do_arity_2_names_dc(Table,DataCall,2,ArgTypes))).

do_arity_2_names_dc(Table,DataCall,N,[Nth|ArgTypes]):-
  do_arity_2_names_dc1(Table,DataCall,N,Nth),!,
  N2 is N+1, do_arity_2_names_dc(Table,DataCall,N2,ArgTypes).
do_arity_2_names_dc(_Table,_DataCall,_N,[]).

do_arity_2_names_dc1(Table,DataCall,N,Nth):-
 must_det_ll((
  arg(1,DataCall,Arg1Data),
  arg(N,DataCall,Arg2Data),
  make_arity_2_name(Table,Nth,Arity2),
  Arg1=..[Table,Arg1Data],
  clip_id(Nth,NthNoID),
  (Nth==NthNoID -> Arg2=Arg2Data ;  Arg2 =..[NthNoID,Arg2Data]),
  Arity2Call=..[Arity2,Arg1,Arg2],
  fbug((Arity2Call:-DataCall)),
  fb_assert((Arity2Call:-DataCall)))).

make_arity_2_name(Table,Nth,Arity2):-
  clip_id(Nth,NthNoID),
  (symbol_concat(Table,_,Nth)
    -> Arity2 = Nth
    ; symbolic_list_concat([Table,NthNoID],'_',Arity2)).


clip_id(Nth,ID):- (symbol_concat(ID,'_id',Nth)->true;Nth=ID),!.




setup_flybase_cols:-
 for_all(flybase_cols(Table,Columns),
  use_flybase_cols(Table,Columns)).

%:- load_flybase("das_precomputed/allele_genetic_interactions_fb_2022_06.tsv").





:- use_module(library(csv)).

write_flybase_data(_ArgTypes,_Fn,[]):-!.
write_flybase_data(_ArgTypes,_Fn,['']):-!.
write_flybase_data(_ArgTypes,_Fn,[_]):-!.
write_flybase_data(_ArgTypes,Fn,DataL0):-
 maplist(fast_column,DataL0,DataL), !, Data=..[Fn|DataL], assert_MeTTa(Data).
%write_flybase_data(_ArgTypes,Fn,DataL):- into_datum(Fn,DataL,Data), assert_OBO(Data).


/*

assert_MeTTa(Data):- Data=..[Fn|DataL],assert_MeTTa(Fn,DataL),!.

assert_MeTTa(Fn,DataL0):-
  make_assertion(Fn,DataL0,Data,OldData),
  ignore((
    heartbeat,
    functor(Data,F,A), A>=2,
   decl_fb_pred(F,A),
    flag(loaded_from_file_count,X,X+1),
    flag(total_loaded_symbols,TA,TA+1),
    assert(Data),
    ignore((((has_list(_ArgTypes)->(X<23,X>20); (X<13,X>10)); (X>0,(0 is X rem 1_000_000),fb_stats)),nl,nl,fbug(X=Data),ignore((OldData\==DataL0,fbug(oldData=OldData))))),
    ignore((fail,catch_ignore(ignore((X<1000,must_det_ll((write_canonical(OutputStream,Data),writeln(OutputStream,'.')))))))))),!.
 */

make_assertion(Fn, Cols, NewData, OldData):- !, make_assertion4(Fn, Cols, NewData, OldData).

make_assertion(Fn,DataL0,Data,DataL0):-
 must_det_ll((
    into_datum(Fn,DataL0,Data0),
    Data0=..[F|Args],
    Args=DataL,
    Data=..[F|DataL])).

make_assertion(ArgTypes,Fn,DataL0,Data,DataL0):-
 must_det_ll((
    into_datum(Fn,DataL0,Data0),
    Data0=..[F|Args],
    skip(if_t(var(ArgTypes),must_det_ll((once((length(Args,Len),length(ArgTypes,Len),once((table_columns(Fn,ArgTypes);table_columns(F,ArgTypes))))))))),
    fix_list_args(Fn,ArgTypes,Args,DataL),
    Data=..[F|DataL])).


% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743
%reprefix(['GO_','GO--','FBgn','BiologicalProcess:GO:'],'GO:').
reprefix(['GO_','GO--','BiologicalProcess:GO:'],'GO:').
reprefix(['flybase:','FLYBASE:','comment:'],'').
reprefix(['FBpp:'],'FBpp').
reprefix(['FBgn:'],'FBgn').
reprefix(['FB:FB'],'FB').
%./KBs/SUMO-OBO/gene-merged-SUMO.kif
%#
%FBbt_00051628=

as_list(A,New):- is_list(A),!,A=New.
as_list(N,[N]):- \+ symbol(N), \+ string(N),!.
%as_list(A,New):- var(A),!,New = [A].
as_list('-',[]). as_list("-",[]). as_list('',[]).
as_list("",[]). as_list(' ',[]). as_list(" ",[]).
%as_list(N,[N]):- !.
as_list(_,S,O):- as_list(S,O),!.
as_list(SepL,A,List):-  member(Sep,SepL),catch_ignore(symbolic_list_concat(List,Sep,A)),List\=[_],!.
as_list([],A,ListO):-  member(Sep,['|',',',';']),catch_ignore(symbolic_list_concat(List,Sep,A)),List\=[_],!,maplist(fix_concept,List,ListO).
as_list(_Sep,A,[A]).
has_list(Header):- is_list(Header),member(listOf(_),Header).

% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743

% =======================================
% Fix Concept1
% =======================================

fix_concept1(A,L):- as_list(['|'],A,L),(L\=@=[A],A\=@=L).
fix_concept1(A,N):-  symbol_number(A,N),!.
%fix_concept1(A,AO):- reprefix(List,To),member(E,List),symbol_concat(E,AM,A),symbol_concat(To,AM,AO).
%fix_concept1(A,AO):- symbol_concat('FB',_,A),symbolic_list_concat([Type,Number],':',A),!,symbol_concat(Type,Number,AO).
fix_concept1(A,AO):- symbol_concat('"',Mid,A),symbol_concat(AS,'"',Mid),symbol_string(AS,AO).
fix_concept1(A,AO):- symbol_concat(AO,'(gene name)',A),AO\==''.

fix_concept1(A,N):- symbol(A),!,N=A.
%fix_concept(S,A):- number_string(A,S),!.


% =======================================
% Fix Concept
% =======================================

fix_concept(A,New):- is_list(A),!,maplist(fix_concept,A,L),!,New=L.
fix_concept(A,New):- \+ symbol(A), !,New=A.
fix_concept(S,O):- once(fix_concept1(S,M)),S\=@=M,!,fix_concept(M,O).
fix_concept(A,New):- =(A,New),!.


fix_columns_nth('genome-cyto-seq', 1).
fix_columns_nth('genome-cyto-seq', 2).
fix_columns_nth('genome-cyto-seq', 3).
fix_columns_nth(allele_genetic_interactions, 3).
fix_columns_nth(dmel_human_orthologs_disease, 6).
fix_columns_nth(dmel_human_orthologs_disease, 7).
fix_columns_nth(dmel_paralogs, 10).
fix_columns_nth(dmel_paralogs, 11).
fix_columns_nth(dmel_paralogs, 5).
fix_columns_nth(dmel_paralogs, 8).
fix_columns_nth(entity_publication, 4).
fix_columns_nth(fbgn_NAseq_Uniprot, 7).
fix_columns_nth(fbrf_pmid_pmcid_doi, 2).
fix_columns_nth(gene_genetic_interactions, 1).
fix_columns_nth(gene_genetic_interactions, 2).
fix_columns_nth(gene_genetic_interactions, 3).
fix_columns_nth(gene_genetic_interactions, 4).
fix_columns_nth(gene_groups_HGNC, 4).
fix_columns_nth(gene_rpkm_matrix, _).
fix_columns_nth(gene_rpkm_report, 10).
fix_columns_nth(gene_rpkm_report, 11).
fix_columns_nth(gene_rpkm_report, 8).
fix_columns_nth(gene_rpkm_report, 9).
fix_columns_nth(gene_snapshots, 4).
fix_columns_nth(genotype_phenotype, 5).
fix_columns_nth(genotype_phenotype, 6).
fix_columns_nth(gp_information, 9).
fix_columns_nth(insertion_mapping, 5).
fix_columns_nth(insertion_mapping, 6).
fix_columns_nth(physical_interactions_mitab, _).
fix_columns_nth(pmid_fbgn_uniprot, 2).
fix_columns_nth(stocks, 7).
fix_columns_nth(synonym, 5).
fix_columns_nth(synonym, 6).
fix_columns_nth(transposon_sequence_set, 4).
fix_columns_nth(transposon_sequence_set, 5).
fix_columns_nth(transposon_sequence_set, 8).



:- discontiguous column_description/4.
:- discontiguous primary_column/2.
:- discontiguous column_names/2.
:- discontiguous file_location/2.



% 466_896_429
% Descriptions for allele_genetic_interactions columns
% Descriptions for genotype_phenotype_data columns
% For the file allele_genetic_interactions_*.tsv
% For the file genotype_phenotype_data_*.tsv


%file_to_sep(_File,9).
file_to_sep(csv,',').
file_to_sep(tsv,'\t').
file_to_sep(metta_x,'\t').
file_to_sep(File,Sep):- file_name_extension(_,Ext,File),clause(file_to_sep(Ext,Sep),true),!.
file_to_sep(_,'\t').




load_flybase(Sep,File,Stream,Fn):-
 must_det_ll((
  %ignore(swi_only(format(":- ~q.\n",[encoding(utf8)]))),
  symbolic_list_concat([data,Fn],'_',Fn0),
  data_pred(Fn0,Fn),
  load_flybase_sv(Sep,File,Stream,Fn))).

% Sep,File,Stream,OutputStream,Fn
load_flybase_sv(Sep,File,Stream,Fn):- at_end_of_stream(Stream),!,
  once(load_fb_data(_ArgTypes,File,Stream,Fn,Sep,end_of_file)).

load_flybase_sv(Sep,File,Stream,Fn):-
 must_det_ll((
  flag(loaded_from_file_count,_,0),
  notrace(ignore(attempt_header_names(Sep,File,Stream,Fn,ArgTypes))),
  ArgTypes=[N|_],
  ignore(N=1),
  if_t(is_list(ArgTypes),ignore((length(ArgTypes,A),decl_fb_pred(Fn,A)))),
  read_line_to_chars(Stream, Chars0), wdmsg(Chars0), once(load_flybase_chars(ArgTypes,File,Stream,Fn,Sep,Chars0)),!,
  %\+ reached_file_max,
  % \+ done_reading(File),
  %\+ at_end_of_stream(Stream),
  time((repeat,
        read_line_to_chars(Stream, Chars), wdmsg(Chars),
        once(load_flybase_chars(ArgTypes,File,Stream,Fn,Sep,Chars)),
       once(reached_file_max;
         % done_reading(File);
         at_end_of_stream(Stream)),!,
      once(load_fb_data(ArgTypes,File,Stream,Fn,Sep,end_of_file)))),
      loaded_from_file_count(X),!,
      metta_stats(Fn),
      pl_stats(File,X))),!.



attempt_header_names(Sep,File,_Stream,Fn,NArgTypes):-
  ((
    ignore(once((table_columns(File,Header);table_columns(Fn,Header)))),
    if_t(nonvar(Header),fix_header_names(Fn,Header,ArgTypes)),
    forall((table_columns(File,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(File,ColInfo))),
    forall((table_columns(Fn,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(Fn,ColInfo))),
    ((primary_column(Fn,Name),nth1(N,ArgTypes,Name))->NArgTypes=[N|ArgTypes];NArgTypes=[1|ArgTypes]),
    if_t(is_list(ArgTypes),add_table_n_types(Fn,1,ArgTypes)))),
    ground(NArgTypes),
    wdmsg(fix_header_names(Sep,File,Fn,Header,ArgTypes,N,NArgTypes,A)), nonvar(A), A>0,!.

attempt_header_names(Sep,File,Stream,Fn,NArgTypes):- fail,
  read_line_to_chars(Stream, Chars),
  once(load_flybase_chars(NArgTypes,File,Stream,Fn,Sep,Chars)),
  wdmsg(load_flybase_chars(NArgTypes,File,Stream,Fn,Sep,Chars)).


%save_conversion_data(ArgTypes,Fn,OutputStream,Data):- maplist(write_flybase_data(ArgTypes,ArgTypes,Fn,OutputStream),Data).

is_really_header_row([H|_],_Names):- symbol_concat('',_,H),!.

%read_csv_stream(Sep,CharsStream,Header):- read_string(CharsStream, "\n", "\r\t ",_,)
read_csv_stream(Sep,CharsStream,Header):- %  \+ option_value(full_canon,[]),!,
  read_line_to_string(CharsStream,Chars),
  (Chars == end_of_file -> Header= Chars ; symbolic_list_concat(Header, Sep, Chars)).
read_csv_stream(Sep,CharsStream,Header):- \+ option_value(full_canon,[]),!, read_line_to_string(CharsStream,Chars),
  (Chars == end_of_file -> Header= Chars ; split_string(Chars, Sep, "\s\t\n", Header)).
read_csv_stream(Sep,CharsStream,Header):-
  name(Sep,[SepCode]),
  csv_options(CompiledHeaderOptions,[separator(SepCode)]),
  csv_read_row(CharsStream, HeaderRow, CompiledHeaderOptions),
  HeaderRow=..[_|Header],!.

%read_csv(Sep,Chars,Header):- \+ option_value(full_canon,[]),!, split_string(Chars, Sep, "\s\t\n", Header).
read_csv(Sep,Chars,Header):-
  open_string(Chars,CharsStream),read_csv_stream(Sep,CharsStream,Header).


attempt_header_row(Sep,Chars,Fn,Header,ArgTypes):-
  read_csv(Sep,Chars,Header),
  fix_header_names(Fn,Header,ArgTypes),!.

:- dynamic(t_h_n/3).

load_flybase_chars(ArgTypes,File,_Stream,_Fn,Sep,Chars):-
  ( \+ member(Sep,Chars); (['#','#',' '|_]=Chars) ;  (ground(ArgTypes),['#'|_]=Chars)),
  %writeln(comment(Sep)=Chars),!,
  (format("~n ; ~s",[Chars])),
  ignore((loaded_from_file_count(X),X>2000,!,assert(done_reading(File)))).

load_flybase_chars([N|ArgTypes],File,Stream,Fn,Sep,Chars):-
  var(ArgTypes),member(Sep,Chars),['#'|_]=Chars,
  (format("~n ; Maybe Header: ~s",[Chars])),
  attempt_header_row(Sep,Chars,Fn,Header,ArgTypes),
  is_really_header_row(Header,ArgTypes),
  (fbug(t_h_n(Fn,Header,ArgTypes)),fb_assert(t_h_n(Fn,Header,ArgTypes))),!,
  load_fb_data([N|ArgTypes],File,Stream,Fn,Sep,is_swipl).

load_flybase_chars([N|ArgTypes],File,Stream,Fn,Sep,Chars):- is_swipl,
  attempt_header_row(Sep,Chars,Fn,Header,_),
  write_flybase_data([N|ArgTypes],Fn,Header),!,
  load_fb_data([N|ArgTypes],File,Stream,Fn,Sep,is_swipl).


load_fb_data(_ArgTypes,File,_Stream,_Fn,_Sep,Data):-
  (Data == end_of_file;done_reading(File)),!.

load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl):-  % \+ option_value(full_canon,[]), !,
  (option_value(max_per_file,Max)->true;Max=inf),
  fbug(load_fb_data(ArgTypes,File,Max,Fn,Sep)),
  add_table_n_types(Fn,1,ArgTypes),!,% trace,
   repeat,
     once(read_csv_stream(Sep,Stream,Data)),
     loaded_from_file_count(X),
      (((Data== end_of_file);reached_file_max;(X>Max)) -> assert(done_reading(File)) ;
       (once(write_flybase_data(ArgTypes,Fn,Data)),fail)),!.

load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl):- !,
   name(Sep,[SepCode]),
  csv_options(CompiledOptions,[separator(SepCode)]),
  (option_value(max_per_file,Max)->true;Max=inf),
  fbug(load_fb_data(ArgTypes,File,Max,Fn,Sep)),
  add_table_n_types(Fn,1,ArgTypes),!,
   repeat,
     once((csv_read_row(Stream, RData, CompiledOptions))),
     loaded_from_file_count(X),
      (((RData== end_of_file);reached_file_max;(X>Max)) -> assert(done_reading(File)) ;
       (RData =..[_|Data],
       once(write_flybase_data(ArgTypes,Fn,Data)),fail)),!.

% recursion depth 16 million rows
load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl):-
  name(Sep,[SepCode]),
  csv_options(CompiledOptions,[strip(true),convert(true),separator(SepCode)]),
   (option_value(max_per_file,Max)->true;Max=inf),
     once((csv_read_row(Stream, RData, CompiledOptions))),
     loaded_from_file_count(X),
      (((RData== end_of_file);(X>Max)) -> assert(done_reading(File)) ;
       (RData =..[_|Data], once(write_flybase_data(ArgTypes,Fn,Data)),
         load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl))),!.




