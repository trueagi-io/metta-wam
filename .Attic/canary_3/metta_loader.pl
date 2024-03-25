load_metta(Filename):-
 %clear_spaces,
 load_metta('&self',Filename).


load_metta(_Self,Filename):- Filename=='--repl',!,repl.
load_metta(Self,Filename):-
  (\+ atom(Filename); \+ exists_file(Filename)),!,
  with_wild_path(load_metta(Self),Filename),!,loonit_report.
load_metta(Self,RelFilename):-
 atom(RelFilename),
 exists_file(RelFilename),!,
 absolute_file_name(RelFilename,Filename),
 track_load_into_file(Filename,
   include_metta(Self,RelFilename)).

include_metta(Self,Filename):-
  (\+ atom(Filename); \+ exists_file(Filename)),!,
  must_det_ll(with_wild_path(include_metta(Self),Filename)),!.
include_metta(Self,RelFilename):-
  must_det_ll((
     atom(RelFilename),
     exists_file(RelFilename),!,
     absolute_file_name(RelFilename,Filename),
     directory_file_path(Directory, _, Filename),
     assert(metta_file(Self,Filename,Directory)),
     include_metta_directory_file(Self,Directory, Filename))).


% count_lines_up_to_200(Filename, Count).
count_lines_up_to_200(Filename, Count) :-
  open(Filename, read, Stream,[encoding(utf8)]),
  count_lines_in_stream(Stream, 0, Count),
  close(Stream).

% count_lines_in_stream(Stream, CurrentCount, FinalCount).
count_lines_in_stream(Stream, CurrentCount, FinalCount) :-
  ( CurrentCount >= 2000
  -> FinalCount = 2000
  ;  read_line_to_codes(Stream, Codes),
    ( Codes == end_of_file
    -> FinalCount = CurrentCount
    ;  NewCount is CurrentCount + 1,
        count_lines_in_stream(Stream, NewCount, FinalCount)
    )
  ).


include_metta_directory_file_prebuilt(_Self,_Directory, Filename):-
  atom_concat(_,'.metta',Filename),
  atom_concat(Filename,'.qlf',QLFFilename),
  exists_file(QLFFilename),
  ensure_loaded(QLFFilename),!.
include_metta_directory_file_prebuilt(_Self,_Directory, Filename):- just_load_datalog,
  atom_concat(_,'.metta',Filename),
  atom_concat(Filename,'.datalog',QLFFilename),
  exists_file(QLFFilename),
  ensure_loaded(QLFFilename),!.

include_metta_directory_file(Self,Directory, Filename):-
   atom_concat(_,'/__init__.py',Filename), !, 
   file_base_name(Directory,Module),
   self_extend_py(Self,Module,Filename,_).

include_metta_directory_file(Self, _Directory, Filename):- 
   atom_concat(FileBaseName,'.py',Filename),
   file_base_name(FileBaseName,Module),
   !, self_extend_py(Self,Module,Filename,_).

include_metta_directory_file(Self,Directory, Filename):-
  include_metta_directory_file_prebuilt(Self,Directory, Filename),!.
include_metta_directory_file(Self,Directory, Filename):-
  count_lines_up_to_200(Filename, Count), Count > 1980,
  convert_metta_to_qlf(Filename,Load),  
  (exists_file(Load)-> ensure_loaded(Load);
    include_metta_directory_file_prebuilt(Self,Directory, Filename)),!.
include_metta_directory_file(Self,Directory, Filename):-
  setup_call_cleanup(open(Filename,read,In, [encoding(utf8)]),
    with_cwd(Directory, must_det_ll( load_metta_file_stream(Filename,Self,In))),
    close(In)).

convert_metta_to_datalog(Filename,DatalogFile):-
  atom_concat(Filename,'.datalog',DatalogFile),
  setup_call_cleanup(open(Filename,read,Input,[encoding(utf8)]),
    setup_call_cleanup(open(DatalogFile, write, Output,[encoding(utf8)]),
      translate_metta_file_to_datalog_io(Filename,Input,Output),
                    close(Output)),
                  close(Input)),!.


translate_metta_file_to_datalog_io(Filename,Input,Output):-
  must_det_ll((
  %write header
  write(Output,'/* '),write(Output,Filename),writeln(Output,' */'),
  % write the translation time and date
  get_time(Time),stamp_date_time(Time,Date,'UTC'),
  format_time(string(DateStr),'%FT%T%z',Date),
  write(Output,'/* '),write(Output,DateStr),writeln(Output,' */'),
  % make the predicate dynamic/multifile
  writeln(Output,':- dynamic(asserted_metta/4).'),
  writeln(Output,':- multifile(asserted_metta/4).'),
  flag(translated_forms,_,0),
  % translate the file
  once(call((
  repeat,
  (at_end_of_stream(Input)->!;
  ( must_det_ll((
    line_count(Input,Lineno),
    read_line_to_string(Input,Line),
    read_sform(Line,Term))),
    (Term==end_of_file->!;
    (once(((if_t((1 is (Lineno mod 3000)),writeln(Term:Lineno)),
      flag(translated_forms,X,X+1),
      write_metta_datalog_term(Output,Term,Filename,Lineno)))),fail))))))),
  flush_output(Output),
  % teell the user we are done  
  flag(translated_forms,TF,TF),
  writeln('/* Done translating */':TF))).


% write comments
write_metta_datalog_term(Output,'$COMMENT'(Term,_,_),_File,_Lineno):-
  format(Output,"/* ~w */~n",[Term]).
% write executed terms
write_metta_datalog_term(Output,exec(Term),File,Lineno):-
  format(Output,":-eval_H('&self',~q,~q,~q).~n",[Term,File,Lineno]).
% write asserted terms
write_metta_datalog_term(Output,Term,File,Lineno):-
  format(Output,"asserted_metta('&self',~q,~q,~q).~n",[Term,File,Lineno]).

translate_metta_datalog(Input,Output):- translate_metta_datalog('',Input,Output),!.

translate_metta_datalog(_,Input,_):- at_end_of_stream(Input),!.
translate_metta_datalog(Ch,Input,Output):- peek_char(Input,Char),
  translate_metta_datalog(Ch,Input,Output,Char).
  
translate_metta_datalog(_,Input,Output,')'):- !, get_char(Input,_),   
  writeq(Output,']'),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'('):- !,get_char(Input,_),   
  write(Output,Ch),writeq(Output,'['),translate_metta_datalog('',Input,Output).
translate_metta_datalog(Ch,Input,Output,Space):-char_type(Space,space),!,
  get_char(Input,Char),  write(Output,Char),translate_metta_datalog(Ch,Input,Output).
translate_metta_datalog(Ch,Input,Output,';'):-!,read_line_to_string(Input, Comment),
  format(Output, '/* ~w */',[Comment]),translate_metta_datalog(Ch,Input,Output).
translate_metta_datalog(Ch,Input,Output,'"'):-!,read_term(Input,Term,[]), 
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'`'):-!,read_term(Input,Term,[]), 
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'\''):-!,read_term(Input,Term,[]),   
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'$'):-!,
  read_chars_until([type(space),')'],Input,Codes),name(Term,Codes), 
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,Peek):-!,
  read_chars_until([type(space),')'],Peek,Input,Codes),name(Term,Codes), 
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).

read_chars_until(_StopsBefore,Input,[]):- at_end_of_stream(Input),!.
read_chars_until(StopsBefore,Input,Codes):- peek_char(Input,Char),
      read_chars_until(StopsBefore, Char, Input, Codes).

stops_before([type(Type)|StopsBefore],Char):- char_type(Char,Type); stops_before(StopsBefore,Char).
stops_before([Ch|StopsBefore],Char):-  Ch==Char; stops_before(StopsBefore,Char).

read_chars_until(StopsBefore,Char,_, []):- stops_before(StopsBefore,Char),!.
read_chars_until(StopsBefore, '\\', Input, [Code|Codes]):- get_char(Input,Code),
    read_chars_until(StopsBefore, Input, Codes).
read_chars_until(StopsBefore, Char, Input, [Char|Codes]):- get_char(Input,_),
  read_chars_until(StopsBefore, Input, Codes).

  just_load_datalog:-!, fail.
convert_datalog_to_qlf(DatalogFile,DatalogFile):-just_load_datalog,!.
convert_datalog_to_qlf(DatalogFile,QlfFile):-
  sformat(S,'swipl -g "qcompile(~q)" -t halt',[DatalogFile]),
  shell(S,_),
  file_name_extension(Base, _, DatalogFile),
  file_name_extension(Base,'qlf',QlfFile).
      
convert_metta_to_qlf(Filename,QlfFile):- 
  must_det_ll((
  convert_metta_to_datalog(Filename,DatalogFile),
  convert_datalog_to_qlf(DatalogFile,QlfFile))),!.

convert_metta_to_qlf(Filename,_):-
  metta_dir(Dir),    
  sformat(S,'~w/cheap_convert.sh --verbose=1 ~w',[Dir,Filename]),
  shell(S,Ret),!,Ret==0.

accept_line(_Self,end_of_file):-!.
accept_line(Self,I):- normalize_space(string(Str),I),!,accept_line2(Self,Str),!.

accept_line2(_Self,S):- string_concat(";",_,S),!,writeln(S).
accept_line2(Self,S):- string_concat('(',RS,S),string_concat(M,')',RS),!,
  atomic_list_concat([F|LL],' ',M),PL =..[F,Self|LL],assert(PL),!,flag(next_assert,X,X+1),
  if_t((0 is X mod 10_000_000),(writeln(X=PL),statistics)).
accept_line2(Self,S):- fbug(accept_line2(Self,S)),!.


load_metta_file_stream(Filename,Self,In):-  
  if_t((atomic(Filename),exists_file(Filename)), size_file(Filename, Size)),
  if_t(var(Size),is_file_stream_and_size(In, Size)),
  %once((is_file_stream_and_size(In, Size),Size>102400) -> P2 = read_sform2 ;
  P2 = read_metta2, %)  
  with_option(loading_file,Filename,
  %current_exec_file(Filename),
  must_det_ll((must_det_ll((
      set_exec_num(Filename,1),
      load_answer_file(Filename),
      set_exec_num(Filename,0))),
  load_metta_file_stream_fast(Size,P2,Filename,Self,In)))).


load_metta_file_stream_fast(_Size,_P2,Filename,Self,S):- fail, atomic_list_concat([_,_,_|_],'.',Filename),
  \+ option_value(html,true),
  atomic(S),is_stream(S),stream_property(S,input),!,
  repeat,
  read_line_to_string(S,I),
  accept_line(Self,I),
  I==end_of_file,!.


load_metta_file_stream_fast(_Size,P2,Filename,Self,In):-
      repeat,
            current_read_mode(file,Mode),
            must_det_ll(call(P2, In,Expr)), %write_src(read_metta=Expr),nl,
            must_det_ll((((do_metta(file(Filename),Mode,Self,Expr,_O)))->true; pp_m(unknown_do_metta(file(Filename),Mode,Self,Expr)))),
      flush_output,
      at_end_of_stream(In),!.


%read_metta(In,Expr):- current_input(CI), \+ is_same_streams(CI,In), !, read_sform(In,Expr).
read_metta(_,O):- clause(t_l:s_reader_info(O),_,Ref),erase(Ref).
read_metta(I,O):- string(I),normalize_space(string(M),I),!,parse_sexpr_metta1(M,O),!.
read_metta(In,Expr):- current_input(In0),In==In0,!, repl_read(Expr).
read_metta(In,Expr):- read_metta1(In,Expr).

read_metta1(In,Expr):- is_file_stream_and_size(In, Size) , Size>10240,!,read_sform1([],In,Expr).
read_metta1(In,Expr):- read_metta2(In,Expr).

read_metta2(_,O):- clause(t_l:s_reader_info(O),_,Ref),erase(Ref).
read_metta2(In,Expr):- peek_char(In,Char), read_metta2(In,Char,Expr).
read_metta2(In,Char,Expr):- char_type(Char,space),get_char(In,Char),not_compatio(put(Char)),!,read_metta2(In,Expr).
%read_metta2(In,'"',Expr):- read_sform2(In,Expr),!.
%read_metta2(In,'\'',Expr):- read_sform2(In,Expr),!.
read_metta2(In,'!',Expr):- get_char(In,_), !, read_metta2(In,Read1),!,Expr=exec(Read1).
read_metta2(In,';',Expr):- get_char(In,_), !, (maybe_read_pl(In,Expr)-> true ; 
  (read_line_to_string(In,Str),Expr='$COMMENT'(Str,0,0))).
% write_comment(Str),!,read_metta2(In,Expr))),!.
% read_metta2(In,_,Expr):-  maybe_read_pl(In,Expr),!.
read_metta2(In,_,Read1):- parse_sexpr_metta(In,Expr),!,must_det_ll(Expr=Read1).


% Predicate to check if a stream is a file stream and get its size.
is_file_stream_and_size(Stream, Size) :-
    % Check if the stream is associated with a file.
    stream_property(Stream, file_name(FileName)),
    % Check if the file is accessible and get its size.
    exists_file(FileName),
    size_file(FileName, Size).


maybe_read_pl(In,Expr):-
  peek_line(In,Line1), Line1\=='', atom_contains(Line1, '.'),atom_contains(Line1, ':-'),
  notrace(((catch_err((read_term_from_atom(Line1, Term, []), Term\==end_of_file, Expr=call(Term)),_, fail),!,
  read_term(In, Term, [])))).
peek_line(In,Line1):- peek_string(In, 1024, Str), split_string(Str, "\r\n", "\s", [Line1,_|_]),!.
peek_line(In,Line1):- peek_string(In, 4096, Str), split_string(Str, "\r\n", "\s", [Line1,_|_]),!.




%read_line_to_sexpr(Stream,UnTyped),
read_sform(Str,F):- string(Str),open_string(Str,S),!,read_sform(S,F).
read_sform(S,F):-
  read_sform1([],S,F1),
  ( F1\=='!' -> F=F1 ;
    (read_sform1([],S,F2), F = exec(F2))).


%read_sform2(S,F1):- !, read_metta2(S,F1).
read_sform2(S,F1):- read_sform1([],S,F1).

read_sform1(_,_,O):- clause(t_l:s_reader_info(O),_,Ref),erase(Ref).
read_sform1( AltEnd,Str,F):- string(Str),open_string(Str,S),!,read_sform1( AltEnd,S,F).
read_sform1(_AltEnd,S,F):- at_end_of_stream(S),!,F=end_of_file.
read_sform1( AltEnd,S,M):- get_char(S,C),read_sform3(s, AltEnd,C,S,F),
	  untyped_to_metta(F,M).
%read_sform1( AltEnd,S,F):- profile(parse_sexpr_metta(S,F)).

read_sform3(_AoS,_AltEnd,C,_,F):- C == end_of_file,!,F=end_of_file.
read_sform3(       s, AltEnd,C,S,F):- char_type(C,space),!,read_sform1( AltEnd,S,F).
%read_sform3(AoS,_AltEnd,';',S,'$COMMENT'(F,0,0)):- !, read_line_to_string(S,F).
read_sform3(       s, AltEnd,';',S,F):- read_line_to_string(S,_),!,read_sform1( AltEnd,S,F).
read_sform3(       s, AltEnd,'!',S,exec(F)):- !,read_sform1( AltEnd,S,F).

read_sform3(_AoS,_AltEnd,'"',S,Text):- !,must_det_ll(atom_until(S,[],'"',Text)).
read_sform3(_AoS,_AltEnd,'`',S,Text):- !,atom_until(S,[],'`',Text).
read_sform3(_AoS,_AltEnd,'\'',S,Text):- fail, !,atom_until(S,[],'\'',Text).
read_sform3(_AoS,_AltEnd,',',_,','):- fail, !.
read_sform3(     s , AltEnd,C,S,F):- read_sform4( AltEnd,C,S,F),!.
read_sform3(_AoS, AltEnd,P,S,Sym):- peek_char(S,Peek),!,read_symbol_or_number( AltEnd,Peek,S,[P],Expr),into_symbol_or_number(Expr,Sym).

into_symbol_or_number(Expr,Sym):- atom_number(Expr,Sym),!.
into_symbol_or_number(Sym,Sym).

read_sform4(_AltEnd,B,S,Out):-  read_sform5(s,B,S,List,E), c_list(E,List,Out).
c_list(')',List,List).  c_list('}',List,['{...}',List]). c_list(']',List,['[...]',List]).


read_sform5(AoS,'(',S,List,')'):- !,collect_list_until(AoS,S,')',List),!.
read_sform5(AoS,'{',S,List,'}'):- !,collect_list_until(AoS,S,'}',List),!.
read_sform5(AoS,'[',S,List,']'):- !,collect_list_until(AoS,S,']',List),!.


read_symbol_or_number(_AltEnd,Peek,_S,SoFar,Expr):- char_type(Peek,space),!,
    must_det_ll(( atomic_list_concat(SoFar,Expr))).
read_symbol_or_number( AltEnd,Peek,_S,SoFar,Expr):- member(Peek,AltEnd),!,
    must_det_ll(( do_atomic_list_concat(Peek,SoFar,Expr))).
read_symbol_or_number(AltEnd,B,S,SoFar,Expr):- fail,read_sform5(AltEnd,B,S,List,E),
  flatten([List,E],F), append(SoFar,F,NSoFar),!,
   peek_char(S,NPeek), read_symbol_or_number(AltEnd,NPeek,S,NSoFar,Expr).
read_symbol_or_number( AltEnd,_Peek,S,SoFar,Expr):- get_char(S,C),append(SoFar,[C],NSoFar),
    peek_char(S,NPeek), read_symbol_or_number(AltEnd,NPeek,S,NSoFar,Expr).

atom_until(S,SoFar,End,Text):- get_char(S,C),atom_until(S,SoFar,C,End,Text).
atom_until(_,SoFar,C,End,Expr):- C ==End,!,must_det_ll((do_atomic_list_concat(End,SoFar,Expr))).
atom_until(S,SoFar,'\\',End,Expr):-get_char(S,C),!,atom_until2(S,SoFar,C,End,Expr).
atom_until(S,SoFar,C,End,Expr):- atom_until2(S,SoFar,C,End,Expr).
atom_until2(S,SoFar,C,End,Expr):- append(SoFar,[C],NSoFar),get_char(S,NC),
    atom_until(S,NSoFar,NC,End,Expr).

do_atomic_list_concat('"',SoFar,Expr):- \+ string_to_syms,!, atomics_to_string(SoFar,Expr),!.
do_atomic_list_concat(_End,SoFar,Expr):- atomic_list_concat(SoFar,Expr).

collect_list_until(AoS,S,End,List):- get_char(S,C), cont_list(AoS,C,End,S,List).

cont_list(_AoS,End,_End1,_,[]):- End==end_of_file, !.
cont_list(_AoS,End,End1,_,[]):- End==End1, !.
cont_list( AoS,C,End,S,[F|List]):- read_sform3(AoS,[End],C,S,F),!,collect_list_until(AoS,S,End,List).



in2_stream(N1,S1):- integer(N1),!,stream_property(S1,file_no(N1)),!.
in2_stream(N1,S1):- atom(N1),stream_property(S1,alias(N1)),!.
in2_stream(N1,S1):- is_stream(N1),S1=N1,!.
in2_stream(N1,S1):- atom(N1),stream_property(S1,file_name(N1)),!.
is_same_streams(N1,N2):- in2_stream(N1,S1),in2_stream(N2,S2),!,S1==S2.



parse_sexpr_metta(I,O):- string(I),normalize_space(string(M),I),parse_sexpr_metta1(M,O),!.
parse_sexpr_metta(I,O):- parse_sexpr_untyped(I,U),trly(untyped_to_metta,U,O).

parse_sexpr_metta1(M,exec(O)):- string_concat('!',I,M),!,parse_sexpr_metta1(I,O).
parse_sexpr_metta1(M,(O)):- string_concat('+',I,M),!,parse_sexpr_metta1(I,O).
parse_sexpr_metta1(I,O):- parse_sexpr_untyped(I,U),trly(untyped_to_metta,U,O).



write_comment(_):- is_compatio,!.
write_comment(_):- silent_loading,!.
write_comment(Cmt):- connlf,format(';;~w~n',[Cmt]).
do_metta_cmt(_,'$COMMENT'(Cmt,_,_)):- write_comment(Cmt),!.
do_metta_cmt(_,'$STRING'(Cmt)):- write_comment(Cmt),!.
do_metta_cmt(Self,[Cmt]):- !, do_metta_cmt(Self, Cmt),!.


mlog_sym('@').

%untyped_to_metta(I,exec(O)):- compound(I),I=exec(M),!,untyped_to_metta(M,O).
untyped_to_metta(I,O):-
 must_det_ll((
  trly(mfix_vars1,I,M),
  trly(cons_to_c,M,OM),
  trly(cons_to_l,OM,O))).


trly(P2,A,B):- once(call(P2,A,M)),A\=@=M,!,trly(P2,M,B).
trly(_,A,A).

mfix_vars1(I,O):- var(I),!,I=O.
mfix_vars1('$_','$VAR'('_')).
mfix_vars1('$','$VAR'('__')).
mfix_vars1(I,'$VAR'(O)):- atom(I),atom_concat('$',N,I),atom_concat('_',N,O).
%mfix_vars1('$t','$VAR'('T')):-!.
%mfix_vars1('$T','$VAR'('T')):-!.
%mfix_vars1(I,O):- I=='T',!,O='True'.
%mfix_vars1(I,O):- I=='F',!,O='False'.
%mfix_vars1(I,O):- is_i_nil(I),!,O=[].
mfix_vars1(I,O):- I=='true',!,O='True'.
mfix_vars1(I,O):- I=='false',!,O='False'.
mfix_vars1('$STRING'(I),O):- \+ string_to_syms, mfix_vars1(I,OO),text_to_string(OO,O),!.
%mfix_vars1('$STRING'(I),O):- \+ string_to_syms, text_to_string(I,O),!.
mfix_vars1('$STRING'(I),O):- !, mfix_vars1(I,M),atom_chars(O,M),!.
%mfix_vars1('$STRING'(I),O):- !, mfix_vars1(I,M),name(O,M),!.
mfix_vars1([H|T],O):-   H=='[', is_list(T), last(T,L),L==']',append(List,[L],T), !, O = ['[...]',List].
mfix_vars1([H|T],O):-   H=='{', is_list(T), last(T,L),L=='}',append(List,[L],T), !, O = ['{...}',List].
mfix_vars1([H|T],O):-   is_list(T), last(T,L),L=='}',append(List,[L],T),
   append(Left,['{'|R],List),append([H|Left],[['{}',R]],NewList),mfix_vars1(NewList,O).
mfix_vars1('$OBJ'(claz_bracket_vector,List),O):- is_list(List),!, O = ['[...]',List].
mfix_vars1(I,O):-  I = ['[', X, ']'], nonvar(X), !, O = ['[...]',X].
mfix_vars1(I,O):-  I = ['{', X, '}'], nonvar(X), !, O = ['{...}',X].
mfix_vars1('$OBJ'(claz_bracket_vector,List),Res):- is_list(List),!, append(['['|List],[']'],Res),!.
mfix_vars1(I,O):- I==[Quote, S], Quote==quote,S==s,!, O=is.
mfix_vars1([K,H|T],Cmpd):- atom(K),mlog_sym(K),is_list(T),mfix_vars1([H|T],[HH|TT]),atom(HH),is_list(TT),!,
  compound_name_arguments(Cmpd,HH,TT).
%mfix_vars1([H|T],[HH|TT]):- !, mfix_vars1(H,HH),mfix_vars1(T,TT).
mfix_vars1(List,ListO):- is_list(List),!,maplist(mfix_vars1,List,ListO).
mfix_vars1(I,O):- string(I),string_to_syms,!,atom_string(O,I).

mfix_vars1(I,O):- compound(I),!,compound_name_arguments(I,F,II),F\=='$VAR',maplist(mfix_vars1,II,OO),!,compound_name_arguments(O,F,OO).
mfix_vars1(I,O):- \+ atom(I),!,I=O.
mfix_vars1(I,I).

no_cons_reduce.
svar_fixvarname_dont_capitalize(O,O):-!.
svar_fixvarname_dont_capitalize(M,O):- svar_fixvarname(M,O),!.


%dvar_name(t,'T'):- !.
dvar_name(N,O):-atom_concat('_',_,N),!,O=N.
dvar_name(N,O):- integer(N),atom_concat('_',N,O).
dvar_name(N,O):- atom(N),atom_number(N,Num),dvar_name(Num,O),!.
dvar_name(N,O):- \+ atom(N),!,format(atom(A),'~w',[N]),dvar_name(A,O).
dvar_name(N,O):- !, format(atom(A),'_~w',[N]),dvar_name(A,O).
%dvar_name(  '',''):-!. % "$"
%dvar_name('_','__'):-!. % "$_"
dvar_name(N,O):-atom_concat('_',_,N),!,atom_concat('_',N,O).
dvar_name(N,O):- svar_fixvarname_dont_capitalize(N,O),!.
dvar_name(N,O):- must_det_ll((atom_chars(N,Lst),maplist(c2vn,Lst,NList),atomic_list_concat(NList,S),svar_fixvarname_dont_capitalize(S,O))),!.
c2vn(A,A):- char_type(A,prolog_identifier_continue),!.
c2vn(A,A):- char_type(A,prolog_var_start),!.
c2vn(A,AA):- char_code(A,C),atomic_list_concat(['_C',C,'_'],AA).

cons_to_l(I,I):- no_cons_reduce,!.
cons_to_l(I,O):- var(I),!,O=I.
cons_to_l(I,O):- is_i_nil(I),!,O=[].
cons_to_l(I,O):- I=='nil',!,O=[].
cons_to_l(C,O):- \+ compound(C),!,O=C.
cons_to_l([Cons,H,T|List],[HH|TT]):- List==[], atom(Cons),is_cons_f(Cons), t_is_ttable(T), cons_to_l(H,HH),!,cons_to_l(T,TT).
cons_to_l(List,ListO):- is_list(List),!,maplist(cons_to_l,List,ListO).
cons_to_l(I,I).

cons_to_c(I,I):- no_cons_reduce,!.
cons_to_c(I,O):- var(I),!,O=I.
cons_to_c(I,O):- is_i_nil(I),!,O=[].
cons_to_c(I,O):- I=='nil',!,O=[].
cons_to_c(C,O):- \+ compound(C),!,O=C.
cons_to_c([Cons,H,T|List],[HH|TT]):- List==[], atom(Cons),is_cons_f(Cons), t_is_ttable(T), cons_to_c(H,HH),!,cons_to_c(T,TT).
cons_to_c(I,O):- \+ is_list(I), compound_name_arguments(I,F,II),maplist(cons_to_c,II,OO),!,compound_name_arguments(O,F,OO).
cons_to_c(I,I).



t_is_ttable(T):- var(T),!.
t_is_ttable(T):- is_i_nil(T),!.
t_is_ttable(T):- is_ftVar(T),!.
t_is_ttable([F|Args]):- F=='Cons',!,is_list(Args).
t_is_ttable([_|Args]):- !, \+ is_list(Args).
t_is_ttable(_).

is_cons_f(Cons):- is_cf_nil(Cons,_).
is_cf_nil('Cons','NNNil').
%is_cf_nil('::','nil').

is_i_nil(I):-
  is_cf_nil('Cons',Nil), I == Nil.

subst_vars(TermWDV, NewTerm):-
   subst_vars(TermWDV, NewTerm, NamedVarsList),
   maybe_set_var_names(NamedVarsList).

subst_vars(TermWDV, NewTerm, NamedVarsList) :-
    subst_vars(TermWDV, NewTerm, [], NamedVarsList).

subst_vars(Term, Term, NamedVarsList, NamedVarsList) :- var(Term), !.
subst_vars([], [], NamedVarsList, NamedVarsList):- !.
subst_vars([TermWDV|RestWDV], [Term|Rest], Acc, NamedVarsList) :- !,
    subst_vars(TermWDV, Term, Acc, IntermediateNamedVarsList),
    subst_vars(RestWDV, Rest, IntermediateNamedVarsList, NamedVarsList).
subst_vars('$VAR'('_'), _, NamedVarsList, NamedVarsList) :- !.
subst_vars('$VAR'(VName), Var, Acc, NamedVarsList) :- nonvar(VName), svar_fixvarname_dont_capitalize(VName,Name), !,
    (memberchk(Name=Var, Acc) -> NamedVarsList = Acc ; ( !, Var = _, NamedVarsList = [Name=Var|Acc])).
subst_vars(Term, Var, Acc, NamedVarsList) :- atom(Term),atom_concat('$',DName,Term),
   dvar_name(DName,Name),!,subst_vars('$VAR'(Name), Var, Acc, NamedVarsList).

subst_vars(TermWDV, NewTerm, Acc, NamedVarsList) :-
    compound(TermWDV), !,
    compound_name_arguments(TermWDV, Functor, ArgsWDV),
    subst_vars(ArgsWDV, Args, Acc, NamedVarsList),
    compound_name_arguments(NewTerm, Functor, Args).
subst_vars(Term, Term, NamedVarsList, NamedVarsList).


connlf:- check_silent_loading, not_compat_io((format('~N'))).
connl:- check_silent_loading,not_compat_io((nl)).
% check_silent_loading:- silent_loading,!,trace,break.
check_silent_loading.
silent_loading:- option_value('load','silent'), !.
silent_loading:- is_converting,!.
silent_loading:- option_value('html','True'), !,fail.
silent_loading:- option_value('trace-on-load','False'), !.




uncompound(OBO,Src):- \+ compound(OBO),!, Src = OBO.
uncompound('$VAR'(OBO),'$VAR'(OBO)):-!.
uncompound(IsList,Src):- is_list(IsList),!,maplist(uncompound,IsList,Src).
uncompound([Is|NotList],[SrcH|SrcT]):-!, uncompound(Is,SrcH),uncompound(NotList,SrcT).
uncompound(Compound,Src):- compound_name_arguments(Compound,Name,Args),maplist(uncompound,[Name|Args],Src).

assert_to_metta(_):- reached_file_max,!.
assert_to_metta(OBO):-
    must_det_ll((OBO=..[Fn|DataLL],
    maplist(better_arg,DataLL,DataL),
    into_datum(Fn, DataL, Data),
    functor(Data,Fn,A),decl_fb_pred(Fn,A),
    real_assert(Data),!,
   incr_file_count(_))).

assert_to_metta(OBO):-
 ignore(( A>=2,A<700,
  OBO=..[Fn|Cols],
 must_det_ll((
  make_assertion4(Fn,Cols,Data,OldData),
  functor(Data,FF,AA),
  decl_fb_pred(FF,AA),
  ((fail,call(Data))->true;(
   must_det_ll((
     real_assert(Data),
     incr_file_count(_),
     ignore((((should_show_data(X),
       ignore((fail,OldData\==Data,write('; oldData '),write_src(OldData),format('  ; ~w ~n',[X]))),
       write_src(Data),format('  ; ~w ~n',[X]))))),
     ignore((
       fail, option_value(output_stream,OutputStream),
       is_stream(OutputStream),
       should_show_data(X1),X1<1000,must_det_ll((display(OutputStream,Data),writeln(OutputStream,'.'))))))))))))),!.

assert_MeTTa(OBO):- !, assert_to_metta(OBO).
%assert_MeTTa(OBO):- !, assert_to_metta(OBO),!,heartbeat.
/*
assert_MeTTa(Data):- !, heartbeat, functor(Data,F,A), A>=2,
   decl_fb_pred(F,A),
   incr_file_count(_),
   ignore((((should_show_data(X),
       write(newData(X)),write(=),write_src(Data))))),
   assert(Data),!.
*/


%:- dynamic((metta_type/3,metta_defn/3,get_metta_atom/2)).

