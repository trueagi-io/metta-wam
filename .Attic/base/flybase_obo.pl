% ===========================================
% BEGIN OBO Loader
%   - Douglas R. Miles 2023
% ===========================================

% requires:  assert_MeTTa/1, track_load_into_file/2

:- use_module(library(logicmoo_utils)).
:- ensure_loaded(flybase_main).

assert_OBO(P,X,Y):- assert_OBO(ontology_info(P,X,Y)).

% Main entry point
load_obo(Filename) :- \+ atomic(Filename),
  absolute_file_name(Filename,X,[read(exists),extension(['']),file_type(directory),
     file_errors(fail),solutions(first)]), !, load_obo(X).
load_obo(Filename) :- \+ atomic(Filename), !,
  absolute_file_name(Filename,X,[read(exists),extension(['']), file_errors(fail),solutions(first)]), !, load_obo(X).
load_obo(Filename) :-
  atomic(Filename), \+ exists_file(Filename), expand_file_name(Filename,List),
  List\==[], List\==[Filename],
  maplist(load_obo,List).
load_obo(Directory) :-
  atomic(Directory), exists_directory(Directory),
  directory_file_path(Directory, "*.obo", Filename),
  expand_file_name(Filename,List),!,maplist(load_obo,List).
load_obo(Filename) :- fail,
  process_obo_file(Filename),!.
load_obo(Filename) :-
  process_obo_direct(Filename),!.


process_obo_file(File):- atom_concat(File,'.metta_x',MXFile),process_obo_file(File,MXFile).
process_obo_file(_File,MXFile):- exists_file(MXFile),!,process_metta_x_file(MXFile).
process_obo_file(File, MXFile):- exists_file(File),!,
          setup_call_cleanup(
             open(MXFile,write,Strm,[]),
             setup_call_cleanup(
                    set_stream(Strm,alias(metta_x_output)),
                    with_option(make_metta_x,'False',
                       process_obo_direct(File)),
                    set_stream(current_output,alias(metta_x_output))),
             close(Strm)),
          remove_duplicates(MXFile),
          process_metta_x_file(MXFile).
process_obo_file(File, MXFile):- throw(process_obo_file(File, MXFile)).



% remove_duplicates(+InputFile) - removes duplicate lines from a file in place.
remove_duplicates(InputFile) :-
    % Create a temporary output file
    format(atom(OutputFile), '~w.tmp', [InputFile]),
    % Build the command to remove duplicates and output to the temporary file
    format(atom(Command), 'sort ~w | uniq > ~w', [InputFile, OutputFile]),
    % Execute the command
    shell(Command, ExitStatus),
    % Check the exit status
    ExitStatus =:= 0,
    % Move the temporary output file to the original input file
    format(atom(Command2), 'mv ~w ~w', [OutputFile, InputFile]),
    shell(Command2, ExitStatus2),
    ExitStatus2 =:= 0,!.

% rename_file(+Source, +Destination) - renames or moves a file.
%rename_file(Source, Destination) :-
 %   rename(Source, Destination).

process_obo_direct(Filename):-
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
 symbol_concat(Id,'.metta',OutputFile),
 nop(tell(OutputFile)),
 track_load_into_file(Filename,
 must_det_ll((
    Type = 'OntologyFile',
    assert_OBO(id_type,Id,Type),
    nb_setval(obo_id,Id),nb_setval(obo_type,Type),
    assert_OBO('pathname',Id,Filename),!,
    assert_OBO('basename',Id,BaseName),!,
    assert_OBO('directory',Id,Directory),!,
    setup_call_cleanup(open(Filename, read, Stream, [encoding(utf8)]),
      process_obo_stream_repeat(Stream),
      close(Stream))))),
 nop(told).


process_obo_stream_repeat(Stream):-
  repeat,
     nb_current(obo_type,Type),
     nb_current(obo_id, Id),
     once((read_line_to_string(Stream, Line),
     ((should_show_data(_),fail) -> writeln(Line); true),
        normalize_space(chars(Chars),Line))),
        Chars\==[],
        once(process_obo_chars( Type, Chars, Id)),
     ((at_end_of_stream(Stream);reached_file_max) -> ! ; fail).


process_obo_stream(Stream,_Type,_Id) :- (at_end_of_stream(Stream);reached_file_max),!.
process_obo_stream(Stream, Type, Id) :-
  must_det_ll((
    read_line_to_string(Stream, Line), %writeln(Line),
    normalize_space(chars(Chars),Line),
    process_obo_chars( Type, Chars, Id))).


into_rest(Rest,RestChars,RestStr):-
  obo_string(Str,Rest),
  normalize_space(chars(RestChars),Str),
  obo_string(RestStr,RestChars).

obo_string(String,[C|Chars]):- var(String), C==' ',!, obo_string(String,Chars).
obo_string(String,Chars):- string_chars(String,Chars).

process_obo_chars( _, [e,n,d,'_',o,f,'_',f,i,l,e], _):-!.
process_obo_chars( _, [], _) :- !.

process_obo_chars( _, ['['|Chars], _):- append(Left,[']'],Chars),!,
  must_det_ll(( symbol_chars(Type,Left),!, nb_setval(obo_type,Type))).

process_obo_chars( Type, Chars, _):-
  get_key(Key,Chars,Rest),Key == id,
  into_rest(Rest,RestChars,_RestStr),
  symbol_chars(Id,RestChars), assert_OBO(id_type,Id,Type),
  nb_setval(obo_id,Id),nb_setval(obo_type,Type).

process_obo_chars( Type, Chars, Id):-
 must_det_ll((
    get_key(Key,Chars,Rest),
    into_rest(Rest,RestChars,RestStr),
    process_obo_rest_line(Type,Id,Key,RestChars,RestStr))),!.

process_obo_rest_line(Type,Id,Reln,Rest,_):- Reln = id,
   get_some_items([item(Id)],Rest,[]),!, assert_OBO(id_type,Id,Type),!.
process_obo_rest_line(_Type,Id,Ref,_Chars,S):-
   member(Ref,[name,comment]),
   assert_OBO(Ref,Id,S),!.

process_obo_rest_line(Type,Id,Reln,Chars,_):-  Reln = relationship,!,
  must_det_ll((
   key_like_string(KeyLike,Chars,Rest),
    symbol_chars(Key,KeyLike),
    into_rest(Rest,RestChars,RestStr),
    process_obo_rest_line(Type,Id,Key,RestChars,RestStr))).

process_obo_rest_line(_Type,Id,Ref,Chars,_):-
    \+ (member(C,Chars),member(C,['!','[','"'])),
    ( \+ member(' ',Chars)-> symbol_chars(S,Chars);obo_string(S,Chars)),
    assert_OBO(Ref,Id,S),!.

process_obo_rest_line(_Type,Id,is_a,Chars,Str):-
    member('!',Chars), atomic_list_concat([L,R],' ! ',Str),
    normalize_space(atom(T),L),normalize_space(string(N),R),
    assert_OBO(is_a,Id,T), assert_OBO(name,T,N),!.

process_obo_rest_line(_Type,Id,Reln,Chars,_):-
  %  member(Reln,[synonym]),
    get_some_items(List,Chars,[]),
    maplist(fix_obo_arg,List,Args),
    Assert=..[Reln,Id|Args],
    assert_OBO(Assert),!.

%process_obo_rest_line(_Type,Id,Reln,Chars,_):- get_some_items(List,Chars,[]), maplist(arg(1),List,Args), assert_OBO(Reln,Id,Args).
process_obo_rest_line(Type,Id,Miss,Rest,Str):-
  pp_fb('ERROR'(process_obo_rest_line(Type,Id,Miss,Rest,Str))),!.

fix_obo_arg(Var,Var):- var(Var),!.
fix_obo_arg("[]",[]):- !.
fix_obo_arg('[]',[]):- !.
fix_obo_arg(X,Y):- string(X),!,normalize_space(string(Y),X).
fix_obo_arg(X,Y):- atom(X),!,normalize_space(atom(Y),X).
fix_obo_arg(X,Y):- compound(X),arg(1,X,XX),!,fix_obo_arg(XX,Y).

fix_obo_arg(X,X).

/*
Given the DCG rules we've defined, the input

``` OBO

[Term]
id: FBcv:0000391
name: bang sensitive
namespace: phenotypic_class
def: "A phenotype exhibited following mechanical shock and consisting of a brief period of intense, uncoordinated motor activity (legs and wings flailing, abdomen coiling) followed by a prolonged period of paralysis." [FlyBase:FBrf0022877]
synonym: "easily shocked" RELATED [FlyBase:FBrf0022877]
is_a: FBcv:0000389 ! paralytic

```
Would be parsed into the following Prolog terms:
```
[
    bracketed(['Term']),
    key('id'), item('FBcv:0000391'),
    key('name'), item('bang sensitive'),
    key('namespace'), item('phenotypic_class'),
    key('def'), quoted("A phenotype exhibited following mechanical shock and consisting of a brief period of intense, uncoordinated motor activity (legs and wings flailing, abdomen coiling) followed by a prolonged period of paralysis."), bracketed(['FlyBase:FBrf0022877']),
    key('synonym'), quoted("easily shocked"), keyword('RELATED'), bracketed(['FlyBase:FBrf0022877']),
    key('is_a'), item('FBcv:0000389'), named('paralytic')
]
```

*/


get_key(Key)-->key_like_string(Chars),[':'],{symbol_chars(Key,Chars)},!.
get_some_items(I)--> [' '],!,get_some_items(I).
get_some_items(_,[],[]):-!.
get_some_items([H|T])-->get_one_item(H),get_some_items(T). get_some_items([])-->[].
get_one_item(I)--> [' '],!,get_one_item(I).
get_one_item(quoted(Item))-->[x,s,d,':'],symbol_or_url(Chars),{symbol_chars(Item,[x,s,d,':'|Chars])}.
get_one_item(quoted(Item))-->[h,t,t,p],symbol_or_url(Chars),{obo_string(Item,[h,t,t,p|Chars])}.
get_one_item(quoted(Item))-->[f,t,p],symbol_or_url(Chars),{obo_string(Item,[f,t,p|Chars])}.
get_one_item(quoted(Item))-->['"'],string_until_end_quote(Chars),{obo_string(Item,Chars)}.
get_one_item(named(Item))-->['!'],whs,named_like_string(Chars),{symbol_chars(Item,Chars)}.
get_one_item(bracketed(Items))-->['['],whs,items(Items),whs,[']'].
get_one_item(bracketed(Items))-->['{'],whs,items(Items),whs,['}'].
%get_one_item(item(Item))--> whs,key_like_string(Chars),whs,{Chars \==[], symbol_chars(Item,Chars)}.
get_one_item(keyword(Keyword))-->whs,id_like_string(Chars),{Chars\==[]},whs,{symbol_chars(Keyword,Chars)}.
get_one_item(text(Text))-->named_like_string(Chars),{obo_string(Text,Chars)}.
get_one_item(text(Text),[H|T],[]):- ground([H|T]),obo_string(Text,[H|T]),!.
items([Item|Rest])-->item(Item),whs,[','],whs,items(Rest).
items([Item])-->item(Item),!.
item(Item)-->symbol_or_url(Chars),{Chars\==[],symbol_chars(Item,Chars)}.
key_like_string([H|T])-->[H],{\+member(H,[':',' ','\t','\n'])},key_like_string(T).
key_like_string([])-->[].
id_like_string([H|T])-->[H],{\+member(H,['!',' ','\t','\n',',','[',']','{','}','"'])},id_like_string(T).
id_like_string([])-->[].
symbol_or_url([H|T])-->[H],{\+member(H,[',','[',']','"',' '])},symbol_or_url(T).
symbol_or_url([])-->[].
string_until_end_quote([])-->['"'],!.
string_until_end_quote([H|T])-->(['\\',H];[H]),!,string_until_end_quote(T).
named_like_string([H|T])-->[H],{\+member(H,['\n'])},named_like_string(T).
named_like_string([])-->[].
whs-->[''],!,whs. whs-->[].

% ===========================================
% END OBO Loader
% ===========================================

assert_OBO(property_value(Term, URI, V, 'xsd:string')):- assert_OBO(property_value(Term, URI, V)).
assert_OBO(property_value(Term, URI, V)):- simplify_obo_arg(URI,Pred),!,assert_OBO(property_value(Term, Pred, V)).
assert_OBO(property_value(Term, Pred, V)):- simplify_obo_arg(V,VV),!,assert_OBO(property_value(Term, Pred, VV)).
assert_OBO(property_value(Term, Pred, V)):- atom(Pred),!,OBO=..[Pred,Term,V],assert_OBO(OBO).
assert_OBO(synonym(Pred,A,Term,V)):- simplify_obo_arg(V,VV),!,assert_OBO(synonym(Pred,A,Term,VV)).
assert_OBO(ontology_info(Pred,Term,V)):- assert_OBO(property_value(Term, Pred, V)).
assert_OBO([F|List]):- is_list([F|List]),atom(F),OBO=..[F|List],!,assert_OBO(OBO).
assert_OBO(OBO):-
  must_det_ll((
  OBO=..[Fn|Cols],
  into_obofn(Fn,OboFn),
  assert_OB1([OboFn|Cols]))).

assert_OB1(List):- nb_current(make_metta_x,'True'),!, assert_OB2(List).
assert_OB1([OboFn|Cols]):-
  OBO1=..[OboFn|Cols],
  assert_to_metta(OBO1).

assert_OB2(List):- maplist(to_metta_x_args,List,ListO),
 atomics_to_string(ListO,'\t',Str),
  writeln(metta_x_output,Str).

to_metta_x_args(X,O):- X==[],!,O='[]'.
to_metta_x_args(X,O):- atomic(X),!,O=X.
to_metta_x_args(X,O):- term_to_atom(X,O).

args_x_metta(X,O):- X=='[]',!,O=[].
args_x_metta(X,O):- atomic(X),atom_concat('?-',Read,X),!,atom_to_term(Read,O,_).
args_x_metta(X,O):- X=O.
/*
  OBOW=..[OboFn|Cols],
  (is_converting -> (format('~N'), write_src(OBOW));(OBO1=..OBOW,assert_MeTTa(OBO1))))),
  !.
*/

into_obofn(Fn,OboFn):- atom_concat('obo-',_,Fn),!,Fn=OboFn,!.
into_obofn(Fn,OboFn):- atom_concat('obo-',Fn,OboF_),!,use_dashes(OboF_,OboFn).


use_dashes(OboF_,OboFn):-
  atomic_list_concat(List,'_',OboF_),
  atomic_list_concat(List,'-',OboFn),!.


simplify_obo_arg(I,_O):- \+ string(I), \+ atom(I),!,fail.
simplify_obo_arg([],_O):- !, fail.
simplify_obo_arg("[]",[]):-!.
simplify_obo_arg(I,O):- atom_concat('http://purl.obolibrary.org/obo/chebi/',O,I),!.
simplify_obo_arg(I,O):- atom_concat(' ',O,I),!.
simplify_obo_arg(I,O):- atom_concat(O,' ',I),!.
simplify_obo_arg(I,O):- atom_number(I,O),!.


