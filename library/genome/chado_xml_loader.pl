:- use_module(library(sgml)).
%:- use_module(library(logicmoo/xml_reader)).

% Predicate to load XML and extract DTD file name
extract_dtd_file(XMLFile, DTDFileName) :-
    open(XMLFile, read, Stream),
    load_structure(Stream, _XML, [dialect(xml), doctype(Doctype)]),
    close(Stream),
    dtd_file_from_doctype(Doctype, DTDFileName).

% Helper to extract DTD file name from DOCTYPE declaration
dtd_file_from_doctype(Doctype, FileName) :-
    nonvar(Doctype),
    Doctype = doctype(_Name, ExternalID),
    extract_system_id(ExternalID, FileName).

% Extract SYSTEM identifier which usually contains the DTD file name
extract_system_id(ExternalID, FileName) :-
    nonvar(ExternalID),
    ExternalID = system(FileName).


/*
root@gitlab:/wam/data/FB_current# du . -h
14M     ./precomputed_files/insertions
26M     ./precomputed_files/stocks
231M    ./precomputed_files/references
3.9M    ./precomputed_files/human_disease
43M     ./precomputed_files/synonyms
293M    ./precomputed_files/ontologies
34M     ./precomputed_files/clones
22M     ./precomputed_files/orthologs
4.9G    ./precomputed_files/genes
1.4M    ./precomputed_files/species
102M    ./precomputed_files/alleles
148K    ./precomputed_files/map_conversion
35M     ./precomputed_files/go
125M    ./precomputed_files/collaborators
988K    ./precomputed_files/transposons
1.9G    ./precomputed_files/metadata
7.6G    ./precomputed_files
11G     ./dmel_r6.56/fasta
6.3G    ./dmel_r6.56/gff
75M     ./dmel_r6.56/gtf
47G     ./dmel_r6.56/chado-xml
139M    ./dmel_r6.56/dna
64G     ./dmel_r6.56
329G    ./chado-xml
400G    .


*/
/*
load_dtd(DTDFile, DTD) :-
    catch(
        (
            open(DTDFile, read, Stream),
            load_structure(Stream, DTD, [dtd(DTD), dialect(xml)]),
            close(Stream)
        ),
        Error,
        (   print_message(error, Error),
            fail
        )
    ).
*/
    lfb0:-
      fileToLineInfoElements(_Ctx,'/wam/data/FB_current/chado-xml/chado_FBim.xml',XML),
      writeln(XML),!.
    lfb1:-
      load_chado_xml('/wam/data/FB_current/chado-xml/chado_FBgn.xml').
    lfb2:-
      load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_gene_models.xml').
    lfb3:-
      load_chado_xml('/wam/data/FB_current/dmel_r6.56/chado-xml/chado_dmel_predicted.xml').


load_chado_xml(File) :-
        open(File, read, In),
        new_sgml_parser(Parser, []),
        set_sgml_parser(Parser, file(File)),
        set_sgml_parser(Parser, dialect(xml)),
        set_sgml_parser(Parser, space(remove)),

        sgml_parse(Parser,
                   [ source(In),
                     call(begin, on_begin),
                     call(end, on_end)
                   ]),
        close(In).

:- dynamic(feature_data/3).

on_end('feature', _) :-  !,
  finish_feature_data,!,
  listing(feature_data(_,_,_)),
  retractall(feature_data(_,_,_)),
  sleep(0.1),!.
%on_end(Tag, _Parser):- current_tag(Is), Is = Tag, !, pop_tag(Tag), finish_tag(Tag).
on_end(_, _).

on_begin('chado', _, _) :- !.
on_begin('feature', _, _Parser) :- !.
%on_begin(Tag, _Attr, _Parser):- push_tag(Tag),fail.
on_begin(Tag,Attr,Parser):- read_element(Parser, Content, Reset),!,
    (store_feature(Tag,Attr,Content)->true;(set_sgml_parser(Parser,position(Reset)),fail)).
on_begin(Tag,Attr,Parser):- read_element(Parser, Content, Reset),!,
    (try_begin(Tag,Attr,Content)->true;(set_sgml_parser(Parser,position(Reset)),fail)).

on_begin(Any, _, Parser) :- read_element(Parser,Content,_),nl,print(Any=Content),nl.
on_begin(Tag, Attr, Parser) :-
        sgml_parse(Parser,
                   [ document(Content),
                     parse(content)
                   ]),
        FD = feature_data(Tag, Attr, Content),
        print(FD),nl,
        assertz(FD).


current_tag(Tag):- once(clause(current_tag_stack(Was),true,_Ref);Was=[]),append(_New,[Tag],Was),!.
current_tag(none).
parent_tag(Tag):- once(clause(current_tag_stack(Was),true,_Ref);Was=[]),append(_New,[Tag,_],Was),!.
parent_tag(none).

pop_tag(Tag):-
   once(clause(current_tag_stack(Was),true,Ref);Was=[]),append(New,[Tag],Was),
   it_t(nonvar(Ref),erase(Ref)), assert(current_tag_stack(New)),!.
push_tag(Tag):-
  once(retract(current_tag_stack(Was));Was=[]),append(Was,[Tag],New),assert(current_tag_stack(New)).
finish_tag(_Tag).

peek_element(Parser, Content):-
 call_cleanup(
     read_element(Parser, Content, Pos),
     set_sgml_parser(Parser,position(Pos))).

read_element(Parser, Content, Pos):-

    get_sgml_parser(Parser,source(S)),
    stream_property(S,position(Pos)),
    sgml_parse(Parser,
                      [ document(Content),
                        parse(content)
                      ]),!.


    try_begin(Tag,Attr, element(T,A,L)):-!,
        append(Attr,A,AttrA), try_begin(Tag=T,AttrA,L).


    try_begin(Tag,Attr,List):- is_list(List),
        absorb_type_ids(Tag,Attr,List),!.


     try_begin(Tag,Attr,V):- process_feature_data(Tag, Attr, V).

    %try_begin(Tag,Attr, element(T,A,L)):-
     %  %absorb_type_ids(Tag,Attr, element(T,A,L)),
      % maplist(try_begin(T,A),L).


        absorb_type_ids(Tag,Attr,Elements):-
            select(element(type_id,[],C),Elements, Rest),
            get_content([cv,name],C,TypeName),!,
            must_det_ll((get_content([cvterm,name],C,Name),
            maplist(get_element_value_each,Rest,Values),
            maplist(process_feature_data(ntv(Tag,TypeName,Name),Attr),Values))),!.



        absorb_type_ids(_Tag,Attr,Elements):-
            select(element(type_id,[],C),Elements, Rest),
            must_det_ll((get_content([cvterm,name],C,Name),
            maplist(get_element_value_each,Rest,Values),
            maplist(process_feature_data(nv(Name),Attr),Values))),!.

store_feature(Tag,Attr,Content):- cvt_element(element(Tag,Attr,Content),Val),
    assert(feature_data(Tag, Attr, Val)).

skip_over(cvterm).
skip_over(cv).
skip_over(pub).
skip_over_s(featureprop).
skip_over_s(featureprop_pub).
skip_over_s(E):-
member(E,[dbxref_id,
dbxref,
db_id,
library_id,
library,
library_feature]).

skip_over_s(X):- skip_over(X).

cvt_element(List,Val):- is_list(List),!,maplist(cvt_element,List,Val).
cvt_element(element(Tag,[],[element(CVTerm,[],L)]), TagVal):- skip_over_s(CVTerm), !,
    cvt_element(element(Tag,[],L),TagVal).
cvt_element(element(CVTerm,[],[Atomic]),Val):-skip_over_s(CVTerm),!,cvt_element(Atomic,Val).
cvt_element(element(CVTerm,[],Atomic),Val):-skip_over(CVTerm),!,cvt_element(Atomic,Val).
cvt_element(element(Tag,[],[element(T,A,L)]),Tag=Val):- !, cvt_element(element(T,A,L),Val).
cvt_element(element(Tag,[],[Atomic]),Tag=Atomic):-!.
cvt_element(element(Tag,[],List),Tag=Val):- !, cvt_element(List,Val).
cvt_element(Val,Val).


get_content([],R,R):-!.
get_content([S|Tags],L,R):- is_list(L),member(E,L),get_content([S|Tags],E,R),!.
get_content([S|Tags],element(S,_,L),R):- get_content(Tags,L,R),!.
get_content(   STags,element(_,_,L),R):- member(C,L),get_content(STags,C,R),!.

get_element_value_each(element(R,[],List),Out):-
    \+ \+ member(element(_,_,_),List),
   try_begin(R,[],List),
   get_element_value(element(R,[],List),Out).
get_element_value_each(R,Out):- get_element_value(R,Out),!.

get_element_value([L],R):-!,get_element_value(L,R).
get_element_value(element(T,[],[L]),T=R):-  get_element_value(L,R),!.
get_element_value(element(T,[],L),T=R):- is_list(L),!,maplist(get_element_value,L,R).
get_element_value(L,V):- is_list(L),!,maplist(get_element_value,L,V).
get_element_value(L,v(L)).

finish_feature_data:-
  forall(feature_data(Tag, Attr, Content),
    once(process_feature_data(Tag, Attr, Content))),
 writeln('====================================').


sub_prop(name).
sub_prop(value).

%process_feature_data(_,_,_).
/*
process_feature_data(featureprop, Attr, element(T,A,B)):-!,
  process_feature_data(T, A, B).



%process_feature_data(Tag, Attr, Content):- is_list(Content),!,
%  maplist(process_feature_data(_, Attr), Content).
process_feature_data(Tag, Attr, element(T,A,B)):- !,
   process_feature_data(T, Attr, B).
*//*
process_feature_data(Tag, Attr, element(cvterm,A,B)):- !, % sub_prop(T),
   append(Attr,A,AttrA),
   process_feature_data(Tag, AttrA, B).
process_feature_data(Tag, Attr, Content):- is_list(Content),
  member(element(_,_,_),Content),
  maplist(process_feature_data(Tag, Attr), Content).*/
process_feature_data(T, A, B):- print(tab(T,A,B)),nl,sleep(0.1).


