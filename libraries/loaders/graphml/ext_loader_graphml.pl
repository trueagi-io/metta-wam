
/** <module> read_graphml - Read graph information from a yEd graphml file
 *
 * Extracts nodes with label and description, edges with label,
 * from a yEd graphml file,
 * using SWI-Prolog.
 *
 * @author Carlos Lang-Sanou
 */

 :- module(read_graphml, [read_graphml/2,
    rgml/0,rgml2/0,
    load_graphml/2,
    load_fb_graphml/2]).



% ==============================
% GFF/GTF/GFF3 Reader
% ==============================

load_fb_graphml(Fn,Filename):-
 track_load_into_file(Filename,
  must_det_ll((
    fbug(load_fb_graphml(Fn,Filename)),
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
    Type = 'SequenceFile',
    assert_OBO(id_type(Id,Type)),
    assert_OBO(pathname(Id,Filename)),!,
    assert_OBO(basename(Id,BaseName)),!,
    assert_OBO(directory(Id,Directory)),!,
    read_graphml(Filename,In),
     load_fb_graphml_read(Id,In)))).


load_fb_graphml_read(Id,In):- is_list(In),!,maplist(load_fb_graphml_read(Id),In).
load_fb_graphml_read(Id,In):- In=..[P|InL],Save=..[P,Id|InL],assert_OBO(Save),writeln(Save).

s_list_assert(S,List,Assert):-
    must_det_ll((into_name_values(List,Ns,Vs),
    atomic_list_concat([S|Ns],'_',Pred),
    Assert=..[Pred|Vs])).

fix_value(X,Y):- \+ callable(X),X=Y.
fix_value([X],Y):- !, fix_value(X,Y).
fix_value(X,Y):- is_list(X),!,maplist(fix_value,X,Y).
fix_value(X,Y):- \+ atom(X),!,X=Y.
fix_value(X,Y):- atom_number(X,Y),!.
fix_value(X,X).

elements_are_kv(Data,element(Data,[id=Key],LValue),Key=Value):- fix_value(LValue,Value).
elements_are_kv(Data,element(Data,[key=Key],LValue),Key=Value):- fix_value(LValue,Value).
%elements_are_kv(S2,Content,List2):- maplist(elements_are_kv(S2),Content,List2).

restructure_graphml(Term_list,Terms):- is_list(Term_list),!,maplist(restructure_graphml,Term_list,Terms).
restructure_graphml(element(S,List,[]),Assert):- List \==[], s_list_assert(S,List,Assert).
restructure_graphml(element(graphml,_,Term_list),Terms):-!,restructure_graphml(Term_list,Terms).
restructure_graphml(element(graph,[id='G'|_],Term_list),Terms):-!,restructure_graphml(Term_list,Terms).
restructure_graphml(element(S,[ source=B,target=E],Term_list),Assert):- S == edge,
   %atomic_list_concat([B,E],'_',Id),
   maplist(elements_are_kv(data),Term_list,NVList),
   findall(edge_prop(B,E,N,V),(member(N=V,NVList), \+ member(V,['false','None'])),Assert).
restructure_graphml(element(S,[id=Id],Term_list),Assert):- S == node,
   maplist(elements_are_kv(data),Term_list,NVList),
   findall(node_prop(Id,N,V),(member(N=V,NVList), \+ member(V,['false','None'])),Assert).
restructure_graphml(IO,IO).

%! read_graphml(+File_basename:atom, -Term_list:list) is det
% Read the file File_basename.graphml and produce the corresponding list of terms
%
% @arg File_basename base-name filename without its file extension
% @arg Term_list list of corresponding terms for the graph.
read_graphml(Graphfile, Term_list):-
    file_name_extension(Base_name, '.graphml', Graphfile), !, read_graphml(Base_name, Term_list).
read_graphml(Id, Terms):-
    file_name_extension(Id, '.graphml', Graphfile), !,
      load_structure(Graphfile,Term_list,
    [dialect(xml),space(remove),case_preserving_attributes(false)]),
  restructure_graphml(Term_list,Terms).

read_graphml(Id, Term_list) :-
    file_name_extension(Id, '.graphml', Graphfile), !,
   %load_html(Graphfile, [Graphml], []), !, graphml_term_list(Graphml, Term_list).
%read_graphml(File,Out) :-
    Graphfile = File,
   file_name_extension(Id, _, File),
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
            close(In),
      findall(feature_data(A,B,C), feature_data(A,B,C),Term_list).





:- dynamic(feature_data/3).

on_end('graphml', _) :-  !,
  finish_feature_data,!,
  listing(feature_data(_,_,_)),
  retractall(feature_data(_,_,_)),
  sleep(0.1),!.
%on_end(Tag, _Parser):- current_tag(Is), Is = Tag, !, pop_tag(Tag), finish_tag(Tag).
on_end(_, _).

on_begin('chado', _, _) :- !.
on_begin('graphml', _, _) :- !.
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
    member(E,[dbxref_id, dbxref,
    db_id, library_id, library, library_feature]).
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

%process_feature_data(featureprop, Attr, element(T,A,B)):-!,
%  process_feature_data(T, A, B).

%process_feature_data(Tag, Attr, Content):- is_list(Content),!,
%  maplist(process_feature_data(_, Attr), Content).
%process_feature_data(Tag, Attr, element(T,A,B)):- !,
%   process_feature_data(T, Attr, B).

%process_feature_data(Tag, Attr, element(cvterm,A,B)):- !, % sub_prop(T),
%  append(Attr,A,AttrA),
%   process_feature_data(Tag, AttrA, B).

process_feature_data(S, List, [Value]):- \+ compound(Value),!,
  append(List,[value=Value],VList),
  process_feature_data(S, VList, []).


process_feature_data(S, List, Nil):-  Nil == [], List \==[],
  must_det_ll((into_name_values(List,Ns,Vs),
    atomic_list_concat([S|Ns],'_',Pred),
    Assert=..[Pred|Vs],!,
    afd(Assert))),!.



process_feature_data(S1, List1, Content):- fail, Content\==[], is_list(Content),
   maplist(elements_are_kv(S2),Content,List2),
   atomic_list_concat([S1,S2],'_',SS),
   append(List1,List2,List),!,
   process_feature_data(SS, List, []).

process_feature_data(S, List, Ele):-
   Ele = element(S2,List2, Nil),
   List==List2, S2==S, !,
   process_feature_data(S, List, Nil).

process_feature_data(S, [N=V], Content):- is_list(Content), member(element(_,_,_),Content),!,
   process_feature_data(S, [N=V], []),
   process_feature_data(V, [], Content).

process_feature_data(S1, List1, Ele):- fail,
       Ele = element(S2, List2, Nil),
   S2\==S1, !, atomic_list_concat([S1,S2],'_',SS),
    append(List1,List2,List),
    %process_feature_data(S1, List1, []),
    process_feature_data(SS, List, Nil).

process_feature_data(Tag, Attr, Content):- is_list(Content), member(element(_,_,_),Content),
  maplist(process_feature_data(Tag, Attr), Content).


process_feature_data(T, A, B):- print(tab(T,A,B)),nl,sleep(0.1).




afd(Assert):- wdmsg(Assert).

into_name_values([],[],[]):-!.
into_name_values([N=V|List],[FN|Ns],[FV|Vs]):-
  fix_name(N,FN),fix_value(V,FV),into_name_values(List,Ns,Vs).

fix_name(N,FN):- \+ atom(N),!,FN=N.
fix_name(N,FN):- atom_concat('attr.',FFN,N),!,fix_name(FFN,FN).
%fix_name(N,FN):- atom_concat('v_',FFN,N),!,fix_name(FFN,FN).
%fix_name(N,FN):- atom_concat('n_',FFN,N),!,fix_name(FFN,FN).
%fix_name(N,FN):- atom_concat('e_',FFN,N),!,fix_name(FFN,FN).
fix_name(N,FN):- atom_concat('_',FFN,N),!,fix_name(FFN,FN).
fix_name(N,N).

rgml:-  load_graphml('CKG_N','tests/performance/knowledge_graphs/graphml_csv/cml/ckg_neighbors_cml_graph_n15612_e21425.graphml').
rgml2:- load_graphml('&self','library/graphml/tests/*.graphml').

load_graphml(KB,Paths):- atom(Paths),expand_file_name(Paths,List),List\==[Paths],!,maplist(load_graphml(KB),List).
load_graphml(KB,Paths):- is_list(Paths),!,maplist(load_graphml(KB),Paths).
load_graphml(KB,Paths):- read_graphml(Paths,To),!,load_fb_graphml_read(KB,To).





%! dump_graph(+File_basename:atom) is det
% Read the file File_basename.graphml and write the parsed structure into File_basename.pl
%
% @arg File_basename base-name filename without its file extension
dump_graph(Base_name) :-
    atomic_list_concat([Base_name, '.graphml'], Graphfile),
    atomic_list_concat([Base_name, '.pl'], PLfile),
    load_html(Graphfile, Graphml, []),
    open(PLfile, write, Out),
        print_term(Graphml, [output(Out)]),
        writeln(Out, '.'),
        flush_output(Out),
    close(Out).


%! run(+File_basename:atom) is det
% Read the file File_basename.graphml and print the corresponding list of terms
%
% @arg File_basename base-name filename without its file extension
run(Base_name) :-
    read_graphml(Base_name, Term_list),
    print_term(Term_list, []),
    !.


%! new_node(Node_dict) is det.
% defines the structure of a node dict
new_node(node{id:_, label:_, description:_}).

%! new_edge(Edge_dict) is det.
% defines the structure of an edge dict
new_edge(edge{id:_, source_id:_, target_id:_, label:_}).


%! graphml_term_list(++Graph_element:term, -Term_list:list) is det
% Term_list is the list of terms for Graph_element
%
% @arg Graph_element term of the form element(graphml, _Graphml_prop_list, Element_list)
% @arg Term_list list of corresponding list of terms for the given graph.
graphml_term_list( element(graphml, _Graphml_prop_list, Element_list), Term_list ) :-
    keys(Element_list, Key_list),
    memberchk(element(graph, _Graph_prop_list, Graph_element_list), Element_list),
    element_list_term_list(Graph_element_list, Key_list, Term_list).


%! element_list_term_list(++Element_list:list, ++Attr_key_list:list, -Term_list:list) is det
% Term_list is the list of terms that corresponds to Element_list
% given the list of attribute keys Attr_key_list
%
% @arg Element_list list of graph elements
% @arg Attr_key_list list of key(From, Attr, Key)
% @arg Term_list list of terms extracted from the Element_list
element_list_term_list(Element_list, Attr_key_list, Term_list) :-
    findall(
        Term,
        (
            member(Element, Element_list),
            graph_element_term(Element, Attr_key_list, Term)
        ),
        Term_list
    ).


%! graph_element_term( ++Element:term, ++Attr_key_list:list, -Term ) is det
% Term is the term that corresponds to Element.
%
% @arg Element graph element
% @arg Attr_key_list list of element attribute keys of the form key(From, Attr, Key)
% @arg Term term corresponding to Element

% node(Node_id:atom, Node_label:string, Node_description:string)
graph_element_term(
    element(node, Node_props, Node_elements),
    Attr_key_list,
    Node
) :- !,
    memberchk(id=Node_id, Node_props),
    node_description(Node_elements, Node_description, Attr_key_list),
    node_label(Node_elements, Node_label, Attr_key_list),
    new_node(Node),
    Node.id = Node_id, Node.label = Node_label, Node.description = Node_description.


% edge(Edge_id:atom, Source_id:atom, Target_id:atom, Edge_label:string)
graph_element_term(
    element(edge, Edge_props, Edge_elements),
    Attr_key_list,
    Edge
) :- !,
    memberchk(id=Edge_id, Edge_props),
    memberchk(source=Source_id, Edge_props),
    memberchk(target=Target_id, Edge_props),
    edge_label(Edge_elements, Edge_label, Attr_key_list),
    new_edge(Edge),
    Edge.id = Edge_id, Edge.source_id = Source_id, Edge.target_id = Target_id, Edge.label = Edge_label.


%! node_description(++Node_element_list:list, -Node_description:string, ++Attr_key_list) is det
% Node_description is the node description found in Node_element_list
% where Attr_key_list is used to
%
% @arg Node_element_list list of elements describing the node
% @arg Node_description description for the node
% @arg Attr_key_list list of attribute keys to be used for reference
node_description(Node_element_list, Node_description, Attr_key_list) :-
    memberchk(key(node, description, Key_node_description), Attr_key_list),
    data(Key_node_description, Node_element_list, [Node_description]), !.

node_description(_, "", _).


%! node_label(++Node_element_list:list, -Node_label:string, ++Attr_key_list:list) is det
% Node_label is the node label found in Node_element_list
% where Attr_key_list is used to
%
% @arg Node_element_list list of elements describing the node
% @arg Node_label label for the node
% @arg Attr_key_list list of attribute keys to be used for reference
node_label(Node_element_list, Node_label, Attr_key_list) :-
    memberchk(key(node, nodegraphics, Key_nodegraphics), Attr_key_list),
    (
        data(Key_nodegraphics, Node_element_list, Nodegraphics_elements),
        member(element('y:ImageNode', _Image_props, Image_elements ), Nodegraphics_elements),
        member(element('y:NodeLabel', _Label_props, [Node_label]), Image_elements)
    ;
        Node_label = ""
    ),
    !.

%! edge_label(Edge_element_list, Edge_label, Attr_key_list).
% Edge_label is the edge label found in Edge_element_list
% where Attr_key_list is used to
%
% @arg Edge_element_list list of elements describing the node
% @arg Edge_label label for the node
% @arg Attr_key_list list of attribute keys to be used for reference
edge_label(Edge_element_list, Edge_label, Attr_key_list) :-
    memberchk(key(edge, edgegraphics, Key_edgegraphics), Attr_key_list),
    (
        data(Key_edgegraphics, Edge_element_list, Edgegraphics_elements),
        member(element(_Graphic_type, _Graphic_type_props, Graphic_type_elements), Edgegraphics_elements),
        member(element('y:EdgeLabel', _Label_props, Label_elements), Graphic_type_elements),
        member(Edge_label_1, Label_elements),
        normalize_space(atom(Edge_label),Edge_label_1)
    ;
        Edge_label = ''
    ),
    !.


%! data(+Key_id:atom, ++Element_list:list, -Sub_element_list:list) is nondet
% Sub_element_list is the list of sub_elements
% of an element within Element_list
% of type 'data' and key=Key_id
%
% @arg Key_id of the data searched for
% @arg Element_list list of elements among which to search for a data element
% @arg Sub_element_list content of the data element in Element_list with the given Key_id
data(Key_id, Element_list, Sub_element_list) :-
    member(element(data, Props, Sub_element_list), Element_list),
    member(key=Key_id, Props).


%! element_attribute(?Element_type, ?Attr_name) is nondet
% Attr_name is the name of an attribute of interest for elements of type Element_type
%
% @arg Element_type type of element
% @arg Attr_name name of the attribute
element_attribute(node, nodegraphics).
element_attribute(node, description).
element_attribute(edge, edgegraphics).
element_attribute(edge, description).


%! keys(++Element_list:list, -Key_list:list) is det
% Key_list is the list of terms of form key(element_type, attribute_name, key_id),
% corresponding to elements in Element_list which define key_ids
%
% @arg Element_list list of elements to pull keys from
% @arg Key_list list of terms of the form key(element_type, attribute_name, key_id)
keys(Elements, Keys) :-
    findall(
        key(For, Attr, Key_id),
        (
            element_attribute(For, Attr),
            key(Elements, For, Attr, Key_id)
        ),
        Keys
    ).


%! key(++Element_list:list, +Element_type:atom, +Attr_name:atom, -Key_id:atom) is det
% Key_id is used for the Attr_name of the Element_type.
%
% @arg Element_list list of graphml elements
% @arg Element_type type of element
% @arg Attr_name name of the attribute
% @arg Key_id used for the corresponding Element_type and Attr_name
key(Element_list, Element_type, Attr_name, Key_id) :-
    member(element(key, Key_props, _), Element_list),
    member(for=Element_type, Key_props),
    (   member('attr.name'=Attr_name, Key_props)
    ;   member('yfiles.type'=Attr_name, Key_props)
    ),
    member(id=Key_id, Key_props),
    !.


% :- rgml.

