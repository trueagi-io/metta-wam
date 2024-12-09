:- module(lsp_metta_colours, [
                        token_types/1,
                        token_modifiers/1]).

% these are LSP token types/modifiers.

token_types([namespace,
             type,
             class,
             enum,
             interface,
             struct,
             typeParameter,
             parameter,
             variable,
             property,
             enumMember,
             event,
             function,
             member,
             macro,
             keyword,
             modifier,
             comment,
             string,
             number,
             regexp,
             operator
            ]).
token_modifiers([declaration,
                 definition,
                 readonly,
                 static,
                 deprecated,
                 abstract,
                 async,
                 modification,
                 documentation,
                 defaultLibrary
                ]).


:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/3, exclude/3]).
%:- use_module(library(prolog_xref)).
%:- use_module(library(prolog_source), [read_source_term_at_location/3]).
%:- use_module(library(help), [help_html/3, help_objects/3]).
:- use_module(library(lynx/html_text), [html_text/1]).
:- use_module(library(solution_sequences), [distinct/2]).
:- use_module(library(lists), [append/3, member/2, selectchk/4]).
:- use_module(library(sgml), [load_html/3]).

    :- include(lsp_metta_include).


get_document_symbols(Path, S) :-
    lsp_metta_changes:doc_text_d4(Path,SplitText),
    get_document_symbol_aux(SplitText,0,S).

get_document_symbol_aux([],_,[]).
get_document_symbol_aux([d(L,Text,_,_)|Rest],Line0,Result) :-
    setup_call_cleanup(
        open_string(Text,Stream),
        annotated_read_sexpr_list(p(Line0,0),_,Stream,ItemList),
        close(Stream)),
    %debug(lsp(low),"XXXXXXXXXXXXX: ~w~w",[Line0,ItemList]),
    get_document_symbol_aux2(ItemList,R0),
    %debug(lsp(low),"X1",[]),
    Line1 is Line0+L,
    get_document_symbol_aux(Rest,Line1,Result0),
    %debug(lsp(low),"X2",[]),
    append(R0,Result0,Result).

get_atom_kind_name('',0,'') :- !.
get_atom_kind_name(')',0,'') :- !.
get_atom_kind_name(']',0,'') :- !.
get_atom_kind_name('}',0,'') :- !.
get_atom_kind_name(var(Term),13,Term) :- !.
get_atom_kind_name(Term,16,'number') :- number(Term),!.
get_atom_kind_name(Term,12,Term) :- !.

get_document_symbol_aux2([], []) :- !.
get_document_symbol_aux2([Head|Tail], FlatList) :- !,
    get_document_symbol_aux2(Head, FlatHead),
    get_document_symbol_aux2(Tail, FlatTail),
    append(FlatHead,FlatTail,FlatList).
get_document_symbol_aux2(exec(H),FlatList) :- !,get_document_symbol_aux2(H,FlatList).
get_document_symbol_aux2(a(L,C0,C1,Sym),FlatList) :-
    (get_atom_kind_name(Sym,Type,Name),Type>0 -> FlatList=[x(L,C0,C1,Type,Name)] ; FlatList=[]).
get_document_symbol_aux2(_,[]).
