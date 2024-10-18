:- module(lsp_metta_utils, [
                        help_at_position/4,
                        get_document_symbols/2,
                        clause_with_arity_in_file_at_position/4,
                        help_at_position/4
                        ]).
:- use_module(library(debug), [debug/3]).
:- use_module(lsp_metta_xref).
:- use_module(lsp_metta_changes, [handle_doc_changes/2]).
:- use_module(lsp_metta_parser, [annotated_read_sexpr_list/4]).
:- use_module(lsp_metta_split, [
        split_document_get_section_only/4
]).

:- dynamic lsp_metta_changes:doc_text/2.

% /** <module> LSP Utils
%
% Module with a bunch of helper predicates for looking through prolog
% source and stuff.
%
% @author James Cash
% */

%! help_at_position(+Path:atom, +Line:integer, +Char:integer, -Help:string) is det.
%
%  =Help= is the documentation for the term under the cursor at line
%  =Line=, character =Char= in the file =Path=.
help_at_position(Path, Line, Char0, S) :-
    %debug(server,"help_at_position",[]),
    clause_with_arity_in_file_at_position(Clause, Arity, Path, line_char(Line, Char0)),
    % TODO - add this in when I can import eval_args
    %debug(server,"Clause=~w",[Clause]),
    predicate_help(Path,Clause,Arity,S).

predicate_help(_,Var,_,S) :- var(Var),!,format(string(S),"Var: ~w",[Var]).
predicate_help(_,'',_,"") :- !.
predicate_help(_,var(Term),_,S) :- !,format(string(S),"Variable: ~w",[Term]).
predicate_help(_,Term,_,"") :- number(Term),!.
predicate_help(_,')',_,"") :- !.
predicate_help(_,']',_,"") :- !.
predicate_help(_,'}',_,"") :- !.
predicate_help(_,Term,_,S) :- find_at_doc(Term,S), !.
predicate_help(Path,Clause,Arity,S) :-
  user:(
     current_predicate(predicate_help_hook/5),
     predicate_help_hook(first,Path,Clause,Arity,S)),!.
predicate_help(_,Term,Arity,S) :- metta_atom(_KB,['@doc',Term|Help]),
    format_metta_doc(Term,Arity,Help,S),!.
predicate_help(Path,Clause,Arity,S) :-  
  user:(
     current_predicate(predicate_help_hook/5),
     predicate_help_hook(last,Path,Clause,Arity,S)),!.
predicate_help(_,Term,_,S) :- format(string(S),"Unknown: ~w",[Term]).

format_metta_doc(Term,Arity,[['@desc',Description], ['@params', Params], ['@return', Return]],String) :-
    maplist(format_metta_Param,Params,Params_formatted),
    atomic_list_concat(Params_formatted,'\n',Params_formattednl),
    length(Params,LP),
    ((Arity=unknown;Arity=LP) -> Warning="" ; format(string(Warning),"\n Arity warning, found ~w, expected ~w",[Arity,LP])),
    format(string(String),"~w: ~w\n~w\nReturns: ~w~w",[Term,Description,Params_formattednl,Return,Warning]).

format_metta_Param(['@param',P],Pf) :- format(string(Pf),"Param: ~w",[P]).

find_at_doc(Term,S) :-
    lsp_metta_changes:doc_text(Path,SplitFile),
    find_at_doc_aux(Path,Term,SplitFile,S).

find_at_doc_aux(_Path,Term,[d(_,Doc,_,Metadata)|_],S) :-
    find_at_doc_aux2(Term,Metadata),!,
    format(string(S),"@doc found: ~w",[Doc]).
find_at_doc_aux(Path,Term,[_|T],S) :-
    find_at_doc_aux(Path,Term,T,S).

find_at_doc_aux2(Term,[doc(Term)|_]) :- !.
find_at_doc_aux2(Term,[_|T]) :-
    find_at_doc_aux2(Term,T).


%!  clause_with_arity_in_file_at_position(-Clause, -Arity, +Path, +Position) is det.
%
%   Reads the clause located at the specified position within the given file.
%
%   @arg Clause is the clause found at the specified position.
%   @arg Path is the path to the file being analyzed.
%   @arg Position is the position in the file (typically line/character position).
%
%   @example Example usage:
%       ?- clause_with_arity_in_file_at_position(Clause, 'file.pl', line_char(5, 10)).
%       Clause = (some_prolog_fact :- some_prolog_goal).
%
clause_with_arity_in_file_at_position(Clause, Arity, Path, line_char(Line, Char)) :-
    % Setup a stream to read the file and find the clause at the specified position.
    lsp_metta_changes:doc_text(Path,SplitText),
    split_document_get_section_only(Line,LinesLeft,SplitText,d(_,Text,_EndPosition,_Meta)),
    %string_codes(Text,TextChars),
    %debug(server,"Input ~w",[TextChars]),
    setup_call_cleanup(
        open_string(Text,Stream),
        annotated_read_sexpr_list(p(0,0),_,Stream,ItemList),
        close(Stream)),
    %debug(server,"1 ~w ~w ~w",[ItemList,0,Char]),
    (find_term_in_annotated_stream(0,ItemList,LinesLeft,Char,Clause,Arity) -> true ; Clause='',Arity=0).
    %debug(server,"2 ~w ~w",[Clause,Arity]).

find_term_in_annotated_stream(_,a(Lpos,S,E,Term),Lpos,CPos,Term,-1) :- CPos=<E,!,S=<CPos.
find_term_in_annotated_stream(Depth,[a(Lpos,S,E,Term)|T],Lpos,CPos,Term,Arity) :- CPos=<E,!,S=<CPos,
    (Depth==0 -> Arity=unknown ; length(T,Arity)).
find_term_in_annotated_stream(_,exec(L),Lpos,CPos,Term,Arity) :- find_term_in_annotated_stream(1,L,Lpos,CPos,Term,Arity).
find_term_in_annotated_stream(Depth,[H|T],Lpos,CPos,Term,Arity) :-
    Depth1 is Depth+1,
    (find_term_in_annotated_stream(Depth1,H,Lpos,CPos,Term,Arity) -> true ; find_term_in_annotated_stream(Depth,T,Lpos,CPos,Term,Arity)).

get_document_symbols(Path, S) :-
    lsp_metta_changes:doc_text(Path,SplitText),
    get_document_symbol_aux(SplitText,0,S).

get_document_symbol_aux([],_,[]).
get_document_symbol_aux([d(L,Text,_,_)|Rest],Line0,Result) :-
    setup_call_cleanup(
        open_string(Text,Stream),
        annotated_read_sexpr_list(p(Line0,0),_,Stream,ItemList),
        close(Stream)),
    %debug(server,"XXXXXXXXXXXXX: ~w~w",[Line0,ItemList]),
    get_document_symbol_aux2(ItemList,R0),
    %debug(server,"X1",[]),
    Line1 is Line0+L,
    get_document_symbol_aux(Rest,Line1,Result0),
    %debug(server,"X2",[]),
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

