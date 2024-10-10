:- module(lsp_metta_split, [
    split_text_document/2,
    split_document_get_multiple_sections/7,
    split_document_get_section_only/4,
    coalesce_text/2
]).
:- use_module(lsp_metta_parser, [
        annotated_get_blank_lines/3
]).

%!  split_document_get_section(+N, -M, +SplitText, -Pre, -This, -Post) is det.
%
%   Splits a document into three parts based on a starting line number N.
%
%   This predicate takes a list of document sections, represented as terms `d(L, Body , EndPosition, Meta)`,
%   where L is the number of lines, Body is the content, and Meta is additional metadata.
%   The goal is to split the document into a prefix (Pre), the section containing the Nth line (This),
%   and the remainder (Post).
%
%   @arg N        The line number at which to split the document.
%   @arg M        Unifies with the offset from N at which the splitting occurs.
%   @arg SplitText The input list of document sections to be split.
%   @arg Pre      The prefix sections before the Nth line.
%   @arg This     The section containing the Nth line.
%   @arg Post     The remaining sections after the Nth line.
%
split_document_get_section(N,N,[],[],d(0,"", 0, []),[]).
split_document_get_section(N,N,[d(L,Body,EndPosition,Meta)|SplitText],[],d(L,Body,EndPosition,Meta),SplitText) :- L>N,!.
split_document_get_section(N,M,[d(L,Body,EndPosition,Meta)|SplitText],[d(L,Body,EndPosition,Meta)|Pre],This,Post) :-
    N1 is N-L,
    split_document_get_section(N1,M,SplitText,Pre,This,Post).

split_document_get_section_only(N,N,[],d(0,"", 0, [])).
split_document_get_section_only(N,N,[d(L,Body,EndPosition,Meta)|_],d(L,Body,EndPosition,Meta)) :- L>N,!.
split_document_get_section_only(N,M,[d(L,_,_,_)|SplitText],This) :-
    N1 is N-L,
    split_document_get_section_only(N1,M,SplitText,This).

split_document_get_multiple_sections(N1,_N2,N1,[],  [],[],[]). % empty list
split_document_get_multiple_sections(_N1,N2,0,SplitText,  [],[],SplitText) :- 0>N2,!. % past the end
split_document_get_multiple_sections(N1,N2,M,[d(L,Body,EndPosition,Meta)|SplitText],  [d(L,Body,EndPosition,Meta)|Pre],This,Post) :- L=<N1,!, % in pre
    N1n is N1-L,
    N2n is N2-L,
    split_document_get_multiple_sections(N1n,N2n,M,SplitText,Pre,This,Post).
split_document_get_multiple_sections(N1,N2,N1,[d(L,Body,EndPosition,Meta)|SplitText],  Pre,[d(L,Body,EndPosition,Meta)|This],Post) :- % in list
    N1n is N1-L,
    N2n is N2-L,
    split_document_get_multiple_sections(N1n,N2n,_M1,SplitText,Pre,This,Post).

% Choose the split strategy
% Have only one of these commented out - any split strategy should work as long as lines are not broken up
%split_text_document(FullText,SplitText) :- split_text_single_lines(FullText,SplitText).
% should use the number of lines in the file, but that would need to be calculated
split_text_document(FullText,[d(Big,FullText,Big,[])]) :- current_prolog_flag(max_tagged_integer,Big).

split_text_document_by_clause(FullText,SplitText) :-
    setup_call_cleanup(
        open_string(FullText,Stream),
        split_text_document_by_clause_aux(Stream, SplitText),
        close(Stream)).

split_text_document_by_clause_aux(Stream,[]) :- at_end_of_stream(Stream),!.
%split_text_document_by_clause_aux(Stream,Out) :-

% get an empty line split (if one exists), followed by a clause (if one exists)
split_text_document_by_clause_get_empty_plus_clause(Stream,[]) :- at_end_of_stream(Stream),!.
split_text_document_by_clause_get_empty_plus_clause(Stream,[]) :-
    seek(Stream,0,current,CurrentPos),
    annotated_get_blank_lines(p(0,0),LCblank,Stream).
    annotated_skip_spaces_until_eol(p(0,0),Pout,Stream,EolFound). %% TODO FIXME

%annotated_read_sexpr_list(LC0,LC0,Stream,[]) :- at_end_of_stream(Stream),!.
%annotated_read_sexpr_list(LC0,LC2,Stream,[Item|L]) :-
%    annotated_read_sexpr(LC0,LC1,Stream,Item),
%    %debug(server,"x ~w",[Item]),
%    annotated_read_sexpr_list(LC1,LC2,Stream,L).

create_line_entry(N,S,d(N,S,Size,false)) :- string_length(S,Size).

extract_line_entry(d(_,S,_,_),S).

concat_strings([],"").
concat_strings([S],S) :- !.
concat_strings([H|T], Result) :-
    concat_strings(T, TailResult),
    string_concat(H, "\n", H1),
    string_concat(H1, TailResult, Result).

split_text_single_lines(FullText,SplitText) :-
    split_string(FullText, "\n", "", SplitText0),
    maplist(create_line_entry(1),SplitText0,SplitText).

coalesce_text(SplitText,FullText) :-
    maplist(extract_line_entry,SplitText,Strings),
    concat_strings(Strings,FullText).
