%:- module(lsp_metta_outline, [
%                        xref_document_symbols/2,
%                        xref_metta_source/1]).
:- include(lsp_metta_include).

:- dynamic(lsp_cache:gave_document_symbols/2).
:- retractall(lsp_cache:gave_document_symbols(_, _)). % when we reload this file
xref_document_symbols(Doc, Symbols):- debugging(optimize(no_cache)),lsp_cache:gave_document_symbols(Doc, Symbols),Symbols\==[],!.
xref_document_symbols(Doc, Symbols):- %   sample_outline_test(SS),
    xref_metta_source(Doc),
    maybe_doc_path(Doc,Path),!,
%xref_submit_and_wait(Path),
    findall(
         Symbol,
         ( xref_document_symbol(Path, Outline, KindNumber, StartEnd, Detail),
           once(into_json_range(StartEnd, Location)),
           Symbol = _{name: Outline,
                      kind: KindNumber,
                      detail: Detail,
                      location:
                      _{uri: Doc,
                        range: Location }}
         ),
         Symbols),
    retractall(lsp_cache:gave_document_symbols(Doc, _)),
    asserta(lsp_cache:gave_document_symbols(Doc, Symbols)).

xref_document_symbol(Doc,  Outline, KindNumber, StartEnd, Detail):- maybe_doc_path(Doc,Path),!,xref_document_symbol(Path, Outline, KindNumber, StartEnd, Detail).
xref_document_symbol(Path, Path, 1, range(line_char(0,0), line_char(1000000,0)),Path).
%xref_document_symbol(Path, Outline, KindNumber, StartEnd):- xref_document_symbol_d4(Path, Outline, KindNumber, StartEnd), fail.
xref_document_symbol(Path, Outline, KindNumber, StartEnd, Detail):- xref_document_symbol_fb(Path, Outline, KindNumber, StartEnd, Detail).
%xref_document_symbol(Path, Outline, KindNumber, StartEnd):- xref_document_symbol_examples(Path, Outline, KindNumber, StartEnd).


% for Iconagraphy
xref_document_symbol_examples(_Path, "By Type...", 1, 1000:0, 10000:0,"Types").
xref_document_symbol_examples(_Path, Outline, KindNumber, Start:1, End:0,KindName):-
  lsp_xref_kind(KindNumber, KindName), KindNumber>1,
  Start is KindNumber*10+1000,End is Start+9,
  nonvar(KindName),
  atom_concat('Example ',KindName,KindExample), toPropercase(KindExample,Outline).

% Roy's `d/4`s
xref_document_symbol_d4(Doc, PrettyString, KindNumber, StartEnd, PrettyString):-
   doc_path(Doc,Path),lsp_metta_changes:doc_text_d4(Path,D4s),
   nth1(Nth,D4s,D4), nonvar(D4),
   d4_document_symbol(Nth,D4, PrettyString, KindNumber, StartEnd).

d4_document_symbol(Nth, d(_,Str,_,_), S, 12, Nth:1, End:1):- succl(Nth,End), outline_name(Str,S).

% Douglas' file_buffer
xref_document_symbol_fb(Doc, PrettyString, KindNumber, Range, InfoString):-
   doc_path(Doc,Path),
   user:metta_file_buffer(N,_Ord, Info, What, VL, Path, Range),
   (N > 0 -> interesting_sub_item(N, What) ; true),
   ((Info = index(TypeName,_Type)) -> true;(Info=TypeName,Outline=What)),

   must_succeed1((maybe_name_vars(VL),
      outline_name(Outline, PrettyString),
      type_kind(TypeName,KindName),
      sformat(InfoString,'~q',[Info]),
      lsp_xref_kind(KindNumber, KindName))).


interesting_sub_item(0, _):- !. % in case we use this elsewhere
interesting_sub_item(_, What):- is_ftVar(What), !, fail.
interesting_sub_item(_, What):- number(What), !, fail.
interesting_sub_item(_, What):- is_list(What), !, \+ uninteresting_list(What).
interesting_sub_item(_, Cmt):- Cmt = '$COMMENT'(_,_,_).
interesting_sub_item(1, What):- symbol(What), !, \+ is_documented(What).

uninteresting_list(Var):- \+ is_list(Var),!, fail. % non-list lists are "interesting"
uninteresting_list([What]):- is_ftVar(What), !.
uninteresting_list([]).
uninteresting_list([What]):- \+ symbol(What), \+ is_list(What).

xrefed_outline_type_kind(What,Outline,KindName):-
   xrefed_outline_type(What,Outline,TypeName),
   type_kind(TypeName,KindName),!.


outline_name(Str,S):- string(Str),!,atom_length(Str,Len),Len>2,!,sformat(S,'~q',[Str]).
outline_name(Str,S):- is_ftVar(Str),wots(S, write_src_woi(Str)),!.
outline_name(Str,S):- is_list(Str), wots(S, write_src_woi(Str)),!.
outline_name(Str,S):- Str = exec(_),wots(S, write_src_woi(Str)),!.
outline_name(Cmt,S):- Cmt = '$COMMENT'(Str,_,_),!,sformat(S,'~q',[Str]).
outline_name(Str,S):- symbol(Str), !, sformat(S,'~w',[Str]).
outline_name(Str,S):- wots(S, write_src_woi(Str)),!.
outline_name(Str,S):- sformat(S,'~q',[Str]).

next_line(S:SC,E:SC):- number(S),!,succl(S,E).
next_line(S,E):- number(S),!,succl(S,E).

line_col(Position,LineM1:Col):-
     stream_position_data(line_count, Position, Line),  % Extract the line number.
     LineM1 is Line-1,
     stream_position_data(line_position, Position, Col).  % Extract the column number.


type_kind(Var,WillBe):- var(Var),!,freeze(Var,type_kind(Var,WillBe)).
type_kind(metta_import,number).
type_kind(metta_symbol,key).
type_kind(metta_directive,constant).
type_kind(metta_comment,string).
type_kind(metta_typedecl,typeParameter).
type_kind(metta_defun,function).
type_kind(metta_exec,class).
type_kind(metta_other,interface).
type_kind(Was,Keep):- clause(lsp_xref_kind(_,Was),true),!,Keep=Was.
type_kind(_,array).

lsp_xref_kind(N, LU):- number(LU),var(N),!,LU=N.
lsp_xref_kind(1, file).
lsp_xref_kind(2, module).
lsp_xref_kind(3, namespace).
lsp_xref_kind(4, package).
lsp_xref_kind(5, class).
lsp_xref_kind(6, method).
lsp_xref_kind(7, property).
lsp_xref_kind(8, field).
lsp_xref_kind(9, constructor).
lsp_xref_kind(10, enum).
lsp_xref_kind(11, interface).
lsp_xref_kind(12, function).
lsp_xref_kind(13, variable).
lsp_xref_kind(14, constant).
lsp_xref_kind(15, string).
lsp_xref_kind(16, number).
lsp_xref_kind(17, boolean).
lsp_xref_kind(18, array).
lsp_xref_kind(19, object).
lsp_xref_kind(20, key).
lsp_xref_kind(21, null).
lsp_xref_kind(22, enumMember).
lsp_xref_kind(23, struct).
lsp_xref_kind(24, event).
lsp_xref_kind(25, operator).
lsp_xref_kind(26, typeParameter).
lsp_xref_kind(26, Nonvar):- nonvar(Nonvar).


