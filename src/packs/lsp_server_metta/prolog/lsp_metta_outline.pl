%:- module(lsp_metta_outline, [
%                        xref_document_symbols/2,
%                        xref_metta_source/1]).

:- dynamic(gave_document_symbols/2).
:- retractall(gave_document_symbols(_, _)). % when we reload this file
xref_document_symbols(Doc, Symbols):- gave_document_symbols(Doc, Symbols),!.
xref_document_symbols(Doc, Symbols):- %   sample_outline_test(SS),
    xref_metta_source(Doc),    
    maybe_doc_path(Doc,Path),!,
%xref_submit_and_wait(Path),
    findall(
         Symbol,
         ( xref_document_symbol(Path, Outline, KindNumber, StartEnd),
           once(into_json_range(StartEnd, Location)),
           Symbol = _{name: Outline,
                      kind: KindNumber, 
                      location:
                      _{uri: Doc,
                        range: Location }}
         ),
         Symbols),
    retractall(gave_document_symbols(Doc, _)),
    asserta(gave_document_symbols(Doc, Symbols)).
xref_document_symbol(Doc,  Outline, KindNumber, StartEnd):- maybe_doc_path(Doc,Path),!,xref_document_symbol(Path, Outline, KindNumber, StartEnd).
xref_document_symbol(Path, Path, 1, range(line_char(0,0), line_char(1000000,0))).
%xref_document_symbol(Path, Outline, KindNumber, StartEnd):- xref_document_symbol_d4(Path, Outline, KindNumber, StartEnd), fail.
xref_document_symbol(Path, Outline, KindNumber, StartEnd):- xref_document_symbol_fb(Path, Outline, KindNumber, StartEnd).
%xref_document_symbol(Path, Outline, KindNumber, StartEnd):- xref_document_symbol_examples(Path, Outline, KindNumber, StartEnd).

% for Iconagraphy
xref_document_symbol_examples(_Path, "By Type...", 1, 1000:0, 10000:0).
xref_document_symbol_examples(_Path, Outline, KindNumber, Start:1, End:0):-
  lsp_xref_kind(KindNumber, KindName), KindNumber>1,
  Start is KindNumber*10+1000,End is Start+9,
  nonvar(KindName),
  atom_concat('Example ',KindName,KindExample), toPropercase(KindExample,Outline).

% Roy's `d/4`s
xref_document_symbol_d4(Doc, PrettyString, KindNumber, StartEnd):- 
   doc_path(Doc,Path),lsp_metta_changes:doc_text(Path,D4s), 
   nth1(Nth,D4s,D4), nonvar(D4), 
   d4_document_symbol(Nth,D4, PrettyString, KindNumber, StartEnd).

d4_document_symbol(Nth, d(_,Str,_,_), S, 12, Nth:1, End:1):- succl(Nth,End), outline_name(Str,S).

% Douglas' file_buffer
xref_document_symbol_fb(Doc, PrettyString, KindNumber, Range):- 
   doc_path(Doc,Path),
   metta_file_buffer(0,_Ord, KindName, What,VL,Path, Range),
   must_succeed1((maybe_name_vars(VL),
      outline_name(What, PrettyString),
      lsp_xref_kind(KindNumber, KindName))).
   

/*
xref_document_symbol_fb(Doc, PrettyString, KindNumber, StartEnd):- 
   doc_path(Doc,Path),
   clause(metta_file_buffer(0,_Ord,_Kind,What,VL,Path,PosStart),true,Ref), line_col(PosStart,Start),
   ignore(maybe_name_vars(VL)),
   once(((xrefed_outline_type_kind(What,Outline,KindName),outline_name(Outline,PrettyString),lsp_xref_kind(KindNumber, KindName)))),   
   once(((next_clause(Ref, metta_file_buffer(0,_Ord,_Kind,_,_,Path,PosEnd)), line_col(PosEnd,End)))-> true ; next_line(Start,End)).
*/
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

%


nop_mod
:- module(lsp_metta_references, 
                     [metta_called_at/4,
                      metta_defined_at/4,
                      defined_at/4,
                      matta_name_callable/2,
                      metta_relative_ref_location/4
                      %help_at_position/4,
                      %clause_in_file_at_position/3,
                      %clause_variable_positions/3,
                      %seek_to_line/2,
                      %linechar_offset/3
                     ]).
/** <module> LSP Utils

Module with a bunch of helper predicates for looking through prolog
source and stuff.

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/3, exclude/3]).
%:- use_module(library(prolog_xref)).
%:- use_module(library(prolog_source), [read_source_term_at_location/3]).
%:- use_module(library(help), [help_html/3, help_objects/3]).
:- use_module(library(lynx/html_text), [html_text/1]).
:- use_module(library(solution_sequences), [distinct/2]).
:- use_module(library(lists), [append/3, member/2, selectchk/4]).
:- use_module(library(sgml), [load_html/3]).

%! metta_called_at(+Path:atom, +Clause:term, -By:term, -Location:term) is nondet.
%  Find the callers and locations of the goal =Clause=, starting from
%  the file =Path=. =Location= will be bound to all the callers and
%  locations that the =Clause= is called from like =Caller-Location=.

% textDocument/references: returns a list of specific locations where the symbol is referenced or called from. Moreover, it includes the results from textDocument/implementation (which itself includes textDocument/definition and textDocument/declaration), providing a comprehensive overview of the symbol's usage across the codebase.
metta_called_at(Path, Clause, By, Location) :-
    matta_name_callable(Clause, Callable),
    xref_metta_source(Path),
    metta_line_buffer(_,CallerLine, _VL, Path, Location),
    metta_callee(CallerLine, Callable),
    metta_caller(CallerLine, By).

metta_caller(Clause, Symbol):- is_definition(decl(_),Symbol,Clause).
metta_callee(Clause, Symbol):- is_definition(ref(_) ,Symbol,Clause).

into_op_head_body(Clause,Op,Head,Body):- var(Clause),!,freeze(into_op_head_body(Clause,Op,Head,Body)).
into_op_head_body(exec(List),Op,Head,Body):- !, into_op_head_body_exec(List,Op,Head,Body).
into_op_head_body('$COMMENT'(List,_,_),none,[],List):- !.
into_op_head_body([Op|List],Op,Head,Body):- nonvar(Op), op_type(import,Op),!,append(Body,[Head],List).
into_op_head_body([Op,Head|Body],Op,Head,Body):- nonvar(Op), op_type(_,Op),!.
into_op_head_body(Head,'=',Head,[]).

into_op_head_body_exec([Op|List],Op,Head,Body):- nonvar(Op), op_type(import,Op),!,append(Body,[Head],List).
into_op_head_body_exec([Op,Head|Body],Op,Head,Body):- nonvar(Op), op_type(_,Op),!.
into_op_head_body_exec(Body,[],[],Body).

is_exec(exec(_)).

is_definition(Type,Symbol,Clause):- 
   freeze(Type, (is_exec(Clause),compound(Type))),
   freeze(Clause, (is_exec(Clause),compound(Type))),
   into_op_fun_rest_body(Clause,Op,Fun,Rest,Body), 
   type_op_head_rest_body(Type,Symbol,Op,Fun,Rest,Body).

type_symbol_clause(Type,Symbol,Clause):-
  clause_type_op_fun_rest_body(Type,Symbol,Clause,_Op,_Fun,_Rest,_Body).

clause_type_op_fun_rest_body(Type,Symbol,Clause,Op,Fun,Rest,Body):-
   ( ( \+ var(Clause)) -> true ; (metta_file_buffer(0,_Ord,_Kind, Clause, VL, _Filename, _LineCount),ignore(maybe_name_vars(VL)))),
   once(into_op_fun_rest_body(Clause,Op,Fun,Rest,Body)),
   type_op_head_rest_body(Type,Symbol,Op,Fun,Rest,Body).
   

into_op_fun_rest_body(Clause,Op,Fun,Rest,Body):- 
  into_op_head_body(Clause,Op,Head,Body), split_head(Head,Fun,Rest).

split_head([Fun|Rest],Fun,Rest):- is_list(Rest),!.
split_head(Head,Head,[]).

type_op_head_rest_body(decl(use), Symbol, Op,_Head,_Rest, Body):- op_type(import,Op),    sub_symbol(Symbol,Body).
type_op_head_rest_body(ref(a), Symbol, Op, Head,_Rest,_Body):- op_type(import,Op), !, sub_symbol(Symbol,Head).

type_op_head_rest_body(ref(a), Symbol,_Op,_Head, Rest, Body):- not_promiscuous(Symbol),sub_symbol(Symbol,[Body, Rest]).
type_op_head_rest_body(Type,Symbol, Op, Head,_Rest,_Body):- op_type(Type,Op),!,sub_symbol(Symbol,Head).

not_promiscuous(Symbol):- var(Symbol), !, freeze(Symbol,not_promiscuous(Symbol)).
not_promiscuous(Symbol):- number(Symbol),!, fail.
not_promiscuous(Symbol):- \+ promiscuous_symbol(Symbol).

sub_symbol(Symbol,Head):- ground(Symbol),!,sub_var(Symbol,Head),!.
sub_symbol(Symbol,Head):- \+ var(Symbol), once(sub_term(Symbol,Head)),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),atom(Symbol),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),string(Symbol),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),atomic(Symbol),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),!.

xref_metta_defined(Path, Target, Ref):-
  xref_metta_defined(Type, Target, Path, Ref), Type\=ref(_).

xref_metta_defined(Type, Target, Path, Ref):- 
  xref_metta_defined(Type, Target, _Clause, Path, Ref).

xref_metta_defined(Type, Target, Clause, Path, PosStart):- 
   type_expand(Type,RefType),
   metta_file_buffer(0,_Ord,_Kind,Clause,VL, Path, PosStart),
    ignore(maybe_name_vars(VL)), 
    once(type_symbol_clause(ExpTypeO,Target,Clause)),ExpTypeO=RefType.

type_expand(Var,Var):- var(Var),!.
type_expand(definition,RefType):- member(RefType, [decl(_)]).
type_expand(declaration,RefType):- member(RefType, [decl(use)]).
type_expand(references,RefType):- member(RefType, [ref(_)]).
type_expand(typeDefinition,RefType):- member(RefType, [decl(ftype)]).
type_expand(implementation,RefType):- member(RefType, [decl(_),decl(use)]).

% textDocument/declaration: returns the specific location of the symbol's type declaration, which can include its function definition, symbol definition, etc. Since only one location can be returned, the system chooses the most relevant type declaration for the symbol.
% textDocument/implementation: returns a list of specific locations where the symbol is implemented. Additionally, it includes the locations returned by both textDocument/definition and textDocument/declaration, showing the full picture of where the symbol is implemented and its type associations.
% textDocument/definition: returns the specific location in the document or file where the symbol is defined or documented. It points to the exact spot where the symbol is introduced in the code.
defined_at(Type, HintPath, NameArity, Location):- metta_defined_at(Type, HintPath, NameArity, Location).
metta_defined_at(Type, HintPath, NameArity, Location):- metta_defined_at(Type, HintPath, NameArity, _, Location).

%metta_defined_at(RefType, HintPath, Target, Clause, Location):- Target=Name/Arity, nonvar(Name),!,metta_defined_at(RefType, HintPath, Name, Clause, Location).
%metta_defined_at(RefType, HintPath, Target, Clause, Location):- nonvar(HintPath),!, xref_metta_source(HintPath), metta_defined_at(RefType, HintPath, Target, Clause, Location).
metta_defined_at(RefType, HintPath, NameArity, Clause, Location):-  
  xref_metta_source(HintPath),
  matta_name_callable(NameArity, Target),
  each_type_at_sorted(Target, Sort),!,
  member(each_type_at(Target,Clause, Path, Ref, Type),Sort),
  once(type_expand(RefType,Type)),
  path_doc(Path, Doc),
  once(metta_relative_ref_location(Doc, Clause, Ref, Location)).
 
/*
metta_defined_at(Type, HintPath, Callable, Clause, Location) :-
    %matta_name_callable(NameArity, Callable),
    xref_metta_source(HintPath),
    xref_metta_defined(Type, HintPath, Callable, Clause, Path, Ref),
    path_doc(Path, Doc),
    once(metta_relative_ref_location(Doc, Callable, Ref, Location)).
*/

%! matta_name_callable(?Name:functor, ?Callable:term) is det.
%  True when, if Name = Func/Arity, Callable = Func(_, _, ...) with
%  =Arity= args.
matta_name_callable(Name/_, Callable) :- nonvar(Name), !, matta_name_callable(Name, Callable).
matta_name_callable(Name/Arity, Callable) :- integer(Arity),!,
    length(FakeArgs, Arity),
    (Callable = [Name|FakeArgs];
     Callable =.. [Name|FakeArgs]).
matta_name_callable(Name, Name).
%matta_name_callable(Name, [Name|_]):- atom(Name).

%! metta_relative_ref_location(+Path:atom, +Goal:term, +Position:position(int, int), -Location:dict) is semidet.
%  Given =Goal= found in =Path= and position =Position= (from
%  metta_called_at/3), =Location= is a dictionary suitable for sending as an
%  LSP response indicating the position in a file of =Goal=.
metta_relative_ref_location(_, _, Dict, Location) :- is_dict(Dict),!, Location=Dict.
metta_relative_ref_location(Here, Goal, '$stream_position'(A,B,C,D), Out):- !, line_col('$stream_position'(A,B,C,D), line_char(Line0,Char0)),
   metta_relative_ref_location(Here, Goal,  position(Line0, Char0), Out).
metta_relative_ref_location(Here, _, position(Line0, Char0),
                      _{uri: Here, range: _{start: _{line: Line0, character: Char0},
                                            end: _{line: Line1, character: 0}}}) :-
    !, succl(Line0, Line1).
metta_relative_ref_location(Here, _, line_char(Line0, Char0),
                      _{uri: Here, range: _{start: _{line: Line0, character: Char0},
                                            end: _{line: Line1, character: 0}}}) :-
    !, succl(Line0, Line1).
metta_relative_ref_location(Here, _, range(line_char(Line0,Char0),line_char(Line1,Char1)),
                      _{uri: Here, range: _{start: _{line: Line0, character: Char0},
                                            end: _{line: Line1, character: Char1}}}) :- !.
    
metta_relative_ref_location(Here, _, local(Line1),
                      _{uri: Here, range: _{start: _{line: Line0, character: 1},
                                            end: _{line: NextLine, character: 0}}}) :-
    !, succl(Line0, Line1), succl(Line1, NextLine).
metta_relative_ref_location(_, Goal, imported(Path), Location) :-
    path_doc(Path, ThereUri),
    xref_metta_source(Path),
    xref_metta_defined(Path, Goal, Loc),
    metta_relative_ref_location(ThereUri, Goal, Loc, Location).


