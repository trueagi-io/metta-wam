:- module(lsp_metta_checking, [check_errors/2]).
/** <module> LSP Checking
 Module for checking Prolog source files for errors and warnings.
 @author Roy Ward
*/
% :- use_module(library(apply_macros)).
% :- use_module(library(assoc), [list_to_assoc/2,
%                                get_assoc/3]).
% :- use_module(library(apply), [maplist/3]).
% :- use_module(library(debug), [debug/3]).
% :- use_module(library(lists), [member/2]).
% :- use_module(library(prolog_xref), [xref_clean/1, xref_source/1]).
% :- use_module(lsp_metta_utils, [clause_variable_positions/3]).
%
% :- dynamic message_hook/3.
% :- multifile message_hook/3.
%
% %! check_errors(+Path:atom, -Errors:List) is det.
% %
% %  =Errors= is a list of the errors in the file given by =Path=.
% %  This predicate changes the =user:message_hook/3= hook.

% will do some real error checking later
check_errors(_,[]).


