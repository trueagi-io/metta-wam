:- module(lsp_metta_handler_utils, [ safe_clause_call/1,
                                     must_succeed/1,
                                     must_succeed1/1
                                   ]).

% General Helper Predicates
% These helpers are used across different handlers.
safe_clause_call(Head):- clause(Head,Body), catch(Body,_,true).

:- meta_predicate must_succeed(0).
must_succeed(G):- call(G)*->true;fail.

:- meta_predicate must_succeed1(0).
must_succeed1((A,B)):- !, must_succeed1(A),must_succeed1(B),!.
must_succeed1(G):- call(G)->true;(throw(failed_succeed(G)),fail).
