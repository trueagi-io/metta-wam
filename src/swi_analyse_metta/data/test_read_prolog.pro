

%prolog_tokens(string("eval_20(Eq,RetType,Depth,Self,['eval-for',Type,X],Res):- !,    ignore(Type=RetType),    eval_args(Eq,Type,Depth,Self,X, Res)."), PL ).

%do():-
% Str = "eval_20(Eq,RetType,Depth,Self,['eval-for',Type,X],Res):- !,    ignore(Type=RetType),    eval_args(Eq,Type,Depth,Self,X, Res).",
% prolog_parsetree(string(Str), PL ),
% write_term( PL, [] ).

/* 
 % nest 
 comment here 
  */

eval_20(Eq,RetType,Depth,Self,['eval-for',Type,X],Res):- !,    ignore(Type=RetType),    eval_args(Eq,Type,Depth,Self,X, Res).

/* 
 % nest 
 comment here 
  */

% test short 
eval_20(Eq,RetType,Depth,Self,[],Res):- !.

eval_20(Eq,RetType,Depth,Self,['eval-for',_Why,Type,X],Res):- !,
    ignore(Type=RetType),
    eval_args(Eq,Type,Depth,Self,X, Res).
eval_20(Eq,RetType,Depth,Self,['filter-atom',List,Var,Pred],Res):- !,
   call_filter_atom(Eq,RetType,Depth,Self,List,Var,Pred,Res).
