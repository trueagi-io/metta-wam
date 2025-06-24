
% install swi-prolog extended idalct to say "mmodule"
:- use_dialect(metta).

% mmodule/N is like module/2 but exports metta functions
:- mmodule(call_prolog_module_ex_1,[
          (+)//0,    % //0 means varaible arity  0 = one or more arguments
          (-)//1,    % //1 means varaible arity  1 = one or more arguments
          exp/1]).   % /1 means exactly 1 arguement

% installl
:- use_module(library(metta_rt)).

:- add_history("!(exp 1.2)").
% `mx` means to evaluate all the arguments first
mx(exp,Arg,Res) :- Res is exp(Arg).

% `mc` means to leave all the arguments alone (thus ee was used)
% mc(exp,Arg,Res) :- ee(Arg, Val),  Res is exp(Val).


:- add_history("!(+ 1 2 32 3) ; 38").
:- add_history("!(+) ; returns 0").
 % _n is to define a variable arity plus
mx_n(+,List,Sum):- sum_list(List,Sum).

:- add_history("!(- 1 2 32 3) ; -36 ").
% this can be guess to be 1 or mmot arguments 4-1
mx_n(-,Arg,List,Res):- foldl([X,FL,TR]>>(TR is X+FL),List,0,Sum).



:- metta_eval(`

(= (am-defined-here $a)
   (you gave me $a))

`).




