% (track_load_into_file  "../../examples/VRUN_tests1.metta")
:-metta_eval(['extend-py!',mettalog]).

%;; stdlib extension
metta_type('&self','If',[->,'Bool','Atom','Atom']).

metta_defn_ES(['If','True',Then],Then).

metta_defn_ES(['If','False',Then],[]).

metta_type('&self','If',[->,'Bool','Atom','Atom','Atom']).

metta_defn_ES(
  ['If',Cond,Then,Else],
  [if,Cond,Then,Else]).

metta_defn_ES(
  ['TupleConcat',Ev1,Ev2],
  [ collapse,
    [ superpose,
      [ [ superpose,   Ev1   ],
        [ superpose,   Ev2   ]]]]).

metta_defn_ES(
  [max,Num1,Num2],
  [ 'If',
    [>,Num1,Num2], Num1,Num2]).

metta_defn_ES(
  [min,Num1,Num2],
  [ 'If',
    [<,Num1,Num2], Num1,Num2]).

metta_defn_ES(
  [abs,X],
  [ 'If',
    [<,X,0],
    [-,0,X],
    X]).

metta_type('&self',sequential,[->,'Expression','%Undefined%']).

metta_defn_ES([sequential,Num1],[superpose,Num1]).

metta_type('&self',do,[->,'Expression','%Undefined%']).

metta_defn_ES([do,Num1],[case,Num1,[]]).

metta_defn_ES(['TupleCount',[]],0).

metta_defn_ES(['TupleCount',[1]],1).

metta_defn_ES(
  ['BuildTupleCounts',TOld,C,N],
  [ let,
    T,
    [ collapse,
      [ superpose,
        [ 1,
          [superpose,TOld]]]],
    [ superpose,
      [ [ 'add-atom',
          '&self',
          [ =,
            ['TupleCount',T],
            [+,C,2]]],
        [ 'If',
          [<,C,N],
          [ 'BuildTupleCounts',
            T,
            [+,C,1],
            N]]]]]).

metta_type('&self','CountElement',[->,'Expression','Number']).

metta_defn_ES(
  ['CountElement',X],
  [ case,
    X,
    [ [ Y,1]]]).

%;;Build for count up to 100 (takes a few sec but it is worth it if space or generally collapse counts are often needed)
:-metta_eval(['BuildTupleCounts',[1],0,100]).

metta_defn_ES(
  [ 'BuildTupleCounts',
    [1], 0,100],
  [ let,
    A,
    [ collapse,
      [ superpose,
        [ 1,
          [ superpose,
            [1]]]]],
    [ superpose,
      [ [ 'add-atom',
          '&self',
          [ =,
            ['TupleCount',A],
            [+,0,2]]],
        [ 'If',
          [<,0,100],
          [ 'BuildTupleCounts',
            A,
            [+,0,1],
            100]]]]]).

metta_type('&self','CollapseCardinality',[->,'Expression','Number']).

metta_defn_ES(
  ['CollapseCardinality',Expression],
  [ 'TupleCount',
    [ collapse,
      ['CountElement',Expression]]]).

%;; Truth functions
metta_defn_ES(
  ['Truth_c2w',C],
  [ /,
    C,
    [-,1,C]]).

metta_defn_ES(
  ['Truth_w2c',W],
  [ /,
    W,
    [+,W,1]]).

metta_defn_ES(
  [ 'Truth_Deduction',
    [F1,C1],
    [F2,C2]],
  [ [*,F1,F2],
    [ *,
      [*,F1,F2],
      [*,C1,C2]]]).

metta_defn_ES(
  [ 'Truth_Abduction',
    [F1,C1],
    [F2,C2]],
  [ F2,
    [ 'Truth_w2c',
      [ *,
        [*,F1,C1],
        C2]]]).

metta_defn_ES(
  ['Truth_Induction',T1,T2],
  ['Truth_Abduction',T2,T1]).

metta_defn_ES(
  [ 'Truth_Exemplification',
    [F1,C1],
    [F2,C2]],
  [ 1.0,
    [ 'Truth_w2c',
      [ *,
        [*,F1,F2],
        [*,C1,C2]]]]).

metta_defn_ES(
  ['Truth_StructuralDeduction',T],
  [ 'Truth_Deduction',
    T,
    [1.0,0.9]]).

metta_defn_ES(
  [ 'Truth_Negation',
    [F,C]],
  [ [-,1,F],
    C]).

metta_defn_ES(
  ['Truth','StructuralDeductionNegated',T],
  [ 'Truth_Negation',
    ['Truth_StructuralDeduction',T]]).

metta_defn_ES(
  [ 'Truth_Intersection',
    [F1,C1],
    [F2,C2]],
  [ [ * ,F1,F2],
    [ * ,C1,C2]]).

metta_defn_ES(
  ['Truth_StructuralIntersection',T],
  [ 'Truth_Intersection',
    T,
    [1.0,0.9]]).

metta_defn_ES(
  ['Truth_or',A,B],
  [ -,
    1,
    [ *,
      [-,1,A],
      [-,1,B]]]).

metta_defn_ES(
  [ 'Truth_Comparison',
    [F1,C1],
    [F2,C2]],
  [ let,
    F0,
    ['Truth_or',F1,F2],
    [ [ 'If',
        [==,F0,0.0],
        0.0,
        [ /,
          [*,F1,F2],
          F0]],
      [ 'Truth_w2c',
        [ *,
          F0,
          [*,C1,C2]]]]]).

metta_defn_ES(
  [ 'Truth_Analogy',
    [F1,C1],
    [F2,C2]],
  [ [*,F1,F2],
    [ *,
      [*,C1,C2],
      F2]]).

metta_defn_ES(
  [ 'Truth_Resemblance',
    [F1,C1],
    [F2,C2]],
  [ [*,F1,F2],
    [ *,
      [*,C1,C2],
      ['Truth_or',F1,F2]]]).

metta_defn_ES(
  [ 'Truth_Union',
    [F1,C1],
    [F2,C2]],
  [ [ 'Truth_or',    F1    ,    F2    ],
    [     *     ,    C1    ,    C2    ]]).

metta_defn_ES(
  [ 'Truth_Difference',
    [F1,C1],
    [F2,C2]],
  [ [ *,
      F1,
      [-,1,F2]],
    [*,C1,C2]]).

metta_defn_ES(
  [ 'Truth_DecomposePNN',
    [F1,C1],
    [F2,C2]],
  [ let,
    Fn,
    [ *,
      F1,
      [-,1,F2]],
    [ [-,1,Fn],
      [ *,
        Fn,
        [*,C1,C2]]]]).

metta_defn_ES(
  [ 'Truth_DecomposeNPP',
    [F1,C1],
    [F2,C2]],
  [ let,
    F,
    [ *,
      [-,1,F1],
      F2],
    [ F,
      [ *,
        F,
        [*,C1,C2]]]]).

metta_defn_ES(
  [ 'Truth_DecomposePNP',
    [F1,C1],
    [F2,C2]],
  [ let,
    F,
    [ *,
      F1,
      [-,1,F2]],
    [ F,
      [ *,
        F,
        [*,C1,C2]]]]).

metta_defn_ES(
  ['Truth_DecomposePPP',V1,V2],
  [ 'Truth_DecomposeNPP',
    ['Truth_Negation',V1],
    V2]).

metta_defn_ES(
  [ 'Truth_DecomposeNNN',
    [F1,C1],
    [F2,C2]],
  [ let,
    Fn,
    [ *,
      [-,1,F1],
      [-,1,F2]],
    [ [-,1,Fn],
      [ *,
        Fn,
        [*,C1,C2]]]]).

metta_defn_ES(
  [ 'Truth_Eternalize',
    [F,C]],
  [ F,
    ['Truth_w2c',C]]).

metta_defn_ES(
  [ 'Truth_Revision',
    [F1,C1],
    [F2,C2]],
  [ 'let*',
    [ [ W1,
        ['Truth_c2w',C1]],
      [ W2,
        ['Truth_c2w',C2]],
      [ W,
        [+,W1,W2]],
      [ F,
        [ /,
          [ +,
            [*,W1,F1],
            [*,W2,F2]],
          W]],
      [ C,
        ['Truth_w2c',W]]],
    [ [min,1.0,F],
      [ min,
        0.99,
        [ max,
          [max,C,C1],
          C2]]]]).

metta_defn_ES(
  [ 'Truth_Expectation',
    [F,C]],
  [ +,
    [ *,
      C,
      [-,F,0.5]],
    0.5]).
% ;;NAL-1
% ;;!Syllogistic rules for Inheritance:
metta_defn_ES(['|-',[[A,-->,B],C],[[B,-->,D],E]] ,  [[A,-->,D],['Truth_Deduction',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[D,-->,B],['Truth_Induction',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[D,-->,A],['Truth_Abduction',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[B,-->,D],E]] ,  [[D,-->,A],['Truth_Exemplification',C,E]]).
% ;;NAL-2
% ;;!Rules for Similarity:
metta_defn_ES(['|-',[[A,<->,B],C]] ,  [[B,<->,A],['Truth_StructuralIntersection',C]]).
metta_defn_ES(['|-',[[A,<->,B],C],[[D,<->,A],E]] ,  [[D,<->,B],['Truth_Resemblance',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[D,<->,A],['Truth_Comparison',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[D,<->,B],['Truth_Comparison',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,<->,A],E]] ,  [[D,-->,B],['Truth_Analogy',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,<->,B],E]] ,  [[A,-->,D],['Truth_Analogy',C,E]]).
% ;;!Dealing with properties and instances:
metta_defn_ES(['|-',[[A,-->,['{',B,'}']],C]] ,  [[A,<->,['{',B,'}']],['Truth_StructuralIntersection',C]]).
metta_defn_ES(['|-',[[['$OBJ'(claz_bracket_vector,['$S'])],-->,A],B]] ,  [[['$OBJ'(claz_bracket_vector,['$S'])],<->,A],['Truth_StructuralIntersection',B]]).
metta_defn_ES(['|-',[[['{',A,'}'],-->,B],C],[[D,<->,A],E]] ,  [[['{',D,'}'],-->,B],['Truth_Analogy',C,E]]).
metta_defn_ES(['|-',[[A,-->,['$OBJ'(claz_bracket_vector,['$M'])]],B],[[_,<->,_],C]] ,  [[A,-->,['$OBJ'(claz_bracket_vector,['$S'])]],['Truth_Analogy',B,C]]).
get_metta_atom(Eq,'&self',[=,['|-',[[['{',A,'}'],<->,['{',B,'}']]],[A,<->,B],['Truth_StructuralIntersection',_]]]).
get_metta_atom(Eq,'&self',[=,['|-',[[['$OBJ'(claz_bracket_vector,[A])],<->,['$OBJ'(claz_bracket_vector,[B])]]],[_,<->,_],['Truth_StructuralIntersection',_]]]).
% ;;NAL-3
% ;;!Set decomposition:
metta_defn_ES(['|-',[[['{',A,_,'}'],-->,B],C]] ,  [[['{',A,'}'],-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[['{',_,A,'}'],-->,B],C]] ,  [[['{',A,'}'],-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[['M',-->,['$OBJ'(claz_bracket_vector,[A,B])]],A]] ,  [['M',-->,['$OBJ'(claz_bracket_vector,[A])]],['Truth_StructuralDeduction',A]]).
metta_defn_ES(['|-',[['M',-->,['$OBJ'(claz_bracket_vector,[A,B])]],A]] ,  [['M',-->,['$OBJ'(claz_bracket_vector,[B])]],['Truth_StructuralDeduction',A]]).
% ;;!Extensional and intensional intersection decomposition:
metta_defn_ES(['|-',[[[A,'|',_],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[A,-->,[B,&,_]],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[[_,'|',A],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[A,-->,[_,&,B]],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[[A,~,_],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[A,-->,[B,-,_]],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn_ES(['|-',[[[_,~,A],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeductionNegated',C]]).
metta_defn_ES(['|-',[[A,-->,[_,-,B]],C]] ,  [[A,-->,B],['Truth_StructuralDeductionNegated',C]]).
% ;;!Extensional and intensional intersection composition: (sets via reductions).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[[A,'|',D],-->,B],['Truth_Intersection',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[[A,&,D],-->,B],['Truth_Union',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[[A,~,D],-->,B],['Truth_Difference',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[A,-->,[B,&,D]],['Truth_Intersection',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[A,-->,[B,'|',D]],['Truth_Union',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[A,-->,[B,-,D]],['Truth_Difference',C,E]]).
% ;;!Extensional and intensional intersection decomposition:
metta_defn_ES(['|-',[[A,-->,B],C],[[[A,'|',D],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposePNN',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[[D,'|',A],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposePNN',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[[A,&,D],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposeNPP',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[[D,&,A],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposeNPP',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[[A,~,D],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposePNP',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[[D,~,A],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposeNNN',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,[B,&,D]],E]] ,  [[A,-->,D],['Truth_DecomposePNN',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,[D,&,B]],E]] ,  [[A,-->,D],['Truth_DecomposePNN',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,[B,'|',D]],E]] ,  [[A,-->,D],['Truth_DecomposeNPP',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,[D,'|',B]],E]] ,  [[A,-->,D],['Truth_DecomposeNPP',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,[B,-,D]],E]] ,  [[A,-->,D],['Truth_DecomposePNP',C,E]]).
metta_defn_ES(['|-',[[A,-->,B],C],[[A,-->,[D,-,B]],E]] ,  [[A,-->,D],['Truth_DecomposeNNN',C,E]]).
% ;; NAL-4
% ;;!Transformation rules between product and image:
metta_defn_ES(['|-',[[[A,*,B],-->,C],D]] ,  [[A,-->,[C,'/1',B]],['Truth_StructuralIntersection',D]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D]] ,  [[B,-->,[C,'/2',A]],['Truth_StructuralIntersection',D]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D]] ,  [[[A,'\\1',C],-->,B],['Truth_StructuralIntersection',D]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D]] ,  [[[A,'\\2',B],-->,C],['Truth_StructuralIntersection',D]]).
% ;;other direction of same rules (as these are bi-directional).
metta_defn_ES(['|-',[[A,-->,[B,'/1',C]],D]] ,  [[[A,*,C],-->,B],['Truth_StructuralIntersection',D]]).
metta_defn_ES(['|-',[[A,-->,[B,'/2',C]],D]] ,  [[[C,*,A],-->,B],['Truth_StructuralIntersection',D]]).
metta_defn_ES(['|-',[[[A,'\\1',B],-->,C],D]] ,  [[A,-->,[C,*,B]],['Truth_StructuralIntersection',D]]).
metta_defn_ES(['|-',[[[A,'\\2',B],-->,C],D]] ,  [[A,-->,[B,*,C]],['Truth_StructuralIntersection',D]]).
% ;;!Comparative relations
metta_defn_ES(['|-',[[['{',A,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],B],[[['{',C,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],D]] ,  [[[['{',A,'}'],*,['{',C,'}']],-->,[>>>,_]],['Truth_FrequencyGreater',B,D]]).
metta_defn_ES(['|-',[[[A,*,B],-->,[>>>,C]],D],[[[B,*,E],-->,[>>>,C]],F]] ,  [[[A,*,E],-->,[>>>,C]],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[['{',A,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],B],[[['{',C,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],D]] ,  [[[['{',A,'}'],*,['{',C,'}']],-->,[===,_]],['Truth_FrequencyEqual',B,D]]).
metta_defn_ES(['|-',[[[A,*,B],-->,[===,C]],D],[[[B,*,E],-->,[===,C]],F]] ,  [[[A,*,E],-->,[===,C]],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,[===,C]],D]] ,  [[[B,*,A],-->,[===,C]],['Truth_StructuralIntersection',D]]).
% ;;!Optional rules for more efficient reasoning about relation components:
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[[E,*,B],-->,C],F]] ,  [[E,-->,A],['Truth_Abduction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[[A,*,E],-->,C],F]] ,  [[E,-->,B],['Truth_Abduction',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[A,-->,[E,*,C]],F]] ,  [[E,-->,B],['Truth_Induction',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[A,-->,[B,*,E]],F]] ,  [[E,-->,C],['Truth_Induction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[E,-->,A],F]] ,  [[[E,*,B],-->,C],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[A,-->,E],F]] ,  [[[E,*,B],-->,C],['Truth_Induction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[E,<->,A],F]] ,  [[[E,*,B],-->,C],['Truth_Analogy',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[E,-->,B],F]] ,  [[[A,*,E],-->,C],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[B,-->,E],F]] ,  [[[A,*,E],-->,C],['Truth_Induction',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[E,<->,B],F]] ,  [[[A,*,E],-->,C],['Truth_Analogy',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[B,-->,E],F]] ,  [[A,-->,[E,*,C]],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[E,-->,B],F]] ,  [[A,-->,[E,*,C]],['Truth_Abduction',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[E,<->,B],F]] ,  [[A,-->,[E,*,C]],['Truth_Analogy',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[C,-->,E],F]] ,  [[A,-->,[B,*,E]],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[E,-->,C],F]] ,  [[A,-->,[B,*,E]],['Truth_Abduction',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[E,<->,C],F]] ,  [[A,-->,[B,*,E]],['Truth_Analogy',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[[E,*,B],-->,C],F]] ,  [[A,<->,E],['Truth_Comparison',D,F]]).
metta_defn_ES(['|-',[[[A,*,B],-->,C],D],[[[A,*,E],-->,C],F]] ,  [[B,<->,E],['Truth_Comparison',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[A,-->,[E,*,C]],F]] ,  [[B,<->,E],['Truth_Comparison',D,F]]).
metta_defn_ES(['|-',[[A,-->,[B,*,C]],D],[[A,-->,[B,*,E]],F]] ,  [[C,<->,E],['Truth_Comparison',D,F]]).
% ;;NAL-5
% ;;!Negation conjunction and disjunction decomposition:
metta_defn_ES(['|-',[[!,A],B]],[A,['Truth_Negation',B]]).
metta_defn_ES(['|-',[[A,&&,_],B]],[A,['Truth_StructuralDeduction',B]]).
metta_defn_ES(['|-',[[_,&&,A],B]],[A,['Truth_StructuralDeduction',B]]).
metta_defn_ES(['|-',[[A,&&,B],C]] ,  [[B,&&,A],['Truth_StructuralIntersection',C]]).
metta_defn_ES(['|-',[A,B],[[A,&&,C],D]],[C,['Truth_DecomposePNN',B,D]]).
metta_defn_ES(['|-',[A,B],[[A,'||',C],D]],[C,['Truth_DecomposeNPP',B,D]]).
metta_defn_ES(['|-',[A,B],[[[!,A],&&,C],D]],[C,['Truth_DecomposeNNN',B,D]]).
metta_defn_ES(['|-',[A,B],[[[!,A],'||',C],D]],[C,['Truth_DecomposePPP',B,D]]).
% ;;!Syllogistic rules for Implication:
metta_defn_ES(['|-',[[A,==>,B],C],[[B,==>,D],E]] ,  [[A,==>,D],['Truth_Deduction',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[D,==>,B],['Truth_Induction',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[D,==>,A],['Truth_Abduction',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[B,==>,D],E]] ,  [[D,==>,A],['Truth_Exemplification',C,E]]).
% ;;!Conditional composition for conjunction and disjunction:
metta_defn_ES(['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[[A,&&,D],==>,B],['Truth_Union',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[[A,'||',D],==>,B],['Truth_Intersection',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[A,==>,[B,&&,D]],['Truth_Intersection',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[A,==>,[B,'||',D]],['Truth_Union',C,E]]).
% ;;!Multi-conditional inference:
metta_defn_ES(['|-',[[[A,&&,B],==>,C],D],[[A,==>,C],E]],[B,['Truth_Abduction',D,E]]).
metta_defn_ES(['|-',[[[A,&&,B],==>,C],D],[[E,==>,B],F]] ,  [[[A,&&,E],==>,C],['Truth_Deduction',D,F]]).
metta_defn_ES(['|-',[[[A,&&,B],==>,C],D],[[[A,&&,E],==>,C],F]] ,  [[E,==>,B],['Truth_Abduction',D,F]]).
metta_defn_ES(['|-',[[[A,&&,B],==>,C],D],[[B,==>,E],F]] ,  [[[A,&&,E],==>,C],['Truth_Induction',D,F]]).
% ;;!Rules for equivalence:
metta_defn_ES(['|-',[[A,<=>,B],C]] ,  [[B,<=>,A],['Truth_StructuralIntersection',C]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[B,==>,A],D]] ,  [[A,<=>,B],['Truth_Intersection',C,D]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[D,<=>,A],['Truth_Comparison',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[D,<=>,B],['Truth_Comparison',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[D,<=>,A],E]] ,  [[D,==>,B],['Truth_Analogy',C,E]]).
metta_defn_ES(['|-',[[A,==>,B],C],[[D,<=>,B],E]] ,  [[A,==>,D],['Truth_Analogy',C,E]]).
metta_defn_ES(['|-',[[A,<=>,B],C],[[D,<=>,A],E]] ,  [[D,<=>,B],['Truth_Resemblance',C,E]]).
% ;;!Higher-order decomposition
metta_defn_ES(['|-',[A,B],[[A,==>,C],D]],[C,['Truth_Deduction',B,D]]).
metta_defn_ES(['|-',[A,B],[[[A,&&,C],==>,D],E]] ,  [[C,==>,D],['Truth_Deduction',B,E]]).
metta_defn_ES(['|-',[A,B],[[C,==>,A],D]],[C,['Truth_Abduction',B,D]]).
metta_defn_ES(['|-',[A,B],[[A,<=>,C],D]],[C,['Truth_Analogy',B,D]]).
% ;;NAL term reductions
% ;;!Extensional intersection, union, conjunction reductions:
metta_defn_ES([A,&,A],A).
metta_defn_ES([A,'|',A],A).
metta_defn_ES([A,&&,A],A).
metta_defn_ES([A,'||',A],A).
% ;;!Extensional set reductions:
metta_defn_ES([['{',A,'}'],'|',['{',B,'}']],['{',A,B,'}']).
metta_defn_ES([['{',A,B,'}'],'|',['{',C,'}']],['{',[A|B],C,'}']).
metta_defn_ES([['{',A,'}'],'|',['{',B,C,'}']],['{',A,[B|C],'}']).
% ;;!Intensional set reductions:
metta_defn_ES([['$OBJ'(claz_bracket_vector,[A])],&,['$OBJ'(claz_bracket_vector,[B])]],['$OBJ'(claz_bracket_vector,[A,B])]).
metta_defn_ES([['$OBJ'(claz_bracket_vector,[A,B])],&,['$OBJ'(claz_bracket_vector,[C])]],['$OBJ'(claz_bracket_vector,[[A|B],C])]).
metta_defn_ES([['$OBJ'(claz_bracket_vector,[A])],&,['$OBJ'(claz_bracket_vector,[B,C])]],['$OBJ'(claz_bracket_vector,[A,[B|C]])]).
% ;;!Reduction for set element copula:
metta_defn_ES(['{',[A|B],'}'],['{',A,B,'}']).
metta_defn_ES(['$OBJ'(claz_bracket_vector,[[A|B]])],['$OBJ'(claz_bracket_vector,[A,B])]).

%;params
metta_defn_ES(['BeliefEventsMax'],10).

metta_defn_ES(['GoalEventsMax'],10).

%;spaces
:-metta_eval(['bind!','&belief_events',['new-space']]).

:-metta_eval(['bind!','&goal_events',['new-space']]).

%;states
:-metta_eval(['bind!','&currentTime',['new-state',1]]).

:-metta_eval(['bind!','&evidentialBase',['new-state',1]]).

metta_defn_ES(
  [increment,Atom],
  [ 'change-state!',
    Atom,
    [ +,
      1,
      ['get-state',Atom]]]).

metta_defn_ES(
  ['UpdateReasonerState'],
  [ [     increment    , '&currentTime'  ],
    [     increment    ,'&evidentialBase']]).

metta_defn_ES(
  ['GetReasonerState'],
  [ ['get-state','&currentTime'],
    [ [    'get-state'   ,'&evidentialBase']]]).

%;priority of events
metta_defn_ES(
  [ 'EventPriorityNow',
    [T,P],
    T],
  [ *,
    P,
    [ /,
      1,
      [ +,
        1,
        [-,T,T]]]]).

%;retrieve the best candidate (allows to use tuples / collapse results / spaces as a PQ)
:-metta_eval(['bind!','&tempbest',['new-state',[]]]).

:-metta_eval(['bind!','&tempbestscore',['new-state',0]]).

metta_defn_ES(
  ['BestCandidate',Tuple,EvaluateCandidateFunction,T],
  [ sequential,
    [ [ do,
        ['change-state!','&tempbestscore',0]],
      [ do,
        ['change-state!','&tempbest',[]]],
      [ do,
        [ 'let*',
          [ [ X,
              [superpose,Tuple]],
            [ Fx,
              [EvaluateCandidateFunction,X,T]]],
          [ superpose,
            [ [ 'If',
                [ >,
                  Fx,
                  ['get-state','&tempbestscore']],
                [ sequential,
                  [ [ 'change-state!' ,  '&tempbest'   ,       X        ],
                    [ 'change-state!' ,'&tempbestscore',       Fx       ]]]]]]]],
      ['get-state','&tempbest']]]).

%;functions to select highest-priority events in belief and goal PQ
metta_defn_ES(
  [ 'PriorityOf',
    [ 'Event',
      Sentence,
      [OccT,Ev,Prio]],
    T],
  ['EventPriorityNow',Prio,T]).

metta_defn_ES(
  ['SelectHighestPriorityEvent',Collection,T],
  [ 'BestCandidate',
    [ collapse,
      ['get-atoms',Collection]], 'PriorityOf',T]).

%;a belief event to process, which demands adding it to the PQ and updating its concept
metta_defn_ES(
  ['ProcessBeliefEvent',Ev,T],
  [ sequential,
    [ [    'add-atom'   ,'&belief_events',       Ev       ],
      [ 'UpdateConcept' ,       Ev       ,       T        ]]]).

%;bound the size of the attentional focus for tasks / events
metta_defn_ES(
  [ 'BoundEvents', Collection,Threshold,
       Increment, TargetAmount, T],
  [ sequential,
    [ [ do,
        [ 'let*',
          [ [ Ev,
              ['get-atoms',Collection]],
            [ [ 'Event',
                Sentence,
                [Time,Evidence,EPrio]],
              Ev]],
          [ 'If',
            [ <,
              ['EventPriorityNow',EPrio,T],
              Threshold],
            ['remove-atom',Collection,Ev]]]],
      [ let,
        CurrentAmount,
        [ 'CollapseCardinality',
          ['get-atoms',Collection]],
        [ 'If',
          [>,CurrentAmount,TargetAmount],
          [ 'BoundEvents',
            Collection,
            [+,Threshold,Increment],            Increment, TargetAmount, T]]]]]).

%;params
metta_defn_ES(['AttentionalFocusConceptsMax'],10).

%;spaces
:-metta_eval(['bind!','&concepts',['new-space']]).

:-metta_eval(['bind!','&attentional_focus',['new-space']]).

%;priority of concepts
metta_defn_ES(
  [ 'ConceptPriorityNow',
    [T,P],
    T],
  [ *,
    P,
    [ /,
      1,
      [ +,
        1,
        [-,T,T]]]]).

%;whether evidence was just counted once
:-metta_eval(['bind!','&tempstate',['new-state','False']]).

:-metta_eval(['bind!','&tempset',['new-space']]).

metta_defn_ES(
  ['StampDisjoint',X],
  [ not,
    [ sequential,
      [ [ do,
          ['change-state!','&tempstate','False']],
        [ do,
          [ case,
            ['get-atoms','&tempset'],
            [ [ Y,
                ['remove-atom','&tempset',Y]]]]],
        [ do,
          [ let,
            Z,
            [superpose,X],
            [ case,
              [match,'&tempset',Z,Z],
              [ [ W,
                  ['change-state!','&tempstate','True']],
                [ '%void%',
                  ['add-atom','&tempset',Z]]]]]],
        ['get-state','&tempstate']]]]).

%;revise if there is no evidential overlap, else use higher-confident candidate
metta_defn_ES(
  [ 'RevisionAndChoice',
    [ 'Event',
      [ Term1,
        [F1,C1]],
      [eternal,Ev1,EPrio1]],
    [ 'Event',
      [ Term2,
        [F2,C2]],
      [eternal,Ev2,EPrio2]]],
  [ let,
    ConclusionStamp,
    ['TupleConcat',Ev1,Ev2],
    [ 'If',
      ['StampDisjoint',ConclusionStamp],
      [ 'Event',
        [ Term1,
          [ 'Truth_Revision',
            [F1,C1],
            [F2,C2]]],
        [ eternal,
          ConclusionStamp,
          [0,0.0]]],
      [ 'If',
        [>,C1,C2],
        [ 'Event',
          [ Term1,
            [F1,C1]],
          [ eternal,
            Ev1,
            [0,0.0]]],
        [ 'Event',
          [ Term2,
            [F2,C2]],
          [ eternal,
            Ev2,
            [0,0.0]]]]]]).

%;;update beliefs in existing concept with the new event or create new concept to enter the new evidence
metta_defn_ES(
  ['UpdateConcept',NewEvent,T],
  [ 'let*',
    [ [ [ 'Event',
          [Term,TV],
          [Time,Evidence,EPrio]],
        NewEvent],
      [ NewEventEternalized,
        ['Eternalize',NewEvent]],
      [ MatchConcept,
        [ 'Concept', Term,Belief,
          BeliefEvent,CPrio]]],
    [ sequential,
      [ [ case,
          [match,'&attentional_focus',MatchConcept,MatchConcept],
          [ [ MatchConcept,
              [ sequential,
                [ ['remove-atom','&attentional_focus',MatchConcept],
                  [ 'let*',
                    [ [ RevisedBelief,
                        ['RevisionAndChoice',Belief,NewEventEternalized]],
                      [ MaxPrio,
                        [ 'If',
                          [ >,
                            ['EventPriorityNow',EPrio,T],
                            ['ConceptPriorityNow',CPrio,T]], EPrio,CPrio]]],
                    [ 'add-atom',
                      '&attentional_focus',
                      [ 'Concept',            Term, RevisedBelief, NewEvent, MaxPrio]]]]]],
            [ '%void%',
              [ case,
                [match,'&concepts',MatchConcept,MatchConcept],
                [ [ MatchConcept,
                    [ sequential,
                      [ [    'remove-atom'    ,    '&concepts'     ,    MatchConcept    ],
                        [      'add-atom'     ,'&attentional_focus',    MatchConcept    ],
                        [   'UpdateConcept'   ,      NewEvent      ,         T          ]]]],
                  [ '%void%',
                    [ 'add-atom',
                      '&attentional_focus',
                      [ 'Concept',                Term, NewEventEternalized, NewEvent, EPrio]]]]]]]]]]]).

%;bound the size of attentional focus of concepts
metta_defn_ES(
  [ 'BoundAttention', Threshold,Increment,
    TargetAmount,T],
  [ sequential,
    [ [ do,
        [ 'let*',
          [ [ C,
              ['get-atoms','&attentional_focus']],
            [ [ 'Concept',
                Term,
                ['Event',Sentence,Metadata], BeliefEvent,CPrio],
              C]],
          [ 'If',
            [ <,
              ['ConceptPriorityNow',CPrio,T],
              Threshold],
            [ sequential,
              [ [    'remove-atom'    ,'&attentional_focus',         C          ],
                [      'add-atom'     ,    '&concepts'     ,         C          ]]]]]],
      [ let,
        CurrentAmount,
        [ 'CollapseCardinality',
          ['get-atoms','&attentional_focus']],
        [ 'If',
          [>,CurrentAmount,TargetAmount],
          [ 'BoundAttention',
            [+,Threshold,Increment],            Increment, TargetAmount, T]]]]]).

%;get eternal belief of concept
metta_type('&self','EternalQuestion',[->,'Expression',T]).

metta_defn_ES(
  ['EternalQuestion',Term],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept', Term,Belief,
        BeliefEvent,CPrio],
      Belief],
    [ [Ev,Ev],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [eternal,[],0.0]]]]]).

%;get event belief of concept
metta_type('&self','EventQuestion',[->,'Expression',T]).

metta_defn_ES(
  ['EventQuestion',Term],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept', Term,Belief,
        BeliefEvent,CPrio],
      BeliefEvent],
    [ [Ev,Ev],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [0,[],0.0]]]]]).

%;;Declarative inference (deriving events and knowledge from observed events)
%;Derived belief event priority
metta_defn_ES(
  ['ConclusionPriority',EPrio,CPrio,ConcTV],
  [ *,
    [*,EPrio,CPrio],
    ['Truth_Expectation',ConcTV]]).

%;making declarative inferences on two events (task from PQ and belief from concept)
metta_defn_ES(
  [ 'Conclude',
    [ 'Event',
      S1,
      [Time1,Ev1,Prio1]],
    [ 'Event',
      S2,
      [Time2,Ev2,Prio2]], CPrio,T],
  [ let,
    ConclusionStamp,
    ['TupleConcat',Ev1,Ev2],
    [ 'If',
      ['StampDisjoint',ConclusionStamp],
      [ let,
        [ConcTerm,ConcTV],
        [ superpose,
          [ [ '|-', S1 , S2 ],
            [ '|-', S2 , S1 ]]],
        [ 'Event',
          [ConcTerm,ConcTV],
          [ Time1,
            ConclusionStamp,
            [ T,
              [ 'ConclusionPriority',
                ['EventPriorityNow',Prio1,T],
                ['ConceptPriorityNow',CPrio,T],
                ConcTV]]]]]]]).

%;find a belief for the task to generate conclusions with
metta_defn_ES(
  [ 'ReasonWithTask',
    [ 'Event',
      S1,
      [Time1,Ev1,Prio1]],
    T],
  [ let,
    [Belief,CPrio],
    [ case,
      ['get-atoms','&attentional_focus'],
      [ [ [ 'Concept',
            Term,
            [ 'Event',
              SE2,
              [TimeE2,EvE2,PrioE2]],
            [ 'Event',
              S2,
              [Time2,Ev2,Prio2]],
            CPrio],
          [ 'If',
            [ and,
              [ not,
                [==,Time1,eternal]],
              [ >,
                [ abs,
                  [-,Time1,Time2]],
                20]],
            [ [ 'Event',
                SE2,
                [TimeE2,EvE2,PrioE2]],
              Cprio],
            [ [ 'Event',
                S2,
                [Time2,Ev2,Prio2]],
              CPrio]]]]],
    [ case,
      [ 'Conclude',
        [ 'Event',
          S1,
          [Time1,Ev1,Prio1]],
        ['TemporallyAlignedBelief',Time1,Belief], CPrio,T],
      [ [ ['Event',Num1,Num2],
          [ 'ProcessBeliefEvent',
            ['Event',Num1,Num2],
            T]]]]]).

%;select the highest priority belief event from the PQ and use it for reasoning
metta_defn_ES(
  ['BeliefCycle',T],
  [ do,
    [ sequential,
      [ [ let,
          Ev,
          ['SelectHighestPriorityEvent','&belief_events',T],
          [ sequential,
            [ [  'remove-atom'  ,'&belief_events',       Ev       ],
              [ 'ReasonWithTask',       Ev       ,       T        ]]]],
        ['UpdateReasonerState'],
        [ 'BoundEvents', '&belief_events',0.0,0.1,
          ['BeliefEventsMax'],
          T],
        [ 'BoundAttention', 0.0,0.1,
          ['AttentionalFocusConceptsMax'],
          T]]]]).

%;;Temporal inference (sequence and implication formation based on FIFO)
%;use the event's evidence to induce a time-independent belief which can be used in the future
metta_defn_ES(
  ['Eternalize',Ev],
  [ let,
    [ 'Event',
      [Term,TV],
      [Time,Evidence,EPrio]],
    Ev,
    [ 'If',
      [==,Time,eternal],
      Ev,
      [ 'Event',
        [ Term,
          ['Truth_Eternalize',TV]],
        [ eternal,
          Evidence,
          [0,0.0]]]]]).

%;use evidence of an event at a slightly different moment in time
metta_defn_ES(
  [ 'Projection',
    [ 'Event',
      [ Term,
        [F,C]],
      [Time,Evidence,EPrio]],
    TargetTime],
  [ 'Event',
    [ Term,
      [ F,
        [ *,
          C,
          [ min,
            1,
            [ /,
              1,
              [ abs,
                [-,Time,TargetTime]]]]]]],
    [TargetTime,Evidence,EPrio]]).

%;make the belief occurrence time compatible with the task's
metta_defn_ES(
  ['TemporallyAlignedBelief',TaskTime,Belief],
  [ 'If',
    [==,TaskTime,eternal],
    ['Eternalize',Belief],
    ['Projection',Belief,TaskTime]]).

%;FIFO max. size bound
:-metta_eval(['bind!','&FIFO',['new-state',[]]]).

metta_defn_ES(['ListFirstK',C,[]],[]).

metta_defn_ES(
  [ 'ListFirstK',
    C,
    [LH,LT]],
  [ 'If',
    [>,C,0],
    [ LH,
      [ 'ListFirstK',
        [-,C,1],
        LT]],
    []]).

%;Add event to FIFO
metta_defn_ES(
  ['EventToFIFO',Ev],
  [ let,
    Newlist,
    [ 'ListFirstK',
      3,
      [ Ev,
        ['get-state','&FIFO']]],
    ['change-state!','&FIFO',Newlist]]).

%;Form a sequence of two events
metta_defn_ES(
  [ 'TemporalSequence',
    Ev1,
    [ 'Event',
      [Term2,Truth2],
      [Time2,Evidence2,EPrio2]]],
  [ let,
    [ 'Event',
      [Term1,Truth1],
      [Time1,Evidence1,EPrio1]],
    ['Projection',Ev1,Time2],
    [ 'Event',
      [ [        Term1        ,         &/         ,       Term2        ],
        [ 'Truth_Intersection',       Truth1       ,       Truth2       ]],
      [ Time2,
        ['TupleConcat',Evidence1,Evidence2],
        [0,0.0]]]]).

%;Form a temporal implication between two events
metta_defn_ES(
  [ 'TemporalImplication',
    Ev1,
    [ 'Event',
      [Term2,Truth2],
      [Time2,Evidence2,EPrio2]]],
  [ let,
    [ 'Event',
      [Term1,Truth1],
      [Time1,Evidence1,EPrio1]],
    ['Projection',Ev1,Time2],
    [ 'Event',
      [ [       Term1      ,       =/>       ,      Term2      ],
        [ 'Truth_Induction',     Truth1      ,     Truth2      ]],
      [ Time2,
        ['TupleConcat',Evidence1,Evidence2],
        [0,0.0]]]]).

%;Whether an event's term is an operation
metta_defn_ES(
  [ 'IsOp',
    [ 'Event',
      [Term,Truth],
      Metadata]],
  [ case,
    Term,
    [ [ [^,Opname],
        'True'],
      [Otherwise,'False']]]).

%;Find implications in the event FIFO:
%;procedural implications
metta_defn_ES(
  [ 'TemporalImplicationInduction',
    [ Cons,
      [ Op,
        [Prec,Tail]]]],
  [ 'If',
    [ and,
      ['IsOp',Op],
      [ and,
        [ not,
          ['IsOp',Cons]],
        [ not,
          ['IsOp',Prec]]]],
    [ let,
      PrecOp,
      ['TemporalSequence',Prec,Op],
      ['TemporalImplication',PrecOp,Cons]]]).

%;and temporal without operation
metta_defn_ES(
  [ 'TemporalImplicationInduction',
    [ Cons,
      [Prec,Tail]]],
  [ 'If',
    [ and,
      [ not,
        ['IsOp',Prec]],
      [ not,
        ['IsOp',Cons]]],
    ['TemporalImplication',Prec,Cons]]).

%;Add negative evidence for implications which predicted the input unsuccessfully
metta_defn_ES(
  ['NegConfirmation',PrecTerm,ObservedCons,T],
  [ let,
    [ 'Event',
      [ [PrecTerm,=/>,PredictedCons],
        ImpTV],
      ImpMetadata],
    [ 'EternalQuestion',
      [PrecTerm,=/>,PredictedCons]],
    [ 'If',
      [ not,
        [==,ObservedCons,PredictedCons]],
      [ 'UpdateConcept',
        [ 'Event',
          [ [   PrecTerm   ,     =/>     ,PredictedCons],
            [      0.0     ,     0.1     ]],
          [ T,
            [],
            [0,0.0]]],
        T]]]).

%;Check if the implication's preconditions are met to anticipate the by the implication predicted outcome
get_metta_atom(Eq, '&self', [
  =,
  [ 'Anticipate',
    [Pos,[]],
    T]]).

metta_defn_ES(
  [ 'Anticipate',
    [ Pos,
      [Pre,[]]],
    T],
  [ 'let*',
    [ [ [ 'Event',
          [PreTerm,PreTV],
          PreMetadata],
        Pre],
      [ [ 'Event',
          [PosTerm,PosTV],
          PosMetadata],
        Pos]],
    [ 'If',
      [ not,
        ['IsOp',Pre]],
      ['NegConfirmation',PreTerm,PosTerm,T]]]).

metta_defn_ES(
  [ 'Anticipate',
    [ Pos,
      [ Op,
        [Pre,Trail]]],
    T],
  [ 'let*',
    [ [ [ 'Event',
          [PreTerm,PreTV],
          PreMetadata],
        Pre],
      [ [ 'Event',
          [OpTerm,OpTV],
          OpMetadata],
        Op],
      [ [ 'Event',
          [PosTerm,PosTV],
          PosMetadata],
        Pos],
      [ Sequence,
        [Pre,&/,'Pos']]],
    [ 'If',
      [ and,
        ['IsOp',Op],
        [ not,
          ['IsOp',Pre]]],
      [ 'NegConfirmation',
        [PreTerm,&/,OpTerm], PosTerm,T]]]).

%;;Input procedure
metta_defn_ES(
  ['AddBeliefEvent',Sentence],
  [ 'let*',
    [ [ [         T         ,  EvidentialBase  ],
        [ 'GetReasonerState']],
      [ InputEvent,
        [ 'Event',
          Sentence,
          [ T,
            EvidentialBase,
            [T,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',InputEvent],
          [ let,
            InducedHypothesis,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',InducedHypothesis,T]],
          ['ProcessBeliefEvent',InputEvent,T],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            T],
          ['BeliefCycle',T]]]]]).

%;;Procedural inference (decision making with operation execution and subgoaling)
%;Derived goal event priority
metta_defn_ES(
  ['SubgoalPriority',EPrio,ConcTV],
  [ *,
    EPrio,
    ['Truth_Expectation',ConcTV]]).

%;Expectation of an operation is the truth expectation of its desire value
metta_defn_ES(
  [ 'OpExpectation',
    [ 'Decision',
      [Opname,DVOp],
      Subgoal],
    T],
  ['Truth_Expectation',DVOp]).

%;Inject executed operation as an event and return its name
metta_defn_ES(
  ['Execute',Opname],
  [ superpose,
    [ [ 'AddBeliefEvent',
        [ Opname,
          [1.0,0.9]]],
      Opname]]).

%;Add subgoals to the PQ
metta_defn_ES(
  ['DeriveSubgoals',Options],
  [ do,
    [ let,
      ['Decision',Op,Subgoal],
      [superpose,Options],
      ['add-atom','&goal_events',Subgoal]]]).

%;execute the operation which most likely gets the goal achieved in current contexts, and if contexts are not yet fulfilled, derive them as subgoals
metta_defn_ES(
  [ 'BestDecision',
    T,
    [ 'Event',
      [Term,DV],
      [GoalTime,GoalEvBase,GoalPrio]],
    FIFO],
  [ let,
    Options,
    [ collapse,
      [ 'let*',
        [ [ [ 'Event',
              [ [ [ Prec,
                    &/,
                    [^,Op]], =/>,Term],
                ImpTV],
              [ImpTime,ImpEvBase,ImpPrio]],
            [ 'EternalQuestion',
              [ [ Prec,
                  &/,
                  [^,Op]], =/>,Term]]],
          [ DVPrecOp,
            ['Truth_Deduction',DV,ImpTV]],
          [ [ 'Event',
              [PrecTerm,PrecTV],
              PrecMetadata],
            [ 'Projection',
              ['EventQuestion',Prec],
              T]],
          [ DVOp,
            ['Truth_Deduction',PrecTV,DVPrecOp]],
          [ DVPrec,
            ['Truth_StructuralDeduction',DVPrecOp]],
          [ SubgoalStamp,
            ['TupleConcat',GoalEvBase,ImpEvBase]]],
        [ 'If',
          ['StampDisjoint',SubgoalStamp],
          [ 'Decision',
            [ [^,Op],
              DVOp],
            [ 'Event',
              [ Prec,
                ['Truth_StructuralDeduction',DVPrecOp]],
              [ T,
                SubgoalStamp,
                [ T,
                  [ 'SubgoalPriority',
                    ['EventPriorityNow',GoalPrio,T],
                    DVPrec]]]]]]]],
    [ let,
      [ 'Decision',
        [Opname,DVOp],
        Subgoal],
      ['BestCandidate',Options,'OpExpectation',T],
      [ 'If',
        [ >,
          ['Truth_Expectation',DVOp],
          0.5],
        ['Execute',Opname],
        ['DeriveSubgoals',Options]]]]).

%;;select the highest priority goal event from the PQ and use it for decision making
metta_defn_ES(
  ['GoalCycle',T],
  [ sequential,
    [ [ let,
        Ev,
        ['SelectHighestPriorityEvent','&goal_events',T],
        [ sequential,
          [ [ do,
              ['remove-atom','&goal_events',Ev]],
            [ 'BestDecision', T,Ev,
              ['get-state','&FIFO']]]]],
      [ do,
        ['UpdateReasonerState']],
      [ do,
        [ 'BoundEvents', '&goal_events',0.0,0.1,
          ['GoalEventsMax'],
          T]]]]).

%;;Input procedure
metta_defn_ES(
  ['AddGoalEvent',Sentence],
  [ 'let*',
    [ [ [         T         ,  EvidentialBase  ],
        [ 'GetReasonerState']],
      [ InputEvent,
        [ 'Event',
          Sentence,
          [ T,
            EvidentialBase,
            [T,1.0]]]]],
    [ sequential,
      [ [ do,
          ['add-atom','&goal_events',InputEvent]],
        ['GoalCycle',T]]]]).

:-metta_eval([print,'$STRING'("NARS test!!!!!!!!!!!!!!!!!!")]).

:-metta_eval(['mettalog::vspace-main']).

( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [ ['{',garfield,'}'], -->,cat],
                   [1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ ['{',garfield,'}'], -->,cat],
      [1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ ['{',garfield,'}'], -->,cat],
            [1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [ [cat,*,sky], -->,like],
                   [1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ [cat,*,sky], -->,like],
      [1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ [cat,*,sky], -->,like],
            [1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [ sky,
                     -->,
                     [ '$OBJ'(claz_bracket_vector,[blue])]],
                   [1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ sky,
        -->,
        [ '$OBJ'(claz_bracket_vector,[blue])]],
      [1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ sky,
              -->,
              [ '$OBJ'(claz_bracket_vector,[blue])]],
            [1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

%;The following question needs both a deduction and abduction step:
( :- (
   metta_eval( [ 'EternalQuestion',
                 [ [ ['{',garfield,'}'],
                     *,
                     [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like]])) ).

metta_defn_ES(
  [ 'EternalQuestion',
    [ [ ['{',garfield,'}'],
        *,
        [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like]],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept',
        [ [ ['{',garfield,'}'],
            *,
            [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like], A,_,_],
      A],
    [ [B,B],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [eternal,[],0.0]]]]]).

%;expected: [(Event (((({ garfield }) * ([ blue ])) --> like) (1.0 0.2965825874694874)) (eternal (Cons 2 (Cons 1 (Cons 3 Nil))) 0.643288027761712))]
%;Lets stress the control mechanism as these type of events with common extension or intension causes dozens of derivations:
( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [  A ,-->,cat],
                   [ 1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ 'A',-->,cat],
      [ 1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ 'A',-->,cat],
            [ 1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [  B ,-->,cat],
                   [ 1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ 'B',-->,cat],
      [ 1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ 'B',-->,cat],
            [ 1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [  C ,-->,cat],
                   [ 1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ 'C',-->,cat],
      [ 1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ 'C',-->,cat],
            [ 1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

:-metta_eval(['EternalQuestion',[['A',&,'B'],-->,cat]]).

metta_defn_ES(
  [ 'EternalQuestion',
    [['A',&,'B'],-->,cat]],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept',
        [['A',&,'B'],-->,cat], A,_,_],
      A],
    [ [B,B],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [eternal,[],0.0]]]]]).

%;expected: [(Event (((A & B) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 4 (Cons 5 Nil)) (5 0.4525)))]
:-metta_eval(['EternalQuestion',[['B',&,'C'],-->,cat]]).

metta_defn_ES(
  [ 'EternalQuestion',
    [['B',&,'C'],-->,cat]],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept',
        [['B',&,'C'],-->,cat], A,_,_],
      A],
    [ [B,B],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [eternal,[],0.0]]]]]).

%;expected: [(Event (((B & C) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 5 (Cons 6 Nil)) (6 0.4525)))]
( :- (
   metta_eval( [ 'EternalQuestion',
                 [ [['A',&,'B'],&,'C'], -->,cat]])) ).

metta_defn_ES(
  [ 'EternalQuestion',
    [ [['A',&,'B'],&,'C'], -->,cat]],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept',
        [ [['A',&,'B'],&,'C'], -->,cat], A,_,_],
      A],
    [ [B,B],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [eternal,[],0.0]]]]]).

%;expected: [(Event ((((A & B) & C) --> cat) (1.0 0.42163100057836905)) (eternal (Cons 5 (Cons 4 (Cons 6 Nil))) (6 0.195593125)))
( :- (
   metta_eval( [ 'AddBeliefEvent',
                 [ [ [ ['{',garfield,'}'],
                       *,
                       [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like],
                   [1.0,0.9]]])) ).

metta_defn_ES(
  [ 'AddBeliefEvent',
    [ [ [ ['{',garfield,'}'],
          *,
          [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like],
      [1.0,0.9]]],
  [ 'let*',
    [ [ [         A         ,        B         ],
        [ 'GetReasonerState']],
      [ C,
        [ 'Event',
          [ [ [ ['{',garfield,'}'],
                *,
                [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like],
            [1.0,0.9]],
          [ A,
            B,
            [A,1.0]]]]],
    [ do,
      [ sequential,
        [ ['EventToFIFO',C],
          [ let,
            D,
            [ 'TemporalImplicationInduction',
              ['get-state','&FIFO']],
            ['UpdateConcept',D,A]],
          ['ProcessBeliefEvent',C,A],
          [ 'Anticipate',
            ['get-state','&FIFO'],
            A],
          ['BeliefCycle',A]]]]]).

( :- (
   metta_eval( [ 'EternalQuestion',
                 [ [ ['{',garfield,'}'],
                     *,
                     [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like]])) ).

metta_defn_ES(
  [ 'EternalQuestion',
    [ [ ['{',garfield,'}'],
        *,
        [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like]],
  [ case,
    [ match,
      [ superpose,
        ['&attentional_focus','&concepts']],
      [ 'Concept',
        [ [ ['{',garfield,'}'],
            *,
            [ '$OBJ'(claz_bracket_vector,[blue])]], -->,like], A,_,_],
      A],
    [ [B,B],
      [ '%void%',
        [ 'Event',
          [ 'None',
            [0.5,0.0]],
          [eternal,[],0.0]]]]]).

%;expected: [(Event (((({ garfield }) * ([ blue ])) --> like) (1.0 0.5692683291397822)) (eternal (Cons 7 (Cons 2 (Cons 1 (Cons 3 Nil)))) 0.0))]
%;Please notice that it has revised it with the prior derived result, as you can also see in the evidence trail 1,2,3 being included
:-metta_eval(['mettalog::vspace-main']).

%;debug:
:-metta_eval(['CollapseCardinality',['get-atoms','&belief_events']]).

metta_defn_ES(
  [ 'CollapseCardinality',
    ['get-atoms','&belief_events']],
  [ 'TupleCount',
    [ collapse,
      [ 'CountElement',
        ['get-atoms','&belief_events']]]]).

metta_defn_ES(
  [ 'CountElement',
    ['get-atoms','&belief_events']],
  [ case,
    ['get-atoms','&belief_events'],
    [ [ _,1]]]).

%;[8]
:-metta_eval(['CollapseCardinality',['get-atoms','&attentional_focus']]).

metta_defn_ES(
  [ 'CollapseCardinality',
    ['get-atoms','&attentional_focus']],
  [ 'TupleCount',
    [ collapse,
      [ 'CountElement',
        ['get-atoms','&attentional_focus']]]]).

metta_defn_ES(
  [ 'CountElement',
    ['get-atoms','&attentional_focus']],
  [ case,
    ['get-atoms','&attentional_focus'],
    [ [ _,1]]]).

%;[8]
:-metta_eval(['CollapseCardinality',['get-atoms','&concepts']]).

metta_defn_ES(
  [ 'CollapseCardinality',
    ['get-atoms','&concepts']],
  [ 'TupleCount',
    [ collapse,
      [ 'CountElement',
        ['get-atoms','&concepts']]]]).

metta_defn_ES(
  [ 'CountElement',
    ['get-atoms','&concepts']],
  [ case,
    ['get-atoms','&concepts'],
    [ [ _,1]]]).

%;[100]
:-metta_eval(['mettalog::vspace-main']).

% 17,439,387 inferences, 1.561 CPU in 1.572 seconds (99% CPU, 11172049 Lips)

