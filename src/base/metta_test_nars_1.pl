% ?- load_metta('./examples/VRUN_tests1.metta').
%```prolog
%`
% (track_load_into_file  "./examples/VRUN_tests1.metta").
:- metta_eval(['extend-py!',metta_learner]).
% ;; stdlib extension
metta_type('&self','If',[->,'Bool','Atom','Atom']).
metta_defn('&self',['If','True',A],A).
metta_defn('&self',['If','False',_],[]).
metta_type('&self','If',[->,'Bool','Atom','Atom','Atom']).
metta_defn('&self',['If',A,B,C],[if,A,B,C]).
metta_defn('&self',['TupleConcat',A,B],[collapse,[superpose,[[superpose,A],[superpose,B]]]]).
metta_defn('&self',[max,A,B],['If',[>,A,B],A,B]).
metta_defn('&self',[min,A,B],['If',[<,A,B],A,B]).
metta_defn('&self',[abs,A],['If',[<,A,0],[-,0,A],A]).
metta_type('&self',sequential,[->,'Expression','%Undefined%']).
metta_defn('&self',[sequential,A],[superpose,A]).
metta_type('&self',do,[->,'Expression','%Undefined%']).
metta_defn('&self',[do,A],[case,A,[]]).
metta_defn('&self',['TupleCount',[]],0).
metta_defn('&self',['TupleCount',[1]],1).
metta_defn('&self',['BuildTupleCounts',A,B,C],[let,D,[collapse,[superpose,[1,[superpose,A]]]],[superpose,[['add-atom','&self',[=,['TupleCount',D],[+,B,2]]],['If',[<,B,C],['BuildTupleCounts',D,[+,B,1],C]]]]]).
metta_type('&self','CountElement',[->,'Expression','Number']).
metta_defn('&self',['CountElement',A],[case,A,[[_,1]]]).
% ;;Build for count up to 100 (takes a few sec but it is worth it if space or generally collapse counts are often needed).
:- metta_eval(['BuildTupleCounts',[1],0,100]).
metta_defn('&self',['BuildTupleCounts',[1],0,100],[let,A,[collapse,[superpose,[1,[superpose,[1]]]]],[superpose,[['add-atom','&self',[=,['TupleCount',A],[+,0,2]]],['If',[<,0,100],['BuildTupleCounts',A,[+,0,1],100]]]]]).
metta_type('&self','CollapseCardinality',[->,'Expression','Number']).
metta_defn('&self',['CollapseCardinality',A],['TupleCount',[collapse,['CountElement',A]]]).
% ;; Truth functions
metta_defn('&self',['Truth_c2w',A],[/,A,[-,1,A]]).
metta_defn('&self',['Truth_w2c',A],[/,A,[+,A,1]]).
metta_defn('&self',['Truth_Deduction',[A,B],[C,D]] ,  [[*,A,C],[*,[*,A,C],[*,B,D]]]).
metta_defn('&self',['Truth_Abduction',[A,B],[C,D]],[C,['Truth_w2c',[*,[*,A,B],D]]]).
metta_defn('&self',['Truth_Induction',A,B],['Truth_Abduction',B,A]).
metta_defn('&self',['Truth_Exemplification',[A,B],[C,D]],[1.0,['Truth_w2c',[*,[*,A,C],[*,B,D]]]]).
metta_defn('&self',['Truth_StructuralDeduction',A],['Truth_Deduction',A,[1.0,0.9]]).
metta_defn('&self',['Truth_Negation',[A,B]] ,  [[-,1,A],B]).
metta_defn('&self',['Truth','StructuralDeductionNegated',A],['Truth_Negation',['Truth_StructuralDeduction',A]]).
metta_defn('&self',['Truth_Intersection',[A,B],[C,D]] ,  [[*,A,C],[*,B,D]]).
metta_defn('&self',['Truth_StructuralIntersection',A],['Truth_Intersection',A,[1.0,0.9]]).
metta_defn('&self',['Truth_or',A,B],[-,1,[*,[-,1,A],[-,1,B]]]).
metta_defn('&self',['Truth_Comparison',[A,B],[C,D]],[let,E,['Truth_or',A,C],[['If',[==,E,0.0],0.0,[/,[*,A,C],E]],['Truth_w2c',[*,E,[*,B,D]]]]]).
metta_defn('&self',['Truth_Analogy',[A,B],[C,D]] ,  [[*,A,C],[*,[*,B,D],C]]).
metta_defn('&self',['Truth_Resemblance',[A,B],[C,D]] ,  [[*,A,C],[*,[*,B,D],['Truth_or',A,C]]]).
metta_defn('&self',['Truth_Union',[A,B],[C,D]] ,  [['Truth_or',A,C],[*,B,D]]).
metta_defn('&self',['Truth_Difference',[A,B],[C,D]] ,  [[*,A,[-,1,C]],[*,B,D]]).
metta_defn('&self',['Truth_DecomposePNN',[A,B],[C,D]],[let,E,[*,A,[-,1,C]] ,  [[-,1,E],[*,E,[*,B,D]]]]).
metta_defn('&self',['Truth_DecomposeNPP',[A,B],[C,D]],[let,E,[*,[-,1,A],C],[E,[*,E,[*,B,D]]]]).
metta_defn('&self',['Truth_DecomposePNP',[A,B],[C,D]],[let,E,[*,A,[-,1,C]],[E,[*,E,[*,B,D]]]]).
metta_defn('&self',['Truth_DecomposePPP',A,B],['Truth_DecomposeNPP',['Truth_Negation',A],B]).
metta_defn('&self',['Truth_DecomposeNNN',[A,B],[C,D]],[let,E,[*,[-,1,A],[-,1,C]] ,  [[-,1,E],[*,E,[*,B,D]]]]).
metta_defn('&self',['Truth_Eternalize',[A,B]],[A,['Truth_w2c',B]]).
metta_defn('&self',['Truth_Revision',[A,B],[C,D]],['let*',[[E,['Truth_c2w',B]],[F,['Truth_c2w',D]],[G,[+,E,F]],[H,[/,[+,[*,E,A],[*,F,C]],G]],[I,['Truth_w2c',G]]] ,  [[min,1.0,H],[min,0.99,[max,[max,I,B],D]]]]).
metta_defn('&self',['Truth_Expectation',[A,B]],[+,[*,B,[-,A,0.5]],0.5]).
% ;;NAL-1
% ;;!Syllogistic rules for Inheritance:
metta_defn('&self',['|-',[[A,-->,B],C],[[B,-->,D],E]] ,  [[A,-->,D],['Truth_Deduction',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[D,-->,B],['Truth_Induction',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[D,-->,A],['Truth_Abduction',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[B,-->,D],E]] ,  [[D,-->,A],['Truth_Exemplification',C,E]]).
% ;;NAL-2
% ;;!Rules for Similarity:
metta_defn('&self',['|-',[[A,<->,B],C]] ,  [[B,<->,A],['Truth_StructuralIntersection',C]]).
metta_defn('&self',['|-',[[A,<->,B],C],[[D,<->,A],E]] ,  [[D,<->,B],['Truth_Resemblance',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[D,<->,A],['Truth_Comparison',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[D,<->,B],['Truth_Comparison',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,<->,A],E]] ,  [[D,-->,B],['Truth_Analogy',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,<->,B],E]] ,  [[A,-->,D],['Truth_Analogy',C,E]]).
% ;;!Dealing with properties and instances:
metta_defn('&self',['|-',[[A,-->,['{',B,'}']],C]] ,  [[A,<->,['{',B,'}']],['Truth_StructuralIntersection',C]]).
metta_defn('&self',['|-',[[['$OBJ'(claz_bracket_vector,['$S'])],-->,A],B]] ,  [[['$OBJ'(claz_bracket_vector,['$S'])],<->,A],['Truth_StructuralIntersection',B]]).
metta_defn('&self',['|-',[[['{',A,'}'],-->,B],C],[[D,<->,A],E]] ,  [[['{',D,'}'],-->,B],['Truth_Analogy',C,E]]).
metta_defn('&self',['|-',[[A,-->,['$OBJ'(claz_bracket_vector,['$M'])]],B],[[_,<->,_],C]] ,  [[A,-->,['$OBJ'(claz_bracket_vector,['$S'])]],['Truth_Analogy',B,C]]).
metta_atom('&self',[=,['|-',[[['{',A,'}'],<->,['{',B,'}']]],[A,<->,B],['Truth_StructuralIntersection',_]]]).
metta_atom('&self',[=,['|-',[[['$OBJ'(claz_bracket_vector,['$A'])],<->,['$OBJ'(claz_bracket_vector,['$B'])]]],[_,<->,_],['Truth_StructuralIntersection',_]]]).
% ;;NAL-3
% ;;!Set decomposition:
metta_defn('&self',['|-',[[['{',A,_,'}'],-->,B],C]] ,  [[['{',A,'}'],-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[['{',_,A,'}'],-->,B],C]] ,  [[['{',A,'}'],-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[['M',-->,['$OBJ'(claz_bracket_vector,['$A','$B'])]],A]] ,  [['M',-->,['$OBJ'(claz_bracket_vector,['$A'])]],['Truth_StructuralDeduction',A]]).
metta_defn('&self',['|-',[['M',-->,['$OBJ'(claz_bracket_vector,['$A','$B'])]],A]] ,  [['M',-->,['$OBJ'(claz_bracket_vector,['$B'])]],['Truth_StructuralDeduction',A]]).
% ;;!Extensional and intensional intersection decomposition:
metta_defn('&self',['|-',[[[A,'|',_],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[A,-->,[B,&,_]],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[[_,'|',A],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[A,-->,[_,&,B]],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[[A,~,_],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[A,-->,[B,-,_]],C]] ,  [[A,-->,B],['Truth_StructuralDeduction',C]]).
metta_defn('&self',['|-',[[[_,~,A],-->,B],C]] ,  [[A,-->,B],['Truth_StructuralDeductionNegated',C]]).
metta_defn('&self',['|-',[[A,-->,[_,-,B]],C]] ,  [[A,-->,B],['Truth_StructuralDeductionNegated',C]]).
% ;;!Extensional and intensional intersection composition: (sets via reductions).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[[A,'|',D],-->,B],['Truth_Intersection',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[[A,&,D],-->,B],['Truth_Union',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[D,-->,B],E]] ,  [[[A,~,D],-->,B],['Truth_Difference',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[A,-->,[B,&,D]],['Truth_Intersection',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[A,-->,[B,'|',D]],['Truth_Union',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,D],E]] ,  [[A,-->,[B,-,D]],['Truth_Difference',C,E]]).
% ;;!Extensional and intensional intersection decomposition:
metta_defn('&self',['|-',[[A,-->,B],C],[[[A,'|',D],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposePNN',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[[D,'|',A],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposePNN',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[[A,&,D],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposeNPP',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[[D,&,A],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposeNPP',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[[A,~,D],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposePNP',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[[D,~,A],-->,B],E]] ,  [[D,-->,B],['Truth_DecomposeNNN',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,[B,&,D]],E]] ,  [[A,-->,D],['Truth_DecomposePNN',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,[D,&,B]],E]] ,  [[A,-->,D],['Truth_DecomposePNN',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,[B,'|',D]],E]] ,  [[A,-->,D],['Truth_DecomposeNPP',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,[D,'|',B]],E]] ,  [[A,-->,D],['Truth_DecomposeNPP',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,[B,-,D]],E]] ,  [[A,-->,D],['Truth_DecomposePNP',C,E]]).
metta_defn('&self',['|-',[[A,-->,B],C],[[A,-->,[D,-,B]],E]] ,  [[A,-->,D],['Truth_DecomposeNNN',C,E]]).
% ;; NAL-4
% ;;!Transformation rules between product and image:
metta_defn('&self',['|-',[[[A,*,B],-->,C],D]] ,  [[A,-->,[C,'/1',B]],['Truth_StructuralIntersection',D]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D]] ,  [[B,-->,[C,'/2',A]],['Truth_StructuralIntersection',D]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D]] ,  [[[A,'\\1',C],-->,B],['Truth_StructuralIntersection',D]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D]] ,  [[[A,'\\2',B],-->,C],['Truth_StructuralIntersection',D]]).
% ;;other direction of same rules (as these are bi-directional).
metta_defn('&self',['|-',[[A,-->,[B,'/1',C]],D]] ,  [[[A,*,C],-->,B],['Truth_StructuralIntersection',D]]).
metta_defn('&self',['|-',[[A,-->,[B,'/2',C]],D]] ,  [[[C,*,A],-->,B],['Truth_StructuralIntersection',D]]).
metta_defn('&self',['|-',[[[A,'\\1',B],-->,C],D]] ,  [[A,-->,[C,*,B]],['Truth_StructuralIntersection',D]]).
metta_defn('&self',['|-',[[[A,'\\2',B],-->,C],D]] ,  [[A,-->,[B,*,C]],['Truth_StructuralIntersection',D]]).
% ;;!Comparative relations
metta_defn('&self',['|-',[[['{',A,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],B],[[['{',C,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],D]] ,  [[[['{',A,'}'],*,['{',C,'}']],-->,[>>>,_]],['Truth_FrequencyGreater',B,D]]).
metta_defn('&self',['|-',[[[A,*,B],-->,[>>>,C]],D],[[[B,*,E],-->,[>>>,C]],F]] ,  [[[A,*,E],-->,[>>>,C]],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[['{',A,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],B],[[['{',C,'}'],'|-',>,['$OBJ'(claz_bracket_vector,['$P'])]],D]] ,  [[[['{',A,'}'],*,['{',C,'}']],-->,[===,_]],['Truth_FrequencyEqual',B,D]]).
metta_defn('&self',['|-',[[[A,*,B],-->,[===,C]],D],[[[B,*,E],-->,[===,C]],F]] ,  [[[A,*,E],-->,[===,C]],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,[===,C]],D]] ,  [[[B,*,A],-->,[===,C]],['Truth_StructuralIntersection',D]]).
% ;;!Optional rules for more efficient reasoning about relation components:
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[[E,*,B],-->,C],F]] ,  [[E,-->,A],['Truth_Abduction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[[A,*,E],-->,C],F]] ,  [[E,-->,B],['Truth_Abduction',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[A,-->,[E,*,C]],F]] ,  [[E,-->,B],['Truth_Induction',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[A,-->,[B,*,E]],F]] ,  [[E,-->,C],['Truth_Induction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[E,-->,A],F]] ,  [[[E,*,B],-->,C],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[A,-->,E],F]] ,  [[[E,*,B],-->,C],['Truth_Induction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[E,<->,A],F]] ,  [[[E,*,B],-->,C],['Truth_Analogy',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[E,-->,B],F]] ,  [[[A,*,E],-->,C],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[B,-->,E],F]] ,  [[[A,*,E],-->,C],['Truth_Induction',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[E,<->,B],F]] ,  [[[A,*,E],-->,C],['Truth_Analogy',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[B,-->,E],F]] ,  [[A,-->,[E,*,C]],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[E,-->,B],F]] ,  [[A,-->,[E,*,C]],['Truth_Abduction',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[E,<->,B],F]] ,  [[A,-->,[E,*,C]],['Truth_Analogy',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[C,-->,E],F]] ,  [[A,-->,[B,*,E]],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[E,-->,C],F]] ,  [[A,-->,[B,*,E]],['Truth_Abduction',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[E,<->,C],F]] ,  [[A,-->,[B,*,E]],['Truth_Analogy',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[[E,*,B],-->,C],F]] ,  [[A,<->,E],['Truth_Comparison',D,F]]).
metta_defn('&self',['|-',[[[A,*,B],-->,C],D],[[[A,*,E],-->,C],F]] ,  [[B,<->,E],['Truth_Comparison',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[A,-->,[E,*,C]],F]] ,  [[B,<->,E],['Truth_Comparison',D,F]]).
metta_defn('&self',['|-',[[A,-->,[B,*,C]],D],[[A,-->,[B,*,E]],F]] ,  [[C,<->,E],['Truth_Comparison',D,F]]).
% ;;NAL-5
% ;;!Negation conjunction and disjunction decomposition:
metta_defn('&self',['|-',[[!,A],B]],[A,['Truth_Negation',B]]).
metta_defn('&self',['|-',[[A,&&,_],B]],[A,['Truth_StructuralDeduction',B]]).
metta_defn('&self',['|-',[[_,&&,A],B]],[A,['Truth_StructuralDeduction',B]]).
metta_defn('&self',['|-',[[A,&&,B],C]] ,  [[B,&&,A],['Truth_StructuralIntersection',C]]).
metta_defn('&self',['|-',[A,B],[[A,&&,C],D]],[C,['Truth_DecomposePNN',B,D]]).
metta_defn('&self',['|-',[A,B],[[A,'||',C],D]],[C,['Truth_DecomposeNPP',B,D]]).
metta_defn('&self',['|-',[A,B],[[[!,A],&&,C],D]],[C,['Truth_DecomposeNNN',B,D]]).
metta_defn('&self',['|-',[A,B],[[[!,A],'||',C],D]],[C,['Truth_DecomposePPP',B,D]]).
% ;;!Syllogistic rules for Implication:
metta_defn('&self',['|-',[[A,==>,B],C],[[B,==>,D],E]] ,  [[A,==>,D],['Truth_Deduction',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[D,==>,B],['Truth_Induction',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[D,==>,A],['Truth_Abduction',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[B,==>,D],E]] ,  [[D,==>,A],['Truth_Exemplification',C,E]]).
% ;;!Conditional composition for conjunction and disjunction:
metta_defn('&self',['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[[A,&&,D],==>,B],['Truth_Union',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[[A,'||',D],==>,B],['Truth_Intersection',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[A,==>,[B,&&,D]],['Truth_Intersection',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[A,==>,[B,'||',D]],['Truth_Union',C,E]]).
% ;;!Multi-conditional inference:
metta_defn('&self',['|-',[[[A,&&,B],==>,C],D],[[A,==>,C],E]],[B,['Truth_Abduction',D,E]]).
metta_defn('&self',['|-',[[[A,&&,B],==>,C],D],[[E,==>,B],F]] ,  [[[A,&&,E],==>,C],['Truth_Deduction',D,F]]).
metta_defn('&self',['|-',[[[A,&&,B],==>,C],D],[[[A,&&,E],==>,C],F]] ,  [[E,==>,B],['Truth_Abduction',D,F]]).
metta_defn('&self',['|-',[[[A,&&,B],==>,C],D],[[B,==>,E],F]] ,  [[[A,&&,E],==>,C],['Truth_Induction',D,F]]).
% ;;!Rules for equivalence:
metta_defn('&self',['|-',[[A,<=>,B],C]] ,  [[B,<=>,A],['Truth_StructuralIntersection',C]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[B,==>,A],D]] ,  [[A,<=>,B],['Truth_Intersection',C,D]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[D,==>,B],E]] ,  [[D,<=>,A],['Truth_Comparison',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[A,==>,D],E]] ,  [[D,<=>,B],['Truth_Comparison',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[D,<=>,A],E]] ,  [[D,==>,B],['Truth_Analogy',C,E]]).
metta_defn('&self',['|-',[[A,==>,B],C],[[D,<=>,B],E]] ,  [[A,==>,D],['Truth_Analogy',C,E]]).
metta_defn('&self',['|-',[[A,<=>,B],C],[[D,<=>,A],E]] ,  [[D,<=>,B],['Truth_Resemblance',C,E]]).
% ;;!Higher-order decomposition
metta_defn('&self',['|-',[A,B],[[A,==>,C],D]],[C,['Truth_Deduction',B,D]]).
metta_defn('&self',['|-',[A,B],[[[A,&&,C],==>,D],E]] ,  [[C,==>,D],['Truth_Deduction',B,E]]).
metta_defn('&self',['|-',[A,B],[[C,==>,A],D]],[C,['Truth_Abduction',B,D]]).
metta_defn('&self',['|-',[A,B],[[A,<=>,C],D]],[C,['Truth_Analogy',B,D]]).
% ;;NAL term reductions
% ;;!Extensional intersection, union, conjunction reductions:
metta_defn('&self',[A,&,A],A).
metta_defn('&self',[A,'|',A],A).
metta_defn('&self',[A,&&,A],A).
metta_defn('&self',[A,'||',A],A).
% ;;!Extensional set reductions:
metta_defn('&self',[['{',A,'}'],'|',['{',B,'}']],['{',A,B,'}']).
metta_defn('&self',[['{',A,B,'}'],'|',['{',C,'}']],['{',[A|B],C,'}']).
metta_defn('&self',[['{',A,'}'],'|',['{',B,C,'}']],['{',A,[B|C],'}']).
% ;;!Intensional set reductions:
metta_defn('&self',[['$OBJ'(claz_bracket_vector,['$A'])],&,['$OBJ'(claz_bracket_vector,['$B'])]],['$OBJ'(claz_bracket_vector,['$A','$B'])]).
metta_defn('&self',[['$OBJ'(claz_bracket_vector,['$A','$B'])],&,['$OBJ'(claz_bracket_vector,['$C'])]],['$OBJ'(claz_bracket_vector,[['$A'|'$B'],'$C'])]).
metta_defn('&self',[['$OBJ'(claz_bracket_vector,['$A'])],&,['$OBJ'(claz_bracket_vector,['$B','$C'])]],['$OBJ'(claz_bracket_vector,['$A',['$B'|'$C']])]).
% ;;!Reduction for set element copula:
metta_defn('&self',['{',[A|B],'}'],['{',A,B,'}']).
metta_defn('&self',['$OBJ'(claz_bracket_vector,[['$A'|'$B']])],['$OBJ'(claz_bracket_vector,['$A','$B'])]).
% ;params
metta_defn('&self',['BeliefEventsMax'],10).
metta_defn('&self',['GoalEventsMax'],10).
% ;spaces
:- metta_eval(['bind!','&belief_events',['new-space']]).
:- metta_eval(['bind!','&goal_events',['new-space']]).
% ;states
:- metta_eval(['bind!','&currentTime',['new-state',1]]).
:- metta_eval(['bind!','&evidentialBase',['new-state',1]]).
metta_defn('&self',[increment,A],['change-state!',A,[+,1,['get-state',A]]]).
metta_defn('&self',['UpdateReasonerState'],[[increment,'&currentTime'],[increment,'&evidentialBase']]).
metta_defn('&self',['GetReasonerState'],[['get-state','&currentTime'],[['get-state','&evidentialBase']]]).
% ;priority of events
metta_defn('&self',['EventPriorityNow',[A,B],A],[*,B,[/,1,[+,1,[-,A,A]]]]).
% ;retrieve the best candidate (allows to use tuples / collapse results / spaces as a PQ).
:- metta_eval(['bind!','&tempbest',['new-state',[]]]).
:- metta_eval(['bind!','&tempbestscore',['new-state',0]]).
metta_defn('&self',['BestCandidate',A,B,C],[sequential,[[do,['change-state!','&tempbestscore',0]],[do,['change-state!','&tempbest',[]]],[do,['let*',[[D,[superpose,A]],[E,[B,D,C]]],[superpose,[['If',[>,E,['get-state','&tempbestscore']],[sequential,[['change-state!','&tempbest',D],['change-state!','&tempbestscore',E]]]]]]]],['get-state','&tempbest']]]).
% ;functions to select highest-priority events in belief and goal PQ
metta_defn('&self',['PriorityOf',['Event',_,[_,_,A]],B],['EventPriorityNow',A,B]).
metta_defn('&self',['SelectHighestPriorityEvent',A,B],['BestCandidate',[collapse,['get-atoms',A]],'PriorityOf',B]).
% ;a belief event to process, which demands adding it to the PQ and updating its concept
metta_defn('&self',['ProcessBeliefEvent',A,B],[sequential,[['add-atom','&belief_events',A],['UpdateConcept',A,B]]]).
% ;bound the size of the attentional focus for tasks / events
metta_defn('&self',['BoundEvents',A,B,C,D,E],[sequential,[[do,['let*',[[F,['get-atoms',A]] ,  [['Event',_,[_,_,G]],F]],['If',[<,['EventPriorityNow',G,E],B],['remove-atom',A,F]]]],[let,H,['CollapseCardinality',['get-atoms',A]],['If',[>,H,D],['BoundEvents',A,[+,B,C],C,D,E]]]]]).
% ;params
metta_defn('&self',['AttentionalFocusConceptsMax'],10).
% ;spaces
:- metta_eval(['bind!','&concepts',['new-space']]).
:- metta_eval(['bind!','&attentional_focus',['new-space']]).
% ;priority of concepts
metta_defn('&self',['ConceptPriorityNow',[A,B],A],[*,B,[/,1,[+,1,[-,A,A]]]]).
% ;whether evidence was just counted once
:- metta_eval(['bind!','&tempstate',['new-state','False']]).
:- metta_eval(['bind!','&tempset',['new-space']]).
metta_defn('&self',['StampDisjoint',A],[not,[sequential,[[do,['change-state!','&tempstate','False']],[do,[case,['get-atoms','&tempset'],[[B,['remove-atom','&tempset',B]]]]],[do,[let,C,[superpose,A],[case,[match,'&tempset',C,C],[[_,['change-state!','&tempstate','True']],['%void%',['add-atom','&tempset',C]]]]]],['get-state','&tempstate']]]]).
% ;revise if there is no evidential overlap, else use higher-confident candidate
metta_defn('&self',['RevisionAndChoice',['Event',[A,[B,C]],[eternal,D,_]],['Event',[E,[F,G]],[eternal,H,_]]],[let,I,['TupleConcat',D,H],['If',['StampDisjoint',I],['Event',[A,['Truth_Revision',[B,C],[F,G]]],[eternal,I,[0,0.0]]],['If',[>,C,G],['Event',[A,[B,C]],[eternal,D,[0,0.0]]],['Event',[E,[F,G]],[eternal,H,[0,0.0]]]]]]).
% ;;update beliefs in existing concept with the new event or create new concept to enter the new evidence
metta_defn('&self',['UpdateConcept',A,B],['let*',[[['Event',[C,_],[_,_,D]],A],[E,['Eternalize',A]],[F,['Concept',C,G,_,H]]],[sequential,[[case,[match,'&attentional_focus',F,F],[[F,[sequential,[['remove-atom','&attentional_focus',F],['let*',[[I,['RevisionAndChoice',G,E]],[J,['If',[>,['EventPriorityNow',D,B],['ConceptPriorityNow',H,B]],D,H]]],['add-atom','&attentional_focus',['Concept',C,I,A,J]]]]]],['%void%',[case,[match,'&concepts',F,F],[[F,[sequential,[['remove-atom','&concepts',F],['add-atom','&attentional_focus',F],['UpdateConcept',A,B]]]],['%void%',['add-atom','&attentional_focus',['Concept',C,E,A,D]]]]]]]]]]]).
% ;bound the size of attentional focus of concepts
metta_defn('&self',['BoundAttention',A,B,C,D],[sequential,[[do,['let*',[[E,['get-atoms','&attentional_focus']] ,  [['Concept',_,['Event',_,_],_,F],E]],['If',[<,['ConceptPriorityNow',F,D],A],[sequential,[['remove-atom','&attentional_focus',E],['add-atom','&concepts',E]]]]]],[let,G,['CollapseCardinality',['get-atoms','&attentional_focus']],['If',[>,G,C],['BoundAttention',[+,A,B],B,C,D]]]]]).
% ;get eternal belief of concept
metta_type('&self','EternalQuestion',[->,'Expression',_]).
metta_defn('&self',['EternalQuestion',A],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',A,B,_,_],B],[[C,C],['%void%',['Event',['None',[0.5,0.0]],[eternal,[],0.0]]]]]).
% ;get event belief of concept
metta_type('&self','EventQuestion',[->,'Expression',_]).
metta_defn('&self',['EventQuestion',A],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',A,_,B,_],B],[[C,C],['%void%',['Event',['None',[0.5,0.0]],[0,[],0.0]]]]]).
% ;;Declarative inference (deriving events and knowledge from observed events).
% ;Derived belief event priority
metta_defn('&self',['ConclusionPriority',A,B,C],[*,[*,A,B],['Truth_Expectation',C]]).
% ;making declarative inferences on two events (task from PQ and belief from concept).
metta_defn('&self',['Conclude',['Event',A,[B,C,D]],['Event',E,[_,F,_]],G,H],[let,I,['TupleConcat',C,F],['If',['StampDisjoint',I],[let,[J,K],[superpose,[['|-',A,E],['|-',E,A]]],['Event',[J,K],[B,I,[H,['ConclusionPriority',['EventPriorityNow',D,H],['ConceptPriorityNow',G,H],K]]]]]]]).
% ;find a belief for the task to generate conclusions with
metta_defn('&self',['ReasonWithTask',['Event',A,[B,C,D]],E],[let,[F,G],[case,['get-atoms','&attentional_focus'],[[['Concept',_,['Event',H,[I,J,K]],['Event',L,[M,N,O]],G],['If',[and,[not,[==,B,eternal]],[>,[abs,[-,B,M]],20]] ,  [['Event',H,[I,J,K]],_],[['Event',L,[M,N,O]],G]]]]],[case,['Conclude',['Event',A,[B,C,D]],['TemporallyAlignedBelief',B,F],G,E],[[['Event',P,Q],['ProcessBeliefEvent',['Event',P,Q],E]]]]]).
% ;select the highest priority belief event from the PQ and use it for reasoning
metta_defn('&self',['BeliefCycle',A],[do,[sequential,[[let,B,['SelectHighestPriorityEvent','&belief_events',A],[sequential,[['remove-atom','&belief_events',B],['ReasonWithTask',B,A]]]],['UpdateReasonerState'],['BoundEvents','&belief_events',0.0,0.1,['BeliefEventsMax'],A],['BoundAttention',0.0,0.1,['AttentionalFocusConceptsMax'],A]]]]).
% ;;Temporal inference (sequence and implication formation based on FIFO).
% ;use the event''s evidence to induce a time-independent belief which can be used in the future
metta_defn('&self',['Eternalize',A],[let,['Event',[B,C],[D,E,_]],A,['If',[==,D,eternal],A,['Event',[B,['Truth_Eternalize',C]],[eternal,E,[0,0.0]]]]]).
% ;use evidence of an event at a slightly different moment in time
metta_defn('&self',['Projection',['Event',[A,[B,C]],[D,E,F]],G],['Event',[A,[B,[*,C,[min,1,[/,1,[abs,[-,D,G]]]]]]],[G,E,F]]).
% ;make the belief occurrence time compatible with the task''s
metta_defn('&self',['TemporallyAlignedBelief',A,B],['If',[==,A,eternal],['Eternalize',B],['Projection',B,A]]).
% ;FIFO max. size bound
:- metta_eval(['bind!','&FIFO',['new-state',[]]]).
metta_defn('&self',['ListFirstK',_,[]],[]).
metta_defn('&self',['ListFirstK',A,[B,C]],['If',[>,A,0],[B,['ListFirstK',[-,A,1],C]],[]]).
% ;Add event to FIFO
metta_defn('&self',['EventToFIFO',A],[let,B,['ListFirstK',3,[A,['get-state','&FIFO']]],['change-state!','&FIFO',B]]).
% ;Form a sequence of two events
metta_defn('&self',['TemporalSequence',A,['Event',[B,C],[D,E,_]]],[let,['Event',[F,G],[_,H,_]],['Projection',A,D],['Event',[[F,&/,B],['Truth_Intersection',G,C]],[D,['TupleConcat',H,E],[0,0.0]]]]).
% ;Form a temporal implication between two events
metta_defn('&self',['TemporalImplication',A,['Event',[B,C],[D,E,_]]],[let,['Event',[F,G],[_,H,_]],['Projection',A,D],['Event',[[F,=/>,B],['Truth_Induction',G,C]],[D,['TupleConcat',H,E],[0,0.0]]]]).
% ;Whether an event''s term is an operation
metta_defn('&self',['IsOp',['Event',[A,_],_]],[case,A,[[[^,_],'True'],[_,'False']]]).
% ;Find implications in the event FIFO:
% ;procedural implications
metta_defn('&self',['TemporalImplicationInduction',[A,[B,[C,_]]]],['If',[and,['IsOp',B],[and,[not,['IsOp',A]],[not,['IsOp',C]]]],[let,D,['TemporalSequence',C,B],['TemporalImplication',D,A]]]).
% ;and temporal without operation
metta_defn('&self',['TemporalImplicationInduction',[A,[B,_]]],['If',[and,[not,['IsOp',B]],[not,['IsOp',A]]],['TemporalImplication',B,A]]).
% ;Add negative evidence for implications which predicted the input unsuccessfully
metta_defn('&self',['NegConfirmation',A,B,C],[let,['Event',[[A,=/>,D],_],_],['EternalQuestion',[A,=/>,D]],['If',[not,[==,B,D]],['UpdateConcept',['Event',[[A,=/>,D],[0.0,0.1]],[C,[],[0,0.0]]],C]]]).
% ;Check if the implication''s preconditions are met to anticipate the by the implication predicted outcome
metta_atom('&self',[=,['Anticipate',[_,[]],_]]).
metta_defn('&self',['Anticipate',[A,[B,[]]],C],['let*',[[['Event',[D,_],_],B],[['Event',[E,_],_],A]],['If',[not,['IsOp',B]],['NegConfirmation',D,E,C]]]).
metta_defn('&self',['Anticipate',[A,[B,[C,_]]],D],['let*',[[['Event',[E,_],_],C],[['Event',[F,_],_],B],[['Event',[G,_],_],A],[_,[C,&/,'Pos']]],['If',[and,['IsOp',B],[not,['IsOp',C]]],['NegConfirmation',[E,&/,F],G,D]]]).
% ;;Input procedure
metta_defn('&self',['AddBeliefEvent',A],['let*',[[[B,C],['GetReasonerState']],[D,['Event',A,[B,C,[B,1.0]]]]],[do,[sequential,[['EventToFIFO',D],[let,E,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',E,B]],['ProcessBeliefEvent',D,B],['Anticipate',['get-state','&FIFO'],B],['BeliefCycle',B]]]]]).
% ;;Procedural inference (decision making with operation execution and subgoaling).
% ;Derived goal event priority
metta_defn('&self',['SubgoalPriority',A,B],[*,A,['Truth_Expectation',B]]).
% ;Expectation of an operation is the truth expectation of its desire value
metta_defn('&self',['OpExpectation',['Decision',[_,A],_],_],['Truth_Expectation',A]).
% ;Inject executed operation as an event and return its name
metta_defn('&self',['Execute',A],[superpose,[['AddBeliefEvent',[A,[1.0,0.9]]],A]]).
% ;Add subgoals to the PQ
metta_defn('&self',['DeriveSubgoals',A],[do,[let,['Decision',_,B],[superpose,A],['add-atom','&goal_events',B]]]).
% ;execute the operation which most likely gets the goal achieved in current contexts, and if contexts are not yet fulfilled, derive them as subgoals
metta_defn('&self',['BestDecision',A,['Event',[B,C],[_,D,E]],_],[let,F,[collapse,['let*',[[['Event',[[[G,&/,[^,H]],=/>,B],I],[_,J,_]],['EternalQuestion',[[G,&/,[^,H]],=/>,B]]],[K,['Truth_Deduction',C,I]] ,  [['Event',[_,L],_],['Projection',['EventQuestion',G],A]],[M,['Truth_Deduction',L,K]],[N,['Truth_StructuralDeduction',K]],[O,['TupleConcat',D,J]]],['If',['StampDisjoint',O],['Decision',[[^,H],M],['Event',[G,['Truth_StructuralDeduction',K]],[A,O,[A,['SubgoalPriority',['EventPriorityNow',E,A],N]]]]]]]],[let,['Decision',[P,M],_],['BestCandidate',F,'OpExpectation',A],['If',[>,['Truth_Expectation',M],0.5],['Execute',P],['DeriveSubgoals',F]]]]).
% ;;select the highest priority goal event from the PQ and use it for decision making
metta_defn('&self',['GoalCycle',A],[sequential,[[let,B,['SelectHighestPriorityEvent','&goal_events',A],[sequential,[[do,['remove-atom','&goal_events',B]],['BestDecision',A,B,['get-state','&FIFO']]]]],[do,['UpdateReasonerState']],[do,['BoundEvents','&goal_events',0.0,0.1,['GoalEventsMax'],A]]]]).
% ;;Input procedure
metta_defn('&self',['AddGoalEvent',A],['let*',[[[B,C],['GetReasonerState']],[D,['Event',A,[B,C,[B,1.0]]]]],[sequential,[[do,['add-atom','&goal_events',D]],['GoalCycle',B]]]]).
:- metta_eval([print,'$STRING'("NARS test!!!!!!!!!!!!!!!!!!")]).
:- metta_eval(['metta_learner::vspace-main']).
:- metta_eval(['AddBeliefEvent',[[['{',garfield,'}'],-->,cat],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[[['{',garfield,'}'],-->,cat],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[[['{',garfield,'}'],-->,cat],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
:- metta_eval(['AddBeliefEvent',[[[cat,*,sky],-->,like],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[[[cat,*,sky],-->,like],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[[[cat,*,sky],-->,like],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
:- metta_eval(['AddBeliefEvent',[[sky,-->,['$OBJ'(claz_bracket_vector,[blue])]],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[[sky,-->,['$OBJ'(claz_bracket_vector,[blue])]],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[[sky,-->,['$OBJ'(claz_bracket_vector,[blue])]],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
% ;The following question needs both a deduction and abduction step:
:- metta_eval(['EternalQuestion',[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like]]).
metta_defn('&self',['EternalQuestion',[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like]],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like],A,_,_],A],[[B,B],['%void%',['Event',['None',[0.5,0.0]],[eternal,[],0.0]]]]]).
% ;expected: [(Event (((({ garfield }) * ([ blue ])) --> like) (1.0 0.2965825874694874)) (eternal (Cons 2 (Cons 1 (Cons 3 Nil))) 0.643288027761712))]
% ;Lets stress the control mechanism as these type of events with common extension or intension causes dozens of derivations:
:- metta_eval(['AddBeliefEvent',[['A',-->,cat],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[['A',-->,cat],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[['A',-->,cat],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
:- metta_eval(['AddBeliefEvent',[['B',-->,cat],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[['B',-->,cat],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[['B',-->,cat],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
:- metta_eval(['AddBeliefEvent',[['C',-->,cat],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[['C',-->,cat],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[['C',-->,cat],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
:- metta_eval(['EternalQuestion',[['A',&,'B'],-->,cat]]).
metta_defn('&self',['EternalQuestion',[['A',&,'B'],-->,cat]],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',[['A',&,'B'],-->,cat],A,_,_],A],[[B,B],['%void%',['Event',['None',[0.5,0.0]],[eternal,[],0.0]]]]]).
% ;expected: [(Event (((A & B) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 4 (Cons 5 Nil)) (5 0.4525)))]
:- metta_eval(['EternalQuestion',[['B',&,'C'],-->,cat]]).
metta_defn('&self',['EternalQuestion',[['B',&,'C'],-->,cat]],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',[['B',&,'C'],-->,cat],A,_,_],A],[[B,B],['%void%',['Event',['None',[0.5,0.0]],[eternal,[],0.0]]]]]).
% ;expected: [(Event (((B & C) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 5 (Cons 6 Nil)) (6 0.4525)))]
:- metta_eval(['EternalQuestion',[[['A',&,'B'],&,'C'],-->,cat]]).
metta_defn('&self',['EternalQuestion',[[['A',&,'B'],&,'C'],-->,cat]],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',[[['A',&,'B'],&,'C'],-->,cat],A,_,_],A],[[B,B],['%void%',['Event',['None',[0.5,0.0]],[eternal,[],0.0]]]]]).
% ;expected: [(Event ((((A & B) & C) --> cat) (1.0 0.42163100057836905)) (eternal (Cons 5 (Cons 4 (Cons 6 Nil))) (6 0.195593125))).
:- metta_eval(['AddBeliefEvent',[[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like],[1.0,0.9]]]).
metta_defn('&self',['AddBeliefEvent',[[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like],[1.0,0.9]]],['let*',[[[A,B],['GetReasonerState']],[C,['Event',[[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like],[1.0,0.9]],[A,B,[A,1.0]]]]],[do,[sequential,[['EventToFIFO',C],[let,D,['TemporalImplicationInduction',['get-state','&FIFO']],['UpdateConcept',D,A]],['ProcessBeliefEvent',C,A],['Anticipate',['get-state','&FIFO'],A],['BeliefCycle',A]]]]]).
:- metta_eval(['EternalQuestion',[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like]]).
metta_defn('&self',['EternalQuestion',[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like]],[case,[match,[superpose,['&attentional_focus','&concepts']],['Concept',[[['{',garfield,'}'],*,['$OBJ'(claz_bracket_vector,[blue])]],-->,like],A,_,_],A],[[B,B],['%void%',['Event',['None',[0.5,0.0]],[eternal,[],0.0]]]]]).
% ;expected: [(Event (((({ garfield }) * ([ blue ])) --> like) (1.0 0.5692683291397822)) (eternal (Cons 7 (Cons 2 (Cons 1 (Cons 3 Nil)))) 0.0))]
% ;Please notice that it has revised it with the prior derived result, as you can also see in the evidence trail 1,2,3 being included
:- metta_eval(['metta_learner::vspace-main']).
% ;debug:
:- metta_eval(['CollapseCardinality',['get-atoms','&belief_events']]).
metta_defn('&self',['CollapseCardinality',['get-atoms','&belief_events']],['TupleCount',[collapse,['CountElement',['get-atoms','&belief_events']]]]).
metta_defn('&self',['CountElement',['get-atoms','&belief_events']],[case,['get-atoms','&belief_events'],[[_,1]]]).
% ;[8]
:- metta_eval(['CollapseCardinality',['get-atoms','&attentional_focus']]).
metta_defn('&self',['CollapseCardinality',['get-atoms','&attentional_focus']],['TupleCount',[collapse,['CountElement',['get-atoms','&attentional_focus']]]]).
metta_defn('&self',['CountElement',['get-atoms','&attentional_focus']],[case,['get-atoms','&attentional_focus'],[[_,1]]]).
% ;[8]
:- metta_eval(['CollapseCardinality',['get-atoms','&concepts']]).
metta_defn('&self',['CollapseCardinality',['get-atoms','&concepts']],['TupleCount',[collapse,['CountElement',['get-atoms','&concepts']]]]).
metta_defn('&self',['CountElement',['get-atoms','&concepts']],[case,['get-atoms','&concepts'],[[_,1]]]).
% ;[100]
:- metta_eval(['metta_learner::vspace-main']).
% 13,197,556 inferences, 1.885 CPU in 1.885 seconds (100% CPU, 7002657 Lips).
% (=  "./examples/VRUN_tests1.metta" 0).
%```
