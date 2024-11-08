% METTA CLAUSE 
(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs)  
(if  
   (and    
    (conditional_probability_consistency $As $Bs $ABs)      
   (conditional_probability_consistency $Bs $Cs $BCs))
 (if (< 0.99 $Bs)
       $Cs
       (+ (* $ABs $BCs) (/ (* (- 1 $ABs) (- $Cs (* $Bs $BCs))) (- 1 $Bs))  )   )   
  
   0))		
% PROLOG VERSION OF THIS METTA CLAUSE
% 0 1.
simple_deduction_strength_formula( AS , BS , CS , ABS , BCS ) :-
 conditional(  ABS , BS , AS , CS , BCS  ,  , 0 ) .
% 22 0.
conditional(  ABS , BS , AS , CS , BCS  ,  , 0 ) :-
 conjuction(  BS , AS , ABS  ,  CS , BCS  ) , conditional(  BS  , CS ,  BCS , ABS , CS  ) , number(0).
% 6 0.
conjuction(  BS , AS , ABS  ,  CS , BCS  ) :-
 conditional_probability_consistency( AS , BS , ABS ) , conditional_probability_consistency( BS , CS , BCS ) .
% 21 0.
conditional(  BS  , CS ,  BCS , ABS , CS  ) :-
 smallerthan( 0.99 , BS ) , plus(  ABS , BCS  ,  BS , CS  ) .
% 20 0.
plus(  ABS , BCS  ,  BS , CS  ) :-
 multiplication( ABS , BCS ) , division(  ABS , BS , BCS , CS  ,  ) .
% 19 0.
division(  ABS , BS , BCS , CS  ,  ) :-
 multiplication(  ABS  ,  CS , BS , BCS  ) , minus( 1 , BS ) .
% 17 0.
multiplication(  ABS  ,  CS , BS , BCS  ) :-
 minus( 1 , ABS ) , minus( CS ,  BS , BCS  ) .
% 16 0.
minus( CS ,  BS , BCS  ) :-
 multiplication( BS , BCS ) .
 
 
 % METTA HEADSUB

head_subs(1,[22],["conditional(  ABS , BS , AS , CS , BCS  ,  , 0 ) "])
head_subs(22,[21,6],["conditional(  BS  , CS ,  BCS , ABS , CS  ) ","conjuction(  BS , AS , ABS  ,  CS , BCS  ) "])
head_subs(6,[5,4],["conditional_probability_consistency( BS , CS , BCS ) ","conditional_probability_consistency( AS , BS , ABS ) "])
head_subs(21,[20,8],["plus(  ABS , BCS  ,  BS , CS  ) ","smallerthan( 0.99 , BS ) "])
head_subs(20,[19,10],["division(  ABS , BS , BCS , CS  ,  ) ","multiplication( ABS , BCS ) "])
head_subs(19,[18,17],["minus( 1 , BS ) ","multiplication(  ABS  ,  CS , BS , BCS  ) "])
head_subs(17,[16,13],["minus( CS ,  BS , BCS  ) ","minus( 1 , ABS ) "])
head_subs(16,[15],["multiplication( BS , BCS ) "])
% INTERNAL TRANSPILED INFO 1 
is_trpile(44,1,namex("simple_deduction_strength_formula"),[],[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")],[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])
is_trpile(44,22,conditional,[6,21],[numf(6),numf(21),number(0)],[transpiled(6,"conjuction( ["$Bs","$As","$ABs"] , ["$Cs","$BCs"] ) "),transpiled(21,"conditional( ["$Bs"] , CS , ["$BCs","$ABs","$Cs"] ) "),number(0)])
is_trpile(44,6,conjuction,[4,5],[numf(4),numf(5)],[transpiled(4,"conditional_probability_consistency( AS , BS , ABS ) "),transpiled(5,"conditional_probability_consistency( BS , CS , BCS ) ")])
is_trpile(44,21,conditional,[8,20],[numf(8),variabel("$Cs"),numf(20)],[transpiled(8,"smallerthan( 0.99 , BS ) "),variabel("$Cs"),transpiled(20,"plus( ["$ABs","$BCs"] , ["$Bs","$Cs"] ) ")])
is_trpile(44,4,namex("conditional_probability_consistency"),[],[variabel("$As"),variabel("$Bs"),variabel("$ABs")],[variabel("$As"),variabel("$Bs"),variabel("$ABs")])
is_trpile(44,5,namex("conditional_probability_consistency"),[],[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")],[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")])
is_trpile(44,8,smallerthan,[],[number(0.99),variabel("$Bs")],[number(0.99),variabel("$Bs")])
is_trpile(44,20,plus,[10,19],[numf(10),numf(19)],[transpiled(10,"multiplication( ABS , BCS ) "),transpiled(19,"division( ["$ABs","$Bs","$BCs","$Cs"] , [] ) ")])
is_trpile(44,10,multiplication,[],[variabel("$ABs"),variabel("$BCs")],[variabel("$ABs"),variabel("$BCs")])
is_trpile(44,19,division,[17,18],[numf(17),numf(18)],[transpiled(17,"multiplication( ["$ABs"] , ["$Cs","$Bs","$BCs"] ) "),transpiled(18,"minus( 1 , BS ) ")])
is_trpile(44,17,multiplication,[13,16],[numf(13),numf(16)],[transpiled(13,"minus( 1 , ABS ) "),transpiled(16,"minus( CS , ["$Bs","$BCs"] ) ")])
is_trpile(44,18,minus,[],[number(1),variabel("$Bs")],[number(1),variabel("$Bs")])
is_trpile(44,13,minus,[],[number(1),variabel("$ABs")],[number(1),variabel("$ABs")])
is_trpile(44,16,minus,[15],[variabel("$Cs"),numf(15)],[variabel("$Cs"),transpiled(15,"multiplication( BS , BCS ) ")])
is_trpile(44,15,multiplication,[],[variabel("$Bs"),variabel("$BCs")],[variabel("$Bs"),variabel("$BCs")])
% INTERNAL PARSE INFO 2 
is_transp(44,1,namex("simple_deduction_strength_formula"),[],[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")],[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])
is_transp(44,4,namex("conditional_probability_consistency"),[],[variabel("$As"),variabel("$Bs"),variabel("$ABs")],[variabel("$As"),variabel("$Bs"),variabel("$ABs")])
is_transp(44,5,namex("conditional_probability_consistency"),[],[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")],[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")])
is_transp(44,6,conjuction,[4,5],[numf(4),numf(5)],[transpiled(4,"conditional_probability_consistency( AS , BS , ABS ) "),transpiled(5,"conditional_probability_consistency( BS , CS , BCS ) ")])
is_transp(44,8,smallerthan,[],[number(0.99),variabel("$Bs")],[number(0.99),variabel("$Bs")])
is_transp(44,10,multiplication,[],[variabel("$ABs"),variabel("$BCs")],[variabel("$ABs"),variabel("$BCs")])
is_transp(44,13,minus,[],[number(1),variabel("$ABs")],[number(1),variabel("$ABs")])
is_transp(44,15,multiplication,[],[variabel("$Bs"),variabel("$BCs")],[variabel("$Bs"),variabel("$BCs")])
is_transp(44,16,minus,[15],[variabel("$Cs"),numf(15)],[variabel("$Cs"),transpiled(15,"multiplication( BS , BCS ) ")])
is_transp(44,17,multiplication,[13,16],[numf(13),numf(16)],[transpiled(13,"minus( 1 , ABS ) "),transpiled(16,"minus( CS , transx(- 15 , [variabel("$Bs"),variabel("$BCs")] -) ) ")])
is_transp(44,18,minus,[],[number(1),variabel("$Bs")],[number(1),variabel("$Bs")])
is_transp(44,19,division,[17,18],[numf(17),numf(18)],[transpiled(17,"multiplication( transx(- 13 , [number(1),variabel("$ABs")] -) , transx(- 16 , [variabel("$Cs"),numf(15)] -) ) "),transpiled(18,"minus( 1 , BS ) ")])
is_transp(44,20,plus,[10,19],[numf(10),numf(19)],[transpiled(10,"multiplication( ABS , BCS ) "),transpiled(19,"division( transx(- 17 , [numf(13),numf(16)] -) , transx(- 18 , [number(1),variabel("$Bs")] -) ) ")])
is_transp(44,21,conditional,[8,20],[numf(8),variabel("$Cs"),numf(20)],[transpiled(8,"smallerthan( 0.99 , BS ) "),variabel("$Cs"),transpiled(20,"plus( transx(- 10 , [variabel("$ABs"),variabel("$BCs")] -) , transx(- 19 , [numf(17),numf(18)] -) ) ")])
is_transp(44,22,conditional,[6,21],[numf(6),numf(21),number(0)],[transpiled(6,"conjuction( transx(- 4 , [variabel("$As"),variabel("$Bs"),variabel("$ABs")] -) , transx(- 5 , [variabel("$Bs"),variabel("$Cs"),variabel("$BCs")] -) ) "),transpiled(21,"conditional( transx(- 8 , [number(0.99),variabel("$Bs")] -) , CS , transx(- 20 , [numf(10),numf(19)] -) ) "),number(0)])
% METTA PARSE TREE 
par_atom_list(equal,[
metta_sx(par_atom_list(namex("simple_deduction_strength_formula"),[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])),
metta_sx(par_atom_list(conditional,[
metta_sx(par_atom_list(conjuction,[
metta_sx(par_atom_list(namex("conditional_probability_consistency"),[variabel("$As"),variabel("$Bs"),variabel("$ABs")])),
metta_sx(par_atom_list(namex("conditional_probability_consistency"),[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")]))])),
metta_sx(par_atom_list(conditional,[
metta_sx(par_atom_list(smallerthan,[number(0.99),variabel("$Bs")])),variabel("$Cs"),
metta_sx(par_atom_list(plus,[
metta_sx(par_atom_list(multiplication,[variabel("$ABs"),variabel("$BCs")])),
metta_sx(par_atom_list(division,[
metta_sx(par_atom_list(multiplication,[
metta_sx(par_atom_list(minus,[number(1),variabel("$ABs")])),
metta_sx(par_atom_list(minus,[variabel("$Cs"),
metta_sx(par_atom_list(multiplication,[variabel("$Bs"),variabel("$BCs")]))]))])),
metta_sx(par_atom_list(minus,[number(1),variabel("$Bs")]))]))]))])),number(0)]))])
