% orginal metta 
(= (simple_deduction_strength_formula $As $Bs $Cs $ABs $BCs)  
(if  
   (and    
    (conditional_probability_consistency $As $Bs $ABs)      
   (conditional_probability_consistency $Bs $Cs $BCs))
(if (< 0.99 $Bs)
      $Cs
      (+ (* $ABs $BCs) (/ (* (- 1 $ABs) (- $Cs (* $Bs $BCs))) (- 1 $Bs))  )   )   
   0))		
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
% METTA TRANSPILED 
par_atom_list(equal,[
transpilex(1,simple_deduction_strength_formula( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$ABs" ) , variabel( "$BCs" ) ] ) ),
transpilex(22,conditional( [ 
transpilex( 6 , conjuction( [ 
transpilex( 4 , conditional_probability_consistency( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$ABs" ) ] )  ) , 
transpilex( 5 , conditional_probability_consistency( [ variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$BCs" ) ] )  ) ] )  ) , 
transpilex( 21 , conditional( [ 
transpilex( 8 , smallerthan( [ number(0.99) , variabel( "$Bs" ) ] )  ) , variabel( "$Cs" ) , 
transpilex( 20 , plus( [ 
transpilex( 10 , multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] )  ) , 
transpilex( 19 , division( [ 
transpilex( 17 , multiplication( [ 
transpilex( 13 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , 
transpilex( 16 , minus( [ variabel( "$Cs" ) , 
transpilex( 15 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , 
transpilex( 18 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] )  ) ] )  ) ] )  ) , number(0) ] ) )])
% INTERNAL PARSE INFO 
is_transp(1,namex("simple_deduction_strength_formula"),[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])
is_transp(4,namex("conditional_probability_consistency"),[variabel("$As"),variabel("$Bs"),variabel("$ABs")])
is_transp(5,namex("conditional_probability_consistency"),[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")])
is_transp(6,conjuction,[transpiled(4,"conditional_probability_consistency( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$ABs" ) ] ) "),transpiled(5,"conditional_probability_consistency( [ variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$BCs" ) ] ) ")])
is_transp(8,smallerthan,[number(0.99),variabel("$Bs")])
is_transp(10,multiplication,[variabel("$ABs"),variabel("$BCs")])
is_transp(13,minus,[number(1),variabel("$ABs")])
is_transp(15,multiplication,[variabel("$Bs"),variabel("$BCs")])
is_transp(16,minus,[variabel("$Cs"),transpiled(15,"multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] ) ")])
is_transp(17,multiplication,[transpiled(13,"minus( [ number(1) , variabel( "$ABs" ) ] ) "),transpiled(16,"minus( [ variabel( "$Cs" ) , transpiled( 15 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] ) ")])
is_transp(18,minus,[number(1),variabel("$Bs")])
is_transp(19,division,[transpiled(17,"multiplication( [ transpiled( 13 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 16 , minus( [ variabel( "$Cs" ) , transpiled( 15 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] ) "),transpiled(18,"minus( [ number(1) , variabel( "$Bs" ) ] ) ")])
is_transp(20,plus,[transpiled(10,"multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] ) "),transpiled(19,"division( [ transpiled( 17 , multiplication( [ transpiled( 13 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 16 , minus( [ variabel( "$Cs" ) , transpiled( 15 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , transpiled( 18 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] ) ")])
is_transp(21,conditional,[transpiled(8,"smallerthan( [ number(0.99) , variabel( "$Bs" ) ] ) "),variabel("$Cs"),transpiled(20,"plus( [ transpiled( 10 , multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] )  ) , transpiled( 19 , division( [ transpiled( 17 , multiplication( [ transpiled( 13 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 16 , minus( [ variabel( "$Cs" ) , transpiled( 15 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , transpiled( 18 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] )  ) ] ) ")])
is_transp(22,conditional,[transpiled(6,"conjuction( [ transpiled( 4 , conditional_probability_consistency( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$ABs" ) ] )  ) , transpiled( 5 , conditional_probability_consistency( [ variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$BCs" ) ] )  ) ] ) "),transpiled(21,"conditional( [ transpiled( 8 , smallerthan( [ number(0.99) , variabel( "$Bs" ) ] )  ) , variabel( "$Cs" ) , transpiled( 20 , plus( [ transpiled( 10 , multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] )  ) , transpiled( 19 , division( [ transpiled( 17 , multiplication( [ transpiled( 13 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 16 , minus( [ variabel( "$Cs" ) , transpiled( 15 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , transpiled( 18 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] )  ) ] )  ) ] ) "),number(0)])
