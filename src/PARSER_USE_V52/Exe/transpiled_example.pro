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
% METTA TRANSPILED LEVEL 1
par_atom_list(equal,[
transpilex(45,"simple_deduction_strength_formula( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$ABs" ) , variabel( "$BCs" ) ] ) "),
transpilex(66,"conditional( [ 
transpilex( 50 , conjuction( [ 
transpilex( 48 , conditional_probability_consistency( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$ABs" ) ] )  ) , 
transpilex( 49 , conditional_probability_consistency( [ variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$BCs" ) ] )  ) ] )  ) , 
transpilex( 65 , conditional( [ 
transpilex( 52 , smallerthan( [ number(0.99) , variabel( "$Bs" ) ] )  ) , variabel( "$Cs" ) , 
transpilex( 64 , plus( [ 
transpilex( 54 , multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] )  ) , 
transpilex( 63 , division( [ 
transpilex( 61 , multiplication( [ 
transpilex( 57 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , 
transpilex( 60 , minus( [ variabel( "$Cs" ) , 
transpilex( 59 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , 
transpilex( 62 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] )  ) ] )  ) ] )  ) , number(0) ] ) ")])
% METTA TRANSPILED LEVEL 2
par_atom_list(equal,[
simple_deduction_strength_formula( AS , BS , CS , ABS , BCS ) :-
conditional(  
  conjuction(  
  conditional_probability_consistency( AS , BS , ABS )   ,  
  conditional_probability_consistency( BS , CS , BCS )   )   ,  
  conditional(  
  smallerthan( 0.99 , BS )   , CS ,  
  plus(  
  multiplication( ABS , BCS )   ,  
  division(  
  multiplication(  
  minus( 1 , ABS )   ,  
  minus( CS ,  
  multiplication( BS , BCS )   )   )   ,  
  minus( 1 , BS )   )   )   )   , 0 ) .
  
  
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
is_transp(23,namex("simple_deduction_strength_formula"),[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])
is_transp(26,namex("conditional_probability_consistency"),[variabel("$As"),variabel("$Bs"),variabel("$ABs")])
is_transp(27,namex("conditional_probability_consistency"),[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")])
is_transp(28,conjuction,[transpiled(26,"conditional_probability_consistency( AS , BS , ABS ) "),transpiled(27,"conditional_probability_consistency( BS , CS , BCS ) ")])
is_transp(30,smallerthan,[number(0.99),variabel("$Bs")])
is_transp(32,multiplication,[variabel("$ABs"),variabel("$BCs")])
is_transp(35,minus,[number(1),variabel("$ABs")])
is_transp(37,multiplication,[variabel("$Bs"),variabel("$BCs")])
is_transp(38,minus,[variabel("$Cs"),transpiled(37,"multiplication( BS , BCS ) ")])
is_transp(39,multiplication,[transpiled(35,"minus( 1 , ABS ) "),transpiled(38,"minus( CS ,  ##  multiplication( BS , BCS )   ) ")])
is_transp(40,minus,[number(1),variabel("$Bs")])
is_transp(41,division,[transpiled(39,"multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   ) "),transpiled(40,"minus( 1 , BS ) ")])
is_transp(42,plus,[transpiled(32,"multiplication( ABS , BCS ) "),transpiled(41,"division(  ##  multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   )   ,  ##  minus( 1 , BS )   ) ")])
is_transp(43,conditional,[transpiled(30,"smallerthan( 0.99 , BS ) "),variabel("$Cs"),transpiled(42,"plus(  ##  multiplication( ABS , BCS )   ,  ##  division(  ##  multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   )   ,  ##  minus( 1 , BS )   )   ) ")])
is_transp(44,conditional,[transpiled(28,"conjuction(  ##  conditional_probability_consistency( AS , BS , ABS )   ,  ##  conditional_probability_consistency( BS , CS , BCS )   ) "),transpiled(43,"conditional(  ##  smallerthan( 0.99 , BS )   , CS ,  ##  plus(  ##  multiplication( ABS , BCS )   ,  ##  division(  ##  multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   )   ,  ##  minus( 1 , BS )   )   )   ) "),number(0)])
is_transp(45,namex("simple_deduction_strength_formula"),[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])
is_transp(48,namex("conditional_probability_consistency"),[variabel("$As"),variabel("$Bs"),variabel("$ABs")])
is_transp(49,namex("conditional_probability_consistency"),[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")])
is_transp(50,conjuction,[transpiled(48,"conditional_probability_consistency( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$ABs" ) ] ) "),transpiled(49,"conditional_probability_consistency( [ variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$BCs" ) ] ) ")])
is_transp(52,smallerthan,[number(0.99),variabel("$Bs")])
is_transp(54,multiplication,[variabel("$ABs"),variabel("$BCs")])
is_transp(57,minus,[number(1),variabel("$ABs")])
is_transp(59,multiplication,[variabel("$Bs"),variabel("$BCs")])
is_transp(60,minus,[variabel("$Cs"),transpiled(59,"multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] ) ")])
is_transp(61,multiplication,[transpiled(57,"minus( [ number(1) , variabel( "$ABs" ) ] ) "),transpiled(60,"minus( [ variabel( "$Cs" ) , transpiled( 59 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] ) ")])
is_transp(62,minus,[number(1),variabel("$Bs")])
is_transp(63,division,[transpiled(61,"multiplication( [ transpiled( 57 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 60 , minus( [ variabel( "$Cs" ) , transpiled( 59 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] ) "),transpiled(62,"minus( [ number(1) , variabel( "$Bs" ) ] ) ")])
is_transp(64,plus,[transpiled(54,"multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] ) "),transpiled(63,"division( [ transpiled( 61 , multiplication( [ transpiled( 57 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 60 , minus( [ variabel( "$Cs" ) , transpiled( 59 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , transpiled( 62 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] ) ")])
is_transp(65,conditional,[transpiled(52,"smallerthan( [ number(0.99) , variabel( "$Bs" ) ] ) "),variabel("$Cs"),transpiled(64,"plus( [ transpiled( 54 , multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] )  ) , transpiled( 63 , division( [ transpiled( 61 , multiplication( [ transpiled( 57 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 60 , minus( [ variabel( "$Cs" ) , transpiled( 59 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , transpiled( 62 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] )  ) ] ) ")])
is_transp(66,conditional,[transpiled(50,"conjuction( [ transpiled( 48 , conditional_probability_consistency( [ variabel( "$As" ) , variabel( "$Bs" ) , variabel( "$ABs" ) ] )  ) , transpiled( 49 , conditional_probability_consistency( [ variabel( "$Bs" ) , variabel( "$Cs" ) , variabel( "$BCs" ) ] )  ) ] ) "),transpiled(65,"conditional( [ transpiled( 52 , smallerthan( [ number(0.99) , variabel( "$Bs" ) ] )  ) , variabel( "$Cs" ) , transpiled( 64 , plus( [ transpiled( 54 , multiplication( [ variabel( "$ABs" ) , variabel( "$BCs" ) ] )  ) , transpiled( 63 , division( [ transpiled( 61 , multiplication( [ transpiled( 57 , minus( [ number(1) , variabel( "$ABs" ) ] )  ) , transpiled( 60 , minus( [ variabel( "$Cs" ) , transpiled( 59 , multiplication( [ variabel( "$Bs" ) , variabel( "$BCs" ) ] )  ) ] )  ) ] )  ) , transpiled( 62 , minus( [ number(1) , variabel( "$Bs" ) ] )  ) ] )  ) ] )  ) ] ) "),number(0)])
is_transp(67,namex("simple_deduction_strength_formula"),[variabel("$As"),variabel("$Bs"),variabel("$Cs"),variabel("$ABs"),variabel("$BCs")])
is_transp(70,namex("conditional_probability_consistency"),[variabel("$As"),variabel("$Bs"),variabel("$ABs")])
is_transp(71,namex("conditional_probability_consistency"),[variabel("$Bs"),variabel("$Cs"),variabel("$BCs")])
is_transp(72,conjuction,[transpiled(70,"conditional_probability_consistency( AS , BS , ABS ) "),transpiled(71,"conditional_probability_consistency( BS , CS , BCS ) ")])
is_transp(74,smallerthan,[number(0.99),variabel("$Bs")])
is_transp(76,multiplication,[variabel("$ABs"),variabel("$BCs")])
is_transp(79,minus,[number(1),variabel("$ABs")])
is_transp(81,multiplication,[variabel("$Bs"),variabel("$BCs")])
is_transp(82,minus,[variabel("$Cs"),transpiled(81,"multiplication( BS , BCS ) ")])
is_transp(83,multiplication,[transpiled(79,"minus( 1 , ABS ) "),transpiled(82,"minus( CS ,  ##  multiplication( BS , BCS )   ) ")])
is_transp(84,minus,[number(1),variabel("$Bs")])
is_transp(85,division,[transpiled(83,"multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   ) "),transpiled(84,"minus( 1 , BS ) ")])
is_transp(86,plus,[transpiled(76,"multiplication( ABS , BCS ) "),transpiled(85,"division(  ##  multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   )   ,  ##  minus( 1 , BS )   ) ")])
is_transp(87,conditional,[transpiled(74,"smallerthan( 0.99 , BS ) "),variabel("$Cs"),transpiled(86,"plus(  ##  multiplication( ABS , BCS )   ,  ##  division(  ##  multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   )   ,  ##  minus( 1 , BS )   )   ) ")])
is_transp(88,conditional,[transpiled(72,"conjuction(  ##  conditional_probability_consistency( AS , BS , ABS )   ,  ##  conditional_probability_consistency( BS , CS , BCS )   ) "),transpiled(87,"conditional(  ##  smallerthan( 0.99 , BS )   , CS ,  ##  plus(  ##  multiplication( ABS , BCS )   ,  ##  division(  ##  multiplication(  ##  minus( 1 , ABS )   ,  ##  minus( CS ,  ##  multiplication( BS , BCS )   )   )   ,  ##  minus( 1 , BS )   )   )   ) "),number(0)])
