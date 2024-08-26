% do():-   get_time(Stamp), stamp_day_atom(Stamp, Dat), do2(Dat),!.
%do2(DaStr):-
  % date_time_stamp(date(2006,7,214,0,0,0,0,-,-), Stamp),
  %get_time(Stamp),
%   stamp_date_time(Stamp, D, 0),FnAbs =
%   date_time_value(date, D, Date),
%   print(Date), fail.
% Date = date(2007, 1, 30).

:- dynamic found_search_result/4.

:- use_module(library(dcg/basics)).


today_key(Dk):-  get_time(Stamp), stamp_day_atom(Stamp, Dk),!.


%number_zero2(Y, Y2):- atom_number(Ya, Y), atom_length(Ya, Le), Le == 1,!, atom_concat(0, Ya, Y2).
%number_zero2(Y, Ya):- atom_number(Ya, Y),!.
number_zero2(Y, Y2):-  atom_length(Y, Le), Le == 1,!, atom_concat(0, Y, Y2).
number_zero2(Y, Y):- !.
%number_zero2(Y, 11):- number(Y),!.
%number_zero2(Y, 12):- atom(Y),!.
stamp_day_atom(Stamp, Dat):- stamp_date_time(Stamp, D, 0),
 date_time_value(date, D, Dx), Dx = date(Y,M,Dag),
 number_zero2(M, M2), number_zero2(Dag, Dag2),
 atomic_list_concat([Y,M2,Dag2], Dat),!.
stamp_day_atom(_, 00):- !.


% atom_number(Sta2, Sta),
add_file_to_zip(Sta, Gma, El):-  atom_concat('totgs_',Sta, Xu), atom_concat(Xu,'.zip', Cx),
 atomic_list_concat(['zip ',Cx,' /var/www/html/',Gma,El], Bg), atom_string(Bg, Str), shell(Str).
% DUS AANROEP MET bvb do(20190519).

% today_key(Dk):-
do(Sta):- retractall(file(_,_,_)),
 Dir = '/var/www/html/', gmap(Gma), atom_concat(Dir, Gma, Fdir),
 directory_files(Fdir, Lis), member(El, Lis), El \= '..', El \= '.',
 atom_concat(Fdir, El, Cx), time_file(Cx, Stamp),
 stamp_day_atom(Stamp, Dat), atom_number(Dat, Datnum),
 Datnum > Sta, assert(file(Gma, El, Dat)), fail.
% stamp_date_time(Stamp, D, 0),
% date_time_value(date, D, Date),
do(_Sta):-   today_key(Dk),
 file(Gma, El, Dat),
 format(Gma), format(El), format(Dat), nl,
 add_file_to_zip(Dk, Gma, El), fail.
do(_):- !.
% zip -r compressed_filename.zip foldername

%  directory_member(Dx, Item, [ recursive(true)]),
%--------

:- dynamic dir_level/10.
:- discontiguous dir_level/10.
:- dynamic file_level/11.
:- discontiguous file_level/11.

% htdoc_m_app
% 1 is alleen met de apps , 0 is alleen gym files 

%--
special_directory_files('H:/metta-wam-main/src/canary/', [ 'metta_eval.pl' , 'metta_interp.pl' , 'metta_ontology.pfc.pl' ]):-!.
 % 

special_directory_files(Dx, Lis):- !, directory_files(Dx, Lis).

%----

get_all_prolog_files( Tp, _, Dx):- retractall(dir_level(_, _, _,_, _,_,_,_,_,_)), retractall(file_level(_, _, _, _,_, _,_,_,_,_,_)),

 directory_files(Dx, Lis), member(El, Lis), El \= '..', El \= '.',
 allow_dir(Tp, Dx, El),
 atom_concat(Dx, El, Cx), exists_directory(Cx),
 assert( dir_level(1, El, '', '',   '','','','','','') ), fail, !.

get_all_prolog_files( Tp, _, Dx):-  
 dir_level( 1, Item, _, _, _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' ], Cy ), 
 directory_files( Cy, Lis ), 
 member( El, Lis ), El \= '..', El \= '.', 
 allow_dir(Tp, Cy, El),
 atom_concat( Cy, El, Cxy ), exists_directory( Cxy ),
 assert( dir_level( 2, Item, El, '',  '', '', '', '', '', '' ) ), fail, !.


% dir_level(2,'PR_12_NvoDesign',tools,'','','','','','','').

get_all_prolog_files( Tp, _, Dx ):-   dir_level( 2, Item, Item2, _, _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',  
 allow_dir(Tp, Cy, El),

 atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 3, Item, Item2, El, '',  '', '', '', '', '' ) ), fail, !.

get_all_prolog_files( Tp, _, Dx):-   dir_level( 3, Item, Item2, Item3, _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/', Item3, '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',  
 allow_dir(Tp, Cy, El),

 atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 4, Item, Item2, Item3, El, '', '', '', '', '' ) ), fail, !.


get_all_prolog_files( Tp, _, Dx):-   dir_level( 4, Item, Item2, Item3, Item4, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/', Item3, '/', Item4, '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',  
 allow_dir(Tp, Cy, El),

 atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 5, Item, Item2, Item3, Item4, El, '', '', '', '' ) ), fail, !.

get_all_prolog_files( Tp, _, Dx):-   dir_level( 5, Item, Item2, Item3, Item4, Item5, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/', Item3, '/', Item4, '/', Item5, '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',
 allow_dir(Tp, Cy, El),

   atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 6, Item, Item2, Item3, Item4, Item5, El, '', '', '' ) ), fail, !.

% canary
get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 %  dir_level(1, Item, _,_, _,_,_,_,_,_), atomic_list_concat([ Dx, Item, '/'], Cy), 
 special_directory_files(Dx, Lis), 
 % 'H:/metta-wam-main/src/canary/'
 member(El, Lis), El \= '..', El \= '.',  
 % write( El ), nl,
 is_prolog_atom_file( El, Dx ),
  atom_concat(Dx, El, Cx), time_file(Cx, Stamp), stamp_day_atom(Stamp, Dat),
 match_date_atom(Dat, Is_mdf_day_after),
 assert( file_level(0, Dx, '', '','','','','','', El, Dat) ), fail,!.
% assert( file_level(0, Item, '', '','','','','','', El, Dat) ), fail,!.


get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 dir_level(1, Item, _,_, _,_,_,_,_,_), atomic_list_concat([ Dx, Item, '/'], Cy), directory_files(Cy, Lis), 
 member(El, Lis), El \= '..', El \= '.',  

 is_prolog_atom_file( El, Item ),
  atom_concat(Cy, El, Cx), time_file(Cx, Stamp), stamp_day_atom(Stamp, Dat),
 match_date_atom(Dat, Is_mdf_day_after),
 assert( file_level(1, Item, '', '','','','','','', El, Dat) ), fail,!.


get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 dir_level(2, Item, Sub, _,  _, _, _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', 

 is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 2, Item, Sub, '', '', '', '', '', '', El, Dat ) ), fail,!.

get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 dir_level(3, Item, Item2, Sub,  _, _, _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 3, Item, Item2, Sub, '', '', '', '', '', El, Dat ) ), fail,!.

get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 dir_level(4, Item, Item2, Item3, Sub,  _, _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 4, Item, Item2, Item3, Sub, '', '', '', '', El, Dat ) ), fail,!.

get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 dir_level(5, Item, Item2, Item3, Item4, Sub,  _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Item4, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 5, Item, Item2, Item3, Item4, Sub, '', '', '', El, Dat ) ), fail,!.

get_all_prolog_files(_, Is_mdf_day_after, Dx):- 
 dir_level(6, Item, Item2, Item3, Item4, Item5, Sub,   _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Item4, '/', Item5,  '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 6, Item, Item2, Item3, Item4, Item5, Sub, '', '', El, Dat ) ), fail,!.


get_all_prolog_files(_, _, _):- 
 write("start writing to data/htm_file_list.pl\n"),
 tell('data/htm_file_list.pl'), 
 write(":- dynamic dir_level/10."), nl,
 write(":- discontiguous dir_level/10."), nl,
 write(":- dynamic file_level/11."), nl,
 write(":- discontiguous file_level/11."), nl,
 fail.

get_all_prolog_files(_, _, _):- 
 dir_level(1, Item, Sub, X,   A,B,C,D,E,F),
 write_term(dir_level(1, Item, Sub, X, A,B,C,D,E,F),[ quoted(true) ]), write("."), nl, fail,!.

get_all_prolog_files(_,_, _):- 
 dir_level(2, Item, Sub, X,  A,B,C,D,E,F),
 write_term(dir_level(2, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail,!.

get_all_prolog_files(_,_, _):- 
 dir_level(3, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(3, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail,!.

get_all_prolog_files(_,_, _):- 
 dir_level(4, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(4, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail,!.

get_all_prolog_files(_,_, _):- 
 dir_level(5, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(5, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail,!.

get_all_prolog_files(_,_, _):- 
 dir_level(6, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(6, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail,!.


get_all_prolog_files(_,_, _):- retract( file_level(0, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(0, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.

get_all_prolog_files(_,_, _):- retract( file_level(1, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(1, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.
get_all_prolog_files(_,_, _):- retract( file_level(2, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(2, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.
get_all_prolog_files(_,_, _):- retract( file_level(3, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(3, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.
get_all_prolog_files(_,_,_):- retract( file_level(4, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(4, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.
get_all_prolog_files(_,_,_):- retract( file_level(5, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(5, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.
get_all_prolog_files(_,_,_):- retract( file_level(6, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(6, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail,!.


get_all_prolog_files( _, _, _ ):- told(),
  write( "finished writing to data/htm_file_list.pl\n" ),
!.

% hier dus  Als het JS is  opschoon comment er uit 
% ALS het  htm is ?versie=  
% 
%   <script language="javascript" type="text/javascript" src="rjstool.js?vers=12" > </script>
% copyr(),
% 1 is htm 2 is js 
wr_newlin(0):- !.
wr_newlin(_):- !,nl.

update_line( Lx , 1, Nwline, 1):- Tag = "?vers=", string_length(Tag, Le), string_length(Lx, Lex),
 sub_string(Lx, P,_,_, Tag), sub_string(Lx, 0, P, _, Sub1), Y is P + Le + 2, Y2 is Lex - Y,
 sub_string(Lx, Y, Y2, _, Sub2),!, string_concat(Sub1, "?vers=", C1),
 string_concat(C1, "41", C2), string_concat(C2, Sub2, Nwline),!.

update_line( Lx , 1, Nwline, 1):- Tag = "?versie=", string_length(Tag, Le), string_length(Lx, Lex),
 sub_string(Lx, P,_,_, Tag), sub_string(Lx, 0, P, _, Sub1), Y is P + Le + 2, Y2 is Lex - Y,
 sub_string(Lx, Y, Y2, _, Sub2),!, string_concat(Sub1, "?versie=", C1),
 string_concat(C1, "41", C2), string_concat(C2, Sub2, Nwline),!.

update_line( Lx , 2, "", 0):- sub_string(Lx, P, _, _, "//"), P < 5,!.
update_line( Lx , _, Lx, 1):-!.

update_file00(Sea, Tp):- not(at_end_of_stream(Sea)),
 read_line_to_string(Sea, Lx), Lx \= end_of_file, 
  update_line( Lx , Tp, Nwline, Resu),!, write(Nwline ),  wr_newlin(Resu),
   update_file00(Sea, Tp).
update_file00(Sea, Tp):- not(at_end_of_stream(Sea)),!, update_file00(Sea, Tp).
update_file00(_ ,_):- !.

update_file(From, To, Tp):-  open(From, read, Sea ), tell(To),
 update_file00(Sea, Tp), told(),!, close( Sea ).
update_file(_, _, _):- !.

% write(From), nl, write(To), nl,

% :- dynamic found_base_teur/4.


read_has_search_strings( From, Zk, Level, Is_include ):- 
  sub_atom( From, _, _, _, '.metta' ), 
  size_file( From ,  Size ), Size < 700, 
  read_file_to_string(From, Bstr, [] ), !,
  if_extra_string_demand_search( Bstr ),
  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 ).

read_has_search_strings( From, Zk, Level, Is_include ):- 
  sub_atom( From, _, _, _, '.py' ), 
  size_file( From ,  Size ), Size < 700, 
  read_file_to_string(From, Bstr, [] ), !,
  if_extra_string_demand_search( Bstr ),
  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 ).

read_has_search_strings( From, Zk, Level, Is_include ):- 
  sub_atom( From, _, _, _, '.rs' ), 
  size_file( From ,  Size ), Size < 700, 
  read_file_to_string(From, Bstr, [] ), !,
  if_extra_string_demand_search( Bstr ),
  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 ).


read_has_search_strings( From, Zk, Level, Is_include ):- 
 retractall( has_db_header( _, _, _, _, _ ) ),
 retractall( com_start() ),
 open( From, read, Sea ), 
 read_has_search_stream( Sea, Zk, From, Level, 1, Is_include ),  
 close( Sea ).

read_has_search_strings( _, _, _, _ ):- !.




% als we deze weg ha;en dan copieert hij de file 
kopie_htm_file(From, To, is_touch_clean_files):-  exists_file(From),
 sub_atom(From, _, _, _, '.htm'), update_file( From, To, 1),!.
kopie_htm_file(From, To, is_touch_clean_files):-  exists_file(From),
 sub_atom(From, _, _, _, '.js'), update_file( From, To, 2),!.

kopie_htm_file(From, To, _):-  exists_file(From),
 downcase_atom(To, To2),
 copy_file( From, To2),!.



 
 
%---
%read_all_prolog_files(_Tp, _, _Isalways_copy, _Is_update, _, _):- 
% Xfi = 'data/htm_file_list.pl',
% retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ), 
% retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
% consult( Xfi ), 
% write("Consulted\n"), write(Xfi),
% fail.

% separate_prolog_code( Zk, Dirx_walk ),
read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, _Dx, Is_include):-  
 file_level(0, Item, _, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Item,  Xf ], From ), 
% write("44debok"), write(),

 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 0, Is_include ), fail.

read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include):-  
 file_level(1, Item, _, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 1, Is_include ), fail.

read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include):-  
 file_level(2, Item, Sub, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Sub, '/', Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 2, Is_include ), fail.


read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include):-  
 file_level(3, Item, Item2, Sub,  _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 3, Is_include ), fail.

read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include):-  
 file_level(4, Item, Item2, Item3, Sub,   _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 4, Is_include ), fail.

read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include):-  
 file_level(5, Item, Item2, Item3, Item4, Sub,    _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Item4, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 5, Is_include ), fail.

read_all_prolog_files(_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include):- 
 file_level(6, Item, Item2, Item3, Item4, Item5, Sub,   _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Item4, '/', Item5, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( From, Zk, 6, Is_include ), fail.


% HIER NOG LEVEL   4  5  6  7
% assert( file_level( 2, Item, Sub, El, Dat,  '', '', '', '', '', '' ) ), fail,!.



%read_all_prolog_files(Tp, Isalways_copy, Is_update):- 
% prolog_new_dir(Tp, Is_update, Tgdir), prolog_src_dir(Dx),
% file_level(2, Item, Sub, Xf, _,  _,_,_,_,_,_),
% atomic_list_concat([ Tgdir, Item, '/', Sub, '/', Xf], To), 
% atomic_list_concat([ Dx, Item, '/', Sub, '/', Xf], From), 
% kopie_htm_file(From, To, Isalways_copy), fail.

read_all_prolog_files( _, _, _, _, _, _ ):-  !.

% 1 is alleen met de apps , 0 is alleen gym files 
% % today_key(Dk):-
%do(Sta):- retractall(file(_,_,_)),
% Dir = '/var/www/html/', gmap(Gma), atom_concat(Dir, Gma, Fdir),
% directory_files(Fdir, Lis), member(El, Lis), El \= '..', El \= '.',
% atom_concat(Fdir, El, Cx), time_file(Cx, Stamp),
% stamp_day_atom(Stamp, Dat), atom_number(Dat, Datnum),
% Datnum > Sta,



% hier dus zelf de mappen aanpassen  voor waar je naartoe wilt kopieeren 
% htm_new_dir(0, is_mk_update, 'G:/htdoc_mk_update/'):-!.
% htm_new_dir(0, _, 'G:/htdoc_mk/'):-!.
% htm_new_dir(1, _, 'G:/htdoc_xm_app/'):-!.

prolog_new_dir(0, is_mk_update, 'F:/htdoc_mk_update/'):-!.
prolog_new_dir(0, _, 'F:/htdoc_mk/'):-!.
prolog_new_dir(1, _, 'F:/htdoc_xm_app/'):-!.

% aanroep: vb : start_analyse(0, '20210608', is_mk_update).
% aanroep: vb : start_analyse(0, '20210810', is_mk_update).
% aanroep: vb : start_analyse(0, '20211010', is_mk_update).
% aanroep: vb : start_analyse(0, '', is_mk_update).

% C:\Users\Renee\Documents\jdklog\sources\PR_12_NvoDesign\tools


match_date_atom( _ , _ ):- !.
% MATCH ALL  Prolog JDKlog
match_date_atom( _Dat, '' ):- !.
match_date_atom( Dat, Is_mdf_day_after ):- atom_number( Dat, Nu1 ), atom_number( Is_mdf_day_after, Nu2 ), 
 Nu1 >= Nu2,!.

:- dynamic com_start/0.

not_is_comment(Lx):-
 sub_string( Lx, Sta, _, _, "%"), Sta < 5,!, fail.
not_is_comment(_):- !.

% gaat fout bij genest 
zet_comment_started(Lx):-
 sub_string( Lx, _, _, _, "/*"),!,  assert( com_start() ).
zet_comment_started(_):-!.

zet_comment_ended(Lx):-   sub_string( Lx, _, _, _, "*/"),   retractall(  com_start() ),   !. 
zet_comment_ended(_):- !.
 is_not_inside_comment():- com_start(),!, fail.
is_not_inside_comment():- !.

search_and( _, [] ):- !.
search_and( Lx, [ H | Lis_and ] ):-
    sub_string( Lx, _, _, _, H ),!,
 search_and( Lx,  Lis_and  ).


search_or(Lx, [ H | _Lis_or]):-  
  split_string(H, "&", "", Lis_and),
   search_and( Lx, Lis_and ),!.
%  sub_string( Lx, _, _, _, H ),!.
search_or(Lx, [ _H | Lis_or]):-  !, search_or(Lx,  Lis_or ).


search_o_y(Lx, Zk):- !,
 string_lower( Lx, Lx2 ),
   split_string(Zk, ",", "", Lis_or),
   search_or(Lx2, Lis_or).

 %  sub_string( Lx2, _, _, _, Zk ),!,

%prolog_src_dir( 'C:/Users/Renee/Documents/jdklog/sources/PR_12_NvoDesign/tools/' ):- !.
% prolog_src_dir( 'C:/Users/Renee/Documents/jdklog/sources/PR_12_NvoDesign/' ):- !.

%   C:/Users/Renee/Documents/jdklog/swi_pilote_analyse_upd_prolog
%  [prolog_analyse_update].
%  start_analyse( 0, '', is_mk_update ).

% k_div
%string_lower( Lx, Lx2 ),
 %  sub_string( Lx2, _, _, _, Zk ),!,





:- dynamic found_include_file/11.

str_part_after_tag(Str, Tag, AfterStr):-
 string_length(Str, Sle),
 string_length(Tag, Tagl),
 sub_string(Str, Beg, _ , _, Tag),
 Beg2 is Beg + Tagl,
 Lx is Sle - Beg2,
 sub_string(Str,  Beg2 , Lx, _,  AfterStr),!.


str_part_before_tag(At, Tag, Pa_before):- atom_length(Tag, TagLe), TagLe > 0,  sub_atom(At, Bg, _, _, Tag),
 sub_atom(At, 0, Bg, _, Pa_before),!.

str_replace_tag(At, Repwhat, RepWith, Resu):-  str_part_before_tag(At, Repwhat, Pa_before), str_part_after_tag(At, Repwhat, Pa_aft),!,
 string_concat(Pa_before,RepWith, C1), string_concat(C1,Pa_aft, Resu).
str_replace_tag(At, _,_, At):- !.



sub_str_between( Str, Btag, Etag, Subz ):-
 string_length( Str, Le0 ),
 sub_string( Str, Sta1, _, _, Btag ),
 string_length( Btag, Le1 ),
 Y is Sta1 + Le1,
 Z is Le0 - Y,
 sub_string( Str, Y, Z, _, Sub1 ),
 sub_string( Sub1, Sta2, _, _, Etag ),
 Z2 is Sta2 - 0,
 sub_string( Sub1, 0, Z2, _, Subz ),!.
 

file_path_get_str(F , Pa2, Fn):-    atom_string(Fa, F),
 sub_atom( Fa, _, _, _, '/' ),
 file_directory_name( Fa , Pa), atom_concat(Pa, '/', Pa2),  file_base_name( F , Fn ),!.
file_path_get_str(F , '', Fa):-  atom_string(Fa, F),  !.

file_path_get(Fa , Pa2, Fn):-   
 sub_atom( Fa, _, _, _, '/' ),
 file_directory_name( Fa , Pa), atom_concat(Pa, '/', Pa2),  file_base_name( Fa , Fn ),!.
file_path_get(Fa , '', Fa):-   !.

lis_code_replace([], _, _, [] ):-!.
lis_code_replace([H|Codes1], Cde, Cde2, [Cde2 |Codes2] ):-  H = Cde, !, 
 lis_code_replace(Codes1, Cde, Cde2, Codes2).
lis_code_replace([H|Codes1], Cde, Cde2, [H|Codes2] ):-  !, lis_code_replace(Codes1, Cde, Cde2, Codes2).

lis_code_remove([], _,  [], [] ):-!.
lis_code_remove([H|Codes1], Cde,  [Cde |Codes2], Lisnot ):-  H = Cde, !, 
 lis_code_remove(Codes1, Cde,  Codes2, Lisnot).
lis_code_remove([H|Codes1], Cde,  Codes2 , [ H |Lisnot]):-  !, lis_code_remove(Codes1, Cde, Codes2, Lisnot).

str_code_replace( Str, Cde, Cde2, Str2):- 
 string_codes(Str, Codes1), 
  lis_code_replace(Codes1, Cde, Cde2, Codes2),!, string_codes(Str2, Codes2).

str_code_remove( Str, Cde,  Str2):- 
 string_codes(Str, Codes1), 
  lis_code_remove(Codes1, Cde, _, Codes2),!, string_codes(Str2, Codes2).


lis_concat( [], Res, _, Res):- !.
lis_concat( [H], Hs, _Use, C1):- !,  string_concat( Hs, H, C1).
lis_concat( [H|L], Hs, Use, Res):- !,
  string_concat( Hs, H, C1), string_concat(C1, Use, C2),
 lis_concat( L, C2, Use, Res).
  
str_count_up_dirs( Str,  Ata):- sub_string(Str, _, _, _, ".."),!,
  split_string(Str, "..", "..", L), length(L, Ata).
str_count_up_dirs( _,  0):- !.

ato_count_up_dirs( At,  Ata):-
 atom_string( At, Str),
 str_count_up_dirs( Str,  Ata).

str_dubbel_slash_to_single( Fn, Res):-
%  split_string(Fn, "//", "", L),
  split_string(Fn, "/", "/", L),
  lis_concat( L, "", "/", Res),!.


omz_fw_slash('', ''):-!.
   % 
omz_fw_slash(_Pa4z, '/'):-!.

% deze was de laatste  van de include lees 
%chercher_o_y_and_assert( 0, Lx, _Zk, F, Level, Linum ):-
%  string_lower( Lx, Lx2 ),
%  sub_str_between( Lx2, "include \"", "\"", Incfile ),!,
%  downcase_atom(F, F2), 
%  string_lower( Incfile, Incfile2 ),
%  str_code_replace( Incfile2, 92, 47, Incfile3x),
%  str_dubbel_slash_to_single( Incfile3x, Incfile3), atom_string(Incfile4, Incfile3),
%  file_path_get(F2 , Pa2, Fn2),
%  file_path_get_str(Incfile3 , Pa3, Fn3),
%  ato_count_up_dirs( Pa3,  Ata3),
%   dir_path_to_num_up( Pa2 , Ata3, Pa2z ),
%   file_base_name( Pa3 , Pa4z ),
%   omz_fw_slash(Pa4z, Sla),
%   atomic_list_concat([ Pa2z, Pa4z, Sla, Fn3 ], Nfilez ),	
%   assert( found_include_file( Level, Nfilez, Linum, Pa2, Pa2z, Fn2,  Pa3, Pa4z, Fn3, Ata3, Incfile4 ) ).
% split_string(H, "&", "", Lis_and),
% split_string( "[\"..\\tools\\reports\\parser.dom\",\"..\\tools\\reports\\repcomm.pro\"]", "\",\"", "", P).
%   incl_file("commun.pro",
% ["trilog_net.inc","calculateur.pro","gestion_bases.pro",
% "memor.pro","gestion_prest.pro","acces_calcul.pro",
% "..\\..\\..\\partage_prolog\\co2_calcul_prevar_dll_net.pro","..\\..\\..\\partage_prolog\\charger_data_prevar_dll_net.pro","..\\..\\..\\partage_prolog\\log_functs.pro","lect_offre.pro","body.pro","body_parametrage.pro","body_logsecu.pro","body_accord.pro","gestion_equ_serie.pro","gestion_base_ct.pro"])

asser_lis( Level, Linum, F, Lx2 ):-
   sub_str_between( Lx2, "[\"", "\"]", Incfile3x ),
   split_string( Incfile3x, "\",\"", "\",\"", Lz), 
   member(El, Lz ),
   string_lower( El, Incfile4x ),
   str_code_replace( Incfile4x, 92, 47, Incfile5x),
   str_dubbel_slash_to_single( Incfile5x, Incfile3), 
   atom_string(Incfile4, Incfile3),

   file_base_name( Incfile4 , Pa4z ),
   assert( found_include_file( Level, '', Linum, '', '', '',  '', '', F, 0, Pa4z ) ),
   fail,!.
      
   % split
   
 

asser_lis( _,_,_, _ ):-!.



:- dynamic has_db_clause/8.
:- dynamic has_db_header/5.

txt_is_clauses_begin( Lx, is_load  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "clauses" ),!.

txt_is_clauses_end( Lx, is_end  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "predicates" ),!.
txt_is_clauses_end( Lx, is_end  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "database " ),!.
txt_is_clauses_end( Lx, is_end  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "domains" ),!.
txt_is_clauses_end( Lx, is_end  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "constants" ),!.

%txt_is_save_load( Lx, is_write  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "save(" ),!.
%txt_is_save_load( Lx, is_write  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "openwrite" ),!.
%txt_is_save_load( Lx, is_write  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "openappend" ),!.
%txt_is_save_load( Lx, is_write  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "chain_insert" ),!.

txt_is_db_header( Lx, Subn ):-  string_lower( Lx, Lx2 ),
  sub_string( Lx2, _, _, _, "database" ),  sub_string( Lx2, Sta, _, _, " -" ), Sta2 is Sta + 2,
  sub_string( Lx2, Sta2, _, 0, Subx ),  str_code_remove( Subx, 32,  Subn), 
  !.
%--

% ook bij iedere file open leeg maken 

txt_is_db_header_end( Lx ):-  has_db_header( _, _, _, _, _ ), string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "predicates" ),!.
txt_is_db_header_end( Lx ):-  has_db_header( _, _, _, _, _ ), string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "clauses" ),!.
txt_is_db_header_end( Lx ):-  has_db_header( _, _, _, _, _ ), string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "database" ),!.
txt_is_db_header_end( Lx ):-  has_db_header( _, _, _, _, _ ), string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "constants" ),!.
txt_is_db_header_end( Lx ):-  has_db_header( _, _, _, _, _ ), string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "domains" ),!.

% split
read_db_predicate(Lx, Subn2, Le ):-  sub_string( Lx, Sta0, _, _, "determ " ), Y is Sta0 + 7,
 sub_string( Lx, _Sta, _, _, "(" ), sub_string( Lx, _, _, _, ")" ), string_length(Lx, Lxle), Z is Lxle - Y,
     sub_string( Lx, Y, Z, _,  Subx ), 
   str_code_remove( Subx, 32,  Subn), str_code_remove( Subn, 9,  Subn2), !,  split_string(Lx, ",", "", Lisx), length( Lisx, Le ), Le > 0.

read_db_predicate(Lx, Subn2, Le ):-
 sub_string( Lx, Sta, _, _, "(" ), sub_string( Lx, _, _, _, ")" ),
   Sta2 is Sta - 0,  sub_string( Lx, 0, Sta2, _, Subx ), 
   str_code_remove( Subx, 32,  Subn),  str_code_remove( Subn, 9,  Subn2), !,  split_string(Lx, ",", "", Lisx), length( Lisx, Le ), Le > 0.


%------
% has_save_load
% substr
lees_preds( Lx, 0, Preda  ):-  sub_string(Lx, Sta, _, _, "("),
 Sta2 is Sta - 0, sub_string(Lx, 0, Sta2, _, Preds0), str_code_remove( Preds0, 32,  Predsx), str_code_remove( Predsx, 9,  Preds),
 atom_string(Preda, Preds),!.
% lees_preds( _Lx, 1, "geenpred fou"  ):- !.

:- dynamic pred_is_called_from/2.
:- dynamic last_phead/1.

%assert_has_save_load( 1, Lx, _Zk, _F, _Level, _Linum ):-  sub_string( Lx, _, _, _,  "." ) ,!,
% retractall( last_phead(  _ ) ).

% txt_is_clauses_begin( Lx, is_load  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "clauses" ),!.
% txt_is_clauses_end( Lx, is_end  ):-  string_lower( Lx, Lx2 ), sub_string( Lx2, _, _, _, "predicates" ),!.

assert_clauses_have_started( 1, Lx, _Zk, _F, _Level, _Linum ):-  txt_is_clauses_end( Lx, _  ),!, 
 retractall( last_phead(  _ ) ).

assert_clauses_have_started( _Is_include, _Lx, _Zk, _F, _Level, _Linum ):-  !.


assert_has_clause( 1, Lx, _Zk, _F, _Level, _Linum ):-  sub_string( Lx, _, _, _,  ":-" ),
  lees_preds( Lx, _, Preds  ), !,
 retractall( last_phead(  _ ) ), assert( last_phead(  Preds ) ).

assert_has_clause( 1, Lx, _Zk, _F, Level, Linum ):-  last_phead(  Ph ),
 lees_preds( Lx, Tp, Pred_nm  ),
 % write("pred is called "),
 Ph \= Pred_nm,
 not( pred_is_called_from( Ph, Pred_nm ) ),!,
 assert( pred_is_called_from( Ph, Pred_nm ) ).
assert_has_clause( _Is_include, _Lx, _Zk, _F, _Level, _Linum ):-  !.

 
%---
assert_has_dbase_clauses( 1, Lx, _Zk, _F, _Level, _Linum ):- txt_is_db_header_end( Lx ),!,
 retractall( has_db_header( _, _, _, _, _ ) ).

assert_has_dbase_clauses( 1, Lx, _Zk, F, Level, Linum ):- txt_is_db_header( Lx, Dbname ),!,
 retractall( has_db_header( _, _, _, _, _ ) ),
 assert( has_db_header( Lx, F, Level, Linum, Dbname ) ).

assert_has_dbase_clauses( 1, Lx, _Zk, F, Level, Linum ):-   
 has_db_header( Lxh, _Ff, _Lev, _, Dbname ) ,
 read_db_predicate(Lx, Pred, A_args ) ,
% sub_string( Lx, _, _, _, "(" ), sub_string( Lx, _, _, _, ")" ),
 assert( has_db_clause( Dbname, Pred, A_args, Lxh, Lx, F, Level, Linum ) ).
 
 
assert_has_dbase_clauses( _Is_include, _Lx, _Zk, _F, _Level, _Linum ):- !.
%---

search_o_y_and_assert( 0, Lx, _Zk, F, Level, Linum ):-  
 string_lower( Lx, Lx2 ),
  sub_string( Lx2, _, _, _, "incl_file(\"" ),

%  write(Lx2), nl, 
  sub_str_between( Lx2, "incl_file(\"", "\",", Incfile3x ),!,
  
  string_lower( Incfile3x, Incfile4x ),
  str_code_replace( Incfile4x, 92, 47, Incfile5x),
   str_dubbel_slash_to_single( Incfile5x, Incfile3), 
  atom_string(Incfile4, Incfile3),
  file_base_name(Incfile4, Base),
  % atom_string( Incfile4, Incfile3 ),  
  downcase_atom( Base, Incfile5 ), 
  % nog iets met het pad doen als het kan 
  assert( found_include_file( Level, '', Linum, '', '', '',  '', '', F, 0, Incfile5 ) ),
  asser_lis( Level, Linum, F, Lx2 ).
  
  


search_o_y_and_assert( 0, Lx, _Zk, F, Level, Linum ):-  
  string_lower( Lx, Lx2 ),
  sub_string( Lx2, _, _, _, "\\obj\\" ),
  sub_str_between( Lx2, "\\obj\\", "'", Incfile3 ),!,
  atom_string( Incfile4, Incfile3 ),  downcase_atom( Incfile4, Incfile5 ), 
  % nog iets met het pad doen als het kan 
  assert( found_include_file( Level, '', Linum, '', '', '',  '', '', F, 0, Incfile5 ) ).




% ,BRO file 
%incl_file("..\\com\\acc_equip.pro",["t8w.inc","hlptopic.con"])
%incl_file("actions.pro",["..\\com\\actions.pro"])
%incl_file("..\\com\\actions.pro",["t8w.inc"])
%incl_file("annexes.pro",["t8w.inc"])
%incl_file("avenants.pro",["t8w.inc"])


%-----
:- dynamic eval_tag/1.

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _,_,_, 'metta_eval'),
  search_o_y( Lx, "eval_20&:-" ),
%  sub_str_between( Lx, "['", "'", Tag ),
  try_read_find_eval20_tag_in_string( Lx , Tag ),
  string_length( Tag, Lex ), Lex > 1,
  Tag \= "eval",
  not( eval_tag( Tag ) ),  assert( eval_tag( Tag ) ),
  !,
  asserta( found_search_result( Level, Linum, F, Tag ) ).

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _,_,_, 'metta_interp'),
  search_o_y( Lx, "eval_h(&:-" ),
  eval_tag( Tag_s ),
  string_concat("'", Tag_s, C1 ), string_concat( C1, "'", C2 ),
  sub_string( Lx, _, _, _ , C2 ), 
  % string_concat( Lx, C2 , C3 ),
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C2, Cp ), string_concat( Cp, " </b> " , C3 ), 
  !,
  assert( found_search_result( Level, Linum, F, C3 ) ).

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _, _, _, 'metta_ontology.pfc' ),
  search_o_y( Lx, "properties(" ),
  eval_tag( Tag_s ),
  string_concat("'", Tag_s, C1 ), string_concat( C1, "'", C2 ),
  sub_string( Lx, _, _, _ , C2 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C3 ), 
 % string_concat( Lx, C2 , C3 ),
  !,
  assert( found_search_result( Level, Linum, F, C3 ) ).

%  search_o_y( Lx, "assertequaltoresult&" ),

% (@doc intersection

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _, _, _, '.metta' ),  eval_tag( Tag_s ),  string_concat( "(", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , C1 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _, _, _, '.metta' ),  eval_tag( Tag_s ),  string_concat( "(@doc ", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , C1 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).


search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _, _, _, '.rs' ),  eval_tag( Tag_s ),  
  % string_concat( "(", Tag_s, C1 ), 
  string_concat( "regex(r\"", Tag_s, C1 ), 
  
  sub_string( Lx, _, _, _ , C1 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).


search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
  sub_atom( F, _, _, _, '.py' ),  eval_tag( Tag_s ),  string_length( Tag_s, Lex ), Lex > 3, 
  % concat( "(", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , Tag_s ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, Tag_s, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).


:- dynamic extra_string_to_search/1.

if_extra_string_demand_search( Lx ):-
  extra_string_to_search( Str ), !, search_o_y( Lx, Str ).
if_extra_string_demand_search( _Lx ):- !.

%search_o_y_and_assert( 1, Lx, Zk, F, Level, Linum ):-
 % search_o_y( Lx, Zk ),!,
 % assert( found_search_result( Level, Linum, F, Lx ) ).

%----

read_has_search_stream( Sea, Zk, F, Level, Linum, Is_include):- not( at_end_of_stream( Sea ) ),
 read_line_to_string( Sea, Lx ), Lx \= end_of_file, not_is_comment(Lx),
   zet_comment_started(Lx), zet_comment_ended(Lx),   is_not_inside_comment(),
   %assert_has_dbase_clauses( Is_include, Lx, Zk, F, Level, Linum ),
   %assert_clauses_have_started( Is_include, Lx, Zk, F, Level, Linum ),
   %assert_has_clause( Is_include, Lx, Zk, F, Level, Linum ),
   if_extra_string_demand_search( Lx ),
   search_o_y_and_assert( Is_include, Lx, Zk, F, Level, Linum ),
   
   !,  
   Linum2 is Linum + 1,
 read_has_search_stream( Sea, Zk, F, Level, Linum2, Is_include ).

read_has_search_stream( Sea, Zk, F, Level, Linum, Is_include):- not( at_end_of_stream(Sea) ),!, 
  Linum2 is Linum + 1, read_has_search_stream( Sea, Zk, F, Level, Linum2, Is_include).
read_has_search_stream(_ , _, _, _, _, _):- !.

% allow all dirs 

allow_dir( Tp, Pa, Xfile ):- 
 downcase_atom(Pa, Pa2),  downcase_atom(Xfile, Xfile2),
 allow_dir2( Tp, Pa2, Xfile2 ),
 !.


atom_last_part( At, Le, Lp):-
 atom_length(At, Lex), Lex > Le, Y is Lex - Le,
 sub_atom( At, Y, Le, _, Lp),!.


%prolog_src_dir( 'C:/jdklog/sources/PR_12_NvoDesign/UTILITAIRES/' ):- !.
% prolog_src_dir( 'C:/jdklog/sources/PR_12_NvoDesign/' ):- !.

% 
% C:/jdklog/sources/PR_12_NvoDesign/UTILITAIRES/
%  SOURCE  DLM   

:- dynamic chercher_a/1.

chercher_a( "eval_20" ).

:- dynamic current_file_extension/1.
current_file_extension( '.pl' ).

:- dynamic prolog_src_dir_perform/1.
prolog_src_dir_perform( 'H:/metta-wam-main/src/canary/' ).

% prolog_src_dir_perform( 'H:/metta-wam-main/src/' ).

% prolog_src_dir_perform( 'C:/jdklog/sources/PR_10_NvoDesign/TRILOG_NET_LIGHT_NVO_DESIGN_PREVAR - MalusPoids/' ).

% prolog_src_dir_perform( 'C:/jdklog/sources/PR_12_NvoDesign/UTILITAIRES/PARAM10E - Design/' ).


% prolog_src_dir_perform( 'C:/jdklog/sources/PR_12_NvoDesign/Trilog5_NouveauLook_Design_MalusPoids/' ).
% prolog_src_dir_perform( 'C:/jdklog/sources/partage_prolog/' ).



%prolog_src_dir_perform( 'C:/jdklog/sources/PR_12_NvoDesign/UTILITAIRES/param_div_nvo - Design/' ).
%prolog_src_dir_perform( 'C:/jdklog/sources/PR_12_NvoDesign/CART_GR2 - Design/' ).

% prolog_src_dir_perform( 'C:/jdklog/sources/PR_10_NvoDesign/TRILOG_NET_LIGHT_NVO_DESIGN_PREVAR - MalusPoids/' ).
%prolog_src_dir_perform( 'C:/jdklog/sources/PR_12_NvoDesign/Trilog5_NouveauLook_Design_MalusPoids/' ).

%prolog_src_dir_perform( 'C:/jdklog/sources/PR_10_NvoDesign/Tools/').


% prolog_src_dir_perform( 'C:/logiciels_clients/_Gesloc_Agl/' ).

%prolog_src_dir_perform( 'C:/jdklog/sources/PR_12_NvoDesign/UTILITAIRES/REGLES_NVOLOOK - Design/' ).


% prolog_src_dir_perform( 'c:/jdklog/sources/PR_12_NVODESIGN/TRILOG5_NOUVEAULOOK_DESIGN/' ).
%  impr
%  je  

% Module:D:\PR_12_NVODESIGN\TRILOG5_NOUVEAULOOK_DESIGN\AUTO_44\IMPRESS.PRO Include:C:\VIP52\VPI\INCLUDE\REPORT\EDITPS.PRO Pos:31571 Message:6316 (PROLOG.ERR can not be opened)
% prolog_src_dir( 'C:/jdklog/sources/PR_12_NvoDesign/UTILITAIRES/' ):- !.
% prolog_src_dir( 'C:/logiciels_clients/' ):- !.

% chercher_a( "e_user(99" ):-!.

% k_div
%chercher_a( "konstant_pre32(&\"k_div\"" ):-!.
%chercher_a( "konstant_pre32(&\"k_div2\"" ):-!.

%  all co2
% chercher_a( "\"co2_&konstant_pre32(,co2_&konstant_pre(" ):-!.

% chercher_a( "avenant" ):-!.
%chercher_a( "imp_cw&save,impr(" ):-!.

%chercher_a( "kons_pn_icare&ifdef,kons_pn_tab&ifdef" ):-!.
%chercher_a( "def_param&\"rel,def_param&\"pn" ):-!.
%chercher_a( "anc.,nouv." ):-!.
%chercher_a( "file_exist&logisciels" ):-!.
%chercher_a( "cg_v(,co2_bm,wltp" ):-!.
% chercher_a( "poids&format(" ):-!.
%chercher_a( "do_metta" ):-!.
%chercher_a( "eval_20" ):-!.

% chercher_a( "nature" ):-!.
%chercher_a( "pre32&co2_bm" ):-!.

%chercher_a( "ifdef&use_wars" ):-!.
 
% chercher_a( "\\veh\\"  ):-!.

%chercher_a( "co2_cg(" ):-!.
% chercher_a( "charge_co2_base" ):-!.
% chercher_a( "concordance" ):-!.
% convert_file_to_text_if_il_es_change
% chercher_a( "charger_data_t8w.pro" ):-!.
% chercher_a( "afficher_veh" ):-!.
% chercher_a( "comline" ):-!.
% chercher_a( "comline" ):-!.
% use_version_evaluation
% chercher_a( "met_opt_suivant_choix(" ):-!.
% chercher_a( "convert_file_to_text_if_il_es_change" ):-!.
%  met_opt_suivant_choix(
% chercher_a( "\"co2_bm&\"cg_base_bm" ):-!.
% chercher_a( "creer_page_pdf" ):-!.
% chercher_a( "konstant_pre32&\"ica_pn\",konstant_pre32&\"ica_mois\",\"pneu" ):-!.
% chercher_a( "chain_insert" ):-!.

 
% 
% chercher_a( "\"k_div\"" ):-!.

% 
% chercher_a( "clauses" ):-!.

% is_prolog_atom_file( _At, _Dx ):- !.


is_prolog_atom_file( At, _Dx ):- downcase_atom(At , At2),  
 current_file_extension( Fext ),
 sub_atom( At2, _, _, _ , Fext ),!.

is_prolog_atom_file( _At, _Dx ):-  !, fail.



is_prolog_atom_file( _At, Dx ):- downcase_atom(Dx, At2),  
 sub_atom( At2, _, _, _ , 'auto_44' ),!, fail.

is_prolog_atom_file( _At, Dx ):- downcase_atom(Dx, At2),  
 sub_atom( At2, _, _, _ , '- copie' ),!, fail.
is_prolog_atom_file( _At, Dx ):- downcase_atom(Dx, At2),  
 sub_atom( At2, _, _, _ , 'modificier' ),!, fail.
is_prolog_atom_file( _At, Dx ):- downcase_atom(Dx, At2),  
 sub_atom( At2, _, _, _ , 'modifice' ),!, fail.



is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.pro~' ),!, fail.

is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.pro' ),!.

is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.dba' ),!.


is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.pre' ),!.

is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.inc' ),!.

is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.map' ),!.

is_prolog_atom_file( At, _Dx ):- downcase_atom(At, At2),  
 sub_atom( At2, _, _, _ , '.bro' ),!.



% is_prolog_atom_file( At, _Dx ):-  sub_atom( At, _, _, _ , 'base_teur32.dba' ),!.


only_via_map_file( 1 ):-!.
% only_via_map_file( 0 ):-!.

% e_mousedown

allow_dir2( _, _, _ ):- !.

allow_dir2( _, _Pa, Xfile ):- sub_atom( Xfile, _, _, 0, 'canary' ), !.
allow_dir2( _, _Pa, _Xfile ):- !, fail,  !.


allow_dir2( _, _Pa, Xfile ):- sub_atom( Xfile, _, _, _, 'automoto' ),!, fail.

allow_dir2( _, Pa, _Xfile ):-   sub_atom(Pa, _, _, _, '/old' ) ,!, fail.
allow_dir2( _, _Pa, Xfile ):-   

 sub_atom(Xfile, 0, 3, _, Sub ) , Sub == 'old',!, fail.


%allow_dir2( _, Pa, Xfile ):- 
 % write("check path OLD "), write(Pa), nl,
% ( sub_atom(Xfile, _, _, _, '/old' ); sub_atom(Pa, _, _, _, '/old' ) ),!, fail.


% allow_dir2( _, Pa, Xfile ):- sub_atom(Pa, _, _, _, 'utilitaires' ),!,
% ( sub_atom( Xfile, _, _, _, 'param_div_nvo') ;
%  sub_atom( Xfile, _, _, _, 'param10e')   ), !.

allow_dir2( _, _, _ ):- !.

allow_dir2( 1, _, _ ):- !.

  
% hier files uitsluiten indien nodig

%is_relevant_file( From ):-
% found_include_file( _Level, _Lnum, _Fulldir_file, Dir_lesshigher,  _File_search_in, _Dir_inc_full,  Dirlast_inc, File_inc, _Aant_up, _Str_file_up),
%  atomic_list_concat( [ Dir_lesshigher, ], '',
% und_include_file( _Level, _Lnum, _Xz, _Fulldir_file, Dir_lesshigher,  File_search_in, _Dir_inc_full,  Dirlast_inc, File_inc, _Aant_up, _Str_file_up),

is_relevant_file(_, From ):-  downcase_atom( From, From2 ),
%  write( From ), nl, 
  current_file_extension( Fext ),
  sub_atom(From2, _, _, _,  Fext ),!.

is_relevant_file(_, _From ):-   !, fail.


is_relevant_file( 0, From ):-  only_via_map_file( N ), N = 1, 
  downcase_atom( From, From2), 
  sub_atom(From2, _, _, _, '.map'),
  not( sub_atom(From2, _, _, _, 'goal$') ),
  !.

is_relevant_file( 0, From ):-  only_via_map_file( N ), N = 1, !,
  downcase_atom( From, From2),  sub_atom(From2, _, _, _, '.bro').


is_relevant_file( 1, From ):-    downcase_atom( From, From2),  sub_atom(From2, _, _, _, '.bro'), !, fail.
is_relevant_file( 1, From ):-    downcase_atom( From, From2),  sub_atom(From2, _, _, _, '.map'), !, fail.

is_relevant_file(1,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'def'),
 sub_atom(From2, _, _, _, '.dba'),!.

is_relevant_file(1,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'def'),
 sub_atom(From2, _, _, _, '.inc'),!.

is_relevant_file(1,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'base'),
 sub_atom(From2, _, _, _, '.dba'),!.

is_relevant_file(1,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'partage_prolog'),!.


is_relevant_file( 1, From ):-  only_via_map_file( N ), N = 1, !,
  downcase_atom( From, From2),  
  file_base_name( From2 , Fnx ),
  file_name_extension( Base, _Ext, Fnx),
  file_includes( _Cz, _Linum, Base), !.


% base_name  
%  sub_atom(From2, _, _, _, '.map').
  
%  file_includes( _Cz, _Linum, From2), !.
%  atom_string( Cz, File_incS), !.

% found_include_file( _, From2, _, _, _, _,_,_,_,_,_ ), !.


is_relevant_file(_, From ):-  downcase_atom( From, From2),
  sub_atom(From2, _, _, _, 'avant modif'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom( From, From2),
  file_base_name( From2 , Fnx ),  file_name_extension( Base, _Ext, Fnx),
  atom_last_part(Base, 4, Lp),  Lp == '_anc', !, fail.

is_relevant_file(_,  From ):-  downcase_atom( From, From2),
  file_base_name( From2 , Fnx ),  file_name_extension( Base, _Ext, Fnx),
  atom_last_part(Base, 1, Lp),  Lp == '-', !, fail.

% is_relevant_file( From ):-  sub_string(From, _, _, _, "PR_10_NvoDesig"),  !.
% atom_last_part('aabbccdd', 2, Lp).
is_relevant_file(_,  From ):-  downcase_atom( From, From2),
  file_base_name( From2 , Fnx ),  file_name_extension( Base, _Ext, Fnx),
  atom_last_part(Base, 8, Lp),  atom_number( Lp, _), !, fail.


% extension 
%  sub_atom(Fnx, _, _, _, "spec_loueur"), 
% nl, write(Fnx), nl,
%  atom_length(Fnx, Le),
%  Le \= 15,
% !, 

 % fail.


is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'goal$'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, '2017_'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, '-2013'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, '20130'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, '-avant nettoyage'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, '--'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'if_in-save.pro'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'gridmain.pro'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'toolbar.pro'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'treeview.pro'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, '_old.pro'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'auto_44'),!, fail.


% spec_loueur.pro
is_relevant_file(_,  From ):-  downcase_atom(From, From2),
  file_base_name( From2 , Fnx ),
  sub_atom(Fnx, _, _, _, "spec_loueur"), 
% nl, write(Fnx), nl,
  atom_length(Fnx, Le),
  Le \= 15,
 !, 

 fail.


is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_atom(From2, _, _, _, 'automoto'),!, fail.

is_relevant_file(_,  From ):-  downcase_atom(From, From2),
 sub_string(From2, _, _, _, 'design - cacf'),!, fail.

% is_relevant_file( From ):-  sub_string(From, _, _, _, "PARAM"),!.
  

is_relevant_file(_,  _From ):- !.



%  [prolog_analyse_update].
%  start_analyse( 0, '', is_mk_update ).
dir_path_to_num_up( Fpa , 0, Fpa):- !.

dir_path_to_num_up( Filepath , N, Dir2):-
 dir_path_to_num_up0( Filepath , N, Dir), atom_concat(Dir, '/', Dir2).
%---
dir_path_to_num_up0( Filepath , 1, Dir):- !,  file_directory_name( Filepath , Dir).
dir_path_to_num_up0( Filepath , 2, Dir):- !,  
 file_directory_name( Filepath , Dir0), file_directory_name( Dir0 , Dir).

dir_path_to_num_up0( Filepath , 3, Dir):- !,  
 file_directory_name( Filepath , Dir01), file_directory_name( Dir01 , Dir0), file_directory_name( Dir0 , Dir).

dir_path_to_num_up0( Filepath , 4, Dir):- !,  
 file_directory_name( Filepath , Dir001), file_directory_name( Dir001 , Dir01), 
 file_directory_name( Dir01 , Dir0), file_directory_name( Dir0 , Dir).
  
  
dir_get_last_path( Filepath , Lp):-
   file_directory_name( Filepath , Dir1),
   file_base_name( Dir1 , Lp ),!.
%   atom_concat(Filepath, Lp, Fnx),!.

dir_get_last_path( Lp , Lp):- !.


dir_get_last_path2( Filepath , Lp):-
   file_directory_name( Filepath , Dir1),
   file_directory_name( Dir1 , Dir2),
   file_base_name( Dir2 , Lp ),!.
%   atom_concat(Filepath, Lp, Fnx),!.

dir_get_last_path2( Lp , Lp):- !.



% found_include_file( _, From2, _, _, _, _File_search_in, _Dir_inc_full,  _Dirlast_inc, File_inc, _Aant_up, _Str_file_up ),
% found_include_file( 1, 'c:/users/renee/documents/jdklog/sources/pr_12_nvodesign/trilog5_nouveaulook_design_maluspoids/com/acc_equip.pro',
% 12,'c:/users/renee/documents/jdklog/sources/pr_12_nvodesign/trilog5_nouveaulook_design_maluspoids/agl/',
% 'c:/users/renee/documents/jdklog/sources/pr_12_nvodesign/trilog5_nouveaulook_design_maluspoids/',
% 'acc_equip.pro','../com/',com,'acc_equip.pro',1,'../com/acc_equip.pro')

%  assert( found_include_file( Level, Nfilez, Linum, Pa2, Pa2z, Fn2,  Pa3, Pa4z, Fn3, Ata3, Incfile4 ) ).


% werkt niet 
display_if_via_include( Filepath  ):-
  downcase_atom( Filepath, Filepath2),
  file_base_name( Filepath2, Base ),
    file_name_extension( Base2, _Ext, Base),

  file_includes( Cz, _Linum, Base2 ) ,
  atom_string( Cz, File_incS), 
  write("\n<br>  \n"),
  write("VIA MAPFILE: "), write(File_incS), nl, 
  write("\n<br>  \n"),

  !.

% found_include_file( _, File_does_include, _, _File_search_in, Deze, _, _Dir_inc_full,      Dirlast_inc,File_inc, _Aant_up, _Str_file_up ),
% atomic_list_concat([ Deze, '/', Dirlast_inc, '/', File_inc], Fullf),
% Fullf = Filepath2,
% atom_string( File_does_include, File_incS), !.

display_if_via_include( _Filepath ):-!.




% laatste versie
%include_to_includes():-
% retractall( file_includes( _,_,_) ),
% found_include_file( _, _File_does_include, Linum, File_search_in, Deze, F1, _Dir_inc_full,      Dirlast_inc, File_inc, _Aant_up, _Str_file_up ),
% omz_fw_slash(Dirlast_inc, Sla),
% atomic_list_concat([ Deze,  Dirlast_inc, Sla, File_inc], Fullf),
% atom_concat( File_search_in , F1, Cz), 
% assert( file_includes( Cz, Linum, Fullf)  ),
% fail,!. 

include_to_includes():-
 found_include_file( _, _, Linum, _, _, _, _, _, File_map, _Aant_up, File_van_map_file ),
  file_name_extension( Base, _Ext, File_van_map_file ),

 not( file_includes( _, _, Base ) ),
 assert( file_includes( File_map, Linum, Base )  ),
 fail,!. 
include_to_includes():- !.

% temp _ext

write_copy_link( Pa, _Fnx ):-
 write( "<a onclick=\"prompt('copy path','"),
 write(Pa), 
 % write(Fnx), 
 write("');\" style=\"cursor: pointer\"> Fullpath  </a> \n" ).

if_small_no_newline( Lex ):- Lex < 12, !,  write( "  &nbsp  &nbsp " ) .
if_small_no_newline( Lex ) :-   write( " <br> &nbsp <br>" ), nl.



fresults_par_file( Lp2, Lp, Fnx,  Filepath , Zk ):-
 write_copy_link( Filepath, Fnx ),
 write( "\n<br> <a onclick=\"open_div('"), write(Lp2), write(Lp),  write(Fnx), write("')\"  style=\"cursor:pointer\">  Open </a>" ), nl,
 write( "\n <a onclick=\"close_div('"), write(Lp2), write(Lp),  write(Fnx), write("')\" style=\"cursor:pointer\">  Close </a>" ), nl,
 write( "\n<div id=\""), write(Lp2), write(Lp),  write(Fnx), write("\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #676767; color: #E7E7E7 \" >  " ), nl,
 found_search_result( Level, Lnum, Filepath, Txtline ),
  % write( Fnx ), 
  % write(" line: "), write( Lnum ), write(" : "), 
   
%   write("depth:"), write( Level ),   nl, 
% write( Txtline ),  
 write_line_with_tag_colors( Txtline, Zk ), 
  write("\n"),
  string_length( Txtline, Lex ), if_small_no_newline( Lex ) ,
%   write( " <br> aa &nbsp <br>" ), nl,
 fail,!.
fresults_par_file( _, _, _Fnx, _ , _):- !,
 write( "\n  </div>\n" ).


 
%display_found_sresults( _Zk, _Dirx ):-
%  write("\n\n###results INCLUDE "), nl,
%  file_includes( File_map, Linum, File_van_map_file ),
%  write_term( file_includes( File_map, Linum, File_van_map_file ), [ quoted(true) ] ), nl,
%  fail,!.

% ###results for searching:

:- dynamic per_file/1.


display_metta_sresults( _Zk, _Dirx ):-  retractall( per_file( _ ) ) ,
  found_search_result( _, _, Fpz, _ ),
  not( per_file( Fpz ) ),  assert( per_file( Fpz ) ), fail.


display_metta_sresults(Zk, Dirx):-
 write("\n\n<h2>  <pre> "),
 str_code_replace( Zk, 60, 32, Zkx5), str_code_replace( Zkx5, 62, 32, Zkx6),
 % write( Zkx6 ), 
  write("</pre> </h2>"),  
 write("<h2>"),write(" in directory: \n"), write("</h2>"),
 write("<h2>"), write( Dirx ), write("</h2> <hr>"), nl, nl,
 % findall( Fpz,  found_search_result( _, _, Fpz, _ ), Fpzl),
  findall( Fpz,  per_file(  Fpz ), Fpzl ),
  % sort( Fpzl , Fpzl2 ), 
 member( Filepath, Fpzl ), 
   file_base_name( Filepath , Fnx ),
   dir_get_last_path( Filepath , Lp),  dir_get_last_path2( Filepath , Lpx2), file_base_name( Filepath , Fnx ),
 write("\n<br> &nbsp <br> \n"), write("\n<br> &nbsp <br> \n"),
 nl,nl,nl, write("***"), write(" <i><b> "), write( Fnx ), write("</i></b> <b> &nbsp &nbsp &nbsp "), write(Lp), write("</b> &nbsp &nbsp &nbsp "), write(Lpx2), nl,
 display_if_via_include( Filepath ), file_directory_name( Filepath , Dir ), write( Dir ), nl,
 fresults_par_file( Lpx2, Lp, Fnx, Filepath, Zk ),
 fail, !.

 display_metta_sresults(_, _):-  write("</pre>"), !.
%---

display_found_sresults(Zk, Dirx):-
 write("\n\n<h2> ###results for searching: <pre> "),
     str_code_replace( Zk, 60, 32, Zkx5), str_code_replace( Zkx5, 62, 32, Zkx6),
 
 write( Zkx6 ),  write("</pre> </h2>"),  write("<h2>"),write(" in directory: \n"), write("</h2>"),
 write("<h2>"), write( Dirx ), write("</h2> <hr>"), nl, nl,
 findall( Fpz,  found_search_result( _, _, Fpz, _ ), Fpzl),
 sort( Fpzl , Fpzl2 ),
 member( Filepath, Fpzl2 ),
 file_base_name( Filepath , Fnx ),
 % file_directory_name( Filepath , Dir1),
  dir_get_last_path( Filepath , Lp),
  dir_get_last_path2( Filepath , Lpx2),
 file_base_name( Filepath , Fnx ),
 write("\n<br> &nbsp <br> \n"),
 write("\n<br> &nbsp <br> \n"),

 nl,nl,nl, write("***"), write(" <i><b> "), write( Fnx ), write("</i></b> <b> &nbsp &nbsp &nbsp "), write(Lp), write("</b> &nbsp &nbsp &nbsp "), write(Lpx2), nl,
 display_if_via_include( Filepath ),
 file_directory_name( Filepath , Dir ),
 write( Dir ), nl,
 fresults_par_file( Lpx2, Lp, Fnx, Filepath, Zk ),
 fail,
 !.

 display_found_sresults(_, _):-  write("</pre>"),
 !.

vars_for_ata( 1, "(A)" ):-!.
vars_for_ata( 2, "(A, B)" ):-!.
vars_for_ata( 3, "(A, B, C)" ):-!.
vars_for_ata( 4, "(A, B, C, D)" ):-!.
vars_for_ata( 5, "(A, B, C, D, E)" ):-!.
vars_for_ata( 6, "(A, B, C, D, E, F)" ):-!.
vars_for_ata( 7, "(A, B, C, D, E, F, G)" ):-!.
vars_for_ata( 8, "(A, B, C, D, E, F, G, H)" ):-!.
vars_for_ata( 9, "(A, B, C, D, E, F, G, H, I)" ):-!.
vars_for_ata( 10, "(A, B, C, D, E, F, G, H, I, J)" ):-!.
vars_for_ata( 11, "(A, B, C, D, E, F, G, H, I, J, K)" ):-!.
vars_for_ata( 12, "(A, B, C, D, E, F, G, H, I, J, K, L)" ):-!.
vars_for_ata( 13, "(A, B, C, D, E, F, G, H, I, J, K, L, M)" ):-!.
vars_for_ata( 14, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N)" ):-!.
vars_for_ata( 15, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)" ):-!.
vars_for_ata( 16, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)" ):-!.
vars_for_ata( 17, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)" ):-!.
vars_for_ata( 18, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)" ):-!.
vars_for_ata( 19, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)" ):-!.
vars_for_ata( 20, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)" ):-!.
vars_for_ata( 21, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)" ):-!.
vars_for_ata( 22, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)" ):-!.
vars_for_ata( 23, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)" ):-!.
vars_for_ata( 24, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X)" ):-!.
vars_for_ata( 25, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y)" ):-!.
vars_for_ata( 26, "(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)" ):-!.

vars_for_ata( A, X ):- number_string(A, A2), string_concat(A2, "TOO Many arguments", X), !.

write_save_to_db( Dbname, El, A_args  ):- 
 write("% "),  write(El), write(" "), write(A_args), nl, 
 write("save_all_data():-"), nl, 
 vars_for_ata( A_args, Str_args ),
 write(" "), write( El), write( Str_args ), write(","), nl,
 write( " term_str( "), write( Dbname ), write(", "), write( El), write( Str_args ), write(", Strx ),"), nl,
 write(" write( Strx, \"\\n\" ), fail,!. \n" ), !. 


write_read_db( Dbname, El, A_args  ):-
 write("read_data_line( Xx ):-"), nl, 
 vars_for_ata( A_args, Str_args ),
 write( " trap( term_str( "), write( Dbname ), write(", "), write( El), write( Str_args ), write(", Xx ), _, fail ), !, "), nl,
 write(" assert( " ), write(El),  write( Str_args ), write(" ). "),  !. 

write_retract_db( _Dbname, El, A_args  ):-
 vars_for_ata( A_args, Str_args ),   str_to_und_score( Str_args, Str_args2 ),
 write( " retractall( "), write( El), write("("), write( Str_args2 ), write(") ),"),   nl,  !. 


lis_und_concat( [], Hs,  Hs ):- !.
lis_und_concat( [ _ ], Hs,  P2 ):- string_concat( Hs, "_", P2).
lis_und_concat( [ _ |Lis], Hs,  A_args2 ):- string_concat( Hs, "_,", P2),
  lis_und_concat( Lis, P2,  A_args2 ).

 
% write_term(  has_pred_db_ata( Dbname, El, A_args ), [] ),
% split 
str_to_und_score( A_args, A_args2 ):-
   split_string( A_args, ",", " ()", Lis),
   lis_und_concat( Lis, "",  A_args2),!.



display_prolog_database_calls():- nl,  write("\npredicates"), nl, 
 write("\nsave_all_data_file( string ) - (i)"), nl, 
 write("\nsave_all_data()"), nl, 
 write("\nclean_all_data()"), nl, 
 write(" read_all_data_from_file( string ) - (i)"), nl,
 write("\nread_data_line( string ) - (i)"), nl, 

 write("\nclauses"), nl, nl,
 write(" save_all_data_file( Fn ):- "), nl,
   write(" openwrite( fileselector1, Fn),"), nl,
   write(" writedevice( fileselector1 ),"), nl,
   write(" save_all_data(), "), nl,
   write(" closefile( fileselector1 ), writedevice(stdout)."), nl, nl,

 
 write("repeateofnorm(F):- not(eof(F)). "), nl,
 write("repeateofnorm(F):- not(eof(F)),repeateofnorm(F). "), nl, nl,
 
 findall( Pred,  has_db_clause( _Dbname, Pred, _, _, _, _, _, _ ), Xlis0 ), sort(Xlis0, Xlis ),
 member( El, Xlis ),
 has_db_clause( Dbname, El, A_args, _Lxh, _Lx, _F, _Level, _Linum ) ,
 write_save_to_db( Dbname, El, A_args  ),   fail  .

display_prolog_database_calls():- 
 write("\nsave_all_data():- !."), nl, 
 nl,  write("\n"), nl, 
 findall( Pred,  has_db_clause( _Dbname, Pred, _, _, _, _, _, _ ), Xlis0 ), sort( Xlis0, Xlis ),
 member( El, Xlis ),
 has_db_clause( Dbname, El, A_args, _Lxh, _Lx, _F, _Level, _Linum ) ,
 write_read_db( Dbname, El, A_args  ), nl,  fail  .

display_prolog_database_calls():- nl,  write("\n"), nl, 
 write("\nclean_all_data():-"), nl, 
 findall( Pred,  has_db_clause( _Dbname, Pred, _, _, _, _, _, _ ), Xlis0 ), sort( Xlis0, Xlis ),
 member( El, Xlis ), 
 has_db_clause( Dbname, El, A_args, _Lxh, _Lx, _F, _Level, _Linum ) ,
 write_retract_db( Dbname, El, A_args  ),  fail  .

display_prolog_database_calls():-  write("!.\n"), nl, 
  write("read_all_data_from_file( Xfn ):- "), nl,
  write(" clean_all_data(), "), nl,
  write(" closefile(fileselector1), "), nl,
  write(" openread(fileselector1, Xfn ), "), nl,
  write(" readdevice(fileselector1), "), nl,
  write(" repeateofnorm(fileselector1 ), "), nl,
  write(" readln( Lin ), "), nl,
  write(" read_data_line( Lin ), "), nl,
  write(" fail,!. "), nl,
  write("read_all_data_from_file( _ ):- !, "), nl,
  write(" closefile(fileselector1),readdevice(stdin).   "), nl.




display_prolog_database_calls():-  write("!."),nl,!.


%wri_save_list(Xel):-
% write(" <div style=\"max-height: 90px; border-radius: 5px;overflow-y: scroll; border: 1px solid black; display: inline-block; width: 98%\">  <pre>"),
% pred_is_called_from( _Dbn, Ph, _X, is_write, Lx, Xel, _Level, Linum ),
% write( Ph ), nl, write(Linum), write(Lx), write(" "),  fail,!.
%wri_save_list(_):- write(" </pre></div>").

%wri_load_list(Xel):-
% write(" <div style=\"max-height: 90px;  border-radius: 5px; overflow-y: scroll; border: 1px solid black; display: inline-block; width: 98%\">  <pre>"),
% pred_is_called_from( _Dbn, Ph, _X, 0, _Lx, Xel, _Level, _Linum ),
% write( Ph ), nl, 
% write(" "), 
% write(Xel), write(" "),  fail,!.
%wri_load_list(_):- write(" </pre></div>").


%display_pred_used():- nl,write("<br> &nbsp <br> &nbsp <br>  &nbsp <br>  <h1> <b> LOADING: </b> </h1> <hr>\n "), nl,
% findall( F, pred_is_called_from( _, _, _, 0, _, F, _,_ ), Flis ),
% sort( Flis, Flis2 ), member( Xel , Flis2 ), file_base_name( Xel, Fb), write(" <h2> <b>"), write(Fb), write("</b> </h2>"),
% wri_load_list(Xel),
%  fail,!.

numspace(Tel):- Tel > 0, !, write("&nbsp"), Tel2 is Tel - 1,
 numspace(Tel2).
numspace(_):- !.

%find_reverse_pad( Zk, "", Tel):-  Tel < 20,
% write(" <br> &nbsp <br> "), nl, 
% write(Zk), write("( <br>"), nl,
% pred_is_called_from( Ph, Pred_nm ), sub_string( Pred_nm, _, _, _, Zk), !,
% Tel2 is Tel + 1,
%   numspace(Tel), write(Ph), write("( <br>"), nl,
% find_reverse_pad( Zk, Ph, Tel2).

write_line_with_tag_colors( Head2, Zk ):-
concat( Zk, "", Zk2 ),
 concat( "<b>", Zk2, C1 ), concat( C1, "</b>", C2 ),
 str_replace_tag(Head2, Zk2, C2, Resu),
 write(Resu ), !.
write_line_with_tag_colors( Head2, Zk ):- write(Head2 ). 

find_reverse_pad( Pred_nm, Tel, Zk):-  Tel < 20,
  retract( pred_is_called_from( Head2, Pred_nm ) ), !,
  % pred_is_called_from( Head2, Pred_nm ), !,
 % retractall( pred_is_called_from( Head2, Pred_nm ) ),
 %pred_is_called_from( Head2, Head_search ),!,
    X is Tel * 2,
	numspace(X), 
%    write(Head2), 
  write_line_with_tag_colors( Head2, Zk ),
   write("  <br>"), nl,

  Tel2 is Tel + 1,
 find_reverse_pad( Head2, Tel2, Zk).
find_reverse_pad(  _, _Tel, Zk):- !.
%-----
find_reverse_pads([], [],_):- !.
find_reverse_pads([C|Clau], [ H | Ph_heads ], Zk):- !,
  write("<hr>"),  
  % write(C), 
  write_line_with_tag_colors( C, Zk ),
  write("( <br> &nbsp "), nl,
%  write(H), 
  write_line_with_tag_colors( H, Zk ),
  write(" <br>"), nl,
  find_reverse_pad( H, 3, Zk),
 find_reverse_pads( Clau, Ph_heads, Zk).

get_all_pred_is_called_from( Head, Pred_nm, Zk ):-
 pred_is_called_from( Head, Pred_nm ), sub_string( Pred_nm, _, _, _, Zk).


%  find_reverse_pad("cg_v(", "", 20),!.
display_pred_used():- 
%  Zk = "cg_v",
 chercher_a(Zk),
%  Zk = "pdf",
  write(" <br> &nbsp <br>"), write(" <br> &nbsp <br>"), nl,
  write(" <div style=\"background-color: black; color: white; min-width: 90%; padding: 10px\"> <b>"),
    write(Zk ), write("</b> predicate structure </div> <br> "),
  findall( Ph_head, get_all_pred_is_called_from( Ph_head, _, Zk ), Ph_heads ),
  findall( Clau, get_all_pred_is_called_from( _, Clau, Zk ), Claus ),
  % write_term( Ph_heads, []),
%  sort( Ph_heads0, Ph_heads ),
  find_reverse_pads(Claus, Ph_heads, Zk),
  fail,
%  find_reverse_pad(Zk, "", 0),
%  find_reverse_pad( Zk, "", 0),
%  find_reverse_pad( Zk, "", 0),
%  find_reverse_pad( Zk, "", 0),
  
  
  !.

%display_pred_used():- 
%  pred_is_called_from( Ph, Pred_nm ),
%  write_term( pred_is_called_from( Ph, Pred_nm ), [] ), nl, write("<br>"), nl,
%  fail,!.
  

display_pred_used():-  nl,  nl, !.
  

%display_save_load():- nl,write("</pre>\n<h1><b> SAVING: </b> </h1>\n<pre> <hr>"), nl,
% findall( F, pred_is_called_from( _, _, _, is_write, _, F, _,_ ), Flis ),
% sort( Flis, Flis2 ), member( Xel , Flis2 ), file_base_name( Xel, Fb), nl, write("<h2><b>"), write(Fb), write("</b></h2>"), 
% wri_save_list(Xel),
% write(" <div style=\"max-height: 90px; overflow-y: scroll; border: 1px solid black\">  <pre>"),
% has_save_load( Dbn, Ph, X, is_write, Lx, Xel, Level, Linum ),
%  write( Ph ), nl, write(Lx), write(" "),  
%fail,!.


%----

start_analyse00(Tp, MdfKeyaft, Is_mk_update, Dirx_zz ):- !,
  chercher_a( Zk ),
  today_key( Dk ), write( "today key " ), write( Dk ),
  get_all_prolog_files( Tp, MdfKeyaft, Dirx_zz ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( found_search_result( _, _, _, _ ) ),
  Xfi = 'data/htm_file_list.pl',
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ), 
  retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  retractall( found_include_file( _, _, _, _, _, _, _, _, _, _, _ )),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,
  write( "\n<br> &nbsp <br> \n" ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz, 0 ),
  include_to_includes(),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz, 1 ),
  append( 'result/prolog_codes_analyses.htm' ),
  display_found_sresults( Zk, Dirx_zz ),
  told(),
  write( "analyses result written to file: \n" ),
  write( "result/prolog_codes_analyses.htm\n" ).
%---

start_analyse_metta( Tp, MdfKeyaft, Is_mk_update,  _X ):- !,
    retractall( eval_tag( _ ) ),
   retractall( found_search_result( _, _, _, _ ) ),

  retractall( extra_string_to_search( _ ) ), 
   % assert( extra_string_to_search( "count" ) ),
  retractall( chercher_a( _ ) ), assert( chercher_a( "eval_20&:-" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.pl' ) ),
  retractall( prolog_src_dir_perform( _ ) ),  assert( prolog_src_dir_perform( 'H:/metta-wam-main/src/canary/' ) ),
    prolog_src_dir_perform( Dirx_zz ),

  chercher_a( Zk ),
  today_key( Dk ), write( "today key " ), write( Dk ),
  get_all_prolog_files( Tp, MdfKeyaft, Dirx_zz ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  
  Xfi = 'data/htm_file_list.pl',
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ), 
  retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  retractall( found_include_file( _, _, _, _, _, _, _, _, _, _, _ )),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz, 0 ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz, 1 ),


  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( prolog_src_dir_perform( _ ) ),  
  assert( prolog_src_dir_perform( 'H:/metta-wam-main/tests/baseline_compat/hyperon-mettalog_sanity/' ) ),
  prolog_src_dir_perform( Dirx_zz_2 ),  get_all_prolog_files( Tp, MdfKeyaft, Dirx_zz_2 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 0 ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 1 ),


  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( prolog_src_dir_perform( _ ) ),  assert( prolog_src_dir_perform( 'H:/hyperon-experimental/python/' ) ),
  prolog_src_dir_perform( Dirx_zz_3 ),  get_all_prolog_files( Tp, MdfKeyaft, Dirx_zz_3 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_3, 0 ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_3, 1 ),


  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.py' ) ),
  retractall( prolog_src_dir_perform( _ ) ),  assert( prolog_src_dir_perform( 'H:/hyperon-experimental/python/' ) ),
  prolog_src_dir_perform( Dirx_zz_4 ),  get_all_prolog_files( Tp, MdfKeyaft, Dirx_zz_4 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_4, 0 ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_4, 1 ),

  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.rs' ) ),
  retractall( prolog_src_dir_perform( _ ) ),  assert( prolog_src_dir_perform( 'H:/hyperon-experimental/' ) ),
  prolog_src_dir_perform( Dirx_zz_5 ),  get_all_prolog_files( Tp, MdfKeyaft, Dirx_zz_5 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 0 ),
  read_all_prolog_files( Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 1 ),


  append( 'result/prolog_codes_analyses.htm' ),
  display_metta_sresults( Zk, Dirx_zz ),
  told(),

  write( "analyses result written to file: \n" ),
  write( "result/prolog_codes_analyses.htm\n" ).



% aanroep: vb : start_analyse(0, '20210608', is_mk_update).
% aanroep: vb : start_analyse(0, '20210810', is_mk_update).
% aanroep: vb : start_analyse(0, '20211010', is_mk_update).
% aanroep: vb : start_analyse(0, '', is_mk_update).
% .pl
% H:/metta-wam-main/src/swi_analyse_metta/
%  working_directory(_,'H:/metta-wam-main/src/swi_analyse_metta/').
%  [metta_analyse].
%  start_analyse( 0, '', is_mk_update ).


% 
test11():- atom_codes( '42 times' , Codes ),   
 phrase( integer( X ), Codes, Res ),
 write( X ), nl .
 


%eval20claus( Lis ) --> "eval_20("  , Lis .
%test12():-  string_codes( "eval_20( aaqq ", Codes ),  
%   phrase( eval20claus( Lis ), Codes, _P ) ,
%   write_term( Lis, [] ), nl.

komma -->  [C],  { C == 44  }.
leftparenthese -->  [C],  { C == 40  }.
leftbracket -->  [C],  { C == 91  }.
singlequote -->  [C],  { C == 39  }.

% tag_x( foo ) --> "foo" .
% tag_x( bar( A, B ) ) -->  "bar", whites,    digits( ACodes ), whites, digits( BCodes ),  { number_codes( A, ACodes), number_codes( B, BCodes ) }.

% tag_x( bar( A, B ) ) -->  "bar(",   whites ,  digits( ACodes ), whites, komma , whites, digits( BCodes ),  { number_codes( A, ACodes), number_codes( B, BCodes ) }.
%tag_x( bar( Vn , B ) ) -->  "bar(",   whites ,  prolog_var_name( Vn ), whites, komma , whites, digits( BCodes ),  {  number_codes( B, BCodes ) }.
% tag_x( eval20( A , A2, A3, A4, B , X5 ) ) -->  "eval_20(",   whites ,  string( Vn ), whites, komma , whites, 
tag_x( eval20(  B  ) ) -->  "eval_20(",   whites ,  string( Vn ), whites, komma , whites, 
       string( Vn2 ), whites, komma , whites ,
       string( Vn3 ), whites, komma , whites ,
       string( Vn4 ), whites, komma , whites ,
     leftbracket , whites, singlequote ,  string( BCodes ),  singlequote, whites, komma ,
     string( Rest5 ),

  { string_codes( A, Vn ), string_codes( A2, Vn2 ), string_codes( A3, Vn3 ), string_codes( A4, Vn4 ),  string_codes( B, BCodes ), string_codes( X5, Rest5 ) }.


% tag_x( quux( S ) ) -->  "quux", whites, nonblanks( Codes ),   { string_codes( S, Codes ) }.


tag_list( [ Tag_token | Tags_tokens ]) -->    tag_x( Tag_token ),    whites,    tag_list( Tags_tokens ).
tag_list( [] ) --> [].

try_read_find_eval20_tag_in_string( Str , Result_tag ):-
%   string_codes( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).", Codes_x ),
   string_codes( Str , Codes_x ),   phrase( tag_list( Tags_tokens ),  Codes_x ),    Tags_tokens = [ Xres ],   Xres = eval20(  Result_tag  ) ,    !.



% eval_20(Eq,RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).
test12():-
% phrase(commands(Commands), `bar 1 2 foo quux bloop`),
   % string_codes( "eval_20( _Zxs , _Pa foo quux bloop", Codes_x ),

%   string_codes( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).", Codes_x ),
%   phrase( tag_list( Tags_tokens ),  Codes_x ), 
%   Tags_tokens = [ Xres ],   Xres = eval20(  Result_tag  ) ,    !,
   try_read_find_eval20_tag_in_string( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y)." , Result_tagx ),

%   maplist( writeln, Tags_tokens ).
   write_term( Result_tagx , [] ).



%----

start_analyse(Tp, MdfKeyaft, Is_mk_update ):-
  
  %retractall( chercher_a( _ ) ), assert( chercher_a( "eval_20&:-" ) ),
  %retractall( current_file_extension( _ ) ), assert( current_file_extension( '.pl' ) ),
  %retractall( prolog_src_dir_perform( _ ) ),  assert( prolog_src_dir_perform( 'H:/metta-wam-main/src/canary/' ) ),


  retractall( file_includes( _, _, _ ) ),

  retractall( has_db_clause( _, _, _, _, _, _, _, _ ) ),
  retractall( pred_is_called_from( _, _  ) ),
  retractall(  last_phead(_)),


  tell('result/prolog_codes_analyses.htm'),
  write("\n<html> <head> \n"),
  write("\n<script> \n"),
  write("function open_div(xid){ document.getElementById(xid).style.display = 'block'; }\n"),
  write("function close_div(xid){ document.getElementById(xid).style.display = 'none'; }\n"),
  write("\n</script> \n"),
  write("\n</head> \n"),
  write("\n<body style=\"font-family: arial; font-size: 12pt;\"> \n"),


  write(" reset file to empty \n"),
  told(),
  write("EMPTY file: \n"),
  write("\n<br> &nbsp <br> \n"),

  write("result/prolog_codes_analyses.htm\n"),
  prolog_src_dir_perform( Dirx_zz ),
%  start_analyse00(Tp, MdfKeyaft, Is_mk_update, Dirx_zz ), 
  start_analyse_metta(Tp, MdfKeyaft, Is_mk_update, Dirx_zz ), 
  fail, !.

start_analyse(_Tp, _MdfKeyaft, _Is_mk_update ):- 
%  append('result/prolog_codes_analyses.htm'),
%  display_pred_used(),
%  told(),
%  tell('result/prolog_code_db.pro'),
%  display_prolog_database_calls(),
%  told(),

!.
%---*******************

% ,
separate_prolog_code( Zk, _Dx ):-  
 file_level(0, Item, _, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Item,  Xf ], From ), 
% write("44debok"), write(),

 is_relevant_file(Is_include, From ),
 write( From ), nl,
 
 fail.

separate_prolog_code( Zk, Dx ):-  
 file_level(1, Item, _, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Xf ], From ), 
 is_relevant_file(Is_include, From ),
 write( From ), nl,
 fail.

separate_prolog_code( Zk, Dx ):-  
 file_level(2, Item, Sub, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Sub, '/', Xf ], From ), 
 is_relevant_file(Is_include, From ),
 write( From ), nl,
 fail.


separate_prolog_code( Zk, Dx ):-  
 file_level(3, Item, Item2, Sub,  _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 write( From ), nl,
  fail.

separate_prolog_code( Zk, Dx ):-  
 file_level(4, Item, Item2, Item3, Sub,   _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 write( From ), nl,
  fail.

separate_prolog_code( Zk, Dx ):-  
 file_level(5, Item, Item2, Item3, Item4, Sub,    _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Item4, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 write( From ), nl,
 fail.

separate_prolog_code( Zk, Dx ):- 
 file_level(6, Item, Item2, Item3, Item4, Item5, Sub,   _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Item4, '/', Item5, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 write( From ), nl,
 fail.

separate_prolog_code( _Zk, _Dx ):-  !.

%------


basic_walk00( MdfKeyaft, Dirx_walk ):-
  today_key( Dk ), write( "today key " ), write( Dk ),
  get_all_prolog_files( 0, MdfKeyaft, Dirx_walk ),
  sleep(2),
 % deze later 
  write(" start read files \n"),
  write("\n<br> &nbsp <br> \n"),
  retractall( found_search_result( _, _, _, _ ) ),


   Xfi = 'data/htm_file_list.pl',
   retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ), 
   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
   retractall( found_include_file( _, _, _, _, _, _, _, _, _, _, _ )),
   consult( Xfi ), 
   write("Consulted\n"), write(Xfi), nl, !.


% prolog_src_dir_perform( 'C:/jdklog/sources/partage_prolog/' ).
% basic 
% C:\jdklog\sources\util_pilote\swi_pilote_analyse_upd_prolog\prolog_analyse_update.pl
% working 
% working_directory(_,'C:/jdklog/sources/util_pilote/swi_pilote_analyse_upd_prolog').
% [prolog_analyse_update].
% basic_walk( '20240101', 'C:/jdklog/sources/PR_12_NvoDesign/Trilog5_NouveauLook_Design_MalusPoids_rn28mars2024_nv17mai/' ).
% 

basic_walk( MdfKeyaft, Dirx_walk ):-
  retractall( file_includes( _, _, _ ) ),

  retractall( has_db_clause( _, _, _, _, _, _, _, _ ) ),
  retractall( pred_is_called_from( _, _  ) ),
  retractall(  last_phead(_)),

  basic_walk00( MdfKeyaft, Dirx_walk ), 
  
  fail, !.

basic_walk( _MdfKeyaft, Dirx_walk ):-
   Zk = "Preserver",
   % read_all_prolog_files
   separate_prolog_code( Zk, Dirx_walk ),
   !.





 