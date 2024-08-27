
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
special_directory_files('../canary/', [ 'metta_eval.pl' , 'metta_interp.pl' , 'metta_ontology.pfc.pl' ]):-!.
 % 

special_directory_files(Dx, Lis):- !, directory_files(Dx, Lis).

%----

get_all_singularity_files( Tp, _, Dx):- retractall(dir_level(_, _, _,_, _,_,_,_,_,_)), retractall(file_level(_, _, _, _,_, _,_,_,_,_,_)),

 directory_files(Dx, Lis), member(El, Lis), El \= '..', El \= '.',
 allow_dir(Tp, Dx, El),
 atom_concat(Dx, El, Cx), exists_directory(Cx),
 assert( dir_level(1, El, '', '',   '','','','','','') ), fail, !.

get_all_singularity_files( Tp, _, Dx):-  
 dir_level( 1, Item, _, _, _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' ], Cy ), 
 directory_files( Cy, Lis ), 
 member( El, Lis ), El \= '..', El \= '.', 
 allow_dir(Tp, Cy, El),
 atom_concat( Cy, El, Cxy ), exists_directory( Cxy ),
 assert( dir_level( 2, Item, El, '',  '', '', '', '', '', '' ) ), fail, !.


% dir_level(2,'PR_12_NvoDesign',tools,'','','','','','','').

get_all_singularity_files( Tp, _, Dx ):-   dir_level( 2, Item, Item2, _, _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',  
 allow_dir(Tp, Cy, El),

 atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 3, Item, Item2, El, '',  '', '', '', '', '' ) ), fail, !.

get_all_singularity_files( Tp, _, Dx):-   dir_level( 3, Item, Item2, Item3, _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/', Item3, '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',  
 allow_dir(Tp, Cy, El),

 atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 4, Item, Item2, Item3, El, '', '', '', '', '' ) ), fail, !.


get_all_singularity_files( Tp, _, Dx):-   dir_level( 4, Item, Item2, Item3, Item4, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/', Item3, '/', Item4, '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',  
 allow_dir(Tp, Cy, El),

 atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 5, Item, Item2, Item3, Item4, El, '', '', '', '' ) ), fail, !.

get_all_singularity_files( Tp, _, Dx):-   dir_level( 5, Item, Item2, Item3, Item4, Item5, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/' , Item2 , '/', Item3, '/', Item4, '/', Item5, '/' ], Cy ),  directory_files( Cy, Lis ), 
 member( El, Lis ),  El \= '..', El \= '.',
 allow_dir(Tp, Cy, El),

   atom_concat( Cy, El, Cxy ),  exists_directory( Cxy ),
 assert( dir_level( 6, Item, Item2, Item3, Item4, Item5, El, '', '', '' ) ), fail, !.

% canary
get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
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


get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
 dir_level(1, Item, _,_, _,_,_,_,_,_), atomic_list_concat([ Dx, Item, '/'], Cy), directory_files(Cy, Lis), 
 member(El, Lis), El \= '..', El \= '.',  

 is_prolog_atom_file( El, Item ),
  atom_concat(Cy, El, Cx), time_file(Cx, Stamp), stamp_day_atom(Stamp, Dat),
 match_date_atom(Dat, Is_mdf_day_after),
 assert( file_level(1, Item, '', '','','','','','', El, Dat) ), fail,!.


get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
 dir_level(2, Item, Sub, _,  _, _, _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', 

 is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 2, Item, Sub, '', '', '', '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
 dir_level(3, Item, Item2, Sub,  _, _, _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 3, Item, Item2, Sub, '', '', '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
 dir_level(4, Item, Item2, Item3, Sub,  _, _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 4, Item, Item2, Item3, Sub, '', '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
 dir_level(5, Item, Item2, Item3, Item4, Sub,  _, _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Item4, '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 5, Item, Item2, Item3, Item4, Sub, '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):- 
 dir_level(6, Item, Item2, Item3, Item4, Item5, Sub,   _, _, _), 
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Item4, '/', Item5,  '/', Sub, '/' ], Cy ), 
 directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 6, Item, Item2, Item3, Item4, Item5, Sub, '', '', El, Dat ) ), fail,!.


get_all_singularity_files(_, _, _):- 
 write("start writing to data/htm_file_list.pl\n"),
 tell('data/htm_file_list.pl'), 
 write(":- dynamic dir_level/10."), nl,
 write(":- discontiguous dir_level/10."), nl,
 write(":- dynamic file_level/11."), nl,
 write(":- discontiguous file_level/11."), nl,
 fail.

get_all_singularity_files(_, _, _):-  dir_level(1, Item, Sub, X,   A,B,C,D,E,F),
 write_term(dir_level(1, Item, Sub, X, A,B,C,D,E,F),[ quoted(true) ]), write("."), nl, fail, !.

get_all_singularity_files(_,_, _):-  dir_level(2, Item, Sub, X,  A,B,C,D,E,F),
 write_term(dir_level(2, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail, !.

get_all_singularity_files(_,_, _):-  dir_level(3, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(3, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail, !.

get_all_singularity_files(_,_, _):-  dir_level(4, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(4, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail, !.

get_all_singularity_files(_,_, _):-  dir_level(5, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(5, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail, !.

get_all_singularity_files(_,_, _):-  dir_level(6, Item, Sub, X,  A,B,C,D,E,F),
  write_term(dir_level(6, Item, Sub, X,  A,B,C,D,E,F ),[ quoted(true) ]), write("."), nl, fail, !.


get_all_singularity_files(_,_, _):- retract( file_level(0, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(0, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files(_,_, _):- retract( file_level(1, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(1, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files(_,_, _):- retract( file_level(2, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(2, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files(_,_, _):- retract( file_level(3, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(3, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files(_,_,_):- retract( file_level(4, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(4, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files(_,_,_):- retract( file_level(5, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(5, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files(_,_,_):- retract( file_level(6, Item, Sub, El, Q, A,B,C,D,E,F) ),
 write_term( file_level(6, Item, Sub, El, Q,  A,B,C,D,E,F) ,[ quoted(true) ]), write("."), nl, fail, !.
get_all_singularity_files( _, _, _ ):- told(),  write( "finished writing to data/htm_file_list.pl\n" ), !.

% hier dus  Als het JS is  opschoon comment er uit 
% ALS het  htm is ?versie=  
% 
%   <script language="javascript" type="text/javascript" src="rjstool.js?vers=12" > </script>
% copyr(),
% 1 is htm 2 is js 
wr_newlin( 0 ):- ! .
wr_newlin( _ ):- ! , nl .


% write(From), nl, write(To), nl,

% :- dynamic found_base_teur/4.


read_has_search_strings( _, From, Zk, Level, Is_include , Isleading):- 
  sub_atom( From, _, _, _, '.metta' ),  size_file( From ,  Size ), Size < 700,   read_file_to_string(From, Bstr, [] ), !,
  if_extra_string_demand_search( Bstr ),  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 , Isleading).

read_has_search_strings( _,  From, Zk, Level, Is_include , Isleading):- 
  sub_atom( From, _, _, _, '.py' ),   size_file( From ,  Size ), Size < 700,   read_file_to_string(From, Bstr, [] ), !,
  if_extra_string_demand_search( Bstr ),  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 , Isleading).

read_has_search_strings(  _, From, Zk, Level, Is_include , Isleading):- 
  sub_atom( From, _, _, _, '.rs' ),   size_file( From ,  Size ), Size < 700,   read_file_to_string(From, Bstr, [] ), !,
  if_extra_string_demand_search( Bstr ),  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 , Isleading).


read_has_search_strings(  is_octet , From, Zk, Level, Is_include , Isleading):- 
 retractall( comment_started() ),
 open( From, read, Sea , [ encoding(octet) ] ), 
 read_has_search_stream( Sea, Zk, From, Level, 1, Is_include , Isleading ),  
 close( Sea ).

read_has_search_strings(  is_utf8 , From, Zk, Level, Is_include , Isleading):- 
 retractall( comment_started() ),
 open( From, read, Sea , [ encoding(utf8) ] ), 
 read_has_search_stream( Sea, Zk, From, Level, 1, Is_include , Isleading),  
 close( Sea ).

read_has_search_strings( _, _, _, _, _ , _ ):- !.







 
 
%---
%read_all_singularity_files(_Tp, _, _Isalways_copy, _Is_update, _, _):- 
% Xfi = 'data/htm_file_list.pl',
% retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ), 
% retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
% consult( Xfi ), 
% write("Consulted\n"), write(Xfi),
% fail.

% separate_prolog_code( Zk, Dirx_walk ),

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, _Dx, Is_include , Isleading):-  
 file_level(0, Item, _, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Item,  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( Is_file_type, From, Zk, 0, Is_include, Isleading ), fail.

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include, Isleading):-  
 file_level(1, Item, _, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings(Is_file_type, From, Zk, 1, Is_include , Isleading), fail.

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include, Isleading):-  
 file_level(2, Item, Sub, _, _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Sub, '/', Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( Is_file_type,From, Zk, 2, Is_include , Isleading), fail.

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include, Isleading):-  
 file_level(3, Item, Item2, Sub,  _, _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( Is_file_type,From, Zk, 3, Is_include , Isleading), fail.

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include, Isleading):-  
 file_level(4, Item, Item2, Item3, Sub,   _, _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( Is_file_type,From, Zk, 4, Is_include , Isleading), fail.

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include, Isleading):-  
 file_level(5, Item, Item2, Item3, Item4, Sub,    _, _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Item4, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( Is_file_type,From, Zk, 5, Is_include , Isleading), fail.

read_all_singularity_files(Is_file_type,_Tp, Zk, _Isalways_copy, _Is_update, Dx, Is_include, Isleading):- 
 file_level(6, Item, Item2, Item3, Item4, Item5, Sub,   _, _, Xf, _ ),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Item3, '/', Item4, '/', Item5, '/', Sub, '/',  Xf ], From ), 
 is_relevant_file(Is_include, From ),
 read_has_search_strings( Is_file_type,From, Zk, 6, Is_include , Isleading), fail.


% HIER NOG LEVEL   4  5  6  7
% assert( file_level( 2, Item, Sub, El, Dat,  '', '', '', '', '', '' ) ), fail,!.




read_all_singularity_files( _, _, _, _, _, _, _ , _):-  !.




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

:- dynamic comment_started/0.

not_is_comment( Lx ):-
 sub_string( Lx, Sta, _, _, "%"), Sta < 5,!, fail.
not_is_comment( _ ):- !.

% gaat fout bij genest 
zet_comment_started(Lx):-
 sub_string( Lx, _, _, _, "/*"),!,  assert( comment_started() ).
zet_comment_started(_):-!.

zet_comment_ended(Lx):-   sub_string( Lx, _, _, _, "*/"),   retractall(  comment_started() ),   !. 
zet_comment_ended(_):- !.

is_not_inside_comment():- comment_started(),!, fail.
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



%---
% we dont want all the assertEqual etc here 
is_metta_function( Tag ):- string_lower( Tag, Tag_lower ), sub_string( Tag_lower, _,_,_, "assert" ), !, fail.

is_metta_function( Tag ):-
  string_length( Tag, Lex ), Lex > 1,  Tag \= "eval", !.

%-----
:- dynamic eval_tag/1.


%  sub_str_between( Lx, "['", "'", Tag ),

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_leading ):-
  sub_atom( F, _,_,_, 'metta_eval'),  search_o_y( Lx, "eval_20&:-" ),
  try_read_find_eval20_tag_in_string( Lx , Tag ),  is_metta_function( Tag ),
  not( eval_tag( Tag ) ),  assert( eval_tag( Tag ) ),  !,
  asserta( found_search_result( Level, Linum, F, Tag ) ).

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_not_leading ):-
  sub_atom( F, _,_,_, 'metta_eval'),  search_o_y( Lx, "eval_20&:-" ),
  try_read_find_eval20_tag_in_string( Lx , Tag ),
  eval_tag( Tag ),  
  % sub_string( Lx, _, _, _ , Tag ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, Tag, Cp ), string_concat( Cp, " </b> " , C3 ), 
  assert( found_search_result( Level, Linum, F, C3 ) ).

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , _Isleading):-
  sub_atom( F, _,_,_, 'metta_interp'),  search_o_y( Lx, "eval_h(&:-" ),  eval_tag( Tag_s ),
  string_concat("'", Tag_s, C1 ), string_concat( C1, "'", C2 ),  sub_string( Lx, _, _, _ , C2 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C2, Cp ), string_concat( Cp, " </b> " , C3 ),   !,
  assert( found_search_result( Level, Linum, F, C3 ) ).


search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , _Isleading):-
  sub_atom( F, _, _, _, 'metta_ontology.pfc' ),  search_o_y( Lx, "properties(" ),  eval_tag( Tag_s ),
  string_concat("'", Tag_s, C1 ), string_concat( C1, "'", C2 ),
  sub_string( Lx, _, _, _ , C2 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C3 ),   !,
  assert( found_search_result( Level, Linum, F, C3 ) ).

%  search_o_y( Lx, "assertequaltoresult&" ),
% (@doc intersection

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_not_leading ):-
  sub_atom( F, _, _, _, '.metta' ),  eval_tag( Tag_s ),  string_concat( "(", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , C1 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).

search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ,  is_not_leading ):-
  sub_atom( F, _, _, _, '.metta' ),  eval_tag( Tag_s ),  string_concat( "(@doc ", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , C1 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).


%search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
%  file_name_extension( _, Ext, F ), Ext == 'rs',
%  C1 = "assert_eq!(result",
%  sub_string( Lx, _, _, _ , C1 ), !,
%  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
%  assert( found_search_result( Level, Linum, F, C2  ) ).


search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_not_leading):-
  % sub_atom( F, _, _, _, '.rs' ),  
  % file_base_name( From2 , Fnx ),  
  file_name_extension( _, Ext, F ), Ext == 'rs',
  eval_tag( Tag_s ),  
  % string_concat( "new(\"!(", Tag_s, C1 ), 
  string_concat( "regex(r\"", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , C1 ), !,
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).

% kopie 
search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_leading ):-
  file_name_extension( _, Ext, F ), Ext == 'rs',
  % sub_str_between( Lx, "new(\"!(", " ", Subz ),
  sub_str_between( Lx, "regex(r\"", "\"", Subz ),
  not( eval_tag( Subz ) ),  assert( eval_tag( Subz ) ),  
  asserta( found_search_result( Level, Linum, F, Subz  ) ).



search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_not_leading ):-
  sub_atom( F, _, _, _, '.py' ),  
  eval_tag( Tag_s ),  string_length( Tag_s, Lex ), Lex > 3, 
  % assert_eq 
  concat( "metta.run('!(", Tag_s, C1 ), 
  sub_string( Lx, _, _, _ , C1 ), 
  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
  assert( found_search_result( Level, Linum, F, C2  ) ).

% kopie 
search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum , is_leading ):-
  sub_atom( F, _, _, _, '.py' ),  
%  eval_tag( Tag_s ),  string_length( Tag_s, Lex ), Lex > 3, 
  sub_str_between( Lx, "metta.run('!(", " ", Subz ), !,
  not( eval_tag( Subz ) ),  assert( eval_tag( Subz ) ),  
  asserta( found_search_result( Level, Linum, F, Subz  ) ).



:- dynamic extra_string_to_search/1.

if_extra_string_demand_search( Lx ):-
  extra_string_to_search( Str ), !, search_o_y( Lx, Str ).
if_extra_string_demand_search( _Lx ):- !.

%search_o_y_and_assert( 1, Lx, Zk, F, Level, Linum ):-
 % search_o_y( Lx, Zk ),!,
 % assert( found_search_result( Level, Linum, F, Lx ) ).

%----

read_has_search_stream( Sea, Zk, F, Level, Linum, Is_include, Isleading):- not( at_end_of_stream( Sea ) ),
 read_line_to_string( Sea, Lx ), Lx \= end_of_file, not_is_comment(Lx),
   zet_comment_started(Lx), zet_comment_ended(Lx),   is_not_inside_comment(),
   if_extra_string_demand_search( Lx ),
   search_o_y_and_assert( Is_include, Lx, Zk, F, Level, Linum , Isleading ),
   
   !,  
   Linum2 is Linum + 1,
 read_has_search_stream( Sea, Zk, F, Level, Linum2, Is_include , Isleading).

read_has_search_stream( Sea, Zk, F, Level, Linum, Is_include, Isleading):- not( at_end_of_stream(Sea) ),!, 
  Linum2 is Linum + 1, read_has_search_stream( Sea, Zk, F, Level, Linum2, Is_include, Isleading).
read_has_search_stream(_ , _, _, _, _, _, _):- !.

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

:- dynamic singularity_src_dir_perform/1.
singularity_src_dir_perform( '../canary/' ).


% singularity_src_dir_perform( 'c:/jdklog/sources/PR_12_NVODESIGN/TRILOG5_NOUVEAULOOK_DESIGN/' ).
%  impr
%  je  



is_prolog_atom_file( At, _Dx ):- downcase_atom(At , At2),  
 current_file_extension( Fext ),
 sub_atom( At2, _, _, _ , Fext ),!.

is_prolog_atom_file( _At, _Dx ):-  !, fail.


% is_prolog_atom_file( At, _Dx ):-  sub_atom( At, _, _, _ , 'base_teur32.dba' ),!.


only_via_map_file( 1 ):-!.
% only_via_map_file( 0 ):-!.

% e_mousedown

allow_dir2( _, _, _ ):- !.



%allow_dir2( _, Pa, Xfile ):- 
 % write("check path OLD "), write(Pa), nl,
% ( sub_atom(Xfile, _, _, _, '/old' ); sub_atom(Pa, _, _, _, '/old' ) ),!, fail.


% allow_dir2( _, Pa, Xfile ):- sub_atom(Pa, _, _, _, 'utilitaires' ),!,
% ( sub_atom( Xfile, _, _, _, 'param_div_nvo') ;
%  sub_atom( Xfile, _, _, _, 'param10e')   ), !.

allow_dir2( _, _, _ ):- !.

allow_dir2( 1, _, _ ):- !.

  
% hier files uitsluiten indien nodig


% ignore all .pyc files 
is_relevant_file(_, From ):-  downcase_atom( From, From2 ),
   sub_atom(From2, _, _, _,  '.pyc' ), !, fail .

is_relevant_file(_, From ):-  downcase_atom( From, From2 ),
  current_file_extension( Fext ),  sub_atom(From2, _, _, _,  Fext ), !.

is_relevant_file(_, _From ):-   !, fail.


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





% werkt niet 


% temp _ext

write_copy_link( Pa, _Fnx ):-
 write( "<a onclick=\"prompt('copy path','"),
 write(Pa), 
 % write(Fnx), 
 write("');\" style=\"cursor: pointer\"> Fullpath  </a> \n" ).

if_small_no_newline( Lex ):- Lex < 12, !,  write( "  &nbsp  &nbsp " ) .
if_small_no_newline( _Lex ) :-   write( " <br> &nbsp <br>" ), nl.



fresults_par_file( Lp2, Lp, Fnx,  Filepath , Zk ):-
 write_copy_link( Filepath, Fnx ),
 write( "\n<br> <a onclick=\"open_div('"), write(Lp2), write(Lp),  write(Fnx), write("')\"  style=\"cursor:pointer\">  Open </a>" ), nl,
 write( "\n <a onclick=\"close_div('"), write(Lp2), write(Lp),  write(Fnx), write("')\" style=\"cursor:pointer\">  Close </a>" ), nl,
 write( "\n<div id=\""), write(Lp2), write(Lp),  write(Fnx), 
 write("\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #676767; color: #E7E7E7 \" >  " ), 
 nl,
 found_search_result( _Level, _Lnum, Filepath, Txtline ),
 write_line_with_tag_colors( Txtline, Zk ), 
 write( "\n" ),
 string_length( Txtline, Lex ), if_small_no_newline( Lex ) ,
 fail, ! .

fresults_par_file( _, _, _Fnx, _ , _):- !, write( "\n  </div>\n" ).


 
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


display_metta_sresults( Zk, _Dirx ):-
 write("\n\n<h2>  <pre> "),
 % str_code_replace( Zk, 60, 32, Zkx5), str_code_replace( Zkx5, 62, 32, Zkx6),
 % write( Zkx6 ), 
  write("</pre> </h2>"),  
 % write("<h2>"),write(" in directory: \n"), write("</h2>"),
 % write("<h2>"), write( Dirx ), write("</h2> <hr>"), nl, nl,
 % findall( Fpz,  found_search_result( _, _, Fpz, _ ), Fpzl),
  findall( Fpz,  per_file(  Fpz ), Fpzl ),
  % sort( Fpzl , Fpzl2 ), 
   member( Filepath, Fpzl ), 
   file_base_name( Filepath , Fnx ),
   dir_get_last_path( Filepath , Lp),  dir_get_last_path2( Filepath , Lpx2), file_base_name( Filepath , Fnx ),
 write("\n<br> &nbsp <br> \n"), write("\n<br> &nbsp <br> \n"),
 nl,nl,nl, write("***"), write(" <i><b> "), write( Fnx ), write("</i></b> <b> &nbsp &nbsp &nbsp "), 
 write(Lp), write("</b> &nbsp &nbsp &nbsp "), write(Lpx2), nl,
 % display_if_via_include( Filepath ), 
 file_directory_name( Filepath , Dir ), write( Dir ), nl,
 fresults_par_file( Lpx2, Lp, Fnx, Filepath, Zk ),
 fail, !.

 display_metta_sresults(_, _):-  write("</pre>"), !.
%---






lis_und_concat( [], Hs,  Hs ):- !.
lis_und_concat( [ _ ], Hs,  P2 ):- string_concat( Hs, "_", P2).
lis_und_concat( [ _ |Lis], Hs,  A_args2 ):- string_concat( Hs, "_,", P2),
  lis_und_concat( Lis, P2,  A_args2 ).

 
% write_term(  has_pred_db_ata( Dbname, El, A_args ), [] ),
% split 
str_to_und_score( A_args, A_args2 ):-
   split_string( A_args, ",", " ()", Lis),
   lis_und_concat( Lis, "",  A_args2),!.







%display_pred_used():- nl,write("<br> &nbsp <br> &nbsp <br>  &nbsp <br>  <h1> <b> LOADING: </b> </h1> <hr>\n "), nl,
% findall( F, pred_is_called_from( _, _, _, 0, _, F, _,_ ), Flis ),
% sort( Flis, Flis2 ), member( Xel , Flis2 ), file_base_name( Xel, Fb), write(" <h2> <b>"), write(Fb), write("</b> </h2>"),
% wri_load_list(Xel),
%  fail,!.

numspace(Tel):- Tel > 0, !, write("&nbsp"), Tel2 is Tel - 1, numspace(Tel2).
numspace(_):- !.

%find_reverse_pad( Zk, "", Tel):-  Tel < 20,
% write(" <br> &nbsp <br> "), nl, 
% write(Zk), write("( <br>"), nl,
% pred_is_called_from( Ph, Pred_nm ), sub_string( Pred_nm, _, _, _, Zk), !,
% Tel2 is Tel + 1,
%   numspace(Tel), write(Ph), write("( <br>"), nl,
% find_reverse_pad( Zk, Ph, Tel2).

write_line_with_tag_colors( Head2, Zk ):-
 concat( Zk, "", Zk2 ), concat( "<b>", Zk2, C1 ), concat( C1, "</b>", C2 ),
 str_replace_tag(Head2, Zk2, C2, Resu),
 write(Resu ), !.

write_line_with_tag_colors( Head2, _Zk ):- write( Head2 ). 





%----

search_prolog_files( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi , Isleading ):- !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "eval_20&:-" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.pl' ) ),
  retractall( singularity_src_dir_perform( _ ) ),  assert( singularity_src_dir_perform( '../canary/' ) ),
  singularity_src_dir_perform( Dirx_zz_00 ),
  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_00 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ), 
  retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  retractall( found_include_file( _, _, _, _, _, _, _, _, _, _, _ )),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, 'dummy_001', 0 , Isleading ),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, 'dummy_001', 1, Isleading ).

%--

search_metta_files( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),  
  assert( singularity_src_dir_perform( '../../tests/baseline_compat/hyperon-mettalog_sanity/' ) ),
  singularity_src_dir_perform( Dirx_zz_2 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_2 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 0 , Isleading),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 1 , Isleading).


search_metta_files_hyperon( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),  assert( singularity_src_dir_perform( '../../../hyperon-experimental/python/' ) ),
  singularity_src_dir_perform( Dirx_zz_3 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_3 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_3, 0 , Isleading),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_3, 1 , Isleading).


search_python_files_hyperon( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.py' ) ),
  retractall( singularity_src_dir_perform( _ ) ),  assert( singularity_src_dir_perform( '../../../hyperon-experimental/python/' ) ),
  singularity_src_dir_perform( Dirx_zz_4 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_4 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_4, 0 , Isleading),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_4, 1 , Isleading).


search_rust_files_hyperon( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.rs' ) ),
  retractall( singularity_src_dir_perform( _ ) ),  assert( singularity_src_dir_perform( '../../../hyperon-experimental/' ) ),
  singularity_src_dir_perform( Dirx_zz_5 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_5 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ), 
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 0 , Isleading),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 1 , Isleading).

%---

search_all_files_with_prolog_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ):-
  search_prolog_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_leading ),
%  search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
%  search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_python_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_rust_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ), !.


search_all_files_with_python_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ):-
  search_python_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_leading ),
  search_prolog_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
%  search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
%  search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_rust_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ), !.

search_all_files_with_rust_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ):-
  search_rust_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_leading ), 
  search_prolog_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_python_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),!.


%  search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
%  search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),


reset_htm( ResTag ):-
  atomic_list_concat([ 'result/prolog_codes_analyses_', ResTag, '.htm' ], Htmfile ),
  tell(Htmfile),
  write("\n<html> <head> \n"),
  write("\n<script> \n"),
  write("function open_div(xid){ document.getElementById(xid).style.display = 'block'; }\n"),
  write("function close_div(xid){ document.getElementById(xid).style.display = 'none'; }\n"),
  write("\n</script> \n"),
  write("\n</head> \n"),
  write("\n<body style=\"font-family: arial; font-size: 12pt;\"> \n"),


  write(" <H2> \n"), write( ResTag ), 
  write(" </H2> \n"),
  told().



%---

start_analyse_metta( Tp, MdfKeyaft, Is_mk_update,  _X ):- !,
  retractall( eval_tag( _ ) ),
  retractall( found_search_result( _, _, _, _ ) ),
  % ResTag = 'prolog_leading', 
  ResTag = 'python_leading', 
  % ResTag = 'rust_leading', 
  reset_htm( ResTag ),
%  retractall( extra_string_to_search( _ ) ), 
   % assert( extra_string_to_search( "count" ) ),
  chercher_a( Zk ),  today_key( Dk ), write( "today key " ), write( Dk ),
  Xfi = 'data/htm_file_list.pl',
 
  % search_all_files_with_prolog_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi  ),
  search_all_files_with_python_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ),
  % search_all_files_with_rust_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ),
  atomic_list_concat([ 'result/prolog_codes_analyses_', ResTag, '.htm' ], Htmfile ),
  append( Htmfile ),
  display_metta_sresults( Zk, 'dummyxxaaq' ),
  told(),

  write( "analyses result written to file: \n" ),
  write( Htmfile ), nl.



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
 phrase( integer( X ), Codes, _Res ),
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

  { string_codes( _A, Vn ), string_codes( _A2, Vn2 ), string_codes( _A3, Vn3 ), string_codes( _A4, Vn4 ),  string_codes( B, BCodes ), string_codes( _X5, Rest5 ) }.


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
  %retractall( chercher_a( _ ) ), assert( chercher_a( "eval_20&:-" ) ),
  %retractall( current_file_extension( _ ) ), assert( current_file_extension( '.pl' ) ),
  %retractall( singularity_src_dir_perform( _ ) ),  assert( singularity_src_dir_perform( 'H:/metta-wam-main/src/canary/' ) ),


start_analyse(Tp, MdfKeyaft, Is_mk_update ):-

  retractall( file_includes( _, _, _ ) ),
  retractall( has_db_clause( _, _, _, _, _, _, _, _ ) ),
  retractall( pred_is_called_from( _, _  ) ),
  retractall(  last_phead(_)),
  
  write("EMPTY file: \n"),
  write("\n<br> &nbsp <br> \n"),
  write("result/prolog_codes_analyses.htm\n"),
  
%  start_analyse00(Tp, MdfKeyaft, Is_mk_update, Dirx_zz ), 
  start_analyse_metta(Tp, MdfKeyaft, Is_mk_update, 'xdummy' ), 
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

%------




 