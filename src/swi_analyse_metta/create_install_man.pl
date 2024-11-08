


:- use_module(library(dcg/basics)).

:- encoding(utf8).


%-----
:- dynamic eval_tag/6.
:- dynamic eval_tag_secondary/2.
:- dynamic eval_tag_found/6.
:- dynamic found_search_result/6.




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
special_directory_files('../canary/', [ 'metta_eval.pl' , 'metta_interp.pl' , 'metta_ontology.pfc.pl', 'stdlib_mettalog.metta' ]):- !.
% experime
%special_directory_files('../../../hyperon-experimental/lib/src/metta/runner/', [ 'stdlib_minimal.rs' , 'stdlib.rs'  ]):- !.
special_directory_files('../../../hyperon-experimental/lib/src/metta/runner/', [ 'stdlib_minimal.rs' , 'stdlib.rs'  ]):- !.



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
 dir_level(1, Item, _,_, _,_,_,_,_,_), atomic_list_concat([ Dx, Item, '/'], Cy), special_directory_files(Cy, Lis),
 member(El, Lis), El \= '..', El \= '.',

 is_prolog_atom_file( El, Item ),
  atom_concat(Cy, El, Cx), time_file(Cx, Stamp), stamp_day_atom(Stamp, Dat),
 match_date_atom(Dat, Is_mdf_day_after),
 assert( file_level(1, Item, '', '','','','','','', El, Dat) ), fail,!.


get_all_singularity_files(_, Is_mdf_day_after, Dx):-
 dir_level(2, Item, Sub, _,  _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/', Sub, '/' ], Cy ),
 special_directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.',

 is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 2, Item, Sub, '', '', '', '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):-
 dir_level(3, Item, Item2, Sub,  _, _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/', Item2, '/', Sub, '/' ], Cy ),
 special_directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 3, Item, Item2, Sub, '', '', '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):-
 dir_level(4, Item, Item2, Item3, Sub,  _, _, _, _, _),
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Sub, '/' ], Cy ),
 special_directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 4, Item, Item2, Item3, Sub, '', '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):-
 dir_level(5, Item, Item2, Item3, Item4, Sub,  _, _, _, _),
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Item4, '/', Sub, '/' ], Cy ),
 special_directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
 atom_concat(Cy, El, Cx), time_file( Cx, Stamp ), stamp_day_atom( Stamp, Dat ),
 match_date_atom( Dat, Is_mdf_day_after ),
 assert( file_level( 5, Item, Item2, Item3, Item4, Sub, '', '', '', El, Dat ) ), fail,!.

get_all_singularity_files(_, Is_mdf_day_after, Dx):-
 dir_level(6, Item, Item2, Item3, Item4, Item5, Sub,   _, _, _),
 atomic_list_concat([ Dx, Item, '/', Item2, '/',  Item3, '/', Item4, '/', Item5,  '/', Sub, '/' ], Cy ),
 special_directory_files( Cy, Lis ), member( El, Lis ), El \= '..', El \= '.', is_prolog_atom_file( El, Sub ),
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


%read_has_search_strings( _, From, Zk, Level, Is_include , Isleading):-
%  sub_atom( From, _, _, _, '.metta' ),  size_file( From ,  Size ), Size < 700,   read_file_to_string(From, Bstr, [] ), !,
%  if_extra_string_demand_search( Bstr ),  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 , Isleading).

%read_has_search_strings( _,  From, Zk, Level, Is_include , Isleading):-
%  sub_atom( From, _, _, _, '.py' ),   size_file( From ,  Size ), Size < 700,   read_file_to_string(From, Bstr, [] ), !,
%  if_extra_string_demand_search( Bstr ),  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 , Isleading).

%read_has_search_strings(  _, From, Zk, Level, Is_include , Isleading):-
%  sub_atom( From, _, _, _, '.rs' ),   size_file( From ,  Size ), Size < 700,   read_file_to_string(From, Bstr, [] ), !,
%  if_extra_string_demand_search( Bstr ),  search_o_y_and_assert( Is_include, Bstr, Zk, From, Level, 0 , Isleading).


read_has_search_strings(  is_octet , From, Zk, Level, Is_include , Isleading):-
 retractall( comment_started() ),

 write( "check must read : " ), write( From ), nl ,
 not_exclude_metta_file( From ),
 max_file_size( From , Max),
 size_file( From, Size ),  Size < Max,
 write( "Start read : " ), write( From ), nl ,
 open( From, read, Sea , [ encoding(octet) ] ),
 read_has_search_stream( Sea, Zk, From, Level, 1, 1, Is_include , Isleading ),
 close( Sea ).


% % mogelijke waarden   octet, ascii,  iso_latin_1  , text, utf8    , unicode_be, unicode_le

read_has_search_strings(  is_utf8 , From, Zk, Level, Is_include , Isleading):-
 retractall( comment_started() ),

 write( "check must read  : " ), write( From ), nl ,
 not_exclude_metta_file( From ),
 max_file_size( From , Max ),
 size_file( From, Size ),  Size < Max,
 write( "Start read : " ), write( From ), nl ,
 open( From, read, Sea , [ encoding(utf8) ] ),
 read_has_search_stream( Sea, Zk, From, Level, 1, 1, Is_include , Isleading),
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

%str_replace_tag(At, Repwhat, RepWith, Resu):-  str_part_before_tag(At, Repwhat, Pa_before), str_part_after_tag(At, Repwhat, Pa_aft),!,
% string_concat(Pa_before,RepWith, C1), string_concat(C1,Pa_aft, Resu).
%str_replace_tag(At, _,_, At):- !.


str_replace_tag(At, Repwhat, RepWith, Resu):-  str_part_before_tag(At, Repwhat, Pa_before), str_part_after_tag(At, Repwhat, Pa_aft),!,
 string_concat(Pa_before,RepWith, C1), string_concat(C1,Pa_aft, Resu).
str_replace_tag(At, _,_, At):- !.



lis_code_remove([], _,  [], [] ):-!.
lis_code_remove([H|Codes1], Cde,  [Cde |Codes2], Lisnot ):-  H = Cde, !,
 lis_code_remove(Codes1, Cde,  Codes2, Lisnot).
lis_code_remove([H|Codes1], Cde,  Codes2 , [ H |Lisnot]):-  !, lis_code_remove(Codes1, Cde, Codes2, Lisnot).

%---

lis_code_count( [], _,  Res, Res ):- !.
lis_code_count( [ H | Codes1 ], Cde,  Count, Res ):-  H = Cde, !,
 Count2 is Count + 1, lis_code_count( Codes1, Cde,  Count2, Res ).
lis_code_count( [ _ | Codes1 ], Cde,  Count , Res ):-  !, lis_code_count( Codes1, Cde , Count , Res ).

%---

str_code_remove( Str, Cde,  Str2 ):-
 string_codes( Str, Codes1 ),
  lis_code_remove( Codes1, Cde, _, Codes2 ),!, string_codes( Str2, Codes2 ).

str_code_occurence_count( Str, Cde,  Count ):-
  string_codes( Str, Codes1 ),
  lis_code_count( Codes1, Cde, 0, Count ), !.

%----

% ,
read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, Count, _Max, _Sea , StrBuf, StrBuf, Count ):-  Count_is_open == 0, !.


read_extra_lines_max_until_full_metta_claus( Rej_zero, Count_is_open, Count, Max, Sea , StrBuf, Xtra_lines, ResultCount ):-   Count < Max,
	read_line_to_string( Sea, Lx ), Lx \= end_of_file, !,
    parenthesis_count( Lx, Count_open ), NewCount is Count_is_open + Count_open,
	string_concat( StrBuf, "\n", C2 ), string_concat( C2, Lx, StrBuf2 ),
	Count2 is Count + 1,
  read_extra_lines_max_until_full_metta_claus( Rej_zero, NewCount, Count2, Max, Sea , StrBuf2, Xtra_lines, ResultCount ).


read_extra_lines_max_until_full_metta_claus( _, _Count_is_open, Count, _Max, _Sea , StrBuf, StrBuf, Count ):-   !.
%--
%write_line_with_tag_colors( Ext, Txt ):-  !,
%  string_codes( Txt, Codes ),
%  write_codes_with_tag_colors( Ext, Codes ).

%---
write_metta_clause_htm( Lx ):-

%   str_replace_tag(At, Repwhat, RepWith, Resu)

  string_codes( Lx, Codes ),
  % write_codes_with_tag_colors( Ext, Codes ).
  Parenthesis_level is 0,
  write_codes_metta_clause_htm( Codes , Parenthesis_level ).

%--
write_big_tag( Tag ):-
  write( "\n<div class=\"big_tag\"  >  " ),
  write( Tag ),
  write( "\n</div>  " ).



%--
write_metta_clause( Lx ):-  parenthesis_count( Lx, Count_is_open ), Count_is_open == 0, !,
 write( "\n<div class=\"metta_clause\"  >  " ),
 write_metta_clause_htm( Lx ),
 write( "\n</div>  " ).

write_metta_clause( Lx ):-   parenthesis_count( Lx, Count_is_open ),
 number_string( Count_is_open, S2 ), !, write( "parenthesis" ), write( S2 ), write( " " ),
 write( Lx ), !.

write_metta_clause( Lx ):- write( Lx ), !.




%------
parenthesis_count( Lx, Count_is_open ):-  str_code_occurence_count( Lx, 40,  Count1 ),
  str_code_occurence_count( Lx, 41,  Count2 ),  Count_is_open is Count1 - Count2.
%  Count1 = Count2 , !.
% lines_extra_for_metta_count_line_open_close( _Lx, 4 ):-   !.


extra_lines_for_open_parenthesis( Count_is_open, Xtra_lines ):-   Count_is_open == 0, Xtra_lines is 0 , !.
extra_lines_for_open_parenthesis( _Count_is_open, Xtra_lines ):-  Xtra_lines is 4, !.

%adhoc_parse_rust_regx( Str, Btag, Etag, Subz ):-  sub_str_between( Str, Btag, Etag, Subz ), !.

%adhoc_parse_rust_regx( Str, Btag, _Etag,  Subz2 ):-
%  sub_str_between( Str, Btag, ")", Subz ),
%  str_code_remove( Subz, 34,  Subz2 ).


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


sub_str_between_plus_rest( Str, Btag, Etag, Subz , Subz2 ):-
 string_length( Str, Le0 ),
 sub_string( Str, Sta1, _, _, Btag ),
 string_length( Btag, Le1 ),
 Y is Sta1 + Le1,
 Z is Le0 - Y,
 sub_string( Str, Y, Z, _, Sub1 ),
 sub_string( Sub1, Sta2, _, _, Etag ),
 Z2 is Sta2 - 0,
 sub_string( Sub1, 0, Z2, Aft, Subz ),
 string_length( Etag, Le01 ),
 %Q is Le01 - Z2,
 Z3 is Z2 + Le01,
 Aft3 is Aft - Le01,
 sub_string( Sub1, Z3, Aft3, _, Subz2 ),

 !.



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

%lis_code_remove([], _,  [], [] ):-!.
%lis_code_remove([H|Codes1], Cde,  [Cde |Codes2], Lisnot ):-  H = Cde, !,
% lis_code_remove(Codes1, Cde,  Codes2, Lisnot).
%lis_code_remove([H|Codes1], Cde,  Codes2 , [ H |Lisnot]):-  !, lis_code_remove(Codes1, Cde, Codes2, Lisnot).

str_code_replace( Str, Cde, Cde2, Str2):-
 string_codes(Str, Codes1),
  lis_code_replace(Codes1, Cde, Cde2, Codes2),!, string_codes(Str2, Codes2).

%str_code_remove( Str, Cde,  Str2):-
% string_codes(Str, Codes1),
%  lis_code_remove(Codes1, Cde, _, Codes2),!, string_codes(Str2, Codes2).


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

assert_eval_tag_found( Type, Tag , _ , _ , _ ):- eval_tag_found( Type, Tag, _, _, _ , _), !.
assert_eval_tag_found( Type, Tag , Tg1, Tg2, Tg3 ):- !, assert( eval_tag_found( Type, Tag, Tg1, Tg2, Tg3, 0 ) ).
%--------------------
assert_eval_tag_mult( Type_tag, Linum, Tag, F, Sline ):-    string_lower( Tag, Taglow ),
  assert( eval_tag( Type_tag, Linum, Taglow, F , Sline, 0 ) ),  !.

%-----------
assert_eval_tag( Type_tag, _Linum, Tag, _F, _, _Ari ):-  string_lower( Tag, Taglow ) , eval_tag( Type_tag, _, Taglow, _ , _, _ ), !.
assert_eval_tag( Type_tag, Linum, Tag, F, Sline, Ari ):-    string_lower( Tag, Taglow ),  assert( eval_tag( Type_tag, Linum, Taglow, F , Sline , Ari) ),  !.

%-----

trim_string(Str, Sc):-  split_string(Str, "", " ", [Sc]),!.
trim_string(Sc, Sc):- !.

%  sub_str_between( Lx, "['", "'", Tag ),

%is_debugging( 1 ):- !.
is_debugging( 0 ):- !.

debug_string( S1, S2 ):- is_debugging( Is ), Is == 1,!,
 write( S1 ), write( " " ), write( S2 ), nl .
debug_string( _, _ ):-!.

% try_read_ontology("properties('&corelib','if', [flow_control, qhelp(\"Conditional execution.\"), conditional_execution]).", T1, T2, T3 ).
% try_read_ontology("properties('&corelib','let', [variable_assignment, qhelp(\"Variable assignment.\")]).").
try_read_ontology( Lx0, Tag1, Tag2, Tag3 ):-
  string_lower( Lx0, Lx2 ),  str_code_remove( Lx2, 34,  Lx ),
  sub_str_between_plus_rest( Lx, "[", ",", Tag1, Rest1  ),
  sub_str_between_plus_rest( Rest1, "qhelp(", ")", Tag2, Rest2 ),
  sub_str_between_plus_rest( Rest2, ",", "]", Tag3, _Rest3 ), !.

try_read_ontology( Lx0, Tag1, Tag2, "" ):-
  string_lower( Lx0, Lx2 ),  str_code_remove( Lx2, 34,  Lx ),
  sub_str_between_plus_rest( Lx, "[", ",", Tag1, Rest1  ),
  sub_str_between_plus_rest( Rest1, "qhelp(", ")", Tag2, _Rest2 ), !.


try_read_ontology( _Lx, "cantf1",  "cantf2" ,  "cantf3" ):-  !.


not_exclude_metta_file( F ):- downcase_atom( F, F2 ), sub_atom( F2,_,_,_, 'data_subset' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'edges' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'aunt-kg' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'miner' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'nars' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'flybase' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'node' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'royal92' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'bigram' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'gpt2-like' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, 'bio-atom' ), !, fail .
not_exclude_metta_file( F ):- downcase_atom( F, F2 ),sub_atom( F2,_,_,_, '.gz' ), !, fail .
not_exclude_metta_file( _F ):-!.

max_file_size( F , 500000 ):- downcase_atom( F, F2 ), sub_atom( F2,_,_,_, 'stdlib' ), ! .
max_file_size( F , 10000 ):- downcase_atom( F, F2 ), sub_atom( F2,_,_,_, '.metta' ), ! .
max_file_size( _From , 500000):-!.

dont_accept_fail( Lx ):- sub_string( Lx, _,_,_, "fail" ), !, fail.
dont_accept_fail( _Lx ):-!.


eval_pl_tag_or_rs_tag( Tag_s ):-  eval_tag( 'pl_tag', _, Tag_s, _ ,_,_).
eval_pl_tag_or_rs_tag( Tag_s ):-  eval_tag( 'rust', _, Tag_s, _ ,_,_).



%search_o_y_and_assert( 1, Sea, Linum, Lx, _Zk, F, Level, Linum , is_leading, Lines_read_extra ):-
% get_type(Arg,Type):- eval_H(['get-type',Arg],Type).
%:- discontiguous eval_40/6.
%:- discontiguous eval_70/6.

% properties('&corelib','parse', [data_structures, qhelp("Parse a string to an expression."), parse ]).
%  between
% properties('&corelib','parse', [data_structures, qhelp("Parse a string to an expression."), parse ]).
 % between
  %sub_str_between_plus_rest( Lx, "[", ",", Tag1, Rest1  ),
  %sub_str_between_plus_rest( Rest1, "qhelp(", ")", Tag2, Rest2 ),
  %sub_str_between_plus_rest( Rest2, ",", "]", Tag3, _Rest3 ),
%  str_part_after_tag( Lx, "qhelp(", AfterStr2 ), str_part_before_tag( AfterStr2, ")" , Tag2 ),
%  str_part_after_tag( AfterStr2 , ",", AfterStr4 ),
%  str_part_before_tag( AfterStr4 , "]", Tag3 ),
%  search_o_y( Lx, "assertequaltoresult&" ),
% (@doc intersection

search_o_y_and_assert_succeed( Is_include, Sea, Linum, Code_lines_n, Lx, Zk, F, Level, Linum , Isleading, Result_lines_red ):-
 search_o_y_and_assert2( Is_include, Sea, Linum, Code_lines_n, Lx, Zk, F, Level, Linum , Isleading, Result_lines_red ), ! .
search_o_y_and_assert_succeed( _,_,_,_,_,_,_,_,_,_, 0 ):- !.

search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , is_not_leading, 0 ):-
  sub_atom( F, _,_,_, 'metta_eval'),  ( search_o_y( Lx, "eval_20&:-" ) ; search_o_y( Lx, "eval_40&:-" ) ; search_o_y( Lx, "eval_70&:-" ) ),
  dont_accept_fail( Lx ),  try_read_find_eval20_tag_in_string( Lx , Tag, Arities ),  is_metta_function( Tag ),
  write(" ASSERT PL TAG "), write( Tag ), nl,  assert_eval_tag( 'pl_tag', Linum, Tag, F, Lx , Arities ),
  asserta( found_search_result( 'pl_tag', Tag, Level, Linum, F, Lx ) ).

search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , is_not_leading, 0 ):-
  sub_atom( F, _,_,_, 'metta_eval'),  search_o_y( Lx, "eval_20&:-" ),
  try_read_find_eval20_tag_in_string( Lx , Tag, _Arities ),  eval_tag( _, _, Tag, _ , _,_),  !,
  assert_eval_tag_found( 'metta', Tag , "", "", "" ),  assert( found_search_result( 'metta', Tag, Level, Linum, F, Lx  ) ).

search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , is_not_leading,  0 ):-
  sub_atom( F, _,_,_, 'metta_interp'),  search_o_y( Lx, "eval_h(&:-" ),
  metta_interp_tag( Tag_from, Tag_til ),  sub_str_between( Lx, Tag_from, Tag_til, Subz ),
  assert_eval_tag( 'pl_interp_tag', Linum, Subz, F, Lx, 0 ),  !.

search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , _Isleading,  0 ):-
  sub_atom( F, _,_,_, 'metta_interp'),  search_o_y( Lx, "eval_h(&:-" ),  eval_tag( _, _, Tag_s, _ ,_,_),
  metta_interp_tag( Tag_from, Tag_til ),  string_concat( Tag_from, Tag_s, C1 ), string_concat( C1, Tag_til, C2 ),
  search_o_y( Lx, C2 ) , !,  assert_eval_tag_found( 'metta_interp_pl', Tag_s , "", "", "" ),
  assert( found_search_result( 'metta_interp_pl', Tag_s, Level, Linum, F, Lx  ) ).


search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , _Isleading, 0):-
  sub_atom( F, _, _, _, 'metta_ontology.pfc' ),  search_o_y( Lx, "properties(" ),  eval_tag( _, _, Tag_s, _ ,_,_),
  metta_ontology_tag( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ), string_concat( C1, Tag_til, C2 ),
  search_o_y( Lx, C2 ), !,  try_read_ontology( Lx, Tag1, Tag2, Tag3 ),
  write( "succeed-" ), write( Tag_s ), write("-"), write( Tag1 ), write( Tag2 ), write( Tag3 ), nl,
  assert_eval_tag_found( 'metta_ontol_pl', Tag_s , Tag1, Tag2, Tag3 ),
  assert( found_search_result( 'metta_ontol_pl', Tag_s, Level, Linum, F, Lx  ) ).


search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , metta_else_where , Lines_read_extra ):-
  not_exclude_metta_file( F ),  file_name_extension( _, Ext, F ), Ext == 'metta',
  eval_pl_tag_or_rs_tag( Tag_s ),   metta_file_tag_til_space( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ),   string_concat( C1, Tag_til, C4 ),
  search_o_y( Lx, C4 ), !,
  parenthesis_count( Lx, Count_is_open ), extra_lines_for_open_parenthesis( Count_is_open, Lines_read_extra ),
  read_extra_lines( 'metta' , Lines_read_extra, Sea , Xtra_lines ), string_concat( Lx, Xtra_lines , Lx2 ),
  assert_eval_tag_mult( 'metta_tag_extra', Linum, Tag_s , F, Lx2 ),
  % metta_example
  assert( found_search_result( 'metta_example', Tag_s, Level, Linum, F, Lx2  ) ).


search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , metta_std_lib , ResultCount ):-
  not_exclude_metta_file( F ),  file_name_extension( _, Ext, F ), Ext == 'metta',
  eval_pl_tag_or_rs_tag( Tag_s ),    metta_docfile_tag( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ),   string_concat( C1, Tag_til, C4 ),
  search_o_y( Lx, C4 ), !,  parenthesis_count( Lx, Count_is_open ),
  read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, 0, 8, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),
  assert_eval_tag_mult( 'metta_tag_std_lib', Linum, Tag_s , F, Lx2 ).

% metta_docfile_tag( "(@doc ", "" ):- !.

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , metta_std_lib , ResultCount ):-
  not_exclude_metta_file( F ),  file_name_extension( _, Ext, F ), Ext == 'metta',
  eval_pl_tag_or_rs_tag( Tag_s ),    metta_function_declaration( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ),   string_concat( C1, Tag_til, C4 ),
  search_o_y( Lx, C4 ), !,  parenthesis_count( Lx, Count_is_open ),
  read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, 0, 8, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),  assert_eval_tag_mult( 'metta_tag_std_lib', Linum, Tag_s , F, Lx2 ).

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum ,  is_not_leading , Lines_read_extra):-
  not_exclude_metta_file( F ),  file_name_extension( _, Ext, F ), Ext == 'metta',
  eval_pl_tag_or_rs_tag( Tag_s ),    metta_docfile_tag( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ), string_concat( C1, Tag_til, C4 ),   search_o_y( Lx, C4 ),!,
  parenthesis_count( Lx, Count_is_open ), extra_lines_for_open_parenthesis( Count_is_open, Lines_read_extra ),
  assert_eval_tag_mult( 'metta_doc', Linum, Tag_s , F, Lx ),
  read_extra_lines( 'metta' , Lines_read_extra, Sea , Xtra_lines ), string_concat( Lx, Xtra_lines , Lx2 ),
  assert( found_search_result( 'metta', Tag_s , Level, Linum, F, Lx2  ) ).

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , is_not_leading, ResultCount ):-
  not_exclude_metta_file( F ),   file_name_extension( _, Ext, F ), Ext == 'metta',
  eval_pl_tag_or_rs_tag( Tag_s ),    metta_file_tag( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ),   string_concat( C1, Tag_til, C4 ),   search_o_y( Lx, C4 ), !,
  parenthesis_count( Lx, Count_is_open ),
  % eval_tag(pl_tag,2089,"py-eval",'../canary/metta_eval.pl',"eval_70(_Eq,_RetType,_Depth,_Self,['py-eval',Arg],Res):- !,",1)
  % Type_tag, Linum, Tag, F, Sline
  assert_eval_tag_mult( 'metta_example', Linum, Tag_s , F, Lx ),
  read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, 0, 8, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),  assert( found_search_result( 'metta_example', Tag_s , Level, Linum, F, Lx2  ) ).



%search_o_y_and_assert( 1, Lx, _Zk, F, Level, Linum ):-
%  file_name_extension( _, Ext, F ), Ext == 'rs',
%  C1 = "assert_eq!(result",
%  sub_string( Lx, _, _, _ , C1 ), !,
%  string_concat( Lx, " ** <b>" , Cx ), string_concat( Cx, C1, Cp ), string_concat( Cp, " </b> " , C2 ),   !,
%  assert( found_search_result( Level, Linum, F, C2  ) ).


search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , Is_leading , 0):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'rs',
  eval_tag( _, _, Tag_s, _ , _ , _ ),
  rust_metta_tag( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ), string_concat( C1, Tag_til, C4 ),
  search_o_y( Lx, C4 ), !,
  assert_eval_tag_found( 'rust_metta', Tag_s , "", "", "" ),
  assert( found_search_result( 'rust_metta', Tag_s , Level, Linum, F, Lx  ) ).



search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , Is_leading, Lines_read_extra ):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'rs',
  eval_tag_secondary( Tag_org, Tag_s ),
  search_o_y( Lx, Tag_s ), !,
  assert_eval_tag_found( 'rust', Tag_org , "", "", "" ),  Lines_read_extra =  4,
  read_extra_lines( 'rust' , Lines_read_extra, Sea , Xtra_lines ), string_concat( Lx, Xtra_lines , Lx2 ),
  assert( found_search_result( 'rust', Tag_org , Level, Linum, F, Lx2  ) ).


search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , Is_leading , 0):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'rs',
  eval_tag( _, _, Tag_s, _ ,_,_),
  rust_detect_tag( Tag_from, Tag_til , _, _),
  string_concat( Tag_from, Tag_s, C1 ), string_concat( C1, Tag_til, C4 ),
  search_o_y( Lx, C4 ), !,
  assert_eval_tag_found( 'rust', Tag_s , "", "", "" ),
  assert( found_search_result( 'rust', Tag_s , Level, Linum, F, Lx  ) ).




% IS FREE TAG SEARCH
%search_o_y_and_assert( 1, _Sea, Linum, Lx, _Zk, F, _Level, Linum , is_not_leading, 0 ):-
%  not_exclude_metta_file( F ),  file_name_extension( _, Ext, F ), Ext == 'metta',
%  metta_file_tag_find( Tag_from, Tag_til ),  sub_str_between_plus_rest( Lx, Tag_from, Tag_til, Subz, Rest  ),
%  assert_eval_tag( 'metta_tag', Linum, Subz, F, Lx , 0 ),    sub_str_between_plus_rest( Rest, Tag_from, Tag_til, Subz2, Rest2  ),
%  assert_eval_tag( 'metta_tag', Linum, Subz2, F, Lx  , 0),   sub_str_between_plus_rest( Rest2, Tag_from, Tag_til, Subz3, _Rest3  ),
%  assert_eval_tag( 'metta_tag', Linum, Subz3, F, Lx  , 0 ),    !.
%search_o_y_and_assert( 1, _Sea, Linum, Lx, _Zk, F, _Level, Linum , is_not_leading, 0 ):-
%  not_exclude_metta_file( F ),  file_name_extension( _, Ext, F ), Ext == 'metta',
%  metta_file_tag_find( Tag_from, Tag_til ),  sub_str_between_plus_rest( Lx, Tag_from, Tag_til, Subz, Rest  ),
%  assert_eval_tag( 'metta_tag', Linum, Subz, F, Lx  , 0),    sub_str_between_plus_rest( Rest, Tag_from, Tag_til, Subz2, _Rest2  ),
%  assert_eval_tag( 'metta_tag', Linum, Subz2, F, Lx  , 0),   !.
%search_o_y_and_assert( 1, _Sea, Linum, Lx, _Zk, F, _Level, Linum , is_not_leading, 0 ):-  not_exclude_metta_file( F ),
%  file_name_extension( _, Ext, F ), Ext == 'metta',  metta_file_tag_find( Tag_from, Tag_til ),
%  sub_str_between_plus_rest( Lx, Tag_from, Tag_til, Subz, _Rest  ),  assert_eval_tag( 'metta_tag', Linum, Subz, F, Lx  , 0),   !.


%search_o_y_and_assert( 1, _Sea, Linum, Lx, _Zk, F, Level, Linum , is_leading ):-
%  file_name_extension( _, Ext, F ), Ext == 'rs',
%  rust_detect_tag( Tag_from, Tag_til , Second_tag_begin, Second_tag_end ),
%  debug_string( "TRY rust between ", Lx ),
%  sub_string( Lx, _, _, _, "er_tok"),
 % debug_string( "TRY rust in file  ", F ),
%  adhoc_parse_rust_regx( Lx, Tag_from, Tag_til, Subz ),
%  debug_string( "succeed rust between ", Lx ),
%  is_metta_function( Subz ),
 % not( eval_tag( _, Subz, _ ) ),  assert( eval_tag( Linum, Subz, F ) ),
 % asserta( found_search_result( Level, Linum, F, Lx, Subz  ) ), !.
% temp
%  sub_str_between( Lx, Second_tag_begin, Second_tag_end, Subz2 ),
%  trim_string( Subz2 ,  Subz3 ) ,
%  not( eval_tag_secondary( Subz, Subz3 ) ), !, assert( eval_tag_secondary( Subz, Subz3 ) ) .

% { assert_equal_to_result_op.clone() });


% python_metta_test_tag1( "assertEqualMettaRunnerResults(", "" ):- !.
% python_metta_test_tag2( "assertEqual(", "" ):- !.
% python_metta_test_tag3( "metta.run(", "" ):- !.

% not_exclude_metta_file( F ),

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , Is_leading , ResultCount ):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'py',
  python_metta_test_tag1( Tag_from, _Tag_til ),
  search_o_y( Lx, Tag_from ), !,  parenthesis_count( Lx, Count_is_open ),
  read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, 0, 25, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),  assert_eval_tag_mult( 'python_src_metta_test', Linum, Tag_from , F, Lx2 ).

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , Is_leading , ResultCount ):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'py',
  python_metta_test_tag2( Tag_from, _Tag_til ),
  search_o_y( Lx, Tag_from ), !,  parenthesis_count( Lx, Count_is_open ),
  read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, 0, 25, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),  assert_eval_tag_mult( 'python_src_metta_test', Linum, Tag_from , F, Lx2 ).

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , Is_leading , ResultCount ):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'py',
  python_metta_test_tag3( Tag_from, _Tag_til ),
  search_o_y( Lx, Tag_from ), !,  parenthesis_count( Lx, Count_is_open ),
  read_extra_lines_max_until_full_metta_claus( reject_zero, Count_is_open, 0, 25, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),  assert_eval_tag_mult( 'python_src_metta_test', Linum, Tag_from , F, Lx2 ).

search_o_y_and_assert2( 1, Sea, Linum, Code_lines_n, Lx, _Zk, F, _Level, Linum , Is_leading , ResultCount ):-
  ( Is_leading == 'is_not_leading' ;  Is_leading == 'is_leading' ),
  file_name_extension( _, Ext, F ), Ext == 'py',
  python_metta_test_tag4( Tag_from, _Tag_til ),
  search_o_y( Lx, Tag_from ), !,  parenthesis_count( Lx, Count_is_open ),
  read_extra_lines_max_until_full_metta_claus( not_reject_zero, Count_is_open, 0, 25, Sea , "", Xtra_lines, ResultCount ),
  string_concat( Lx, Xtra_lines , Lx2 ),  assert_eval_tag_mult( 'python_src_metta_test', Linum, Tag_from , F, Lx2 ).


search_o_y_and_assert2( 1, _Sea, Linum, Code_lines_n, Lx, _Zk, F, Level, Linum , is_not_leading, 0 ):-
  sub_atom( F, _, _, _, '.py' ),
  eval_tag( _, _, Tag_s, _ ,_,_),  string_length( Tag_s, Lex ), Lex > 3,
  python_detect_tag( Tag_from, Tag_til ),
  string_concat( Tag_from, Tag_s, C1 ),  string_concat( C1, Tag_til, C4 ),
  search_o_y( Lx, C4 ), !,
  assert_eval_tag_found( 'python', Tag_s , "", "", "" ),
  assert( found_search_result( 'python', Tag_s , Level, Linum, F, Lx  ) ).

% kopie
search_o_y_and_assert2( 1, _Sea, Lx, Linum, Code_lines_n, _Zk, F, Level, Linum , is_leading, 0 ):-
  sub_atom( F, _, _, _, '.py' ),
  python_detect_tag( Tag_from, Tag_til ),
  sub_str_between( Lx, Tag_from, Tag_til, Subz ),
  is_metta_function( Subz ),
  assert_eval_tag_found( 'python', Subz , "", "", "" ),
  assert_eval_tag( 'python', Linum, Subz, F, Lx  , 0),
%  not( eval_tag( _, _, Subz, _ ) ),  !, assert( eval_tag( Linum, Subz, F ) ),
  asserta( found_search_result( 'python', Subz , Level, Linum, F, Lx  ) ).



%  :- dynamic extra_string_to_search/1.

%if_extra_string_demand_search( Lx ):-
%  extra_string_to_search( Str ), !, search_o_y( Lx, Str ).
%if_extra_string_demand_search( _Lx ):- !.

%search_o_y_and_assert( 1, Lx, Zk, F, Level, Linum ):-
 % search_o_y( Lx, Zk ),!,
 % assert( found_search_result( Level, Linum, F, Lx ) ).

read_number_of_lines( Sea, Number_of_lines, Hs , Xtra_lines ):- Number_of_lines > 0,
    read_line_to_string( Sea, Lx ), Lx \= end_of_file, !,
    Number_of_lines2 is Number_of_lines - 1,
	string_concat( Hs , " \n<br>\n ", Hs2 ), string_concat( Hs2, Lx, Hs3 ),
  read_number_of_lines( Sea, Number_of_lines2, Hs3 , Xtra_lines ).
read_number_of_lines( _Sea, _Number_of_lines, Hs , Hs ):- !.


%--
% read_extra_lines_max_until_full_metta_claus( Count_is_open, 0, 8, Sea , Xtra_lines ),
read_extra_lines( _X , Number_of_lines, Sea , Xtra_lines ):- !,
  read_number_of_lines( Sea, Number_of_lines, "" , Xtra_lines ).
read_extra_lines( _ , _Number_of_lines, _Sea , " "  ):- !.


%---
trim_string2(Str, Sc):-  split_string(Str, "", " \n\t\r", [Sc]),!.
trim_string2(Sc, Sc):- !.

%---
check_is_empty_line_or_comment( F, Lx, 0 ):-
 sub_string( F, _ , _, _ , ".py" ),
 trim_string2( Lx , Sc ), sub_string( Sc, Sta , _, _ , "#" ), Sta < 2 ,!.
check_is_empty_line_or_comment( _F, _Lx, 1 ):- !.

%----

read_has_search_stream( Sea, Zk, F, Level, Linum, Code_lines_n, Is_include, Isleading):- not( at_end_of_stream( Sea ) ),
   read_line_to_string( Sea, Lx ), Lx \= end_of_file,
   % not_is_comment(Lx),
   % zet_comment_started(Lx), zet_comment_ended(Lx),   is_not_inside_comment(),
   % if_extra_string_demand_search( Lx ),
   check_is_empty_line_or_comment( F, Lx, Is_code_line ),
    Is_code_line == 1,
   search_o_y_and_assert_succeed( Is_include, Sea, Linum, Code_lines_n, Lx, Zk, F, Level, Linum , Isleading, Result_lines_red ),
   Code_lines_n2 is Code_lines_n + Is_code_line,
   !,
   Linum2 is Linum + Result_lines_red + 1,
 read_has_search_stream( Sea, Zk, F, Level, Linum2, Code_lines_n2, Is_include , Isleading).

read_has_search_stream( Sea, Zk, F, Level, Linum, Code_lines_n, Is_include, Isleading):-
  not( at_end_of_stream(Sea) ), ! ,
  Linum2 is Linum + 0, read_has_search_stream( Sea, Zk, F, Level, Linum2, Code_lines_n, Is_include, Isleading).
read_has_search_stream(_ , _, _, _, _, _, _, _):- !.

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



%-----
nondeterm_found_search_result( 'display_per_file', Filewithpath, TxLine1, Txtline, Filewithpath, Lnum ):-
  retract( found_search_result(  _, Txtline, _Level, Lnum, Filewithpath, TxLine1  ) ).


%nondeterm_found_search_result( 'display_per_tag', Filetag, TxLine1, Filetag, Filewithpath ):-
%  eval_tag_secondary( Filetag, Filetag2 ),
%  retract( found_search_result(  _Level, _Lnum, Filewithpath, TxLine1, Filetag2 ) ).

nondeterm_found_search_result( 'display_per_tag', Filetag, TxLine1, Filetag, Filewithpath , Lnum ):-
  retract( found_search_result(  _, Filetag, _Level, Lnum, Filewithpath, TxLine1  ) ).



%---
show_file_also( 'display_per_tag', Filewithpath , Lnum ):- !,
  write( " <hr> \n" ),
  write( " &nbsp &nbsp " ),  write( Filewithpath ), write( " - line: " ), write( Lnum ), write( " <br> \n" ), !.
show_file_also( _ , _Filewithpath ,_ ):- !.

%----

doesnt_have_metta_tag( Tag ):-   eval_tag( 'metta_tag' , _Linum, Tag, _F, _ ,_), !, fail.
doesnt_have_metta_tag( _Tag ):- !.


try_find_types( Tag_s , Tag1, Tag2, Tag3 ):-
  eval_tag_found( 'metta_ontol_pl' , Tag_s , Tag1, Tag2, Tag3 , _), !.

try_find_types( _Tag , "" , "" , "" ):- !.


eval_tag_or_similar_category( Ontol_1, Linum, Tag, Fn, Lx , From_similar , is_self ):-
    eval_tag( 'metta_tag_std_lib' , Linum, Tag, Fn, Lx, _ ) ,  From_similar = Ontol_1.

eval_tag_or_similar_category( Ontol_1, Linum, Tag, Fn, Lx , From_similar , is_self ):-
    eval_tag( 'metta_tag_extra' , Linum, Tag, Fn, Lx, _ ) ,  From_similar = Ontol_1.

% this one exists too
  % assert_eval_tag_mult( 'metta_example', Linum, Tag_s , F, Lx2 ),
  % metta_example

eval_tag_or_similar_category( Ontol_1, Linum, Tag, Fn, Lx , Tag_other , similar_to ):-
    eval_tag_found( 'metta_ontol_pl' , Tag , Ontol_1, _, _ ,_),
	eval_tag_found( 'metta_ontol_pl' , Tag_other , Ontol_1, _, _ ,_),
	Tag_other \= Tag,
    eval_tag( 'metta_tag_std_lib' , Linum, Tag_other, Fn, Lx,_ ).

eval_tag_or_similar_category( Ontol_1, Linum, Tag, Fn, Lx , Tag_other , similar_to ):-
    eval_tag_found( 'metta_ontol_pl' , Tag , Ontol_1, _, _ ,_),
	eval_tag_found( 'metta_ontol_pl' , Tag_other , Ontol_1, _, _ ,_),
	Tag_other \= Tag,
    eval_tag( 'metta_tag_extra' , Linum, Tag_other, Fn, Lx,_ ).

eval_tag_or_similar_category( Ontol_1, Linum, Tag, Fn, Lx , Tag_other , similar_to ):-
    eval_tag_found( 'metta_ontol_pl' , Tag , Ontol_1, _, _ ,_),
	eval_tag_found( 'metta_ontol_pl' , Tag_other , Ontol_1, _, _ ,_),
	Tag_other \= Tag,
    eval_tag( 'metta_tag' , Linum, Tag_other, Fn, Lx,_ ).
	%string_concat( " similar to :  ", Tag_other , C4 )  .





	%string_concat( "<b> similar to: </b>", Tag_other , C4 )  .
%---

enlarge_tag( similar_to, _Tag, From_similar, Lx, Lx2 ):- !,
   string_concat( "<div class=\"enlarged\"> " , From_similar, C1 ), string_concat( C1 , "</div>", C2 ),
   str_replace_tag( Lx , From_similar, C2, Lx2).

enlarge_tag( _, Tag, _From_similar, Lx, Lx2 ):- !,
   string_concat( "<div class=\"enlarged\"> " , Tag, C1 ), string_concat( C1 , "</div>", C2 ),
   str_replace_tag( Lx , Tag, C2, Lx2).


%---
write_similar_to( similar_to, Tag, From_similar ):- !,
  write_big_tag( Tag ), write( " is similar to: "), write_big_tag( From_similar ).
%is_self
write_similar_to( _, Tag, _From_similar ):-  write( " function: "), write_big_tag( Tag ), !.




%---
write_metta_args( 1 ):- !, write( "aq" ).
write_metta_args( 2 ):- !, write( "aq au" ).
write_metta_args( 3 ):- !, write( "aq au as" ).
write_metta_args( 4 ):- !, write( "aq au as at" ).
write_metta_args( _ ):- !.

write_metta_list_args( 1 ):- !, write( "(superpose (a b c))" ).
write_metta_list_args( 2 ):- !, write( "(superpose (a b c)) (superpose (c d e))" ).
write_metta_list_args( 3 ):- !, write( "(superpose (a b c)) (superpose (c d e)) (superpose (e f g))" ).
write_metta_list_args( 4 ):- !, write( "(superpose (a b c)) (superpose (c d e)) (superpose (e f g)) (superpose (g h i))" ).
write_metta_list_args( _ ):- !.




%--
make_test_arity( 'type1' , N, Tag  ):-
 write( "!(" ), write( Tag ), write( " " ), write_metta_args( N ),  write( " )" ), write( " <br> \n" ),
 write( "!(assertEqual (" ), write( Tag ), write( " " ), write_metta_args( N ),  write( " )  ( result_with_type )) " ), write( " <br> \n" ),
 write( "!(assertEqualToResult (" ), write( Tag ), write( " " ), write_metta_args( N ),  write( " )  ( result_with_type )) " ), write( " <br> \n" ),
 %  !(assertEqual (call! aq ) ( result_with_type ))
 % !(assertEqualToResult (parse "(foo 1 2 3)" ) ((foo 1 2 3)) )
 !.

make_test_arity( 'type2' , N, Tag  ):-
 write( "eval( \"!(" ), write( Tag ), write( " " ), write_metta_args( N ),  write( " )\")." ), write( " <br>  \n" ),
 write( "eval( \""), write( "!(assertEqual (" ), write( Tag ), write( " " ), write_metta_args( N ),  write( " )  ( result_with_type ))" ), write( "\" ). <br> \n" ),
 write( "eval( \""), write( "!(assertEqualToResult (" ), write( Tag ), write( " " ), write_metta_args( N ),  write( " )  ( result_with_type ))" ), write( "\" ).  <br>  \n" ),
 %  !(assertEqual (call! aq ) ( result_with_type ))
 % !(assertEqualToResult (parse "(foo 1 2 3)" ) ((foo 1 2 3)) )
 !.

make_test_arity( 'type3' , N, Tag  ):-
 write( "eval( \"!(" ), write( Tag ), write( " " ), write_metta_list_args( N ),  write( " )\")." ), write( " <br>  \n" ),
 write( "eval( \""), write( "!(assertEqual (" ), write( Tag ), write( " " ), write_metta_list_args( N ),  write( " )  ( superpose (e f g) ))" ), write( "\" ). <br> \n" ),
 write( "eval( \""), write( "!(assertEqualToResult (" ), write( Tag ), write( " " ), write_metta_list_args( N ),  write( " )  ( superpose (e f g) ))" ), write( "\" ).  <br>  \n" ),
 %  !(assertEqual (call! aq ) ( result_with_type ))
 % !(assertEqualToResult (parse "(foo 1 2 3)" ) ((foo 1 2 3)) )
 !.


make_test_arity( _, _, _  ):-!.
%----
%(@doc =
%  (@desc "A symbol used to define reduction rules for expressions.")
%  (@params (
%    (@param "Pattern to be matched against expression to be reduced")
%    (@param "Result of reduction or transformation of the first pattern")))
%  (@return "Not reduced itself unless custom equalities over equalities are added") )


%---
write_doc_args( 1 ):- !,
    write( "<br> (@params (   (@param \"\") ) )").


write_doc_args( 2 ):- !,
    write( "<br> (@params (   (@param \"\") "),
    write( " (@param \"\")))" ).
write_doc_args( _ ):- !.
%----

%(@doc cons-atom
%  (@desc "Constructs an expression using two arguments")
%  (@params (
%    (@param "Head of an expression")
%    (@param "Tail of an expression")))
%  )
% (: cons-atom (-> Atom Expression Expression))


make_doc_arity( 'type1' , Arit, Tag  ):-
  write("(@doc "), write( Tag ),  write( " <br> " ),
  write("(@desc \""),  write( "\" )"),
  write_doc_args( Arit ),
%  write( "<br> ) <br>" ),
  write( "<br> " ),
  write("(@return \""), write( "\") ) "),
!.


make_doc_arity( _, _, _  ):- ! .

%----
make_tests_and_doc( Tag, Arit ):-
   write( "\n<div id=\"\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #FFFFD9; color: #3A3A3A \" >  " ),
   make_test_arity( 'type1' , Arit , Tag  ),
   make_test_arity( 'type2' , Arit , Tag  ),
   make_test_arity( 'type3' , Arit , Tag  ),
   write( "\n <hr> \n " ),
   make_doc_arity( 'type1' , Arit, Tag ),
   fail , ! .
make_tests_and_doc( _Tag, _Arit ):-   write( "\n  </div>\n" ), !.

%---

% try_find_types( Tag , Tag1, Tag2, Tag3 ),
% eval_tag( 'pl_tag' , _Linum, Tag2, _F ),

try_find_metta_elsewhere( Tag, Ontol_1  ):-    retractall( tel(_)) , assert( tel( 0 ) ),

  write( "\n<div id=\"\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #676767; color: #E7E7E7 \" >  " ),
  % eval_tag( 'metta_tag_extra' , Linum, Tag, Fn, Lx ),
  eval_tag_or_similar_category( Ontol_1, Linum, Tag, Fn, Lx , From_similar , Is_similar ),
  incr( N2 ),  N2 < 10 ,
  number_string( N2, S2 ),
  write( "\n <br> "),  write( S2 ),  write( " " ), write_similar_to( Is_similar, Tag, From_similar  ),
  write( " Metta file: " ), write( Fn ), write( " " ),   write( Linum ), write( " <br> \n" ),
  enlarge_tag( Is_similar, Tag, From_similar, Lx, Lx2 ),
   % str_replace_tag(At, Repwhat, RepWith, Resu)
  write_metta_clause( Lx2 ),
  fail,  !.
try_find_metta_elsewhere( _Tag , _ ):-   write( "\n  </div>\n" ), !.




show_tags_which_dont_have_tests():-   retractall( tel_item(_)), assert( tel_item( 0 )),
  write( "TAGS ONTOLOGY <br> \n" ),
  eval_tag_found( 'metta_ontol_pl', Tag , Tag1, Tag2, Tag3 ,_),
  incr_item( N2 ), number_string( N2, S2 ),
  write( S2 ), write( " " ), write( Tag ),  write( " " ), write( Tag1 ), write( " " ), write( Tag2 ),
  write( " " ), write( Tag3 ),
  write( "<br>\n" ), fail, !.



show_tags_which_dont_have_tests():-   retractall( tel_item(_)), assert( tel_item( 0 )),
  write( "<b> THESE  TAGS  have Tests  here </b> <hr> \n" ),

  findall( Tag2, eval_tag( 'pl_tag' , _Linum, Tag2, _F, _ , _ ), Taglis ), sort( Taglis, Taglis2 ),
  % write_term( Taglis2, [] ), nl,
  member( Tag, Taglis2 ),
  % doesnt_have_metta_tag( Tag ),
  eval_tag( 'metta_tag' , Linum, Tag, F, Lx, Arit ), number_string( Arit, Arit_s  ),

  incr_item( N2 ), number_string( N2, S2 ),
  write( S2 ), write( " " ), write( Tag ), write( " " ), write( Arit_s ),  write( " " ),
   write( F ),  write( " line : " ), write( Linum ), write( "<br>\n" ),
  write( Lx ), write( "<br>\n" ),

  try_find_types( Tag , Ontol1, Ontol2, Ontol3 ),
  write( Ontol1 ), write( " " ), write( Ontol2 ),   write( " " ), write( Ontol3 ),
  fail, !.


show_tags_which_dont_have_tests():-   retractall( tel_item(_)), assert( tel_item( 0 )),
%  write( " <br> <h2> <b> TAGS Which dont have Tests in sanity directory </b> </h2> <hr> \n" ),
  write( " <br> <h2> <b> TAGS identified in current mettalog prolog-implementation interpreter</b> </h2> <hr> \n" ),
  findall( Tag2, eval_tag( 'pl_tag' , _Linum, Tag2, _F, _ , _), Taglis ),

  sort( Taglis, Taglis2 ),
  % write_term( Taglis2, [] ), nl,
  member( Tag, Taglis2 ),
  eval_tag( 'pl_tag' , _, Tag, _, Lx , Arit ), number_string( Arit, Arit_s  ),
  % doesnt_have_metta_tag( Tag ),
  write( "<br>\n" ),
  incr_item( N2 ), number_string( N2, S2 ),
  write( S2 ), write( " " ), write( Tag ),  write( " <b> Arity: </b>" ),  write( Arit_s ),  write( " " ),
  write( " <br> \n " ) , write( Lx ),  write( " <br> \n " ),
  make_tests_and_doc( Tag, Arit ),
  try_find_types( Tag , Ontol1, Ontol2, Ontol3 ),
  % eval_tag_found( 'metta_ontol_pl', Tag_s , Tag1, Tag2, Tag3 )
  write( Ontol1 ), write( " " ), write( Ontol2 ),
  write( " " ), write( Ontol3 ),
  try_find_metta_elsewhere( Tag , Ontol1 ),
  % , Fn, Linum , Line_s
  % write( "\n <br> Metta elsewhere: " ), write( Fn ), write( " " ), write( Linum ), write( " " ), write( Line_s ),

  fail, !.


show_tags_which_dont_have_tests():- !.



%---
:- dynamic tel_item/1.

incr_item( N2 ):- retract( tel_item( N ) ),!, N2 is N + 1,  assert( tel_item( N2 ) ).

:- dynamic tel/1.

incr( N2 ):- retract( tel( N ) ),!, N2 is N + 1,  assert( tel( N2 ) ).

:- dynamic tel2/1.
init_tel2():- retractall( tel2( _ ) ),!,  assert( tel2( 0 ) ).
incr2( N2 ):- retract( tel2( N ) ),!, N2 is N + 1,  assert( tel2( N2 ) ).


show_identified_eval_secondary_tags():-
 write( "<h2> Secondary Tags identified </h2> <br>" ), nl,
 write( "\n<div id=\"\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #676767; color: #E7E7E7 \" >  " ),
 eval_tag_secondary( _, Tag ),
 write( " -" ),  write( Tag ), write( "-  " ), write( " <br> \n" ), fail,!.
show_identified_eval_secondary_tags():-  write( "\n  </div>\n" ), !.
%---

show_identified_eval_tags( Type ):-   retractall( tel_item(_)), assert( tel_item( 0 )),
 findall( Tagx, eval_tag( Type , _, Tagx, _ ,_ , _), Tagx_list ), sort( Tagx_list, Tagx_list2 ),
 write( "<h2> Tags identified </h2> "), write( Type ),
 write(" <br>" ), nl,
 write( "\n<div id=\"\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #676767; color: #E7E7E7 \" >  " ),
 member( El, Tagx_list2 ),
 eval_tag( Type , Linum, El, F ,_ , _ ),
 incr_item( N2 ), number_string( N2, S2 ),
 write( " " ), write( S2 ),  write( "  " ), write( El ), write( "  " ), write( Linum ), write( " " ),
 write( F ),
 write( " <br> \n" ), fail,!.

show_identified_eval_tags( _ ):-  write( "\n  </div>\n" ), !.

%----
fresults_par_file( Display_per_file_or_per_tag, Lp2, Lp, Fnx,  Filepath , _Zk ):-

 write_copy_link( Filepath, Fnx ),
 write( "\n<br> <a onclick=\"open_div('"), write(Lp2), write(Lp),  write(Fnx), write("')\"  style=\"cursor:pointer\">  Open </a>" ), nl,
 write( "\n <a onclick=\"close_div('"), write(Lp2), write(Lp),  write(Fnx), write("')\" style=\"cursor:pointer\">  Close </a>" ), nl,
 write( "\n<div id=\""), write(Lp2), write(Lp),  write(Fnx),
 write("\" style=\"border: 1px solid black; border-radius : 9px; padding: 5px; margin-left: 40px; max-width: 90% ; background-color: #676767; color: #E7E7E7 \" >  " ),
 nl,
 nondeterm_found_search_result( Display_per_file_or_per_tag, Filepath, TxLine1, Txtline, Filewithpath , Lnum ),
 file_name_extension( _, Ext, Filewithpath ),
 color_for_file_type( Filewithpath, Htm_color_string ),
 write( " <div style=\"color: " ), write( Htm_color_string ), write( "\"> " ),

 show_file_also( Display_per_file_or_per_tag, Filewithpath , Lnum ),
 make_used_tag_bold( TxLine1, Txtline , TxLine2 ),
  %concat( Zk, "", Zk2 ), concat( "<b>", Zk2, C1 ), concat( C1, "</b>", C2 ),
  %str_replace_tag( Head2, Zk2, C2, Resu ),
  % write_line_with_tag_colors( '', " ajjahsgdfgfgf ffhhfh fhfhfhh ffhhf", "ffhh" ).
  % % string_length( Zk, Lx ), Lx > 0,
  % write( "<div style=\"color:black; display: inline-block\">" ) ,

 write_line_with_tag_colors( Ext, TxLine2 ),
 % , Txtline

 % write( "<b> <i>" ),  write( Txtline ), write( " </i> </b>" ), write( " <br> \n" ),
   % write( " " ),  write( Txtline ), write( "  " ),
   % write( " <br> \n" ),
 % string_length( Txtline, Lex ),
 % if_small_no_newline( Lex ) ,
 write( " </div>" ),
 fail, ! .

fresults_par_file( _, _, _, _Fnx, _ , _):- !, write( "\n  </div>\n" ).




:- dynamic per_file/1.

file_characteristics( File_with_path , Fnx , Lp, Lpx2 , Dir ):-
  sub_atom( File_with_path, _, _, _, '/' ),
  file_base_name( File_with_path , Fnx ),
  dir_get_last_path( File_with_path , Lp ),  dir_get_last_path2( File_with_path , Lpx2 ),
  file_directory_name( File_with_path , Dir ), !.

file_characteristics( _File_with_path , '','','','' ):- !.


display_metta_sresults( 'display_per_file', _Zk, _Dirx ):-  retractall( per_file( _ ) ) ,
  found_search_result( _,_,_, _, Fpz, _ ),  not( per_file( Fpz ) ),  assert( per_file( Fpz ) ), fail.
display_metta_sresults( 'display_per_file',  Zk, _Dirx ):-
   findall( Fpz,  per_file(  Fpz ), Fpzl ),      member( File_with_path, Fpzl ),
   file_characteristics( File_with_path , Fnx , Lp, Lpx2 , Dir ),
   write( "\n<br> &nbsp <br> \n" ), write( "\n<br> &nbsp <br> \n" ),
   nl,nl,nl, write( "***" ), write( " <i><b> " ), write( Fnx ), write( "</i></b> <b> &nbsp &nbsp &nbsp " ),
   write( Lp ), write( "</b> &nbsp &nbsp &nbsp " ), write( Lpx2 ), nl,   write( Dir ), nl,
   fresults_par_file( 'display_per_file', Lpx2, Lp, Fnx, File_with_path, Zk ),
   fail, !.

display_metta_sresults( 'display_per_tag', _Zk, _Dirx ):-  retractall( per_file( _ ) ) ,
  found_search_result( _,Tag, _, _, _Fpz, _  ),  not( per_file( Tag ) ),  assert( per_file( Tag ) ), fail.
display_metta_sresults( 'display_per_tag',  Zk, _Dirx ):-
   findall( Fpz,  per_file(  Fpz ), Tags0 ), sort( Tags0, Tags ),     member( Tag, Tags ),
   write( "\n<br> &nbsp <br> \n" ), write( "\n<br> &nbsp <br> \n" ),
   nl,nl,nl, write( "***" ), write( " <i><b> " ), write( Tag ), write( "</i></b>  " ),
   fresults_par_file( 'display_per_tag', '', '', '', Tag, Zk ),   fail, !.

% display_metta_sresults( _, _, _ ):-
%   show_identified_eval_tags(),
%   show_identified_eval_secondary_tags(), fail, !.


% display_metta_sresults( _, _ ):-  write("</pre>"), !.
display_metta_sresults( _, _, _ ):-  write(" "), !.

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








numspace(Tel):- Tel > 0, !, write("&nbsp"), Tel2 is Tel - 1, numspace(Tel2).
numspace(_):- !.

 %concat( Zk, "", Zk2 ), concat( "<b>", Zk2, C1 ), concat( C1, "</b>", C2 ),
 %str_replace_tag( Head2, Zk2, C2, Resu ),
 % write_line_with_tag_colors( '', " ajjahsgdfgfgf ffhhfh fhfhfhh ffhhf", "ffhh" ).
% % string_length( Zk, Lx ), Lx > 0,
% write( "<div style=\"color:black; display: inline-block\">" ) ,

write_codes_metta_clause_htm( [] , _Parenthesis_level ):- !.

write_codes_metta_clause_htm( [ 45, 62 | Codes ] , Parenthesis_level ):-    !,
  write( "<div class=\"tag_arrow"),  write( "\">" ) ,   write( "->" ),  write( "</div>" ) ,
  write_codes_metta_clause_htm( Codes  , Parenthesis_level ).

%write_codes_metta_clause_htm( [ 61 | Codes ] , Parenthesis_level ):-    !,
%  write( "<div class=\"tag_equal\">"),    write( "=" ),  write( "</div>" ) ,
%  write_codes_metta_clause_htm( Codes  , Parenthesis_level ).

%write_codes_metta_clause_htm( [ 58 | Codes ] , Parenthesis_level ):-    !,
%  write( "<div class=\"tag_assignment\">"),   write( ":" ),  write( "</div>" ) ,
%  write_codes_metta_clause_htm( Codes  , Parenthesis_level ).

write_codes_metta_clause_htm( [ Co , Co2 | Codes ] , Parenthesis_level ):-   Co == 40, Parenthesis_level == 0 ,
  ( Co2 == 61; Co2 == 58 ),  !,
  write( "<div class=\"tag_parenthesis0\">" ) ,    write( "(" ),
  write( "</div>" ) ,
  string_codes( Zx,  [ Co2 ] ),
  write( "<div class=\"tag_operator\">" ) ,    write( Zx ),
  write( "</div>" ) ,

  Parenthesis_level2 is Parenthesis_level + 1,
  write_codes_metta_clause_htm( Codes  , Parenthesis_level2 ).


write_codes_metta_clause_htm( [ Co | Codes ] , Parenthesis_level ):-   Co == 40, !,
  write( "<div class=\"tag_parenthesis"), write( Parenthesis_level ), write( "\">" ) , numspace( Parenthesis_level ),   write( "(" ),
  write( "</div>" ) ,  Parenthesis_level2 is Parenthesis_level + 1,
  write_codes_metta_clause_htm( Codes  , Parenthesis_level2 ).

write_codes_metta_clause_htm( [ Co | Codes ] , Parenthesis_level ):-   Co == 41, !,
  Parenthesis_level2 is Parenthesis_level - 1,
  write( "<div class=\"tag_parenthesis"), write( Parenthesis_level2 ), write( "\">" ) , write( ") &nbsp " ),  write( "</div>" ) ,
  write_codes_metta_clause_htm( Codes  , Parenthesis_level2 ).
write_codes_metta_clause_htm( [ Co | Codes ] , Parenthesis_level ):-   Co == 10, !,
  write( "<br>"),   write_codes_metta_clause_htm( Codes  , Parenthesis_level ).

write_codes_metta_clause_htm( [ H | Codes ], Plevel ):-  !,  string_codes( Zx,  [ H ] ), write( Zx ),  write_codes_metta_clause_htm( Codes , Plevel ).


%----
write_codes_with_tag_colors( _Ext, [] ):-!.
write_codes_with_tag_colors( Ext, [ H | Codes ] ):-  H == 40, !,  string_codes( Zx,  [ H ] ),
  write( "<div class=\"tag_"), write( Ext ), write(" leftpar_"), write( Ext ), write( "\">" ) , write( Zx ),
  write( "</div>" ) ,   write_codes_with_tag_colors( Ext,  Codes  ).

write_codes_with_tag_colors( Ext, [ H | Codes ] ):-  H == 41, !,  string_codes( Zx,  [ H ] ),
  write( "<div class=\"tag_"), write( Ext ), write(" rightpar_"), write( Ext ), write( "\">" ) , write( Zx ),
  write( "</div>" ) ,   write_codes_with_tag_colors( Ext,  Codes  ).

write_codes_with_tag_colors( Ext, [ H | Codes ] ):-  !,  string_codes( Zx,  [ H ] ), write( Zx ),
  write_codes_with_tag_colors( Ext,  Codes  ).


%  sub_string( Txt, Pos, Len, Aft, "(" ) ,
%  sub_string( Txt, 0, Pos, _, Begin ), Y is Pos + Len,
%  sub_string( Txt, Y, Aft, 0, Rest_string ),  !,
%  write( Begin ) ,
  % write( "<div style=\"color:black; display: inline-block\">" ) ,  write( "(" ),  write( "</div>" ) ,
  % write( "<div style=\"color:black; display: inline-block\">" ) ,  write( "(" ),  write( "</div>" ) ,
%  write_line_with_tag_colors( Ext, Rest_string ).

make_used_tag_bold( TxLine1, Zk , Cat2 ):-
  string_lower( TxLine1, Txlow ) ,   string_lower( Zk, Zk_low ) ,
  sub_string( Txlow , Sta, Le, Aft , Zk_low ),
  concat( Zk, "", Zk2 ), concat( "<div class=\"tag_searched\">" , Zk2, C1 ), concat( C1, "</div>" , C2 ),
  sub_string( TxLine1 , 0, Sta, _ , Begin ),
  Y is Sta + Le,
  sub_string( TxLine1 , Y, Aft, _ , Rest ),
  string_concat( Begin, C2, Cat1 ),
  string_concat( Cat1, Rest, Cat2 ), !.

  % str_replace_tag( TxLine1 , Zk, C2, Txtline2 ), !.

make_used_tag_bold( Tx, _Zk , Tx ):-  !.

  % write_line_with_tag_colors( '', " ajjahsgdfgfgf ffhhfh fhfhfhh ffhhf", "ffhh" ).
  % % string_length( Zk, Lx ), Lx > 0,
  % write( "<div style=\"color:black; display: inline-block\">" ) ,
write_line_with_tag_colors( Ext, Txt ):-  !,
  string_codes( Txt, Codes ),
  write_codes_with_tag_colors( Ext, Codes ).

% write_line_with_tag_colors( _Ext, Txt ):- write( Txt ).

%----

search_prolog_files( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi , Isleading ):-
  read_files_dir( 'prolog_source_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "eval_20&:-" ) ),

  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.pl' ) ),
  retractall( singularity_src_dir_perform( _ ) ),

  %  assert( singularity_src_dir_perform( '../canary/' ) ),
  assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_00 ),
  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_00 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),
  retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  retractall( found_include_file( _, _, _, _, _, _, _, _, _, _, _ )),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, 'dummy_001', 0 , Isleading ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, 'dummy_001', 1, Isleading ).
search_prolog_files( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi , _Isleading ):- !.
%--
       % assert( singularity_src_dir_perform( '../../tests/baseline_compat/' ) ),
        %  assert( singularity_src_dir_perform( '../../tests/' ) ),

search_metta_files( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  read_files_dir( 'search_metta_files_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),
       assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_2 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_2 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 0 , Isleading),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 1 , Isleading).
search_metta_files( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.

search_metta_files_else_where( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, _Isleading ):-
  read_files_dir( 'search_metta_files_else_where_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),
    assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_2 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_2 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 0 , metta_else_where ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 1 , metta_else_where ).
search_metta_files_else_where( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.



search_metta_files_std_lib( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, _Isleading ):-
  read_files_dir( 'metta_std_lib_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),
  % canary
%  assert( singularity_src_dir_perform( '../../tests/extended_compat/metta-examples/' ) ),

  % assert( singularity_src_dir_perform( '../canary/' ) ),
  assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_2 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_2 ),   sleep(2),
  write(" start read files search_metta_files_std_lib \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 0 , metta_std_lib ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 1 , metta_std_lib ).
search_metta_files_std_lib( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.




  % assert( singularity_src_dir_perform( '../../tests/extended_compat/' ) ),
  % assert( singularity_src_dir_perform( '../../tests/features/' ) ),
  % \\wsl.localhost\Ubuntu\home\drspro\metta-wam\tests\extended_compat\metta-examples
  % assert( singularity_src_dir_perform( '../../tests/baseline_compat/hyperon-mettalog_sanity/' ) ),
  % assert( singularity_src_dir_perform( '../../tests/baseline_compat/' ) ),


search_metta_files2( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  read_files_dir( 'examples_source_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),

  % assert( singularity_src_dir_perform( '../../../metta-examples-main/' ) ),
  assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_2 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_2 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 0 , Isleading),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_2, 1 , Isleading).
search_metta_files2( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.




search_metta_files_hyperon( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  read_files_dir( 'metta_python_source_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.metta' ) ),
  retractall( singularity_src_dir_perform( _ ) ),

  % assert( singularity_src_dir_perform( '../../../hyperon-experimental/python/' ) ),
     assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_3 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_3 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_3, 0 , Isleading),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_3, 1 , Isleading).
search_metta_files_hyperon( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.


search_python_files_hyperon( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  read_files_dir( 'python_source_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.py' ) ),
  retractall( singularity_src_dir_perform( _ ) ),

%	   assert( singularity_src_dir_perform( '../../../hyperon-experimental/python/' ) ),
       assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_4 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_4 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_4, 0 , Isleading),
  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_4, 1 , Isleading).
search_python_files_hyperon( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.

search_rust_files_hyperon( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, Isleading ):-
  read_files_dir( 'hyperon_source_dir',  Sdir ),  !,
  retractall( chercher_a( _ ) ), assert( chercher_a( "assertEqual" ) ),
  retractall( current_file_extension( _ ) ), assert( current_file_extension( '.rs' ) ),
  retractall( singularity_src_dir_perform( _ ) ),
	 % assert( singularity_src_dir_perform( '../../../hyperon-experimental/lib/src/' ) ),
	 assert( singularity_src_dir_perform( Sdir ) ),
  singularity_src_dir_perform( Dirx_zz_5 ),  get_all_singularity_files( Tp, MdfKeyaft, Dirx_zz_5 ),   sleep(2),
  write(" start read files \n"),  write("\n<br> &nbsp <br> \n"),
  retractall( dir_level( _, _, _, _, _, _, _, _, _, _) ),   retractall( file_level(_, _, _, _, _, _, _, _, _, _, _) ),
  consult( Xfi ),
  write( "Consulted\n" ), write( Xfi ), nl,  write( "\n<br> &nbsp <br> \n" ),
%  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 0 , Isleading),
%  read_all_singularity_files( is_octet, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 1 , Isleading).
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 0 , Isleading),
  read_all_singularity_files( is_utf8, Tp, Zk, only_copy_files, Is_mk_update, Dirx_zz_5, 1 , Isleading).
search_rust_files_hyperon( _Tp, _Dk, _Zk, _MdfKeyaft, _Is_mk_update, _Xfi, _Isleading ):- !.


%---
short_code_metta( include_metta_files, 'inc' ):-!.
short_code_metta( _Inc_Metta, 'notinc' ):-!.

%----
% CENTRAL ALL DIRS
 read_files_dir( 'hyperon_source_dir', '../../../hyperon-experimental/lib/src/' ).

 read_files_dir( 'prolog_source_dir', '../canary/' ).
 read_files_dir( 'metta_std_lib_dir', '../canary/' ).

% the se 2 should not be the same or contain eachother

 read_files_dir( 'search_metta_files_dir', '../../tests/' ).


 read_files_dir( 'search_metta_files_else_where_dir', '../../tests/extended_compat/metta-examples/' ).
 read_files_dir( 'examples_source_dir', '../../../metta-examples-main/' ).

 read_files_dir( 'python_source_dir', '../../../hyperon-experimental/python/' ).
 read_files_dir( 'metta_python_source_dir', '../../../hyperon-experimental/python/' ).


%---

also_search_metta_files( include_metta_files , Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ):- !,

  % TEMP
  write( "Phase- metta STDLIB files" ), nl,
  search_metta_files_std_lib( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),

% If these directorys are equal then only one of them should be performed
  search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_metta_files_else_where( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),

  % TEMP
  search_metta_files2( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),


  !.
%  search_metta_files_else_where( Tp, _Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, _Isleading ),

% TEMP
%  search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ), !.


also_search_metta_files( _, _,_,_, _,_,_ ):- !.

%--
% ( i
search_all_files_with_leading( 'prolog_leading', Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi , Inc_Metta ):- !,
  search_prolog_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_leading ),
  search_python_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_rust_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  also_search_metta_files( Inc_Metta, Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ),
  %search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  %search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),


  !.


search_all_files_with_leading( 'python_leading', Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi , Inc_Metta ):- !,
  search_python_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_leading ),
  search_prolog_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  search_rust_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  also_search_metta_files( Inc_Metta, Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ),
  %search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  %search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
  !.

search_all_files_with_leading( 'rust_leading' , Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi , Inc_Metta ):- !,

  write("Phase- python"), nl,
  search_python_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_leading ),


% TEMP
%  write("Phase-1"), nl,
%  write("Phase-RUST"), nl,
%  read_rustfile_tags( '../../../hyperon-experimental/lib/src/metta/runner/stdlib.rs' ),
%  search_rust_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),

%  write("Phase-PROLOG"), nl,
%  search_prolog_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),

%  write("Phase-metta files"), nl,
%  also_search_metta_files( Inc_Metta, Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ),


  !.


%  search_metta_files( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),
%  search_metta_files_hyperon( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi, is_not_leading ),


reset_htm( _Display_per_file_or_per_tag, _LeadingTag, _Inc_Metta, Htmfile ):-
  tell( Htmfile ),
  % get_time( TimeStamp ),
  today_key( Dk ), write( " " ), write( Dk ),
  write("\n<html> <head> \n"),
  write("\n<script> \n"),
  write("function open_div(xid){ document.getElementById(xid).style.display = 'block'; }\n"),
  write("function close_div(xid){ document.getElementById(xid).style.display = 'none'; }\n"),
  write("\n</script> \n"),
  write("\n</head> \n"),
  write("\n<style> \n"),
   write( ".tag_searched { display: inline-block; font-size: 18pt; font-weight: bold; font-style: italic } "), nl,
   write( ".tag_rs { display: inline-block; font-size: 18pt; margin-left: 7px; } "), nl,
   write( ".leftpar_rs { color: #C64F00 ; font-weight: bold } "), nl,
   write( ".rightpar_rs { color: #C64F00 ; font-weight: bold } "), nl,

   write( ".tag_py { display: inline-block ; font-size: 18pt; margin-left: 7px;} "), nl,
   write( ".leftpar_py { color: #008600 ; font-weight: bold } "), nl,
   write( ".rightpar_py { color: #008600 ; font-weight: bold } "), nl,



   write( ".tag_pl { display: inline-block ; font-size: 18pt; margin-left: 7px;} "), nl,
   write( ".leftpar_pl { color: #C0C0C0 ;  } "), nl,
   write( ".rightpar_pl { color: #C0C0C0 ;  } "), nl,

   write( ".tag_metta { display: inline-block ; font-size: 18pt; margin-left: 7px; } "), nl,
   write( ".leftpar_metta { color: #E2E2E2 ;  } "), nl,
   write( ".rightpar_metta { color: #E2E2E2 ;  } "), nl,
   write( ".big_tag { display: inline-block ; font-size: 12pt; font-style: italic ; background-color: #737373; padding: 6px; border-radius: 4px; border: 0px solid #000000; } "), nl,
   write( ".enlarged { display: inline-block ; font-size: 14pt; font-style: italic ; color: #E7E7E7;   } "), nl,
   write( ".tag_arrow { display: inline-block ; font-size: 14pt; font-style: italic ; color: #FF4040;   } "), nl,

   write( ".tag_operator { display: inline-block ; font-size: 16pt;  color: #FFFFFF;   } "), nl,

   write( ".metta_clause { background-color: #434343; padding: 8px; border-radius: 9px; font-size: 10pt; color: #ADADAD; margin: 8px; } "), nl,
   write( ".tag_parenthesis0 { display: inline-block ;color: #000000 ; font-size: 20pt; font-weight: bold  } "), nl,
   write( ".tag_parenthesis1 { display: inline-block ;color: #FFFF40 ; font-size: 14pt; } "), nl,
   write( ".tag_parenthesis2 { display: inline-block ;color: #A9FFFE ; font-size: 14pt; } "), nl,
   write( ".tag_parenthesis3 { display: inline-block ;color: #13FF13 ; font-size: 14pt; } "), nl,
   write( ".tag_parenthesis4 { display: inline-block ;color: #FFC0FF ; font-size: 14pt; } "), nl,
   write( ".tag_parenthesis5 { display: inline-block ;color: #80FFFF ; font-size: 14pt; } "), nl,
   write( ".tag_parenthesis6 { display: inline-block ;color:  #ECECEC; font-size: 14pt; } "), nl,



  write("\n</style> \n"),
  write("\n<body style=\"font-family: arial; font-size: 12pt; background-color: #E5E5E5\"> \n"),
  write(" <H2> \n Time "), write(  TimeStamp ), write(" </H2> \n"),
  %write(" <H2> \n"), write( Inc_Metta ),   write(" </H2> \n"),
  %write(" <H2> \n"), write( Display_per_file_or_per_tag ),   write(" </H2> \n"),
  %write(" <H2> \n"), write( LeadingTag ),   write(" </H2> \n"),
  told().

%---
find_tags( Tag , Tg1 , Tg2 , Tg3 ):-
  eval_tag_found( 'metta', Tag , Tg1 , Tg2 , Tg3 ,_) , !.

find_tags( _Tag , "cantf1", "cantf2", "cantf3" ):-!.
%  eval_tag_found( 'metta', Tag , Tg1 , Tg2 , Tg3 ) ,


%display_tags_not_found():-
%   eval_tag_found( Tag  ),   retract( eval_tag( Tag  ) ), fail , !.
%display_tags_not_found():-
%  write( "@DOC-tag not found - uncovered / created default <br>" ), nl,
%  eval_tag( _, Tag, _  ),
%  not( eval_tag_found( 'metta_doc', Tag, _ , _ , _  ) ),
%  write( "<b>  Default @doc code : " ) , write( Tag ), write( "</b> <br> " ), nl, fail, !.


tag_types( 'rust' ).
tag_types( 'python' ).
tag_types( 'metta_ontol_pl' ).
tag_types( 'metta_example' ).
tag_types( 'metta' ).
tag_types( '' ).



display_tags_not_found():-
  write( "@METTA-example TAGS <br>" ), nl,
  tag_types( Type ),
  nl, write(" <br> "), write( Type ) ,  nl,
%  eval_tag( _, Tag, _  ),
%  write( "<br> " ), nl,
%  write( "<b>  Default metta example : -" ) , write( Tag ), write( "-</b> "),
  eval_tag_found( Type , Tag , _ , _ , _ ,_) ,
%  write(" not FOUND "),
  nl, write(" <br> "), write( Type ) , write( " " ), write( Tag ), nl,
  fail, !.

  % write( Tg1 ),  write( " " ), write( Tg2 ), write( " " ), write( Tg3 ),

%  find_tags( Tag , Tg1 , Tg2 , Tg3 ),
%  eval_tag_found( 'metta', Tag , Tg1 , Tg2 , Tg3 ) ,


%display_tags_not_found():-
%  write( "Tags not found - uncovered <br>" ), nl,
%  eval_tag( _, Tag, _  ),

%  not( eval_tag_found( _, Tag  ) ),
%  write( "<b>  " ) , write( Tag ), write( "</b> <br> " ), nl, fail, !.

display_tags_not_found():- !.

%---
% i,o,o
get_tag_properties( Tag, "yes", "yes" ):- eval_tag( 'rust', _, Tag, _ , _ , _),  eval_tag( 'pl_tag', _, Tag, _ , _ , _), !.
get_tag_properties( Tag, "yes", "" ):- eval_tag( 'rust', _, Tag, _ , _ , _),   !.
get_tag_properties( Tag, "", "yes" ):- eval_tag( 'pl_tag', _, Tag, _ , _ , _),   !.
get_tag_properties( _El, "", "" ):-!.

write_cell( Tag ):-    write( "<td>" ),      write( Tag ),     write( "</td>" ), !.

write_cell_pre( Tag ):-    write( "<td> <pre>" ),      write( Tag ),     write( "</pre> </td>" ), !.

write_head( Tag ):-    write( "<th style=\"text-align:left\">" ),      write( Tag ),     write( "</th>" ), !.




%---
sequence_of_tags( Prolog_and_rust, Rustonly, Prologonly ):-
   findall( Tag, eval_tag( 'rust', _, Tag, _ , _ , _), Taglis1 ),
   findall( Tag2, eval_tag( 'pl_tag', _, Tag2, _ , _ , _), Taglis2 ),
   intersection( Taglis1, Taglis2,  Prolog_and_rust0 ), sort( Prolog_and_rust0, Prolog_and_rust ),
   subtract( Taglis1, Taglis2,  Rustonly0 ), sort( Rustonly0, Rustonly ),
   subtract( Taglis2, Taglis1,  Prologonly0 ), sort( Prologonly0, Prologonly ).




%   append( Taglis1, Taglis2, Taglis3 ),  sort( Taglis3, Taglis4 ),
%   member( El, Taglis4 ),

% found_search_result(metta_example,
%  "match",1,3,'../
% eval_tag(metta_tag_std_lib,1348,"format-args",'../canary/stdlib_me
% eval_tag(metta_example,58,"let*",'../../tests/baseline_compat/hyp
:- dynamic tag_for_table/1.

set_tags_for_table():-  retractall( tag_for_table( _ ) ),
	sequence_of_tags( Prolog_and_rust, Rustonly, Prologonly ),
    append( Prolog_and_rust, Rustonly, C1 ),  append( C1, Prologonly, Taglis4 ),
    member( El_tag, Taglis4 ),   assert( tag_for_table( El_tag ) ), fail, !.
set_tags_for_table():- !.


find_has_metta_doc( El_tag , "has @doc" ):- eval_tag( metta_tag_std_lib, _, El_tag , _F, _Str, _ ),!.
find_has_metta_doc( _El_tag , "<b> NO </b> @doc" ):- !.

find_has_metta_type_doc( El_tag , "has type @doc" , Str ):- eval_tag( metta_tag_std_lib, _, El_tag , _F, Str, _ ),
 sub_string( Str, _ , _ , _ , "(: "), ! .

find_has_metta_type_doc( _El_tag , "<b> No type</b> @doc" , "<b> No type</b> @doc" ):- !.

%  metta_function_declaration

% is_metta
%---
% 'rust'
show_read_files_dir():-
 read_files_dir( Tp, Dir ), write( Tp ), write(" <b>"), write( Dir ), write(" </b> <br>"), nl, fail, !.
show_read_files_dir():-  write("<br> &nbsp; <br> "), !.


% eval_tag( 'rust', Linum, Taglow, F , Sline , Ari)
% eval_tag( 'rust', Linum, Taglow, F , Sline , Ari)
    %only_small_number_of_occurences_display( Num_oc, Files_list_s0, Files_list_s ),
    %only_small_number_of_occurences_display( Num_oc2, Files_list_s02, Files_list_s2 ),
%   find_metta_test_occurences( El_tag, Numberof_s, _Files_list ),

write_csv_val( Sx ) :-  str_code_remove( Sx, 39,  Sx2 ),
   str_code_remove( Sx2, 91,  Sx3 ),  str_code_remove( Sx3, 93,  Sx4 ),
   str_code_remove( Sx4, 60,  Sx5 ),  str_code_remove( Sx5, 62,  Sx6 ),
   str_code_remove( Sx6, 47,  Sx7 ),
   str_code_remove( Sx7, 40,  Sx8 ), str_code_remove( Sx8, 41,  Sx9 ), str_code_remove( Sx9, 58,  Sx10 ),
     str_code_remove( Sx10, 59,  Sx11 ),
   write( Sx11 ).

write_csv_cols( [] ):-  !.
write_csv_cols( [ H ] ):-  !,  write_csv_val( H ) .
write_csv_cols( [ H | Rest ] ):-  !,  write_csv_val( H ),  write( ";" ),  write_csv_cols(  Rest  ).




:- dynamic last_line/2.

belongs_to_previous_line( Fn, Linum , belongs_to_previous_line, Linum_last ):-
  last_line( Fn, Linum_last ), Diffrnce is Linum - Linum_last,
 % doubt
  Diffrnce < 3 ,
%  Diffrnce < 2 ,
  !.
belongs_to_previous_line( _, Linum , doesnt_belong_to_previous_line , Linum_last ):-  Linum_last is Linum, !.

%---
is_flag_to_metta_files( 'yes' ):-!.

%---
change_output_for_creating_files( Fn , Count ):- is_flag_to_metta_files( 'yes' ),
  atom_number( Ato, Count ),
  file_base_name( Fn, Base ),  file_name_extension( Base2, _Ext, Base ), !,
  atomic_list_concat( [ '../../tests/from_python_generated/', Base2, Ato, '_py.metta' ], Rs ),
  told(),
  tell( Rs ).
change_output_for_creating_files( _Fn , _ ):- !.


%---
write_belongs_to( belongs_to_previous_line , Lx, _Linum, _Fn, Linum_last, Tag1, Tag2  ):- !,
  number_string( Linum_last, Sx2 ), write( "\n; <br> lastline " ), write( Sx2 ), write( " <br> \n" ),
  write( " \n" ),   write( Tag1 ) ,   write( Lx ),   write( Tag2 ) .


% today_key( Dk ),
% incr2( N2 ), number_string( N2, N2_s ),
% concat_slist( [ "; ", N2_s, ". ", Dk, " R.v.Vessum converted python source to metta \n!(assertEqualToResult


write_belongs_to( _ , Lx, Linum, Fn, Linum_last , Tag1, Tag2 ):- !,
  today_key( Dk ),
  number_string( Linum_last, Sx2 ), write( "\n; <br>  lastline " ), write( Sx2 ), write( " <br> \n" ),
  incr( Count ), number_string( Count, Sx ),
  change_output_for_creating_files( Fn , Count ),
  write( "\n; " ), write( Dk ),  write( " R.v.Vessum converted python source to metta " ),
  write( "\n; FILE: " ), write( Sx ),  write( Fn ), write( " :" ), write( Linum ),

  % ves
  write( Tag1 ), write( Lx ) ,   write( Tag2 ), write("  !(assertEqualToResult ( )  (  )  )").

%  write( "\n<br> ends at : "), write( Linum_last ), write( "<br>" ),
%  write( " \n<br> \n" ),
%  write( "\n<br>  ends at : "), write( Linum_last ), write( "<br>" ),



%  write( "</pre> <br>" ).
%---
%	eval_tag
%	eval_tag(python_src_metta_test,64,"assertequalmettarunnerresults(",'../../../hyperon-experimental/python/tests/test_examples.py',"        self.assertEqualMettaRunnerResults(metta.run('!(get-st (name id-001))'),\n                         [[S('Sam')]])",0)
	% file_base_name( Fn, Base ),
	% file_name_extension( Base2, _Ext, Base ),
%	write( "\n<br>  ends at : "), write( Line_end ), write( "<br>" ),

%<pre>        self.assertEqualMettaRunnerResults(metta.run('!(get-st (name id-001))'),
%                         [[S('Fritz')]])</pre>
% !(assertEqualToResult (change-state! aq au ) ( result_with_type ))

% str_replace_tag(At, Repwhat, RepWith, Resu):-
% sub_str_between( Lx, Second_tag_begin, Second_tag_end, Subz2 ),

concat_slist( [], Resu, Resu ):- !.
concat_slist( [ H | Rs ], Hs, Resu ):- !, string_concat( Hs, H, C1 ), concat_slist( Rs , C1, Resu ).

%----
concat_slist_with_delim( [], Resu, _, Resu ):- !.
concat_slist_with_delim( [ H ], Hs, _Delim, Resu ):- !, string_concat( Hs, H, Resu ), !.
%--
% to implement , clean this kind of constructs
clean_python_string_constructs( Subz3, Subz3 ):- !.
%---

% eval_tag(python_src_metta_test,38,
% "assertequalmettarunnerresults(",
% '../../../hyperon-experimental/python/tests/test_pln_tv.py',
% orginal string
% " self.assertEqualMettaRunnerResults(\n   metta.run('!(pln (And (P A) (P $x)))'),\n
%    [metta.parse_all('''\n  ((And (P A) (P A)) (stv 0.5 0.8))\n    ((And (P A) (P B)) (stv 0.3 0.8))\n    ''')])",0)

try_read_python_second_arg_asserteq( AfterStr, Subz30 ):- sub_string( AfterStr, _,_,_, "metta.parse_all" ),
  sub_str_between( AfterStr, "'''", "'''", Subz30 ), !.
try_read_python_second_arg_asserteq( AfterStr, Subz30 ):- sub_string( AfterStr, _,_,_, "metta.parse_all" ),
  sub_str_between( AfterStr, "[", "]", Subz30 ), !.

try_read_python_second_arg_asserteq( AfterStr, Subz30 ):-
  sub_str_between( AfterStr, ",", ")", Subz30 ), !.

% eval_tag(python_src_metta_test,22,
% "assertequalmettarunnerresults(",
% 1. '../../../hyperon-experimental/python/tests/test_run_metta.py',
% "   self.assertEqualMettaRunnerResults(metta.run(program),\n  [metta.parse_all('red  green  blue'),
%   metta.parse_all('5')])",0)

% 2. ---self.assertEqualMettaRunnerResults(metta.run(program),
%             [metta.parse_all('red  green  blue'), metta.parse_all('5')])

try_clean_python_to_metta( Lx0, Lx2 ):- sub_string( Lx0, _,_,_, "self.assertEqualMettaRunnerResults" ),
 str_code_replace( Lx0, 10, 32, Lx ), sub_str_between( Lx, "metta.run('!", "'", Subz2 ),
 str_part_after_tag( Lx, "self.assertEqualMettaRunnerResults", AfterStr ),
 try_read_python_second_arg_asserteq( AfterStr, Subz30 ),
 clean_python_string_constructs( Subz30, Subz3 ), ! , today_key( Dk ),
 incr2( N2 ), number_string( N2, N2_s ),
 concat_slist( [ "; ", N2_s, ". ", Dk, " R.v.Vessum converted python source to metta \n!(assertEqualToResult ", Subz2, " ", Subz3, " &nbsp &nbsp ) "  ], "", Lx2 ).

try_clean_python_to_metta( Lx, Lx3 ):- sub_string( Lx, _,_,_, "self.assertEqualMettaRunnerResults" ),
 sub_str_between( Lx, "metta.run(pro", ")", _Subz2 ),
  % str_part_after_tag( Lx, "self.assertEqualMettaRunnerResults", AfterStr ),
 str_part_after_tag( Lx, "metta.parse_all('", _AfterStr2 ), !,
 % clean_python_string_constructs( Subz30, Subz3 ), ! ,
 today_key( Dk ),
 incr2( N2 ), number_string( N2, N2_s ),
 concat_slist( [ "\n;<br>", N2_s, " ", Dk, " ", "R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  ) ", "\n;<br>\n" ], "", New_s ),
 str_replace_tag( Lx, "assertEqualMettaRunnerResults", New_s, Lx2 ),
 str_replace_tag( Lx2, "self.assertEqual", New_s, Lx3 ).

% try_clean_python_to_metta( Lx, "jajajaj" ):- sub_string( Lx, _,_,_, "self.assertEqual" ), !.

try_clean_python_to_metta( Lx, Lx2 ):- sub_string( Lx, _,_,_, "self.assertEqual" ),
 sub_str_between( Lx, ".run(pro", ")", _Subz2 ), !,
 today_key( Dk ),
 incr2( N2 ), number_string( N2, N2_s ),
 concat_slist( [ "\n;<br>", N2_s, " ", Dk, " ", "R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  )", "\n;<br>\n" ], "", New_s ),
 str_replace_tag( Lx, "self.assertEqual", New_s, Lx2 ).




% concat_slist( [ "; ", N2_s, ". ", Dk, " R.v.Vessum converted python source to metta \n!(assertEqualToResult ", "metta.run(program)", " ", AfterStr2, " &nbsp &nbsp ) "  ], "", Lx2 ).


try_clean_python_to_metta( Lx0, Subz2 ):- sub_string( Lx0, _,_,_, "metta.run" ), sub_string( Lx0, _,_,_, "'''" ),
 %  str_code_replace( Lx0, 10, 32, Lx ),
 sub_str_between( Lx0, "'''", "''')", Subz2 ), !.


try_clean_python_to_metta( Lx0, Subz2 ):- sub_string( Lx0, _, _, _, "metta.run" ),
 sub_str_between( Lx0, "metta.run('", "')" , Subz2 ), !.
% metta.run('!(change-st (name id-001) Fritz)')

try_clean_python_to_metta( Lx, Lx ):- !.



make_python_src_to_metta_table():-    retractall( tel( _ ) ) , assert( tel( 0 ) ),
    init_tel2(),
	eval_tag( 'python_src_metta_test' , Linum, _Tag, Fn, Lx , _ ),
	belongs_to_previous_line( Fn, Linum , Is_belongs_to,  Linum_last ),
	str_code_occurence_count( Lx, 10,  Count ), Line_end is Linum + Count,
	retractall( last_line( _, _ ) ), assert( last_line( Fn, Line_end ) ),
	try_clean_python_to_metta( Lx, Lx2 ),
	write_belongs_to( Is_belongs_to, Lx2, Linum, Fn, Linum_last , "\n; <pre>\n", "\n;</pre>\n" ),
    fail, ! .
make_python_src_to_metta_table():- write( " " ), !.


make_rust_prolog_table():-      show_read_files_dir(),
	write( "<table border=1 cellspacing=0 style=\"border: 1px solid black; min-width:600px; max-width:900px; margin: auto\">" ),
    retractall( tel(_)) , assert( tel( 0 ) ),    write( "<tr>" ),
    write_head( " " ), write_head( "function" ), write_head( "Rust" ), write_head( "Prolog" ),
	write_head( "Occurs in metta-files" ),	write_head( "Has metta-files" ),
	write_head( "Has @doc" ),   write_head( "@doc TYPE decl" ),
	write_head( "Num of metta-test" ),	write_head( "Has metta-testf" ),
    write( "</tr>" ),    tag_for_table( El_tag ),    is_metta_function( El_tag ),
    incr( Count ), number_string( Count, Sx ),
    get_tag_properties( El_tag, Hasrust, Hasprolog ),
	get_tag_occurences( El_tag , _Num_oc, Occurences_string , Files_list_s ),
	get_tag_occurences_test( El_tag , _Num_oc2, Occurences_string2 , Files_list_s2 ),
    find_has_metta_doc( El_tag , Has_doc_s ),
	find_has_metta_type_doc( El_tag , _Has_tdoc_s , Type_descr ),
    write( "<tr>" ), write_cell( Sx ),
    write_cell( El_tag ), write_cell( Hasrust ), write_cell( Hasprolog ),
	write_cell( Occurences_string ),  write_cell( Files_list_s ),
	write_cell( Has_doc_s ),	  write_cell_pre( Type_descr ),	write_cell( Occurences_string2 ),  write_cell( Files_list_s2 ),
    write( "</tr>" ), fail, !.
make_rust_prolog_table():- write( "</table>" ), !.


make_rust_prolog_csv():-
    retractall( tel(_)) , assert( tel( 0 ) ),
    write_csv_cols( [ "num" , "function" , "Rust" , "Prolog"   , "Occurs in metta-files" , "Has metta-files" ,
	"Has @doc" , "@doc TYPE decl" ,	 "Num of metta-test" , "Has metta-testf" ]), write("\r"), nl,


	tag_for_table( El_tag ),    is_metta_function( El_tag ),
    incr( Count ), number_string( Count, Sx ),
    get_tag_properties( El_tag, Hasrust, Hasprolog ),
	get_tag_occurences( El_tag , _Num_oc, Occurences_string , Files_list_s ),
	get_tag_occurences_test( El_tag , _Num_oc2, Occurences_string2 , Files_list_s2 ),
    find_has_metta_doc( El_tag , Has_doc_s ),
	find_has_metta_type_doc( El_tag , _Has_tdoc_s , Type_descr ),
    write_csv_cols( [ Sx , El_tag ,  Hasrust ,  Hasprolog ,  Occurences_string , Files_list_s ,
	 Has_doc_s , Type_descr , Occurences_string2 ,   Files_list_s2 ] ), write("\r"), nl,
	fail, !.
make_rust_prolog_csv():- write( "" ), !.



memory_data_to_file( Fn ):-  tell( Fn ),
 eval_tag( A , B , C , D , E, F ), write_term( eval_tag( A , B , C , D , E, F ), [quoted(true)] ), nl, fail.
memory_data_to_file( _Fn ):-
 eval_tag_found( A , B , C , D , E, F ), write_term( eval_tag_found( A , B , C , D , E, F ), [quoted(true)] ), nl, fail.
memory_data_to_file( _Fn ):-
 found_search_result( A , B , C , D , E, F ), write_term( found_search_result( A , B , C , D , E, F ), [quoted(true)] ), nl, fail.
% eval_tag_secondary/2.
memory_data_to_file( _ ):-  told(), ! .


%---
:- dynamic tag_occurences/3.
:- dynamic tag_occurences_test/3.

% max_length_list( [ A,B,C|_], [A,B,C]):-  !.
% max_length_list( [ A,B|_], [A,B]):-  !.
max_length_list( [ A|_], [A]):-  !.
max_length_list(_Files0, []):-!.


get_tag_occurences( Tag , Count, Sx , Files_s ):-
 tag_occurences( Tag , Count, Files0 ), max_length_list(Files0, Files), number_string( Count, Sx ),  term_string( Files, Files_s, [] ), !.
get_tag_occurences( _Tag , 0, "", "has no metta" ):- !.
%----
get_tag_occurences_test( Tag , Count, Sx , Files_s ):-
 tag_occurences_test( Tag , Count, Files0 ), max_length_list(Files0, Files), number_string( Count, Sx ),  term_string( Files, Files_s, [] ), !.
get_tag_occurences_test( _Tag , 0, "", "has no metta test" ):- !.

%---
only_small_number_of_occurences_display( Num_oc, Files_list_s, Files_list_s ):- Num_oc < 6, !.
only_small_number_of_occurences_display( _Num_oc, _Files_list_s0, "" ):-  !.
% only_small_number_of_occurences_display( _Num_oc, Files_list_s, Files_list_s ):-!.

%----

update_tag_occurences( Tag , Count, Base ):-  retract( tag_occurences( Tag , _ , Files0 ) ), !,
  sort( [ Base | Files0 ], Files ),  assert( tag_occurences( Tag , Count, Files ) ).
update_tag_occurences( Tag , Count, Base ):-  assert( tag_occurences( Tag , Count, [Base] ) ).
%--
update_tag_occurences_test( Tag , Count, Base ):-  retract( tag_occurences_test( Tag , _ , Files0 ) ), !,
  sort( [ Base | Files0 ], Files ),  assert( tag_occurences_test( Tag , Count, Files ) ).
update_tag_occurences_test( Tag , Count, Base ):-  assert( tag_occurences_test( Tag , Count, [Base] ) ).

%----

count_tags_files_occurences():-   retractall( tag_occurences( _ , _ , _  ) ) ,
  tag_for_table( Tag ),   retractall( tel( _ ) ),  assert( tel( 0 ) ),
  found_search_result( metta_example , Tag, _Lev , _Line , Fn , _Str_part ),
  file_base_name( Fn, Base ),  file_name_extension( Base2, _Ext, Base ),
  incr( Count ),  update_tag_occurences( Tag , Count, Base2 ),  fail, ! .
count_tags_files_occurences():- !.
%---
count_tags_files_occurences_test():-   retractall( tag_occurences_test( _ , _ , _  ) ) ,
  tag_for_table( Tag ),   retractall( tel( _ ) ),  assert( tel( 0 ) ),
  found_search_result( metta_example , Tag, _Lev , _Line , Fn , Str_part ),
  sub_string( Str_part, _,_,_, "assert" ),
  file_base_name( Fn, Base ),  file_name_extension( Base2, _Ext, Base ),
  incr( Count ),  update_tag_occurences_test( Tag , Count, Base2 ),  fail, ! .
count_tags_files_occurences_test():- !.


%  found_search_result(metta_example,"match",1,3,'../../tests/baseline_compat/metta-morph_tests/match_feval.metta',"!(match &self (= (f $x) $y) $y)")
% eval_tag(metta_tag_std_lib,1225,"print",'../canary/stdlib_mettalog.metta',"(@doc print-mods!\n  (@desc \"Prints all modules with their correspondent spaces\")\n  (@params ())\n  (@return \"Unit atom\"))",0)


% found_search_result(metta_example,
%  "match",1,3,'../
% eval_tag(metta_tag_std_lib,1348,"format-args",'../canary/stdlib_me
% eval_tag(metta_example,58,"let*",'../../tests/baseline_compat/hyp

% create_view_table().
create_view_table():- !,
 start_analyse( 'display_per_tag', 'rust_leading', 0, '', is_dummy_tag, include_metta_files ).

:- dynamic picture/1.

create_install_manual():-  retractall( picture( _ )),
 % directory
 directory_files( 'result/', Lis0), sort( Lis0, Lis ),
 member(El, Lis), El \= '..', El \= '.',
 sub_atom( El , _,_,_, '.png' ),
 % write( El ), nl,
 assert( picture( El ) ),
 fail, !.
create_install_manual():-
 tell( 'result/metta_install_manual.htm' ),
 write( "\n " ),
 picture( El ),
 write( "<br> "), 
 % write( El ) ,
 write( "<div style=\"display:inline-block; width: 100% ; text-align: center \"> <img src=\"" ), 
 write( El ), 
 write( "\" width=700 > </div> <br> &nbsp <br> "),
 fail.
create_install_manual():- told(), !.


%---
start_analyse_metta( Display_per_file_or_per_tag, LeadingTag, Tp, MdfKeyaft, Is_mk_update,  _X , Inc_Metta  ):- !,
  retractall( eval_tag( _, _, _ , _,_ , _ ) ),
  retractall( eval_tag_secondary( _ , _ ) ),
  retractall( eval_tag_found( _ , _ , _ , _, _ ,_ ) ),

  retractall( found_search_result( _,_, _, _, _, _ ) ),
  retractall( per_file( _ ) ),
%  retractall( extra_string_to_search( _ ) ),
   % assert( extra_string_to_search( "count" ) ),
  % chercher_a( Zk ),
  Zk = "",
  today_key( Dk ), write( "today key " ), write( Dk ),  Xfi = 'data/htm_file_list.pl',
  short_code_metta( Inc_Metta, Short_code ),
  atomic_list_concat([ 'result/analyse_result_', LeadingTag, '_', Display_per_file_or_per_tag, '_', Short_code, '.htm' ], Htmfile ),
  reset_htm( Display_per_file_or_per_tag, LeadingTag, Inc_Metta,  Htmfile ),
  % search_all_files_with_prolog_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi  ),
  search_all_files_with_leading( LeadingTag, Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi , Inc_Metta ),
  % search_all_files_with_rust_leading( Tp, Dk, Zk, MdfKeyaft, Is_mk_update, Xfi ),
  memory_data_to_file( 'data/tags_data.pl' ),
  set_tags_for_table(),
  count_tags_files_occurences(),
  count_tags_files_occurences_test(),
  append( Htmfile ),

% TEMP turn off
%  display_metta_sresults( Display_per_file_or_per_tag , Zk, 'dummyxxaaq' ),


 % show_identified_eval_tags( 'rust' ),
 % show_identified_eval_tags( 'pl_tag' ),
 % show_identified_eval_tags( 'python' ),
 % show_identified_eval_tags( 'pl_interp_tag' ),
 % show_identified_eval_tags( 'metta_tag' ),
 % show_identified_eval_tags( 'metta_tag_extra' ),
 % show_tags_which_dont_have_tests(),
 % show_identified_eval_secondary_tags(),
 % display_tags_not_found(),
  make_rust_prolog_table(),
  make_python_src_to_metta_table(),
  told(),

  tell( 'result/prolog_rust.csv' ),  make_rust_prolog_csv(),  told(),

  retractall( eval_tag_found( _ , _ , _, _, _ ,_) ),
  write( "analyses result written to file: \n" ),
  write( Htmfile ), nl.

make_csv():- !,
   tell( 'result/prolog_rust.csv' ),  make_rust_prolog_csv(),  told().


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
rightbracket -->  [C],  { C == 93  }.
singlequote -->  [C],  { C == 39  }.

% tag_x( foo ) --> "foo" .
% tag_x( bar( A, B ) ) -->  "bar", whites,    digits( ACodes ), whites, digits( BCodes ),  { number_codes( A, ACodes), number_codes( B, BCodes ) }.

% tag_x( bar( A, B ) ) -->  "bar(",   whites ,  digits( ACodes ), whites, komma , whites, digits( BCodes ),  { number_codes( A, ACodes), number_codes( B, BCodes ) }.
%tag_x( bar( Vn , B ) ) -->  "bar(",   whites ,  prolog_var_name( Vn ), whites, komma , whites, digits( BCodes ),  {  number_codes( B, BCodes ) }.
% tag_x( eval20( A , A2, A3, A4, B , X5 ) ) -->  "eval_20(",   whites ,  string( Vn ), whites, komma , whites,

%tag_x( eval20(  B  ) ) -->  "eval_20(",   whites ,  string( Vn ), whites, komma , whites,
%       string( Vn2 ), whites, komma , whites ,
%       string( Vn3 ), whites, komma , whites ,
%       string( Vn4 ), whites, komma , whites ,
%     leftbracket , whites, singlequote ,  string( BCodes ),  singlequote, whites, komma ,
%     string( Rest5 ),
%  { string_codes( _A, Vn ), string_codes( _A2, Vn2 ), string_codes( _A3, Vn3 ), string_codes( _A4, Vn4 ),  string_codes( B, BCodes ), string_codes( _X5, Rest5 ) }.

tag_x( eval20(  B , X5q ) ) -->  "eval_20(",   whites ,  string( Vn ), whites, komma , whites,
       string( Vn2 ), whites, komma , whites ,
       string( Vn3 ), whites, komma , whites ,
       string( Vn4 ), whites, komma , whites ,
     leftbracket , whites, singlequote ,  string( BCodes ),  singlequote, whites, komma ,
     string( Rest5 ),  rightbracket,
	   string( _Rest6 ),
  { string_codes( _A, Vn ), string_codes( _A2, Vn2 ), string_codes( _A3, Vn3 ), string_codes( _A4, Vn4 ),  string_codes( B, BCodes ), string_codes( X5q, Rest5 ) }.

%---

tag_x40( eval40(  B , X5q ) ) -->  "eval_40(",   whites ,  string( Vn ), whites, komma , whites,
       string( Vn2 ), whites, komma , whites ,
       string( Vn3 ), whites, komma , whites ,
       string( Vn4 ), whites, komma , whites ,
     leftbracket , whites, singlequote ,  string( BCodes ),  singlequote, whites, komma ,
     string( Rest5 ),  rightbracket,
	   string( _Rest6 ),
  { string_codes( _A, Vn ), string_codes( _A2, Vn2 ), string_codes( _A3, Vn3 ), string_codes( _A4, Vn4 ),  string_codes( B, BCodes ), string_codes( X5q, Rest5 ) }.

tag_x70( eval70(  B , X5q ) ) -->  "eval_70(",   whites ,  string( Vn ), whites, komma , whites,
       string( Vn2 ), whites, komma , whites ,
       string( Vn3 ), whites, komma , whites ,
       string( Vn4 ), whites, komma , whites ,
     leftbracket , whites, singlequote ,  string( BCodes ),  singlequote, whites, komma ,
     string( Rest5 ),  rightbracket,
	   string( _Rest6 ),
  { string_codes( _A, Vn ), string_codes( _A2, Vn2 ), string_codes( _A3, Vn3 ), string_codes( _A4, Vn4 ),  string_codes( B, BCodes ), string_codes( X5q, Rest5 ) }.

%:- discontiguous eval_40/6.
%:- discontiguous eval_70/6.

% tag_x( quux( S ) ) -->  "quux", whites, nonblanks( Codes ),   { string_codes( S, Codes ) }.


tag_list( [ Tag_token | Tags_tokens ]) -->    tag_x( Tag_token ),    whites,    tag_list( Tags_tokens ).
tag_list( [] ) --> [].

tag_list40( [ Tag_token | Tags_tokens ]) -->    tag_x40( Tag_token ),    whites,    tag_list40( Tags_tokens ).
tag_list40( [] ) --> [].

tag_list70( [ Tag_token | Tags_tokens ]) -->    tag_x70( Tag_token ),    whites,    tag_list70( Tags_tokens ).
tag_list70( [] ) --> [].


try_read_find_eval20_tag_in_string( Str , Result_tag, Len ):-
%   string_codes( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).", Codes_x ),
   string_codes( Str , Codes_x ),   phrase( tag_list( Tags_tokens ),  Codes_x ),    Tags_tokens = [ Xres ],   Xres = eval20(  Result_tag,  Rest_Args  ) ,
   split_string( Rest_Args , ",", "" , Lis ), length( Lis, Len ),   !.

try_read_find_eval20_tag_in_string( Str , Result_tag, Len ):-
%   string_codes( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).", Codes_x ),
   string_codes( Str , Codes_x ),   phrase( tag_list40( Tags_tokens ),  Codes_x ),    Tags_tokens = [ Xres ],   Xres = eval40(  Result_tag,  Rest_Args  ) ,
   split_string( Rest_Args , ",", "" , Lis ), length( Lis, Len ),   !.

try_read_find_eval20_tag_in_string( Str , Result_tag, Len ):-
%   string_codes( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).", Codes_x ),
   string_codes( Str , Codes_x ),   phrase( tag_list70( Tags_tokens ),  Codes_x ),    Tags_tokens = [ Xres ],   Xres = eval70(  Result_tag,  Rest_Args  ) ,
   split_string( Rest_Args , ",", "" , Lis ), length( Lis, Len ),   !.


% eval_20(Eq,RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).
test12():-
 % phrase(commands(Commands), `bar 1 2 foo quux bloop`),
 % string_codes( "eval_20( _Zxs , _Pa foo quux bloop", Codes_x ),
 %   string_codes( "eval_20( _Zxs , Pq , _Za , Qe, ['jajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).", Codes_x ),
 %   phrase( tag_list( Tags_tokens ),  Codes_x ),
 %   Tags_tokens = [ Xres ],   Xres = eval20(  Result_tag  ) ,    !,
   try_read_find_eval20_tag_in_string( "eval_70( _Zxs , Pq , _Za , Qe, ['j2ajaj',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y)." , Result_tagx, _ ),

%   maplist( writeln, Tags_tokens ).
   write_term( Result_tagx , [] ).
%----

% HERE ALL the config

color_for_file_type( Fn , "#ECED99" ):-  sub_atom( Fn, _, _, _, '.metta' ), !.
color_for_file_type( Fn , "#A7E3A7" ):-  sub_atom( Fn, _, _, _, '.py' ), !.
color_for_file_type( Fn , "#D3D4FF" ):-  sub_atom( Fn, _, _, _, '.pl' ), !.
color_for_file_type( Fn , "#FFD6BB" ):-  sub_atom( Fn, _, _, _, '.rs' ), !.

color_for_file_type( _Filewithpath, "#E7E7E7" ):- !.

% we dont want all the assertEqual etc here
is_metta_function( Tag ):- string_lower( Tag, Tag_lower ), sub_string( Tag_lower, _,_,_, "assert" ), !, fail.
is_metta_function( "superpose" ):- !, fail.
is_metta_function( "&self" ):- !, fail.
is_metta_function( Tag ):-  string_length( Tag, Lex ), Lex > 3,  Tag \= "eval", !.

% % { assert_equal_to_result_op.clone() });
rust_detect_tag( "regex(r\"", "\"" , "{", ".clone(" ):- !.

rust_metta_tag( "assert_eq!(", "" ):- !.

python_detect_tag( "metta.run('!(", " " ):- !.

python_metta_test_tag1( "assertequalmettarunnerresults(", "" ):- !.
python_metta_test_tag2( "assertequal(", "" ):- !.
python_metta_test_tag3( "metta.run(", "" ):- !.
python_metta_test_tag4( "program = '''", "" ):- !.


% python_metta_tag( "assertequalmettarunnerresults", "" ):- !.

metta_interp_tag( "'", "'" ):- !.
metta_ontology_tag( "'", "'" ):- !.
metta_file_tag_find( "(", " " ):- !.
metta_file_tag( "(", "" ):- !.
metta_file_tag_til_space( "(", " " ):- !.

metta_function_declaration( "(: ", " " ):- !.

metta_docfile_tag( "(@doc ", "" ):- !.






% README HERE:  USAGE:
% first consult the prolog file with this :
%  [metta_analyse].
% THEN call as below :
% Calling examples :
%  start_analyse( 'display_per_tag', 'rust_leading', 0, '', is_dummy_tag, include_metta_files ).
%  start_analyse( 'display_per_tag', 'prolog_leading', 0, '', is_dummy_tag, not_include_metta_files ).
%  start_analyse( 'display_per_file', 'rust_leading', 0, '', is_dummy_tag , not_include_metta_files).
%  start_analyse( 'display_per_file', 'python_leading', 0, '', is_dummy_tag , not_include_metta_files ).


start_analyse( Display_per_tag_or_file, LeadingTag, Tp, MdfKeyaft, Is_mk_update , Inc_metta ):-
  start_analyse_metta( Display_per_tag_or_file, LeadingTag, Tp, MdfKeyaft, Is_mk_update, 'xdummy' , Inc_metta ),
  fail, !.

start_analyse( _, _, _Tp, _MdfKeyaft, _Is_mk_update , _ ):-  !.


wri_rust( Lx , Linum , F ):-
   rust_detect_tag( Tag_from, Tag_til , Second_tag_begin, Second_tag_end ),
   sub_str_between( Lx, Tag_from, Tag_til, Subz10 ),
%   replace
   str_code_remove( Subz10, 92,  Subz1 ),
%     is_metta_function(
   is_metta_function( Subz1 ),
   sub_str_between( Lx, Second_tag_begin, Second_tag_end, Subz2 ),

   assert_eval_tag( 'rust', Linum, Subz1, F, Lx  , 0),
%   not( eval_tag( _, Subz1, _ ) ),  assert( eval_tag( Linum, Subz1, F ) ),
   asserta( found_search_result( 'rust', Subz1 , 0, Linum, F, Lx  ) ),
   write( Subz1 ), nl,
   not( eval_tag_secondary( _, Subz2 ) ),  assert( eval_tag_secondary( Subz1, Subz2 ) ), ! .

%wri_rust( Lx , _, F ):-
% sub_string( Lx , _, _, _ , "regex" ), !, write("is_line-"), write( Lx ), nl .


wri_rust( _Lx, _ , _ ):- !.

% end_of
read_rust_stream( Sea  , N, Fnx ):-
 read_line_to_string( Sea, Lx ), Lx \= end_of_file, !,
 % write( Lx ), nl,
 wri_rust( Lx , N , Fnx ),  N2 is N + 1,
 read_rust_stream( Sea , N2 , Fnx).
read_rust_stream( _Sea  , _ ,_):- !.

% '../../../hyperon-experimental/lib/src/metta/runner/stdlib.rs'
read_rustfile_tags( Fnx ):-
 retractall( eval_tag( _, _, _, _ , _ , _ ) ),
 retractall( eval_tag_secondary( _, _ ) ),

 open( Fnx , read, Sea , [ encoding(octet) ] ),
 read_rust_stream( Sea, 1, Fnx ),
 close( Sea ).




