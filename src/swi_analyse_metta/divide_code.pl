
% :- encoding(utf8).
:- encoding( octet ).

warning_bad_comment( Base, Lx ):- sub_string( Lx, _,_,_, "/*" ), !,
 write("Bad comment /* in file "), write( Base ), 
 write(" Please clean the file first "), nl .

warning_bad_comment( _, _Lx ):- !.

%   :- use_module(library(dcg/basics)).
is_blank_line( Lastline ):- string_length( Lastline , Le ), Le < 2, ! .

% ADD just that it divides   with a blank line only 
% in this way its regular 

add_to_file_part( Base, Lx ):-  file_part_string( Num, Base, Bstr , _Lastline ),
  string_length( Bstr, Le ), Le < 2900, 
  string_concat( Bstr , "\n", C1 ),  string_concat( C1, Lx, C2 ),
  retract( file_part_string( Num, Base, _ , _ ) ), !,
%  write( Base ), write( Num ), nl,
  asserta( file_part_string( Num, Base, C2 , Lx ) ).

add_to_file_part( Base, Lx ):-  
  file_part_string( Num, Base, _, _Lastline ), !,
  Num2 is Num + 1,
%  write( Base ), write( Num2 ), nl,
  asserta( file_part_string( Num2, Base, Lx , Lx ) ).

add_to_file_part( Base, Lx ):-  !,  
%  write( Base ), write( 1 ), nl,
  asserta( file_part_string( 1, Base, Lx , Lx ) ).



%add_to_file_part( Base, Lx ):-  file_part_string( Num, Base, Bstr , _Lastline ),
%  string_concat( Bstr , "\n", C1 ),  string_concat( C1, Lx, C2 ),
%  retract( file_part_string( Num, Base, _ , _ ) ), !,
%  write( Base ), write( Num ), nl,
%  Num2 is Num + 1,
%  asserta( file_part_string( Num2, Base, C2 , Lx ) ).



read_file_stream_to_memory( Sea, Base, Linum ):- not( at_end_of_stream( Sea ) ),
 read_line_to_string( Sea, Lx ), Lx \= end_of_file,  !,     
% warning_bad_comment( Base, Lx ),
 add_to_file_part( Base, Lx ), 
 Linum2 is Linum + 1,
 read_file_stream_to_memory( Sea, Base, Linum2 ).

%read_file_stream_to_memory( Sea, Base, Linum ):- not( at_end_of_stream( Sea ) ), !, 
%  Linum2 is Linum + 1, read_has_search_stream_to_memory( Sea, Base, Linum2 ).
read_file_stream_to_memory( _ , _, _ ):- !.

% To be able to post them in ChatGPT

% for testing metta_eval with 1 file, is not in my task list ChatGPT

file_to_divide( '../canary_to_post/metta_eval.pl' ).

%file_to_divide( '../canary_to_post/metta_convert.pl' ).
%file_to_divide( '../canary_to_post/metta_ontology.pfc.pl' ).
%file_to_divide( '../canary_to_post/metta_pfc_base.pl' ).
%file_to_divide( '../canary_to_post/metta_printer.pl' ).
%file_to_divide( '../canary_to_post/metta_space.pl' ).
%file_to_divide( '../canary_to_post/metta_utils.pl' ).

%file_to_divide( '../canary_to_post/das_classic_loader.pl' ).
%file_to_divide( '../canary_to_post/chado_xml_loader.pl' ).
%file_to_divide( '../canary_to_post/flybase_convert.pl' ).



% THIS file Doesnt Exist
% file_to_divide( '../../library/genome/ext_loader_json.pl' ).

write_string_to_file( Fn, Str ):-  tell( Fn ), write( Str ),!, told().
write_string_to_file( _, _ ):- !.


ato_num2( Ato, Ato2 ):- atom_length( Ato, Le ), Le == 1, atom_concat( '0', Ato, Ato2 ), !.
ato_num2( Ato, Ato ):- !.


divide_file( Fn ):-
 file_base_name( Fn , Base ), file_name_extension( Base2 , _Ext, Base ),
 open( Fn, read, Sea , [ encoding( octet ) ] ), 
 read_file_stream_to_memory( Sea,  Base2, 1 ),  
 close( Sea ).

memory_to_divided_files():-
  file_part_string( Num2, Base, Lx , _ ),
  atom_number( Ato, Num2 ), ato_num2( Ato, Ato2 ), 
  atomic_list_concat( [ 'divided/', Base, '_', Ato2 , '.pld' ], Fnabs ),
  write_string_to_file( Fnabs , Lx ), fail , ! .

memory_to_divided_files():- !.


:- dynamic file_part_string/4.

% divide_prolog_source_files().

divide_prolog_source_files():-
 retractall( file_part_string( _, _, _ , _) ), 
 file_to_divide( Fn ), write( Fn ), nl,  divide_file( Fn ), fail, !.
divide_prolog_source_files():-  memory_to_divided_files(), !. 
divide_prolog_source_files():- !. 

:- dynamic tmp_atom/1.

file_list_divided_gpt( Base , Lis ):-  retractall( tmp_atom( _ ) ),
  directory_files( 'divided_gpt' , Lis ), member( El, Lis ), El \= '..', El \= '.',
  sub_atom( El, _, _ , _ , Base ),
  sub_atom( El, _, _ , _ , '_gpt' ),
  assert( tmp_atom( El ) ), fail.

file_list_divided_gpt( _Base , Fnl2 ):- findall( Fn, tmp_atom( Fn ) , Fnl ),
 sort( Fnl, Fnl2 ).


%---
assemble_files( Fnq, Lis ):-  length( Lis , Le ), Le > 0,
  tell( Fnq ),
  member( El, Lis ), 
  file_name_extension( Elbase , _, El ),
  atomic_list_concat( [ 'divided_gpt/', Elbase, '.pl' ] , Fnz ),
%  write( " read file " ), write( Fnz ), nl,
  read_file_to_string( Fnz, Bstr, [] ), 
  write( Bstr ), nl ,
  fail, !.
assemble_files( _ , _Lis ):-  told(), !.

%----
assemble_prolog_source_files():-
  write( "start" ), nl,
  file_to_divide( Fn ), 
  write( Fn ), nl ,  
  file_base_name( Fn , Base ), file_name_extension( Base2 , _Ext, Base ),
  write( Base2 ), nl ,  
  file_list_divided_gpt( Base2, Lis ) ,
  write_term( Lis, [] ), nl,
  write( " assembling " ), write( Base2 ), nl,
  atomic_list_concat( ['catted/', Base2, '.pl' ] , Fnq ),
  assemble_files( Fnq, Lis ) , 
  fail , !.
  
assemble_prolog_source_files():- !. 
assemble_prolog_source_files():- !. 


% read_string(+Stream, ?Length, -String)


read_clauses_stream_to_memory( Sea , Fn  ):-
  not( at_end_of_stream( Sea ) ), 
  catch(  read_term( Sea, Terx, [ term_position( Pos ) ] ),  _, fail ),  !,
  stream_position_data( byte_count, Pos, ChaCoufpos ),
  stream_property( Sea, position( Kpos ) ), stream_position_data( byte_count, Kpos, ChaCountEnd ),
  term_string( Terx, Str, [] ),
  assert( clauses_positions( ChaCoufpos, ChaCountEnd, Fn, Str ) ),
read_clauses_stream_to_memory( Sea , Fn  ).

read_clauses_stream_to_memory( _Sea  , _ ):- !.

% read_clauses().

display_mem_x():-
  retract( clauses_positions( ChaCoufpos, ChaCountEnd, Fn, Str ) ),
  write_term( clauses_positions( ChaCoufpos, ChaCountEnd, Fn, Str ) , [] ), nl,
  fail, !.
display_mem_x():- !.


:- dynamic clauses_positions/4.
read_file():-
 retractall( clauses_positions( _, _, _, _ ) ),
 % Fn = 'data/test_read_prolog.pro',
 Fn = '../canary_to_post/metta_eval.pl',
 open( Fn , read, Sea , [ encoding(octet) ] ), 
 read_clauses_stream_to_memory( Sea  , Fn ),  
 close( Sea ),
 display_mem_x().




 %open( From, read, Sea , [ encoding(octet) ] ), 
 %read_file_stream( Sea,  1 ),  
 %close( Sea ).
% [divide_code].
% divide_prolog_source_files().

