

predicates

save_all_data_file( string ) - (i)

save_all_data()

clean_all_data()
 read_all_data_from_file( string ) - (i)

read_data_line( string ) - (i)

clauses

 save_all_data_file( Fn ):- 
 openwrite( fileselector1, Fn),
 writedevice( fileselector1 ),
 save_all_data(), 
 closefile( fileselector1 ), writedevice(stdout).

repeateofnorm(F):- not(eof(F)). 
repeateofnorm(F):- not(eof(F)),repeateofnorm(F). 


save_all_data():- !.







clean_all_data():-
!.

read_all_data_from_file( Xfn ):- 
 clean_all_data(), 
 closefile(fileselector1), 
 openread(fileselector1, Xfn ), 
 readdevice(fileselector1), 
 repeateofnorm(fileselector1 ), 
 readln( Lin ), 
 read_data_line( Lin ), 
 fail,!. 
read_all_data_from_file( _ ):- !, 
 closefile(fileselector1),readdevice(stdin).   
