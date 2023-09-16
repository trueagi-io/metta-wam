:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).
catch_nolog(G):- ignore(catch(notrace(G),E,once(true;nop(u_dmsg(E=G))))).
catch_log(G):- ignore(catch(notrace(G),E,((u_dmsg(E=G),ftrace(G))))).
% catch_log(G):- ignore(catch(notrace(G),E,((writeln(E=G),catch_nolog(ds))))).

get_user_error(UE):- stream_property(UE,file_no(2)),!.
get_user_error(UE):- stream_property(UE,alias(user_error)),!.

ufmt(G):- fmt(G)->true;writeln(G).
u_dmsg(G):- is_list(G),!,my_maplist(u_dmsg,G).
u_dmsg(M):- get_user_error(UE), \+ current_predicate(with_toplevel_pp/2),!, with_output_to(UE,ufmt(M)).
u_dmsg(M):- get_user_error(UE),!, with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))).
u_dmsg(M):- get_user_error(UE),  stream_property(UO,file_no(1)), current_output(CO),!,
  (UO==CO ->  dmsg(M) ;
   (with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))), with_output_to(CO,pp(M)))).
u_dmsg(G):-ufmt(G),!.


