; [findall,X,[member,X,[1,2,3]],L].
(findall $x (member $x (a b c)) $L)
findall(X,member(X,a(b,c)),L)

(findall $x (member $x (:: a b c)) $L)
(@ findall $x (@ member $x (a b c)) $L)

 
!(pragma! denote-cons-list ::)
!(pragma! denote-term-list {})
!(pragma! denote-term-atom @)
!(pragma! term-op )


`{findall $x {member $x (a b c)} $L}` = `findall(_x,member(_x,[a,b,c],_L)`



(findall $x (member@ $x (a b c)) $L)



handle_doc_changes_d4(_, []) :- !.
handle_doc_changes_d4(Path, [Change|Changes]) :-
    handle_doc_change_d4(Path, Change),
    handle_doc_changes_d4(Path, Changes).


{:- {handle_doc_changes_d4 $_ ()} !}
{:- {handle_doc_changes_d4 Path (Change .|. Changes)}
    {handle_doc_change_d4 Path Change}
    {handle_doc_changes_d4 Path Changes}}