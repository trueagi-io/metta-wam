



pp_fb1_e(P):- 
        pp_fb2(fbdebug1, P).

pp_fb1_e(P):- 
        pp_fb2(fmt0(P)).


pp_fb2(F,P):- 
        atom(F), current_predicate(F/1), call(F, P).


pp_sax(V) :- 
        is_final_write(V), !.

pp_sax(S) :- 
        \+ allow_concepts, !, write_src(S).

pp_sax(S) :- 
        is_englishy(S), !, print_concept("StringValue", S).

pp_sax(S) :- 
        symbol_length(S, 1), symbol_string(S, SS), !, print_concept("StringValue", SS).

pp_sax(S) :- 
        is_an_arg_type(S, T), !, print_concept("TypeNode", T).

pp_sax(S) :- 
        has_type(S, T), !, format('(~wValueNode "~w")', [T, S]).

pp_sax(S) :- 
        sub_atom(S, 0, 4, Aft, FB), flybase_identifier(FB, Type), !,
    (Aft > 0 -> format('(~wValueNode "~w")', [Type, S]); format('(TypeNode "~w")', [Type])).

pp_sax(S) :- 
        print_concept("ConceptNode", S).



print_concept(_CType, V):- ignore(write_src(V)).


write_val(V):- 
        is_final_write(V), !.

write_val(V):- 
        number(V), !, write_src(V).

write_val(V):- 
        compound(V), !, write_src(V).

write_val(V):- 
        write('"'), write(V), write('"').


is_final_write(V):- 
        var(V), !, write_dvar(V), !.

is_final_write('$VAR'(S)):- 
        !, write_dvar(S), !.

is_final_write('#\\'(S)):- 
        !, format("'~w'", [S]).

is_final_write(V):- 
        py_is_enabled, py_is_py(V), !, py_ppp(V), !.

is_final_write([VAR, V|T]):- 
        '$VAR' == VAR, T == [], !, write_dvar(V).

is_final_write('[|]'):- 
        write('Cons'), !.

is_final_write([]):- 
        !, write('()').




write_dvar(S):- 
        S == '_', !, write_dname(S).

write_dvar(S):- 
        S == '__', !, write('$').

write_dvar(S):- 
        var(S), get_var_name(S, N), write_dname(N), !.

write_dvar(S):- 
        var(S), !, format('$~p', [S]).

write_dvar(S):- 
        atom(S), symbol_concat('_', N, S), write_dname(N).

write_dvar(S):- 
        string(S), symbol_concat('_', N, S), write_dname(N).



write_dvar(S):- 
        write_dname(S).


write_dname(S):- 
        write('$'), write(S).


pp_as(V):- 
        \+ \+ pp_sex(V), flush_output.


pp_sex_nc(V):- 
        with_no_quoting_symbols(true, pp_sex(V)), !.


unlooped_fbug(Mesg):- 
        fbug_message_hook(fbug_message_hook, fbug(Mesg)).


into_hyphens(D, U):- 
        atom(D), !, always_dash_functor(D, U).

into_hyphens(D, U):- 
        descend_and_transform(into_hyphens, D, U), !.


unlooped_fbug(W, Mesg):- 
        nb_current(W, true), !, print(Mesg), nl, bt, break.

unlooped_fbug(W, Mesg):- 
        setup_call_cleanup(nb_setval(W, true), once(Mesg), nb_setval(W, false)), nb_setval(W, false).

:- dynamic(py_is_enabled/0). 

py_is_enabled:- 
        predicate_property(py_ppp(_), defined), asserta((py_is_enabled:- !)).


write_src(V):- 
        \+ \+ notrace((guess_metta_vars(V), pp_sex(V))), !.


write_src_woi(Term):- 
        notrace((with_indents(false, write_src(Term)))).


write_src_woi_nl(X):- 
        \+ \+ notrace((guess_metta_vars(X), format('~N'), write_src_woi(X), format('~N'))).


pp_sex(V):- 
        pp_sexi(V), !.