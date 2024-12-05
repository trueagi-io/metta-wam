
:- ensure_loaded('/home/deb12user/metta-wam-royward-dev/src/canary/metta_runtime').

% Begin transpiler output

% [[prolog_if,[[assign,_21680,[]],[assign,_21382,[call(==),$VAR(x),_21680]],[native(is_True),_21382]],[[assign,_21120,$VAR(y)]],[[assign,$VAR(h),[call(car-atom),$VAR(x)                                                                                                                             ]],[assign,$VAR(t),[call(cdr-atom),$VAR(x)]],[assign,$VAR(rest),[call(append),$VAR(t),$VAR(y)]],[assign,_21928,[call(cons-atom),$VAR(h),$VAR(rest)]],[assign,_21120,_2                                                                                                                             1928]]]]:- dynamic mc__append/3.
:- multifile mc__append/3.

mc__append(A, B, C) :-
    (   D=[],
        'mc__=='(A, D, E),
        is_True(E)
    *-> C=B
    ;   'mc__car-atom'(A, F),
        'mc__cdr-atom'(A, G),
        mc__append(G, B, H),
        'mc__cons-atom'(F, H, I),
        C=I
    ).

/*
:- dynamic metta_compiled_predicate/3.
:- multifile metta_compiled_predicate/3.
:- module_transparent metta_compiled_predicate/3.

metta_compiled_predicate('&self', mc__append, 3).
*/

% [[prolog_if,[[assign,_45730,[call(==),$VAR(n),0]],[native(is_True),_45730]],[[assign,_45982,[call(car-atom),$VAR(x)]],[assign,_45468,_45982]],[[assign,$VAR(rest),[cal                                                                                                                             l(cdr-atom),$VAR(x)]],[assign,$VAR(nn),[call(-),$VAR(n),1]],[assign,_46132,[call(nth),$VAR(nn),$VAR(rest)]],[assign,_45468,_46132]]]]:- dynamic mc__nth/3.
:- multifile mc__nth/3.

mc__nth(A, B, C) :-
    (   'mc__=='(A, 0, D),
        is_True(D)
    *-> 'mc__car-atom'(B, E),
        C=E
    ;   'mc__cdr-atom'(B, F),
        'mc__-'(A, 1, G),
        mc__nth(G, F, H),
        C=H
    ).

/*
:- dynamic metta_compiled_predicate/3.
:- multifile metta_compiled_predicate/3.
:- module_transparent metta_compiled_predicate/3.

metta_compiled_predicate('&self', mc__nth, 3).
metta_compiled_predicate('&self', mc__append, 3).
*/

% [[prolog_if,[[assign,_64706,[]],[assign,_64538,[call(==),$VAR(format-chars),_64706]],[native(is_True),_64538]],[[assign,_64906,[]],[assign,_64276,_64906]],[[assign,$V                                                                                                                             AR(first),[call(car-atom),$VAR(format-chars)]],[assign,$VAR(rest),[call(cdr-atom),$VAR(format-chars)]],[prolog_if,[[assign,_66444,[call(==),$VAR(first),{]],[native(is                                                                                                                             _True),_66444]],[[prolog_if,[[assign,_66868,[]],[assign,_66700,[call(==),$VAR(rest),_66868]],[native(is_True),_66700]],[[assign,_66696,list([#\ {])]],[[prolog_if,[[as                                                                                                                             sign,_67192,[call(car-atom),$VAR(rest)]],[assign,_67080,[call(==),_67192,}]],[native(is_True),_67080]],[[assign,$VAR(arg),[call(nth),$VAR(n),$VAR(args)]],[assign,$VAR                                                                                                                             (restrest),[call(cdr-atom),$VAR(rest)]],[assign,$VAR(nn),[call(+),$VAR(n),1]],[assign,$VAR(argchars),[call(stringToChars),$VAR(arg)]],[assign,$VAR(rest2),[call(format                                                                                                                             -args-aux),$VAR(restrest),$VAR(args),$VAR(nn)]],[assign,_67452,[call(append),$VAR(argchars),$VAR(rest2)]],[assign,_67076,_67452]],[[assign,$VAR(rest2),[call(format-ar                                                                                                                             gs-aux),$VAR(rest),$VAR(args),$VAR(n)]],[assign,_72260,[call(cons-atom),$VAR(first),$VAR(rest2)]],[assign,_67076,_72260]]],[assign,_66696,_67076]]],[assign,_65008,_66                                                                                                                             696]],[[assign,$VAR(rest2),[call(format-args-aux),$VAR(rest),$VAR(args),$VAR(n)]],[assign,_73042,[call(cons-atom),$VAR(first),$VAR(rest2)]],[assign,_65008,_73042]]],[                                                                                                                             assign,_64276,_65008]]]]:- dynamic'mc__format-args-aux'/4.
:- multifile'mc__format-args-aux'/4.

'mc__format-args-aux'(A, B, C, D) :-
    (   E=[],
        'mc__=='(A, E, F),
        is_True(F)
    *-> G=[],
        D=G
    ;   'mc__car-atom'(A, H),
        'mc__cdr-atom'(A, I),
        (   'mc__=='(H, '{', J),
            is_True(J)
        *-> (   K=[],
                'mc__=='(I, K, L),
                is_True(L)
            *-> M=['{']
            ;   (   'mc__car-atom'(I, N),
                    'mc__=='(N, '}', O),
                    is_True(O)
                *-> mc__nth(C, B, P),
                    'mc__cdr-atom'(I, Q),
                    'mc__+'(C, 1, R),
                    mc__stringToChars(P, S),
                    'mc__format-args-aux'(Q, B, R, T),
                    mc__append(S, T, U),
                    V=U
                ;   'mc__format-args-aux'(I, B, C, T),
                    'mc__cons-atom'(H, T, W),
                    V=W
                ),
                M=V
            ),
            X=M
        ;   'mc__format-args-aux'(I, B, C, T),
            'mc__cons-atom'(H, T, Y),
            X=Y
        ),
        D=X
    ).

/*
:- dynamic metta_compiled_predicate/3.
:- multifile metta_compiled_predicate/3.
:- module_transparent metta_compiled_predicate/3.

metta_compiled_predicate('&self', 'mc__format-args-aux', 4).
metta_compiled_predicate('&self', mc__nth, 3).
metta_compiled_predicate('&self', mc__append, 3).

*/

% [[assign,$VAR(format-chars),[call(stringToChars),$VAR(format)]],[assign,$VAR(formatted),[call(format-args-aux),$VAR(format-chars),$VAR(args),0]],[assign,_105818,[call                                                                                                                             (charsToString),$VAR(formatted)]]]:- dynamic'mc__format-argsx'/3.
:- multifile'mc__format-argsx'/3.

'mc__format-argsx'(A, B, C) :-
    mc__stringToChars(A, D),
    'mc__format-args-aux'(D, B, 0, E),
    mc__charsToString(E, C).

:- dynamic metta_compiled_predicate/3.
:- multifile metta_compiled_predicate/3.
:- module_transparent metta_compiled_predicate/3.

metta_compiled_predicate('&self', 'mc__format-argsx', 3).
metta_compiled_predicate('&self', 'mc__format-args-aux', 4).
metta_compiled_predicate('&self', mc__nth, 3).
metta_compiled_predicate('&self', mc__append, 3).

% ;;; Tests

:- begin_metta_runtime.

:- do_metta_runtime(A,
    'mc__format-argsx'("{}ab", ["XX", "ZZ"], A)).

:- do_metta_runtime(A,
    mc__append([1, 2], [3, 5], A)).

:- do_metta_runtime(A,
    mc__nth(3, [10, 11, 12, 13, 14, 15], A)).

:- do_metta_runtime(A,
    'mc__format-argsx'("ab", ["XX", "ZZ"], A)).

:- do_metta_runtime(A,
    'mc__format-argsx'("{ab", ["XX", "ZZ"], A)).

:- do_metta_runtime(A,
    'mc__format-argsx'("a{b", ["XX", "ZZ"], A)).

:- do_metta_runtime(A,
    'mc__format-argsx'("{}a{b", ["XX", "ZZ"], A)).

:- do_metta_runtime(A,
    'mc__format-argsx'("{}a{}b", ["XX", "ZZ"], A)).

:- do_metta_runtime(A,
    'mc__format-argsx'("a{}b{}", ["XX", "ZZ"], A)).

:- end_metta_runtime.
