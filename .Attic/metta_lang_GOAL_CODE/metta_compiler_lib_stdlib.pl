% To refresh this:
% remove everything below the `AUTO GENERATED ...` line (or temporary uncomment the 'end_of_file')
% In a terminal, run:
%      mettalog --compile=full --log prolog/metta_lang/stdlib_mettalog.metta --repl
% Then while still in mettalog:
%      prolog.
%      listing(transpiler_predicate_store/7).
%
% Take the output of the listing, extract the `transpiler_predicate_store(user` parts, replace it by `transpiler_predicate_store(builtin` and paste below the line.
% Take the non-listing part of the output, and run it through
%      grep mc_ temp1.txt  | cut -d\( -f 1 | grep -v ' ' | sed 's/.*/listing(&),/' | grep -v '_if-decons-expr' | grep -v '_cdr-atom' | grep -v '_quote' | grep -v '_unique-atom' | sort | uniq
% to generate a set of prolog listing commands (with some already defined stuff removed). Replace the last comma by a dot.

% Back in the terminal, run these commands, save the results to another temp file, then remove the dynamic and multifile stuff:
%     cat temp2.txt | grep -v ':- dynamic' | grep -v ':- multifile'
% paste the output of this below

% TODO: Automate this process

:- multifile(transpiler_predicate_store/7).
%:- discontiguous transpiler_predicate_store/7.

% end_of_file.

% AUTO GENERATED BELOW THIS POINT. DIRECT EDITS MAY BE LOST

transpiler_predicate_store(builtin, 'MettaMorph-If', [2], todo, todo, [x(doeval, eager, [boolean]), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'MettaMorph-If', [3], todo, todo, [x(doeval, eager, [boolean]), x(noeval, lazy, []), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'if-unify-or-empty', [2], todo, todo, [x(noeval, lazy, []), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'min-atom', [1], todo, todo, [x(noeval, eager, [])], x(doeval, eager, [number])).
transpiler_predicate_store(builtin, 'pow-math', [2], todo, todo, [x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, id, [1], todo, todo, [x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'atom-subst', [3], todo, todo, [x(noeval, lazy, []), x(doeval, eager, ['Variable']), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'if-error', [3], todo, todo, [x(noeval, lazy, []), x(noeval, lazy, []), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'return-on-error', [2], todo, todo, [x(noeval, lazy, []), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, switch, [2], todo, todo, [x(doeval, eager, ['%Undefined%']), x(noeval, eager, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'is-function', [1], todo, todo, [x(doeval, eager, ['Type'])], x(doeval, eager, [boolean])).
transpiler_predicate_store(builtin, 'match-types', [4], todo, todo, [x(noeval, lazy, []), x(noeval, lazy, []), x(noeval, lazy, []), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'first-from-pair', [1], todo, todo, [x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'match-type-or', [3], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'filter-atom', [3], todo, todo, [x(noeval, eager, []), x(doeval, eager, ['Variable']), x(noeval, lazy, [])], x(noeval, eager, [])).
transpiler_predicate_store(builtin, 'map-atom', [3], todo, todo, [x(noeval, eager, []), x(doeval, eager, ['Variable']), x(noeval, lazy, [])], x(noeval, eager, [])).
transpiler_predicate_store(builtin, 'foldl-atom', [5], todo, todo, [x(noeval, eager, []), x(noeval, lazy, []), x(doeval, eager, ['Variable']), x(doeval, eager, ['Variable']), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'separate-errors', [2], todo, todo, [x(noeval, eager, []), x(noeval, eager, [])], x(noeval, eager, [])).
transpiler_predicate_store(builtin, 'check-alternatives', [1], todo, todo, [x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, interpret, [3], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'interpret-expression', [3], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'interpret-func', [4], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'interpret-args', [5], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'interpret-args-tail', [6], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'interpret-tuple', [2], todo, todo, [x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'metta-call', [3], todo, todo, [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'is-function-type', [1], todo, todo, [x(doeval, eager, ['Type'])], x(doeval, eager, [boolean])).
transpiler_predicate_store(builtin, 'add-reduct-minimal', [2], todo, todo, [x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'add-reduct', [2], todo, todo, [x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, unquote, [1], todo, todo, [x(doeval, eager, ['%Undefined%'])], x(doeval, eager, ['%Undefined%'])).
transpiler_predicate_store(builtin, 'get-doc', [1], todo, todo, [x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'metta-get-doc', [1], todo, todo, [x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'get-doc-single-atom', [1], todo, todo, [x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'get-doc-function', [2], todo, todo, [x(noeval, lazy, []), x(doeval, eager, ['Type'])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'undefined-doc-function-type', [1], todo, todo, [x(noeval, eager, [])], x(doeval, eager, ['Type'])).
transpiler_predicate_store(builtin, 'get-doc-params', [3], todo, todo, [x(noeval, eager, []), x(noeval, lazy, []), x(noeval, eager, [])], x(doeval, eager, [['Expression', 'Atom']])).
transpiler_predicate_store(builtin, 'get-doc-atom', [1], todo, todo, [x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'help!', [1], todo, todo, [x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'metta-help!', [1], todo, todo, [x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'metta-help!', [0], todo, todo, [], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'help-param!', [1], todo, todo, [x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'for-each-in-atom', [2], todo, todo, [x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
transpiler_predicate_store(builtin, 'noreduce-eq', [2], todo, todo, [x(noeval, lazy, []), x(noeval, lazy, [])], x(doeval, eager, [boolean])).
transpiler_predicate_store(builtin, 'get-type-space', [2], todo, todo, [x(doeval, eager, ['hyperon::space::DynSpace']), x(noeval, lazy, [])], x(noeval, lazy, [])).
transpiler_predicate_store(builtin, 'mod-space!', [1], todo, todo, [x(noeval, lazy, [])], x(doeval, eager, ['hyperon::space::DynSpace'])).
transpiler_predicate_store(builtin, 'union-atom', [2], todo, todo, [x(noeval, eager, []), x(noeval, eager, [])], x(noeval, eager, [])).
transpiler_predicate_store(builtin, 'intersection-atom', [2], todo, todo, [x(noeval, eager, []), x(noeval, eager, [])], x(noeval, eager, [])).
transpiler_predicate_store(builtin, 'subtraction-atom', [2], todo, todo, [x(noeval, eager, []), x(noeval, eager, [])], x(noeval, eager, [])).

'mc__1_0_metta-help!'(A) :-
    B=ispu(top),
    'mc__1_1_mod-space!'(B, C),
    D=C,
    E=['@desc', F],
    G=['@doc', H, E, _, _],
    I=ispuU('Empty', (transpiler_apply(mc__1_1_, H, [H, F], J, [F], [F], [x(noeval, eager, [])], [true], [true]), K=['format-args', "{}\n\t{}", J], 'mc__1_1_println!'(K, L), []=L)),
    M=ispu('Empty'),
    mc__1_4_unify(D, G, I, M, A).
'mc__1_0_metta-help!'(A) :-
    B=ispu(top),
    'mc__1_1_mod-space!'(B, C),
    D=C,
    E=['@desc', F],
    G=['@doc', H, E],
    I=ispuU('Empty', (transpiler_apply(mc__1_1_, H, [H, F], J, [F], [F], [x(noeval, eager, [])], [true], [true]), K=['format-args', "{}\n\t{}", J], 'mc__1_1_println!'(K, L), []=L)),
    M=ispu('Empty'),
    mc__1_4_unify(D, G, I, M, A).


'mc__1_1_check-alternatives'(A, B) :-
    catch(( C=['collapse-bind', A],
            D=ispuU(E, E=[[], []]),
            F=ispeEnNC(G, mc__1_1_eval(H, G), I, I=[eval, H], H=['separate-errors', J, K]),
            L=['foldl-atom', M, D, J, K, F],
            as_p1_expr(L, N),
            mc__1_1_eval(N, O),
            transpiler_apply(mc__1_1_,
                             P,
                             [P, Q],
                             R,
                             [Q],
                             [Q],
                             [x(noeval, eager, [])],
                             [true],
                             [true]),
            S=['if-equal', P, [], Q, P],
            mc__1_1_eval(S, T),
            U=['superpose-bind', V],
            throw(metta_return(W)),
            X=[chain, U, W, W],
            Y=[chain, T, V, X],
            'mc__1_1_check-alternatives'(A, Z),
            A1=['Error', Z, "list of results was not filtered correctly"],
            throw(metta_return(A1)),
            B1=['if-unify', C1, R, Y, A1],
            D1=[chain, O, C1, B1],
            B=[chain, C, M, D1]
          ),
          metta_return(E1),
          E1=B).


'mc__1_1_first-from-pair'(A, B) :-
    catch(( transpiler_apply(mc__1_1_,
                             C,
                             [C, D],
                             E,
                             [D],
                             [D],
                             [x(noeval, eager, [])],
                             [true],
                             [true]),
            throw(metta_return(C)),
            'mc__1_1_first-from-pair'(A, F),
            G=['Error', F, "incorrect pair format"],
            throw(metta_return(G)),
            B=['if-unify', A, E, C, G]
          ),
          metta_return(H),
          H=B).


'mc__1_1_get-doc'(A, B) :-
    as_p1_expr(A, C),
    B=ispeEnN(D, ('mc__1_1_metta-get-doc'(C, E), as_p1_exec(E, D)), F, (G=['metta-get-doc', C], G=F)).


'mc__1_1_get-doc-atom'(A, B) :-
    as_p1_expr(A, C),
    B=ispeEnNC(D, mc__1_4_unify(E, F, G, H, D), I, I=[unify, E, F, G, H], (J=ispu(top), 'mc__1_1_mod-space!'(J, K), E=K, 'mc__1_2_get-type-space'(E, C, L), as_p1_exec(L, M), N=M, as_p1_exec(C, O), F=['@doc', O, P], G=ispeEnN(Q, (as_p1_exec(C, R), S=['@item', R], T=['@kind', atom], U=['@type', N], Q=['@doc-formal', S, T, U, P]), V, (as_p1_expr(C, W), X=['@item', W], Y=['@kind', atom], Z=['@type', N], V=['@doc-formal', X, Y, Z, P])), H=ispeEnNC(A1, mc__1_4_unify(E, B1, C1, D1, A1), E1, E1=[unify, E, B1, C1, D1], (as_p1_exec(C, F1), G1=['@params', _], B1=['@doc', F1, _, G1, _], C1=ispeEnN(H1, ('mc__1_2_get-doc-function'(C, '%Undefined%', I1), as_p1_exec(I1, H1)), J1, (K1=['get-doc-function', C, '%Undefined%'], K1=J1)), D1=ispeEnN(L1, (as_p1_exec(C, M1), N1=['@item', M1], O1=['@kind', atom], P1=['@type', N], Q1=['@desc', "No documentation"], L1=['@doc-formal', N1, O1, P1, Q1]), R1, (as_p1_expr(C, S1), T1=['@item', S1], U1=['@kind', atom], V1=['@type', N], W1=['@desc', "No documentation"], R1=['@doc-formal', T1, U1, V1, W1])))))).


'mc__1_1_get-doc-single-atom'(A, B) :-
    as_p1_expr(A, C),
    B=ispeEnNC(D, (('mc__1_1_is-function-type'(E, F), is_True(F)*->'mc__1_2_get-doc-function'(C, E, G), H=G;'mc__1_1_get-doc-atom'(C, I), H=I), as_p1_exec(H, D)), J, (('mc__1_1_is-function-type'(E, F), is_True(F)*->K=['get-doc-function', C, E], L=K;M=['get-doc-atom', C], L=M), L=J), (N=ispu(top), 'mc__1_1_mod-space!'(N, O), P=O, 'mc__1_2_get-type-space'(P, C, Q), as_p1_exec(Q, R), E=R)).


'mc__1_1_help!'(A, B) :-
    'mc__1_1_metta-help!'(A, B).


'mc__1_1_help-param!'(A, B) :-
    A=['@param', ['@type', C], ['@desc', D]],
    E=[type, C],
    F=[E, D],
    G=['format-args', "  {} {}", F],
    'mc__1_1_println!'(G, B).
'mc__1_1_help-param!'(A, B) :-
    A=['@param', ['@type', C], ['@desc', D]],
    E=[type, C],
    F=[E, D],
    G=['format-args', "  {} {}", F],
    'mc__1_1_println!'(G, B).


mc__1_1_id(A, B) :-
    as_p1_expr(A, B).


'mc__1_1_is-function'(A, B) :-
    catch(( C=['get-metatype', A],
            mc__1_1_eval(C, D),
            transpiler_apply(mc__1_1_,
                             A,
                             [A, E],
                             F,
                             [E],
                             [E],
                             [x(noeval, eager, [])],
                             [true],
                             [true]),
            (   A=[G|H],
                G=I,
                H=_
            *-> throw(metta_return('True')),
                throw(metta_return('False')),
                J=['if-unify', I, ->, 'True', 'False'],
                K=J
            ;   L=['is-function', A],
                M=['Error', L, "is-function non-empty expression as an argument"],
                throw(metta_return(M)),
                K=M
            ),
            mc__1_1_eval(K, N),
            throw(metta_return('False')),
            transpiler_apply(mc__2_1_1_,
                             A,
                             [[A, E], 'False'],
                             O,
                             [E, 'False'],
                             [E, 'False'],
                             [x(noeval, eager, []), x(noeval, eager, [])],
                             [true, throw(metta_return('False'))],
                             [true, throw(metta_return('False'))]),
            P=[[[A, 'Expression'], N], O],
            Q=[switch, F, P],
            as_p1_expr(Q, R),
            mc__1_1_eval(R, S),
            B=[chain, D, E, S]
          ),
          metta_return(T),
          T=B).


'mc__1_1_is-function-type'(A, B) :-
    'mc__1_1_get-metatype'(A, C),
    D=C,
    (   true
    *-> E=D
    ;   E='Empty'
    ),
    (   E='Expression'
    *-> 'mc__1_1_car-atom'(A, F),
        G=F,
        (   'mc__1_2_=='(G, ->, H),
            is_True(H)
        *-> I='True'
        ;   I='False'
        ),
        B=I
    ;   (   true
        *-> J='False'
        ;   K='Empty',
            J=K
        ),
        B=J
    ).
'mc__1_1_is-function-type'(A, B) :-
    'mc__1_1_get-metatype'(A, C),
    D=C,
    (   true
    *-> E=D
    ;   E='Empty'
    ),
    (   E='Expression'
    *-> 'mc__1_1_car-atom'(A, F),
        G=F,
        (   'mc__1_2_=='(G, ->, H),
            is_True(H)
        *-> I='True'
        ;   I='False'
        ),
        B=I
    ;   (   true
        *-> J='False'
        ;   K='Empty',
            J=K
        ),
        B=J
    ).


'mc__1_1_metta-get-doc'(A, B) :-
    as_p1_expr(A, C),
    B=ispeEnNC(D, ((E='Expression'*->F=ispeEnN(G, ('mc__1_1_get-doc-atom'(C, H), as_p1_exec(H, G)), I, (J=['get-doc-atom', C], J=I)), K=F;L=ispeEnN(M, ((E=N*->O=ispeEnN(P, ('mc__1_1_get-doc-single-atom'(C, Q), as_p1_exec(Q, P)), R, (S=['get-doc-single-atom', C], S=R)), T=O;U=ispuU(V, V='Empty'), T=U), as_p1_exec(T, M)), W, ((E=N*->S=['get-doc-single-atom', C], T=S;V='Empty', T=V), T=W)), K=L), as_p1_exec(K, D)), X, ((E='Expression'*->J=['get-doc-atom', C], K=J;(E=N*->S=['get-doc-single-atom', C], T=S;V='Empty', T=V), K=T), K=X), (as_p1_expr(C, Y), 'mc__1_1_get-metatype'(Y, Z), A1=Z, (true*->E=A1;E='Empty'))).


'mc__1_1_metta-help!'(A, B) :-
    (   C=ispu(A),
        'mc__1_1_get-doc'(C, D),
        as_p1_exec(D, E)
    *-> F=E
    ;   F='Empty'
    ),
    G=['@item', H],
    I=['@kind', function],
    J=['@type', K],
    L=['@desc', M],
    N=['@params', O],
    P=['@type', Q],
    R=['@desc', S],
    T=['@return', P, R],
    U=['@doc-formal', G, I, J, L, N, T],
    (   F=U
    *-> transpiler_apply(mc__1_2_,
                         H,
                         [H, K, M],
                         V,
                         [K, M],
                         [K, M],
                         [x(noeval, eager, []), x(noeval, eager, [])],
                         [true, true],
                         [true, true]),
        W=['format-args', "Function {}: {} {}", V],
        'mc__1_1_println!'(W, X),
        X=[],
        Y=['format-args', "Parameters:", []],
        'mc__1_1_println!'(Y, Z),
        Z=[],
        'mc__1_2_for-each-in-atom'(O, 'help-param!', A1),
        A1=[],
        transpiler_apply(mc__1_1_,
                         Q,
                         [Q, S],
                         B1,
                         [S],
                         [S],
                         [x(noeval, eager, [])],
                         [true],
                         [true]),
        C1=['format-args', "Return: (type {}) {}", B1],
        'mc__1_1_println!'(C1, D1),
        D1=[],
        B=[]
    ;   E1=['@item', H],
        F1=['@kind', function],
        G1=['@type', K],
        H1=['@desc', M],
        I1=['@doc-formal', E1, F1, G1, H1],
        (   F=I1
        *-> transpiler_apply(mc__1_2_,
                             H,
                             [H, K, M],
                             J1,
                             [K, M],
                             [K, M],
                             [x(noeval, eager, []), x(noeval, eager, [])],
                             [true, true],
                             [true, true]),
            K1=['format-args', "Function {} (type {}) {}", J1],
            'mc__1_1_println!'(K1, L1),
            L1=[],
            M1=[]
        ;   N1=['@item', H],
            O1=['@kind', atom],
            P1=['@type', K],
            Q1=['@desc', M],
            R1=['@doc-formal', N1, O1, P1, Q1],
            (   F=R1
            *-> transpiler_apply(mc__1_2_,
                                 H,
                                 [H, K, M],
                                 S1,
                                 [K, M],
                                 [K, M],
                                 [x(noeval, eager, []), x(noeval, eager, [])],
                                 [true, true],
                                 [true, true]),
                T1=['format-args', "Atom {}: {} {}", S1],
                'mc__1_1_println!'(T1, U1),
                U1=[],
                V1=[]
            ;   (   F=W1
                *-> X1=['Error', W1, "Cannot match @doc-formal structure"],
                    Y1=X1
                ;   Z1='Empty',
                    Y1=Z1
                ),
                V1=Y1
            ),
            M1=V1
        ),
        B=M1
    ).
'mc__1_1_metta-help!'(A, B) :-
    (   C=ispu(A),
        'mc__1_1_get-doc'(C, D),
        as_p1_exec(D, E)
    *-> F=E
    ;   F='Empty'
    ),
    G=['@item', H],
    I=['@kind', function],
    J=['@type', K],
    L=['@desc', M],
    N=['@params', O],
    P=['@type', Q],
    R=['@desc', S],
    T=['@return', P, R],
    U=['@doc-formal', G, I, J, L, N, T],
    (   F=U
    *-> transpiler_apply(mc__1_2_,
                         H,
                         [H, K, M],
                         V,
                         [K, M],
                         [K, M],
                         [x(noeval, eager, []), x(noeval, eager, [])],
                         [true, true],
                         [true, true]),
        W=['format-args', "Function {}: {} {}", V],
        'mc__1_1_println!'(W, X),
        X=[],
        Y=['format-args', "Parameters:", []],
        'mc__1_1_println!'(Y, Z),
        Z=[],
        'mc__1_2_for-each-in-atom'(O, 'help-param!', A1),
        A1=[],
        transpiler_apply(mc__1_1_,
                         Q,
                         [Q, S],
                         B1,
                         [S],
                         [S],
                         [x(noeval, eager, [])],
                         [true],
                         [true]),
        C1=['format-args', "Return: (type {}) {}", B1],
        'mc__1_1_println!'(C1, D1),
        D1=[],
        B=[]
    ;   E1=['@item', H],
        F1=['@kind', function],
        G1=['@type', K],
        H1=['@desc', M],
        I1=['@doc-formal', E1, F1, G1, H1],
        (   F=I1
        *-> transpiler_apply(mc__1_2_,
                             H,
                             [H, K, M],
                             J1,
                             [K, M],
                             [K, M],
                             [x(noeval, eager, []), x(noeval, eager, [])],
                             [true, true],
                             [true, true]),
            K1=['format-args', "Function {} (type {}) {}", J1],
            'mc__1_1_println!'(K1, L1),
            L1=[],
            M1=[]
        ;   N1=['@item', H],
            O1=['@kind', atom],
            P1=['@type', K],
            Q1=['@desc', M],
            R1=['@doc-formal', N1, O1, P1, Q1],
            (   F=R1
            *-> transpiler_apply(mc__1_2_,
                                 H,
                                 [H, K, M],
                                 S1,
                                 [K, M],
                                 [K, M],
                                 [x(noeval, eager, []), x(noeval, eager, [])],
                                 [true, true],
                                 [true, true]),
                T1=['format-args', "Atom {}: {} {}", S1],
                'mc__1_1_println!'(T1, U1),
                U1=[],
                V1=[]
            ;   (   F=W1
                *-> X1=['Error', W1, "Cannot match @doc-formal structure"],
                    Y1=X1
                ;   Z1='Empty',
                    Y1=Z1
                ),
                V1=Y1
            ),
            M1=V1
        ),
        B=M1
    ).


'mc__1_1_min-atom'(A, B) :-
    'mc_n_1__call-fn!'(min_list, [A], B).


'mc__1_1_mod-space!'(A, '&self') :-
    as_p1_expr(A, self).
'mc__1_1_mod-space!'(A, '&self') :-
    as_p1_expr(A, top).
'mc__1_1_mod-space!'(A, '&corelib') :-
    as_p1_expr(A, corelib).
'mc__1_1_mod-space!'(A, '&stdlib') :-
    as_p1_expr(A, stdlib).
'mc__1_1_mod-space!'(A, '&catalog') :-
    as_p1_expr(A, catalog).


'mc__1_1_undefined-doc-function-type'(A, B) :-
    (   'mc__1_2_=='([], A, C),
        is_True(C)
    *-> D=['%Undefined%'],
        B=D
    ;   'mc__1_1_cdr-atom'(A, E),
        F=E,
        'mc__1_1_undefined-doc-function-type'(F, G),
        H=G,
        'mc__1_2_cons-atom'('%Undefined%', H, I),
        B=I
    ).


mc__1_1_unquote([quote, A], A).


'mc__1_2_add-reduct'(A, B, C) :-
    'mc__1_2_add-atom'(A, B, C).


'mc__1_2_add-reduct-minimal'(A, B, C) :-
    'mc__1_2_add-atom'(A, B, C).


'mc__1_2_for-each-in-atom'(A, B, C) :-
    (   D=ispu(A),
        E=ispu([]),
        'mc__1_2_noreduce-eq'(D, E, F),
        is_True(F)
    *-> C=[]
    ;   'mc__1_1_car-atom'(A, G),
        H=G,
        'mc__1_1_cdr-atom'(A, I),
        J=I,
        transpiler_apply(mc__1_1_,
                         B,
                         [B, H],
                         _,
                         [H],
                         [H],
                         [x(noeval, eager, [])],
                         [true],
                         [true]),
        true,
        'mc__1_2_for-each-in-atom'(J, B, K),
        C=K
    ).
'mc__1_2_for-each-in-atom'(A, B, C) :-
    (   D=ispu(A),
        E=ispu([]),
        'mc__1_2_noreduce-eq'(D, E, F),
        is_True(F)
    *-> C=[]
    ;   'mc__1_1_car-atom'(A, G),
        H=G,
        'mc__1_1_cdr-atom'(A, I),
        J=I,
        transpiler_apply(mc__1_1_,
                         B,
                         [B, H],
                         _,
                         [H],
                         [H],
                         [x(noeval, eager, [])],
                         [true],
                         [true]),
        true,
        'mc__1_2_for-each-in-atom'(J, B, K),
        C=K
    ).


'mc__1_2_get-doc-function'(A, B, C) :-
    as_p1_expr(A, D),
    C=ispeEnNC(E, mc__1_4_unify(F, G, H, I, E), J, J=[unify, F, G, H, I], (K=ispu(top), 'mc__1_1_mod-space!'(K, L), F=L, as_p1_exec(D, M), N=['@params', O], G=['@doc', M, P, N, Q], H=ispeEnNC(R, (as_p1_exec(D, S), T=['@item', S], U=['@kind', function], V=['@type', B], W=['@params', X], R=['@doc-formal', T, U, V, P, W, Y]), Z, (as_p1_expr(D, A1), B1=['@item', A1], C1=['@kind', function], D1=['@type', B], E1=['@params', X], Z=['@doc-formal', B1, C1, D1, P, E1, Y]), (('mc__1_2_=='(B, '%Undefined%', F1), is_True(F1)*->'mc__1_1_undefined-doc-function-type'(O, G1), H1=G1;'mc__1_1_cdr-atom'(B, I1), H1=I1), J1=H1, K1=ispu(Q), 'mc__1_3_get-doc-params'(O, K1, J1, L1), [X, Y]=L1)), I=ispeEnN(M1, (as_p1_exec(D, N1), O1=['@item', N1], P1=['@kind', function], Q1=['@type', B], R1=['@desc', "No documentation"], M1=['@doc-formal', O1, P1, Q1, R1]), S1, (as_p1_expr(D, T1), U1=['@item', T1], V1=['@kind', function], W1=['@type', B], X1=['@desc', "No documentation"], S1=['@doc-formal', U1, V1, W1, X1])))).


'mc__1_2_get-type-space'(A, B, C) :-
    as_p1_expr(B, D),
    C=ispeEnN(E, (as_p1_exec(D, F), E=['get-type', F, A]), G, (as_p1_expr(D, H), G=['get-type', H, A])).


'mc__1_2_if-unify-or-empty'(A, B, C) :-
    as_p1_expr(A, D),
    as_p1_expr(B, D),
    C=ispu(unified).
'mc__1_2_if-unify-or-empty'(A, B, C) :-
    as_p1_expr(A, _),
    as_p1_expr(B, _),
    C=ispeEnN(D, mc__1_0_empty(D), E, E=[empty]).


'mc__1_2_interpret-tuple'(A, B, C) :-
    catch(( throw(metta_return(A)),
            D=[interpret, E, '%Undefined%', B],
            F=[eval, D],
            throw(metta_return('Empty')),
            G=['interpret-tuple', H, B],
            I=[eval, G],
            throw(metta_return('Empty')),
            J=['cons-atom', K, L],
            throw(metta_return(M)),
            N=[chain, J, M, M],
            O=['if-equal', L, 'Empty', 'Empty', N],
            P=[eval, O],
            Q=[chain, I, L, P],
            R=['if-equal', K, 'Empty', 'Empty', Q],
            S=[eval, R],
            T=[chain, F, K, S],
            U=['interpret-tuple', A, B],
            V=['Error', U, "Non-empty expression atom is expected as an argument"],
            throw(metta_return(V)),
            W=['if-decons', A, E, H, T, V],
            mc__1_1_eval(W, X),
            C=['if-unify', A, [], A, X]
          ),
          metta_return(Y),
          Y=C).


'mc__1_2_intersection-atom'(A, B, C) :-
    D=ispeEnN(E, (mc__1_1_superpose(A, F), mc__1_1_superpose(B, G), E=[intersection, F, G]), H, (I=[superpose, A], J=[superpose, B], H=[intersection, I, J])),
    mc__1_1_collapse(D, C).


'mc__1_2_MettaMorph-If'('True', A, B) :-
    as_p1_expr(A, B).
'mc__1_2_MettaMorph-If'('False', A, B) :-
    as_p1_expr(A, _),
    B=ispuU(C, (C=0, C=1)).


'mc__1_2_noreduce-eq'(A, B, C) :-
    as_p1_expr(A, D),
    as_p1_expr(B, E),
    F=ispuU([quote, G], as_p1_expr(D, G)),
    H=F,
    as_p1_exec(H, I),
    J=ispuU([quote, K], as_p1_expr(E, K)),
    L=J,
    as_p1_exec(L, M),
    'mc__1_2_=='(I, M, C).


'mc__1_2_pow-math'(A, B, C) :-
    'mc_n_1__call-fn!'(pow, [A, B], C).


'mc__1_2_return-on-error'(A, B, C) :-
    as_p1_expr(A, D),
    as_p1_expr(B, E),
    C=ispeEnN(F, catch((as_p1_expr(D, G), throw(metta_return('Empty')), throw(metta_return('Empty')), H=ispeEnNC(I, as_p1_exec(D, I), J, D=J, (throw(metta_return(D)), throw(metta_return(D)))), K=ispeEnNC(L, as_p1_exec(E, L), M, E=M, throw(metta_return(E))), N=['if-error', D, H, K], as_p1_expr(N, O), P=[eval, O], Q=['if-equal', G, 'Empty', 'Empty', P], mc__1_1_eval(Q, F)), metta_return(R), R=F), S, catch((as_p1_expr(D, G), throw(metta_return('Empty')), throw(metta_return('Empty')), H=ispeEnNC(I, as_p1_exec(D, I), J, D=J, (throw(metta_return(D)), throw(metta_return(D)))), K=ispeEnNC(L, as_p1_exec(E, L), M, E=M, throw(metta_return(E))), N=['if-error', D, H, K], as_p1_expr(N, O), P=[eval, O], Q=['if-equal', G, 'Empty', 'Empty', P], S=[eval, Q]), metta_return(T), T=S)).


'mc__1_2_separate-errors'(A, B, C) :-
    catch(( transpiler_apply(mc__1_1_,
                             D,
                             [D, E],
                             F,
                             [E],
                             [E],
                             [x(noeval, eager, [])],
                             [true],
                             [true]),
            transpiler_apply(mc__1_1_,
                             G,
                             [G, H],
                             I,
                             [H],
                             [H],
                             [x(noeval, eager, [])],
                             [true],
                             [true]),
            J=ispu(G),
            K=ispeEnN(L, ('mc__1_2_cons-atom'(B, E, M), transpiler_apply(mc__1_1_, D, [D, N], O, [N], [N], [x(noeval, eager, [])], [true], [true]), throw(metta_return(O)), L=[chain, M, N, O]), P, (Q=['cons-atom', B, E], R=[D, N], throw(metta_return(R)), P=[chain, Q, N, R])),
            S=ispeEnN(T, ('mc__1_2_cons-atom'(B, D, U), transpiler_apply(mc__1_1_, V, [V, E], W, [E], [E], [x(noeval, eager, [])], [true], [true]), throw(metta_return(W)), T=[chain, U, V, W]), X, (Y=['cons-atom', B, D], Z=[V, E], throw(metta_return(Z)), X=[chain, Y, V, Z])),
            A1=['if-error', J, K, S],
            as_p1_expr(A1, B1),
            mc__1_1_eval(B1, C1),
            throw(metta_return(A)),
            D1=['if-unify', B, I, C1, A],
            throw(metta_return(A)),
            C=['if-unify', A, F, D1, A]
          ),
          metta_return(E1),
          E1=C).


'mc__1_2_subtraction-atom'(A, B, C) :-
    D=ispeEnNC(E, mc__1_2_subtraction(F, G, E), H, H=[subtraction, F, G], (F=ispeEnN(I, mc__1_1_superpose(A, I), J, J=[superpose, A]), G=ispeEnN(K, mc__1_1_superpose(B, K), L, L=[superpose, B]))),
    mc__1_1_collapse(D, C).


mc__1_2_switch(A, [], B) :-
    B=ispuU(C, ((mc__1_1_eval(A, D)*->E=D;E='Empty'), C='Empty')).


'mc__1_2_union-atom'(A, B, C) :-
    D=ispeEnNC(E, mc__1_2_union(F, G, E), H, H=[union, F, G], (F=ispeEnN(I, mc__1_1_superpose(A, I), J, J=[superpose, A]), G=ispeEnN(K, mc__1_1_superpose(B, K), L, L=[superpose, B]))),
    mc__1_1_collapse(D, C).


'mc__1_3_atom-subst'(A, B, C, D) :-
    as_p1_expr(A, E),
    as_p1_expr(C, F),
    D=ispeEnN(G, catch((H=[id, E], as_p1_expr(H, I), mc__1_1_eval(I, J), throw(metta_return(F)), as_p1_exec(F, K), G=[chain, J, B, K]), metta_return(L), L=G), M, catch((H=[id, E], as_p1_expr(H, I), N=[eval, I], throw(metta_return(F)), as_p1_expr(F, O), M=[chain, N, B, O]), metta_return(P), P=M)).


'mc__1_3_filter-atom'(A, B, C, D) :-
    as_p1_expr(C, E),
    catch(( (   A=[F|G],
                F=H,
                G=I
            *-> J=['filter-atom', I, B, E],
                K=[eval, J],
                L=ispu(H),
                M=['atom-subst', L, B, E],
                as_p1_expr(M, N),
                O=[eval, N],
                (   is_True(P)
                *-> Q=['cons-atom', H, R],
                    throw(metta_return(S)),
                    T=[chain, Q, S, S],
                    U=T
                ;   throw(metta_return(R)),
                    U=R
                ),
                V=[eval, U],
                W=[chain, X, P, V],
                Y=[chain, O, X, W],
                Z=[chain, K, R, Y],
                A1=Z
            ;   throw(metta_return([])),
                A1=[]
            ),
            mc__1_1_eval(A1, D)
          ),
          metta_return(B1),
          B1=D).


'mc__1_3_get-doc-params'(A, B, C, D) :-
    as_p1_expr(B, E),
    'mc__1_1_car-atom'(C, F),
    G=F,
    'mc__1_1_cdr-atom'(C, H),
    I=H,
    (   'mc__1_2_=='([], A, J),
        is_True(J)
    *-> as_p1_exec(E, K),
        K=['@return', L],
        M=['@type', G],
        N=['@desc', L],
        O=['@return', M, N],
        P=[[], O],
        D=P
    ;   'mc__1_1_car-atom'(A, Q),
        Q=['@param', R],
        'mc__1_1_cdr-atom'(A, S),
        T=S,
        'mc__1_3_get-doc-params'(T, E, I, U),
        U=[V, W],
        X=['@type', G],
        Y=['@desc', R],
        Z=['@param', X, Y],
        'mc__1_2_cons-atom'(Z, V, A1),
        B1=A1,
        transpiler_apply(mc__1_1_,
                         B1,
                         [B1, W],
                         C1,
                         [W],
                         [W],
                         [x(noeval, eager, [])],
                         [true],
                         [true]),
        D=C1
    ).
'mc__1_3_get-doc-params'(A, B, C, D) :-
    as_p1_expr(B, E),
    'mc__1_1_car-atom'(C, F),
    G=F,
    'mc__1_1_cdr-atom'(C, H),
    I=H,
    (   'mc__1_2_=='([], A, J),
        is_True(J)
    *-> as_p1_exec(E, K),
        K=['@return', L],
        M=['@type', G],
        N=['@desc', L],
        O=['@return', M, N],
        P=[[], O],
        D=P
    ;   'mc__1_1_car-atom'(A, Q),
        Q=['@param', R],
        'mc__1_1_cdr-atom'(A, S),
        T=S,
        'mc__1_3_get-doc-params'(T, E, I, U),
        U=[V, W],
        X=['@type', G],
        Y=['@desc', R],
        Z=['@param', X, Y],
        'mc__1_2_cons-atom'(Z, V, A1),
        B1=A1,
        transpiler_apply(mc__1_1_,
                         B1,
                         [B1, W],
                         C1,
                         [W],
                         [W],
                         [x(noeval, eager, [])],
                         [true],
                         [true]),
        D=C1
    ).


'mc__1_3_if-error'(A, B, C, D) :-
    as_p1_expr(A, E),
    as_p1_expr(B, F),
    as_p1_expr(C, G),
    D=ispeEnN(H, catch((as_p1_expr(E, I), J=['get-metatype', I], mc__1_1_eval(J, K), as_p1_expr(E, L), throw(metta_return(G)), as_p1_expr(G, M), as_p1_expr(E, N), O=['decons-atom', N], P=[Q, R], throw(metta_return(F)), as_p1_expr(F, S), throw(metta_return(G)), as_p1_expr(G, T), U=['if-equal', Q, 'Error', S, T], V=[eval, U], throw(metta_return(G)), as_p1_expr(G, W), X=['if-unify', Y, P, V, W], Z=[chain, O, Y, X], A1=['if-equal', L, [], M, Z], B1=[eval, A1], throw(metta_return(G)), as_p1_expr(G, C1), D1=['if-equal', E1, 'Expression', B1, C1], mc__1_1_eval(D1, F1), H=[chain, K, E1, F1]), metta_return(G1), G1=H), H1, catch((as_p1_expr(E, I), J=['get-metatype', I], I1=[eval, J], as_p1_expr(E, L), throw(metta_return(G)), as_p1_expr(G, M), as_p1_expr(E, N), O=['decons-atom', N], P=[Q, R], throw(metta_return(F)), as_p1_expr(F, S), throw(metta_return(G)), as_p1_expr(G, T), U=['if-equal', Q, 'Error', S, T], V=[eval, U], throw(metta_return(G)), as_p1_expr(G, W), X=['if-unify', Y, P, V, W], Z=[chain, O, Y, X], A1=['if-equal', L, [], M, Z], B1=[eval, A1], throw(metta_return(G)), as_p1_expr(G, C1), D1=['if-equal', E1, 'Expression', B1, C1], J1=[eval, D1], H1=[chain, I1, E1, J1]), metta_return(K1), K1=H1)).


mc__1_3_interpret(A, B, C, D) :-
    catch(( E=['get-metatype', A],
            mc__1_1_eval(E, F),
            throw(metta_return(A)),
            throw(metta_return(A)),
            transpiler_apply(mc__1_1_,
                             B,
                             [B, G],
                             H,
                             [G],
                             [G],
                             [x(noeval, eager, [])],
                             [true],
                             [true]),
            throw(metta_return(A)),
            I=['type-cast', A, B, C],
            mc__1_1_eval(I, J),
            throw(metta_return(K)),
            L=[chain, J, K, K],
            transpiler_apply(mc__2_1_1_,
                             B,
                             [[B, 'Symbol'], L],
                             M,
                             ['Symbol', L],
                             ['Symbol', N],
                             [x(noeval, eager, []), x(noeval, eager, [])],
                             [ true,
                               (I=['type-cast', A, B, C], mc__1_1_eval(I, J), throw(metta_return(K)), L=[chain, J, K, K])
                             ],
                             [ true,
                               (I=['type-cast', A, B, C], O=[eval, I], throw(metta_return(K)), N=[chain, O, K, K])
                             ]),
            P=['type-cast', A, B, C],
            mc__1_1_eval(P, Q),
            throw(metta_return(K)),
            R=[chain, Q, K, K],
            transpiler_apply(mc__2_1_1_,
                             B,
                             [[B, 'Grounded'], R],
                             S,
                             ['Grounded', R],
                             ['Grounded', T],
                             [x(noeval, eager, []), x(noeval, eager, [])],
                             [ true,
                               (P=['type-cast', A, B, C], mc__1_1_eval(P, Q), throw(metta_return(K)), R=[chain, Q, K, K])
                             ],
                             [ true,
                               (P=['type-cast', A, B, C], U=[eval, P], throw(metta_return(K)), T=[chain, U, K, K])
                             ]),
            V=['interpret-expression', A, B, C],
            mc__1_1_eval(V, W),
            X=['check-alternatives', W],
            mc__1_1_eval(X, Y),
            throw(metta_return(K)),
            Z=[chain, Y, K, K],
            transpiler_apply(mc__2_1_1_,
                             B,
                             [[B, 'Expression'], Z],
                             A1,
                             ['Expression', Z],
                             ['Expression', B1],
                             [x(noeval, eager, []), x(noeval, eager, [])],
                             [ true,
                               (V=['interpret-expression', A, B, C], mc__1_1_eval(V, W), X=['check-alternatives', W], mc__1_1_eval(X, Y), throw(metta_return(K)), Z=[chain, Y, K, K])
                             ],
                             [ true,
                               (V=['interpret-expression', A, B, C], mc__1_1_eval(V, W), X=['check-alternatives', W], C1=[eval, X], throw(metta_return(K)), B1=[chain, C1, K, K])
                             ]),
            D1=[[[B, 'Variable'], A], M, S, A1],
            E1=[switch, H, D1],
            as_p1_expr(E1, F1),
            G1=[eval, F1],
            H1=['if-equal', B, G, A, G1],
            I1=[eval, H1],
            J1=['if-equal', B, 'Atom', A, I1],
            mc__1_1_eval(J1, K1),
            D=[chain, F, G, K1]
          ),
          metta_return(L1),
          L1=D).


'mc__1_3_interpret-expression'(A, B, C, D) :-
    catch(( E=['get-type', F, C],
            G=[eval, E],
            H=['is-function', I],
            J=[eval, H],
            K=['interpret-func', A, I, B, C],
            L=[eval, K],
            M=['metta-call', N, B, C],
            O=[eval, M],
            throw(metta_return(P)),
            Q=[chain, O, P, P],
            R=[chain, L, N, Q],
            S=['interpret-tuple', A, C],
            T=[eval, S],
            U=['metta-call', N, B, C],
            V=[eval, U],
            throw(metta_return(P)),
            W=[chain, V, P, P],
            X=[chain, T, N, W],
            Y=['if-unify', Z, 'True', R, X],
            A1=[chain, J, Z, Y],
            B1=[chain, G, I, A1],
            C1=['type-cast', A, B, C],
            D1=[eval, C1],
            throw(metta_return(P)),
            E1=[chain, D1, P, P],
            F1=['if-decons', A, F, _, B1, E1],
            mc__1_1_eval(F1, D)
          ),
          metta_return(G1),
          G1=D).


'mc__1_3_map-atom'(A, B, C, D) :-
    as_p1_expr(C, E),
    catch(( (   A=[F|G],
                F=H,
                G=I
            *-> J=['map-atom', I, B, E],
                K=[eval, J],
                L=ispu(H),
                M=['atom-subst', L, B, E],
                as_p1_expr(M, N),
                O=[eval, N],
                P=['cons-atom', Q, R],
                throw(metta_return(S)),
                T=[chain, P, S, S],
                U=[chain, V, Q, T],
                W=[chain, O, V, U],
                X=[chain, K, R, W],
                Y=X
            ;   throw(metta_return([])),
                Y=[]
            ),
            mc__1_1_eval(Y, D)
          ),
          metta_return(Z),
          Z=D).


'mc__1_3_match-type-or'(A, B, C, D) :-
    catch(( E=ispu(B),
            F=ispu(C),
            G=ispu('True'),
            H=ispu('False'),
            I=['match-types', E, F, G, H],
            as_p1_expr(I, J),
            mc__1_1_eval(J, K),
            L=[or, A, M],
            mc__1_1_eval(L, N),
            throw(metta_return(O)),
            P=[chain, N, O, O],
            D=[chain, K, M, P]
          ),
          metta_return(Q),
          Q=D).


'mc__1_3_metta-call'(A, B, C, D) :-
    catch(( E=ispu(A),
            F=ispuU(A, throw(metta_return(A))),
            G=ispeEnN(H, (mc__1_1_eval(A, I), throw(metta_return(A)), throw(metta_return('Empty')), J=ispu(K), L=ispuU(K, throw(metta_return(K))), M=ispeEnNC(N, (mc__1_1_eval(O, P), throw(metta_return(Q)), N=[chain, P, Q, Q]), R, (S=[eval, O], throw(metta_return(Q)), R=[chain, S, Q, Q]), O=[interpret, K, B, C]), T=['if-error', J, L, M], as_p1_expr(T, U), V=[eval, U], W=['if-equal', K, 'Empty', 'Empty', V], X=[eval, W], Y=['if-equal', K, 'NotReducible', A, X], mc__1_1_eval(Y, Z), H=[chain, I, K, Z]), A1, (B1=[eval, A], throw(metta_return(A)), throw(metta_return('Empty')), J=ispu(K), L=ispuU(K, throw(metta_return(K))), M=ispeEnNC(N, (mc__1_1_eval(O, P), throw(metta_return(Q)), N=[chain, P, Q, Q]), R, (S=[eval, O], throw(metta_return(Q)), R=[chain, S, Q, Q]), O=[interpret, K, B, C]), T=['if-error', J, L, M], as_p1_expr(T, U), V=[eval, U], W=['if-equal', K, 'Empty', 'Empty', V], X=[eval, W], Y=['if-equal', K, 'NotReducible', A, X], C1=[eval, Y], A1=[chain, B1, K, C1])),
            D1=['if-error', E, F, G],
            as_p1_expr(D1, E1),
            mc__1_1_eval(E1, D)
          ),
          metta_return(F1),
          F1=D).


'mc__1_3_MettaMorph-If'(A, B, C, D) :-
    as_p1_expr(B, E),
    as_p1_expr(C, F),
    D=ispeEnN(G, ((is_True(A)*->H=E;H=F), as_p1_exec(H, G)), I, ((is_True(A)*->J=E;J=F), J=I)).


'mc__1_4_interpret-func'(A, B, C, D, E) :-
    catch(( F=[interpret, G, B, D],
            H=[eval, F],
            I=ispu(J),
            K=ispeEnNC(L, mc__1_1_eval(M, L), N, N=[eval, M], (O=['interpret-args', A, P, Q, C, D], R=[eval, O], S=ispu(T), U=ispeEnN(V, ('mc__1_2_cons-atom'(J, T, W), throw(metta_return(X)), V=[chain, W, X, X]), Y, (Z=['cons-atom', J, T], throw(metta_return(X)), Y=[chain, Z, X, X])), A1=['return-on-error', S, U], as_p1_expr(A1, B1), C1=[eval, B1], D1=[chain, R, T, C1], E1=['Error', B, "Function type expected"], throw(metta_return(E1)), M=['if-decons', B, _, Q, D1, E1])),
            F1=['return-on-error', I, K],
            as_p1_expr(F1, G1),
            H1=[eval, G1],
            I1=[chain, H, J, H1],
            J1=['Error', A, "Non-empty expression atom is expected"],
            throw(metta_return(J1)),
            K1=['if-decons', A, G, P, I1, J1],
            mc__1_1_eval(K1, E)
          ),
          metta_return(L1),
          L1=E).


'mc__1_4_match-types'(A, B, C, D, E) :-
    as_p1_expr(A, F),
    as_p1_expr(B, G),
    as_p1_expr(C, H),
    as_p1_expr(D, I),
    E=ispeEnN(J, catch((as_p1_expr(F, K), throw(metta_return(H)), as_p1_expr(H, L), as_p1_expr(G, M), throw(metta_return(H)), as_p1_expr(H, N), as_p1_expr(F, O), throw(metta_return(H)), as_p1_expr(H, P), as_p1_expr(G, Q), throw(metta_return(H)), as_p1_expr(H, R), as_p1_expr(F, S), as_p1_expr(G, T), throw(metta_return(H)), as_p1_expr(H, U), throw(metta_return(I)), as_p1_expr(I, V), W=['if-unify', S, T, U, V], X=['if-equal', Q, 'Atom', R, W], Y=[eval, X], Z=['if-equal', O, 'Atom', P, Y], A1=[eval, Z], B1=['if-equal', M, '%Undefined%', N, A1], C1=[eval, B1], D1=['if-equal', K, '%Undefined%', L, C1], mc__1_1_eval(D1, J)), metta_return(E1), E1=J), F1, catch((as_p1_expr(F, K), throw(metta_return(H)), as_p1_expr(H, L), as_p1_expr(G, M), throw(metta_return(H)), as_p1_expr(H, N), as_p1_expr(F, O), throw(metta_return(H)), as_p1_expr(H, P), as_p1_expr(G, Q), throw(metta_return(H)), as_p1_expr(H, R), as_p1_expr(F, S), as_p1_expr(G, T), throw(metta_return(H)), as_p1_expr(H, U), throw(metta_return(I)), as_p1_expr(I, V), W=['if-unify', S, T, U, V], X=['if-equal', Q, 'Atom', R, W], Y=[eval, X], Z=['if-equal', O, 'Atom', P, Y], A1=[eval, Z], B1=['if-equal', M, '%Undefined%', N, A1], C1=[eval, B1], D1=['if-equal', K, '%Undefined%', L, C1], F1=[eval, D1]), metta_return(G1), G1=F1)).


'mc__1_5_foldl-atom'(A, B, C, D, E, F) :-
    as_p1_expr(B, G),
    as_p1_expr(E, H),
    F=ispeEnN(I, catch(((A=[J|K], J=L, K=M*->N=['atom-subst', G, C, H], as_p1_expr(N, O), P=[eval, O], Q=ispu(L), R=ispu(S), T=['atom-subst', Q, D, R], as_p1_expr(T, U), V=[eval, U], W=ispu(X), Y=['foldl-atom', M, W, C, D, H], as_p1_expr(Y, Z), A1=[eval, Z], throw(metta_return(B1)), C1=[chain, A1, B1, B1], D1=[chain, E1, X, C1], F1=[chain, V, E1, D1], G1=[chain, P, S, F1], H1=G1;throw(metta_return(G)), H1=G), as_p1_expr(H1, I1), mc__1_1_eval(I1, I)), metta_return(J1), J1=I), K1, catch(((A=[J|K], J=L, K=M*->N=['atom-subst', G, C, H], as_p1_expr(N, O), P=[eval, O], Q=ispu(L), R=ispu(S), T=['atom-subst', Q, D, R], as_p1_expr(T, U), V=[eval, U], W=ispu(X), Y=['foldl-atom', M, W, C, D, H], as_p1_expr(Y, Z), A1=[eval, Z], throw(metta_return(B1)), C1=[chain, A1, B1, B1], D1=[chain, E1, X, C1], F1=[chain, V, E1, D1], G1=[chain, P, S, F1], H1=G1;throw(metta_return(G)), H1=G), as_p1_expr(H1, I1), K1=[eval, I1]), metta_return(L1), L1=K1)).


'mc__1_5_interpret-args'(A, B, C, D, E, F) :-
    catch(( G=[==, [], H],
            I=[eval, G],
            (   is_True(J)
            *-> K=ispu(L),
                M=ispu(D),
                N=ispuU([], throw(metta_return([]))),
                O=ispuU(P, (P=['Error', A, 'BadType'], throw(metta_return(P)))),
                Q=['match-types', K, M, N, O],
                as_p1_expr(Q, R),
                S=[eval, R],
                T=S
            ;   U=['Error', A, 'BadType'],
                throw(metta_return(U)),
                T=U
            ),
            V=[eval, T],
            W=[chain, I, J, V],
            X=['Error', A, "Too many arguments"],
            throw(metta_return(X)),
            Y=['if-decons', C, L, H, W, X],
            mc__1_1_eval(Y, Z),
            A1=[interpret, B1, C1, E],
            D1=[eval, A1],
            E1=['interpret-args-tail', A, F1, G1, H1, D, E],
            I1=[eval, E1],
            throw(metta_return(J1)),
            K1=[chain, I1, J1, J1],
            L1=ispu(F1),
            M1=ispeEnNC(N1, (mc__1_1_eval(O1, P1), throw(metta_return(J1)), N1=[chain, P1, J1, J1]), Q1, (R1=[eval, O1], throw(metta_return(J1)), Q1=[chain, R1, J1, J1]), O1=['interpret-args-tail', A, F1, G1, H1, D, E]),
            S1=['return-on-error', L1, M1],
            as_p1_expr(S1, T1),
            U1=[eval, T1],
            V1=['if-equal', F1, B1, K1, U1],
            W1=[eval, V1],
            X1=[chain, D1, F1, W1],
            Y1=['Error', A, 'BadType'],
            throw(metta_return(Y1)),
            Z1=['if-decons', C, C1, H1, X1, Y1],
            A2=[eval, Z1],
            B2=['interpret-atom', A, B, C, E],
            C2=['Error', B2, "Non-empty expression atom is expected"],
            throw(metta_return(C2)),
            D2=['if-decons', B, B1, G1, A2, C2],
            mc__1_1_eval(D2, E2),
            F=['if-unify', B, [], Z, E2]
          ),
          metta_return(F2),
          F2=F).


'mc__1_6_interpret-args-tail'(A, B, C, D, E, F, G) :-
    catch(( H=['interpret-args', A, C, D, E, F],
            mc__1_1_eval(H, I),
            J=ispu(K),
            L=ispeEnN(M, ('mc__1_2_cons-atom'(B, K, N), throw(metta_return(O)), M=[chain, N, O, O]), P, (Q=['cons-atom', B, K], throw(metta_return(O)), P=[chain, Q, O, O])),
            R=['return-on-error', J, L],
            as_p1_expr(R, S),
            mc__1_1_eval(S, T),
            G=[chain, I, K, T]
          ),
          metta_return(U),
          U=G).



