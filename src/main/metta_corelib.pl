/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */


metta_atom_corelib( [=, ['car-atom', A], [eval, ['if-decons', A, B, _, B, ['Error', ['car-atom', A], "car-atom expects a non-empty expression as an argument"]]]]).
metta_atom_corelib( [=, ['cdr-atom', A], [eval, ['if-decons', A, _, B, B, ['Error', ['cdr-atom', A], "cdr-atom expects a non-empty expression as an argument"]]]]).
metta_atom_corelib( [=, ['filter-atom', A, B, C], [function, [eval, ['if-decons', A, D, E, [chain, [eval, ['filter-atom', E, B, C]], F, [chain, [eval, [apply, D, B, C]], G, [chain, G, H, [eval, [if, H, [chain, [cons, D, F], I, [return, I]], [return, F]]]]]], [return, []]]]]]).
metta_atom_corelib( [=, ['foldl-atom', A, B, C, D, E], [function, [eval, ['if-decons', A, F, G, [chain, [eval, [apply, B, C, E]], H, [chain, [eval, [apply, F, D, H]], I, [chain, I, J, [chain, [eval, ['foldl-atom', G, J, C, D, E]], K, [return, K]]]]], [return, B]]]]]).
metta_atom_corelib( [=, ['if-decons', A, B, C, D, E], [eval, ['if-non-empty-expression', A, [chain, [decons, A], F, [match, F, [B, C], D, E]], E]]]).
metta_atom_corelib( [=, ['if-decons', A, B, C, D, E], [function, [eval, ['if-non-empty-expression', A, [chain, [decons, A], F, [unify, F, [B, C], [return, D], [return, E]]], [return, E]]]]]).
metta_atom_corelib( [=, ['if-empty', A, B, C], [eval, ['if-equal', A, 'Empty', B, C]]]).
metta_atom_corelib( [=, ['if-empty', A, B, C], [function, [eval, ['if-equal', A, 'Empty', [return, B], [return, C]]]]]).
metta_atom_corelib( [=, ['if-error', A, B, C], [eval, ['if-decons', A, D, _, [eval, ['if-equal', D, 'Error', B, C]], C]]]).
metta_atom_corelib( [=, ['if-error', A, B, C], [function, [eval, ['if-decons', A, D, _, [eval, ['if-equal', D, 'Error', [return, B], [return, C]]], [return, C]]]]]).
metta_atom_corelib( [=, ['if-non-empty-expression', A, B, C], [chain, [eval, ['get-metatype', A]], D, [eval, ['if-equal', D, 'Expression', [eval, ['if-equal', A, [], C, B]], C]]]]).
metta_atom_corelib( [=, ['if-non-empty-expression', A, B, C], [function, [chain, [eval, ['get-metatype', A]], D, [eval, ['if-equal', D, 'Expression', [eval, ['if-equal', A, [], [return, C], [return, B]]], [return, C]]]]]]).
metta_atom_corelib( [=, ['if-not-reducible', A, B, C], [function, [eval, ['if-equal', A, 'NotReducible', [return, B], [return, C]]]]]).
metta_atom_corelib( [=, ['interpret-args', A, B, C, D, E], [function, [unify, B, [], [eval, ['if-decons', C, F, _, [eval, ['match-types', F, D, [return, []], [return, ['Error', A, 'BadType']]]], [return, ['Error', ['interpret-args', A, B, C, D, E], "interpret-args expects a non-empty value for $arg-types argument"]]]], [eval, ['if-decons', B, G, H, [eval, ['if-decons', C, I, J, [chain, [eval, [interpret, G, I, E]], K, [eval, ['if-equal', K, G, [chain, [eval, ['interpret-args-tail', A, K, H, J, D, E]], L, [return, L]], [eval, ['return-on-error', K, [chain, [eval, ['interpret-args-tail', A, K, H, J, D, E]], L, [return, L]]]]]]], [return, ['Error', A, 'BadType']]]], [return, ['Error', ['interpret-atom', A, B, C, E], "Non-empty expression atom is expected"]]]]]]]).
metta_atom_corelib( [=, ['interpret-args', A, B, C, D], [match, B, [], [match, C, [_], [], ['Error', A, 'BadType']], [eval, ['if-decons', B, E, F, [eval, ['if-decons', C, G, H, [chain, [eval, [interpret, E, G, D]], I, [eval, ['if-equal', I, E, [eval, ['interpret-args-tail', A, I, F, H, D]], [eval, ['return-on-error', I, [eval, ['interpret-args-tail', A, I, F, H, D]]]]]]], ['Error', A, 'BadType']]], ['Error', ['interpret-atom', A, B, C, D], "Non-empty expression atom is expected"]]]]]).
metta_atom_corelib( [=, ['interpret-args-tail', A, B, C, D, E, F], [function, [chain, [eval, ['interpret-args', A, C, D, E, F]], G, [eval, ['return-on-error', G, [chain, [cons, B, G], H, [return, H]]]]]]]).
metta_atom_corelib( [=, ['interpret-args-tail', A, B, C, D, E], [chain, [eval, ['interpret-args', A, C, D, E]], F, [eval, ['return-on-error', F, [cons, B, F]]]]]).
metta_atom_corelib( [=, ['interpret-expression', A, B, C], [eval, ['if-decons', A, D, _, [chain, [eval, ['get-type', D, C]], E, [chain, [eval, ['is-function', E]], F, [match, F, 'True', [chain, [eval, ['interpret-func', A, E, C]], G, [eval, [call, G, B, C]]], [chain, [eval, ['interpret-tuple', A, C]], G, [eval, [call, G, B, C]]]]]], [eval, ['type-cast', A, B, C]]]]]).
metta_atom_corelib( [=, ['interpret-expression', A, B, C], [function, [eval, ['if-decons', A, D, _, [chain, [eval, ['get-type', D, C]], E, [chain, [eval, ['is-function', E]], F, [unify, F, 'True', [chain, [eval, ['interpret-func', A, E, B, C]], G, [chain, [eval, ['metta-call', G, B, C]], H, [return, H]]], [chain, [eval, ['interpret-tuple', A, C]], G, [chain, [eval, ['metta-call', G, B, C]], H, [return, H]]]]]], [chain, [eval, ['type-cast', A, B, C]], H, [return, H]]]]]]).
metta_atom_corelib( [=, ['interpret-func', A, B, C, D], [function, [eval, ['if-decons', A, E, F, [chain, [eval, [interpret, E, B, D]], G, [eval, ['return-on-error', G, [eval, ['if-decons', B, _, H, [chain, [eval, ['interpret-args', A, F, H, C, D]], I, [eval, ['return-on-error', I, [chain, [cons, G, I], J, [return, J]]]]], [return, ['Error', B, "Function type expected"]]]]]]], [return, ['Error', A, "Non-empty expression atom is expected"]]]]]]).
metta_atom_corelib( [=, ['interpret-func', A, B, C], [eval, ['if-decons', A, D, E, [chain, [eval, [interpret, D, B, C]], F, [eval, ['return-on-error', F, [eval, ['if-decons', B, _, G, [chain, [eval, ['interpret-args', A, E, G, C]], H, [eval, ['return-on-error', H, [cons, F, H]]]], ['Error', B, "Function type expected"]]]]]], ['Error', A, "Non-empty expression atom is expected"]]]]).
metta_atom_corelib( [=, ['interpret-tuple', A, B], [function, [unify, A, [], [return, A], [eval, ['if-decons', A, C, D, [chain, [eval, [interpret, C, '%Undefined%', B]], E, [eval, ['if-empty', E, [return, 'Empty'], [chain, [eval, ['interpret-tuple', D, B]], F, [eval, ['if-empty', F, [return, 'Empty'], [chain, [cons, E, F], G, [return, G]]]]]]]], [return, ['Error', ['interpret-tuple', A, B], "Non-empty expression atom is expected as an argument"]]]]]]]).
metta_atom_corelib( [=, ['interpret-tuple', A, B], [match, A, [], A, [eval, ['if-decons', A, C, D, [chain, [eval, [interpret, C, '%Undefined%', B]], E, [chain, [eval, ['interpret-tuple', D, B]], F, [cons, E, F]]], ['Error', ['interpret-tuple', A, B], "Non-empty expression atom is expected as an argument"]]]]]).
metta_atom_corelib( [=, ['is-function', A], [chain, [eval, ['get-metatype', A]], B, [eval, [switch, [A, B], [[[_, 'Expression'], [chain, [eval, [car, A]], C, [match, C, ->, 'True', 'False']]], [_, 'False']]]]]]).
metta_atom_corelib( [=, ['is-function', A], [function, [chain, [eval, ['get-metatype', A]], B, [eval, [switch, [A, B], [[[_, 'Expression'], [eval, ['if-decons', A, C, _, [unify, C, ->, [return, 'True'], [return, 'False']], [return, ['Error', ['is-function', A], "is-function non-empty expression as an argument"]]]]], [_, [return, 'False']]]]]]]]).
metta_atom_corelib( [=, ['let*', A, B], [eval, ['if-decons', A, [C, D], E, [let, C, D, ['let*', E, B]], B]]]).
metta_atom_corelib( [=, ['map-atom', A, B, C], [function, [eval, ['if-decons', A, D, E, [chain, [eval, ['map-atom', E, B, C]], F, [chain, [eval, [apply, D, B, C]], G, [chain, G, H, [chain, [cons, H, F], I, [return, I]]]]], [return, []]]]]]).
metta_atom_corelib( [=, ['match-types', A, B, C, D], [function, [eval, ['if-equal', A, '%Undefined%', [return, C], [eval, ['if-equal', B, '%Undefined%', [return, C], [eval, ['if-equal', A, 'Atom', [return, C], [eval, ['if-equal', B, 'Atom', [return, C], [unify, A, B, [return, C], [return, D]]]]]]]]]]]]).
metta_atom_corelib( [=, ['metta-call', A, B, C], [function, [eval, ['if-error', A, [return, A], [chain, [eval, A], D, [eval, ['if-not-reducible', D, [return, A], [eval, ['if-empty', D, [return, 'Empty'], [eval, ['if-error', D, [return, D], [chain, [eval, [interpret, D, B, C]], E, [return, E]]]]]]]]]]]]]).
metta_atom_corelib( [=, ['return-on-error', A, B], [eval, ['if-empty', A, 'Empty', [eval, ['if-error', A, A, B]]]]]).
metta_atom_corelib( [=, ['return-on-error', A, B], [function, [eval, ['if-empty', A, [return, [return, 'Empty']], [eval, ['if-error', A, [return, [return, A]], [return, B]]]]]]]).
metta_atom_corelib( [=, ['switch-internal', A, [[B, C], D]], [function, [unify, A, B, [return, C], [chain, [eval, [switch, A, D]], E, [return, E]]]]]).
metta_atom_corelib( [=, ['switch-internal', A, [[B, C], D]], [match, A, B, C, [eval, [switch, A, D]]]]).
metta_atom_corelib( [=, ['type-cast', A, B, C], [chain, [eval, ['get-type', A, C]], D, [eval, [switch, [D, B], [[['%Undefined%', _], A], [[_, '%Undefined%'], A], [[B, _], A], [_, ['Error', A, 'BadType']]]]]]]).
metta_atom_corelib( [=, ['type-cast', A, B, C], [function, [chain, [eval, ['get-metatype', A]], D, [eval, ['if-equal', B, D, [return, A], [chain, [eval, ['collapse-get-type', A, C]], E, [chain, [eval, ['foldl-atom', E, 'False', F, G, [chain, [eval, ['match-types', G, B, 'True', 'False']], H, [chain, [eval, [or, F, H]], I, I]]]], J, [eval, [if, J, [return, A], [return, ['Error', A, 'BadType']]]]]]]]]]]).
metta_atom_corelib( [=, [and, 'False', 'False'], 'False']).
metta_atom_corelib( [=, [and, 'False', 'True'], 'False']).
metta_atom_corelib( [=, [and, 'True', 'False'], 'False']).
metta_atom_corelib( [=, [and, 'True', 'True'], 'True']).
metta_atom_corelib( [=, [apply, A, B, C], [function, [chain, [eval, [id, A]], B, [return, C]]]]).
metta_atom_corelib( [=, [call, A, B, C], [chain, [eval, A], D, [eval, ['if-empty', D, A, [eval, ['if-error', D, D, [eval, [interpret, D, B, C]]]]]]]]).
metta_atom_corelib( [=, [car, A], [eval, ['if-decons', A, B, _, B, ['Error', [car, A], "car expects a non-empty expression as an argument"]]]]).
metta_atom_corelib( [=, [id, A], A]).
metta_atom_corelib( [=, [if, 'False', _, A], A]).
metta_atom_corelib( [=, [if, 'True', A, _], A]).
metta_atom_corelib( [=, [interpret, A, B, C], [chain, [eval, ['get-metatype', A]], D, [eval, [switch, [B, D], [[['Atom', _], A], [[D, D], A], [[E, 'Variable'], A], [[E, 'Symbol'], [eval, ['type-cast', A, B, C]]], [[E, 'Grounded'], [eval, ['type-cast', A, B, C]]], [[E, 'Expression'], [eval, ['interpret-expression', A, B, C]]]]]]]]).
metta_atom_corelib( [=, [interpret, A, B, C], [function, [chain, [eval, ['get-metatype', A]], D, [eval, ['if-equal', B, 'Atom', [return, A], [eval, ['if-equal', B, D, [return, A], [eval, [switch, [B, D], [[[E, 'Variable'], [return, A]], [[E, 'Symbol'], [chain, [eval, ['type-cast', A, B, C]], F, [return, F]]], [[E, 'Grounded'], [chain, [eval, ['type-cast', A, B, C]], F, [return, F]]], [[E, 'Expression'], [chain, [eval, ['interpret-expression', A, B, C]], F, [return, F]]]]]]]]]]]]]).
metta_atom_corelib( [=, [let, A, B, C], [unify, B, A, C, 'Empty']]).
metta_atom_corelib( [=, [match, A, B, C], [unify, B, A, C, 'Empty']]).
metta_atom_corelib( [=, [nop, _], []]).
metta_atom_corelib( [=, [nop], []]).
metta_atom_corelib( [=, [or, 'False', 'False'], 'False']).
metta_atom_corelib( [=, [or, 'False', 'True'], 'True']).
metta_atom_corelib( [=, [or, 'True', 'False'], 'True']).
metta_atom_corelib( [=, [or, 'True', 'True'], 'True']).
metta_atom_corelib( [=, [quote, _], 'NotReducible']).
metta_atom_corelib( [=, [reduce, A, B, C], [chain, [eval, A], D, [eval, ['if-error', D, D, [eval, ['if-empty', D, [eval, [subst, A, B, C]], [eval, [reduce, D, B, C]]]]]]]]).
metta_atom_corelib( [=, [subst, A, B, C], [match, A, B, C, ['Error', [subst, A, B, C], "subst expects a variable as a second argument"]]]).
metta_atom_corelib( [=, [switch, A, B], [chain, [decons, B], C, [eval, ['switch-internal', A, C]]]]).
metta_atom_corelib( [=, [switch, A, B], [function, [chain, [decons, B], C, [chain, [eval, ['switch-internal', A, C]], D, [chain, [eval, ['if-not-reducible', D, 'Empty', D]], E, [return, E]]]]]]).
metta_atom_corelib( [=, [unquote, [quote, A]], A]).


metta_atom_corelib( [:, 'Error', [->, 'Atom', 'Atom', 'ErrorType']]).
metta_atom_corelib( [:, 'ErrorType', 'Type']).
metta_atom_corelib( [:, 'ReturnType', 'Type']).
metta_atom_corelib( [:, 'add-atom', [->, 'Space', 'Atom', [->]]]).
metta_atom_corelib( [:, 'car-atom', [->, 'Expression', 'Atom']]).
metta_atom_corelib( [:, 'cdr-atom', [->, 'Expression', 'Expression']]).
metta_atom_corelib( [:, 'filter-atom', [->, 'Expression', 'Variable', 'Atom', 'Expression']]).
metta_atom_corelib( [:, 'foldl-atom', [->, 'Expression', 'Atom', 'Variable', 'Variable', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'get-atoms', [->, 'Space', 'Atom']]).
metta_atom_corelib( [:, 'if-decons', [->, 'Atom', 'Variable', 'Variable', 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'if-empty', [->, 'Atom', 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'if-error', [->, 'Atom', 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'if-non-empty-expression', [->, 'Atom', 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'if-not-reducible', [->, 'Atom', 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'let*', [->, 'Expression', 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'map-atom', [->, 'Expression', 'Variable', 'Atom', 'Expression']]).
metta_atom_corelib( [:, 'remove-atom', [->, 'Space', 'Atom', [->]]]).
metta_atom_corelib( [:, 'return-on-error', [->, 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, and, [->, 'Bool', 'Bool', 'Bool']]).
metta_atom_corelib( [:, apply, [->, 'Atom', 'Variable', 'Atom', 'Atom']]).
metta_atom_corelib( [:, chain, [->, 'Atom', 'Variable', 'Atom', 'Atom']]).
metta_atom_corelib( [:, cons, [->, 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, decons, [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, empty, [->, '%Undefined%']]).
metta_atom_corelib( [:, eval, [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, function, [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, id, [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, if, [->, 'Bool', 'Atom', 'Atom', _]]).
metta_atom_corelib( [:, let, [->, 'Atom', '%Undefined%', 'Atom', 'Atom']]).
metta_atom_corelib( [:, match, [->, 'Atom', 'Atom', 'Atom', '%Undefined%']]).
metta_atom_corelib( [:, or, [->, 'Bool', 'Bool', 'Bool']]).
metta_atom_corelib( [:, quote, [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, return, [->, 'Atom', 'ReturnType']]).
metta_atom_corelib( [:, switch, [->, '%Undefined%', 'Expression', 'Atom']]).
metta_atom_corelib( [:, unify, [->, 'Atom', 'Atom', 'Atom', 'Atom', '%Undefined%']]).
metta_atom_corelib( [:, unify, [->, 'Atom', 'Atom', 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, unquote, [->, '%Undefined%', '%Undefined%']]).

metta_atom_corelib( [:, 'get-metatype', [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'get-type', [->, 'Atom', 'Atom']]).
metta_atom_corelib( [:, 'get-type', [->, 'Atom', 'Atom', 'Atom']]).
metta_atom_corelib( [:, '==', [->, T, T, 'Bool']]).
metta_atom_corelib( [:, ':', '%Undefined%']).
