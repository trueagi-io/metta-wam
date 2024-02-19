# Curried Chaining

Examples of backward and forward curried chainers.

The forward chainer is present in the same file as the backward
chainer because in, in the curried case, in order to go forward, the
chainer still need to unfold the rules backward.  For that reason the
forward chainer calls the backward chainer with a rule abstraction as
query instead of invoking a match query.
