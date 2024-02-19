:-encoding(iso_latin_1).
:- dynamic pllm:trigram/3.
:- multifile pllm:trigram/3.
:- module_transparent pllm:trigram/3.

pllm:trigram(are, you, 'a-fan').
pllm:trigram(you, 'a-fan', of).
pllm:trigram('a-fan', of, 'the-star-wars-series').
pllm:trigram(yeah, love, them).
pllm:trigram(who, is, 'your-least-favorite-character').
pllm:trigram(without, 'a-doubt', 'jar-jar-binks').
pllm:trigram(absolutely, hate, him).
pllm:trigram(who, '\'s', you).
pllm:trigram('\'s', you, 'favorite-star-wars').
pllm:trigram(you, 'favorite-star-wars', character).
pllm:trigram(do, 'n\'t', you).
pllm:trigram('n\'t', you, wish).
pllm:trigram(you, wish, he).
pllm:trigram(wish, he, was).
pllm:trigram(he, was, in).
pllm:trigram(was, in, 'the-new-movies').
pllm:trigram(dead, he, is).

