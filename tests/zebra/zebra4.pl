% Main predicate to find the owner of the fish
zebra_owner(Owner) :-
    % Initialize Houses and exists_props
    init_houses(['S', ['S', ['S', ['S', ['S', 'Z']]]]]),  % Equivalent to 5
    b_setval(exists_props, 'Nil'),

    % Clue 1: The Brit lives in the red house.
    declare(nationality, Brit, brit),
    declare(color, Brit, red),

    % Clue 2: The Swede keeps dogs as pets.
    declare(nationality, Swede, swede),
    declare(pet, Swede, dog),

    % Clue 3: The Dane drinks tea.
    declare(nationality, Dane, dane),
    declare(drink, Dane, tea),

    % Clue 4: The green house is immediately to the left of the white house.
    left_of(Green, White),
    declare(color, Green, green),
    declare(color, White, white),

    % Clue 5: The owner of the green house drinks coffee.
    declare(color, Green, green),
    declare(drink, Green, coffee),

    % Clue 6: The person who smokes Pall Mall rears birds.
    declare(smokes, PallMallSmoker, 'PallMall'),
    declare(pet, PallMallSmoker, 'Birds'),

    % Clue 7: The owner of the yellow house smokes Dunhill.
    declare(color, Yellow, yellow),
    declare(smokes, Yellow, 'Dunhill'),

    % Clue 8: The man living in the center house drinks milk.
    center_house(CenterHouse),
    declare(drink, CenterHouse, milk),

    % Clue 9: The Norwegian lives in the first house.
    first_house(FirstHouse),
    declare(nationality, FirstHouse, norwegian),

    % Clue 10: The man who smokes Blends lives next to the one who keeps cats.
    declare(smokes, SmokesBlends, 'Blends'),
    next_to(SmokesBlends, HouseCat),
    declare(pet, HouseCat, cat),

    % Clue 11: The man who keeps horses lives next to the man who smokes Dunhill.
    declare(pet, HorseKeeper, horse),
    next_to(HorseKeeper, SmokesDunhills),
    declare(smokes, SmokesDunhills, 'Dunhill'),

    % Clue 12: The owner who smokes BlueMaster drinks beer.
    declare(smokes, SmokesBlueMaster, 'BlueMaster'),
    declare(drink, SmokesBlueMaster, beer),

    % Clue 13: The German smokes Prince.
    declare(nationality, German, german),
    declare(smokes, German, 'Prince'),

    % Clue 14: The Norwegian lives next to the blue house.
    declare(nationality, Norwegian, norwegian),
    next_to(Norwegian, HouseBlue),
    declare(color, HouseBlue, blue),

    % Clue 15: The man who smokes Blends has a neighbor who drinks water.
    declare(smokes, BlendsSmoker, 'Blends'),
    next_to(BlendsSmoker, WaterDrinker),
    declare(drink, WaterDrinker, water),

    % Determine who owns the fish
    declare(pet, FishOwner, fish),
    write('.'),
    declare(nationality, FishOwner, Owner).

% Initialize Houses as a list of uninitialized existss
init_houses(NatNumber) :-
    create_empty_list(NatNumber, Houses),
    b_setval(neighborhood, Houses).

% Create an empty list of length N using Nat numbers
create_empty_list('Z', 'Nil').
create_empty_list(['S', N], 'Cons'(Exists, Tail)) :-
    create_exists(Exists),
    create_empty_list(N, Tail).

create_exists('Cons'(_, _)).

% Helper predicates

% Predicate: L is immediately to the left of R in the list
left_of(L, R) :-
    b_getval(neighborhood, Houses),
    left_of_list(L, R, Houses).

left_of_list(L, R, 'Cons'(L, 'Cons'(R, _))).
left_of_list(L, R, 'Cons'(_, Rest)) :-
    left_of_list(L, R, Rest).

% Predicate: A and B are next to each other in the list
next_to(A, B) :-
    left_of(A, B);
    left_of(B, A).

% Predicate to get the first house in the list
first_house(First) :-
    % left_of(First, H2), left_of(H2, Center), left_of(Center, H4), left_of(H4, _H5).
    b_getval(neighborhood, Houses),
    nth0_nat('Z', Houses, First).

% Predicate to get the center house in the list
center_house(Center) :-
    %left_of(_H1, H2), left_of(H2, Center), left_of(Center, H4), left_of(H4, _H5).
    b_getval(neighborhood, Houses),
    nth0_nat(['S', ['S', 'Z']], Houses, Center).

% =============================
% Property handling predicates
% =============================

% General property declaration predicate
declare(PropName, Exists, PropValue) :-
    prop(PropName, Exists, PropValue).

prop(PropName, Object, PropValue) :-
    get_prop_num(PropName, NumNat), !,
    nth0_nat(NumNat, Object, PropValue), !,
    something_existing(Object).

% General property access predicate
prop2(PropName, Exists, PropValue) :-
    get_prop_num(PropName, NumNat),
    nth0_nat(NumNat, Object, PropValue), !,
    something_existing(Object), Exists = Object.

% Ensure Exists is a member of Houses
something_existing(Exists) :-
    b_getval(neighborhood, Houses),
    member_cons(Exists, Houses).

% Member predicate for 'Cons'/'Nil' lists
member_cons(Elem, 'Cons'(Elem, _)).
member_cons(Elem, 'Cons'(_, Tail)) :-
    member_cons(Elem, Tail).

% Map property names to their positions (numbers) using Nat
get_prop_num(PropName, NumNat) :-
    b_getval(exists_props, PropList),
    (   nth0_nat_exists(NumNat, PropList, Prop),
        Prop == PropName -> true
    ;   append_cons(PropList, 'Cons'(PropName, 'Nil'), NewPropList),
        length_nat(PropList, NumNat),
        b_setval(exists_props, NewPropList)
    ).

% Zero-based nth predicate using Nat that might extend an open list
nth0_nat('Z', 'Cons'(Elem, _), Elem).
nth0_nat(['S', N], 'Cons'(_, Rest), Elem) :-
    nth0_nat(N, Rest, Elem).

% Zero-based nth predicate using Nat that requires bound existss
nth0_nat_exists('Z', 'Cons'(Elem, _), Elem) :- nonvar(Elem).
nth0_nat_exists(['S', N], 'Cons'(_, Rest), Elem) :-
    nonvar(Rest),
    nth0_nat_exists(N, Rest, Elem).

% Length of list using Nat
length_nat('Nil', 'Z').
length_nat('Cons'(_, Rest), ['S', N]) :-
    length_nat(Rest, N).

% Append two 'Cons'/'Nil' lists
append_cons('Nil', List2, List2).
append_cons('Cons'(H, T), List2, 'Cons'(H, T2)) :-
    append_cons(T, List2, T2).

