% Main predicate to find the owner of the fish
zebra_owner(Owner) :-
    % Initialize Houses and object_props
    length(Houses, 5),
    b_setval(neighborhood, Houses),
    b_setval(object_props, []),

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
    left_of(HouseG, HouseW),
    declare(color, HouseG, green),
    declare(color, HouseW, white),

    % Clue 5: The owner of the green house drinks coffee.
    declare(color, CoffeeDrinker, green),
    declare(drink, CoffeeDrinker, coffee),

    % Clue 6: The person who smokes Pall Mall rears birds.
    declare(smokes, BirdHouse, 'Pall Mall'),
    declare(pet, BirdHouse, bird),

    % Clue 7: The owner of the yellow house smokes Dunhill.
    declare(color, HouseY, yellow),
    declare(smokes, HouseY, 'Dunhill'),

    % Clue 8: The man living in the center house drinks milk.
    house_center(CenterHouse),
    declare(drink, CenterHouse, milk),

    % Clue 9: The Norwegian lives in the first house.
    house_first(FirstHouse),
    declare(nationality, FirstHouse, norwegian),

    % Clue 10: The man who smokes Blends lives next to the one who keeps cats.
    declare(smokes, HouseBlends, 'Blends'),
    next_to(HouseBlends, HouseCat),
    declare(pet, HouseCat, cat),

    % Clue 11: The man who keeps horses lives next to the man who smokes Dunhill.
    declare(pet, Brit1, horse),
    next_to(Brit1, HouseDunhill),
    declare(smokes, HouseDunhill, 'Dunhill'),

    % Clue 12: The owner who smokes BlueMaster drinks beer.
    declare(smokes, BeerHouse, 'BlueMaster'),
    declare(drink, BeerHouse, beer),

    % Clue 13: The German smokes Prince.
    declare(nationality, German, german),
    declare(smokes, German, 'Prince'),

    % Clue 14: The Norwegian lives next to the blue house.
    declare(nationality, Norwegian, norwegian),
    next_to(Norwegian, HouseBlue),
    declare(color, HouseBlue, blue),

    % Clue 15: The man who smokes Blends has a neighbor who drinks water.
    declare(smokes, BlendsSmoker, 'Blends'),
    next_to(BlendsSmoker, HouseWater),
    declare(drink, HouseWater, water),

    % Determine who owns the fish
    declare(pet, FishKeeper, fish),
    declare(nationality, FishKeeper, Owner).

% Helper predicates
init_houses(Number):-
    length(Houses, Number),
    maplist(init_object,Houses),
    b_setval(neighborhood, Houses).

init_object([_|_]).

% Predicate: L is immediately to the left of R in the list
left_of(L, R) :-
    b_getval(neighborhood, Houses),
    left_of_list(L, R, Houses).

left_of_list(L, R, [L, R | _]).
left_of_list(L, R, [_ | Rest]) :-
    left_of_list(L, R, Rest).

% Predicate: A and B are next to each other in the list
next_to(A, B) :-
    left_of(A, B);
    left_of(B, A).

% Predicate to get the first house in the list
house_first(House) :-
    b_getval(neighborhood, Houses),
    Houses = [House | _].

% Predicate to get the center house in the list
house_center(House) :-
    b_getval(neighborhood, Houses),
    Houses = [_, _, House, _, _].

% General property declaration predicate
declare(PropName, Object, PropValue) :-
    find_object(PropName, Object, PropValue) -> true ;
    create_object(PropName, Object, PropValue).

% Find the property value if it's already set
find_object(PropName, Object, PropValue) :-
    get_prop_num(PropName, NumNat),
    an_object(Object),
    nth0_nat(NumNat, Object, PropValue),
    nonvar(PropValue).

% Create and set the property value if it's not already set
create_object(PropName, Object, PropValue) :-
    get_prop_num(PropName, NumNat),
    an_object(Object),
    nth0_nat(NumNat, Object, PropValue).

% Ensure Object is a member of Houses
an_object(Object) :-
    nonvar(Object), !.
an_object(Object) :-
    b_getval(neighborhood, Houses),
    member(Object, Houses).

% Map property names to their positions (numbers) using Nat
get_prop_num(PropName, NumNat) :-
    b_getval(object_props, PropList),
    (   nth0_nat_exists(NumNat, PropList, Prop),
        Prop == PropName -> true
    ;  (append(PropList, [PropName], NewPropList),
        length_nat(NewPropList, NumNat),
        b_setval(object_props, NewPropList))
    ).

% Define natural numbers using Peano arithmetic
% Z represents zero, ['S', N] represents the successor of N

% Zero-based nth predicate using Nat that might extend an open list
nth0_nat('Z', [Elem | _], Elem).
nth0_nat(['S', N], [_ | Rest], Elem) :-
    nth0_nat(N, Rest, Elem).

% Zero-based nth predicate using Nat that requires bound objects
nth0_nat_exists('Z', [Elem | _], Elem):- nonvar(Elem).
nth0_nat_exists(['S', N], [_ | Rest], Elem) :- nonvar(Rest),
    nth0_nat(N, Rest, Elem).

% Length of list using Nat
length_nat([], 'Z').
length_nat([_ | Rest], ['S', N]) :-
    length_nat(Rest, N).

