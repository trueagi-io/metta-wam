% Helper predicate: L is immediately to the left of R in the list
left_of(L, R, [L, R | _]).
left_of(L, R, [_ | Rest]) :-
    left_of(L, R, Rest).

% Helper predicate: A and B are next to each other in the list
next_to(A, B, Houses) :-
    left_of(A, B, Houses);
    left_of(B, A, Houses).

% Predicate to get the first house in the list
house_first(House, Houses) :-
    Houses = [House | _].

% Predicate to get the center house in the list
house_center(House, Houses) :-
    Houses = [_, _, House, _, _].

% Get the house with a specific nationality
house_nationality(House, Nationality, Houses) :-
    member(House, Houses),
    House = house(Nationality, _, _, _, _).

% Get the house with a specific color
house_color(House, Color, Houses) :-
    member(House, Houses),
    House = house(_, Color, _, _, _).

% Get the house with a specific pet
house_pet(House, Pet, Houses) :-
    member(House, Houses),
    House = house(_, _, Pet, _, _).

% Get the house with a specific drink
house_drink(House, Drink, Houses) :-
    member(House, Houses),
    House = house(_, _, _, Drink, _).

% Get the house with a specific smoke
house_smoke(House, Smoke, Houses) :-
    member(House, Houses),
    House = house(_, _, _, _, Smoke).


% Define the main predicate to find the owner of the fish
zebra_owner(Owner) :-
    % There are five houses represented as a list
    Houses = [_H1, _H2, _H3, _H4, _H5],

    % Clue 1: The Brit lives in the red house.
    house_nationality(House1, brit, Houses),
    house_color(House1, red, Houses),

    % Clue 2: The Swede keeps dogs as pets.
    house_nationality(House2, swede, Houses),
    house_pet(House2, dog, Houses),

    % Clue 3: The Dane drinks tea.
    house_nationality(House3, dane, Houses),
    house_drink(House3, tea, Houses),

    % Clue 4: The green house is immediately to the left of the white house.
    left_of(HouseG, HouseW, Houses),
    house_color(HouseG, green, Houses),
    house_color(HouseW, white, Houses),

    % Clue 5: The owner of the green house drinks coffee.
    house_color(House5, green, Houses),
    house_drink(House5, coffee, Houses),

    % Clue 6: The person who smokes Pall Mall rears birds.
    house_smoke(House6, 'Pall Mall', Houses),
    house_pet(House6, bird, Houses),

    % Clue 7: The owner of the yellow house smokes Dunhill.
    house_color(House7, yellow, Houses),
    house_smoke(House7, 'Dunhill', Houses),

    % Clue 8: The man living in the center house drinks milk.
    house_center(CenterHouse, Houses),
    house_drink(CenterHouse, milk, Houses),

    % Clue 9: The Norwegian lives in the first house.
    house_first(FirstHouse, Houses),
    house_nationality(FirstHouse, norwegian, Houses),

    % Clue 10: The man who smokes Blends lives next to the one who keeps cats.
    house_smoke(House10, 'Blends', Houses),
    next_to(House10, HouseCat, Houses),
    house_pet(HouseCat, cat, Houses),

    % Clue 11: The man who keeps horses lives next to the man who smokes Dunhill.
    house_pet(House11, horse, Houses),
    next_to(House11, HouseDunhill, Houses),
    house_smoke(HouseDunhill, 'Dunhill', Houses),

    % Clue 12: The owner who smokes BlueMaster drinks beer.
    house_smoke(House12, 'BlueMaster', Houses),
    house_drink(House12, beer, Houses),

    % Clue 13: The German smokes Prince.
    house_nationality(House13, german, Houses),
    house_smoke(House13, 'Prince', Houses),

    % Clue 14: The Norwegian lives next to the blue house.
    house_nationality(House14, norwegian, Houses),
    next_to(House14, HouseBlue, Houses),
    house_color(HouseBlue, blue, Houses),

    % Clue 15: The man who smokes Blends has a neighbor who drinks water.
    house_smoke(House15, 'Blends', Houses),
    next_to(House15, HouseWater, Houses),
    house_drink(HouseWater, water, Houses),

    % Determine who owns the fish
    house_pet(FishHouse, fish, Houses),
    house_nationality(FishHouse, Owner, Houses).

