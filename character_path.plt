:- begin_tests(character_path).

:- include(lifepaths).


%% Lifepath Rules 
% simplified predicate for testing when we dont care about constraints

test(character_path_validates_born_character, [nondet]) :- 
    character_path(char{lifepaths:[id(born_peasant, peasant)]}).
test(character_path_validates_simple_character, [nondet]) :- 
    character_path(char{lifepaths: [id(born_peasant, peasant), id(farmer, peasant), id(trapper, peasant)]}).


:- end_tests(character_path). 
