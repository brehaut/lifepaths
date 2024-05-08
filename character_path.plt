:- begin_tests(character_path).

:- include(lifepaths).

test(normalize_lifepath_name_bare_atom) :-
    normalize_lifepath_name(born_peasant, id(born_peasant, peasant)).
test(normalize_lifepath_name_fully_qualified_id) :-
    normalize_lifepath_name(id(born_peasant, peasant), id(born_peasant, peasant)).
test(normalize_lifepath_name_shorthand_id) :-
    normalize_lifepath_name(born_peasant-peasant, id(born_peasant, peasant)).


test(character_path_validates_born_character, [nondet]) :- 
    character_path([id(born_peasant, peasant)]).
test(character_path_validates_simple_character, [nondet]) :- 
    character_path([id(born_peasant, peasant), id(farmer, peasant), id(trapper, peasant)]).


:- end_tests(character_path). 
