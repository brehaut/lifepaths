:- begin_tests(satisfy_requirement).

:- include(lifepaths).

%% Simple requirements checks


test(satisfy_requirement_flag_single_lifepath, []) :-
    satisfy_requirement(flag(female), [], [id(country_wife, peasant)]).
test(satisfy_requirement_flag_with_multiple_lifepaths, []) :-
    satisfy_requirement(flag(female), [], [id(farmer, peasant), id(country_wife, peasant)]).
test(satisfy_requirement_flag_single_lifepath_fail, [fail]) :-
    satisfy_requirement(flag(female), [], [id(farmer, peasant)]).
test(satisfy_requirement_not_flag) :-
    satisfy_requirement(not(flag(female)), [], [id(farmer, peasant)]).

test(satisfy_requrements_position) :-  
    satisfy_requirement(position(2), [], [id(farmer, peasant)]).
test(satisfy_requrements_position_fail, [fail]) :-  
    satisfy_requirement(position(3), [], [id(farmer, peasant)]).

test(satisfy_requrements_lifepath) :-  
    satisfy_requirement(lifepath(farmer), [], [id(farmer, peasant)]).
test(satisfy_requrements_lifepath_with_later_lps, [nondet]) :-  
    satisfy_requirement(lifepath(farmer), [], [id(farmer, peasant), id(elder, peasant)]).
test(satisfy_requrements_lifepath_fails, [fail]) :-  
    satisfy_requirement(lifepath(farmer), [], [id(born_peasant, peasant)]).


%% Aggregated requirements checks

test(satisifies_requirements_lifepath_with_no_requirements, all(Conditions = [[]])) :-
    satisfies_requirements(id(farmer, peasant), [], [], Conditions).
test(satisfies_requirements_lifepath, all(Conditions = [[]])) :-
    satisfies_requirements(id(augur, peasant), [], [id(midwife, peasant), id(farmer, peasant)], Conditions).

test(satisfies_requirements_lifepath_flag_female, all(Conditions = [[max(lifepaths, 3)]])) :-
    satisfies_requirements(id(augur, peasant), [], [id(serving_girl, villager)], Conditions).

:- end_tests(satisfy_requirement).