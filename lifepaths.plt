:- begin_tests(lifepaths).

:- include(lifepaths).

%% Data consistency checks

%% TODO: make these exhaustive
test(all_lifepaths_have_valid_settings, all(Lp-Setting = [])) :- 
    lp(id(Lp, Setting)),
    \+ setting(Setting, _).

test(all_settings_have_valid_stocks, all(Stock = [])) :-
    findall(Stock, setting(_, Stock), AllStocks),
    list_to_set(AllStocks, DistinctStocks),
    member(Stock, DistinctStocks),
    (\+ stock(Stock)).

page_in_range(Book, Page) :-
    book(Book, PageRange),
    member(range(Start, End), PageRange),
    (Start =< Page, Page =< End).
    
test(all_lifepaths_have_valid_pages, all(_ = [])) :-
    lp(_, page(Book, Number), _, _),
    (\+ page_in_range(Book, Number)).

test(all_lifepaths_have_valid_leads, all(Lead = [])) :-
    lp(id(_, SettingName), _, _, Leads),
    member(Lead, Leads),
    setting(SettingName, Stock),
    \+ setting(Lead, Stock).

test(all_lifepath_providers_have_valid_lifepaths, all(Lp = [])) :-
    findall(Name, lp_provides(Name, _), LpNames),
    member(Lp, LpNames),
    \+ lp(Lp).

test(all_lifepath_requirements_have_valid_lifepaths, all(Lp = [])) :-
    findall(Name, lp_requires(Name, _), LpNames),
    member(Lp, LpNames),
    \+ lp(Lp).

%% Lifepath Rules 
% simplified predicate for testing when we dont care about constraints

test(satisfy_requirement_flag_single_lifepath, []) :-
    satisfy_requirement(flag(female), [id(country_wife, peasant)]).
test(satisfy_requirement_flag_with_multiple_lifepaths, []) :-
    satisfy_requirement(flag(female), [id(farmer, peasant), id(country_wife, peasant)]).
test(satisfy_requirement_flag_single_lifepath_fail, [fail]) :-
    satisfy_requirement(flag(female), [id(farmer, peasant)]).
test(satisfy_requirement_not_flag) :-
    satisfy_requirement(not(flag(female)), [id(farmer, peasant)]).

test(satisfy_requrements_position) :-  
    satisfy_requirement(position(2), [id(farmer, peasant)]).
test(satisfy_requrements_position_fail, [fail]) :-  
    satisfy_requirement(position(3), [id(farmer, peasant)]).

test(satisfy_requrements_lifepath) :-  
    satisfy_requirement(lifepath(farmer), [id(farmer, peasant)]).
test(satisfy_requrements_lifepath_with_later_lps, [nondet]) :-  
    satisfy_requirement(lifepath(farmer), [id(farmer, peasant), id(elder, peasant)]).
test(satisfy_requrements_lifepath_fails, [fail]) :-  
    satisfy_requirement(lifepath(farmer), [id(born_peasant, peasant)]).

test(satisifies_requirements_lifepath_with_no_requirements, all(Conditions = [[]])) :-
    satisfies_requirements(id(farmer, peasant), [], Conditions).
test(satisfies_requirements_lifepath, all(Conditions = [[]])) :-
    satisfies_requirements(id(auger, peasant), [id(midwife, peasant), id(farmer, peasant)], Conditions).

test(satisfies_requirements_lifepath_flag_female, all(Conditions = [[max(lifepaths, 3)]])) :-
    satisfies_requirements(id(auger, peasant), [id(serving_girl, villager)], Conditions).

test(character_path_validates_born_character, [nondet]) :- 
    character_path([id(born_peasant, peasant)]).
test(character_path_validates_simple_character, [nondet]) :- 
    character_path([id(born_peasant, peasant), id(farmer, peasant), id(trapper, peasant)]).


:- end_tests(lifepaths). 
