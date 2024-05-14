:- begin_tests(lifepaths).

:- include(lifepaths).

%% Data consistency checks

test(all_lifepaths_have_valid_settings, all(Lp-Setting = [])) :- 
    lifepath(id(Lp, Setting)),
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
    lifepath(_, page(Book, Number), _, _),
    (\+ page_in_range(Book, Number)).

test(all_lifepaths_have_valid_leads, all(Lead = [])) :-
    lifepath(id(_, SettingName), _, _, Leads),
    member(Lead, Leads),
    setting(SettingName, Stock),
    \+ setting(Lead, Stock).

test(all_lifepath_providers_have_valid_lifepaths, all(Lp = [])) :-
    findall(Name, lifepath_provides(Name, _), LpNames),
    member(Lp, LpNames),
    \+ lifepath(Lp).

test(all_lifepath_requirements_have_valid_lifepaths, all(Lp = [])) :-
    findall(Name, lifepath_requires(Name, _), LpNames),
    member(Lp, LpNames),
    \+ lifepath(Lp).

test(no_lifepaths_have_trait_intimidation_or_skill_intimidating, [fail]) :-
    lifepath_provides(_, trait(intimidation));
    lifepath_provides(_, skill(intimidating)).

test(all_lifepaths_providing_flags_are_valid, all(ProvFails = [[]])) :-
    findall(provider(Id, flag(F)), (
        lifepath_provides(Id, flag(F)),
        \+ flag(F)
    ), ProvFails).

test(all_lifepaths_requiring_flags_are_valid, all(ReqFails = [[]])) :-
    findall(requirement(Id, flag(F)), (
        lifepath_requires(Id, Reqs), 
        member(flag(F), Reqs),
        \+ flag(F)
    ), ReqFails).

:- end_tests(lifepaths). 
