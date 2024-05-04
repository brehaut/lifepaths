:- use_module(gold_revised).

lp(Name) :- lp(Name, _, _, _, _).
lp(Name, Setting):- lp(Name, Setting, _, _, _).

lp_lead(Name, Lead):- lp(Name, _, _, _, Leads), member(Lead, Leads).
lp_years(Name, Years):- lp(Name, _, _, Years, _).

% born is a common enough cast that its worth handling specifically
is_born_lp(Lifepath) :- 
    atom_concat(born_, _, Lifepath);
    atom_concat(_, "_born", Lifepath).

requirement_is_constraint(constraint(_)).

% satisfy_requirement resolves individual lifepath requirements
satisfy_requirement(flag(FlagName), Lifepaths) :-
    member(Lifepath, Lifepaths),
    lp_provides(Lifepath, flag(FlagName)).

satisfy_requirement(trait(TraitName), Lifepaths) :-
    member(Lifepath, Lifepaths),
    lp_provides(Lifepath, trait(TraitName)).
satisfy_requirement(lifepath(Lifepath), Lifepaths) :-
    % print_message(debug, log(Lifepath, Lifepaths)),
    member(Lifepath, Lifepaths).
satisfy_requirement(position(N), Lifepaths) :-
    length(Lifepaths, Len),
    Len =:= N - 1.
satisfy_requirement(not(Requirement), Lifepaths) :-
    \+ Requirement = constraint(_),
    \+ satisfy_requirement(Requirement, Lifepaths).
% constraints are collected and checked later. skip them now.
satisfy_requirement(constraint(_), _). 

satisfies_requirements(Lifepath, ChosenLifepaths, Constraints) :-
    % lifepaths with no requirements are automatically satisfied
    (\+ lp_requires(Lifepath, _),
    Constraints = []) 
    ;
    % otherwise satisfy the specified
    lp_requires(Lifepath, Requirements),
    memberchk(Requirement, Requirements),
    satisfy_requirement(Requirement, ChosenLifepaths), 
    findall(Constraint, Requirement = constraint(Constraint), Constraints).


satisfy_constraint(max(lifepaths, N), Lifepaths) :-
    length(Lifepaths, LN),
    LN =< N.
satisfy_constraint(min(age, N), Lifepaths) :-
    character_age(Lifepaths, Age),
    N =< Age.

satisfies_constraints([], _). 
satisfies_constraints(Constraints, Lifepaths) :-
    member(Constraint, Constraints),
    satisfy_constraint(Constraint, Lifepaths), !.

% available_lifepaths produces a an available lifepath to choose 
% (and any constraints) based on the lifepaths taken so far.
% constraints are extracted from requirements, and are used to check
% a character once all lifepaths have been chosen.
available_lifepath([], Available, []) :- 
    lp(Available), 
    is_born_lp(Available).    

available_lifepath(ChosenLifepaths, Available, Constraints) :-
    ChosenLifepaths = [LastLp|_],
    lp(LastLp, LastSetting, _, _, Leads),
    member(Setting, [LastSetting|Leads]),
    lp(Available, Setting),
    \+ is_born_lp(Available),
    satisfies_requirements(Available, ChosenLifepaths, Constraints).

character_age([], 0).
character_age([Lifepath|Lifepaths], Age) :-
    lp_years(Lifepath, Years),
    character_age(Lifepaths, PriorAge),
    Age is PriorAge + Years.


character_path([], _, []).
character_path([First|Rest], Selected, Constraints) :-
    available_lifepath(Selected, First, NewConstraints),
    append(NewConstraints, LaterConstraints, Constraints),
    character_path(Rest, [First|Selected], LaterConstraints).

% character_path(LifepathNames)
%
% character_path checks a set of lifepaths against the constraints.
% This is the main event.
character_path([]) :- fail.
character_path(LifePaths) :- 
    character_path(LifePaths, [], Constraints),
    % print_message(debug, constraints(Constraints)),
    satisfies_constraints(Constraints, LifePaths).

