:- use_module(gold_revised).

lp(Id) :- lp(Id, _, _, _).

lp_stock(id(_, Setting), Stock) :- 
    setting(Setting, Stock).


lp_leads(id(Name, Setting), Leads):-     
    lp(id(Name, Setting), _, _, LpLeads),
    (LpLeads = any_except(Exclude)
        -> (
            setting(Setting, Stock),
            findall(S, setting(S, Stock), AllSettings),
            subtract(AllSettings, Exclude, Leads)
        )    
        ;        
        (    
            lp(id(Name, Setting), _, _, Leads)
        )
    ).
            
    

lp_years(Name, Years):- lp(Name, _, Years, _).

% born is a common enough cast that its worth handling specifically

is_born_lp(Lifepath) :- 
    id(Name, _) = Lifepath 
    -> (
        atom_concat(born_, _, Name);
        atom_concat(_, "_born", Name)
    )
    ; 
    ( 
        atom_concat(born_, _, Lifepath);
        atom_concat(_, "_born", Lifepath)
    ).
    

requirement_is_constraint(constraint(_)).

% wrapping the goal in negated findall removes a choicepoint but feels gross. 
% theres got to be a nicer way to do this.
any_satisfies(Goal, List) :-
    \+ findall(L, (member(L, List), call(Goal, L)), []).

% satisfy_requirement resolves individual lifepath requirements
satisfy_requirement(flag(FlagName), Lifepaths) :-
    any_satisfies([Lifepath]>>lp_provides(Lifepath, flag(FlagName)), Lifepaths).    

satisfy_requirement(trait(TraitName), Lifepaths) :-
    any_satisfies([Lifepath]>>lp_provides(Lifepath, trait(TraitName)), Lifepaths).

satisfy_requirement(lifepath(Lifepath), Lifepaths) :-
    % print_message(debug, log(Lifepath, Lifepaths)),
    member(id(Lifepath, _), Lifepaths).

satisfy_requirement(position(N), Lifepaths) :-
    length(Lifepaths, Len),
    Len =:= N - 1.

satisfy_requirement(not(Requirement), Lifepaths) :-
    \+ Requirement = constraint(_),
    \+ satisfy_requirement(Requirement, Lifepaths).
% constraints are collected and checked later. skip them now.
satisfy_requirement(constraint(_), _). 

map_satreq(Lifepaths, Constraint) :- 
    satisfy_requirement(Constraint, Lifepaths).

unwrap_constraint(constraint(C), C).

satisfies_requirements(Lifepath, ChosenLifepaths, Constraints) :-
    % lifepaths with no requirements are automatically satisfied
    (
        \+ lp_requires(Lifepath, _),
        Constraints = []
    ) 
    ;
    % otherwise satisfy the specified
    (
        findall(
            Requirements, (
                lp_requires(Lifepath, Requirements), 
                maplist(map_satreq(ChosenLifepaths), Requirements)
            ), 
            AllReqs
        ),
        member(Reqs, AllReqs),
        findall(C, (member(R, Reqs), unwrap_constraint(R, C)), Constraints)
    )
    .

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
    LastLp = id(_, LastSetting),
    lp_leads(LastLp, Leads),
    id(_, Setting) = Available,
    member(Setting, [LastSetting|Leads]),
    lp(Available), % confirm that the lifepath exists.
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

normalize_lifepath_name(-(Name, Setting), id(Name, Setting)).
normalize_lifepath_name(Name, Normalized) :-
    atom(Name) -> (
        (findall(Setting, lp(id(Name, Setting), _, _, _), [Setting]),
        Normalized = id(Name, Setting)) 
        ; throw(ambiguous_lifepath(Name))
    ) ; Normalized = Name. 

% character_path(LifepathNames)
%
% character_path checks a set of lifepaths against the constraints.
% This is the main event.
character_path([]) :- fail.
character_path(LifePaths) :- 
    maplist(normalize_lifepath_name, LifePaths, NormalizedLifePaths),
    character_path(NormalizedLifePaths, [], Constraints),
    satisfies_constraints(Constraints, LifePaths).

