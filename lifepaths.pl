:- use_module(gold_revised).

lifepath(Id) :- lifepath(Id, _, _, _).

lp_stock(id(_, Setting), Stock) :- 
    setting(Setting, Stock).


lp_leads(id(Name, Setting), Leads):-     
    lifepath(id(Name, Setting), _, _, LpLeads),
    (LpLeads = any_except(Exclude)
        -> (
            setting(Setting, Stock),
            findall(S, setting(S, Stock), AllSettings),
            subtract(AllSettings, Exclude, Leads)
        )    
        ;        
        (    
            lifepath(id(Name, Setting), _, _, Leads)
        )
    ).
            
    

lp_years(Id, Years):- lifepath(Id, _, Years, _).

% born is a common enough cast that its worth handling specifically

is_born_lifepath(id(son_of_a_gun, _)). 
is_born_lifepath(id(Id, _)) :- 
    (atom_concat(born_, _, Id);
    atom_concat(_, "_born", Id)), 
    !.
    

requirement_is_constraint(constraint(_)).

% exists(Goal, List) checks if any item in the List satisfies Goal
% roughly the existential version of foreach.
exists(_, []) :- fail.
exists(Goal, [Head|Tail]) :-
    call(Goal, Head), !; exists(Goal, Tail).

% satisfy_requirement resolves individual lifepath requirements
satisfy_requirement(flag(FlagName), CharProps, Lifepaths) :-
    (
        exists([Lifepath]>>lifepath_provides(Lifepath, flag(FlagName)), Lifepaths)
        ; (memberchk(flag(FlagName), CharProps))       
    ), !.    

satisfy_requirement(trait(TraitName), _, Lifepaths) :-
    exists([Lifepath]>>lifepath_provides(Lifepath, trait(TraitName)), Lifepaths).

satisfy_requirement(lifepath(Lifepath), _, Lifepaths) :-
    member(id(Lifepath, _), Lifepaths), !.

satisfy_requirement(setting(Setting), _, Lifepaths) :-
    member(id(_, Setting), Lifepaths), !.   

satisfy_requirement(position(N), _, Lifepaths) :-
    length(Lifepaths, Len),
    Len =:= N - 1.

% boolean requirements operations
satisfy_requirement(not(Requirement), CharProps,  Lifepaths) :-
    \+ Requirement = constraint(_),
    \+ satisfy_requirement(Requirement, CharProps, Lifepaths).

satisfy_requirement(or(Requirements), CharProps, Lifepaths) :-
    exists(([Requirement]>>satisfy_requirement(Requirement, CharProps, Lifepaths)), Requirements).

satisfy_requirement(and(Requirements), CharProps, Lifepaths) :-
    foreach(member(Requirement, Requirements), satisfy_requirement(Requirement, CharProps, Lifepaths)).
    

% constraints are collected and checked later. skip them now.
satisfy_requirement(constraint(_), _, _). 

map_satreq(CharProps, Lifepaths, Constraint) :- 
    satisfy_requirement(Constraint, CharProps, Lifepaths).

satisfies_requirements(Lifepath, CharProps, ChosenLifepaths, Constraints) :-
    % lifepaths with no requirements are automatically satisfied
    (
        \+ lifepath_requires(Lifepath, _),
        Constraints = []
    ) 
    ;
    % otherwise satisfy the specified
    (
        findall(
            Requirements, (
                lifepath_requires(Lifepath, Requirements), 
                maplist(map_satreq(CharProps, ChosenLifepaths), Requirements)
            ), 
            AllReqs
        ),
        member(Reqs, AllReqs),
        findall(C, (member(R, Reqs), (constraint(C) = R)), Constraints)
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
available_lifepath([], _, Available, []) :- 
    lifepath(Available), 
    is_born_lifepath(Available).    

available_lifepath(ChosenLifepaths, CharProps, Available, Constraints) :-
    ChosenLifepaths = [LastLp|_],
    LastLp = id(_, LastSetting),
    lp_leads(LastLp, Leads),
    id(_, Setting) = Available,
    member(Setting, [LastSetting|Leads]),
    lifepath(Available), % confirm that the lifepath exists.
    \+ is_born_lifepath(Available),
    satisfies_requirements(Available, CharProps, ChosenLifepaths, Constraints).

character_age([], 0).
character_age([Lifepath|Lifepaths], Age) :-
    lp_years(Lifepath, Years),
    character_age(Lifepaths, PriorAge),
    Age is PriorAge + Years.


character_path_(_, [], _, []).
character_path_(CharProps, [First|Rest], Selected, Constraints) :-
    available_lifepath(Selected, CharProps, First, NewConstraints),
    append(NewConstraints, LaterConstraints, Constraints),
    character_path_(CharProps, Rest, [First|Selected], LaterConstraints).

normalize_lifepath_name(-(Name, Setting), id(Name, Setting)) :- !.
normalize_lifepath_name(id(Name, Setting), id(Name, Setting)) :- !.
normalize_lifepath_name(Name, Normalized) :-
    (
        findall(Setting, lifepath(id(Name, Setting), _, _, _), [Setting]),
        Normalized = id(Name, Setting), !
    ) ; throw(ambiguous_lifepath(Name)).


% currently we only support one type of prop, and that is flags
normalize_character_property(flag(Prop), flag(Prop)).
normalize_character_property(Prop, flag(Prop)).
    


% character_path(LifepathNames)
%
% character_path checks a set of lifepaths against the constraints.
% This is the main event.
character_path(_, []) :- fail.
character_path(CharProps, LifePaths) :- 
    maplist(normalize_lifepath_name, LifePaths, NormalizedLifePaths),
    maplist(normalize_character_property, CharProps, NormalizedProps),
    character_path_(NormalizedProps, NormalizedLifePaths, [], Constraints),
    satisfies_constraints(Constraints, NormalizedLifePaths).

character_path(Lifepaths) :- character_path([], Lifepaths).