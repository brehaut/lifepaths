:- module(gold_revised, [
    stock/1, 
    setting/2,
    lp/4,
    lp_requires/2,
    lp_provides/2,
    book/2
    ]
).

% book(BookName, LifepathRanges).
% defines a book, used for referencing pages.
% start and end are the pages that contain lifepaths
book(bwg, [range(110, 116), range(131, 137), range(163, 196), range(228, 233)]).
book(codex, []).
book(anthology, []).

% stock(StockName)
% defines a stock for lifepaths to exist in.
stock(human).
stock(elf).
stock(dwarf).
stock(orc).
stock(troll).
stock(roden).
stock(great_wolf).

% setting(SettingName, StockName).
setting(peasant, human).
setting(villager, human).
setting(city, human).
setting(noble, human).
setting(court, human).
setting(religious, human).
setting(soldier, human).
setting(sea, human).
setting(servitude, human).
setting(outcast, human).

% lp(LifepathName, SettingName, PageNumber, Years, Leads)
% lp defines a basic lifepath: its name, its setting (which transitively defines a stock), 
% and the pagenumber for reference (page(book, page))
lp(id(born_peasant, peasant), page(bwg, 163), 8, [servitude, soldier, sea, religious]).
lp(id(farmer, peasant), page(bwg, 163), 8, [villager, soldier, servitude]).
lp(id(head_of_household, peasant), page(bwg, 163), 15, [villager, soldier]).
lp(id(midwife, peasant), page(bwg, 163), 15, [villager, outcast]).
lp(id(lazy_stayabout, peasant), page(bwg, 163), 7, [outcast, servitude, soldier]).
lp(id(conscript, peasant), page(bwg, 163), 1, [servitude, soldier, outcast]).
lp(id(peasant_pilgrim, peasant), page(bwg, 163), 3, [outcast, servitude, villager]).
lp(id(miller, peasant), page(bwg, 163), 7, [villager]).
lp(id(fisherman, peasant), page(bwg, 163), 6, [villager, outcast, sea]).
lp(id(shepherd, peasant), page(bwg,163), 4, [villager, outcast]).

lp(id(woodcutter, peasant), page(bwg, 164), 5, [villager, outcast]).
lp(id(hunter, peasant), page(bwg, 164), 5, [villager, outcast]).
lp(id(trapper, peasant), page(bwg, 164), 5, [villager, outcast, soldier]).
lp(id(peddler, peasant), page(bwg, 164), 5, [villager, servitude, city, outcast]).
lp(id(elder, peasant), page(bwg, 164), 15, [villager, outcast]).
lp(id(auger, peasant), page(bwg, 164), 5, [servitude, outcast]).
lp(id(itinerant_priest, peasant), page(bwg, 164), 6, [villager, outcast, city, religious]).
lp(id(recluse_wizard, peasant), page(bwg, 164), 15, [outcast, villager, city, court]).
lp(id(country_wife, peasant), page(bwg, 164), 10, [religious]).

lp(id(village_born, villager), page(bwg, 165), 10, [peasant, servitude, sea, religious]).
lp(id(kid, villager), page(bwg, 165), 4, any_except([noble, court])).
lp(id(idiot, villager), page(bwg, 165), 10, [outcast, peasant]).
lp(id(pilgrim, villager), page(bwg, 166), 2, [religious, servitude, city]).
lp(id(conscript, villager), page(bwg, 166), 1, [servitude, soldier, outcast]).
lp(id(groom, villager), page(bwg, 166), 4, [peasant, city, soldier]).
lp(id(runner, villager), page(bwg, 166), 6, [city, peasant, soldier]).
lp(id(village_peddler, villager), page(bwg, 166), 5, [peasant, servitude, city, outcast]).
lp(id(shopkeeper, villager), page(bwg, 166), 6, [city, peasant]).

lp(id(serving_girl, villager), page(bwg, 167), 3, any_except([noble])).


% lp_requires(LifepathName, Requirements)
% Requirements is a list of requirements that must all be satisfied
% alternative requirements are specified with separate lp_requires facts.
%
% some requirements are 'constraint' structures. these are collected to
% be checked at the end of lifepath selection rather than during.
lp_requires(id(head_of_household, peasant), [not(position(2))]).
lp_requires(id(elder, peasant), [constraint(min(age, 50))]).
lp_requires(id(midwife, peasant), [flag(female)]).
lp_requires(id(midwife, peasant), [lifepath(farmer)]).
lp_requires(id(midwife, peasant), [lifepath(itinerant_priest)]).
lp_requires(id(auger, peasant), [lifepath(midwife)]).
lp_requires(id(auger, peasant), [lifepath(country_wife)]).
lp_requires(id(auger, peasant), [flag(female), constraint(max(lifepaths, 3))]).
lp_requires(id(itinerant_priest, peasant), [flag(acolyte)]).
lp_requires(id(recluse_wizard, peasant), [skill(sorcery)]).

lp_requires(id(kid, villager), [position(2), constraint(once(kid))]).

% lp_provides(LifepathName, Property)
lp_provides(id(farmer, peasant), trait(hoarding)).
lp_provides(id(midwife, peasant), trait(bedside_manner)).
lp_provides(id(lazy_stayabout, peasant), trait(a_little_fat)).
lp_provides(id(conscript, peasant), trait(flee_from_battle)).
lp_provides(id(peasant_pilgrim, peasant), trait(road_weary)).
lp_provides(id(peasant_pilgrim, peasant), trait(alms_taker)).
lp_provides(id(miller, peasant), trait(lords_favorite)).
lp_provides(id(fisherman, peasant), trait(superstitious)).
lp_provides(id(shepherd, peasant), trait(cry_wolf)).
lp_provides(id(trapper, peasant), trait(foul_smelling)).
lp_provides(id(peddler, peasant), trait(blank_stare)).
lp_provides(id(peddler, peasant), trait(glib)).
lp_provides(id(peddler, peasant), trait(eidetic_memory)).
lp_provides(id(itinerant_priest, peasant), trait(dusty)).
lp_provides(id(itinerant_priest, peasant), trait(faithful)).
lp_provides(id(recluse_wizard, peasant), trait(batshit)).
lp_provides(id(recluse_wizard, peasant), trait(gifted)).
lp_provides(id(country_wife, peasant), flag(female)).

lp_provides(id(kid, villager), trait(bad_egg)).
lp_provides(id(kid, villager), trait(good_for_nothing)).
lp_provides(id(kid, villager), trait(fleet_of_foot)).
lp_provides(id(idiot, villager), trait(problems)).
lp_provides(id(idiot, villager), trait(alcoholic)).
lp_provides(id(idiot, villager), trait(abused)).
lp_provides(id(idiot, villager), trait(handicapped)).

lp_provides(id(serving_girl, villager), flag(female)).
lp_provides(id(serving_girl, villager), trait(bored)).
lp_provides(id(serving_girl, villager), trait(bitter)).
lp_provides(id(serving_girl, villager), trait(comely)).