:- module(gold_revised, [
    stock/1, 
    setting/2,
    lp/5,
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
lp(born_peasant, peasant, page(bwg, 163), 8, [servitude, soldier, sea, religious]).
lp(farmer, peasant, page(bwg, 163), 8, [villager, soldier, servitude]).
lp(head_of_household, peasant, page(bwg, 163), 15, [villager, soldier]).
lp(midwife, peasant, page(bwg, 163), 15, [villager, outcast]).
lp(lazy_stayabout, peasant, page(bwg, 163), 7, [outcast, servitude, soldier]).
lp(conscript, peasant, page(bwg, 163), 1, [servitude, soldier, outcast]).
lp(peasant_pilgrim, peasant, page(bwg, 163), 3, [outcast, servitude, villager]).
lp(miller, peasant, page(bwg, 163), 7, [villager]).
lp(fisherman, peasant, page(bwg, 163), 6, [villager, outcast, sea]).
lp(shepherd, peasant, page(bwg,163), 4, [villager, outcast]).

lp(woodcutter, peasant, page(bwg, 164), 5, [villager, outcast]).
lp(hunter, peasant, page(bwg, 164), 5, [villager, outcast]).
lp(trapper, peasant, page(bwg, 164), 5, [villager, outcast, soldier]).
lp(peddler, peasant, page(bwg, 164), 5, [villager, servitude, city, outcast]).
lp(elder, peasant, page(bwg, 164), 15, [villager, outcast]).
lp(auger, peasant, page(bwg, 164), 5, [servitude, outcast]).
lp(itinerant_priest, peasant, page(bwg, 164), 6, [villager, outcast, city, religious]).
lp(recluse_wizard, peasant, page(bwg, 164), 15, [outcast, villager, city, court]).
lp(country_wife, peasant, page(bwg, 164), 10, [religious]).

lp(village_born, villager, page(bwg, 165), 10, [peasant, servitude, sea, religious]).
lp(kid, villager, page(bwg, 165), 4, any_except([noble, court])).
lp(idiot, villager, page(bwg, 165), 10, [outcast, peasant]).
lp(pilgrim, villager, page(bwg, 166), 2, [religious, servitude, city]).
lp(conscript, villager, page(bwg, 166), 1, [servitude, soldier, outcast]).
lp(groom, villager, page(bwg, 166), 4, [peasant, city, soldier]).
lp(runner, villager, page(bwg, 166), 6, [city, peasant, soldier]).
lp(village_peddler, villager, page(bwg, 166), 5, [peasant, servitude, city, outcast]).
lp(shopkeeper, villager, page(bwg, 166), 6, [city, peasant]).

lp(serving_girl, villager, page(bwg, 167), 3, any_except([noble])).


% lp_requires(LifepathName, Requirements)
% Requirements is a list of requirements that must all be satisfied
% alternative requirements are specified with separate lp_requires facts.
%
% some requirements are 'constraint' structures. these are collected to
% be checked at the end of lifepath selection rather than during.
lp_requires(born_peasant, [born]).
lp_requires(head_of_household, [not(position(2))]).
lp_requires(elder, [constraint(min(age, 50))]).
lp_requires(midwife, [flag(female)]).
lp_requires(midwife, [lifepath(farmer)]).
lp_requires(midwife, [lifepath(itinerant_priest)]).
lp_requires(auger, [lifepath(midwife)]).
lp_requires(auger, [lifepath(country_wife)]).
lp_requires(auger, [flag(female), constraint(max(lifepaths, 3))]).
lp_requires(itinerant_priest, [flag(acolyte)]).
lp_requires(recluse_wizard, [skill(sorcery)]).

lp_requires(kid, [position(2), constraint(once(kid))]).

% lp_provides(LifepathName, Property)
lp_provides(farmer, trait(hoarding)).
lp_provides(midwife, trait(bedside_manner)).
lp_provides(lazy_stayabout, trait(a_little_fat)).
lp_provides(conscript, trait(flee_from_battle)).
lp_provides(peasant_pilgrim, trait(road_weary)).
lp_provides(peasant_pilgrim, trait(alms_taker)).
lp_provides(miller, trait(lords_favorite)).
lp_provides(fisherman, trait(superstitious)).
lp_provides(shepherd, trait(cry_wolf)).
lp_provides(trapper, trait(foul_smelling)).
lp_provides(peddler, trait(blank_stare)).
lp_provides(peddler, trait(glib)).
lp_provides(peddler, trait(eidetic_memory)).
lp_provides(itinerant_priest, trait(dusty)).
lp_provides(itinerant_priest, trait(faithful)).
lp_provides(recluse_wizard, trait(batshit)).
lp_provides(recluse_wizard, trait(gifted)).
lp_provides(country_wife, flag(female)).

lp_provides(kid, trait(bad_egg)).
lp_provides(kid, trait(good_for_nothing)).
lp_provides(kid, trait(fleet_of_foot)).
lp_provides(idiot, trait(problems)).
lp_provides(idiot, trait(alcoholic)).
lp_provides(idiot, trait(abused)).
lp_provides(idiot, trait(handicapped)).

lp_provides(serving_girl, flag(female)).
lp_provides(serving_girl, trait(bored)).
lp_provides(serving_girl, trait(bitter)).
lp_provides(serving_girl, trait(comely)).