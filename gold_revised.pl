:- module(gold_revised, [
    stock/1, 
    setting/2,
    lifepath/4,
    lifepath_requires/2,
    lifepath_provides/2,
    book/2
    ]
).

:- discontiguous([
    lifepath/4,
    lifepath_provides/2,
    lifepath_requires/2
]).


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

% lifepath(id(LifepathName, SettingName), PageNumber, Years, Leads)
% lifepath defines a basic lifepath: its name, its setting (which transitively defines a stock), 
% and the pagenumber for reference (page(book, page))

% lifepath_requires(LifepathName, Requirements)
% Requirements is a list of requirements that must all be satisfied
% alternative requirements are specified with separate lifepath_requires facts.
%
% some requirements are 'constraint' structures. these are collected to
% be checked at the end of lifepath selection rather than during.

% lifepath_provides(id(LifepathName, Setting), Property)
%
% associate a property structure with a give lifepath. These properties are used
% to satisfy requirements defined above. Theres are a number of properties that 
% can be provided: trait and skill are obvious. Flags are less obvious, 
% a flag, such as female, is some arbirary symbolic property. 
%
% Why is female a flag? Taking some lifepaths, such as country_wife, implicitly
% indicate that the character is female. Alternatively you can supply flags 
% to the program separately from target lifepaths


%%% ----- PEASANT SETTING ----- %%%

lifepath(id(born_peasant, peasant), page(bwg, 163), 8, [servitude, soldier, sea, religious]).

lifepath(id(farmer, peasant), page(bwg, 163), 8, [villager, soldier, servitude]).
lifepath_provides(id(farmer, peasant), trait(hoarding)).
lifepath_provides(id(farmer, peasant), skill(farming)).
lifepath_provides(id(farmer, peasant), skill(mending)).
lifepath_provides(id(farmer, peasant), skill(animal_husbandry)).
lifepath_provides(id(farmer, peasant), skill(weaving)).
lifepath_provides(id(farmer, peasant), skill(cooking)).
lifepath_provides(id(farmer, peasant), skill(sewing)).
lifepath_provides(id(farmer, peasant), skill(firebuilding)).
lifepath_provides(id(farmer, peasant), skill(sing)).

lifepath(id(head_of_household, peasant), page(bwg, 163), 15, [villager, soldier]).
lifepath_requires(id(head_of_household, peasant), [not(position(2))]).
lifepath_provides(id(head_of_household, peasant), skill(carpentry)).
lifepath_provides(id(head_of_household, peasant), skill(hunting)).
lifepath_provides(id(head_of_household, peasant), skill(haggling)).
lifepath_provides(id(head_of_household, peasant), skill(almanac)).

lifepath(id(midwife, peasant), page(bwg, 163), 15, [villager, outcast]).
lifepath_requires(id(midwife, peasant), [flag(female)]).
lifepath_requires(id(midwife, peasant), [lifepath(farmer)]).
lifepath_requires(id(midwife, peasant), [lifepath(itinerant_priest)]).
lifepath_provides(id(midwife, peasant), trait(bedside_manner)).
lifepath_provides(id(midwife, peasant), skill(animal_husbandry)).
lifepath_provides(id(midwife, peasant), skill(herbalism)).
lifepath_provides(id(midwife, peasant), skill(midwifery)).
lifepath_provides(id(midwife, peasant), skill(omen_wise)).

lifepath(id(lazy_stayabout, peasant), page(bwg, 163), 7, [outcast, servitude, soldier]).
lifepath_provides(id(lazy_stayabout, peasant), trait(a_little_fat)).
lifepath_provides(id(lazy_stayabout, peasant), skill(lazy_wise)).
lifepath_provides(id(lazy_stayabout, peasant), skill(peasant_wise)).
lifepath_provides(id(lazy_stayabout, peasant), skill(wife_wise)).
lifepath_provides(id(lazy_stayabout, peasant), skill(work_wise)).

lifepath(id(conscript, peasant), page(bwg, 163), 1, [servitude, soldier, outcast]).
lifepath_provides(id(conscript, peasant), trait(flee_from_battle)).
lifepath_provides(id(conscript, peasant), skill(foraging)).
lifepath_provides(id(conscript, peasant), skill(battle_wise)).
lifepath_provides(id(conscript, peasant), skill(rumor_wise)).

lifepath(id(peasant_pilgrim, peasant), page(bwg, 163), 3, [outcast, servitude, villager]).
lifepath_provides(id(peasant_pilgrim, peasant), trait(road_weary)).
lifepath_provides(id(peasant_pilgrim, peasant), trait(alms_taker)).
lifepath_provides(id(peasant_pilgrim, peasant), skill(doctrine)).
lifepath_provides(id(peasant_pilgrim, peasant), skill(pilgrimage_wise)).
lifepath_provides(id(peasant_pilgrim, peasant), skill(saint_wise)).

lifepath(id(miller, peasant), page(bwg, 163), 7, [villager]).
lifepath_provides(id(miller, peasant), trait(lords_favorite)).
lifepath_provides(id(miller, peasant), skill(miller)).
lifepath_provides(id(miller, peasant), skill(brewer)).
lifepath_provides(id(miller, peasant), skill(mending)).
lifepath_provides(id(miller, peasant), skill(carpentry)).

lifepath(id(fisherman, peasant), page(bwg, 163), 6, [villager, outcast, sea]).
lifepath_provides(id(fisherman, peasant), trait(superstitious)).
lifepath_provides(id(fisherman, peasant), skill(fishing)). 
lifepath_provides(id(fisherman, peasant), skill(rigging)).
lifepath_provides(id(fisherman, peasant), skill(knots)).
lifepath_provides(id(fisherman, peasant), skill(mending)).
lifepath_provides(id(fisherman, peasant), skill(cooking)).
lifepath_provides(id(fisherman, peasant), skill(boarwright)).

lifepath(id(shepherd, peasant), page(bwg,163), 4, [villager, outcast]).
lifepath_provides(id(shepherd, peasant), trait(cry_wolf)).
lifepath_provides(id(shepherd, peasant), skill(animal_husbandry)).
lifepath_provides(id(shepherd, peasant), skill(sing)).
lifepath_provides(id(shepherd, peasant), skill(climbing)).
lifepath_provides(id(shepherd, peasant), skill(flute)).
lifepath_provides(id(shepherd, peasant), skill(throwing)).

lifepath(id(woodcutter, peasant), page(bwg, 164), 5, [villager, outcast]).
lifepath_provides(id(woodcutter, peasant), skill(firebuilding)).
lifepath_provides(id(woodcutter, peasant), skill(mending)).
lifepath_provides(id(woodcutter, peasant), skill(foraging)).
lifepath_provides(id(woodcutter, peasant), skill(orienteering)).
lifepath_provides(id(woodcutter, peasant), skill(tree_wise)).

lifepath(id(hunter, peasant), page(bwg, 164), 5, [villager, outcast]).
lifepath_provides(id(hunter, peasant), skill(hunting)).
lifepath_provides(id(hunter, peasant), skill(tracking)).
lifepath_provides(id(hunter, peasant), skill(stealthy)).
lifepath_provides(id(hunter, peasant), skill(cooking)).
lifepath_provides(id(hunter, peasant), skill(orienteering)).
lifepath_provides(id(hunter, peasant), skill(javelin)).
lifepath_provides(id(hunter, peasant), skill(bow)).

lifepath(id(trapper, peasant), page(bwg, 164), 5, [villager, outcast, soldier]).
lifepath_provides(id(trapper, peasant), trait(foul_smelling)).
lifepath_provides(id(trapper, peasant), skill(trapper)).
lifepath_provides(id(trapper, peasant), skill(stealthy)).
lifepath_provides(id(trapper, peasant), skill(tracking)).
lifepath_provides(id(trapper, peasant), skill(cooking)).
lifepath_provides(id(trapper, peasant), skill(haggling)).
lifepath_provides(id(trapper, peasant), skill(taxidermy)).

lifepath(id(peddler, peasant), page(bwg, 164), 5, [villager, servitude, city, outcast]).
lifepath_provides(id(peddler, peasant), trait(blank_stare)).
lifepath_provides(id(peddler, peasant), trait(glib)).
lifepath_provides(id(peddler, peasant), trait(eidetic_memory)).
lifepath_provides(id(peddler, peasant), skill(mending)).
lifepath_provides(id(peddler, peasant), skill(sing)).
lifepath_provides(id(peddler, peasant), skill(haggling)).
lifepath_provides(id(peddler, peasant), skill(chandler)).
lifepath_provides(id(peddler, peasant), skill(persuasion)).
lifepath_provides(id(peddler, peasant), skill(inconspicuous)).
lifepath_provides(id(peddler, peasant), skill(falsehood)).

lifepath(id(elder, peasant), page(bwg, 164), 15, [villager, outcast]).
lifepath_requires(id(elder, peasant), [constraint(min(age, 50))]).
lifepath_provides(id(elder, peasant), trait(crotechty)).
lifepath_provides(id(elder, peasant), skill(observation)).
lifepath_provides(id(elder, peasant), skill(persuasion)).
lifepath_provides(id(elder, peasant), skill(ugly_truth)).
lifepath_provides(id(elder, peasant), skill(peasant_wise)).
lifepath_provides(id(elder, peasant), skill(local_history)).

lifepath(id(auger, peasant), page(bwg, 164), 5, [servitude, outcast]).
lifepath_requires(id(auger, peasant), [lifepath(midwife)]).
lifepath_requires(id(auger, peasant), [lifepath(country_wife)]).
lifepath_requires(id(auger, peasant), [flag(female), constraint(max(lifepaths, 3))]).
lifepath_provides(id(auger, peasant), trait(disturbed)).
lifepath_provides(id(auger, peasant), trait(dreamer)).
lifepath_provides(id(auger, peasant), trait(cassandra)).
lifepath_provides(id(auger, peasant), trait(touch_of_ages)).
lifepath_provides(id(auger, peasant), skill(astrology)).
lifepath_provides(id(auger, peasant), skill(sorcery)).
lifepath_provides(id(auger, peasant), skill(falsehood)).
lifepath_provides(id(auger, peasant), skill(ugly_truth)).
lifepath_provides(id(auger, peasant), skill(omen_wise)).

lifepath(id(itinerant_priest, peasant), page(bwg, 164), 6, [villager, outcast, city, religious]).
lifepath_requires(id(itinerant_priest, peasant), [flag(acolyte)]).
lifepath_provides(id(itinerant_priest, peasant), trait(dusty)).
lifepath_provides(id(itinerant_priest, peasant), trait(faithful)).
lifepath_provides(id(itinerant_priest, peasant), skill(oratory)).
lifepath_provides(id(itinerant_priest, peasant), skill(suasion)).
lifepath_provides(id(itinerant_priest, peasant), skill(chandler)).
lifepath_provides(id(itinerant_priest, peasant), skill(riding)).
lifepath_provides(id(itinerant_priest, peasant), skill(write)).
lifepath_provides(id(itinerant_priest, peasant), skill(read)).
lifepath_provides(id(itinerant_priest, peasant), skill(doctrine)).

lifepath(id(recluse_wizard, peasant), page(bwg, 164), 15, [outcast, villager, city, court]).
lifepath_requires(id(recluse_wizard, peasant), [skill(sorcery)]).
lifepath_provides(id(recluse_wizard, peasant), skill(astrology)).
lifepath_provides(id(recluse_wizard, peasant), skill(alchemy)).
lifepath_provides(id(recluse_wizard, peasant), skill(enchanting)).
lifepath_provides(id(recluse_wizard, peasant), skill(illuminations)).
lifepath_provides(id(recluse_wizard, peasant), skill(ancient_history)).
lifepath_provides(id(recluse_wizard, peasant), skill(obscure_history)).
lifepath_provides(id(recluse_wizard, peasant), trait(batshit)).
lifepath_provides(id(recluse_wizard, peasant), trait(gifted)).

lifepath(id(country_wife, peasant), page(bwg, 164), 10, [religious]).
lifepath_provides(id(country_wife, peasant), flag(female)).
lifepath_provides(id(country_wife, peasant), skill(child_rearing)).
lifepath_provides(id(country_wife, peasant), skill(cooking)).


%%% ----- VILLAGER SETTING ----- %%%
lifepath(id(village_born, villager), page(bwg, 165), 10, [peasant, servitude, sea, religious]).
lifepath(id(kid, villager), page(bwg, 165), 4, any_except([noble, court])).
lifepath_requires(id(kid, villager), [position(2), not(lifepath(kid))]).
lifepath_provides(id(kid, villager), trait(bad_egg)).
lifepath_provides(id(kid, villager), trait(good_for_nothing)).
lifepath_provides(id(kid, villager), trait(fleet_of_foot)).

lifepath(id(idiot, villager), page(bwg, 165), 10, [outcast, peasant]).
lifepath_provides(id(idiot, villager), trait(problems)).
lifepath_provides(id(idiot, villager), trait(alcoholic)).
lifepath_provides(id(idiot, villager), trait(abused)).
lifepath_provides(id(idiot, villager), trait(handicapped)).

lifepath(id(pilgrim, villager), page(bwg, 166), 2, [religious, servitude, city]).
lifepath(id(conscript, villager), page(bwg, 166), 1, [servitude, soldier, outcast]).
lifepath(id(groom, villager), page(bwg, 166), 4, [peasant, city, soldier]).
lifepath(id(runner, villager), page(bwg, 166), 6, [city, peasant, soldier]).
lifepath(id(village_peddler, villager), page(bwg, 166), 5, [peasant, servitude, city, outcast]).
lifepath(id(shopkeeper, villager), page(bwg, 166), 6, [city, peasant]).
lifepath(id(clerk, villager), page(bwg, 166), 4, [city, outcast, soldier]).
lifepath(id(sailor, villager), page(bwg, 166), 5, [soldier, city, peasant, servitude, sea]).
lifepath(id(laborer, villager), page(bwg, 166), 4, [soldier, outcast, servitude, peasant]).
lifepath(id(miner, villager), page(bwg, 166), 3, [soldier, outcast, servitude, peasant]).
lifepath(id(taskmaster, villager), page(bwg, 166), 6, [city, outcast, soldier]).

lifepath(id(serving_girl, villager), page(bwg, 167), 3, any_except([noble])).
lifepath_provides(id(serving_girl, villager), flag(female)).
lifepath_provides(id(serving_girl, villager), trait(bored)).
lifepath_provides(id(serving_girl, villager), trait(bitter)).
lifepath_provides(id(serving_girl, villager), trait(comely)).

lifepath(id(hosteller, villager), page(bwg, 167), 6, [city, peasant]).
lifepath(id(village_tough, villager), page(bwg, 167), 3, [soldier, city, peasant, servitude]).
lifepath(id(village_sergeant, villager), page(bwg, 167), 5, [soldier, servitude, outcast]).
lifepath(id(corrupt_sergeant, villager), page(bwg, 167), 5, [soldier, servitude, court]).
lifepath(id(tailor, villager), page(bwg, 167), 5, [city, peasant]).
lifepath(id(tax_collector, villager), page(bwg, 167), 5, [city, peasant, outcast]).
lifepath(id(cobbler, villager), page(bwg, 167), 8, [city, peasant, soldier]).
lifepath(id(farrier, villager), page(bwg, 167), 5, [peasant, soldier, city]).

lifepath(id(butcher, villager), page(bwg, 168), 6, [city, peasant, outcast]).
lifepath(id(barber, villager), page(bwg, 168), 7, [city, peasant, outcast]).
lifepath(id(brewer, villager), page(bwg, 168), 8, [city, peasant, court]).
lifepath(id(acolyte, villager), page(bwg, 168), 7, [peasant, servitude, city, religious]).
lifepath_provides(id(acolyte, villager), flag(acolyte)).

lifepath(id(failed_acolyte, villager), page(bwg, 168), 6, [outcast, soldier, peasant]).
lifepath(id(village_priest, villager), page(bwg, 168), 8, any_except([noble, court])).
lifepath(id(venal_priest, villager), page(bwg, 168), 9, any_except([noble, court])).
lifepath(id(apprentice, villager), page(bwg, 168), 7, [city, peasant, soldier, sea]).
lifepath(id(journeyman, villager), page(bwg, 168), 6, [city, peasant, soldier, sea]).
lifepath(id(cloth_dyer, villager), page(bwg, 168), 5, [city, peasant]).

lifepath(id(bowyer, villager), page(bwg, 169), 6, [soldier, outcast, peasant]).
lifepath(id(master_craftsman, villager), page(bwg, 169), 10, [city, soldier]).
lifepath(id(vintner, villager), page(bwg, 169), 10, [court, peasant]).
lifepath(id(apiarist, villager), page(bwg, 169), 8, [city, peasant, court]).
lifepath(id(mining_engineer, villager), page(bwg, 169), 8, [city, court, soldier]).
lifepath(id(town_official, villager), page(bwg, 169), 5, [city, outcast, soldier]).
lifepath(id(merchant, villager), page(bwg, 169), 7, [city, outcast, sea]).
lifepath(id(village_wife, villager), page(bwg, 169), 8, [religious, city, servitude]).

