:- module(gold_revised, [
    stock/1, 
    setting/2,
    lifepath/4,
    lifepath_requires/2,
    lifepath_provides/2,
    book/2,
    flag/1
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

% flags are used for special cases of 
flag(female).
flag(acolyte).
flag(guard).

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
lifepath_provides(id(kid, villager), skill(trouble_wise)).
lifepath_provides(id(kid, villager), skill(inconspicuous)).

lifepath(id(idiot, villager), page(bwg, 165), 10, [outcast, peasant]).
lifepath_provides(id(idiot, villager), trait(problems)).
lifepath_provides(id(idiot, villager), trait(alcoholic)).
lifepath_provides(id(idiot, villager), trait(abused)).
lifepath_provides(id(idiot, villager), trait(handicapped)).
lifepath_provides(id(idiot, villager), skill(inconspicuous)).
lifepath_provides(id(idiot, villager), skill(conspicuous)).
lifepath_provides(id(idiot, villager), skill(ugly_truth)).
lifepath_provides(id(idiot, villager), skill(village_secrets_wise)).

lifepath(id(pilgrim, villager), page(bwg, 166), 2, [religious, servitude, city]).
lifepath_provides(id(pilgrim, villager), trait(collector)).
lifepath_provides(id(pilgrim, villager), skill(religious_rumor_wise)).
lifepath_provides(id(pilgrim, villager), skill(road_wise)).
lifepath_provides(id(pilgrim, villager), skill(shine_wise)).
lifepath_provides(id(pilgrim, villager), skill(alms_wise)).

lifepath(id(conscript, villager), page(bwg, 166), 1, [servitude, soldier, outcast]).
lifepath_provides(id(conscript, villager), trait(hide_before_battle)).
lifepath_provides(id(conscript, villager), skill(foraging)).
lifepath_provides(id(conscript, villager), skill(baggage_train_wise)).

lifepath(id(groom, villager), page(bwg, 166), 4, [peasant, city, soldier]).
lifepath_provides(id(groom, villager), skill(animal_husbandry)).
lifepath_provides(id(groom, villager), skill(riding)).
lifepath_provides(id(groom, villager), skill(mending)).
lifepath_provides(id(groom, villager), skill(horse_wise)).
lifepath_provides(id(groom, villager), skill(road_wise)).

lifepath(id(runner, villager), page(bwg, 166), 6, [city, peasant, soldier]).
lifepath_provides(id(runner, villager), trait(skinny)).
lifepath_provides(id(runner, villager), trait(fleet_of_foot)).
lifepath_provides(id(runner, villager), skill(streetwise)).
lifepath_provides(id(runner, villager), skill(inconspicuous)).
lifepath_provides(id(runner, villager), skill(shortcut_wise)).

lifepath(id(village_peddler, villager), page(bwg, 166), 5, [peasant, servitude, city, outcast]).
lifepath_provides(id(village_peddler, villager), trait(odd)).
lifepath_provides(id(village_peddler, villager), skill(mending)).
lifepath_provides(id(village_peddler, villager), skill(sing)).
lifepath_provides(id(village_peddler, villager), skill(haggling)).
lifepath_provides(id(village_peddler, villager), skill(chandler)).
lifepath_provides(id(village_peddler, villager), skill(persuasion)).
lifepath_provides(id(village_peddler, villager), skill(inconspicuous)).
lifepath_provides(id(village_peddler, villager), skill(falsehood)).

lifepath(id(shopkeeper, villager), page(bwg, 166), 6, [city, peasant]).
lifepath_provides(id(shopkeeper, villager), skill(haggling)).
lifepath_provides(id(shopkeeper, villager), skill(accounting)).
lifepath_provides(id(shopkeeper, villager), skill(observation)).
lifepath_provides(id(shopkeeper, villager), skill(merchant_wise)).

lifepath(id(clerk, villager), page(bwg, 166), 4, [city, outcast, soldier]).
lifepath_provides(id(clerk, villager), trait(cramped_hands)).
lifepath_provides(id(clerk, villager), trait(mind_for_small_details)).
lifepath_provides(id(clerk, villager), skill(bureaucracy)).
lifepath_provides(id(clerk, villager), skill(write)).
lifepath_provides(id(clerk, villager), skill(read)).
lifepath_provides(id(clerk, villager), skill(contract_wise)).

lifepath(id(sailor, villager), page(bwg, 166), 5, [soldier, city, peasant, servitude, sea]).
lifepath_provides(id(sailor, villager), trait(superstitious)).
lifepath_provides(id(sailor, villager), trait(sea_legs)).
lifepath_provides(id(sailor, villager), skill(rigging)).
lifepath_provides(id(sailor, villager), skill(knots)).
lifepath_provides(id(sailor, villager), skill(brawling)).
lifepath_provides(id(sailor, villager), skill(mending)).
lifepath_provides(id(sailor, villager), skill(sing)).
lifepath_provides(id(sailor, villager), skill(fishing)).

lifepath(id(laborer, villager), page(bwg, 166), 4, [soldier, outcast, servitude, peasant]).
lifepath_provides(id(laborer, villager), trait(calloused)).
lifepath_provides(id(laborer, villager), trait(starved)).
lifepath_provides(id(laborer, villager), trait(broken)).
lifepath_provides(id(laborer, villager), trait(hardened)).
lifepath_provides(id(laborer, villager), trait(numb)).
lifepath_provides(id(laborer, villager), skill(ditch_digging)).
lifepath_provides(id(laborer, villager), skill(latrine_wise)).

lifepath(id(miner, villager), page(bwg, 166), 3, [soldier, outcast, servitude, peasant]).
lifepath_requires(id(miner, villager), [lifepath(laborer)]).
lifepath_requires(id(miner, villager), [lifepath(conscript)]).
lifepath_requires(id(miner, villager), [lifepath(farmer)]).
lifepath_requires(id(miner, villager), [lifepath(foot_soldier)]).
lifepath_provides(id(miner, villager), trait(black_lung)).
lifepath_provides(id(miner, villager), trait(drunk)).
lifepath_provides(id(miner, villager), trait(superstitious)).
lifepath_provides(id(miner, villager), skill(mining)).
lifepath_provides(id(miner, villager), skill(cave_in_wise)).

lifepath(id(taskmaster, villager), page(bwg, 166), 6, [city, outcast, soldier]).
lifepath_requires(id(taskmaster, villager), [lifepath(village_sergeant)]).
lifepath_requires(id(taskmaster, villager), [setting(soldier)]).
lifepath_provides(id(taskmaster, villager), trait(hard_hearted)).
lifepath_provides(id(taskmaster, villager), trait(mean)).
lifepath_provides(id(taskmaster, villager), trait(barker)).
lifepath_provides(id(taskmaster, villager), trait(booming_voice)).
lifepath_provides(id(taskmaster, villager), skill(intimidation)).
lifepath_provides(id(taskmaster, villager), skill(brawling)).
lifepath_provides(id(taskmaster, villager), skill(sing)).
lifepath_provides(id(taskmaster, villager), skill(conspicuous)).
lifepath_provides(id(taskmaster, villager), skill(lazy_bastard_wise)).

lifepath(id(serving_girl, villager), page(bwg, 167), 3, any_except([noble])).
lifepath_provides(id(serving_girl, villager), flag(female)).
lifepath_provides(id(serving_girl, villager), trait(bored)).
lifepath_provides(id(serving_girl, villager), trait(bitter)).
lifepath_provides(id(serving_girl, villager), trait(comely)).
lifepath_provides(id(serving_girl, villager), skill(soothing_platitudes)).
lifepath_provides(id(serving_girl, villager), skill(ugly_truth)).
lifepath_provides(id(serving_girl, villager), skill(customer_wise)).
lifepath_provides(id(serving_girl, villager), skill(sleight_of_hand)).

lifepath(id(hosteller, villager), page(bwg, 167), 6, [city, peasant]).
lifepath_requires(id(hosteller, villager), [not(position(2))]).
lifepath_provides(id(hosteller, villager), trait(fixed_smile)).
lifepath_provides(id(hosteller, villager), trait(gossip)).
lifepath_provides(id(hosteller, villager), skill(cooking)).
lifepath_provides(id(hosteller, villager), skill(mending)).
lifepath_provides(id(hosteller, villager), skill(accounting)).
lifepath_provides(id(hosteller, villager), skill(soothing_platitudes)).
lifepath_provides(id(hosteller, villager), skill(guest_wise)).

lifepath(id(village_tough, villager), page(bwg, 167), 3, [soldier, city, peasant, servitude]).
lifepath_provides(id(village_tough, villager), trait(thug)).
lifepath_provides(id(village_tough, villager), skill(appropriate_weapons)).
lifepath_provides(id(village_tough, villager), skill(intimidation)).
lifepath_provides(id(village_tough, villager), skill(brawling)).
lifepath_provides(id(village_tough, villager), skill(graft_wise)).

lifepath(id(village_sergeant, villager), page(bwg, 167), 5, [soldier, servitude, outcast]).
lifepath_requires(id(village_sergeant, villager), [lifepath(village_tough)]).
lifepath_requires(id(village_sergeant, villager), [lifepath(squire)]).
lifepath_requires(id(village_sergeant, villager), [lifepath(freebooter)]).
lifepath_requires(id(village_sergeant, villager), [lifepath(sergant_at_arms)]).
lifepath_requires(id(village_sergeant, villager), [lifepath(man_at_arms)]).
lifepath_provides(id(village_sergeant, villager), trait(overworked)).
lifepath_provides(id(village_sergeant, villager), trait(underpaid)).
lifepath_provides(id(village_sergeant, villager), skill(command)).
lifepath_provides(id(village_sergeant, villager), skill(intimidation)).
lifepath_provides(id(village_sergeant, villager), skill(field_dressing)).
lifepath_provides(id(village_sergeant, villager), skill(appropriate_weapons)).
lifepath_provides(id(village_sergeant, villager), skill(shield_training)).
lifepath_provides(id(village_sergeant, villager), skill(armor_training)).
lifepath_provides(id(village_sergeant, villager), skill(bribe_wise)).
lifepath_provides(id(village_sergeant, villager), skill(wealthy_wise)).

lifepath(id(corrupt_sergeant, villager), page(bwg, 167), 5, [soldier, servitude, court]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(village_tough)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(squire)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(freebooter)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(sergant_at_arms)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(man_at_arms)]).
lifepath_provides(id(corrupt_sergeant, villager), trait(predatory)).
lifepath_provides(id(corrupt_sergeant, villager), skill(intimidation)).
lifepath_provides(id(corrupt_sergeant, villager), skill(appropriate_weapons)).
lifepath_provides(id(corrupt_sergeant, villager), skill(shield_training)).
lifepath_provides(id(corrupt_sergeant, villager), skill(armor_training)).
lifepath_provides(id(corrupt_sergeant, villager), skill(bribe_wise)).
lifepath_provides(id(corrupt_sergeant, villager), skill(wealthy_wise)).
lifepath_provides(id(corrupt_sergeant, villager), skill(criminal_wise)).

lifepath(id(tailor, villager), page(bwg, 167), 5, [city, peasant]).
lifepath_provides(id(tailor, villager), trait(frippery)).
lifepath_provides(id(tailor, villager), skill(sewing)).
lifepath_provides(id(tailor, villager), skill(embroidery)).
lifepath_provides(id(tailor, villager), skill(clothing_wise)).

lifepath(id(tax_collector, villager), page(bwg, 167), 5, [city, peasant, outcast]).
lifepath_requires(id(tax_collector, villager), [not(position(2))]).
lifepath_provides(id(tax_collector, villager), trait(hard_hearted)).
lifepath_provides(id(tax_collector, villager), trait(manhunter)).
lifepath_provides(id(tax_collector, villager), skill(intimidation)).
lifepath_provides(id(tax_collector, villager), skill(accounting)).
lifepath_provides(id(tax_collector, villager), skill(haggling)).
lifepath_provides(id(tax_collector, villager), skill(persuasion)).

lifepath(id(cobbler, villager), page(bwg, 167), 8, [city, peasant, soldier]).
lifepath_requires(id(cobbler, villager), [lifepath(apprentice)]).
lifepath_provides(id(cobbler, villager), trait(comfortable_shoes)).
lifepath_provides(id(cobbler, villager), skill(cobbler)).
lifepath_provides(id(cobbler, villager), skill(shoe_wise)).
lifepath_provides(id(cobbler, villager), skill(feet_wise)).
lifepath_provides(id(cobbler, villager), skill(leather_wise)).
lifepath_provides(id(cobbler, villager), skill(nail_wise)).

lifepath(id(farrier, villager), page(bwg, 167), 5, [peasant, soldier, city]).
lifepath_requires(id(farrier, villager), [lifepath(apprentice)]).
lifepath_provides(id(farrier, villager), trait(a_bit_deaf)).
lifepath_provides(id(farrier, villager), skill(blacksmith)).
lifepath_provides(id(farrier, villager), skill(animal_husbandry)).
lifepath_provides(id(farrier, villager), skill(horse_wise)).
lifepath_provides(id(farrier, villager), skill(haggling)).

lifepath(id(butcher, villager), page(bwg, 168), 6, [city, peasant, outcast]).
lifepath_provides(id(butcher, villager), trait(prominent_scar)).
lifepath_provides(id(butcher, villager), trait(thick_skin)).
lifepath_provides(id(butcher, villager), trait(stinky)).
lifepath_provides(id(butcher, villager), trait(muttering)).
lifepath_provides(id(butcher, villager), skill(butchery)).
lifepath_provides(id(butcher, villager), skill(cooking)).
lifepath_provides(id(butcher, villager), skill(guts_wise)).
lifepath_provides(id(butcher, villager), skill(anatomy)).

lifepath(id(barber, villager), page(bwg, 168), 7, [city, peasant, outcast]).
lifepath_provides(id(barber, villager), trait(agreeable)).
lifepath_provides(id(barber, villager), trait(seemingly_concerned)).
lifepath_provides(id(barber, villager), skill(bloodletting)).
lifepath_provides(id(barber, villager), skill(anatomy)).
lifepath_provides(id(barber, villager), skill(apothecary)).
lifepath_provides(id(barber, villager), skill(village_wise)).
lifepath_provides(id(barber, villager), skill(gossip_wise)).

lifepath(id(brewer, villager), page(bwg, 168), 8, [city, peasant, court]).
lifepath_provides(id(brewer, villager), trait(reeks_of_alcohol)).
lifepath_provides(id(brewer, villager), trait(ruddy_complexion)).
lifepath_provides(id(brewer, villager), skill(brewer)).
lifepath_provides(id(brewer, villager), skill(miller)).
lifepath_provides(id(brewer, villager), skill(grain_wise)).
lifepath_provides(id(brewer, villager), skill(brew_wise)).

lifepath(id(acolyte, villager), page(bwg, 168), 7, [peasant, servitude, city, religious]).
lifepath_provides(id(acolyte, villager), flag(acolyte)).
lifepath_provides(id(acolyte, villager), trait(tonsured)).
lifepath_provides(id(acolyte, villager), trait(early_riser)).
lifepath_provides(id(acolyte, villager), trait(broken)).
lifepath_provides(id(acolyte, villager), trait(perfect_pitch)).
lifepath_provides(id(acolyte, villager), skill(doctrine)).
lifepath_provides(id(acolyte, villager), skill(bureaucracy)).
lifepath_provides(id(acolyte, villager), skill(write)).
lifepath_provides(id(acolyte, villager), skill(read)).
lifepath_provides(id(acolyte, villager), skill(ritual)).
lifepath_provides(id(acolyte, villager), skill(religious_history)).
lifepath_provides(id(acolyte, villager), skill(temple_wise)).

lifepath(id(failed_acolyte, villager), page(bwg, 168), 6, [outcast, soldier, peasant]).
lifepath_provides(id(failed_acolyte, villager), trait(bitter)).
lifepath_provides(id(failed_acolyte, villager), skill(temple_wise)).
lifepath_provides(id(failed_acolyte, villager), skill(dirty_secrets_wise)).
lifepath_provides(id(failed_acolyte, villager), skill(doctrine)).
lifepath_provides(id(failed_acolyte, villager), skill(religious_history)).

lifepath(id(village_priest, villager), page(bwg, 168), 8, any_except([noble, court])).
lifepath_requires(id(village_priest, villager), [lifepath(acolyte)]).
lifepath_provides(id(village_priest, villager), trait(vested)).
lifepath_provides(id(village_priest, villager), trait(devout)).
lifepath_provides(id(village_priest, villager), trait(faithful)).
lifepath_provides(id(village_priest, villager), skill(oratory)).
lifepath_provides(id(village_priest, villager), skill(suasion)).
lifepath_provides(id(village_priest, villager), skill(symbology)).

lifepath(id(venal_priest, villager), page(bwg, 168), 9, any_except([noble, court])).
lifepath_requires(id(venal_priest, villager), [lifepath(acolyte)]).
lifepath_requires(id(venal_priest, villager), [lifepath(clerk)]).
lifepath_requires(id(venal_priest, villager), [lifepath(student)]).
lifepath_requires(id(venal_priest, villager), [setting(religious)]).
lifepath_provides(id(venal_priest, villager), trait(venal)).
lifepath_provides(id(venal_priest, villager), trait(vested)).
lifepath_provides(id(venal_priest, villager), skill(persuasion)).
lifepath_provides(id(venal_priest, villager), skill(soothing_platitudes)).
lifepath_provides(id(venal_priest, villager), skill(falsehood)).

lifepath(id(apprentice, villager), page(bwg, 168), 7, [city, peasant, soldier, sea]).
lifepath_provides(id(apprentice, villager), trait(broken_in)).
lifepath_provides(id(apprentice, villager), trait(back_breaking_labor)).
lifepath_provides(id(apprentice, villager), skill(mending)).
lifepath_provides(id(apprentice, villager), skill(blacksmith)).
lifepath_provides(id(apprentice, villager), skill(carpentry)).
lifepath_provides(id(apprentice, villager), skill(tanner)).
lifepath_provides(id(apprentice, villager), skill(potter)).
lifepath_provides(id(apprentice, villager), skill(cooper)).

lifepath(id(journeyman, villager), page(bwg, 168), 6, [city, peasant, soldier, sea]).
lifepath_requires(id(journeyman, villager), [lifepath(apprentice)]).
lifepath_provides(id(journeyman, villager), trait(made_man)).
lifepath_provides(id(journeyman, villager), trait(geometric)).
lifepath_provides(id(journeyman, villager), skill(haggling)).
lifepath_provides(id(journeyman, villager), skill(appraisal)).
lifepath_provides(id(journeyman, villager), skill(write)).
lifepath_provides(id(journeyman, villager), skill(read)).

lifepath(id(cloth_dyer, villager), page(bwg, 168), 5, [city, peasant]).
lifepath_requires(id(cloth_dyer, villager), [lifepath(apprentice)]).
lifepath_provides(id(cloth_dyer, villager), trait(many_colored_hands)).
lifepath_provides(id(cloth_dyer, villager), skill(cloth_dyeing)).
lifepath_provides(id(cloth_dyer, villager), skill(dye_manufacture)).
lifepath_provides(id(cloth_dyer, villager), skill(accounting)).
lifepath_provides(id(cloth_dyer, villager), skill(haggling)).
lifepath_provides(id(cloth_dyer, villager), skill(fabric_wise)).
lifepath_provides(id(cloth_dyer, villager), skill(mineral_wise)).

lifepath(id(bowyer, villager), page(bwg, 169), 6, [soldier, outcast, peasant]).
lifepath_requires(id(bowyer, villager), [lifepath(apprentice)]).
lifepath_requires(id(bowyer, villager), [lifepath(huntsman)]).
lifepath_requires(id(bowyer, villager), [lifepath(forester)]).
lifepath_requires(id(bowyer, villager), [lifepath(archer)]).
lifepath_provides(id(bowyer, villager), skill(bowyer)).
lifepath_provides(id(bowyer, villager), skill(fletcher)).
lifepath_provides(id(bowyer, villager), skill(mending)).
lifepath_provides(id(bowyer, villager), skill(bow)).
lifepath_provides(id(bowyer, villager), skill(bow_wise)).

lifepath(id(master_craftsman, villager), page(bwg, 169), 10, [city, soldier]).
lifepath_requires(id(master_craftsman, villager), [lifepath(journeyman)]).
lifepath_provides(id(master_craftsman, villager), trait(perfectionist)).
lifepath_provides(id(master_craftsman, villager), trait(early_riser)).
lifepath_provides(id(master_craftsman, villager), trait(stubborn)).
lifepath_provides(id(master_craftsman, villager), trait(healthy)).
lifepath_provides(id(master_craftsman, villager), skill(craftsman_wise)).
lifepath_provides(id(master_craftsman, villager), skill(artisan_wise)).
lifepath_provides(id(master_craftsman, villager), skill(materials_wise)).
lifepath_provides(id(master_craftsman, villager), skill(tools_wise)).

lifepath(id(vintner, villager), page(bwg, 169), 10, [court, peasant]).
lifepath_requires(id(vintner, villager), [not(position(2))]).
lifepath_provides(id(vintner, villager), trait(patient)).
lifepath_provides(id(vintner, villager), trait(lugubrious)).
lifepath_provides(id(vintner, villager), skill(vintner)).
lifepath_provides(id(vintner, villager), skill(wine_tasting)).
lifepath_provides(id(vintner, villager), skill(estate_management)).
lifepath_provides(id(vintner, villager), skill(grape_wise)).

lifepath(id(apiarist, villager), page(bwg, 169), 8, [city, peasant, court]).
lifepath_provides(id(apiarist, villager), trait('stung_one_(once)')).
lifepath_provides(id(apiarist, villager), trait(beespeaker)).
lifepath_provides(id(apiarist, villager), skill(insect_husbandry)).
lifepath_provides(id(apiarist, villager), skill(carpentry)).
lifepath_provides(id(apiarist, villager), skill(firebuilding)).
lifepath_provides(id(apiarist, villager), skill(honey_wise)).

lifepath(id(mining_engineer, villager), page(bwg, 169), 8, [city, court, soldier]).
lifepath_requires(id(mining_engineer, villager), [lifepath(apprentice)]).
lifepath_requires(id(mining_engineer, villager), [lifepath(miner)]).
lifepath_requires(id(mining_engineer, villager), [lifepath(student)]).
lifepath_requires(id(mining_engineer, villager), [lifepath(journeyman)]).
lifepath_provides(id(mining_engineer, villager), trait(grim)).
lifepath_provides(id(mining_engineer, villager), trait(agoraphobic)).
lifepath_provides(id(mining_engineer, villager), trait(deep_sense)).
lifepath_provides(id(mining_engineer, villager), skill(prospecting)).
lifepath_provides(id(mining_engineer, villager), skill(engineer)).
lifepath_provides(id(mining_engineer, villager), skill(ore_wise)).
lifepath_provides(id(mining_engineer, villager), skill(rock_wise)).
lifepath_provides(id(mining_engineer, villager), skill(command)).

lifepath(id(town_official, villager), page(bwg, 169), 5, [city, outcast, soldier]).
lifepath_requires(id(town_official, villager), [lifepath(clerk)]).
lifepath_requires(id(town_official, villager), [lifepath(priest)]).
lifepath_requires(id(town_official, villager), [lifepath(student)]).
lifepath_provides(id(town_official, villager), trait(pragmatic)).
lifepath_provides(id(town_official, villager), skill(rule_of_law)).
lifepath_provides(id(town_official, villager), skill(persuasion)).
lifepath_provides(id(town_official, villager), skill(etiquette)).
lifepath_provides(id(town_official, villager), skill(interogation)).
lifepath_provides(id(town_official, villager), skill(falsehood)).
lifepath_provides(id(town_official, villager), skill(town_wise)).

lifepath(id(merchant, villager), page(bwg, 169), 7, [city, outcast, sea]).
lifepath_requires(id(merchant, villager), [lifepath(accountant)]).
lifepath_requires(id(merchant, villager), [lifepath(sea_captain)]).
lifepath_requires(id(merchant, villager), [lifepath(shopkeeper)]).
lifepath_requires(id(merchant, villager), [lifepath(smuggler)]).
lifepath_requires(id(merchant, villager), [lifepath(fence)]).
lifepath_requires(id(merchant, villager), [lifepath(vintner)]).
lifepath_requires(id(merchant, villager), [lifepath(chamberlain)]).
lifepath_provides(id(merchant, villager), trait(distracted)).
lifepath_provides(id(merchant, villager), skill(accounting)).
lifepath_provides(id(merchant, villager), skill(persuasion)).
lifepath_provides(id(merchant, villager), skill(falsehood)).
lifepath_provides(id(merchant, villager), skill(haggling)).
lifepath_provides(id(merchant, villager), skill(wholesale_wise)).
lifepath_provides(id(merchant, villager), skill(landlord_wise)).

lifepath(id(village_wife, villager), page(bwg, 169), 8, [religious, city, servitude]).
lifepath_provides(id(village_wife, villager), flag(female)).
lifepath_provides(id(village_wife, villager), skill(child_rearing)).
lifepath_provides(id(village_wife, villager), skill(cooking)).


%%% ----- CITY DWELLER SETTING ----- %%%

lifepath(id(city_born, city), page(bwg, 170), 12, [servitude, court, noble, outcast]).

lifepath(id(runner, city), page(bwg, 170), 3, [villager, soldier]).
lifepath_provides(id(runner, city), skill(streetwise)).
lifepath_provides(id(runner, city), skill(inconspicuous)).

lifepath(id(urchin, city), page(bwg, 170), 2, [outcast, servitude, villager]).
lifepath_requires(id(urchin, city), [position(2)]).
lifepath_requires(id(urchin, city), [position(3)]).
lifepath_provides(id(urchin, city), trait(sickly)).
lifepath_provides(id(urchin, city), trait(fleet_of_foot)).
lifepath_provides(id(urchin, city), trait(unheeded)).
lifepath_provides(id(urchin, city), skill(inconspicuous)).
lifepath_provides(id(urchin, city), skill(falsehood)).
lifepath_provides(id(urchin, city), skill(streetwise)).
lifepath_provides(id(urchin, city), skill(stealthy)).

lifepath(id(beggar, city), page(bwg, 170), 5, [outcast, servitude, villager]).
lifepath_provides(id(beggar, city), trait(lame)).
lifepath_provides(id(beggar, city), trait(downtrodden)).
lifepath_provides(id(beggar, city), trait(hurt)).
lifepath_provides(id(beggar, city), trait(broken)).
lifepath_provides(id(beggar, city), skill(beggardry)).
lifepath_provides(id(beggar, city), skill(inconspicuous)).
lifepath_provides(id(beggar, city), skill(persuasion)).
lifepath_provides(id(beggar, city), skill(falsehood)).
lifepath_provides(id(beggar, city), skill(city_wise)).
lifepath_provides(id(beggar, city), skill(city_guard_wise)).
lifepath_provides(id(beggar, city), skill(wealth_wise)).

lifepath(id(courier, city), page(bwg, 170), 4, [villager, soldier]).
lifepath_provides(id(courier, city), skill(riding)).
lifepath_provides(id(courier, city), skill(streetwise)).
lifepath_provides(id(courier, city), skill(countryside_wise)).

lifepath(id(laborer, city), page(bwg, 170), 4, [soldier, outcast, servitude, peasant]).
lifepath_provides(id(laborer, city), trait(drunk)).
lifepath_provides(id(laborer, city), trait(mind_numbing_work)).
lifepath_provides(id(laborer, city), skill(ditch_digging)).
lifepath_provides(id(laborer, city), skill(hauling)).

lifepath(id(pilgrim, city), page(bwg, 170), 2, [religious, servitude, villager]).
lifepath_provides(id(pilgrim, city), trait(tall_tale_teller)).
lifepath_provides(id(pilgrim, city), trait(stinky)).
lifepath_provides(id(pilgrim, city), skill(religious_diatribe)).
lifepath_provides(id(pilgrim, city), skill(city_wise)).
lifepath_provides(id(pilgrim, city), skill(shrine_wise)).
lifepath_provides(id(pilgrim, city), skill(doctrine)).

lifepath(id(groom, city), page(bwg, 170), 4, [peasant, villager, soldier]).
lifepath_provides(id(groom, city), trait(peripatetic)).
lifepath_provides(id(groom, city), skill(road_wise)).
lifepath_provides(id(groom, city), skill(driving)).
lifepath_provides(id(groom, city), skill(riding)).
lifepath_provides(id(groom, city), skill(animal_husbandry)).
lifepath_provides(id(groom, city), skill(mending)).
lifepath_provides(id(groom, city), skill(city_wise)).
lifepath_provides(id(groom, city), skill(traveler_wise)).

lifepath(id(duelist, city), page(bwg, 170), 4, [soldier, outcast, servitude]).
lifepath_requires(id(duelist, city), [lifepath(squire)]).
lifepath_requires(id(duelist, city), [setting(outcast)]).
lifepath_requires(id(duelist, city), [setting(soldier)]).
lifepath_requires(id(duelist, city), [flag(guard)]).
lifepath_provides(id(duelist, city), trait(mercenary)).
lifepath_provides(id(duelist, city), trait(cold_blooded)).
lifepath_provides(id(duelist, city), trait(fearless)).
lifepath_provides(id(duelist, city), skill(sword)).
lifepath_provides(id(duelist, city), skill(brawling)).
lifepath_provides(id(duelist, city), skill(two_fisted_fighting_training)).
lifepath_provides(id(duelist, city), skill(streetwise)).
lifepath_provides(id(duelist, city), skill(haggling)).
lifepath_provides(id(duelist, city), skill(conspicuous)).
lifepath_provides(id(duelist, city), skill(trial_by_combat_wise)).

lifepath(id(coin_clipper, city), page(bwg, 170), 6, [outcast, court]).
lifepath_provides(id(coin_clipper, city), trait(light_sleeper)).
lifepath_provides(id(coin_clipper, city), skill(streetwise)).
lifepath_provides(id(coin_clipper, city), skill(forgery)).
lifepath_provides(id(coin_clipper, city), skill(falsehood)).
lifepath_provides(id(coin_clipper, city), skill(intimidation)).
lifepath_provides(id(coin_clipper, city), skill(counterfeiting)).
lifepath_provides(id(coin_clipper, city), skill(coin_wise)).

lifepath(id(pickpocket, city), page(bwg, 171), 4, [outcast, villager, servitude]).
lifepath_provides(id(pickpocket, city), trait(plain_face)).
lifepath_provides(id(pickpocket, city), skill(inconspicuous)).
lifepath_provides(id(pickpocket, city), skill(streetwise)).
lifepath_provides(id(pickpocket, city), skill(sleight_of_hand)).
lifepath_provides(id(pickpocket, city), skill(crowd_wise)).

lifepath(id(street_thug, city), page(bwg, 171), 3, [outcast, servitude, soldier]).
lifepath_provides(id(street_thug, city), trait(cruel)).
lifepath_provides(id(street_thug, city), trait(street_smart)).
lifepath_provides(id(street_thug, city), skill(brawling)).
lifepath_provides(id(street_thug, city), skill(intimidation)).
lifepath_provides(id(street_thug, city), skill(streetwise)).
lifepath_provides(id(street_thug, city), skill(darkened_streets_wise)).

lifepath(id(criminal, city), page(bwg, 171), 5, [outcast, villager, soldier]).
lifepath_provides(id(criminal, city), trait(cynical)).
lifepath_provides(id(criminal, city), trait(poker_face)).
lifepath_provides(id(criminal, city), trait(rainman)).
lifepath_provides(id(criminal, city), trait(alert)).
lifepath_provides(id(criminal, city), skill(inconspicuous)).
lifepath_provides(id(criminal, city), skill(streetwise)).
lifepath_provides(id(criminal, city), skill(intimidation)).
lifepath_provides(id(criminal, city), skill(knives)).
lifepath_provides(id(criminal, city), skill(climbing)).

lifepath(id(confidence_man, city), page(bwg, 171), 4, [outcast, soldier, villager]).
lifepath_provides(id(confidence_man, city), skill(falsehood)).
lifepath_provides(id(confidence_man, city), skill(inconspicuous)).
lifepath_provides(id(confidence_man, city), skill(disguise)).
lifepath_provides(id(confidence_man, city), skill(persuasion)).
lifepath_provides(id(confidence_man, city), skill(grift_wise)).

lifepath(id(city_peddler, city), page(bwg, 171), 5, [villager, servitude, peasant, outcast]).
lifepath_provides(id(city_peddler, city), trait(the_story)).
lifepath_provides(id(city_peddler, city), skill(mending)).
lifepath_provides(id(city_peddler, city), skill(sing)).
lifepath_provides(id(city_peddler, city), skill(haggling)).
lifepath_provides(id(city_peddler, city), skill(chandler)).
lifepath_provides(id(city_peddler, city), skill(persuasion)).
lifepath_provides(id(city_peddler, city), skill(inconspicuous)).
lifepath_provides(id(city_peddler, city), skill(falsehood)).

lifepath(id(sailor, city), page(bwg, 171), 5, [soldier, sea, peasant, servitude]).
lifepath_provides(id(sailor, city), trait(superstitious)).
lifepath_provides(id(sailor, city), skill(rigging)).
lifepath_provides(id(sailor, city), skill(knots)).
lifepath_provides(id(sailor, city), skill(brawling)).
lifepath_provides(id(sailor, city), skill(sing)).
lifepath_provides(id(sailor, city), skill(gambling)).

lifepath(id(student, city), page(bwg, 171), 4, any_except([noble])).
lifepath_provides(id(student, city), trait(rabble_rouser)).
lifepath_provides(id(student, city), trait(drunk)).
lifepath_provides(id(student, city), trait(geometric)).
lifepath_provides(id(student, city), skill(write)).
lifepath_provides(id(student, city), skill(read)).
lifepath_provides(id(student, city), skill(philosophy)).
lifepath_provides(id(student, city), skill(rule_of_law)).
lifepath_provides(id(student, city), skill(history)).
lifepath_provides(id(student, city), skill(symbology)).
lifepath_provides(id(student, city), skill(anatomy)).
lifepath_provides(id(student, city), skill(astrology)).
lifepath_provides(id(student, city), skill(inconspicuous)).
lifepath_provides(id(student, city), skill(streetwise)).
lifepath_provides(id(student, city), skill(city_wise)).

lifepath(id(ganymede, city), page(bwg, 171), 5, [outcast, servitude, court]).
lifepath_provides(id(ganymede, city), trait(flamboyant)).
lifepath_provides(id(ganymede, city), trait(comely)).
lifepath_provides(id(ganymede, city), trait(sharp_dresser)).
lifepath_provides(id(ganymede, city), trait(catalyst)).
lifepath_provides(id(ganymede, city), skill(fashion_wise)).
lifepath_provides(id(ganymede, city), skill(inconspicuous)).
lifepath_provides(id(ganymede, city), skill(conspicuous)).
lifepath_provides(id(ganymede, city), skill(soothing_platitudes)).

lifepath(id(dilettante, city), page(bwg, 171), 3, [villager, court, outcast]).
lifepath_provides(id(dilettante, city), trait(superstitious)).
lifepath_provides(id(dilettante, city), trait(entropic)).
lifepath_provides(id(dilettante, city), trait(cipher)).
lifepath_provides(id(dilettante, city), skill(sorcery_wise)).
lifepath_provides(id(dilettante, city), skill(obscure_history)).
lifepath_provides(id(dilettante, city), skill(falsehood)).

lifepath(id(neophyte_sorcerer, city), page(bwg, 171), 6, [villager, peasant, outcast, servitude]).
lifepath_provides(id(neophyte_sorcerer, city), trait(extremely_respectful_of_ones_better)).
lifepath_provides(id(neophyte_sorcerer, city), trait(bitter)).
lifepath_provides(id(neophyte_sorcerer, city), trait(gifted)).
lifepath_provides(id(neophyte_sorcerer, city), skill(write)).
lifepath_provides(id(neophyte_sorcerer, city), skill(read)).
lifepath_provides(id(neophyte_sorcerer, city), skill(research)).
lifepath_provides(id(neophyte_sorcerer, city), skill(symbology)).
lifepath_provides(id(neophyte_sorcerer, city), skill(great_masters_wise)).

lifepath(id(temple_acolyte, city), page(bwg, 171), 5, [peasant, outcast, servitude, religious]).
lifepath_provides(id(temple_acolyte, city), flag(acolyte)).
lifepath_provides(id(temple_acolyte, city), trait(believer)).
lifepath_provides(id(temple_acolyte, city), trait(tonsured)).
lifepath_provides(id(temple_acolyte, city), trait(faithful)).
lifepath_provides(id(temple_acolyte, city), skill(doctrine)).
lifepath_provides(id(temple_acolyte, city), skill(bureaucracy)).
lifepath_provides(id(temple_acolyte, city), skill(write)).
lifepath_provides(id(temple_acolyte, city), skill(read)).
lifepath_provides(id(temple_acolyte, city), skill(temple_wise)).

lifepath(id(sculptor, city), page(bwg, 171), 5, [outcast, peasant, court]).
lifepath_provides(id(sculptor, city), trait(passionate)).
lifepath_provides(id(sculptor, city), skill(sculpture)).
lifepath_provides(id(sculptor, city), skill(mason)).
lifepath_provides(id(sculptor, city), skill(blacksmith)).
lifepath_provides(id(sculptor, city), skill(carpentry)).
lifepath_provides(id(sculptor, city), skill(stone_wise)).

lifepath(id(painter, city), page(bwg, 172), 5, [outcast, peasant, court]).
lifepath_provides(id(painter, city), trait(odd)).
lifepath_provides(id(painter, city), trait(perspective)).
lifepath_provides(id(painter, city), trait(greater_muse)).
lifepath_provides(id(painter, city), skill(painting)).
lifepath_provides(id(painter, city), skill(illuminations)).
lifepath_provides(id(painter, city), skill(anatomy)).
lifepath_provides(id(painter, city), skill(paint_wise)).

lifepath(id(composer, city), page(bwg, 172), 4, [outcast, peasant, court]).
lifepath_provides(id(composer, city), trait(esoteric)).
lifepath_provides(id(composer, city), trait(remote)).
lifepath_provides(id(composer, city), trait(deaf)).
lifepath_provides(id(composer, city), skill(music_composition)).
lifepath_provides(id(composer, city), skill(poetry)).
lifepath_provides(id(composer, city), skill(sing)).
lifepath_provides(id(composer, city), skill(musical_instrument)).

lifepath(id(dramaturge, city), page(bwg, 172), 4, [outcast, peasant, court]).
lifepath_provides(id(dramaturge, city), trait(other_life)).
lifepath_provides(id(dramaturge, city), skill(playwright)).
lifepath_provides(id(dramaturge, city), skill(composition)).
lifepath_provides(id(dramaturge, city), skill(write)).
lifepath_provides(id(dramaturge, city), skill(drama_wise)).

lifepath(id(performer, city), page(bwg, 172), 3, [villager, outcast, soldier]).
lifepath_provides(id(performer, city), trait(colorful)).
lifepath_provides(id(performer, city), trait(acting)).
lifepath_provides(id(performer, city), trait(persuasion)).
lifepath_provides(id(performer, city), trait(sing)).
lifepath_provides(id(performer, city), trait(falsehood)).
lifepath_provides(id(performer, city), trait(conspicuous)).
lifepath_provides(id(performer, city), trait(sleight_of_hand)).
lifepath_provides(id(performer, city), trait(musical_instrument)).

lifepath(id(tinker, city), page(bwg, 172), 7, [villager, peasant, outcast]).
lifepath_provides(id(tinker, city), skill(mending)).
lifepath_provides(id(tinker, city), skill(scavenging)).
lifepath_provides(id(tinker, city), skill(junk_wise)).

lifepath(id(coalman, city), page(bwg, 172), 4, [servitude, peasant, outcast, soldier]).
lifepath_provides(id(coalman, city), trait(hacking_cough)).
lifepath_provides(id(coalman, city), skill(firebuilding)).
lifepath_provides(id(coalman, city), skill(streetwise)).
lifepath_provides(id(coalman, city), skill(charcoal_wise)).
lifepath_provides(id(coalman, city), skill(haggling)).

lifepath(id(seamstress, city), page(bwg, 172), 5, [villager, peasant]).
lifepath_provides(id(seamstress, city), trait(fretful)).
lifepath_provides(id(seamstress, city), trait(sharp_dresser)).
lifepath_provides(id(seamstress, city), skill(sewing)).
lifepath_provides(id(seamstress, city), skill(embroidery)).
lifepath_provides(id(seamstress, city), skill(clothing_wise)).
lifepath_provides(id(seamstress, city), skill(fashion_wise)).

lifepath(id(barkeep, city), page(bwg, 172), 5, [villager, peasant]).
lifepath_provides(id(barkeep, city), trait(good_listener)).
lifepath_provides(id(barkeep, city), skill(drink_wise)).
lifepath_provides(id(barkeep, city), skill(persuasion)).

lifepath(id(shopkeeper, city), page(bwg, 172), 6, [villager, peasant]).
lifepath_provides(id(shopkeeper, city), skill(merchant_wise)).
lifepath_provides(id(shopkeeper, city), skill(haggling)).
lifepath_provides(id(shopkeeper, city), skill(accounting)).
lifepath_provides(id(shopkeeper, city), skill(observation)).

lifepath(id(baker, city), page(bwg, 172), 6, [villager, peasant]).
lifepath_provides(id(baker, city), trait(floury)).
lifepath_provides(id(baker, city), skill(baking)).
lifepath_provides(id(baker, city), skill(cooking)).
lifepath_provides(id(baker, city), skill(daily_bread_wise)).
lifepath_provides(id(baker, city), skill(sweet_tooth_wise)).

lifepath(id(alewife, city), page(bwg, 172), 6, [court, peasant, villager]).
lifepath_requires(id(alewife, city), [flag(female)]).
lifepath_provides(id(alewife, city), trait(domineering_presence)).
lifepath_provides(id(alewife, city), skill(brewer)).
lifepath_provides(id(alewife, city), skill(cooking)).
lifepath_provides(id(alewife, city), skill(drinking)).
lifepath_provides(id(alewife, city), skill(administration)).
lifepath_provides(id(alewife, city), skill(drunk_husband_wise)).

lifepath(id(conner, city), page(bwg, 173), 6, [court, peasant, villager]).
lifepath_provides(id(conner, city), trait(red_cheeks)).
lifepath_provides(id(conner, city), trait(drunk)).
lifepath_provides(id(conner, city), skill(brewer)).
lifepath_provides(id(conner, city), skill(mending)).
lifepath_provides(id(conner, city), skill(drinking)).
lifepath_provides(id(conner, city), skill(cooper)).
lifepath_provides(id(conner, city), skill(beer_wise)).

lifepath(id(clerk, city), page(bwg, 173), 4, [villager, outcast, soldier]).
lifepath_provides(id(clerk, city), skill(bureaucracy)).
lifepath_provides(id(clerk, city), skill(write)).
lifepath_provides(id(clerk, city), skill(read)).
lifepath_provides(id(clerk, city), skill(accounting)).
lifepath_provides(id(clerk, city), skill(bribe_wise)).
lifepath_provides(id(clerk, city), skill(paperwork_wise)).

lifepath(id(scribe, city), page(bwg, 173), 7, [villager, peasant]).
lifepath_requires(id(scribe, city), [lifepath(student)]).
lifepath_requires(id(scribe, city), [lifepath(clerk)]).
lifepath_requires(id(scribe, city), [flag(acolyte)]).
lifepath_provides(id(scribe, city), trait(near_sighted)).
lifepath_provides(id(scribe, city), trait(cramped_hands)).
lifepath_provides(id(scribe, city), skill(write)).
lifepath_provides(id(scribe, city), skill(illuminations)).
lifepath_provides(id(scribe, city), skill(foreign_languages)).
lifepath_provides(id(scribe, city), skill(handwriting_wise)).

lifepath(id(accountant, city), page(bwg, 173), 10, [villager, peasant]).
lifepath_requires(id(accountant, city), [lifepath(clerk)]).
lifepath_requires(id(accountant, city), [lifepath(young_lady)]).
lifepath_requires(id(accountant, city), [lifepath(student)]).
lifepath_provides(id(accountant, city), trait(bored)).
lifepath_provides(id(accountant, city), skill(accounting)).
lifepath_provides(id(accountant, city), skill(bureaucracy)).
lifepath_provides(id(accountant, city), skill(extortion)).
lifepath_provides(id(accountant, city), skill(ledger_wise)).
lifepath_provides(id(accountant, city), skill(graft_wise)).

lifepath(id(scholar, city), page(bwg, 173), 10, [villager, outcast, court]).
lifepath_requires(id(scholar, city), [lifepath(scribe)]).
lifepath_requires(id(scholar, city), [lifepath(thinker)]).
lifepath_requires(id(scholar, city), [lifepath(archivist)]).
lifepath_requires(id(scholar, city), [lifepath(interpreter)]).
lifepath_requires(id(scholar, city), [lifepath(custodian)]).
lifepath_requires(id(scholar, city), [lifepath(bishop)]).
lifepath_requires(id(scholar, city), [skill(sorcery)]).
lifepath_provides(id(scholar, city), trait(know_it_all)).
lifepath_provides(id(scholar, city), trait(bookworm)).
lifepath_provides(id(scholar, city), skill(read)).
lifepath_provides(id(scholar, city), skill(research)).
lifepath_provides(id(scholar, city), skill(history)).
lifepath_provides(id(scholar, city), skill(philosophy)).
lifepath_provides(id(scholar, city), skill(symbology)).
lifepath_provides(id(scholar, city), skill(instruction)).
lifepath_provides(id(scholar, city), skill(illuminations)).
lifepath_provides(id(scholar, city), skill(foreign_languages)).
lifepath_provides(id(scholar, city), skill(ancient_languages)).

lifepath(id(moneylender, city), page(bwg, 173), 8, [villager, peasant, outcast]).
lifepath_provides(id(moneylender, city), trait(penny_wise)).
lifepath_provides(id(moneylender, city), skill(currency_wise)).
lifepath_provides(id(moneylender, city), skill(haggling)).
lifepath_provides(id(moneylender, city), skill(accounting)).

lifepath(id(tax_collector, city), page(bwg, 173), 5, [villager, peasant, outcast]).
lifepath_requires(id(tax_collector, city), [not(position(2))]).
lifepath_provides(id(tax_collector, city), trait(hard_hearted)).
lifepath_provides(id(tax_collector, city), skill(intimidation)).
lifepath_provides(id(tax_collector, city), skill(accounting)).
lifepath_provides(id(tax_collector, city), skill(haggling)).
lifepath_provides(id(tax_collector, city), skill(interrogation)).

lifepath(id(taskmaster, city), page(bwg, 173), 6, [villager, outcast, soldier]).
lifepath_requires(id(taskmaster, city), [not(position(2))]).
lifepath_provides(id(taskmaster, city), trait(intimidating)).
lifepath_provides(id(taskmaster, city), trait(dreadful)).
lifepath_provides(id(taskmaster, city), skill(intimidation)).
lifepath_provides(id(taskmaster, city), skill(brawling)).
lifepath_provides(id(taskmaster, city), skill(sing)).
lifepath_provides(id(taskmaster, city), skill(conspicuous)).
lifepath_provides(id(taskmaster, city), skill(laborer_wise)).

lifepath(id(mercenary_captain, city), page(bwg, 173), 5, [soldier, sea, outcast]).
lifepath_requires(id(mercenary_captain, city), [lifepath(sailor)]).
lifepath_requires(id(mercenary_captain, city), [lifepath(pirate)]).
lifepath_requires(id(mercenary_captain, city), [lifepath(knight)]).
lifepath_provides(id(mercenary_captain, city), trait(predatory)).
lifepath_provides(id(mercenary_captain, city), skill(pilot)).
lifepath_provides(id(mercenary_captain, city), skill(fat_merchant_wise)).
lifepath_provides(id(mercenary_captain, city), skill(sword)).
lifepath_provides(id(mercenary_captain, city), skill(climbing)).
lifepath_provides(id(mercenary_captain, city), skill(intimidation)).

lifepath(id(city_guard, city), page(bwg, 173), 5, [soldier, outcast]).
lifepath_provides(id(city_guard, city), flag(guard)).
lifepath_provides(id(city_guard, city), trait(drunk)).
lifepath_provides(id(city_guard, city), skill(brawling)).
lifepath_provides(id(city_guard, city), skill(intimidation)).
lifepath_provides(id(city_guard, city), skill(drinking)).
lifepath_provides(id(city_guard, city), skill(appropriate_weapons)).
lifepath_provides(id(city_guard, city), skill(armor)).