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
flag(female).       % selecting this lifepaths indicates the character is female
flag(acolyte).      % denotes a 'faith' acolyte lifepath
flag(guard).        % denotes a 'guard' lifepath
flag(sergeant).     % denotes a 'sergeant' lifepath
flag(gm_approval).  % this lifepath needs gm approval. Only provided by passing it in as a property to character_path
flag(wife).         % denotes a '[setting] wife' lifepath
flag(priest).       % denotes a 'priest' lifepath

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
lifepath_provides(id(itinerant_priest, peasant), flag(priest)).
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
lifepath_provides(id(country_wife, peasant), flag(wife)).
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
lifepath_requires(id(village_sergeant, villager), [lifepath(sergeant_at_arms)]).
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
lifepath_provides(id(village_sergeant, villager), flag(guard)).
lifepath_provides(id(village_sergeant, villager), flag(sergeant)).

lifepath(id(corrupt_sergeant, villager), page(bwg, 167), 5, [soldier, servitude, court]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(village_tough)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(squire)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(freebooter)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(sergeant_at_arms)]).
lifepath_requires(id(corrupt_sergeant, villager), [lifepath(man_at_arms)]).
lifepath_provides(id(corrupt_sergeant, villager), trait(predatory)).
lifepath_provides(id(corrupt_sergeant, villager), skill(intimidation)).
lifepath_provides(id(corrupt_sergeant, villager), skill(appropriate_weapons)).
lifepath_provides(id(corrupt_sergeant, villager), skill(shield_training)).
lifepath_provides(id(corrupt_sergeant, villager), skill(armor_training)).
lifepath_provides(id(corrupt_sergeant, villager), skill(bribe_wise)).
lifepath_provides(id(corrupt_sergeant, villager), skill(wealthy_wise)).
lifepath_provides(id(corrupt_sergeant, villager), skill(criminal_wise)).
lifepath_provides(id(corrupt_sergeant, villager), flag(guard)).
lifepath_provides(id(corrupt_sergeant, villager), flag(sergeant)).

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
lifepath_provides(id(village_priest, villager), flag(priest)).
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
lifepath_provides(id(venal_priest, villager), flag(priest)).
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
lifepath_provides(id(village_wife, villager), flag(wife)).
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

lifepath(id(sergeant_at_arms, city), page(bwg, 174), 6, [soldier, outcast]).
lifepath_requires(id(sergeant_at_arms, city), [flag(guard)]).
lifepath_requires(id(sergeant_at_arms, city), [flag(sergeant)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(marine)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(first_mate)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(foot_soldier)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(freebooter)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(squire)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(man_at_arms)]).
lifepath_requires(id(sergeant_at_arms, city), [lifepath(cavalryman)]).
lifepath_provides(id(sergeant_at_arms, city), trait(overworked)).
lifepath_provides(id(sergeant_at_arms, city), skill(intimidation)).
lifepath_provides(id(sergeant_at_arms, city), skill(appropriate_weapons)).
lifepath_provides(id(sergeant_at_arms, city), skill(armor)).
lifepath_provides(id(sergeant_at_arms, city), skill(field_dressing)).

lifepath(id(guard_captain, city), page(bwg, 174), 6, [soldier, outcast, court]).
lifepath_requires(id(guard_captain, city), [lifepath(knight)]).
lifepath_requires(id(guard_captain, city), [lifepath(captain)]).
lifepath_requires(id(guard_captain, city), [flag(sergeant)]).
lifepath_provides(id(guard_captain, city), trait(exasperated)).
lifepath_provides(id(guard_captain, city), skill(etiquette)).
lifepath_provides(id(guard_captain, city), skill(riding)).
lifepath_provides(id(guard_captain, city), skill(intimidation)).
lifepath_provides(id(guard_captain, city), skill(command)).
lifepath_provides(id(guard_captain, city), skill(appropriate_weapons)).

lifepath(id(apprentice, city), page(bwg, 174), 7, [villager, peasant, soldier, sea]).
lifepath_provides(id(apprentice, city), trait(broken_in)).
lifepath_provides(id(apprentice, city), trait(back_breaking_labor)).
lifepath_provides(id(apprentice, city), skill(mending)).
lifepath_provides(id(apprentice, city), skill(write)).
lifepath_provides(id(apprentice, city), skill(read)).
lifepath_provides(id(apprentice, city), skill(hauling)).
lifepath_provides(id(apprentice, city), skill(driving)).
lifepath_provides(id(apprentice, city), skill(ditch_digging)).

lifepath(id(apprentice_artisan, city), page(bwg, 174), 8, [villager, soldier]).
lifepath_provides(id(apprentice_artisan, city), skill(artisan_wise)).
lifepath_provides(id(apprentice_artisan, city), skill(mason)).
lifepath_provides(id(apprentice_artisan, city), skill(blacksmith)). 
lifepath_provides(id(apprentice_artisan, city), skill(coppersmith)). 
lifepath_provides(id(apprentice_artisan, city), skill(locksmith)). 
lifepath_provides(id(apprentice_artisan, city), skill(weaponsmith)). 
lifepath_provides(id(apprentice_artisan, city), skill(whitesmith)). 
lifepath_provides(id(apprentice_artisan, city), skill(carpentry)). 
lifepath_provides(id(apprentice_artisan, city), skill(write)). 
lifepath_provides(id(apprentice_artisan, city), skill(read)). 
lifepath_provides(id(apprentice_artisan, city), skill(jargon)). 

lifepath(id(journeyman, city), page(bwg, 174), 6, [villager, peasant, soldier, sea]).
lifepath_requires(id(journeyman, city), [lifepath(apprentice)]).
lifepath_provides(id(journeyman, city), trait(made_man)).
lifepath_provides(id(journeyman, city), trait(geometric)).
lifepath_provides(id(journeyman, city), skill(haggling)).
lifepath_provides(id(journeyman, city), skill(appraisal)).
lifepath_provides(id(journeyman, city), skill(blacksmith)).
lifepath_provides(id(journeyman, city), skill(carpentry)).
lifepath_provides(id(journeyman, city), skill(tanner)).
lifepath_provides(id(journeyman, city), skill(potter)).
lifepath_provides(id(journeyman, city), skill(cooper)).

lifepath(id(engraver, city), page(bwg, 174), 7, [soldier, court]).
lifepath_requires(id(engraver, city), [lifepath(journeyman)]).
lifepath_provides(id(engraver, city), trait(gentle_but_firm)).
lifepath_provides(id(engraver, city), skill(engraving)).
lifepath_provides(id(engraver, city), skill(etching)).
lifepath_provides(id(engraver, city), skill(jargon)).

lifepath(id(saddler, city), page(bwg, 174), 8, [villager, soldier, court]).
lifepath_requires(id(saddler, city), [lifepath(journeyman)]).
lifepath_provides(id(saddler, city), skill(saddlery)).
lifepath_provides(id(saddler, city), skill(tanner)).
lifepath_provides(id(saddler, city), skill(sewing)).
lifepath_provides(id(saddler, city), skill(embroidery)).
lifepath_provides(id(saddler, city), skill(mending)).
lifepath_provides(id(saddler, city), skill(mount_wise)).

lifepath(id(armorer, city), page(bwg, 174), 10, [soldier, court, outcast]).
lifepath_requires(id(armorer, city), [lifepath(journeyman)]).
lifepath_provides(id(armorer, city), trait(diligent)).
lifepath_provides(id(armorer, city), skill(swordsman_wise)).
lifepath_provides(id(armorer, city), skill(blacksmith)).
lifepath_provides(id(armorer, city), skill(tanner)).
lifepath_provides(id(armorer, city), skill(armorer)).
lifepath_provides(id(armorer, city), skill(weaponsmith)).

lifepath(id(plumber, city), page(bwg, 174), 7, [court, outcast]).
lifepath_requires(id(plumber, city), [lifepath(journeyman)]).
lifepath_provides(id(plumber, city), trait(touch_of_madness)).
lifepath_provides(id(plumber, city), skills(plumbing)).
lifepath_provides(id(plumber, city), skills(engineer)).
lifepath_provides(id(plumber, city), skills(coppersmith)).
lifepath_provides(id(plumber, city), skills(waterworks)).

lifepath(id(locksmith, city), page(bwg, 175), 8, [peasant, villager]).
lifepath_requires(id(locksmith, city), [lifepath(journeyman)]).
lifepath_provides(id(locksmith, city), trait(steady_hands)).
lifepath_provides(id(locksmith, city), skills(locksmith)).
lifepath_provides(id(locksmith, city), skills(lock_wise)).

lifepath(id(jeweler, city), page(bwg, 175), 9, [villager, peasant]).
lifepath_requires(id(jeweler, city), [lifepath(journeyman)]).
lifepath_provides(id(jeweler, city), trait(guarded)).
lifepath_provides(id(jeweler, city), skills(jewler)).
lifepath_provides(id(jeweler, city), skills(lapidary)).
lifepath_provides(id(jeweler, city), skills(appraisal)).
lifepath_provides(id(jeweler, city), skills(falsehood)).

lifepath(id(gaol_warden, city), page(bwg, 175), 4, [outcast, court, soldier, villager]).
lifepath_requires(id(gaol_warden, city), [lifepath(born_noble)]).
lifepath_requires(id(gaol_warden, city), [lifepath(merchant)]).
lifepath_requires(id(gaol_warden, city), [flag(sergeant)]).
lifepath_requires(id(gaol_warden, city), [lifepath(man_at_arms)]).
lifepath_requires(id(gaol_warden, city), [lifepath(judge)]).

lifepath(id(advocate, city), page(bwg, 175), 6, [outcast, court, villager]).
lifepath_requires(id(advocate, city), [lifepath(student)]).
lifepath_requires(id(advocate, city), [lifepath(young_lady)]).
lifepath_provides(id(advocate, city), trait(shrewd)).
lifepath_provides(id(advocate, city), skill(rule_of_law)).
lifepath_provides(id(advocate, city), skill(bureaucracy)).
lifepath_provides(id(advocate, city), skill(persuasion)).
lifepath_provides(id(advocate, city), skill(history)).
lifepath_provides(id(advocate, city), skill(rhetoric)).

lifepath(id(doctor, city), page(bwg, 175), 7, [outcast, court, villager]).
lifepath_requires(id(doctor, city), [lifepath(student)]).
lifepath_requires(id(doctor, city), [lifepath(young_lady)]).
lifepath_provides(id(doctor, city), trait(frustrated)).
lifepath_provides(id(doctor, city), skill(anatomy)).
lifepath_provides(id(doctor, city), skill(apothecary)).
lifepath_provides(id(doctor, city), skill(bloodletting)).
lifepath_provides(id(doctor, city), skill(surgery)).
lifepath_provides(id(doctor, city), skill(soothing_platitudes)).

lifepath(id(physician, city), page(bwg, 175), 5, [court, soldier, peasant]).
lifepath_requires(id(physician, city), [lifepath(midwife)]).
lifepath_requires(id(physician, city), [lifepath(young_lady)]).
lifepath_requires(id(physician, city), [lifepath(student)]).
lifepath_provides(id(physician, city), skill(herbalism)).
lifepath_provides(id(physician, city), skill(apothecary)).
lifepath_provides(id(physician, city), skill(anatomy)).
lifepath_provides(id(physician, city), skill(research)).
lifepath_provides(id(physician, city), skill(herbalist_wise)).

lifepath(id(hospital_warden, city), page(bwg, 175), 4, [outcast, servitude, religious]).
lifepath_requires(id(hospital_warden, city), [setting(noble)]).
lifepath_requires(id(hospital_warden, city), [setting(court)]).
lifepath_requires(id(hospital_warden, city), [setting(religious)]).
lifepath_provides(id(hospital_warden, city), trait(overworked)).
lifepath_provides(id(hospital_warden, city), trait(generous)).
lifepath_provides(id(hospital_warden, city), trait(venal)).
lifepath_provides(id(hospital_warden, city), trait(hypochondriac)).
lifepath_provides(id(hospital_warden, city), skill(administration)).
lifepath_provides(id(hospital_warden, city), skill(beggar_wise)).
lifepath_provides(id(hospital_warden, city), skill(vagrant_wise)).
lifepath_provides(id(hospital_warden, city), skill(leper_wise)).

lifepath(id(banker, city), page(bwg, 175), 10, [court, noble]).
lifepath_requires(id(banker, city), [lifepath(merchant)]).
lifepath_requires(id(banker, city), [lifepath(moneylender)]).
lifepath_requires(id(banker, city), [lifepath(steward)]).
lifepath_requires(id(banker, city), [lifepath(accountant)]).
lifepath_requires(id(banker, city), [lifepath(chamberlain)]).
lifepath_provides(id(banker, city), trait(intense)).
lifepath_provides(id(banker, city), skill(accounting)).
lifepath_provides(id(banker, city), skill(administration)).
lifepath_provides(id(banker, city), skill(currency_wise)).

lifepath(id(merchant, city), page(bwg, 175), 6, [villager, peasant, court]).
lifepath_requires(id(merchant, city), [lifepath(master_craftsman)]).
lifepath_requires(id(merchant, city), [lifepath(master_of_horses)]).
lifepath_requires(id(merchant, city), [lifepath(master_of_house)]).
lifepath_requires(id(merchant, city), [lifepath(moneylender)]).
lifepath_requires(id(merchant, city), [lifepath(steward)]).
lifepath_requires(id(merchant, city), [lifepath(jeweler)]).
lifepath_requires(id(merchant, city), [lifepath(saddler)]).
lifepath_requires(id(merchant, city), [lifepath(armorer)]).
lifepath_requires(id(merchant, city), [lifepath(cobbler)]).
lifepath_requires(id(merchant, city), [lifepath(courtier)]).
lifepath_requires(id(merchant, city), [lifepath(chamberlain)]).

lifepath(id(sorcerer, city), page(bwg, 176), 6, [villager, outcast, court]).
lifepath_requires(id(sorcerer, city), [lifepath(neophyte_sorcerer)]).
lifepath_requires(id(sorcerer, city), [lifepath(arcane_devotee)]).
lifepath_requires(id(sorcerer, city), [lifepath(weather_witch)]).
lifepath_provides(id(sorcerer, city), skill(sorcery)).
lifepath_provides(id(sorcerer, city), skill(enchanting)).
lifepath_provides(id(sorcerer, city), skill(calligraphy)).

lifepath(id(temple_priest, city), page(bwg, 176), 5, any_except([noble])).
lifepath_requires(id(temple_priest, city), [lifepath(religious_acolyte)]).
lifepath_requires(id(temple_priest, city), [lifepath(temple_acolyte)]).
lifepath_requires(id(temple_priest, city), [lifepath(military_order)]).
lifepath_provides(id(temple_priest, city), flag(priest)).
lifepath_provides(id(temple_priest, city), trait(vested)).
lifepath_provides(id(temple_priest, city), trait(aloof)).
lifepath_provides(id(temple_priest, city), trait(imperious)).
lifepath_provides(id(temple_priest, city), trait(strong_willed)).
lifepath_provides(id(temple_priest, city), skill(church_politics_wise)).
lifepath_provides(id(temple_priest, city), skill(doctrine)).
lifepath_provides(id(temple_priest, city), skill(oratory)).
lifepath_provides(id(temple_priest, city), skill(suasion)).
lifepath_provides(id(temple_priest, city), skill(symbology)).

lifepath(id(judge, city), page(bwg, 176), 10, [court, villager]).
lifepath_requires(id(judge, city), [lifepath(town_official)]).
lifepath_requires(id(judge, city), [lifepath(tax_collector)]).
lifepath_requires(id(judge, city), [lifepath(bailiff)]).
lifepath_requires(id(judge, city), [lifepath(justiciar)]).
lifepath_provides(id(judge, city), trait(ornery)).
lifepath_provides(id(judge, city), trait(quick_witted)).
lifepath_provides(id(judge, city), skill(rule_of_law)).
lifepath_provides(id(judge, city), skill(amercement)).
lifepath_provides(id(judge, city), skill(criminal_wise)).
lifepath_provides(id(judge, city), skill(interogation)).

lifepath(id(municipal_minister, city), page(bwg, 176), 9, [villager, court, outcast]).
lifepath_requires(id(municipal_minister, city), [lifepath(town_official)]).
lifepath_requires(id(municipal_minister, city), [lifepath(scholar)]).
lifepath_requires(id(municipal_minister, city), [lifepath(priest)]).
lifepath_requires(id(municipal_minister, city), [lifepath(bishop)]).
lifepath_requires(id(municipal_minister, city), [lifepath(captain)]).
lifepath_requires(id(municipal_minister, city), [lifepath(sea_captain)]).
lifepath_requires(id(municipal_minister, city), [lifepath(artisan)]).
lifepath_requires(id(municipal_minister, city), [lifepath(master_craftsman)]).
lifepath_requires(id(municipal_minister, city), [lifepath(knight)]).
lifepath_requires(id(municipal_minister, city), [lifepath(courtier)]).
lifepath_requires(id(municipal_minister, city), [lifepath(master_of_horses)]).

lifepath(id(artisan, city), page(bwg, 176), 10, [soldier, court]).
lifepath_requires(id(artisan, city), [lifepath(apprentice_artisan)]).
lifepath_requires(id(artisan, city), [lifepath(engineer)]).
lifepath_requires(id(artisan, city), [lifepath(master_craftsman)]).
lifepath_provides(id(artisan, city), trait(self_confident)).
lifepath_provides(id(artisan, city), skill(jargon)).
lifepath_provides(id(artisan, city), skill(mason)).
lifepath_provides(id(artisan, city), skill(engineer)).
lifepath_provides(id(artisan, city), skill(architect)).

lifepath(id(master_craftsman, city), page(bwg, 176), 10, [villager, court, soldier]).
lifepath_requires(id(master_craftsman, city), [and(
    lifepath(journeyman), 
    or([
        lifepath(locksmith),
        lifepath(plumber),
        lifepath(engraver),
        lifepath(saddler),
        lifepath(blacksmith),
        lifepath(armorer),
        lifepath(atilliator),
        lifepath(cobbler),
        lifepath(bowyer),
        lifepath(taskmaster)
    ])
)]).
lifepath_provides(id(master_craftsman, city), trait(ambitious)).
lifepath_provides(id(master_craftsman, city), trait(charismatic)).
lifepath_provides(id(master_craftsman, city), skill(craftsman_wise)).
lifepath_provides(id(master_craftsman, city), skill(artisan_wise)).
lifepath_provides(id(master_craftsman, city), skill(materials_wise)).
lifepath_provides(id(master_craftsman, city), skill(tools_wise)).

lifepath(id(bishop, city), page(bwg, 176), 12, [court, religious]).
lifepath_requires(id(bishop, city), [lifepath(archpriest)]).
lifepath_requires(id(bishop, city), [lifepath(canon)]).
lifepath_requires(id(bishop, city), [lifepath(steward)]).
lifepath_requires(id(bishop, city), [lifepath(chamberlain)]).
lifepath_requires(id(bishop, city), [trait(your_grace)]).
lifepath_provides(id(bishop, city), trait(holier)).
lifepath_provides(id(bishop, city), skill(etiquette)).
lifepath_provides(id(bishop, city), skill(bureaucracy)).
lifepath_provides(id(bishop, city), skill(ritual)).
lifepath_provides(id(bishop, city), skill(church_wise)).
lifepath_provides(id(bishop, city), skill(city_wise)).

lifepath(id(magnate, city), page(bwg, 176), 12, any_except([])).
lifepath_requires(id(magnate, city), [lifepath(merchant)]).
lifepath_requires(id(magnate, city), [lifepath(master_of_horses)]).
lifepath_provides(id(magnate, city), trait(self_satisfied)).
lifepath_provides(id(magnate, city), trait(greedy)).
lifepath_provides(id(magnate, city), trait(affinity_for_business)).
lifepath_provides(id(magnate, city), skill(administation)).
lifepath_provides(id(magnate, city), skill(merchant_wise)).
lifepath_provides(id(magnate, city), skill(court_wise)).
lifepath_provides(id(magnate, city), skill(commodities_wise)).
lifepath_provides(id(magnate, city), skill(pirates_wise)).
lifepath_provides(id(magnate, city), skill(bandit_wise)).

lifepath(id(city_wife, city), page(bwg, 177), 6, [religious]).
lifepath_provides(id(city_wife, city), skill(child_rearing)).
lifepath_provides(id(city_wife, city), skill(husband_wise)).
lifepath_provides(id(city_wife, city), flag(female)).
lifepath_provides(id(city_wife, city), flag(wife)).


%%% ----- NOBLE SETTING ----- %%%


lifepath(id(born_noble, noble), page(bwg, 177), 8, any_except([])).
lifepath_provides(id(born_noble, noble), trait(mark_of_priviledge)).
lifepath_provides(id(born_noble, noble), trait(your_lordship)).
lifepath_provides(id(born_noble, noble), trait(your_eminence)).
lifepath_provides(id(born_noble, noble), trait(your_grace)).

lifepath(id(bastard, noble), page(bwg, 177), 6, [outcast, soldier, city, religious]).
lifepath_requires(id(bastard, noble), [position(2)]).
lifepath_provides(id(bastard, noble), trait(bastard)).
lifepath_provides(id(bastard, noble), trait(bitter)).
lifepath_provides(id(bastard, noble), trait(cynical)).
lifepath_provides(id(bastard, noble), trait(happy_go_lucky)).
lifepath_provides(id(bastard, noble), skill(family_secret_wise)).
lifepath_provides(id(bastard, noble), skill(etiquette)).
lifepath_provides(id(bastard, noble), skill(extortion)).

lifepath(id(page, noble), page(bwg, 177), 6, [soldier, city, servitude]).
lifepath_requires(id(page, noble), [position(2)]).
lifepath_provides(id(page, noble), skill(riding)).
lifepath_provides(id(page, noble), skill(brawling)).
lifepath_provides(id(page, noble), skill(write)).
lifepath_provides(id(page, noble), skill(read)).
lifepath_provides(id(page, noble), skill(read)).
lifepath_provides(id(page, noble), skill(sword)).
lifepath_provides(id(page, noble), skill(etiquette)).

lifepath(id(student, noble), page(bwg, 177), 4, [city, court, religious]).
lifepath_provides(id(student, noble), skill(write)).
lifepath_provides(id(student, noble), skill(read)).
lifepath_provides(id(student, noble), skill(rule_of_law)).
lifepath_provides(id(student, noble), skill(oratory)).
lifepath_provides(id(student, noble), skill(doctrine)).
lifepath_provides(id(student, noble), skill(etiquette)).

lifepath(id(squire, noble), page(bwg, 177), 6, [soldier, city, servitude, outcast]).
lifepath_requires(id(squire, noble), [lifepath(page)]).
lifepath_requires(id(squire, noble), [setting(soldier)]).
lifepath_provides(id(squire, noble), skill(sword)).
lifepath_provides(id(squire, noble), skill(mounted_combat_training)).
lifepath_provides(id(squire, noble), skill(shield_training)).
lifepath_provides(id(squire, noble), skill(armor_training)).
lifepath_provides(id(squire, noble), skill(lance)).
lifepath_provides(id(squire, noble), skill(knives)).
lifepath_provides(id(squire, noble), skill(crossbow)).

lifepath(id(arcane_devotee, noble), page(bwg, 177), 6, [city, court, outcast]).
lifepath_provides(id(arcane_devotee, noble), trait(base_humility)).
lifepath_provides(id(arcane_devotee, noble), trait(gifted)).
lifepath_provides(id(arcane_devotee, noble), skill(calligraphy)).
lifepath_provides(id(arcane_devotee, noble), skill(write)).
lifepath_provides(id(arcane_devotee, noble), skill(read)).
lifepath_provides(id(arcane_devotee, noble), skill(research)).
lifepath_provides(id(arcane_devotee, noble), skill(symbology)).

lifepath(id(religious_acolyte, noble), page(bwg, 177), 5, [city, religious, court]).
lifepath_provides(id(religious_acolyte, noble), flag(acolyte)).
lifepath_provides(id(religious_acolyte, noble), trait(tonsured)).
lifepath_provides(id(religious_acolyte, noble), trait(faithful)).
lifepath_provides(id(religious_acolyte, noble), skill(doctrine)).
lifepath_provides(id(religious_acolyte, noble), skill(bureaucracy)).
lifepath_provides(id(religious_acolyte, noble), skill(write)).
lifepath_provides(id(religious_acolyte, noble), skill(read)).
lifepath_provides(id(religious_acolyte, noble), skill(etiquette)).

lifepath(id(young_lady, noble), page(bwg, 178), 10, [city, court, religious]).
lifepath_requires(id(young_lady, noble), [position(2)]).
lifepath_requires(id(young_lady, noble), [position(3), not(lifepath(young_lady))]).
lifepath_provides(id(young_lady, noble), flag(female)).
lifepath_provides(id(young_lady, noble), skill(write)).
lifepath_provides(id(young_lady, noble), skill(read)).
lifepath_provides(id(young_lady, noble), skill(etiquette)).
lifepath_provides(id(young_lady, noble), skill(dance)).
lifepath_provides(id(young_lady, noble), skill(astrology)).
lifepath_provides(id(young_lady, noble), skill(musical_instrument)).
lifepath_provides(id(young_lady, noble), skill(composition)).
lifepath_provides(id(young_lady, noble), skill(field_dressing)).
lifepath_provides(id(young_lady, noble), skill(apothecary)).
lifepath_provides(id(young_lady, noble), skill(doctrine)).

lifepath(id(knight, noble), page(bwg, 178), 5, [soldier, city, outcast, religious]).
lifepath_requires(id(knight, noble), [lifepath(squire)]).
lifepath_requires(id(knight, noble), [lifepath(cavalryman)]).
lifepath_provides(id(knight, noble), trait(sworn_homage)).
lifepath_provides(id(knight, noble), skill(mounted_combat_training)).
lifepath_provides(id(knight, noble), skill(appropriate_weapons)).
lifepath_provides(id(knight, noble), skill(intimidation)).
lifepath_provides(id(knight, noble), skill(hunting)).
lifepath_provides(id(knight, noble), skill(conspicuous)).

lifepath(id(lady, noble), page(bwg, 178), 5, [city, outcast, religious, court]).
lifepath_requires(id(lady, noble), [lifepath(young_lady)]).
lifepath_requires(id(lady, noble), [lifepath(courtier)]).
lifepath_requires(id(lady, noble), [lifepath(knight)]).
lifepath_requires(id(lady, noble), [lifepath(city_wife)]).
lifepath_provides(id(lady, noble), flag(female)).
lifepath_provides(id(lady, noble), skill(etiquette)).
lifepath_provides(id(lady, noble), skill(estate_management)).
lifepath_provides(id(lady, noble), skill(persuasion)).
lifepath_provides(id(lady, noble), skill(seduction)).
lifepath_provides(id(lady, noble), skill(inconspicuous)).
lifepath_provides(id(lady, noble), skill(doctrine)).
lifepath_provides(id(lady, noble), skill(husband_wise)).
lifepath_provides(id(lady, noble), skill(estate_wise)).
lifepath_provides(id(lady, noble), skill(staff_wise)).
lifepath_provides(id(lady, noble), skill(court_wise)).

lifepath(id(lord, noble), page(bwg, 178), 7, [soldier, court, city]).
lifepath_requires(id(lord, noble), [lifepath(knight)]).
lifepath_requires(id(lord, noble), [trait(your_lordship)]).
lifepath_provides(id(lord, noble), skill(hunting)).
lifepath_provides(id(lord, noble), skill(dance)).
lifepath_provides(id(lord, noble), skill(sing)).
lifepath_provides(id(lord, noble), skill(falconry)).
lifepath_provides(id(lord, noble), skill(estate_management)).

lifepath(id(dame, noble), page(bwg, 178), 7, [city, court, outcast, religious]).
lifepath_requires(id(dame, noble), [lifepath(lady)]).
lifepath_requires(id(dame, noble), [lifepath(city_wife)]).
lifepath_provides(id(dame, noble), skill(estate_management)).
lifepath_provides(id(dame, noble), skill(noble_wise)).

lifepath(id(baron, noble), page(bwg, 178), 8, [court, soldier]).
lifepath_requires(id(baron, noble), [and([lifepath(knight), trait(your_lordship)])]).
lifepath_requires(id(baron, noble), [lifepath(magnate)]).
lifepath_requires(id(baron, noble), [lifepath(master_of_horses)]).
lifepath_requires(id(baron, noble), [lifepath(steward)]).
lifepath_requires(id(baron, noble), [lifepath(lord)]).
lifepath_requires(id(baron, noble), [lifepath(constable)]).
lifepath_requires(id(baron, noble), [lifepath(justiciar)]).
lifepath_provides(id(baron, noble), trait(noblesse_oblige)).
lifepath_provides(id(baron, noble), trait(regal_bearing)).
lifepath_provides(id(baron, noble), trait(pompous)).
lifepath_provides(id(baron, noble), trait(sharp_dresser)).
lifepath_provides(id(baron, noble), trait(callous)).

lifepath(id(viscount, noble), page(bwg, 178), 9, [court, soldier]).
lifepath_requires(id(viscount, noble), [and([lifepath(knight), trait(your_eminence)])]).
lifepath_requires(id(viscount, noble), [lifepath(magnate)]).
lifepath_requires(id(viscount, noble), [lifepath(baron)]).
lifepath_requires(id(viscount, noble), [lifepath(constable)]).
lifepath_requires(id(viscount, noble), [lifepath(justiciar)]).
lifepath_provides(id(viscount, noble), trait(noblesse_oblige)).
lifepath_provides(id(viscount, noble), trait(regal_bearing)).
lifepath_provides(id(viscount, noble), trait(pompous)).
lifepath_provides(id(viscount, noble), trait(sharp_dresser)).
lifepath_provides(id(viscount, noble), trait(callous)).

lifepath(id(count, noble), page(bwg, 179), 10, [court, soldier]).
lifepath_requires(id(count, noble), [and([lifepath(knight), trait(your_eminence)])]).
lifepath_requires(id(count, noble), [lifepath(magnate)]).
lifepath_requires(id(count, noble), [lifepath(constable)]).
lifepath_requires(id(count, noble), [lifepath(justiciar)]).
lifepath_provides(id(count, noble), trait(noblesse_oblige)).
lifepath_provides(id(count, noble), trait(regal_bearing)).
lifepath_provides(id(count, noble), trait(pompous)).
lifepath_provides(id(count, noble), trait(sharp_dresser)).
lifepath_provides(id(count, noble), trait(callous)).

lifepath(id(duke, noble), page(bwg, 179), 10, [court, soldier]).
lifepath_requires(id(duke, noble), [and([lifepath(knight), trait(your_grace)])]).
lifepath_provides(id(duke, noble), trait(noblesse_oblige)).
lifepath_provides(id(duke, noble), trait(regal_bearing)).
lifepath_provides(id(duke, noble), trait(pompous)).
lifepath_provides(id(duke, noble), trait(sharp_dresser)).
lifepath_provides(id(duke, noble), trait(callous)).

lifepath(id(noble_prince, noble), page(bwg, 179), 10, [court, soldier]).
lifepath_requires(id(noble_prince, noble), [and([
    or(lifepath(duke), lifepath(knight)),
    trait(your_grace)
])]).
lifepath_provides(id(noble_prince, noble), trait(noblesse_oblige)).
lifepath_provides(id(noble_prince, noble), trait(regal_bearing)).
lifepath_provides(id(noble_prince, noble), trait(pompous)).
lifepath_provides(id(noble_prince, noble), trait(sharp_dresser)).
lifepath_provides(id(noble_prince, noble), trait(callous)).

lifepath(id(prince_of_the_blood, noble), page(bwg, 179), 20, [court, soldier, religious]).
lifepath_requires(id(prince_of_the_blood, noble), [and([flag(gm_approval), trait(your_grace)])]).
lifepath_provides(id(prince_of_the_blood, noble), trait(born_to_be_king)).
lifepath_provides(id(prince_of_the_blood, noble), trait(noblesse_oblige)).
lifepath_provides(id(prince_of_the_blood, noble), trait(regal_bearing)).
lifepath_provides(id(prince_of_the_blood, noble), trait(pompous)).
lifepath_provides(id(prince_of_the_blood, noble), trait(sharp_dresser)).
lifepath_provides(id(prince_of_the_blood, noble), trait(callous)).


%%% ----- NOBLE COURT SUBSETTING ----- %%%


lifepath(id(minstrel, court), page(bwg, 179), 4, [city, outcast, villager]).
lifepath_provides(id(minstrel, court), trait(recondite)).
lifepath_provides(id(minstrel, court), skill(poetry)).
lifepath_provides(id(minstrel, court), skill(sing)).
lifepath_provides(id(minstrel, court), skill(musical_instrument)).

lifepath(id(court_jester, court), page(bwg, 179), 5, [outcast, servitude]).
lifepath_provides(id(court_jester, court), trait(scapegoat)).
lifepath_provides(id(court_jester, court), trait(aura_of_innocence)).
lifepath_provides(id(court_jester, court), skills(sing)).
lifepath_provides(id(court_jester, court), skills(sleight_of_hand)).
lifepath_provides(id(court_jester, court), skills(climbing)).
lifepath_provides(id(court_jester, court), skills(conspicuous)).
lifepath_provides(id(court_jester, court), skills(throwing)).
lifepath_provides(id(court_jester, court), skills(ugly_truth)).

lifepath(id(court_artist, court), page(bwg, 179), 6, [city, outcast]).
lifepath_requires(id(court_artist, court), [lifepath(court_jester)]).
lifepath_requires(id(court_artist, court), [lifepath(painter)]).
lifepath_requires(id(court_artist, court), [lifepath(thinker)]).
lifepath_requires(id(court_artist, court), [lifepath(scholar)]).
lifepath_requires(id(court_artist, court), [lifepath(sculptor)]).
lifepath_provides(id(court_artist, court), trait(romantic)).
lifepath_provides(id(court_artist, court), skill(sculpture)).
lifepath_provides(id(court_artist, court), skill(painting)).
lifepath_provides(id(court_artist, court), skill(engraving)).
lifepath_provides(id(court_artist, court), skill(seduction)).
lifepath_provides(id(court_artist, court), skill(genius_wise)).

lifepath(id(servant, court), page(bwg, 179), 6, [outcast, servitude]).
lifepath_provides(id(servant, court), trait(veneer_of_obedience)).
lifepath_provides(id(servant, court), trait(lifting_heavy_things)).
lifepath_provides(id(servant, court), trait(bored)).
lifepath_provides(id(servant, court), skill(inconspicuous)).
lifepath_provides(id(servant, court), skill(etiquette)).
lifepath_provides(id(servant, court), skill(court_gossip_wise)).

lifepath(id(nurse, court), page(bwg, 180), 6, [outcast, servitude, villager]).
lifepath_provides(id(nurse, court), flag(female)).
lifepath_provides(id(nurse, court), trait(maternal)).
lifepath_provides(id(nurse, court), skill(child_rearing)).
lifepath_provides(id(nurse, court), skill(etiquette)).
lifepath_provides(id(nurse, court), skill(court_gossip_wise)).
lifepath_provides(id(nurse, court), skill(field_dressing)).
lifepath_provides(id(nurse, court), skill(child_wise)).

lifepath(id(groom, court), page(bwg, 180), 4, [city, villager, soldier]).
lifepath_provides(id(groom, court), skill(animal_husbandry)).
lifepath_provides(id(groom, court), skill(riding)).
lifepath_provides(id(groom, court), skill(mending)).
lifepath_provides(id(groom, court), skill(border_wise)).
lifepath_provides(id(groom, court), skill(road_wise)).

lifepath(id(gardener, court), page(bwg, 180), 10, [city, servitude]).
lifepath_provides(id(gardener, court), trait(earthy_smell)).
lifepath_provides(id(gardener, court), trait(salt_of_the_earth)).
lifepath_provides(id(gardener, court), trait(down_to_earth)).
lifepath_provides(id(gardener, court), trait(affinity_for_plants)).
lifepath_provides(id(gardener, court), skill(plant_wise)).
lifepath_provides(id(gardener, court), skill(flower_wise)).
lifepath_provides(id(gardener, court), skill(tree_wise)).
lifepath_provides(id(gardener, court), skill(pest_wise)).
lifepath_provides(id(gardener, court), skill(herbalism)).
lifepath_provides(id(gardener, court), skill(farming)).
lifepath_provides(id(gardener, court), skill(almanac)).

lifepath(id(torturer, court), page(bwg, 180), 5, [outcast, servitude, soldier]).
lifepath_provides(id(torturer, court), trait(unsavory_madman)).
lifepath_provides(id(torturer, court), skill(interogation)).
lifepath_provides(id(torturer, court), skill(torture)).
lifepath_provides(id(torturer, court), skill(anatomy)).
lifepath_provides(id(torturer, court), skill(torture_device_wise)).

lifepath(id(forester, court), page(bwg, 180), 7, [peasant, soldier, villager, outcast]).
lifepath_provides(id(forester, court), skill(observation)).
lifepath_provides(id(forester, court), skill(orienteering)).
lifepath_provides(id(forester, court), skill(foraging)).
lifepath_provides(id(forester, court), skill(survival)).
lifepath_provides(id(forester, court), skill(tracking)).
lifepath_provides(id(forester, court), skill(trapper)).
lifepath_provides(id(forester, court), skill(cudgel)).
lifepath_provides(id(forester, court), skill(staff)).
lifepath_provides(id(forester, court), skill(bow)).
lifepath_provides(id(forester, court), skill(poacher_wise)).
lifepath_provides(id(forester, court), skill(park_wise)).

lifepath(id(student, court), page(bwg, 180), 4, [soldier, city, servitude, noble]).
lifepath_provides(id(student, court), trait(dangerous)).
lifepath_provides(id(student, court), trait(geometric)).
lifepath_provides(id(student, court), skill(write)).
lifepath_provides(id(student, court), skill(read)).
lifepath_provides(id(student, court), skill(rule_of_law)).
lifepath_provides(id(student, court), skill(oratory)).
lifepath_provides(id(student, court), skill(doctrine)).
lifepath_provides(id(student, court), skill(etiquette)).

lifepath(id(page, court), page(bwg, 180), 6, [soldier, city, servitude, noble]).
lifepath_requires(id(page, court), [position(2)]).
lifepath_provides(id(page, court), skills(riding)).
lifepath_provides(id(page, court), skills(brawling)).
lifepath_provides(id(page, court), skills(write)).
lifepath_provides(id(page, court), skills(read)).
lifepath_provides(id(page, court), skills(sword)).

lifepath(id(man_at_arms, court), page(bwg, 180), 6, [soldier, city, outcast]).
lifepath_requires(id(man_at_arms, court), [lifepath(page)]).
lifepath_requires(id(man_at_arms, court), [lifepath(squire)]).
lifepath_requires(id(man_at_arms, court), [setting(soldier)]).
lifepath_provides(id(man_at_arms, court), skill(mounted_combat_training)).
lifepath_provides(id(man_at_arms, court), skill(shield_training)).
lifepath_provides(id(man_at_arms, court), skill(armor_training)).
lifepath_provides(id(man_at_arms, court), skill(brawling)).
lifepath_provides(id(man_at_arms, court), skill(intimidation)).
lifepath_provides(id(man_at_arms, court), skill(etiquette)).
lifepath_provides(id(man_at_arms, court), skill(appropriate_weapons)).

lifepath(id(falconer, court), page(bwg, 180), 5, [peasant, soldier, city]).
lifepath_provides(id(falconer, court), trait(boaster)).
lifepath_provides(id(falconer, court), trait(weird)).
lifepath_provides(id(falconer, court), trait(birdie_talk)).
lifepath_provides(id(falconer, court), skill(falconry)).
lifepath_provides(id(falconer, court), skill(animal_husbandry)).
lifepath_provides(id(falconer, court), skill(hunting)).

lifepath(id(huntsman, court), page(bwg, 180), 5, [peasant, soldier]).
lifepath_provides(id(huntsman, court), skill(hunting)).
lifepath_provides(id(huntsman, court), skill(animal_husbandry)).
lifepath_provides(id(huntsman, court), skill(forest_wise)).
lifepath_provides(id(huntsman, court), skill(stealthy)).
lifepath_provides(id(huntsman, court), skill(cooking)).
lifepath_provides(id(huntsman, court), skill(tracking)).
lifepath_provides(id(huntsman, court), skill(orienteering)).
lifepath_provides(id(huntsman, court), skill(crossbow)).

lifepath(id(herald, court), page(bwg, 181), 4, [soldier, servitude, city]).
lifepath_provides(id(herald, court), trait(formalist)).
lifepath_provides(id(herald, court), trait(rainman)).
lifepath_provides(id(herald, court), trait(eidetic_memory)).
lifepath_provides(id(herald, court), skill(heraldry)).
lifepath_provides(id(herald, court), skill(noble_wise)).

lifepath(id(court_chef, court), page(bwg, 181), 5, [outcast, city]).
lifepath_provides(id(court_chef, court), skill(cooking)).
lifepath_provides(id(court_chef, court), skill(sing)).
lifepath_provides(id(court_chef, court), skill(herbalism)).
lifepath_provides(id(court_chef, court), skill(falsehood)).

lifepath(id(squire, court), page(bwg, 181), 5, [soldier, city, servitude, outcast, noble]).
lifepath_requires(id(squire, court), [lifepath(corrupt_sergeant)]).
lifepath_requires(id(squire, court), [lifepath(village_sergeant)]).
lifepath_requires(id(squire, court), [lifepath(sergeant)]).
lifepath_requires(id(squire, court), [flag(sergeant)]).
lifepath_requires(id(squire, court), [lifepath(veteran)]).
lifepath_requires(id(squire, court), [lifepath(page)]).
lifepath_requires(id(squire, court), [lifepath(man_at_arms)]).
lifepath_provides(id(squire, court), trait(pragmatic)).
lifepath_provides(id(squire, court), trait(tough)).
lifepath_provides(id(squire, court), trait(determined)).
lifepath_provides(id(squire, court), skill(sword)).
lifepath_provides(id(squire, court), skill(armor_training)).
lifepath_provides(id(squire, court), skill(shield_training)).
lifepath_provides(id(squire, court), skill(lance)).
lifepath_provides(id(squire, court), skill(brawling)).
lifepath_provides(id(squire, court), skill(mounted_combat_training)).
lifepath_provides(id(squire, court), skill(etiquette)).

lifepath(id(young_lady, court), page(bwg, 181), 9, [city, noble, religious]).
lifepath_requires(id(young_lady, court), [position(2)]).
lifepath_requires(id(young_lady, court), [and([position(3), not(lifepath(young_lady))])]).
lifepath_provides(id(young_lady, court), skill(write)).
lifepath_provides(id(young_lady, court), skill(read)).
lifepath_provides(id(young_lady, court), skill(etiquette)).
lifepath_provides(id(young_lady, court), skill(poetry)).
lifepath_provides(id(young_lady, court), skill(astrology)).
lifepath_provides(id(young_lady, court), skill(musical_instrument)).
lifepath_provides(id(young_lady, court), skill(composition)).
lifepath_provides(id(young_lady, court), skill(field_dressing)).
lifepath_provides(id(young_lady, court), skill(apothecary)).
lifepath_provides(id(young_lady, court), skill(court_gossip_wise)).

lifepath(id(knight, court), page(bwg, 181), 4, [soldier, noble, outcast]).
lifepath_requires(id(knight, court), [lifepath(squire)]).
lifepath_requires(id(knight, court), [lifepath(cavalryman)]).
lifepath_provides(id(knight, court), trait(sworn_homage)).
lifepath_provides(id(knight, court), trait(sense_of_entitlement)).
lifepath_provides(id(knight, court), skill(conspicuous)).
lifepath_provides(id(knight, court), skill(intimidation)).
lifepath_provides(id(knight, court), skill(falconry)).
lifepath_provides(id(knight, court), skill(appropriate_weapons)).

lifepath(id(courtier, court), page(bwg, 181), 5, [city, outcast, noble]).
lifepath_provides(id(courtier, court), trait(rapier_wit)).
lifepath_provides(id(courtier, court), skill(etiquette)).
lifepath_provides(id(courtier, court), skill(observation)).
lifepath_provides(id(courtier, court), skill(persuasion)).
lifepath_provides(id(courtier, court), skill(seduction)).
lifepath_provides(id(courtier, court), skill(inconspicuous)).
lifepath_provides(id(courtier, court), skill(court_gossip_wise)).
lifepath_provides(id(courtier, court), skill(noble_wise)).

lifepath(id(governess, court), page(bwg, 181), 8, [outcast, servitude, villager]).
lifepath_requires(id(governess, court), [lifepath(nurse)]).
lifepath_requires(id(governess, court), [lifepath(midwife)]).
lifepath_requires(id(governess, court), [lifepath(lady)]).
lifepath_requires(id(governess, court), [flag(wife)]).
lifepath_provides(id(governess, court), trait(dismissive)).
lifepath_provides(id(governess, court), trait(you_should_know_better_than_that)).
lifepath_provides(id(governess, court), trait(bitter)).
lifepath_provides(id(governess, court), skill(family_wise)).
lifepath_provides(id(governess, court), skill(administation)).
lifepath_provides(id(governess, court), skill(intimidation)).
lifepath_provides(id(governess, court), skill(etiquette)).
lifepath_provides(id(governess, court), skill(persuasion)).
lifepath_provides(id(governess, court), skill(ugly_truth)).

lifepath(id(chaplain, court), page(bwg, 181), 5, [soldier, city, religious, servitude]).
lifepath_requires(id(chaplain, court), [lifepath(military_order)]).
lifepath_requires(id(chaplain, court), [flag(priest)]).
lifepath_provides(id(chaplain, court), skill(oratory)).
lifepath_provides(id(chaplain, court), skill(doctrine)).
lifepath_provides(id(chaplain, court), skill(riding)).
lifepath_provides(id(chaplain, court), skill(armor_training)).
lifepath_provides(id(chaplain, court), skill(mounted_combat_training)).
lifepath_provides(id(chaplain, court), skill(shield_training)).
lifepath_provides(id(chaplain, court), skill(appropriate_weapons)).

lifepath(id(court_sorcerer, court), page(bwg, 182), 8, [outcast, city]).
lifepath_requires(id(court_sorcerer, court), [lifepath(arcane_devotee)]).
lifepath_requires(id(court_sorcerer, court), [lifepath(rogue_wizard)]).
lifepath_requires(id(court_sorcerer, court), [lifepath(sorcerer)]).
lifepath_provides(id(court_sorcerer, court), trait(inscrutable)).
lifepath_provides(id(court_sorcerer, court), trait(gifted)).
lifepath_provides(id(court_sorcerer, court), trait(second_sight)).
lifepath_provides(id(court_sorcerer, court), skill(etiquette)).
lifepath_provides(id(court_sorcerer, court), skill(falsehood)).
lifepath_provides(id(court_sorcerer, court), skill(astrology)).
lifepath_provides(id(court_sorcerer, court), skill(alchemy)).
lifepath_provides(id(court_sorcerer, court), skill(sorcery)).

lifepath(id(court_lawyer, court), page(bwg, 182), 8, [city, outcast, religious]).
lifepath_requires(id(court_lawyer, court), [lifepath(student)]).
lifepath_requires(id(court_lawyer, court), [lifepath(advocate)]).
lifepath_provides(id(court_lawyer, court), trait(rhetorical)).
lifepath_provides(id(court_lawyer, court), trait(evasive)).
lifepath_provides(id(court_lawyer, court), skill(etiquette)).
lifepath_provides(id(court_lawyer, court), skill(oratory)).
lifepath_provides(id(court_lawyer, court), skill(persuasion)).
lifepath_provides(id(court_lawyer, court), skill(history)).
lifepath_provides(id(court_lawyer, court), skill(amercement)).

lifepath(id(court_doctor, court), page(bwg, 182), 8, [city, outcast]).
lifepath_requires(id(court_doctor, court), [lifepath(student)]).
lifepath_requires(id(court_doctor, court), [lifepath(itinerant_monk)]).
lifepath_requires(id(court_doctor, court), [lifepath(cloistered_nun)]).
lifepath_requires(id(court_doctor, court), [flag(priest)]).
lifepath_provides(id(court_doctor, court), trait(incomprehensible_diagnosis)).
lifepath_provides(id(court_doctor, court), skill(etiquette)).
lifepath_provides(id(court_doctor, court), skill(apothecary)).
lifepath_provides(id(court_doctor, court), skill(bloodletting)).
lifepath_provides(id(court_doctor, court), skill(surgery)).
lifepath_provides(id(court_doctor, court), skill(anatomy)).
lifepath_provides(id(court_doctor, court), skill(astrology)).
lifepath_provides(id(court_doctor, court), skill(falsehood)).

lifepath(id(chronicler, court), page(bwg, 182), 10, [city, outcast, villager]).
lifepath_requires(id(chronicler, court), [lifepath(student)]).
lifepath_requires(id(chronicler, court), [lifepath(custodian)]).
lifepath_requires(id(chronicler, court), [lifepath(interpreter)]).
lifepath_requires(id(chronicler, court), [lifepath(archivist)]).
lifepath_requires(id(chronicler, court), [lifepath(young_lady)]).
lifepath_provides(id(chronicler, court), trait(prone_to_exaggeration)).
lifepath_provides(id(chronicler, court), trait(flatterer)).
lifepath_provides(id(chronicler, court), trait(denouncer)).
lifepath_provides(id(chronicler, court), trait(cynical)).
lifepath_provides(id(chronicler, court), trait(righteous)).

lifepath(id(armorer, court), page(bwg, 182), 7, [city, soldier]).
lifepath_requires(id(armorer, court), [lifepath(journeyman)]).
lifepath_provides(id(armorer, court), trait(proud)).
lifepath_provides(id(armorer, court), skill(etching)).
lifepath_provides(id(armorer, court), skill(armorer)).
lifepath_provides(id(armorer, court), skill(blacksmith)).
lifepath_provides(id(armorer, court), skill(tanner)).
lifepath_provides(id(armorer, court), skill(sewing)).
lifepath_provides(id(armorer, court), skill(weaponsmith)).

lifepath(id(atilliator, court), page(bwg, 182), 10, [soldier, city]).
lifepath_requires(id(atilliator, court), [lifepath(journeyman)]).
lifepath_provides(id(atilliator, court), trait(professionally_diligent)).
lifepath_provides(id(atilliator, court), skill(atilliator)).
lifepath_provides(id(atilliator, court), skill(carpentry)).
lifepath_provides(id(atilliator, court), skill(carving)).

lifepath(id(court_priest, court), page(bwg, 182), 6, [outcast, city, religious]).
lifepath_requires(id(court_priest, court), [lifepath(chaplain)]).
lifepath_requires(id(court_priest, court), [flag(priest)]).
lifepath_requires(id(court_priest, court), [lifepath(religious_acolyte)]).
lifepath_provides(id(court_priest, court), flag(priest)).
lifepath_provides(id(court_priest, court), trait(royal_favorite)).
lifepath_provides(id(court_priest, court), trait(faithful)).
lifepath_provides(id(court_priest, court), skill(etiquette)).
lifepath_provides(id(court_priest, court), skill(history)).
lifepath_provides(id(court_priest, court), skill(symbology)).
lifepath_provides(id(court_priest, court), skill(doctrine)).
lifepath_provides(id(court_priest, court), skill(persuasion)).

lifepath(id(steward, court), page(bwg, 182), 7, [city, noble, outcast, religious]).
lifepath_requires(id(steward, court), [lifepath(town_official)]).
lifepath_requires(id(steward, court), [lifepath(municipal_minister)]).
lifepath_requires(id(steward, court), [lifepath(judge)]).
lifepath_requires(id(steward, court), [lifepath(court_lawyer)]).
lifepath_requires(id(steward, court), [lifepath(court_doctor)]).
lifepath_requires(id(steward, court), [lifepath(governess)]).
lifepath_requires(id(steward, court), [lifepath(young_lady)]).
lifepath_requires(id(steward, court), [lifepath(magnate)]).
lifepath_requires(id(steward, court), [lifepath(lord)]).
lifepath_provides(id(steward, court), skill(estate_management)).
lifepath_provides(id(steward, court), skill(accounting)).
lifepath_provides(id(steward, court), skill(observation)).
lifepath_provides(id(steward, court), skill(manor_wise)).

lifepath(id(master_of_horses, court), page(bwg, 182), 8, [city, soldier]).
lifepath_requires(id(master_of_horses, court), [lifepath(captain)]).
lifepath_requires(id(master_of_horses, court), [lifepath(baron)]).
lifepath_requires(id(master_of_horses, court), [lifepath(saddler)]).
lifepath_requires(id(master_of_horses, court), [lifepath(merchant)]).
lifepath_requires(id(master_of_horses, court), [lifepath(magnate)]).
lifepath_provides(id(master_of_horses, court), trait(love_of_the_horse)).
lifepath_provides(id(master_of_horses, court), trait(low_speech)).
lifepath_provides(id(master_of_horses, court), trait(affinity_for_horses)).
lifepath_provides(id(master_of_horses, court), skill(horse_husbandry)).
lifepath_provides(id(master_of_horses, court), skill(appraisal)).
lifepath_provides(id(master_of_horses, court), skill(horse_wise)).

lifepath(id(master_of_hounds, court), page(bwg, 183), 6, [city, soldier]).
lifepath_requires(id(master_of_hounds, court), [lifepath(captain)]).
lifepath_requires(id(master_of_hounds, court), [lifepath(baron)]).
lifepath_requires(id(master_of_hounds, court), [lifepath(saddler)]).
lifepath_requires(id(master_of_hounds, court), [lifepath(merchant)]).
lifepath_requires(id(master_of_hounds, court), [lifepath(magnate)]).
lifepath_provides(id(master_of_hounds, court), trait(dog_lover)).
lifepath_provides(id(master_of_hounds, court), trait(pigpen)).
lifepath_provides(id(master_of_hounds, court), trait(emotional)).
lifepath_provides(id(master_of_hounds, court), trait(iron_stomach)).
lifepath_provides(id(master_of_hounds, court), skill(dog_husbandry)).
lifepath_provides(id(master_of_hounds, court), skill(dog_wise)).
lifepath_provides(id(master_of_hounds, court), skill(hunting)).
lifepath_provides(id(master_of_hounds, court), skill(instruction)).
lifepath_provides(id(master_of_hounds, court), skill(mimicry)).

lifepath(id(hostage, court), page(bwg, 183), 6, [city, noble, soldier, religious]).
lifepath_requires(id(hostage, court), [setting(noble)]).
lifepath_provides(id(hostage, court), trait(homesick)).
lifepath_provides(id(hostage, court), trait(bored)).
lifepath_provides(id(hostage, court), trait(darling_of_the_court)).
lifepath_provides(id(hostage, court), skill(etiquette)).
lifepath_provides(id(hostage, court), skill(court_wise)).
lifepath_provides(id(hostage, court), skill(foreign_languages)).
lifepath_provides(id(hostage, court), skill(foreign_history)).

lifepath(id(bailiff, court), page(bwg, 183), 4, [city, soldier, outcast]).
lifepath_requires(id(bailiff, court), [lifepath(knight)]).
lifepath_requires(id(bailiff, court), [trait(your_lordship)]).
lifepath_provides(id(bailiff, court), trait(nose_for_trouble)).
lifepath_provides(id(bailiff, court), skill(intimidation)).
lifepath_provides(id(bailiff, court), skill(interrogation)).
lifepath_provides(id(bailiff, court), skill(rule_of_law)).
lifepath_provides(id(bailiff, court), skill(outlaw_wise)).

lifepath(id(justiciar, court), page(bwg, 183), 5, [city, soldier, religious, outcast]).
lifepath_requires(id(justiciar, court), [lifepath(judge)]).
lifepath_requires(id(justiciar, court), [lifepath(captain)]).
lifepath_requires(id(justiciar, court), [lifepath(bailiff)]).
lifepath_requires(id(justiciar, court), [lifepath(lord)]).
lifepath_provides(id(justiciar, court), trait(stern_demeanor)).
lifepath_provides(id(justiciar, court), trait(amenable_to_other_options)).
lifepath_provides(id(justiciar, court), skill(rule_of_law)).
lifepath_provides(id(justiciar, court), skill(amercement)).
lifepath_provides(id(justiciar, court), skill(interrogation)).
lifepath_provides(id(justiciar, court), skill(criminal_wise)).
lifepath_provides(id(justiciar, court), skill(circuit_wise)).

lifepath(id(coroner, court), page(bwg, 183), 6, [city, soldier]).
lifepath_requires(id(coroner, court), [lifepath(steward)]).
lifepath_requires(id(coroner, court), [lifepath(town_official)]).
lifepath_requires(id(coroner, court), [lifepath(municipal_minister)]).
lifepath_requires(id(coroner, court), [lifepath(judge)]).
lifepath_requires(id(coroner, court), [lifepath(court_lawyer)]).
lifepath_requires(id(coroner, court), [lifepath(court_doctor)]).
lifepath_requires(id(coroner, court), [lifepath(magnate)]).
lifepath_requires(id(coroner, court), [lifepath(lord)]).
lifepath_provides(id(coroner, court), trait(hard_hearted)).
lifepath_provides(id(coroner, court), trait(seen_it_all)).
lifepath_provides(id(coroner, court), skill(rule_of_law)).
lifepath_provides(id(coroner, court), skill(anatomy)).
lifepath_provides(id(coroner, court), skill(writ_wise)).
lifepath_provides(id(coroner, court), skill(obseravtion)).

lifepath(id(constable, court), page(bwg, 183), 6, [city, soldier, outcast]).
lifepath_requires(id(constable, court), [lifepath(captain)]).
lifepath_requires(id(constable, court), [lifepath(baron)]).
lifepath_requires(id(constable, court), [lifepath(viscount)]).
lifepath_requires(id(constable, court), [lifepath(count)]).
lifepath_requires(id(constable, court), [lifepath(duke)]).
lifepath_requires(id(constable, court), [lifepath(prince)]).
lifepath_provides(id(constable, court), trait(weight_of_the_world)).
lifepath_provides(id(constable, court), skill(command)).
lifepath_provides(id(constable, court), skill(heraldry)).
lifepath_provides(id(constable, court), skill(logistics)).
lifepath_provides(id(constable, court), skill(kingdom_wise)).
lifepath_provides(id(constable, court), skill(obligation_wise)).
lifepath_provides(id(constable, court), skill(soldier_wise)).
lifepath_provides(id(constable, court), skill(cavalry_wise)).

lifepath(id(treasurer, court), page(bwg, 183), 7, [city, soldier, religious]).
lifepath_requires(id(treasurer, court), [lifepath(steward)]).
lifepath_requires(id(treasurer, court), [lifepath(town_official)]).
lifepath_requires(id(treasurer, court), [lifepath(municipal_minister)]).
lifepath_requires(id(treasurer, court), [lifepath(judge)]).
lifepath_requires(id(treasurer, court), [lifepath(court_lawyer)]).
lifepath_requires(id(treasurer, court), [lifepath(court_doctor)]).
lifepath_requires(id(treasurer, court), [lifepath(magnate)]).
lifepath_requires(id(treasurer, court), [lifepath(baron)]).
lifepath_requires(id(treasurer, court), [lifepath(bishop)]).
lifepath_requires(id(treasurer, court), [lifepath(viscount)]).
lifepath_requires(id(treasurer, court), [lifepath(count)]).
lifepath_requires(id(treasurer, court), [lifepath(duke)]).
lifepath_provides(id(treasurer, court), trait(pecunious)).
lifepath_provides(id(treasurer, court), trait(lavish_taste)).
lifepath_provides(id(treasurer, court), skill(accounting)).
lifepath_provides(id(treasurer, court), skill(estate_management)).
lifepath_provides(id(treasurer, court), skill(tax_wise)).
lifepath_provides(id(treasurer, court), skill(debt_wise)).

lifepath(id(chamberlain, court), page(bwg, 184), 7, [city, soldier, religious]).
lifepath_requires(id(chamberlain, court), [lifepath(scholar)]).
lifepath_requires(id(chamberlain, court), [lifepath(steward)]).
lifepath_requires(id(chamberlain, court), [lifepath(town_official)]).
lifepath_requires(id(chamberlain, court), [lifepath(municipal_minister)]).
lifepath_requires(id(chamberlain, court), [lifepath(judge)]).
lifepath_requires(id(chamberlain, court), [lifepath(court_lawyer)]).
lifepath_requires(id(chamberlain, court), [lifepath(court_doctor)]).
lifepath_requires(id(chamberlain, court), [lifepath(magnate)]).
lifepath_requires(id(chamberlain, court), [lifepath(baron)]).
lifepath_requires(id(chamberlain, court), [lifepath(bishop)]).
lifepath_requires(id(chamberlain, court), [lifepath(viscount)]).
lifepath_requires(id(chamberlain, court), [lifepath(count)]).
lifepath_requires(id(chamberlain, court), [lifepath(duke)]).

lifepath(id(advisor_to_the_court, court), page(bwg, 184), 3, any_except([])).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(thinker)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(captain)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(sea_captain)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(magnate)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(master_craftsman)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(artisan)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(bishop)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(rogue_wizard)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(mad_summoner)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(heretic_priest)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(dame)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(baron)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(viscount)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(count)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(duke)]).
lifepath_requires(id(advisor_to_the_court, court), [lifepath(prince)]).


%%% ----- RELIGIOUS SUBSETTING ----- %%%

lifepath(id(castrati, religious), page(bwg, 184), 7, [outcast, court]).
lifepath_requires(id(castrati, religious), [not(flag(female))]).
lifepath_provides(id(castrati, religious), trait(eunuch)).
lifepath_provides(id(castrati, religious), trait(perfect_pitch)).
lifepath_provides(id(castrati, religious), trait(patient)).
lifepath_provides(id(castrati, religious), trait(scheming)).
lifepath_provides(id(castrati, religious), skills(sing)).
lifepath_provides(id(castrati, religious), skills(persuasion)).
lifepath_provides(id(castrati, religious), skills(falsehood)).
lifepath_provides(id(castrati, religious), skills(etiquette)).
lifepath_provides(id(castrati, religious), skills(administation)).

lifepath(id(pardoner, religious), page(bwg, 184), 5, [peasant, city, villager, outcast]).
lifepath_provides(id(pardoner, religious), trait(pardoner)).
lifepath_provides(id(pardoner, religious), trait(corrupt)).
lifepath_provides(id(pardoner, religious), skill(persuasion)).
lifepath_provides(id(pardoner, religious), skill(falsehood)).
lifepath_provides(id(pardoner, religious), skill(intimidation)).
lifepath_provides(id(pardoner, religious), skill(doctrine)).

lifepath(id(zealous_convert, religious), page(bwg, 184), 4, [outcast, peasant, servitude, court]).
lifepath_provides(id(zealous_convert, religious), trait(infallible_religious_logic)).
lifepath_provides(id(zealous_convert, religious), trait(righteous)).
lifepath_provides(id(zealous_convert, religious), trait(demagogue)).
lifepath_provides(id(zealous_convert, religious), skill(religious_diatribe)).
lifepath_provides(id(zealous_convert, religious), skill(doctrine)).
lifepath_provides(id(zealous_convert, religious), skill(rhetoric)).

lifepath(id(military_order, religious), page(bwg, 184), 3, any_except([peasant])).
lifepath_requires(id(military_order, religious), [lifepath(squire)]).
lifepath_requires(id(military_order, religious), [lifepath(knight)]).
lifepath_requires(id(military_order, religious), [setting(soldier)]).
lifepath_provides(id(military_order, religious), trait(disciplined)).
lifepath_provides(id(military_order, religious), trait(fanatical_devision)).
lifepath_provides(id(military_order, religious), trait(sword_to_the_order)).
lifepath_provides(id(military_order, religious), skill(doctrine)).
lifepath_provides(id(military_order, religious), skill(riding)).
lifepath_provides(id(military_order, religious), skill(armor_training)).
lifepath_provides(id(military_order, religious), skill(appropriate_weapons)).

lifepath(id(grave_digger, religious), page(bwg, 184), 4, [outcast, servitude, city, villager]).
lifepath_provides(id(grave_digger, religious), trait(superstitious)).
lifepath_provides(id(grave_digger, religious), trait(burial_rites)).
lifepath_provides(id(grave_digger, religious), skill(ditch_digging)).
lifepath_provides(id(grave_digger, religious), skill(grave_wise)).
lifepath_provides(id(grave_digger, religious), skill(cemetery_wise)).

lifepath(id(porter, religious), page(bwg, 184), 3, [outcast, villager, peasant]).
lifepath_provides(id(porter, religious), trait(keys_to_the_church)).
lifepath_provides(id(porter, religious), trait(familiar_face)).
lifepath_provides(id(porter, religious), skill(temple_wise)).
lifepath_provides(id(porter, religious), skill(priest_wise)).
lifepath_provides(id(porter, religious), skill(worshipper_wise)).
lifepath_provides(id(porter, religious), skill(church_treasure_wise)).