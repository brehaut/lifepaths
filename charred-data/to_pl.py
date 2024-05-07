import json
import re

settings_map = {
    'Peasant Setting':'peasant', 
    'Villager Setting':'villager', 
    'City Dweller Setting':'city', 
    'Noble Setting':'noble', 
    'Noble Court Subsetting':'court', 
    'Servitude And Captive Setting':'servitude', 
    'Outcast Subsetting':'outcast', 
    'Professional Soldier Subsetting':'soldier', 
    'Seafaring Setting':'sea', 
    'Religious Subsetting':'religious',
    'Rats':'rats',
}

special_atom_characters_re = re.compile("[^a-z0-9_]")
def string_to_atom(str): 
    base = str.lower().replace(" ", "_").replace("-", "_").replace("'", "")
    if special_atom_characters_re.search(base):
        return f"'{base}'"
    return base

def to_pl_list(list):
    return f"[{','.join(list)}]"

def process_lifepath(name_atom, setting_atom, lifepath_json):
    time = lifepath_json['time']
    leads = [settings_map[lead] for lead in lifepath_json['key_leads']]
    return f"lifepath(id({name_atom}, {setting_atom}), page(bwg, 0), {time}, {to_pl_list(leads)})."


def process_provides(name_atom, setting_atom, kind, list):
    return (f"lifepath_provides(id({name_atom}, {setting_atom}), {kind}({string_to_atom(i)}))." for i in list if isinstance(i, str))

def process_file(filename):
    lp_facts = []
    lifepath_provides_facts = []

    with open(filename, "r") as f:
        data = json.load(f)
        settings = data['settings']
        for setting, lifepaths in settings.items():
            setting_atom = settings_map[setting]
            for lifepath_name, details in lifepaths.items():
                lp_name = string_to_atom(lifepath_name)
                lp_facts.append(process_lifepath(lp_name, setting_atom, details))
                lifepath_provides_facts.extend(process_provides(lp_name, setting_atom, "traits", details.get("traits", [])))
                lifepath_provides_facts.extend(process_provides(lp_name, setting_atom, "skill", sum(details.get("skills", []), [])))

    print("\n".join(lp_facts) + "\n\n" + "\n".join(lifepath_provides_facts))
        
    
process_file("man.json")
