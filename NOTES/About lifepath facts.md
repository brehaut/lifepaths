Lifepath’s are defined in `lp` facts. Shortened because it needs to be typed 
a lot for data entry. A lifepath definition consists of 5 values:

 * name:atom — A direct translation of the name from Burning Wheel 
   Gold Revised into a bare atom.
 * Setting:atom — This is the Burning Wheel concept of a setting and is 
   used by leads. This value must reference a 'setting' fact. This is checked 
   by the unit tests.
 * Page number: page(Book:atom, Number:int) structure — Book is an atom that 
   references a 'book' fact, and Number is an integer for the page in that book.
   Number is checked against the page ranges defined in the 'book' fact. Page
   number exists purely for reference once a lifepath is chosen.
 * Years:integer — How many years this lifepath takes. Used to calculate 
   character age.
 * Leads:List of atoms, or any_except(List of atoms) — Either a list of 
   settings (as atoms), or a structure called any_except that contains a list
   of settings that are not valid leads. See lp_leads predicate in lifepaths.pl

This is already getting verbose and fiddly to deal with so additional properties
are split off into their own facts, such as `lp_provides`, and `lp_requires`.

# `lp_provides`

each `lp_provide` fact associates a lifepath name with an additional property, 
such as a trait, a flag, or a skill. These properties are defined by structures,
and are used to satisfy `lp_requires` rules.

 * name:atom – references the lifepath defined by `lp`.
 * property:structure — This is one of:
    * trait(TraitName:atom) — TraitName is the name of a trait that this 
      lifepath makes available.
    * skill(SkillName:atom) – SkillName is the name of a skill that this lifepath
      makes available. 
    * flag(FlagName:atom) – This is an arbitrary property. For example it’s used 
      annotate lifepaths that are explicitly for female characters.

# `lp_requires``

Each `lp_requires` fact associates a lifepath name with one or more requirements 
that need to be satisfied in order to select it.

Where `lp_provides`is simple and has one fact per property, `lp_requires` has 
a list of properties that must all be true. Additional facts supply alternative 
ways to satisfy requirements. For example an `auger` requires:
  "Midwife, Country Wife or must be female and character has no more than three 
  lifepaths total.". Each of the three options is a separate `lp_provides`, with
  two rules for the final form. 

Additionally, some rules are wrapped in a `constraint(Rule)` structure. These 
constraints are collected during lifepath selection and propagated to the end 
of the process where they are used to check additional rulse that require 
all the lifepaths to be chosen. For example, `elder` requires that the character
starts the game at least 50 years old.
