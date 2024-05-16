# TODO

Lots of things to do.

 * Many unit tests need to be added.
 * Make lifepath name normalization better and more userfriendly. 
 * Characters need to have properties defined before lifepath selection 
   begins, such as the 'female' flag that opens up special rules
   and alternative requirements paths, such as for 'auger'. 
 * Unify constraint and requirement satisfying. These systems have slightly 
   different APIs but it would be useful to support all those rules with
   one piece of code, and reuse rules more freely.
    * constrains need a better name: 'all_lifepaths'? 
 * Investigate generating the bulk of the knowledge base from (for example)
   the charred (or charred black) dataset. Either loading the JSON directly, 
   or by processing it and writing out prolog. 
    * The requirements and some property definitions would still need to be 
      entered by hand.

Data:
  * Annotate horsey lifepaths with flag(horse)
  * Some way of defining 'requires two lifepath': Captain, soldier requires 2x freebooter lifepaths

# Character Path optimizations

The current `character_path` predicate is bruteforce and doesn’t take 
advantage of the structure of lifepaths or characters in any way. It starts
from a born lifepath and works its way to the end of the list and if it runs
out it runs out. 

This is fine for checking an existing character path against the lifepath 
constraints. But it’s not suitable for exploration both in terms of 
performance and user experience.

A better implementation would start from a set of target lifepaths, calculate
all the requirement options, and then work backward trying to resolve those.
This has the benefit that it can look only at lifepaths that provide the 
requirements rather than all lifepaths.
