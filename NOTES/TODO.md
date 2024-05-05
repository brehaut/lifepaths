# TODO

Lots of things to do.

 * Many unit tests need to be added.
 * Currently lifepath names are presumed to be globally unique. This is not 
   true. For example 'groom' exists in both the village, city, and nobel court
   settings. lp_requires, lp_provides need to specify a setting, and those 
   need to be used in `satisfies_requirements`.
 * Characters need to have properties defined before lifepath selection 
   begins, such as the 'female' flag that opens up special rules
   and alternative requirements paths, such as for 'auger'. 
 * Unify constraint and requirement satisfying. These systems have slightly 
   different APIs but it would be useful to support all those rules with
   one piece of code, and reuse rules more freely.
 * Investigate generating the bulk of the knowledge base from (for example)
   the charred (or charred black) dataset. Either loading the JSON directly, 
   or by processing it and writing out prolog. 
    * The requirements and some property definitions would still need to be 
      entered by hand.
