# Metadata facts

In addition to defining lifepaths, there are facts for `setting`, `stock`, 
and `book`. These are supporting metadata used to help validate lifepaths.

`book` is a sourcebook and a list of pages within that sourcebook that contain
lifepath definitions. These page ranges are used to check lifepath page 
definitions.

`stock` defines the existance of particular stocks. This is used to check the
definition of `settings`. 

`setting` defines a setting for a stock. Each lifepath is associated with a 
given setting. Setting is also used to determine avialable leads when 
`any_except` lead rules are specified.