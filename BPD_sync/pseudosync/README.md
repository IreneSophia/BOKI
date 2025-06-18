# BOKI: pseudosync

This folder contains scripts to compute pseudosynchrony based on segment shuffling. It contains a function to compute the pseudosynchrony and a script which uses the function for multiple MEA objects. The pseudosync_seg function takes the following inputs: 

* mea
* sampRate ... sample rate
* lagSec   ... lag in seconds
* winSec   ... window length in seconds
* incSec   ... increment length in seconds
* n        ... number of simulations (default 100)
* peak     ... whether to use peak picking or grand averages (default T)
* r2Z      ... convert correlation to Z values (default T)
* ABS      ... convert output to absolute values (default T)

