# BOKI: crosschannel

This folder contains scripts to compute crosschannel synchrony based on windowed lagged cross-correlation as implemented by the rMEA package. It contains the function sync_cross which takes the following inputs: 
s1, s2   ... two time series based on which synchrony is supposed to be computed
sr1, sr2 ... original sample rates of the time series
sampRate ... desired sample rate
lagSec   ... lag in seconds
winSec   ... window length in seconds
incSec   ... increment length in seconds
r2Z      ... convert correlation to Z values (default T)
ABS      ... convert output to absolute values (default T)

The output is a MEA object containing the CCF values as well as the resampled time series. 
