HurDat2 Florida Storm Analysis
Author: Max Greenwald

Given the current HurDat2 data file, find and print all of
the storms since 1900 that made landfall in Florida.

To run:

`erl
c(storm_parser).
c(storm_analyzer).
c(analysis_worker).
Analyzer = storm_analyzer:start(5).
storm_parser:start("hurdat2.txt", Analyzer).
`
