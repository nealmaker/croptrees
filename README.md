# croptrees
Developing criteria for choosing timber crop trees in the US Northeast 

## Scripts
- data.r builds the starting dataset of trees to consider, defines parameters to guide simulations, and creates a couple of custom functions to help the simulations.

- simulation.R sources data.R, runs a simulation for each tree in the dataset, and creates a 'sim' object with trees' attributes and values at each timestep, absent any discounting.

- lev.R creates a data frame of sites and discount rates, with associated LEVs, based on definitions of expected repacement trees and calculations of NPVs of an infinite rotation of each tree. 

- fct-croptrees.R defines a function that takes simulation data, a discount rate, a price factor (for scaling expected future stumpage values), and the LEV table, and returns a data frame of trees with a variable that defines if they are crop trees. [DOES NOT ACCOUNT FOR LEV YET, BUT SHOULD]

- grades-analysis.R sources simulation.R, explores relationships between grade and value growth (absent considerations of discounting and LEV, but accounting for mortality), and saves resulting plots to output/, allowing for detailed analysis of the effects of grade.

- coarse-analysis.R uses the output of the croptrees function and default discount rates and price factors to build a base rate Random Forest model that predicts crop trees, then does ML interpretation of the model and saves resulting plots to output/, allowing for analysis of general trends.

- run.R sources all the other scripts in order, to allow a user to run the entire analysis in one go.


