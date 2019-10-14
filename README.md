## Directory structure

- data: input data used in the simulations.
- R: R scripts
- results: model outputs, compiled model results and cached regression results
- figures: figures used in the paper
- tables: tables used in the paper.

## Scripts

IMPORTANT: Each scripts lists the versions of the packages under it was developed. Please make sure that you have the right version of all the packages installed before running the scripts, otherwise later packages updates might break the code.

- R/001_simulation.R: Runs simulations and compiles the outputs.
- R/002_figures.R: plots figures 1 to 3
- R/003_tables.R: produces tables 2 and 3.
- R/004_regression.R: Fits model outputs to get a model for the bias. Runs model selection and produces table 4 and figures 4 and 5.
- indirect_estimates_functions.R: functions used to compute indirect estimates.
- simulation_functions.R: functions used to run the simulations.

## Docker

The code in 001_simulation.R does parallel processing and we need to register a parallel backend. Here we use the doRedis package that offers an interface to a Redis server installed on a docker. The number of workers should be adjusted to the memory and cpu resources on the computer. Please visit https://docs.docker.com to learn how to install docker on your computer.