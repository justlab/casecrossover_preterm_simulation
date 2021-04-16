## About

This simulation workflow accompanies "[Evaluating the suitability of the case-crossover design under changing baseline outcome risk: A simulation of ambient temperature and preterm birth](https://doi.org/10.1101/2021.02.17.21251948)" by Carrión et. al, 2021.

...

## Installation

Make sure you have R version 4.x installed, then download or clone this repository to a directory on your computer. 

To run the simulations, you'll need recent versions of the following R packages installed:  

    "targets",
    "tidyverse",
    "here",
    "lubridate",
    "survival",
    "broom",
    "zoo",
    "splines",
    "furrr",
    "scales",
    "ggpubr",
    "future"
  
    
## Number of Simulated Datasets    
    
The publication based on this project used 1000 simulated datasets per simulated relative risk. This is controlled by the value of `repeats` in `_targets.R`.  
We have set this to 10 so that users can run a quick simulation by default, but you may change this back to 1000 or any number. However, memory (RAM) usage scales with the values of `repeats`. 

## Memory and CPU Usage

Megabytes of RAM used *per core* is approximately `463 + 5.6 * repeats`.  
For 10 `repeats`, this is around 500 MB per core, or around 6000 MB per core for 1000 repeats. 

To set the number of cores to use in parallel processing, set the value for `mc.cores` in `_targets.R`. 

The simulation will run more quickly with additional cores, but each core will need the same amount of memory, so avoid using so many cores that your system runs out of memory. You should also reserve one additional core that acts as the supervisor, and it uses approximately the same amount of memory as the worker cores. 

This means to run the default settings of 3 cores and 10 `repeats`, you would need approximately  
`(3+1) * (463 + 5.6 * 10) = 2076` megabytes of RAM free.  
To run with 8 cores and 1000 `repeats`, you would need around 55 GB of RAM free.
    
## Running the Simulation    
    
To run the simulation, start an R or RStudio session in the directory where you've downloaded this repository. In the R console, run:

    library(targets)
    tar_make()
    
And it will begin the workflow provided in the `_targets.R` file. You can cancel the run at any time, and any completed targets will be skipped the next time you run `tar_make()`. 

## Displaying Results

The last section of the `_targets.R` file lists output tables and figures you may want to view after you have completed running the simulation. Each of these are called a "target," and you can display them with the `tar_read` function, or load them into your R environment with `tar_load`. 

For example:
`tar_load(table_coverage_2018)` will load `table_coverage_2018` into your environment.  
`tar_read(vis_2018)` will display the coverage and bias plots for the 2018 simulations. 

## Reproducibility

The workflow has been made reproducible using the [targets package](https://github.com/ropensci/targets) by Will Landau and contributors. Random seeds are set, so you should receive the same results if you run this workflow multiple times while providing the same parameters. 