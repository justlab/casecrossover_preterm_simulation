## About

This simulation workflow accompanies "[Evaluating the suitability of the case-crossover design under changing baseline outcome risk: A simulation of ambient temperature and preterm birth](https://doi.org/10.1101/2021.02.17.21251948)" by Carrión et. al, 2021.

...

## Installation

Make sure you have R version 4.x installed, then download or clone this repository to a directory on your computer. 

To run the simulations, you'll need recent versions of the following R packages installed:  

    packages <- c("targets",
    "tarchetypes",
    "tidyverse",
    "here",
    "lubridate",
    "survival",
    "broom",
    "zoo",
    "purrr",
    "scales",
    "ggpubr",
    "qs",
    "future",
    "future.callr") # or use an alternative future backend, like multisession
    
    install.packages(setdiff(packages, rownames(installed.packages()))) 
    
## Number of Simulated Datasets    
    
The publication based on this project used 1000 simulated datasets per simulated relative risk. This is controlled by the value of `repeats` in `_targets.R`.  
We have set this to 10 so that users can run a quick simulation by default, but you may change this back to 1000 or any number. However, memory (RAM) usage scales with the values of `repeats` (see below). 

## Running the Simulation    
    
To run the simulation, start an R or RStudio session in the directory where you've downloaded this repository. In the R console, run:

    library(targets)
    tar_make()
    
And it will begin the workflow provided in the `_targets.R` file. You can cancel the run at any time, and any completed targets will be skipped the next time you run `tar_make()`. 

When the workflow is finished, it will render a summary report to `code/report.html` that you can open in your web browser.  

Alternatively, you can run the workflow much faster by running multiple workers at once. Instead of `tar_make()`, run:     
    
    tar_make_future(workers = 4L)
    
replacing the number 4 with however many simultaneous workers you would like to run, up to the number of CPU cores on your system. You must also have enough system RAM to support all of the workers running at once.  

## Memory Usage

Megabytes of RAM used *per worker* increases with `repeats`. Systems with 8 GB of RAM will likely be able to run the workflow with the default setting of 10 `repeats`.  

Use these *rough* estimates of memory usage per worker to decide how many simultaneous workers your system can support based on your selection for `repeats`:  

| repeats | estimated RAM per worker |
| --- | --- |
| 10 | 300 MB | 
| 40 | 800 MB | 
| 100 | 2500 MB | 
| 1000 | 3600 MB | 

## Displaying Results

The last section of the `_targets.R` file lists output tables and figures you may want to view after you have completed running the simulation. Each of these are called a "target," and you can display them with the `tar_read` function, or load them into your R environment with `tar_load`.  

For example:  
`tar_load(table_coverage_2018)` will load `table_coverage_2018` into your environment.  
`tar_read(vis_2018)` will display the coverage and bias plots for the 2018 simulations.  

These tables and plots have also been pulled into the `code/report.html` generated at the end of the workflow.  

## Reproducibility

The workflow has been made reproducible using the [targets package](https://github.com/ropensci/targets) by Will Landau and contributors. Random seeds are set, so you should receive the same results if you run this workflow multiple times while providing the same parameters.  

The Targets package gives many ways to examine your run of the workflow. For a useful flowchart showing status and execution time, try: `tar_visnetwork(targets_only = TRUE, label = 'time')`.
