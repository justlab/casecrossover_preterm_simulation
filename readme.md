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
    
Then, start an R or RStudio session in the directory where you've downloaded this repository. In the R console, run:

    library(targets)
    tar_make()
    
And it will begin the workflow provided in the `_targets.R` file. You can cancel the run at any time, and any *completed* targets will be skipped the next time you run `tar_make()`.   

Currently, the simulations are set to run in parallel using however many CPU cores R detects, including virtual/hyperthreaded cores. For each core, you will need around 6GB of RAM. The parent process requires up to an additional 12GB of RAM. This means that on an 8 core system, you would need around 60GB of RAM. 
