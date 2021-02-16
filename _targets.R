library(targets)
source("code/Simulation_21_July_2020.R")
tar_option_set(
  packages = c(
    "tidyverse",
    "here",
    "lubridate",
    "survival",
    "broom",
    "zoo",
    "splines",
    "furrr",
    "scales",
    "ggpubr"
  )
)
list(
  # INPUT DATA

  #tar_target(unused ,"data/Births_byWeek_LMP_NYS_2018.txt", format = "file"),
  #tar_target(unused ,"data/Yr_Month_GestAge_Plurality_NYS_2007to18.txt", format = "file"),
  tar_target(Births_WklyGestAge_07to18_file, "data/Births_NYS_Year_SingletonGestAge.txt", format = "file"),
  tar_target(NYBirths_by_Weekday_file, "data/Day_of_Wk_Natality_NY_2007to18.txt", format = "file"),
  tar_target(NYBirths_by_Month_plural_file, "data/Plurality_by_MonthYear_CDCWONDER.txt", format = "file"),
  tar_target(NYBirths_by_Month_single_file, "data/Births_NYS_YrMonth_SingletonGestAge.txt", format = "file"),
  tar_target(Annual_Singleton_Births_file, "data/Annual_Singleton_NYS.txt", format = "file"),
  tar_target(LaGuardiaTemp_file ,"data/CentralParkTemp_2007to2018.csv", format = "file"), #Actually Laguardia!
  #tar_target(unused ,"data/Births_YrMnth_GestationalAge_NYS.txt", format = "file"),
  #tar_target(unused ,"data/PretermBirths_NY_byDay_2018.txt", format = "file"),
  #tar_target(unused ,"data/FullTermBirths_NY_byDay_2018.txt", format = "file"),
  tar_target(Births_GestWeek_notInduced_file ,"data/NotInduced2018NYSBirths_singleton.txt", format = "file"),
  tar_target(NYBirths_by_Month_single_notInduced_file ,"data/NotInduced2018NYSBirths_bymonth_singleton.txt", format = "file"),

  # FUNCTION OUTPUTS
  tar_target(LaGuardiaTemp1, load_temp(LaGuardiaTemp_file)),
  tar_target(NYBirths_by_Day,
              Clean_and_smooth_data(NYBirths_by_Month_plural_file,
                                    NYBirths_by_Weekday_file)),
  tar_target(Preterms_per_day_all,
              Estimate_all_daily_preterms(NYBirths_by_Day,
                                          NYBirths_by_Month_single_file,
                                          Births_WklyGestAge_07to18_file,
                                          Annual_Singleton_Births_file)),
  tar_target(Preterms_per_day_notInduced,
              Estimate_nonInduced_daily_preterms(NYBirths_by_Day,
                                                 NYBirths_by_Month_single_file,
                                                 Births_GestWeek_notInduced_file,
                                                 Annual_Singleton_Births_file,
                                                 NYBirths_by_Month_single_notInduced_file))
)
