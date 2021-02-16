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
  # INPUT DATA ####
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

  # DATA PREPARATION ####
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
                                                 NYBirths_by_Month_single_notInduced_file)),

  # SIMULATIONS ####
  tar_target(CCO_simulation_2007,
              Simulate_and_analyze_CCO("2007-05-01", "2007-10-01", Preterms_per_day_all,
                                       1000, LaGuardiaTemp1)),
  tar_target(CCO_simulation_2018,
              Simulate_and_analyze_CCO("2018-05-01", "2018-10-01", Preterms_per_day_all,
                                       1000, LaGuardiaTemp1)),
  tar_target(CCO_simulation_2018_notInduced,
              Simulate_and_analyze_CCO("2018-05-01", "2018-10-01", Preterms_per_day_notInduced,
                                       1000, LaGuardiaTemp1)),

  # TABLES AND PLOTS ####
  tar_target(laguardia_temp_plot,
              plot_temp(LaGuardiaTemp1)),
  tar_target(table_bias_2007,
              Create_table_of_bias_results(CCO_simulation_2007)),
  tar_target(table_bias_2018,
              Create_table_of_bias_results(CCO_simulation_2018)),
  tar_target(table_bias_2018_notInduced,
              Create_table_of_bias_results(CCO_simulation_2018_notInduced)),
  tar_target(table_coverage_2007,
              Create_table_of_coverage_results(CCO_simulation_2007)),
  tar_target(table_coverage_2018,
              Create_table_of_coverage_results(CCO_simulation_2018)),
  tar_target(table_coverage_2018_notInduced,
              Create_table_of_coverage_results(CCO_simulation_2018_notInduced)),
  tar_target(vis_2007,
              Visualize_Results(CCO_simulation_2007)),
  tar_target(vis_2018,
              Visualize_Results(CCO_simulation_2018)),
  tar_target(vis_2018_notInduced,
              Visualize_Results(CCO_simulation_2018_notInduced)),
  tar_target(vis_birth_temp_2007,
              Visualize_Births_and_Temp(LaGuardiaTemp1, Preterms_per_day_all, "2007-05-01", "2007-10-01")),
  tar_target(vis_birth_temp_2018,
              Visualize_Births_and_Temp(LaGuardiaTemp1, Preterms_per_day_all, "2018-05-01", "2018-10-01"))
)