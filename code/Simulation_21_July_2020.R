suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
  library(survival)
  library(broom)
  library(zoo)
  library(splines)
  # library(devtools)
  # devtools::install_github("DavisVaughan/furrr")
  library(furrr)
  library(scales)
  #install.packages("ggpubr")
  library(ggpubr)
})


#### Functions used throughout ####


coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}

Month_stratification <- function(Hazard_Periods){ #this is based on a time stratified control day selection. Strata = months

  Control_Periods <- Hazard_Periods %>%
    mutate(Month = month(date),
           FirstDay = floor_date(date, unit = "months"),
           Weeks = as.integer(ceiling(difftime(date, FirstDay, units = "weeks"))),
           Ctrldate1 = if_else(Weeks<=1, date+days(7),
                               if_else(Weeks==2, date-days(7),
                                       if_else(Weeks==3, date-days(14), date-days(21)))),
           Ctrldate2 = if_else(Weeks<=1, date+days(14),
                               if_else(Weeks==2, date+days(7),
                                       if_else(Weeks==3, date-days(7), date-days(14)))),
           Ctrldate3 = if_else(Weeks<=1, date+days(21),
                               if_else(Weeks==2, date+days(14),
                                       if_else(Weeks==3, date+days(7), date-days(7))))) %>%
    ungroup()

  Gest_Ages <- Control_Periods %>%
    mutate(GestAge_Ctrldate1 = as.numeric(difftime(Ctrldate1, date, units = "weeks")+ Gest_Age),
           GestAge_Ctrldate2 = as.numeric(difftime(Ctrldate2, date, units = "weeks")+ Gest_Age),
           GestAge_Ctrldate3 = as.numeric(difftime(Ctrldate3, date, units = "weeks")+ Gest_Age))

  Gest_Ages1 <- Gest_Ages %>%
    dplyr::select(Participant, Ctrldate1, GestAge_Ctrldate1) %>%
    rename(date = "Ctrldate1",
           Gest_Age = GestAge_Ctrldate1)

  Gest_Ages2 <- Gest_Ages %>%
    dplyr::select(Participant, Ctrldate2, GestAge_Ctrldate2) %>%
    rename(date = "Ctrldate2",
           Gest_Age = "GestAge_Ctrldate2")

  Gest_Ages3 <- Gest_Ages %>%
    dplyr::select(Participant, Ctrldate3, GestAge_Ctrldate3) %>%
    rename(date = "Ctrldate3",
           Gest_Age = "GestAge_Ctrldate3") %>%
    bind_rows(., Gest_Ages1, Gest_Ages2)

  Control_Periods1 <- Control_Periods %>%
  dplyr::select(Participant, Ctrldate1, Ctrldate2, Ctrldate3) %>%
    group_by(Participant) %>%
    gather("Control_day", "date", Ctrldate1:Ctrldate3) %>%
    mutate(Case = 0) %>%
    dplyr::select(-Control_day) %>%
    left_join(., Gest_Ages3, by = c("Participant", "date"))

  return(Control_Periods1)
}

Biweekly_stratification <- function(Hazard_Periods){ #time stratified approach for 2 week periods

  Year <- as.character(year(Hazard_Periods$date)[1]) #pick out 1 observation to get year
  Dates_in_Year <- seq.Date(from = as.Date(paste0(Year, "-1-1")), to = as.Date(paste0(Year, "-12-31")), by = "day")
  Date_control_match <- tibble(date = Dates_in_Year) %>%
    mutate(Week = week(date),
           Week = if_else(Week == 53, 52, Week),
           Day = row_number(date),
           WkDay = wday(date),
           Strata = ceiling(Day/14),
           Strata = if_else(Strata == 27, 26, Strata))

  Sequence <- tibble(Sequence = rep.int(1:14, 26)) %>%
    add_row(Sequence = 14)

  Date_control_match1 <- bind_cols(Date_control_match, Sequence) %>%
    group_by(Strata) %>%
    mutate(Control_Period = if_else(Sequence<=7, date + days(7), date - days(7))) %>%
    ungroup() %>%
    rename("Hazard_period" = "date")

  Control_Periods <- Hazard_Periods %>%
    ungroup() %>%
    left_join(., Date_control_match1, by = c("date" = "Hazard_period")) %>%
    dplyr::select(Control_Period, Participant) %>%
    mutate(Case = 0) %>%
    rename("date" = "Control_Period")

  return(Control_Periods)
}

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

#### Cleaning Temperature Data ####

#Visualize min and max temp at LaGuardia Airport
load_temp <- function(LaGuardiaTemp_file){
  LaGuardiaTemp <- read_csv(LaGuardiaTemp_file, col_types = c("cccddd"))
  LaGuardiaTemp1 <- LaGuardiaTemp %>%
    mutate(date = mdy(DATE)) %>%
    rename("x" = "TMAX") %>%
    dplyr::select(date, x)
  LaGuardiaTemp1
}
# LaGuardiaTemp1 <- load_temp(LaGuardiaTemp_file)

plot_temp <- function(LaGuardiaTemp1){
  ggplot(LaGuardiaTemp1, aes(x = date, y = x)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Observed max temperatures at LaGuardia Airport") +
    ylab("Max temperature (F)") +
    theme(text = element_text(size = 15))
}


### Cleaning CDC Wonder and estimating all preterm births per day NYS   ####

Clean_and_smooth_data <- function(NYBirths_by_Month_plural_file, NYBirths_by_Weekday_file){
  #pulling out annual pattern of proportion births per month
  NYBirths_by_Month_plural <- read_tsv(NYBirths_by_Month_plural_file, col_types = cols())
  NYBirths_by_Month1 <- NYBirths_by_Month_plural %>%
    filter(!is.na(Month),
           `Plurality or Multiple Birth`=="Single") %>%
    rename("Month_number" = `Month Code`) %>%
    select(Year, Month, Month_number, Births) %>%
    mutate(Month = factor(Month, levels = month.name)) %>%
    group_by(Year) %>%
    mutate(Births_Year = sum(Births),
           Pct_of_total_births = Births/Births_Year) %>%
    ungroup()

  #get raw number of singleton births per month of each year
  NYBirths_by_MonthYear <- NYBirths_by_Month1 %>%
    distinct(Year, Month_number, Births, Births_Year)

  #Estimate how many births take place in a given week of the year
  NYBirths_by_Month2 <- NYBirths_by_Month1 %>%
    mutate(Week_Births = Births/4, #split by week -- assuming the average is representative of the middle of the series
           FirstWk = week(as.Date(paste(Year, Month_number,"01", sep = "-")))) %>%
    rowwise() %>%
    mutate(Week = sample(1:2, 1)) %>%
    ungroup() %>% #to stop rowwise
    mutate(Wk_of_Year = FirstWk + Week) %>% #randomly selecting either the 2nd or 3rd week to represent middle of time series
    group_by(Year) %>%
    complete(Wk_of_Year = seq(1,52)) %>%
    ungroup() %>%
    mutate(date = as.Date(paste(Year, Wk_of_Year, 1, sep="-"), "%Y-%U-%u"),
           Month_number = if_else(is.na(Month_number), month(date), Month_number)) %>%
    coalesce_join(., NYBirths_by_MonthYear, by = c("Year", "Month_number")) %>%
    rename("Month_Births" = "Births") %>%
    mutate(Week_Births1 = floor(na.approx(Week_Births, rule = 2)), #linear interpolation of births between middle of month estimates
           Week_Births_Pct = Week_Births1/Births_Year) %>%
    dplyr::select(date, Year, Wk_of_Year, Month_number, Week_Births1, Week_Births_Pct)

  #1) Births by day of week
  NYBirths_by_Weekday <- read_tsv(NYBirths_by_Weekday_file, col_types = cols())
  NYBirths_by_Weekday1 <- NYBirths_by_Weekday %>%
    filter(`Plurality or Multiple Birth`=="Single") %>%
    rename("Month_number" = `Month Code`) %>%
    select(Year, Month, Month_number, Births, Weekday) %>%
    mutate(Month = factor(Month, levels = month.name)) %>%
    group_by(Month_number, Year) %>%
    mutate(Births_Month = sum(Births),
           Prop_Births_Wkday = Births/Births_Month)

  #2) use to create a new df for day of year and projected proportion of births
  All_Dates_inTimePeriod <- tibble(date = seq.Date(as.Date("2007-01-01"), as.Date("2018-12-31"), by = "day")) %>%
    mutate(Wk_of_Year = week(date),
           Year = year(date))

  NYBirths_by_Day <- All_Dates_inTimePeriod %>%
    coalesce_join(., NYBirths_by_Month2, by = c("Year", "Wk_of_Year")) %>%
    mutate(Weekday = as.character(wday(date, label = TRUE, abbr = FALSE))) %>%
    left_join(., NYBirths_by_Weekday1, by = c("Year","Month_number","Weekday")) %>%
    mutate(Births_date = floor(Week_Births1 * Prop_Births_Wkday),
           Wk_of_Year = if_else(Wk_of_Year == 53, 52, Wk_of_Year)) %>%
    fill(., Month_number:Week_Births_Pct,Month, Births_Month, .direction = "down") %>%
    mutate(Births_date = na.approx(Births_date, rule = 2))

  return(NYBirths_by_Day)
}

Estimate_all_daily_preterms <- function(NYBirths_by_Day, NYBirths_by_Month_single_file, Births_WklyGestAge_07to18_file, Annual_Singleton_Births_file){

  #now look at singletons by gestational age to get proportion of births
  Annual_Singleton_Births <- read_tsv(Annual_Singleton_Births_file, col_types = cols())
  Annual_Singleton_Births1 <- Annual_Singleton_Births %>%
    filter(is.na(Notes)) %>%
    rename(Total_Singleton_Births_Year = "Births") %>%
    select(Year, Total_Singleton_Births_Year)

  NYBirths_by_Month_single <- read_tsv(NYBirths_by_Month_single_file, col_types = cols())
  MonthBirths_total <- NYBirths_by_Month_single %>%
    filter(Notes == "Total" & !is.na(`Month Code`)) %>%
    rename("Month_number" = `Month Code`) %>%
    mutate(Births_month = as.numeric(Births)) %>%
    select(Year, Month_number, Births_month)

  ## All births by gestational age
  Births_WklyGestAge_07to18 <- read_tsv(Births_WklyGestAge_07to18_file, col_types = cols())
  Births_WklyGestAge_07to18_a <- Births_WklyGestAge_07to18 %>%
    filter(is.na(Notes) & "LMP Gestational Age Weekly Code" != 99) %>%
    rename(Gest_Age = "LMP Gestational Age Weekly Code",
           Year_Births_perAge = "Births") %>%
    select(Year, Gest_Age, Year_Births_perAge) %>%
    left_join(., Annual_Singleton_Births1, by = "Year") %>%
    mutate(Year_Births_perAge = as.numeric(na_if(Year_Births_perAge, "Suppressed")))

  Births_WklyGestAge_07to18_b <- Births_WklyGestAge_07to18_a %>%
    group_by(Year) %>%
    summarise(Births_with_GestAge = sum(Year_Births_perAge, na.rm = T))

  #making estimates for all gestational ages
  Births_WklyGestAge_07to18_c <- Births_WklyGestAge_07to18_a %>%
    left_join(., Births_WklyGestAge_07to18_b, by = "Year") %>%
    mutate(Year_Births_perAge = if_else(is.na(Year_Births_perAge),
                                        Total_Singleton_Births_Year - Births_with_GestAge,
                                        Year_Births_perAge)) %>%
    select(-Births_with_GestAge)

  NYBirths_by_Month_single1 <- NYBirths_by_Month_single %>%
    rename("Month_number" = `Month Code`,
           Gest_Age = `LMP Gestational Age Weekly Code`) %>%
    filter(!is.na(Gest_Age)) %>%
    select(Year, Month, Month_number, Gest_Age, Births) %>%
    mutate(Month = factor(Month, levels = month.name)) %>%
    left_join(., MonthBirths_total, by = c("Year", "Month_number")) %>%
    left_join(., Births_WklyGestAge_07to18_c, by = c("Year", "Gest_Age"))

  Lowest_Preterm_Prop_notSuppressed <- NYBirths_by_Month_single1 %>%
    filter(Births != "Suppressed") %>%
    group_by(Year, Month_number) %>%
    slice_min(order_by = Gest_Age) %>%
    ungroup() %>%
    mutate(Prop_of_LowestPreterm = as.numeric(Births)/Year_Births_perAge) %>%
    select(Year, Month_number, Prop_of_LowestPreterm)

  NYBirths_by_Month_preterm_single2 <- NYBirths_by_Month_single1 %>%
    left_join(., Lowest_Preterm_Prop_notSuppressed, by = c("Year", "Month_number")) %>%
    mutate(Births = as.numeric(na_if(Births, "Suppressed")),
           Births = if_else(is.na(Births), floor(Year_Births_perAge*Prop_of_LowestPreterm), Births),
           Prop_Births = Births/Births_month) %>%
    filter(Gest_Age < 37) %>%
    dplyr::select(Year, Month_number, Gest_Age, Prop_Births)

  Preterms_per_day_all <- NYBirths_by_Day %>% #required input for everything below
    dplyr::select(date, Year, Wk_of_Year, Month_number, Births_date) %>%
    full_join(., NYBirths_by_Month_preterm_single2, by = c("Year", "Month_number")) %>%
    mutate(Preterms = floor(Births_date*Prop_Births)) #round(Births_date*Prop_Births, 0)

  return(Preterms_per_day_all)
}

Estimate_nonInduced_daily_preterms <- function(NYBirths_by_Day, NYBirths_by_Month_single_file, Births_GestWeek_notInduced_file, Annual_Singleton_Births_file, NYBirths_by_Month_single_notInduced_file){

  Annual_Singleton_Births <- read_tsv(Annual_Singleton_Births_file, col_types = cols())
  Annual_Singleton_Births1 <- Annual_Singleton_Births %>%
    filter(is.na(Notes)) %>%
    rename(Total_Singleton_Births_Year = "Births") %>%
    select(Year, Total_Singleton_Births_Year)

  NYBirths_by_Month_single <- read_tsv(NYBirths_by_Month_single_file, col_types = cols())
  MonthBirths_total <- NYBirths_by_Month_single %>%
    filter(Notes == "Total" & !is.na(`Month Code`)) %>%
    rename("Month_number" = `Month Code`) %>%
    mutate(Births_month = as.numeric(Births)) %>%
    select(Year, Month_number, Births_month)

  ## Just those singleton births that arent induced with gestational age
  Births_GestWeek_notInduced <- read_tsv(Births_GestWeek_notInduced_file, col_types = cols())
  Births_GestWeek_notInduced_a <- Births_GestWeek_notInduced %>%
    filter(is.na(Notes) & "LMP Gestational Age Weekly Code" != 99) %>%
    rename(Gest_Age = "LMP Gestational Age Weekly Code",
           Year_Births_perAge = "Births") %>%
    select(Gest_Age, Year_Births_perAge) %>%
    mutate(Year = 2018) %>%
    left_join(., Annual_Singleton_Births1, by = "Year") %>%
    mutate(Year_Births_perAge = as.numeric(na_if(Year_Births_perAge, "Suppressed")),
           Year_Births_perAge = ifelse(is.na(Year_Births_perAge), sample(5:9, 1), Year_Births_perAge))

  NYBirths_by_Month_single_notInduced <- read_tsv(NYBirths_by_Month_single_notInduced_file, col_types = cols())
  NYBirths_by_Month_single_notInduced1 <- NYBirths_by_Month_single_notInduced %>%
    rename("Month_number" = `Month Code`,
           Gest_Age = `LMP Gestational Age Weekly Code`) %>%
    filter(!is.na(Gest_Age)) %>%
    select(Month, Month_number, Gest_Age, Births) %>%
    mutate(Month = factor(Month, levels = month.name),
           Year = 2018) %>%
    left_join(., MonthBirths_total, by = c("Year", "Month_number")) %>%
    left_join(., Births_GestWeek_notInduced_a, by = c("Year", "Gest_Age"))

  Lowest_Preterm_Prop_notSuppressed1 <- NYBirths_by_Month_single_notInduced1 %>%
    filter(Births != "Suppressed") %>%
    group_by(Year, Month_number) %>%
    slice_min(order_by = Gest_Age) %>%
    ungroup() %>%
    mutate(Prop_of_LowestPreterm = as.numeric(Births)/Year_Births_perAge) %>%
    select(Year, Month_number, Prop_of_LowestPreterm)

  NYBirths_by_Month_single_notInduced2 <- NYBirths_by_Month_single_notInduced1 %>%
    left_join(., Lowest_Preterm_Prop_notSuppressed1, by = c("Year", "Month_number")) %>%
    mutate(Births = as.numeric(na_if(Births, "Suppressed")),
           Births = if_else(is.na(Births), floor(Year_Births_perAge*Prop_of_LowestPreterm), Births),
           Prop_Births = Births/Births_month) %>%
    filter(Gest_Age < 37) %>%
    dplyr::select(Year, Month_number, Gest_Age, Prop_Births)

  Preterms_per_day_notInduced <- NYBirths_by_Day %>%
    filter(year(date) == 2018) %>%
    dplyr::select(date, Year, Wk_of_Year, Month_number, Births_date) %>%
    full_join(., NYBirths_by_Month_single_notInduced2, by = c("Year", "Month_number")) %>%
    mutate(Preterms = floor(Births_date*Prop_Births)) #round(Births_date*Prop_Births, 0)

  return(Preterms_per_day_notInduced)
}

# NYBirths_by_Day <- Clean_and_smooth_data(NYBirths_by_Month_plural_file, NYBirths_by_Weekday_file, NYBirths_by_Month_single_file)
# Preterms_per_day_all <- Estimate_all_daily_preterms(NYBirths_by_Day, NYBirths_by_Month_single_file, Births_WklyGestAge_07to18, Annual_Singleton_Births_file)
# Preterms_per_day_notInduced <- Estimate_nonInduced_daily_preterms(NYBirths_by_Day, NYBirths_by_Month_single_file, Births_GestWeek_notInduced_file, Annual_Singleton_Births_file, NYBirths_by_Month_single_notInduced_file)

#### Creating Simulations and conducting case crossovers ####

##need to create lambdas ###
Create_Parameters_for <- function(start_date, end_date, Preterms_per_day_df, LaGuardiaTemp1){ ##RR per 10F

  RiskRatios <- tibble(RR_per_10F = seq.default(from = .9, to = 1.25, length.out = 8),
                       Simulated_RR = seq.default(from = .9, to = 1.25, length.out = 8)) %>%
    mutate(lnRR_per_degreeF = log(RR_per_10F)/10)

  Preterms_per_day_indexYear <- Preterms_per_day_df %>%
    filter(date >= start_date & date < end_date)

  Beta_naughts <- Preterms_per_day_indexYear %>%
    group_by(Gest_Age, Month_number) %>%
    summarise(ln_beta_naught = log(mean(Preterms, na.rm = TRUE)), #calculating as input
              Dispersion = var(Preterms, na.rm = TRUE)) %>%
    crossing(RiskRatios) %>%
    ungroup()

  Parameters <- Preterms_per_day_indexYear %>%
    left_join(., Beta_naughts, by = c("Gest_Age", "Month_number")) %>%
    left_join(., LaGuardiaTemp1, by = "date") %>%
    mutate(lambda = exp(ln_beta_naught + (lnRR_per_degreeF*x)))

  return(Parameters)
}

Random_draws <- function(Parameters_df){ #make a function to repeat x times for monte carlo

  MonteCarlo_df <- Parameters_df %>%
    rowwise() %>%
    mutate(Random_draw = rpois(1, lambda))

  return(MonteCarlo_df)
}


Case_Crossovers <- function(Params_for_Simulated_Year){

  Simulated_RR <- Params_for_Simulated_Year$Simulated_RR[1]

                                                    #get rid of filters when refunctionalized
  CC_Exposures  <-  Params_for_Simulated_Year %>% #Change back to simulated year
    dplyr::select(date, x) %>%
    distinct(date, x)

  CC_casedays <- Params_for_Simulated_Year %>% filter(Random_draw!=0)%>%
    uncount(Random_draw) %>%
    mutate(Participant = row_number(),
           Case = 1) %>%
    dplyr::select(date, Participant, Case, Gest_Age)

  #Month Stratified Case Crossover dataset
  Simulation_df_MonthStrat <- bind_rows(CC_casedays, Month_stratification(CC_casedays)) %>%
    left_join(., CC_Exposures, by = "date") %>%
    mutate(Prop_Month = (day(date)-1)/days_in_month(date))

  #clogit regression - no adjustment
  mod.clogit.month <- clogit(Case ~ x + strata(Participant), # each case day is a strata #number of events in each day
                             method = "efron", # the method tells the model how to deal with ties
                             Simulation_df_MonthStrat)

  CCOResults_monthstrat <- broom::tidy(mod.clogit.month) %>%
    mutate(Analysis = "CCO_Month") %>%
    bind_cols(., tibble(Simulated_RR = Simulated_RR))

  #regression - proportion of month adjustment
  mod.clogit.month.prop <- clogit(Case ~ x + Prop_Month + strata(Participant),
                             method = "efron",
                             Simulation_df_MonthStrat)

  CCOResults_monthstrat_propmth <- broom::tidy(mod.clogit.month.prop) %>%
    filter(term == "x") %>%
    mutate(Analysis = "CCO_Month_PropMth") %>%
    bind_cols(., tibble(Simulated_RR = Simulated_RR))

  #regression - adjustment for Gestational Age
  mod.clogit.month.gestage <- clogit(Case ~ x + Gest_Age + strata(Participant),
                                  method = "efron",
                                  Simulation_df_MonthStrat)

  CCOResults_monthstrat_gestage <- broom::tidy(mod.clogit.month.gestage) %>%
    filter(term == "x") %>%
    mutate(Analysis = "CCO_Month_GestAge") %>%
    bind_cols(., tibble(Simulated_RR = Simulated_RR))

  ### 2 week stratified case crossover ###

  Simulation_df_2WeekStrat <- bind_rows(CC_casedays, Biweekly_stratification(CC_casedays))%>%
    left_join(., CC_Exposures, by = "date")

  mod.clogit.2wk <- clogit(Case ~ x + strata(Participant), # each case day is a strata #number of events in each day
                           method = "efron", # the method tells the model how to deal with ties
                           Simulation_df_2WeekStrat)

  CCOResults_biweekstrat <- broom::tidy(mod.clogit.2wk) %>%
    mutate(Analysis = "CCO_2week") %>%
    bind_cols(., tibble(Simulated_RR = Simulated_RR))

  RegressionResults <- bind_rows(CCOResults_monthstrat, CCOResults_monthstrat_propmth, CCOResults_biweekstrat, CCOResults_monthstrat_gestage)

  return(RegressionResults)

}

#create parameters for 2007 and 2018
#plan(multisession(workers = 14)) #for parallelization

Simulate_and_analyze_CCO <- function(start_date, end_date, Preterms_per_day_df, number_of_repeats, Temp_df){

  Parameters <- Create_Parameters_for(start_date, end_date, Preterms_per_day_df, Temp_df)

  Bootstrapped_counts <- number_of_repeats %>%
    rerun(Random_draws(Parameters)) %>%
    tibble() %>%
    unnest(cols = c(.)) %>%
    dplyr::select(date, Simulated_RR, Gest_Age, x, Random_draw) %>%
    group_by(Simulated_RR, date, Gest_Age) %>%
    mutate(Round_of_Sim = row_number(),
           Splits = paste(Simulated_RR, Round_of_Sim, sep = ".")) %>% #creating one variable on which to split for parallelization
    ungroup()

  Results_CaseCrossovers <- Bootstrapped_counts %>%
    split(.$Splits) %>%
    future_map_dfr(., ~Case_Crossovers(.x), .progress = T)

  return(Results_CaseCrossovers)

}

# CCO_simulation_2007 <- Simulate_and_analyze_CCO("2007-05-01", "2007-10-01", Preterms_per_day_all, 1000, LaGuardiaTemp1)
# CCO_simulation_2018 <- Simulate_and_analyze_CCO("2018-05-01", "2018-10-01", Preterms_per_day_all, 1000, LaGuardiaTemp1)
# CCO_simulation_2018_notInduced <- Simulate_and_analyze_CCO("2018-05-01", "2018-10-01", Preterms_per_day_notInduced, 1000, LaGuardiaTemp1)


#### Analyze Results ####
Create_table_of_bias_results <- function(Simulation_results){

  Results_CaseCrossovers <- Simulation_results %>%
    group_by(Analysis, Simulated_RR) %>%
    mutate(Round_of_Sim = row_number()) %>%
    ungroup()

  Bias_Estimates <- Results_CaseCrossovers %>%
    mutate(Bias_OR = exp((estimate*10)-log(Simulated_RR)),
           Analysis = factor(Analysis, levels = c("CCO_2week", "CCO_Month", "CCO_Month_GestAge", "CCO_Month_PropMth"),
                             labels = c("Time stratified: 2 weeks", "Time Stratified: Month", "Time Stratified: Month,\nAdjustment: Gestational Age",
                                        "Time Stratified: Month,\nAdjustment: Proportion of Month")))
  Bias_Estimates1 <- Bias_Estimates %>%
    group_by(Analysis) %>%
    summarise(bias_median = median(Bias_OR),
              bias_IQR = paste0(round(quantile(Bias_OR, .25), 3), "-", round(quantile(Bias_OR, .75), 3)))

  return(Bias_Estimates1)
}

Create_table_of_coverage_results <- function(Simulation_results){

  Results_CaseCrossovers <- Simulation_results %>%
    group_by(Analysis, Simulated_RR) %>%
    mutate(Round_of_Sim = row_number()) %>%
    ungroup()

  Coverage <- Results_CaseCrossovers %>%
    mutate(Exp_ConfLow = exp(conf.low*10),
           Exp_ConfHigh = exp(conf.high*10),
           Exp_Estimate = exp(estimate*10)) %>%
    dplyr::select(Round_of_Sim, Exp_Estimate, Exp_ConfLow, Exp_ConfHigh, Simulated_RR, Analysis)

  Coverage1 <- Coverage %>%
    ungroup() %>%
    mutate(Covered = if_else(Simulated_RR>=Exp_ConfLow & Simulated_RR<=Exp_ConfHigh, 1, 0)) %>%
    group_by(Simulated_RR, Analysis) %>%
    summarise(Coverage = (sum(Covered)/1000)) %>%
    ungroup() %>%
    mutate(Analysis = factor(Analysis, levels = c("CCO_2week", "CCO_Month", "CCO_Month_GestAge", "CCO_Month_PropMth"),
                             labels = c("Time stratified: 2 weeks", "Time Stratified: Month", "Time Stratified: Month,\nAdjustment: Gestational Age",
                                        "Time Stratified: Month,\nAdjustment: Proportion of Month")))

  Coverage2 <- Coverage1 %>%
    group_by(Analysis) %>%
    summarise(min_coverage = min(Coverage),
              max_coverage = max(Coverage))

  return(Coverage2)
}


Visualize_Results <- function(results_df){

  Results_CaseCrossovers1 <- results_df %>%
    filter(Analysis=="CCO_2week" | Analysis=="CCO_Month") %>%
    group_by(Analysis, Simulated_RR) %>%
    mutate(Round_of_Sim = row_number(),
           Analysis = factor(Analysis, levels = c("CCO_2week", "CCO_Month"),
                             labels = c("Time stratified: 2 weeks", "Time Stratified: Month"))) %>%
    ungroup()

  Bias_Estimates <- Results_CaseCrossovers1 %>%
    mutate(Bias_OR = exp((estimate*10)-log(Simulated_RR)))

  Bias_plot <- ggplot(Bias_Estimates) +
    geom_boxplot(aes(Simulated_RR, Bias_OR, fill = Analysis)) +
    facet_grid(~Simulated_RR, scales = "free", switch = "x") +
    ylab("Bias") +
    theme_minimal(base_size = 22) +
    scale_x_continuous(breaks = NULL) +
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())

  ##Coverage plots ##
  Coverage <- Results_CaseCrossovers1 %>%
    mutate(Exp_ConfLow = exp(conf.low*10),
           Exp_ConfHigh = exp(conf.high*10),
           Exp_Estimate = exp(estimate*10)) %>%
    dplyr::select(Round_of_Sim, Exp_Estimate, Exp_ConfLow, Exp_ConfHigh, Simulated_RR, Analysis)

  Coverage1 <- Coverage %>%
    ungroup() %>%
    mutate(Covered = if_else(Simulated_RR>=Exp_ConfLow & Simulated_RR<=Exp_ConfHigh, 1, 0)) %>%
    group_by(Simulated_RR, Analysis) %>%
    summarise(Coverage = (sum(Covered)/1000)) %>%
    ungroup()

  Coverage_plot <- ggplot() +
    #geom_point(data = Coverage1 %>% filter(Analysis == "Time stratified: 2 weeks"), aes(x = as.numeric(Analysis), y = Coverage, shape = Analysis, fill = Analysis), size = 7, shape = 23) +
    geom_point(data = Coverage1, #%>% filter(Analysis != "Time stratified: 2 weeks")
               aes(x = as.numeric(Analysis), y = Coverage, shape = Analysis, fill = Analysis), size = 9, shape = 23) + #position=position_dodge(0.05),
    facet_grid(~Simulated_RR, switch = "x") +
    geom_hline(yintercept = .95, linetype = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100), minor_breaks = seq(0 , 1, .05), breaks = seq(0, 1, .20), limits = c(0,1)) +
    scale_x_continuous(breaks = NULL, limits = c(.5, 2.75)) + #.5, 4.5
    theme_minimal(base_size = 22) +
    theme(legend.position = "none") +
    xlab("Simulated Relative Risk") +
    ylab("Coverage of 95% CI")

  combined_plot <- ggarrange(Bias_plot, Coverage_plot, ncol = 1, nrow = 2, labels = "AUTO")
  return(combined_plot)
}


# Create_table_of_bias_results(CCO_simulation_2007)
# Create_table_of_bias_results(CCO_simulation_2018)
# Create_table_of_bias_results(CCO_simulation_2018_notInduced)
#
# Create_table_of_coverage_results(CCO_simulation_2007)
# Create_table_of_coverage_results(CCO_simulation_2018)
# Create_table_of_coverage_results(CCO_simulation_2018_notInduced)
#
# Visualize_Results(CCO_simulation_2007)
# Visualize_Results(CCO_simulation_2018) #1450 x 750
# Visualize_Results(CCO_simulation_2018_notInduced)

##
Visualize_Births_and_Temp <- function(Temp_df, Births_df, start_date, end_date){

  year_of_analysis <- year(start_date)

  Temp_plot <- Temp_df %>%
    filter(year(date)==year_of_analysis) %>%
    ggplot() +
    geom_line(aes(x = date, y = x), color = "blue") +
    annotate("rect", fill = "orange", alpha = 0.25,
             xmin = as.Date(start_date), xmax = as.Date(end_date),
             ymin = -Inf, ymax = Inf)+
    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    ylab("Maximum Temperature (F)")

  Preterms_2018_plot <- Births_df %>%
    group_by(date) %>%
    summarise(`Preterm Births` = sum(Preterms)) %>%
    filter(year(date)==year_of_analysis) %>%
    ggplot() +
    geom_line(aes(x = date, y = `Preterm Births`)) +
    annotate("rect", fill = "orange", alpha = 0.25,
             xmin = as.Date(start_date), xmax = as.Date(end_date),
             ymin = -Inf, ymax = Inf) +
    theme_minimal() +
    xlab("Date")

  combined_plot <- ggarrange(Temp_plot, Preterms_2018_plot, ncol = 1, nrow = 2, labels = "AUTO")

  return(combined_plot)
}


# Visualize_Births_and_Temp(LaGuardiaTemp1, Preterms_per_day_all, "2007-05-01", "2007-10-01") #700*550
# Visualize_Births_and_Temp(LaGuardiaTemp1, Preterms_per_day_all, "2018-05-01", "2018-10-01")
