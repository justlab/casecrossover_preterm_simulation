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

#Datasets#
Births_GestWeek_LMP <- read_tsv(here::here("data", "Births_byWeek_LMP_NYS_2018.txt"))
Births_GestWeek_OE <- read_tsv(here::here("data", "Births_byWeek_OE_NYS_2018.txt"))
Births_GestAge_LMP <- read_tsv(here::here("data", "Yr_Month_GestAge_Plurality_NYS_2007to18.txt"))
# NYBirths_by_Month <- read_tsv(here::here("Data", "AllBirthsNY_byMonth_CDCWonder.txt"))
NYBirths_by_Weekday <- read_tsv(here::here("data", "Day_of_Wk_Natality_NY_2007to18.txt"))
NYBirths_by_Month_plural <- read_tsv(here::here("data", "Plurality_by_MonthYear_CDCWONDER.txt"))
CentralParkTemp <- read_csv(here::here("data", "CentralParkTemp_2007to2018.csv"))
Preterm_by_Month <- read_tsv(here::here("data", "Births_YrMnth_GestationalAge_NYS.txt"))
Preterm_by_Day <- read_tsv(here::here("data", "PretermBirths_NY_byDay_2018.txt"))
Fullterm_by_Day <- read_tsv(here::here("data", "FullTermBirths_NY_byDay_2018.txt"))
Holidays <- read_csv(here::here("data", "Holidays.csv"))

###################
#### Functions ####
###################

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
    # group_by(month = month(date)) %>%
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
    ungroup() %>%
    dplyr::select(Participant, Ctrldate1, Ctrldate2, Ctrldate3) %>%
    group_by(Participant) %>%
    gather("Control_day", "date", Ctrldate1:Ctrldate3) %>% ##now give them 0 for Case and merge with temp data
    mutate(Case = 0) %>%
    dplyr::select(-Control_day)
  
  return(Control_Periods)
}

Biweekly_stratification <- function(Hazard_Periods){ #right now only for 2018
  
  Year <- "2018" #figure out how to get this
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
  #left_join(., CC_Exposures, by = c("Control_Period"="date")) %>% ###update this to not pull in the exposure cuz that happens below
  #dplyr::select(Hazard_period, Control_Period)
  
  Control_Periods <- Hazard_Periods %>% #Hazard_Periods %>%
    ungroup() %>%
    left_join(., Date_control_match1, by = c("date" = "Hazard_period")) %>%
    #dplyr::select(date, Participant, Round_of_Sim, Simulated_RR) %>%
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
##############################
#### Simulate Temperature ####
##############################

#Visualize min and max temp at Central Park
CentralParkTemp1 <- CentralParkTemp %>%
  mutate(date = mdy(DATE)) %>%
  rename("x" = "TMAX") %>%
  dplyr::select(date, x)

ggplot(CentralParkTemp1, aes(x = date, y = x)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Observed max temperatures at LaGuardia Airport") +
  ylab("Max temperature (F)") + 
  theme(text = element_text(size = 15))
# 
# #let's model based on the max temp for now
# MeanTemp_CentralPark <- round(mean(CentralParkTemp$TMAX), 2)
# StdDevTemp_CentralPark <- round(mean(loess.sd(CentralParkTemp$TMAX)$sd), 2) #or do I want the sd for the entire year??
# 
# testexp <- sim_exposure(n = 365, central = MeanTemp_CentralPark, 
#                         sd = StdDevTemp_CentralPark, trend = "cos1",
#                         exposure_type = "continuous", 
#                         amp = -0.5,
#                         start.date = "2018-01-01")
# 
# ggplot(testexp, aes(x = date, y = x)) +  
#   geom_point(alpha = 0.5, size = 0.8) + 
#   coord_cartesian(ylim = c(0,110)) + 
#   labs(title = "Temperature modelled on 2018 Central Park", x = "Date", y="Temperature (F)") + 
#   geom_smooth()+
#   theme_classic()
# 
# eesim::calendar_plot(testexp)
#looks like we did a decent job with a simulated temperature 


#Baseline_exposure_2018 <- custom_exposure(n = 364, df = CentralParkTemp1, metric = "TMAX", start.date = "2018-01-01")

#########################
#### Simulate Births ####
#########################



##############################################
###   Simulating all births per day NYS   ####
##############################################

# Holidays1 <- Holidays %>%
#   mutate(Date = as.Date(paste(Year, Date, sep = "-")))

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

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

NYBirths_by_Month1 %>% #visualize birth seasonality over time
  mutate(Order = row_number(),
         Month_Year = paste(Month_number, Year, sep = "-"),
         Month_Year1 = factor(x = Order, levels = Order, labels = Month_Year)) %>%
  ggplot() + geom_line(aes(Month_Year1, Pct_of_total_births, group = 1)) + 
  labs(x = "Month-Year", y = "Percent of Annual Births (Singletons) per Month") + 
  scale_x_discrete(breaks = every_nth(n = 6))

NYBirths_by_MonthYear <- NYBirths_by_Month1 %>% 
  distinct(Year, Month_number, Births, Births_Year)

NYBirths_by_MonthYear %>%
  distinct(Year, Births_Year) %>%
  rename("Singleton Births" = Births_Year) %>%
  kable() %>%
  kable_styling()

NYBirths_by_Month2 <- NYBirths_by_Month1 %>%
  mutate(Week_Births = Births/4, #split by week -- assuming the average is representative of the middle of the series 
         FirstWk = week(as.Date(paste(Year, Month_number,"01", sep = "-")))) %>%
  rowwise() %>% #to make a random selection for each row
  mutate(Week = sample(1:2, 1)) %>%
  ungroup() %>% #to stop rowwise
  mutate(Wk_of_Year = FirstWk + Week) %>%
  group_by(Year) %>%
  complete(Wk_of_Year = seq(1,52)) %>% #should I make it to 53?
  ungroup() %>%
  mutate(date = as.Date(paste(Year, Wk_of_Year, 1, sep="-"), "%Y-%U-%u"),
         Month_number = if_else(is.na(Month_number), month(date), Month_number)) %>%
  coalesce_join(., NYBirths_by_MonthYear, by = c("Year", "Month_number")) %>%
  rename("Month_Births" = "Births") %>%
  mutate(Week_Births1 = floor(na.approx(Week_Births, rule = 2)),
         Week_Births_Pct = Week_Births1/Births_Year) %>%
  dplyr::select(date, Year, Wk_of_Year, Month_number, Week_Births1, Week_Births_Pct)

ggplot(NYBirths_by_Month2) + geom_line(aes(x = date, y = Week_Births1))
ggplot(NYBirths_by_Month2) + geom_line(aes(x = date, y = Week_Births_Pct))
#1) Births by day of week

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
  coalesce_join(., NYBirths_by_Weekday1, by = c("Year","Month_number","Weekday")) %>%
  mutate(Births_date = floor(Week_Births1 * Prop_Births_Wkday),
         Wk_of_Year = if_else(Wk_of_Year == 53, 52, Wk_of_Year)) %>%
  fill(., Month_number:Week_Births_Pct,Month, Births_Month, .direction = "down") %>%
  mutate(Births_date = na.approx(Births_date, rule = 2))

ggplot(NYBirths_by_Day, aes(date, Births_date)) +
  geom_line()+ geom_smooth(method = "lm", se = FALSE) + 
  ylab("Births (count)") + 
  theme(text = element_text(size = 15))

NYBirths_by_Day %>%
  filter(Year >=2017) %>%
  ggplot(aes(date, Births_date)) + geom_line()+ geom_smooth(method = "lm")

########################
#### Preterm births ####
########################

Births_GestWeek_LMP1 <- Births_GestWeek_LMP %>%
  rename(LMP_Age = `LMP Gestational Age Weekly Code`,
         Pct_Births = `% of Total Births`) %>%
  mutate(Pct_Births = as.numeric(str_remove_all(Pct_Births, "%")),
         CumPct_Births = cumsum(Pct_Births)) %>%
  select(LMP_Age, Births, Pct_Births, CumPct_Births) %>%
  filter(!is.na(LMP_Age)) %>%
  filter(LMP_Age!=99)#Should we get rid of the unknown gestational ages? probably

Births_GestWeek_OE1 <- Births_GestWeek_OE %>%
  rename(OE_Age = `OE Gestational Age Weekly Code`,
         Pct_Births = `% of Total Births`) %>%
  mutate(Pct_Births = as.numeric(str_remove_all(Pct_Births, "%")),
         CumPct_Births = cumsum(Pct_Births)) %>%
  select(OE_Age, Births, Pct_Births, CumPct_Births) %>%
  filter(!is.na(OE_Age))%>%
  filter(OE_Age!=99)

ggplot() + geom_col(data = Births_GestWeek_LMP1, aes(LMP_Age, Pct_Births), fill = "green", alpha = .4) + 
  geom_col(data = Births_GestWeek_OE1, aes(OE_Age, Pct_Births), fill = "red", alpha = .6)
##LMP has more of a bellcurve and OE has more of a left skew. the mode is the same for both of them. 

#look at proportion of births by LMP age
ggplot() + geom_line(data = Births_GestWeek_LMP1, aes(x = LMP_Age, y = Pct_Births)) + 
  geom_line(data = Births_GestWeek_LMP1, aes(x = LMP_Age, y = CumPct_Births), color = "red") + 
  geom_vline(xintercept=37, linetype="dotted") + 
  ggtitle("Proportion of births by gestational age. Dotted = 37 weeks")



#weight 225904 by month of conception -- how many born by month in 2018? 

###Another way to look at preterm -- by age categories and month/years 
Births_GestAge_LMP1 <- Births_GestAge_LMP %>%
  filter(`Plurality or Multiple Birth`=="Single") %>%
  rename("Gest_Age" = "LMP Gestational Age 10",
         "Month_number" = `Month Code`) %>%
  dplyr::select(Year, Month_number, Gest_Age, Births) %>%
  mutate(Births = as.numeric(na_if(Births, "Suppressed"))) 

Births_by_Month <- NYBirths_by_Month1 %>% dplyr::select(Year, Month_number, Births) %>% rename("Births_month" = "Births")

Births_GestAge_LMP2 <- Births_GestAge_LMP1 %>%
  mutate(Gest_Age = if_else(Gest_Age=="Under 20 weeks"|Gest_Age=="20 - 27 weeks", "15 - 27 weeks", Gest_Age)) %>%
  group_by(Year, Month_number, Gest_Age) %>%
  summarise(Births = sum(Births)) %>%
  left_join(., Births_by_Month, by = c("Month_number", "Year")) %>%
  group_by(Year, Month_number) %>%
  mutate(Births_w_GestAge = sum(Births, na.rm = TRUE),
         Births = if_else(is.na(Births), Births_month - Births_w_GestAge, Births), 
         Prop_Births = Births/Births_month) %>%
  ungroup()


Births_GestAge_LMP2 %>% #plot to see if there are temporal trends by gestational age (birth counts)
  arrange(Year, Month_number, Gest_Age) %>%
  group_by(Gest_Age) %>%
  mutate(Order = row_number()) %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(Order, Births)) +
  geom_smooth(aes(Order, Births), method = "loess", se = FALSE) +
  facet_wrap(~Gest_Age, scales = "free") + 
  theme(text = element_text(size = 15))

Births_GestAge_LMP2 %>% #plot to see if there are temporal trends by gestational age (proportion of all births)
  arrange(Year, Month_number, Gest_Age) %>%
  group_by(Gest_Age) %>%
  mutate(Order = row_number()) %>%
  ungroup() %>%
  mutate(Prop_Births = round(Prop_Births*100, 2)) %>%
  ggplot() + 
  geom_line(aes(Order, Prop_Births)) +
  geom_smooth(aes(Order, Prop_Births), method = "loess", se = FALSE) +
  facet_wrap(~Gest_Age, scales = "free") 

Births_GestAge_LMP3 <- Births_GestAge_LMP2 %>% #pull out only the preterms
  filter(Gest_Age=="15 - 27 weeks"|Gest_Age=="28 - 31 weeks"|Gest_Age=="32 - 35 weeks"|Gest_Age=="36 weeks") %>%
  dplyr::select(-Births_w_GestAge, -Births_month, -Births)

#Pull preterms out of overall births

Preterms_per_day <- NYBirths_by_Day %>%
  dplyr::select(date, Year, Wk_of_Year, Month_number, Births_date) %>%
  full_join(., Births_GestAge_LMP3, by = c("Year", "Month_number")) %>%
  mutate(Preterms = floor(Births_date*Prop_Births))


Preterms_per_day %>%
  ggplot(aes(date, Preterms)) +
  geom_point()+
  geom_smooth(method = "loess")+
  facet_wrap(~Gest_Age, scales = "free") 

##need to create lambdas ###
Create_Parameters_for <- function(Year_to_simulate){ ##RR per 10F
  
  RiskRatios <- tibble(RR_per_10F = seq.default(from = .9, to = 1.25, length.out = 8),
                       Simulated_RR = seq.default(from = .9, to = 1.25, length.out = 8)) %>%
    mutate(lnRR_per_degreeF = log(RR_per_10F)/10)
  
  Preterms_per_day_indexYear <- Preterms_per_day %>%
    filter(Year == Year_to_simulate) #change to year
  
  Beta_naughts <- Preterms_per_day_indexYear %>%
    group_by(Gest_Age, Month_number) %>%
    summarise(ln_beta_naught = log(mean(Preterms, na.rm = TRUE)), #calculating as input 
              Dispersion = var(Preterms, na.rm = TRUE)) %>%
    crossing(RiskRatios) %>%
    ungroup()
  
  Parameters <- Preterms_per_day_indexYear %>%
    left_join(., Beta_naughts, by = c("Gest_Age", "Month_number")) %>% 
    left_join(., CentralParkTemp1, by = "date") %>%
    mutate(lambda = exp(ln_beta_naught + (lnRR_per_degreeF*x))) 
  
  return(Parameters)
}

Random_draws <- function(Parameters_df){ #make a function to repeat x times for monte carlo
  
  MonteCarlo_df <- Parameters_2018 %>%
    rowwise() %>% 
    mutate(Random_draw = rpois(1, lambda)) %>%
    ungroup() %>% 
    group_by(date, x, Simulated_RR) %>%
    summarise(All_Preterms = sum(Random_draw)) %>%
    ungroup() %>%
    group_by(Simulated_RR) #%>%
  #mutate(Time = time(date))
  
  return(MonteCarlo_df)
}

Parameters_2018 <- Create_Parameters_for(2018) #doing this for 2018 -- will try other years later

set.seed(2) #it would be for the smallest gestational group, which would be ~2-3

Bootstrapped_counts <- 1000 %>% 
  rerun(Random_draws(Parameters_2018)) %>% 
  tibble() %>%
  unnest() %>%
  group_by(Simulated_RR, date) %>%
  mutate(Round_of_Sim = row_number(),
         Splits = paste(Simulated_RR, Round_of_Sim, sep = ".")) %>% #creating one variable on which to split for parallelization
  ungroup()

#needs to be run per RR and per simulation

Case_Crossovers <- function(Simulated_Year){ #make a function to repeat x times for monte carlo
  
  Simulated_RR <- Simulated_Year$Simulated_RR[1]
  
  CC_Exposures  <-  Simulated_Year %>% #Change back to simulated year
    dplyr::select(date, x)
  
  CC_casedays <- Simulated_Year %>% 
    uncount(All_Preterms) %>%
    mutate(Participant = row_number(), 
           Case = 1) %>%
    dplyr::select(date, Participant, Case) 
  
  #Month Stratified Case Crossover
  Simulation_df_MonthStrat <- bind_rows(CC_casedays, Month_stratification(CC_casedays)) %>% 
    left_join(., CC_Exposures, by = "date")
  
  mod.clogit.month <- clogit(Case ~ x + strata(Participant), # each case day is a strata #number of events in each day
                             method = "efron", # the method tells the model how to deal with ties
                             Simulation_df_MonthStrat) 
  
  CCOResults_monthstrat <- broom::tidy(mod.clogit.month) %>%
    mutate(Analysis = "CCO_Month") %>%
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
  
  RegressionResults <- bind_rows(CCOResults_monthstrat, CCOResults_biweekstrat) 
  
  return(RegressionResults)
  
}

#try on one simulated year 
Bootstrapped_counts %>%
  filter(Splits=="0.9.5") %>%
  Case_Crossovers(.)

#now to parallelize
plan(multisession(workers = 30)) #available cores on belle - 2


Results_CaseCrossovers <- Bootstrapped_counts %>%
  split(.$Splits) %>%
  future_map_dfr(., ~Case_Crossovers(.x)) #using furrr -- run the crossovers 

saveRDS(Results_CaseCrossovers, here::here("data", "Bootstrapped_CCO_results.RDS"))

Results_CaseCrossovers1 <- Results_CaseCrossovers %>%
  group_by(Analysis, Simulated_RR) %>%
  mutate(Round_of_Sim = row_number()) %>%
  ungroup()


Bias_Estimates <- Results_CaseCrossovers1 %>%
  group_by(Analysis, Simulated_RR) %>%
  summarise(Mean_Estimate = round(exp(mean(estimate)*10), 3)) %>%
  mutate(Bias_RR = Mean_Estimate-Simulated_RR) 

print(Bias_Estimates)

# View(Results_CaseCrossovers1 %>% 
#        group_by(Analysis, Simulated_RR) %>%
#        summarise(Mean_Estimate = mean(estimate),
#                  StdErr_Estimate = sd(estimate)) %>%
#        mutate(Sim_RR_logperunit = log(Simulated_RR)/10,
#               lnBias = Mean_Estimate-Sim_RR_logperunit,
#               expBias = exp(lnBias*10),
#               Percent_Difference = round((((Mean_Estimate/Sim_RR_logperunit)-1)*100),2)))

View(Results_CaseCrossovers1 %>% 
       group_by(Analysis, Simulated_RR) %>%
       summarise(Mean_Estimate = mean(estimate),
                 StdErr_Estimate = sd(estimate),
                 Mean_Estimate_exp = mean(exp(estimate*10))) %>%
       mutate(Percent_Difference = round((((Mean_Estimate_exp-Simulated_RR)/Simulated_RR)*100),2)))


##Coverage plots ##
Coverage <- Results_CaseCrossovers1 %>%
  mutate(Exp_ConfLow = exp(conf.low*10),
         Exp_ConfHigh = exp(conf.high*10),
         Exp_Estimate = exp(estimate*10)) %>%
  dplyr::select(Round_of_Sim, Exp_Estimate, Exp_ConfLow, Exp_ConfHigh, Simulated_RR, Analysis)

##Visualize coverage
Coverage %>%
  ungroup() %>%
  filter(Analysis == "CCO_Month") %>%
  ggplot() + 
  geom_pointrange(aes(x = Round_of_Sim, y = Exp_Estimate, ymin = Exp_ConfLow, ymax = Exp_ConfHigh)) + 
  geom_hline(aes(yintercept = Simulated_RR), color = "red") +
  facet_grid(~Simulated_RR, scales = "free") + 
  xlab("Round of Simulation (Month stratified)") +
  ylab("Simulated Effect (RR per 10F)")

Coverage %>%
  ungroup() %>%
  filter(Analysis == "CCO_2week") %>%
  ggplot() + 
  geom_pointrange(aes(x = Round_of_Sim, y = Exp_Estimate, ymin = Exp_ConfLow, ymax = Exp_ConfHigh)) + 
  geom_hline(aes(yintercept = Simulated_RR), color = "red") +
  facet_grid(~Simulated_RR, scales = "free") + 
  xlab("Round of Simulation (2 week stratified)") +
  ylab("Simulated Effect (RR per 10F)")

#calculating percent coverage 
Coverage1 <- Coverage %>%
  ungroup() %>%
  mutate(Covered = if_else(Simulated_RR>Exp_ConfLow & Simulated_RR<Exp_ConfHigh, 1, 0)) %>%
  group_by(Simulated_RR, Analysis) %>%
  summarise(Coverage = (sum(Covered)/1000)*100)



