library(cdcfluview)
library(tidyverse)
library(lubridate)
library(datasets)

#add a function to clean up the CDC data

cdc_clean <- function(df) {

  #add state data from base R
  states <- tibble(state_name = state.name,
                   state = state.abb)

  df2 <- df %>%
    #create a week number
    mutate(week_yr = paste(year,ifelse(week<10,paste('0',week,sep=''),week),sep='-')) %>%
    #add the states DF
    left_join(states,by='state_name') %>%
    #fix missing state codes
    mutate(state = ifelse(state_name == 'Puerto Rico','PR',
                               ifelse(state_name == 'District of Columbia','DC',
                              ifelse(state_name == 'Virgin Islands','VI',state))))

  return(df2)

}

#get the mortality data

cdc_pi <- pi_mortality('state',years=2019) %>%
  #standardize some column names
  rename(week = weeknumber,
         state_name = region_name) %>%
  #extract the year from the week start date
  mutate(year = year(wk_start)) %>%
  cdc_clean() %>%
  rename(week_nr = year_wk_num)

#cross-check with earlier data

cdc_pi_2014 <- pi_mortality('state',years=2014) %>%
  rename(week = weeknumber,
         state_name = region_name) %>%
  mutate(year = year(wk_start)) %>%
  cdc_clean() %>%
  rename(week_nr = year_wk_num)

#get the ilinet surveillance data

cdc_ili <- ilinet('state',2019) %>%
  rename(state_name = region) %>%
  cdc_clean()  %>%
  #drop the region_type column
  select(-region_type)

#get the national hospitalization data

cdc_hosp <- hospitalizations('flusurv',years=2019)

#write everything to csv

write.table(cdc_pi,'cdc_pi_mortality_season_2019.csv',quote = FALSE,row.names = FALSE,sep=',')

write.table(cdc_ili,'cdc_ilinet_surveillance_season_2019.csv',quote = FALSE,row.names = FALSE,sep=',')

write.table(cdc_ili,'cdc_fluserv_hospitalization_season_2019.csv',quote = FALSE,row.names = FALSE,sep=',')
